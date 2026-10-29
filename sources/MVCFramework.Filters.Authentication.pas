// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ***************************************************************************
//
// Authentication as filter bundles. Each mechanism of the classic
// authentication middleware is one function that builds all its parts with
// the same parameters, registered with one Use:
//
//   AEngine.Use(JWTAuthenticationBundle(TMyAuth.Create, lClaimsSetup, dotEnv.Env('JWT_SECRET')));
//   lAdmin := AGroup.Use(JWTAuthenticationBundle(TMyAuth.Create, lClaimsSetup, lSecret, '/api/v1/admin/login'));
//
// On the engine the check covers every controller and Minimal API route; on a
// route group it covers the group only, and the HTTP part (login, logout) goes
// on the engine. How the handler is asked on controllers and on Minimal API
// routes, and the order with the other filters and with the classic
// middleware: "Authentication" in MVCFramework.Filters.
// OIDC: OIDCAuthenticationBundle in MVCFramework.Filters.OIDC.

unit MVCFramework.Filters.Authentication;

{$I dmvcframework.inc}

interface

uses
  MVCFramework,
  MVCFramework.HMAC,                           // HMAC_HS512
  MVCFramework.JWT,                            // TJWTClaimsSetup, TJWTCheckableClaims, IJWTSigner, TMVCJWTCookieSameSite
  MVCFramework.JWT.RefreshToken,               // TMVCRefreshTokenConfig, IMVCRefreshTokenStore
  MVCFramework.Filters;                        // TMVCAuthCookieOptions

type
  TMVCJWTDefaults = class sealed
  public const
    /// <summary>
    /// Default authorization header name (RFC 6750)
    /// </summary>
    AUTHORIZATION_HEADER = 'Authorization';
    /// <summary>
    /// Default username header name
    /// </summary>
    USERNAME_HEADER = 'jwtusername';
    /// <summary>
    /// Default password header name
    /// </summary>
    PASSWORD_HEADER = 'jwtpassword';
    /// <summary>
    /// Default AUTH schema
    /// </summary>
    AUTH_SCHEMA = 'Bearer';
    /// <summary>
    /// Default url authorization token
    /// </summary>
    AUTHORIZATION_ACCESS_TOKEN = 'access_token';
  end;

  // The blacklist: OnAcceptToken refuses a token by setting AAccepted to False,
  // OnNewJWTToBlackList receives the token of a logout.
  TMVCOnAcceptTokenProc = reference to procedure(AContext: TWebContext; AJWTToken: String;
    var AAccepted: Boolean);
  TMVCOnNewJWTToBlackList = reference to procedure(AContext: TWebContext; AJWTToken: String);

  // Options of JWTAuthenticationBundle. Start from Default, then set at least
  // Handler, ClaimsSetup and Secret (or Signer).
  TMVCJWTAuthenticationOptions = record
    Handler: IMVCAuthenticationHandler;         // required
    ClaimsSetup: TJWTClaimsSetup;               // required; nil with the refresh token or without a login
    Secret: string;                             // HMAC key, or...
    Signer: IJWTSigner;                         // ...a signer (RS256 & co): one of the two
    LoginURL: string;                           // '/login'; '': no login, see JWTAuthenticationBundle
    ClaimsToCheck: TJWTCheckableClaims;         // [ExpirationTime]
    LeewaySeconds: Cardinal;                    // 300
    HMACAlgorithm: string;                      // HS512, with Secret
    AccessTokenParamName: string;               // '': the token from the Authorization header only
    OnAcceptToken: TMVCOnAcceptTokenProc;       // the blacklist: both callbacks or none
    OnNewJWTToBlackList: TMVCOnNewJWTToBlackList;
    BlackListURL: string;                       // '/logout'; with RefreshTokenStore it also revokes the refresh token
    RefreshToken: TMVCRefreshTokenConfig;       // Issuer, RefreshURLSegment, TTLs; signed as the bundle signs
    RefreshTokenStore: IMVCRefreshTokenStore;   // nil: no refresh token; one for the whole process
    class function Default: TMVCJWTAuthenticationOptions; static;
  end;

  // Options of JWTCookieAuthenticationBundle: those of TMVCJWTAuthenticationOptions
  // that apply to a token in a cookie, plus the cookie, the logout and the
  // origins allowed to log in.
  TMVCJWTCookieAuthenticationOptions = record
    Handler: IMVCAuthenticationHandler;         // required
    ClaimsSetup: TJWTClaimsSetup;               // required
    Secret: string;                             // HMAC key, or...
    Signer: IJWTSigner;                         // ...a signer (RS256 & co): one of the two
    LoginURL: string;                           // '/login', required
    LogoutURL: string;                          // '/logout'
    ClaimsToCheck: TJWTCheckableClaims;         // [ExpirationTime]
    LeewaySeconds: Cardinal;                    // 300
    HMACAlgorithm: string;                      // HS512, with Secret
    Cookie: TMVCAuthCookieOptions;              // TMVCAuthCookieOptions.Default
    LoginAllowedOrigins: TArray<string>;        // empty: the Origin of the login is not checked
    class function Default: TMVCJWTCookieAuthenticationOptions; static;
  end;

// The JWT of TMVCJWTAuthenticationMiddleware, in the Authorization header.
//
// HTTP part, on the engine. The login: a request to LoginURL (any verb, as the
// middleware) brings the credentials in the jwtusername / jwtpassword headers
// or form fields, in a JSON body ({"jwtusername": ...} or {"username": ...,
// "password": ...}), or as `Authorization: Basic`. They go to
// Handler.OnAuthentication; when accepted the answer is {"token": "..."}, a
// token with ClaimsSetup applied plus the "username" and "roles" claims and
// the session data of the handler. Wrong credentials: 401.
// With OnAcceptToken and OnNewJWTToBlackList, before the login, the blacklist
// of TMVCJWTBlackListMiddleware: a request to BlackListURL hands its bearer
// token to OnNewJWTToBlackList and answers 204 (400 without a token); on every
// other request a bearer token goes to OnAcceptToken, and a token it refuses
// gets 403. It reads the token as the check does: from the Authorization
// header or, when the header is missing, from the AccessTokenParamName
// parameter.
// With RefreshTokenStore, the refresh token: the login answers the pair the
// refresh URL answers, {"access_token": ..., "refresh_token": ...,
// "token_type": "bearer", "expires_in": ...}, and a POST to
// RefreshToken.RefreshURLSegment with {"refresh_token": ...} rotates it and
// answers a new pair (RefreshTokenHTTPFilter). Both access tokens, of the login
// and of the refresh, carry "username" and "roles" (what the store keeps), the
// issuer and the lifetime of RefreshToken, and are signed with Secret or
// Signer: ClaimsSetup has nothing to set (it must be nil), and the session
// data of the handler is not in them. With the blacklist too, a request to
// BlackListURL whose JSON body carries {"refresh_token": ...} also revokes that
// refresh token and its family (IMVCRefreshTokenStore.Revoke); without that
// body only the access token is blacklisted.
// The store must be one for the whole process (a global, as in the sample
// jsonwebtoken_refreshtoken): under WebBroker, ISAPI and Apache every web
// module has its own engine, and a token issued by one must be refreshed by
// the others.
// With LoginURL '' there is no login: the tokens come from a login of your own
// that signs them with the same Secret or Signer, or, with RefreshTokenStore,
// issues the pair with a TMVCRefreshTokenCore on the same RefreshToken (Secret
// included) and store. The bundle then has the check, the blacklist and the
// refresh URL when they are set, and ClaimsSetup must be nil. That login must
// be outside the check: with the bundle on the engine a Minimal API route is
// always checked, so the login is a controller action the handler's OnRequest
// makes public; or the bundle goes on the route group it protects, and the
// login stays outside the group.
//
// Endpoint part: the check. The token comes from `Authorization: Bearer
// <token>`, or from the request parameter AccessTokenParamName when given and
// the header is missing (SSE, <img> and download links cannot send a header;
// a token in a URL ends up in logs and Referer, so the default '' takes the
// header only). It must be signed with Secret (HMACAlgorithm) or Signer, pass
// ClaimsToCheck and carry a "username" claim; Context.LoggedUser gets
// username, roles and the custom claims (and, for a token without "iat", the
// time of the request as login time, so AuthorizeEndpointFilter and
// RequireRoleEndpointFilter accept it). Missing or invalid token: 401; refused
// by OnAuthorization: 403. ClaimsToCheck defaults to the expiration ([]
// checks none: a token would never expire). When the token was issued with a
// live validity window and is about to expire, the response carries a
// refreshed one in the Authorization header.
// For a controller action OnRequest may make it public: the token, if any, is
// still read, so the action can tell a logged user from an anonymous one.
//
// A missing handler or claims setup (with the login of the bundle, without
// refresh token) raises, as an empty secret, the secret shipped with the
// framework, or Secret and Signer together.
//
//   AEngine.Use(JWTAuthenticationBundle(TMyAuth.Create, lClaimsSetup, lSecret));
//
//   lOptions := TMVCJWTAuthenticationOptions.Default;
//   lOptions.Handler := TMyAuth.Create;
//   lOptions.Secret := lSecret;
//   lOptions.RefreshTokenStore := gRefreshTokenStore; // a global, created once in initialization
//   AEngine.Use(JWTAuthenticationBundle(lOptions));
function JWTAuthenticationBundle(const AHandler: IMVCAuthenticationHandler; const AClaimsSetup: TJWTClaimsSetup;
  const ASecret: string; const ALoginURL: string = '/login'): IMVCFilterBundle; overload;
function JWTAuthenticationBundle(const AOptions: TMVCJWTAuthenticationOptions): IMVCFilterBundle; overload;

// The JWT in a cookie, of TMVCJWTCookieAuthenticationMiddleware.
//
// HTTP part, on the engine: with LoginAllowedOrigins, a login whose Origin is
// none of them gets 403 (AllowedOriginHTTPFilter on LoginURL: a request
// without Origin passes, it is not a browser cross-origin request); then the
// login, which reads the credentials as JWTAuthenticationBundle and answers
// {"token": "..."} with the token also in the cookie, and LogoutURL, which
// expires the cookie.
// Endpoint part: the check of JWTAuthenticationBundle, with the token read
// from the cookie only. A refreshed token replaces the cookie and the response
// carries `X-JWT-Refreshed: true`.
// No blacklist: TMVCJWTBlackListMiddleware reads the Authorization header, or
// the AccessTokenParamName parameter, never the cookie.
// A missing handler, claims setup or LoginURL raises, as the secrets of
// JWTAuthenticationBundle and a cookie with SameSite=None without Secure.
// The short form checks no Origin on the login: for LoginAllowedOrigins use
// the options form.
//
//   lOptions := TMVCJWTCookieAuthenticationOptions.Default;
//   lOptions.Handler := TMyAuth.Create;
//   lOptions.ClaimsSetup := lClaimsSetup;
//   lOptions.Secret := lSecret;
//   lOptions.Cookie.Secure := dotEnv.Env('JWT_COOKIE_SECURE', True);
//   lOptions.LoginAllowedOrigins := ['https://app.example.com'];
//   AEngine.Use(JWTCookieAuthenticationBundle(lOptions));
function JWTCookieAuthenticationBundle(const AHandler: IMVCAuthenticationHandler; const AClaimsSetup: TJWTClaimsSetup;
  const ASecret: string; const ACookie: TMVCAuthCookieOptions; const ALoginURL: string = '/login';
  const ALogoutURL: string = '/logout'): IMVCFilterBundle; overload;
function JWTCookieAuthenticationBundle(const AOptions: TMVCJWTCookieAuthenticationOptions): IMVCFilterBundle; overload;

// Basic authentication (RFC 7617), TMVCBasicAuthenticationMiddleware: an
// endpoint part only, BasicAuthEndpointFilter(AHandler, ARealm) of
// MVCFramework.Filters, which see. The handler is required.
function BasicAuthenticationBundle(const AHandler: IMVCAuthenticationHandler;
  const ARealm: string = 'DelphiMVCFramework REALM'): IMVCFilterBundle;

// The custom authentication of TMVCCustomAuthenticationMiddleware (and of
// TMVCRoleBasedAuthMiddleware, with a TRoleBasedAuthHandler as handler), on a
// session.
// HTTP part, on the engine: login and logout at ALoginURL. POST with a JSON body
// {"username": ..., "password": ...}: the handler checks them and the identity
// goes into the session; DELETE ends the session.
// Endpoint part: the check. Without the identity of the session 401, refused
// by OnAuthorization 403, both answered as the middleware does (JSON, or HTML
// for a browser) with X-LOGIN-URL / X-LOGIN-METHOD.
// Register it after a session HTTP filter (MemorySessionHTTPFilter, ...): the
// classic session middleware runs after every HTTP filter, too late for the
// login. The handler is required.
function CustomAuthenticationBundle(const AHandler: IMVCAuthenticationHandler;
  const ALoginURL: string = '/system/users/logged'): IMVCFilterBundle;

implementation

uses
  System.SysUtils,
  JsonDataObjects,                             // TJDOJsonObject
  MVCFramework.Commons,
  MVCFramework.JWT.RefreshToken.Filters,       // RefreshTokenHTTPFilter
  MVCFramework.Filters.Internal,               // FilterAuthHandler, CheckHandler
  MVCFramework.Filters.Authentication.Internal; // the cores of the bundles

type
  TMVCJWTAuthenticationBundle = class(TMVCFilterBundle, IMVCJWTAuthenticationBundle)
  private
    fLoginURL: string;
  public
    constructor Create(const AHTTPFilters: TArray<TMVCHTTPFilter>; const AEndpointFilters: TArray<TMVCEndpointFilter>;
      const ALoginURL: string);
    function LoginURL: string;
  end;

constructor TMVCJWTAuthenticationBundle.Create(const AHTTPFilters: TArray<TMVCHTTPFilter>;
  const AEndpointFilters: TArray<TMVCEndpointFilter>; const ALoginURL: string);
begin
  inherited Create(AHTTPFilters, AEndpointFilters);
  fLoginURL := ALoginURL;
end;

function TMVCJWTAuthenticationBundle.LoginURL: string;
begin
  Result := fLoginURL;
end;

// --- The parts of the bundles ---------------------------------------------------

// TryLogin finds out only at the first login, with a 500
procedure CheckClaimsSetup(const AClaimsSetup: TJWTClaimsSetup; const ABundleName: string);
begin
  if not Assigned(AClaimsSetup) then
    raise EMVCException.CreateFmt('%s: the claims setup is required (it sets the expiration of the token)',
      [ABundleName]);
end;

// Signer, or Secret as a signer: the middleware built with a secret signs with
// HS512 whatever algorithm it is given, the one built with a signer uses its algorithm.
function BundleSigner(const ASecret: string; const ASigner: IJWTSigner;
  const AHMACAlgorithm, ABundleName: string): IJWTSigner;
begin
  if not Assigned(ASigner) then
    Exit(THMACJWTSigner.Create(AHMACAlgorithm, ASecret)); // it refuses an empty or published secret
  if ASecret <> '' then
    raise EMVCException.CreateFmt('%s: Secret and Signer are alternatives, give one of them', [ABundleName]);
  Result := ASigner;
end;

function JWTEndpointFilter(const AAuthenticationHandler: IMVCAuthenticationHandler; const ASigner: IJWTSigner;
  const AClaimsToCheck: TJWTCheckableClaims; const ALeewaySeconds: Cardinal;
  const AAccessTokenParamName: string): TMVCEndpointFilter;
var
  lCore: TMVCJWTAuthenticationCore;
begin
  // no login URL: the login is JWTLoginHTTPFilter
  lCore := TMVCJWTAuthenticationCore.Create(FilterAuthHandler(AAuthenticationHandler), nil, '', ASigner, '',
    AClaimsToCheck, ALeewaySeconds, AAccessTokenParamName);
  Result :=
    function(const AContext: TWebContext; const ANext: TMVCEndpointFilterNext): IMVCResponse
    begin
      lCore.Check(AContext, AContext.ControllerQualifiedClassName, AContext.ActionName);
      Result := ANext();
    end;
end;

function JWTLoginHTTPFilter(const AAuthenticationHandler: IMVCAuthenticationHandler;
  const AClaimsSetup: TJWTClaimsSetup; const ASigner: IJWTSigner; const ALoginURLSegment: string;
  const ALeewaySeconds: Cardinal): TMVCHTTPFilter;
var
  lCore: TMVCJWTAuthenticationCore;
begin
  // the login reads no token: the claims to check only keep the core from warning about them
  lCore := TMVCJWTAuthenticationCore.Create(AAuthenticationHandler, AClaimsSetup, '', ASigner,
    ALoginURLSegment, [TJWTCheckableClaim.ExpirationTime], ALeewaySeconds, '');
  Result :=
    procedure(const AContext: TWebContext; const ANext: TMVCHTTPFilterNext)
    var
      lToken: string;
      lExpires: TDateTime;
    begin
      if lCore.TryLogin(AContext, lToken, lExpires) then
        TMVCJWTAuthenticationCore.RenderTokenJSON(AContext, lToken)
      else
        ANext();
    end;
end;

// the token of the login is never answered: nothing to set up
// A function, not a procedure passed as TJWTClaimsSetup: the compiler would build that reference
// in the frame of the caller's closures, and the closure that keeps it would keep itself (a leak).
function NoClaimsSetup: TJWTClaimsSetup;
begin
  Result :=
    procedure(const AJWT: TJWT)
    begin
    end;
end;

function JWTRefreshLoginHTTPFilter(const AAuthenticationHandler: IMVCAuthenticationHandler;
  const ASigner: IJWTSigner; const ALoginURLSegment: string; const ALeewaySeconds: Cardinal;
  const AConfig: TMVCRefreshTokenConfig; const AStore: IMVCRefreshTokenStore): TMVCHTTPFilter;
var
  lLogin: TMVCJWTAuthenticationCore;
  lRefresh: IMVCRefreshTokenCore;
begin
  lLogin := TMVCJWTAuthenticationCore.Create(AAuthenticationHandler, NoClaimsSetup(), '', ASigner,
    ALoginURLSegment, [TJWTCheckableClaim.ExpirationTime], ALeewaySeconds, '');
  lRefresh := TMVCRefreshTokenCore.Create(AConfig, AStore);
  Result :=
    procedure(const AContext: TWebContext; const ANext: TMVCHTTPFilterNext)
    var
      lToken: string;
      lExpires: TDateTime;
    begin
      // the login checks the credentials with the handler and sets LoggedUser; the refresh core answers
      // the pair of tokens, as its refresh URL does
      if lLogin.TryLogin(AContext, lToken, lExpires) then
        lRefresh.IssueTokenPair(AContext, AContext.LoggedUser.UserName,
          string.Join(',', AContext.LoggedUser.Roles.ToArray))
      else
        ANext();
    end;
end;

function NewJWTCookieCore(const AHandler: IMVCAuthenticationHandler;
  const AClaimsSetup: TJWTClaimsSetup; const ASigner: IJWTSigner;
  const ACookie: TMVCAuthCookieOptions; const ALoginURLSegment, ALogoutURLSegment: string;
  const AClaimsToCheck: TJWTCheckableClaims; const ALeewaySeconds: Cardinal): TMVCJWTCookieAuthenticationCore;
begin
  Result := TMVCJWTCookieAuthenticationCore.Create(AHandler, AClaimsSetup, '', ASigner,
    ALoginURLSegment, ALogoutURLSegment, AClaimsToCheck, ALeewaySeconds);
  // SameSite=None without Secure raises in either order of the two setters
  Result.CookieName := ACookie.Name;
  Result.SetCookieSameSite(ACookie.SameSite);
  Result.SetCookieSecure(ACookie.Secure);
  Result.CookiePath := ACookie.Path;
  Result.CookieDomain := ACookie.Domain;
end;

function JWTCookieEndpointFilter(const AAuthenticationHandler: IMVCAuthenticationHandler; const ASigner: IJWTSigner;
  const ACookie: TMVCAuthCookieOptions; const AClaimsToCheck: TJWTCheckableClaims;
  const ALeewaySeconds: Cardinal): TMVCEndpointFilter;
var
  lCore: TMVCJWTCookieAuthenticationCore;
begin
  // no login or logout URL: those are JWTCookieLoginHTTPFilter
  lCore := NewJWTCookieCore(FilterAuthHandler(AAuthenticationHandler), nil, ASigner, ACookie, '', '',
    AClaimsToCheck, ALeewaySeconds);
  Result :=
    function(const AContext: TWebContext; const ANext: TMVCEndpointFilterNext): IMVCResponse
    begin
      lCore.Check(AContext, AContext.ControllerQualifiedClassName, AContext.ActionName);
      Result := ANext();
    end;
end;

function JWTCookieLoginHTTPFilter(const AAuthenticationHandler: IMVCAuthenticationHandler;
  const AClaimsSetup: TJWTClaimsSetup; const ASigner: IJWTSigner; const ACookie: TMVCAuthCookieOptions;
  const ALoginURLSegment, ALogoutURLSegment: string; const ALeewaySeconds: Cardinal): TMVCHTTPFilter;
var
  lCore: TMVCJWTCookieAuthenticationCore;
begin
  lCore := NewJWTCookieCore(AAuthenticationHandler, AClaimsSetup, ASigner, ACookie, ALoginURLSegment,
    ALogoutURLSegment, [TJWTCheckableClaim.ExpirationTime], ALeewaySeconds);
  Result :=
    procedure(const AContext: TWebContext; const ANext: TMVCHTTPFilterNext)
    var
      lToken: string;
      lExpires: TDateTime;
    begin
      if lCore.JWT.TryLogin(AContext, lToken, lExpires) then
        lCore.RenderLoginResponse(AContext, lToken, lExpires)
      else if SameText(AContext.Request.PathInfo, lCore.LogoutURLSegment) then
        lCore.RenderLogoutResponse(AContext)
      else
        ANext();
    end;
end;

function JWTBlackListHTTPFilter(const AOnAcceptToken: TMVCOnAcceptTokenProc;
  const AOnNewJWTToBlackList: TMVCOnNewJWTToBlackList;
  const ABlackListRequestURLSegment, AAccessTokenParamName: string): TMVCHTTPFilter;
var
  lCore: TMVCJWTBlackListCore;
begin
  // TMVCJWTBlackListMiddleware only asserts these, and Assert is off in a release build
  if not (Assigned(AOnAcceptToken) and Assigned(AOnNewJWTToBlackList)) then
    raise EMVCException.Create('JWTAuthenticationBundle: the blacklist needs both OnAcceptToken and OnNewJWTToBlackList');
  if ABlackListRequestURLSegment.Trim = '' then
    raise EMVCException.Create('JWTAuthenticationBundle: the blacklist URL is required');
  lCore := TMVCJWTBlackListCore.Create(AOnAcceptToken, AOnNewJWTToBlackList, ABlackListRequestURLSegment,
    AAccessTokenParamName);
  Result :=
    procedure(const AContext: TWebContext; const ANext: TMVCHTTPFilterNext)
    var
      lHandled: Boolean;
    begin
      lHandled := False;
      lCore.BeforeRouting(AContext, lHandled);
      if not lHandled then
        ANext();
    end;
end;

// AOnNewJWTToBlackList, then the revocation of the refresh token in the JSON body of the logout, if any
function RevokingRefreshToken(const AOnNewJWTToBlackList: TMVCOnNewJWTToBlackList;
  const AStore: IMVCRefreshTokenStore): TMVCOnNewJWTToBlackList;
begin
  Result :=
    procedure(AContext: TWebContext; AJWTToken: string)
    var
      lBody: TJDOJsonObject;
      lRefreshToken: string;
    begin
      AOnNewJWTToBlackList(AContext, AJWTToken);
      lRefreshToken := '';
      lBody := StrToJSONObject(AContext.Request.Body); // nil when the body is not a JSON object
      try
        // S[] raises on an object or an array: after the blacklist that would be a 500
        if Assigned(lBody) and (lBody.Types['refresh_token'] = jdtString) then
          lRefreshToken := lBody.S['refresh_token'];
      finally
        lBody.Free;
      end;
      if lRefreshToken <> '' then
        AStore.Revoke(lRefreshToken);
    end;
end;

function CustomAuthEndpointFilter(const AAuthenticationHandler: IMVCAuthenticationHandler;
  const ALoginURL: string): TMVCEndpointFilter;
var
  lCore: TMVCCustomAuthenticationCore;
begin
  lCore := TMVCCustomAuthenticationCore.Create(FilterAuthHandler(AAuthenticationHandler), ALoginURL);
  Result :=
    function(const AContext: TWebContext; const ANext: TMVCEndpointFilterNext): IMVCResponse
    var
      lHandled: Boolean;
    begin
      lHandled := False;
      lCore.Check(AContext, AContext.ControllerQualifiedClassName, AContext.ActionName, lHandled);
      if lHandled then
        Exit(nil); // the answer is written: nothing else is rendered
      Result := ANext();
    end;
end;

function CustomAuthLoginHTTPFilter(const AAuthenticationHandler: IMVCAuthenticationHandler;
  const ALoginURL: string): TMVCHTTPFilter;
var
  lCore: TMVCCustomAuthenticationCore;
begin
  lCore := TMVCCustomAuthenticationCore.Create(AAuthenticationHandler, ALoginURL);
  Result :=
    procedure(const AContext: TWebContext; const ANext: TMVCHTTPFilterNext)
    var
      lHandled: Boolean;
    begin
      lHandled := False;
      lCore.BeforeRouting(AContext, lHandled);
      if not lHandled then
        ANext();
    end;
end;

// --- The bundles ------------------------------------------------------------

class function TMVCJWTAuthenticationOptions.Default: TMVCJWTAuthenticationOptions;
begin
  Result.Handler := nil;
  Result.ClaimsSetup := nil;
  Result.Secret := '';
  Result.Signer := nil;
  Result.LoginURL := '/login';
  Result.ClaimsToCheck := [TJWTCheckableClaim.ExpirationTime];
  Result.LeewaySeconds := 300;
  Result.HMACAlgorithm := HMAC_HS512;
  Result.AccessTokenParamName := '';
  Result.OnAcceptToken := nil;
  Result.OnNewJWTToBlackList := nil;
  Result.BlackListURL := '/logout';
  Result.RefreshToken := TMVCRefreshTokenConfig.Default(''); // the bundle gives it its secret or signer
  Result.RefreshTokenStore := nil;
end;

class function TMVCJWTCookieAuthenticationOptions.Default: TMVCJWTCookieAuthenticationOptions;
begin
  Result.Handler := nil;
  Result.ClaimsSetup := nil;
  Result.Secret := '';
  Result.Signer := nil;
  Result.LoginURL := '/login';
  Result.LogoutURL := '/logout';
  Result.ClaimsToCheck := [TJWTCheckableClaim.ExpirationTime];
  Result.LeewaySeconds := 300;
  Result.HMACAlgorithm := HMAC_HS512;
  Result.Cookie := TMVCAuthCookieOptions.Default;
  Result.LoginAllowedOrigins := nil;
end;

function JWTAuthenticationBundle(const AHandler: IMVCAuthenticationHandler; const AClaimsSetup: TJWTClaimsSetup;
  const ASecret: string; const ALoginURL: string): IMVCFilterBundle;
var
  lOptions: TMVCJWTAuthenticationOptions;
begin
  lOptions := TMVCJWTAuthenticationOptions.Default;
  lOptions.Handler := AHandler;
  lOptions.ClaimsSetup := AClaimsSetup;
  lOptions.Secret := ASecret;
  lOptions.LoginURL := ALoginURL;
  Result := JWTAuthenticationBundle(lOptions);
end;

function JWTAuthenticationBundle(const AOptions: TMVCJWTAuthenticationOptions): IMVCFilterBundle;
const
  NAME = 'JWTAuthenticationBundle';
var
  lSigner: IJWTSigner;
  lHTTP: TArray<TMVCHTTPFilter>;
  lRefresh: TMVCRefreshTokenConfig;
  lOnNewJWTToBlackList: TMVCOnNewJWTToBlackList;
  lDocumentedLoginURL: string;
begin
  lSigner := BundleSigner(AOptions.Secret, AOptions.Signer, AOptions.HMACAlgorithm, NAME);
  // without a handler the login would give a token to anybody
  CheckHandler(AOptions.Handler, NAME);
  lHTTP := nil;
  lDocumentedLoginURL := AOptions.LoginURL;
  if Assigned(AOptions.OnAcceptToken) or Assigned(AOptions.OnNewJWTToBlackList) then
  begin
    lOnNewJWTToBlackList := AOptions.OnNewJWTToBlackList;
    if Assigned(AOptions.RefreshTokenStore) and Assigned(lOnNewJWTToBlackList) then
      lOnNewJWTToBlackList := RevokingRefreshToken(lOnNewJWTToBlackList, AOptions.RefreshTokenStore);
    lHTTP := lHTTP + [JWTBlackListHTTPFilter(AOptions.OnAcceptToken, lOnNewJWTToBlackList,
      AOptions.BlackListURL, AOptions.AccessTokenParamName)];
  end;
  if Assigned(AOptions.RefreshTokenStore) then
  begin
    if Assigned(AOptions.ClaimsSetup) then
      raise EMVCException.Create(NAME + ': with the refresh token the access tokens carry the claims of ' +
        'RefreshToken only; leave ClaimsSetup nil');
    lRefresh := AOptions.RefreshToken;
    lRefresh.Signer := lSigner; // the very signer the check verifies with
    lDocumentedLoginURL := ''; // it answers the pair: the Swagger document describes {"token": ...}
    if AOptions.LoginURL <> '' then
      lHTTP := lHTTP + [JWTRefreshLoginHTTPFilter(AOptions.Handler, lSigner, AOptions.LoginURL,
        AOptions.LeewaySeconds, lRefresh, AOptions.RefreshTokenStore)];
    lHTTP := lHTTP + [RefreshTokenHTTPFilter(lRefresh, AOptions.RefreshTokenStore)];
  end
  else if AOptions.LoginURL = '' then
  begin
    if Assigned(AOptions.ClaimsSetup) then
      raise EMVCException.Create(NAME + ': without a login (LoginURL empty) the bundle issues no token; ' +
        'leave ClaimsSetup nil');
  end
  else
  begin
    CheckClaimsSetup(AOptions.ClaimsSetup, NAME);
    lHTTP := lHTTP + [JWTLoginHTTPFilter(AOptions.Handler, AOptions.ClaimsSetup, lSigner, AOptions.LoginURL,
      AOptions.LeewaySeconds)];
  end;
  Result := TMVCJWTAuthenticationBundle.Create(lHTTP, [JWTEndpointFilter(AOptions.Handler, lSigner,
    AOptions.ClaimsToCheck, AOptions.LeewaySeconds, AOptions.AccessTokenParamName)], lDocumentedLoginURL);
end;

function JWTCookieAuthenticationBundle(const AHandler: IMVCAuthenticationHandler; const AClaimsSetup: TJWTClaimsSetup;
  const ASecret: string; const ACookie: TMVCAuthCookieOptions; const ALoginURL: string;
  const ALogoutURL: string): IMVCFilterBundle;
var
  lOptions: TMVCJWTCookieAuthenticationOptions;
begin
  lOptions := TMVCJWTCookieAuthenticationOptions.Default;
  lOptions.Handler := AHandler;
  lOptions.ClaimsSetup := AClaimsSetup;
  lOptions.Secret := ASecret;
  lOptions.Cookie := ACookie;
  lOptions.LoginURL := ALoginURL;
  lOptions.LogoutURL := ALogoutURL;
  Result := JWTCookieAuthenticationBundle(lOptions);
end;

function JWTCookieAuthenticationBundle(const AOptions: TMVCJWTCookieAuthenticationOptions): IMVCFilterBundle;
const
  NAME = 'JWTCookieAuthenticationBundle';
var
  lSigner: IJWTSigner;
  lHTTP: TArray<TMVCHTTPFilter>;
begin
  lSigner := BundleSigner(AOptions.Secret, AOptions.Signer, AOptions.HMACAlgorithm, NAME);
  CheckHandler(AOptions.Handler, NAME);
  CheckClaimsSetup(AOptions.ClaimsSetup, NAME);
  // the login sets the cookie; and the origin check on '' would cover every path
  if AOptions.LoginURL.Trim = '' then
    raise EMVCException.Create(NAME + ': the login URL is required');
  lHTTP := nil;
  if Length(AOptions.LoginAllowedOrigins) > 0 then
    lHTTP := [AllowedOriginHTTPFilter(AOptions.LoginAllowedOrigins, AOptions.LoginURL)];
  lHTTP := lHTTP + [JWTCookieLoginHTTPFilter(AOptions.Handler, AOptions.ClaimsSetup, lSigner, AOptions.Cookie,
    AOptions.LoginURL, AOptions.LogoutURL, AOptions.LeewaySeconds)];
  Result := TMVCFilterBundle.Create(lHTTP, [JWTCookieEndpointFilter(AOptions.Handler, lSigner, AOptions.Cookie,
    AOptions.ClaimsToCheck, AOptions.LeewaySeconds)]);
end;

function BasicAuthenticationBundle(const AHandler: IMVCAuthenticationHandler;
  const ARealm: string): IMVCFilterBundle;
begin
  Result := TMVCFilterBundle.Create([], [BasicAuthEndpointFilter(AHandler, ARealm)]);
end;

function CustomAuthenticationBundle(const AHandler: IMVCAuthenticationHandler;
  const ALoginURL: string): IMVCFilterBundle;
begin
  CheckHandler(AHandler, 'CustomAuthenticationBundle');
  Result := TMVCFilterBundle.Create([CustomAuthLoginHTTPFilter(AHandler, ALoginURL)],
    [CustomAuthEndpointFilter(AHandler, ALoginURL)]);
end;

end.
