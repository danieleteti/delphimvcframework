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
// INTERNAL - not an API: shared by the authentication filters, the deprecated
// middleware and the Swagger document; goes away with them in 4.0. The
// mechanisms of the bundles of MVCFramework.Filters.Authentication (login,
// check, cookie, blacklist, custom authentication), which the deprecated
// middleware of MVCFramework.Middleware.JWT and
// MVCFramework.Middleware.Authentication are built on too.

unit MVCFramework.Filters.Authentication.Internal;

{$I dmvcframework.inc}

interface

uses
  MVCFramework,
  MVCFramework.JWT,                            // TJWT, TJWTClaimsSetup, TJWTCheckableClaims, IJWTSigner, TMVCJWTCookieSameSite
  MVCFramework.Filters.Authentication;         // TMVCOnAcceptTokenProc, TMVCOnNewJWTToBlackList

type
  // A JWTAuthenticationBundle, as the Swagger document (MVCFramework.Filters.Swagger.Internal) finds it among
  // TMVCEngine.FilterBundles: the bearer scheme, and the login documented as the one of the JWT middleware.
  IMVCJWTAuthenticationBundle = interface(IMVCFilterBundle)
    ['{6C1E2B57-0F4D-4E8A-9B3C-7A5D2E81F094}']
    // '': no login answering {"token": ...} (none, or the refresh-token login answering the pair)
    function LoginURL: string;
  end;

  // The JWT in the Authorization header: JWTAuthenticationBundle, TMVCJWTAuthenticationMiddleware.
  TMVCJWTAuthenticationCore = record
    Handler: IMVCAuthenticationHandler;         // nil: every action needs a token, and every token is authorized
    ClaimsSetup: TJWTClaimsSetup;               // the token of the login
    Secret: string;                             // signs with HS512, when Signer is nil
    Signer: IJWTSigner;
    LoginURLSegment: string;
    ClaimsToCheck: TJWTCheckableClaims;
    LeewaySeconds: Cardinal;
    AccessTokenParamName: string;               // '': the token from the Authorization header only
    // logs a warning for empty AClaimsToCheck: the tokens would never expire
    constructor Create(const AHandler: IMVCAuthenticationHandler; const AClaimsSetup: TJWTClaimsSetup;
      const ASecret: string; const ASigner: IJWTSigner; const ALoginURLSegment: string;
      const AClaimsToCheck: TJWTCheckableClaims; const ALeewaySeconds: Cardinal;
      const AAccessTokenParamName: string);
    function CreateJWT: TJWT;
    // The login of JWTAuthenticationBundle, on a request to LoginURLSegment (False for any other). Missing or
    // refused credentials raise EMVCJWTException (401); otherwise Context.LoggedUser is the user, AToken the
    // new token and AExpires its expiration. Nothing is rendered.
    function TryLogin(const AContext: TWebContext; out AToken: string; out AExpires: TDateTime): Boolean;
    // The check of an action: AuthenticationRequired, then LoadOptionalToken or Authorize with the token of
    // RequestToken. A refreshed token goes in the Authorization header of the response.
    procedure Check(const AContext: TWebContext; const AControllerQualifiedClassName, AActionName: string);
    // False when Handler.OnRequest makes the action public
    function AuthenticationRequired(const AContext: TWebContext; const AControllerQualifiedClassName,
      AActionName: string): Boolean;
    // A public action: Context.LoggedUser from AToken when it is valid. No token, or an invalid one, is no error.
    procedure LoadOptionalToken(const AContext: TWebContext; const AToken: string);
    // Context.LoggedUser from AToken. EMVCJWTException 401 without a valid token carrying "username", 403 when
    // Handler.OnAuthorization refuses it. For a token about to expire the result is the token with the
    // expiration extended to AExpires; otherwise ''.
    function Authorize(const AContext: TWebContext; const AToken, AControllerQualifiedClassName,
      AActionName: string; out AExpires: TDateTime): string;
    // The token of `Authorization: Bearer <token>` or, when the header is missing, of the parameter
    // AAccessTokenParamName ('': none).
    class function RequestToken(const AContext: TWebContext; const AAccessTokenParamName: string): string; static;
    class function NeedsToBeExtended(const AJWT: TJWT): Boolean; static;
    class procedure ExtendExpirationTime(const AJWT: TJWT); static;
    // AJSON as the body of the response, in AContentCharset
    class procedure RenderJSON(const AContext: TWebContext; const AJSON, AMediaType, AContentCharset: string); static;
    // {"token": AToken} as the body, as the login answers
    class procedure RenderTokenJSON(const AContext: TWebContext; const AToken: string); static;
  end;

  // The JWT in a cookie: JWTCookieAuthenticationBundle, TMVCJWTCookieAuthenticationMiddleware.
  TMVCJWTCookieAuthenticationCore = record
    JWT: TMVCJWTAuthenticationCore;             // the token, its login and its check; AccessTokenParamName is not read
    LogoutURLSegment: string;
    CookieName: string;                         // 'jwt_token'
    CookieSecure: Boolean;                      // True
    CookieSameSite: TMVCJWTCookieSameSite;      // ssStrict
    CookiePath: string;                         // '/'
    CookieDomain: string;                       // '': the host of the request
    constructor Create(const AHandler: IMVCAuthenticationHandler; const AClaimsSetup: TJWTClaimsSetup;
      const ASecret: string; const ASigner: IJWTSigner; const ALoginURLSegment, ALogoutURLSegment: string;
      const AClaimsToCheck: TJWTCheckableClaims; const ALeewaySeconds: Cardinal);
    // both raise for SameSite=None without Secure
    procedure SetCookieSecure(const ASecure: Boolean);
    procedure SetCookieSameSite(const ASameSite: TMVCJWTCookieSameSite);
    procedure SetCookie(const AContext: TWebContext; const AToken: string; const AExpires: TDateTime);
    procedure InvalidateCookie(const AContext: TWebContext);
    function GetTokenFromCookie(const AContext: TWebContext): string;
    // the cookie, and {"token": AToken} as the body
    procedure RenderLoginResponse(const AContext: TWebContext; const AToken: string; const AExpires: TDateTime);
    // the cookie expired, and {"message": "Successful logout"} as the body
    procedure RenderLogoutResponse(const AContext: TWebContext);
    // The check of TMVCJWTAuthenticationCore with the token of the cookie: a refreshed token replaces the
    // cookie and the response carries X-JWT-Refreshed: true.
    procedure Check(const AContext: TWebContext; const AControllerQualifiedClassName, AActionName: string);
  end;

  // The blacklist: JWTAuthenticationBundle with OnAcceptToken and OnNewJWTToBlackList, TMVCJWTBlackListMiddleware.
  TMVCJWTBlackListCore = record
    OnAcceptToken: TMVCOnAcceptTokenProc;
    OnNewJWTToBlackList: TMVCOnNewJWTToBlackList;
    BlackListRequestURLSegment: string;
    AccessTokenParamName: string;               // as the one of the check, or a token sent there is never checked
    constructor Create(const AOnAcceptToken: TMVCOnAcceptTokenProc;
      const AOnNewJWTToBlackList: TMVCOnNewJWTToBlackList;
      const ABlackListRequestURLSegment, AAccessTokenParamName: string);
    // A request to BlackListRequestURLSegment hands its token to OnNewJWTToBlackList and answers 204 (AHandled;
    // EMVCException 400 without a token). On any other request a token refused by OnAcceptToken raises
    // EMVCJWTException 403.
    procedure BeforeRouting(const AContext: TWebContext; var AHandled: Boolean);
  end;

  // The custom authentication on a session: CustomAuthenticationBundle, TMVCCustomAuthenticationMiddleware.
  TMVCCustomAuthenticationCore = record
    Handler: IMVCAuthenticationHandler;
    LoginURL: string;                           // in lower case
    constructor Create(const AHandler: IMVCAuthenticationHandler; const ALoginURL: string);
    // the login (POST with a JSON body) and the logout (DELETE) at LoginURL
    procedure BeforeRouting(const AContext: TWebContext; var AHandled: Boolean);
    // 401 without the identity of the session, 403 when Handler.OnAuthorization refuses it
    procedure Check(const AContext: TWebContext; const AControllerQualifiedClassName, AActionName: string;
      var AHandled: Boolean);
    procedure DoLogin(const AContext: TWebContext; var AHandled: Boolean);
    procedure DoLogout(const AContext: TWebContext; var AHandled: Boolean);
    // JSON, or HTML for a browser, with X-LOGIN-URL / X-LOGIN-METHOD
    procedure SendResponse(const AContext: TWebContext; var AHandled: Boolean; const AHttpStatus: Word);
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.DateUtils,
  System.Math,
  System.StrUtils,
  System.NetEncoding,
  System.Generics.Collections,
  System.JSON,                                 // the body of the custom login, parsed as the middleware always did
  Web.HTTPApp,                                 // TCookie
  JsonDataObjects,                             // TJDOJsonObject
  MVCFramework.Commons,
  MVCFramework.Logger;                         // LogW

const
  CONTENT_HTML_FORMAT = '<html><body><h1>%s</h1><p>%s</p></body></html>';

// The login time of a token: its "iat", or now for a token without it. An absent claim reads as
// 0001-01-01, and LoggedUser.IsValid wants a time after 0: an accepted token would leave it False.
function TokenLoginTime(const AJWT: TJWT): TDateTime;
begin
  Result := AJWT.Claims.IssuedAt;
  if Result <= 0 then
    Result := Now;
end;

procedure LoadLoggedUser(const AContext: TWebContext; const AJWT: TJWT);
begin
  AContext.LoggedUser.Clear;
  AContext.LoggedUser.UserName := AJWT.CustomClaims['username'];
  AContext.LoggedUser.Roles.AddRange(AJWT.CustomClaims['roles'].Split([',']));
  AContext.LoggedUser.LoggedSince := TokenLoginTime(AJWT);
  AContext.LoggedUser.CustomData := AJWT.CustomClaims.AsCustomData;
end;

// the jwtusername / jwtpassword headers or form fields, a JSON body, or Authorization: Basic
procedure ReadLoginCredentials(const AContext: TWebContext; out AUserName, APassword: string);
var
  lBasicAuthHeader: string;
  lBasicAuthParts: TArray<string>;
  lJObj: TJDOJsonObject;
begin
  lBasicAuthHeader := AContext.Request.Headers[TMVCJWTDefaults.AUTHORIZATION_HEADER];
  if lBasicAuthHeader.IsEmpty then
  begin
    // read from headers
    AUserName := TNetEncoding.URL.Decode(AContext.Request.Headers[TMVCJWTDefaults.USERNAME_HEADER]);
    APassword := TNetEncoding.URL.Decode(AContext.Request.Headers[TMVCJWTDefaults.PASSWORD_HEADER]);

    // read from content
    if AUserName.IsEmpty and not SameText(AContext.Request.ContentMediaType, TMVCMediaType.APPLICATION_JSON) then
    begin
      AUserName := AContext.Request.ContentParam(TMVCJWTDefaults.USERNAME_HEADER);
      APassword := AContext.Request.ContentParam(TMVCJWTDefaults.PASSWORD_HEADER);
    end;

    // read from json content
    if AUserName.IsEmpty then
    begin
      lJObj := StrToJSONObject(AContext.Request.Body, False);
      try
        if Assigned(lJObj) then
        begin
          AUserName := lJObj.S[TMVCJWTDefaults.USERNAME_HEADER];
          APassword := lJObj.S[TMVCJWTDefaults.PASSWORD_HEADER];
          if AUserName.IsEmpty then
          begin
            AUserName := lJObj.S['username'];
            APassword := lJObj.S['password'];
          end;
        end;
      finally
        lJObj.Free;
      end;
    end;

    if (AUserName.IsEmpty) or (APassword.IsEmpty) then
      raise EMVCJWTException.Create(HTTP_STATUS.Unauthorized, 'Username and password required');
  end
  else
  begin
    if not lBasicAuthHeader.StartsWith('basic', True) then
      raise EMVCJWTException.Create(HTTP_STATUS.Unauthorized, 'Invalid authorization type');

    lBasicAuthHeader := lBasicAuthHeader.Remove(0, 'basic'.Length).Trim;
    lBasicAuthParts := TBase64Encoding.Base64.Decode(lBasicAuthHeader).Split([':']);

    if Length(lBasicAuthParts) <> 2 then
      raise EMVCJWTException.Create(HTTP_STATUS.Unauthorized, 'Invalid authorization type');

    AUserName := lBasicAuthParts[0];
    APassword := lBasicAuthParts[1];
  end;
end;

{ TMVCJWTAuthenticationCore }

constructor TMVCJWTAuthenticationCore.Create(const AHandler: IMVCAuthenticationHandler;
  const AClaimsSetup: TJWTClaimsSetup; const ASecret: string; const ASigner: IJWTSigner;
  const ALoginURLSegment: string; const AClaimsToCheck: TJWTCheckableClaims; const ALeewaySeconds: Cardinal;
  const AAccessTokenParamName: string);
begin
  Handler := AHandler;
  ClaimsSetup := AClaimsSetup;
  Secret := ASecret;
  Signer := ASigner;
  LoginURLSegment := ALoginURLSegment;
  ClaimsToCheck := AClaimsToCheck;
  LeewaySeconds := ALeewaySeconds;
  AccessTokenParamName := AAccessTokenParamName;
  // Insecure default: an empty claims-to-check set means exp/nbf/iat are NOT
  // validated, so expired tokens are accepted indefinitely. Warn loudly.
  // (Enforcing by default would reject tokens issued without these claims, an
  // API break deferred to v4.)
  if AClaimsToCheck = [] then
    LogW('JWT middleware constructed with an empty claims-to-check set: token expiration (exp) is NOT validated, tokens never expire. Pass [TJWTCheckableClaim.ExpirationTime] to enforce expiration.');
end;

function TMVCJWTAuthenticationCore.CreateJWT: TJWT;
begin
  if Assigned(Signer) then
    Result := TJWT.Create(Signer, LeewaySeconds)
  else
    Result := TJWT.Create(Secret, LeewaySeconds);
end;

class function TMVCJWTAuthenticationCore.NeedsToBeExtended(const AJWT: TJWT): Boolean;
var
  lWillExpireIn: Int64;
begin
  lWillExpireIn := SecondsBetween(Now, AJWT.Claims.ExpirationTime);
  Result := lWillExpireIn <= AJWT.LiveValidityWindowInSeconds;
end;

class procedure TMVCJWTAuthenticationCore.ExtendExpirationTime(const AJWT: TJWT);
begin
  AJWT.Claims.ExpirationTime := Max(AJWT.Claims.ExpirationTime, Now) +
    (AJWT.LeewaySeconds + AJWT.LiveValidityWindowInSeconds) * OneSecond;
end;

class procedure TMVCJWTAuthenticationCore.RenderJSON(const AContext: TWebContext;
  const AJSON, AMediaType, AContentCharset: string);
var
  lEncoding: TEncoding;
  lContentType: string;
begin
  lContentType := BuildContentType(AMediaType, AContentCharset);
  AContext.Response.ContentType := lContentType;

  lEncoding := TEncoding.GetEncoding(AContentCharset);
  try
    AContext.Response.SetContentStream(TBytesStream.Create(TEncoding.Convert(TEncoding.Default, lEncoding,
      TEncoding.Default.GetBytes(AJSON))), lContentType);
  finally
    lEncoding.Free;
  end;
end;

class procedure TMVCJWTAuthenticationCore.RenderTokenJSON(const AContext: TWebContext; const AToken: string);
var
  lJSON: TJDOJsonObject;
begin
  lJSON := TJDOJsonObject.Create;
  try
    lJSON.S['token'] := AToken;
    TMVCJWTAuthenticationCore.RenderJSON(AContext, lJSON.ToJSON, TMVCMediaType.APPLICATION_JSON,
      TMVCConstants.DEFAULT_CONTENT_CHARSET);
  finally
    lJSON.Free;
  end;
end;

class function TMVCJWTAuthenticationCore.RequestToken(const AContext: TWebContext;
  const AAccessTokenParamName: string): string;
var
  lAuthHeader: string;
begin
  Result := '';
  lAuthHeader := AContext.Request.Headers[TMVCJWTDefaults.AUTHORIZATION_HEADER];
  if not lAuthHeader.IsEmpty then
  begin
    // retrieve the token from the "authentication Bearer" header
    if lAuthHeader.Substring(0, TMVCJWTDefaults.AUTH_SCHEMA.Length).ToLower = 'bearer' then
      Result := Trim(TNetEncoding.URL.Decode(lAuthHeader.Remove(0, TMVCJWTDefaults.AUTH_SCHEMA.Length).Trim));
  end
  else if not AAccessTokenParamName.IsEmpty then
    // the bare token, not "Bearer <token>"
    Result := Trim(TNetEncoding.URL.Decode(AContext.Request.Params[AAccessTokenParamName].Trim));
end;

function TMVCJWTAuthenticationCore.TryLogin(const AContext: TWebContext; out AToken: string;
  out AExpires: TDateTime): Boolean;
var
  lUserName: string;
  lPassword: string;
  lRolesList: TList<string>;
  lSessionData: TSessionData;
  lIsValid: Boolean;
  lJWT: TJWT;
  lCustomPair: TPair<string, string>;
begin
  Result := SameText(AContext.Request.PathInfo, LoginURLSegment);
  if not Result then
    Exit;
  ReadLoginCredentials(AContext, lUserName, lPassword);

  // check the authorization for the requested resource
  lRolesList := TList<string>.Create;
  try
    lSessionData := TSessionData.Create;
    try
      if Assigned(Handler) then
      begin
        lIsValid := False; // a var parameter: a handler that sets it only to accept must not read garbage
        Handler.OnAuthentication(AContext, lUserName, lPassword, lRolesList, lIsValid, lSessionData);
        if not lIsValid then
          raise EMVCJWTException.Create(HTTP_STATUS.Unauthorized, 'Unauthorized');
      end;

      lJWT := CreateJWT;
      try
        // let's user config claims and custom claims
        if not Assigned(ClaimsSetup) then
          raise EMVCJWTException.Create('SetupJWTClaims not set');
        lJWT.Data := AContext.Request;
        ClaimsSetup(lJWT);
        AExpires := lJWT.Claims.ExpirationTime;

        // these claims are mandatory and managed by the middleware
        if not lJWT.CustomClaims['username'].IsEmpty then
          raise EMVCJWTException.Create
            ('Custom claim "username" is reserved and cannot be modified in the JWT setup');

        if not lJWT.CustomClaims['roles'].IsEmpty then
          raise EMVCJWTException.Create('Custom claim "roles" is reserved and cannot be modified in the JWT setup');

        lJWT.CustomClaims['username'] := lUserName;
        lJWT.CustomClaims['roles'] := string.Join(',', lRolesList.ToArray);

        if lJWT.LiveValidityWindowInSeconds > 0 then
          if NeedsToBeExtended(lJWT) then
          begin
            ExtendExpirationTime(lJWT);
            AExpires := lJWT.Claims.ExpirationTime;
          end;

        // setup the current logged user from the JWT
        AContext.LoggedUser.Clear;
        AContext.LoggedUser.Roles.AddRange(lRolesList);
        AContext.LoggedUser.UserName := lJWT.CustomClaims['username'];
        AContext.LoggedUser.LoggedSince := TokenLoginTime(lJWT);
        AContext.LoggedUser.Realm := lJWT.Claims.Subject;

        if lSessionData.Count > 0 then
        begin
          AContext.LoggedUser.CustomData := TMVCCustomData.Create;
          for lCustomPair in lSessionData do
          begin
            AContext.LoggedUser.CustomData.AddOrSetValue(lCustomPair.Key, lCustomPair.Value);
            if not lJWT.CustomClaims.Items[lCustomPair.Key].IsEmpty then
              raise EMVCJWTException.CreateFmt('JWT Error: "%s" is a reserved key name', [lCustomPair.Key]);
            lJWT.CustomClaims.Items[lCustomPair.Key] := lCustomPair.Value;
          end;
        end;

        AToken := lJWT.GetToken;
      finally
        lJWT.Free;
      end;
    finally
      lSessionData.Free;
    end;
  finally
    lRolesList.Free;
  end;
end;

function TMVCJWTAuthenticationCore.AuthenticationRequired(const AContext: TWebContext;
  const AControllerQualifiedClassName, AActionName: string): Boolean;
begin
  { Deny by default. The parameter is declared `var`, not `out`, and Delphi
    zeroes nothing: a handler that leaves it unassigned makes the caller read
    stack garbage, and False there skips authentication entirely. }
  Result := True;
  if Assigned(Handler) then
    Handler.OnRequest(AContext, AControllerQualifiedClassName, AActionName, Result);
end;

procedure TMVCJWTAuthenticationCore.LoadOptionalToken(const AContext: TWebContext; const AToken: string);
var
  lJWT: TJWT;
  lErrorMsg: string;
begin
  { An action that doesn't require authentication still loads the token, if
    any: Context.LoggedUser.IsValid then tells a call of an authenticated user
    from an anonymous one. }
  if AToken.IsEmpty then
    Exit;
  lJWT := CreateJWT;
  try
    lJWT.RegClaimsToChecks := ClaimsToCheck;
    if lJWT.LoadToken(AToken, lErrorMsg) then
      LoadLoggedUser(AContext, lJWT); // only a token still valid
  finally
    lJWT.Free;
  end;
end;

function TMVCJWTAuthenticationCore.Authorize(const AContext: TWebContext; const AToken,
  AControllerQualifiedClassName, AActionName: string; out AExpires: TDateTime): string;
var
  lJWT: TJWT;
  lErrorMsg: string;
  lIsAuthorized: Boolean;
begin
  Result := '';
  AExpires := 0;
  lJWT := CreateJWT;
  try
    lJWT.RegClaimsToChecks := ClaimsToCheck;
    if AToken.IsEmpty then
      raise EMVCJWTException.Create(HTTP_STATUS.Unauthorized, 'Authorization Required');
    if not lJWT.LoadToken(AToken, lErrorMsg) then
      raise EMVCJWTException.Create(HTTP_STATUS.Unauthorized, lErrorMsg);

    if lJWT.CustomClaims['username'].IsEmpty then
      raise EMVCJWTException.Create(HTTP_STATUS.Unauthorized, 'Invalid Token, Authorization Required');

    LoadLoggedUser(AContext, lJWT);

    if Assigned(Handler) then
    begin
      lIsAuthorized := False; // deny by default: a var parameter, as AuthenticationRequired
      Handler.OnAuthorization(AContext, AContext.LoggedUser.Roles, AControllerQualifiedClassName,
        AActionName, lIsAuthorized);
      if not lIsAuthorized then
        raise EMVCJWTException.Create(HTTP_STATUS.Forbidden, 'Authorization Forbidden');
    end;

    if lJWT.LiveValidityWindowInSeconds > 0 then
    begin
      if NeedsToBeExtended(lJWT) then
      begin
        ExtendExpirationTime(lJWT);
        AExpires := lJWT.Claims.ExpirationTime;
        Result := lJWT.GetToken;
      end;
    end;
  finally
    lJWT.Free;
  end;
end;

procedure TMVCJWTAuthenticationCore.Check(const AContext: TWebContext;
  const AControllerQualifiedClassName, AActionName: string);
var
  lRefreshed: string;
  lExpires: TDateTime;
begin
  if not AuthenticationRequired(AContext, AControllerQualifiedClassName, AActionName) then
    LoadOptionalToken(AContext, RequestToken(AContext, AccessTokenParamName))
  else
  begin
    lRefreshed := Authorize(AContext, RequestToken(AContext, AccessTokenParamName),
      AControllerQualifiedClassName, AActionName, lExpires);
    if not lRefreshed.IsEmpty then
      AContext.Response.SetCustomHeader(TMVCJWTDefaults.AUTHORIZATION_HEADER, 'Bearer ' + lRefreshed);
  end;
end;

{ TMVCJWTCookieAuthenticationCore }

constructor TMVCJWTCookieAuthenticationCore.Create(const AHandler: IMVCAuthenticationHandler;
  const AClaimsSetup: TJWTClaimsSetup; const ASecret: string; const ASigner: IJWTSigner;
  const ALoginURLSegment, ALogoutURLSegment: string; const AClaimsToCheck: TJWTCheckableClaims;
  const ALeewaySeconds: Cardinal);
begin
  // not TMVCJWTAuthenticationCore.Create: the cookie never warned about empty claims to check
  JWT.Handler := AHandler;
  JWT.ClaimsSetup := AClaimsSetup;
  JWT.Secret := ASecret;
  JWT.Signer := ASigner;
  JWT.LoginURLSegment := ALoginURLSegment;
  JWT.ClaimsToCheck := AClaimsToCheck;
  JWT.LeewaySeconds := ALeewaySeconds;
  JWT.AccessTokenParamName := '';
  LogoutURLSegment := ALogoutURLSegment;
  // Secure defaults
  CookieName := 'jwt_token';
  CookieSecure := True;
  CookieSameSite := ssStrict;
  CookiePath := '/';
  CookieDomain := '';
end;

procedure TMVCJWTCookieAuthenticationCore.SetCookieSecure(const ASecure: Boolean);
begin
  // SameSite=None requires Secure=True per browser requirements
  if (not ASecure) and (CookieSameSite = ssNone) then
    raise EMVCException.Create('Cannot set Secure=False when SameSite=None. ' +
      'Browsers require Secure=True for SameSite=None cookies.');
  CookieSecure := ASecure;
end;

procedure TMVCJWTCookieAuthenticationCore.SetCookieSameSite(const ASameSite: TMVCJWTCookieSameSite);
begin
  // SameSite=None requires Secure=True per browser requirements
  if (ASameSite = ssNone) and (not CookieSecure) then
    raise EMVCException.Create('SameSite=None requires Secure=True. ' +
      'Call SetCookieSecure(True) before setting SameSite=None.');
  CookieSameSite := ASameSite;
end;

procedure TMVCJWTCookieAuthenticationCore.SetCookie(const AContext: TWebContext; const AToken: string;
  const AExpires: TDateTime);
var
  lCookie: TCookie;
  lSameSite: string;
begin
  lCookie := AContext.Response.Cookies.Add;
  lCookie.Name := CookieName;
  lCookie.Value := AToken;
  lCookie.Path := CookiePath;
  lCookie.Expires := AExpires;
  lCookie.HttpOnly := True;  // Always True for XSS protection
  lCookie.Secure := CookieSecure;
  if not CookieDomain.IsEmpty then
    lCookie.Domain := CookieDomain;

  // Set SameSite attribute
  case CookieSameSite of
    ssStrict: lSameSite := 'Strict';
    ssLax: lSameSite := 'Lax';
    ssNone: lSameSite := 'None';
  end;
  {$IF CompilerVersion >= 35.0}  // TCookie.SameSite exists since 10.4.2, but CompilerVersion is 34.0 for the whole 10.4 line: gate at 11
  lCookie.SameSite := lSameSite;
  {$ENDIF}
end;

procedure TMVCJWTCookieAuthenticationCore.InvalidateCookie(const AContext: TWebContext);
begin
  // Set cookie with empty value and past expiration date
  SetCookie(AContext, '', EncodeDate(1970, 1, 1));
end;

function TMVCJWTCookieAuthenticationCore.GetTokenFromCookie(const AContext: TWebContext): string;
begin
  Result := Trim(TNetEncoding.URL.Decode(AContext.Request.Cookie(CookieName)));
end;

procedure TMVCJWTCookieAuthenticationCore.RenderLoginResponse(const AContext: TWebContext;
  const AToken: string; const AExpires: TDateTime);
begin
  SetCookie(AContext, AToken, AExpires);
  TMVCJWTAuthenticationCore.RenderTokenJSON(AContext, AToken);
end;

procedure TMVCJWTCookieAuthenticationCore.RenderLogoutResponse(const AContext: TWebContext);
const
  ReturnMessage = '{ "message": "Successful logout" }';
  ContentType = 'application/json; charset=UTF-8';
begin
  InvalidateCookie(AContext);
  AContext.Response.ContentType := ContentType;
  AContext.Response.SetContentStream(TBytesStream.Create(TEncoding.UTF8.GetBytes(ReturnMessage)), ContentType);
end;

procedure TMVCJWTCookieAuthenticationCore.Check(const AContext: TWebContext;
  const AControllerQualifiedClassName, AActionName: string);
var
  lRefreshed: string;
  lExpires: TDateTime;
begin
  if not JWT.AuthenticationRequired(AContext, AControllerQualifiedClassName, AActionName) then
    JWT.LoadOptionalToken(AContext, GetTokenFromCookie(AContext))
  else
  begin
    lRefreshed := JWT.Authorize(AContext, GetTokenFromCookie(AContext), AControllerQualifiedClassName,
      AActionName, lExpires);
    if not lRefreshed.IsEmpty then
    begin
      SetCookie(AContext, lRefreshed, lExpires);
      AContext.Response.SetCustomHeader('X-JWT-Refreshed', 'true');
    end;
  end;
end;

{ TMVCJWTBlackListCore }

constructor TMVCJWTBlackListCore.Create(const AOnAcceptToken: TMVCOnAcceptTokenProc;
  const AOnNewJWTToBlackList: TMVCOnNewJWTToBlackList;
  const ABlackListRequestURLSegment, AAccessTokenParamName: string);
begin
  OnAcceptToken := AOnAcceptToken;
  OnNewJWTToBlackList := AOnNewJWTToBlackList;
  BlackListRequestURLSegment := ABlackListRequestURLSegment;
  AccessTokenParamName := AAccessTokenParamName;
end;

procedure TMVCJWTBlackListCore.BeforeRouting(const AContext: TWebContext; var AHandled: Boolean);
var
  lAuthToken: string;
  lAccepted: Boolean;
begin
  lAuthToken := TMVCJWTAuthenticationCore.RequestToken(AContext, AccessTokenParamName);
  if SameText(AContext.Request.PathInfo, BlackListRequestURLSegment) then
  begin
    // add the token in the blacklist
    if lAuthToken.IsEmpty then
      raise EMVCException.Create(HTTP_STATUS.BadRequest, 'JWTToken required - cannot blacklist an unknown token');
    OnNewJWTToBlackList(AContext, lAuthToken);
    AContext.Response.StatusCode := HTTP_STATUS.NoContent;
    AHandled := True;
  end
  else if lAuthToken.IsEmpty then
    // no token, nothing to check against the blacklist
    AHandled := False
  else
  begin
    lAccepted := True;
    OnAcceptToken(AContext, lAuthToken, lAccepted);
    if not lAccepted then
      raise EMVCJWTException.Create(HTTP_STATUS.Forbidden, 'JWT not accepted');
  end;
end;

{ TMVCCustomAuthenticationCore }

constructor TMVCCustomAuthenticationCore.Create(const AHandler: IMVCAuthenticationHandler;
  const ALoginURL: string);
begin
  Handler := AHandler;
  LoginURL := ALoginURL.ToLower;
end;

procedure TMVCCustomAuthenticationCore.DoLogin(const AContext: TWebContext; var AHandled: Boolean);
var
  lJo: System.JSON.TJSONObject;
  lUserName, lPassword: string;
  lRolesList: TList<string>;
  lSessionPair: TPair<string, string>;
  lSessionData: TSessionData;
  lIsValid: Boolean;
begin
  AContext.SessionStop(False);
  AContext.LoggedUser.Clear;
  if not AContext.Request.HasBody then
  begin
    AHandled := True;
    AContext.Response.StatusCode := HTTP_STATUS.BadRequest;
    AContext.Response.ContentType := TMVCMediaType.APPLICATION_JSON;
    AContext.Response.Content :=
      '{"status":"error", "message":"username and password are mandatory in the body request as json object"}';
    Exit;
  end;

  lJo := System.JSON.TJSONObject.ParseJSONValue(AContext.Request.Body) as System.JSON.TJSONObject;
  try
    if not Assigned(lJo) then
    begin
      AHandled := True;
      SendResponse(AContext, AHandled, HTTP_STATUS.BadRequest);
      Exit;
    end;

    lUserName := EmptyStr;
    if (lJo.Get('username') <> nil) then
      lUserName := lJo.Get('username').JsonValue.Value;

    lPassword := EmptyStr;
    if (lJo.Get('password') <> nil) then
      lPassword := lJo.Get('password').JsonValue.Value;

    if lUserName.IsEmpty or lPassword.IsEmpty then
    begin
      AHandled := True;
      SendResponse(AContext, AHandled, HTTP_STATUS.Unauthorized);
      Exit;
    end;

    lRolesList := TList<string>.Create;
    try
      lSessionData := TSessionData.Create;
      try
        lIsValid := False;
        Handler.OnAuthentication(AContext, lUserName, lPassword, lRolesList, lIsValid, lSessionData);
        if not lIsValid then
        begin
          SendResponse(AContext, AHandled, HTTP_STATUS.Unauthorized);
          Exit;
        end;

        AContext.LoggedUser.Roles.AddRange(lRolesList);
        AContext.LoggedUser.UserName := lUserName;
        AContext.LoggedUser.LoggedSince := Now;
        AContext.LoggedUser.Realm := 'custom';
        AContext.LoggedUser.SaveToSession(AContext.Session);

        for lSessionPair in lSessionData do
          AContext.Session[lSessionPair.Key] := lSessionPair.Value;

        AContext.Response.StatusCode := HTTP_STATUS.OK;
        AContext.Response.CustomHeaders.Values['X-LOGOUT-URL'] := LoginURL;
        AContext.Response.CustomHeaders.Values['X-LOGOUT-METHOD'] := 'DELETE';
        AContext.Response.ContentType := TMVCMediaType.APPLICATION_JSON;
        AContext.Response.Content := '{"status":"OK"}';

        AHandled := True;
      finally
        lSessionData.Free;
      end;
    finally
      lRolesList.Free;
    end;
  finally
    lJo.Free;
  end;
end;

procedure TMVCCustomAuthenticationCore.DoLogout(const AContext: TWebContext; var AHandled: Boolean);
begin
  AContext.SessionStop(False);
  SendResponse(AContext, AHandled, HTTP_STATUS.OK);
end;

procedure TMVCCustomAuthenticationCore.Check(const AContext: TWebContext;
  const AControllerQualifiedClassName, AActionName: string; var AHandled: Boolean);
var
  lIsValid: Boolean;
  lIsAuthorized: Boolean;
  lAuthRequired: Boolean;
begin
  { `var`, not `out`: an unassigned lAuthRequired would be stack garbage, and
    False there skips authentication altogether. }
  lAuthRequired := True;
  Handler.OnRequest(AContext, AControllerQualifiedClassName, AActionName, lAuthRequired);
  if not lAuthRequired then
  begin
    AHandled := False;
    Exit;
  end;

  AContext.LoggedUser.LoadFromSession(AContext.Session);
  lIsValid := AContext.LoggedUser.IsValid;
  if not lIsValid then
  begin
    AContext.SessionStop(False);
    SendResponse(AContext, AHandled, HTTP_STATUS.Unauthorized);
    Exit;
  end;

  lIsAuthorized := False;
  Handler.OnAuthorization(AContext, AContext.LoggedUser.Roles, AControllerQualifiedClassName,
    AActionName, lIsAuthorized);
  if lIsAuthorized then
    AHandled := False
  else
  begin
    if lIsValid then
      SendResponse(AContext, AHandled, HTTP_STATUS.Forbidden)
    else
      SendResponse(AContext, AHandled, HTTP_STATUS.Unauthorized);
  end;
end;

procedure TMVCCustomAuthenticationCore.BeforeRouting(const AContext: TWebContext; var AHandled: Boolean);
begin
  if (AContext.Request.PathInfo.ToLower = LoginURL) then
  begin
    AHandled := False;

    if (AContext.Request.HTTPMethod = httpPOST) and
      (AContext.Request.ContentType.StartsWith(TMVCMediaType.APPLICATION_JSON)) then
      DoLogin(AContext, AHandled);

    if (AContext.Request.HTTPMethod = httpDELETE) then
      DoLogout(AContext, AHandled);
  end;
end;

procedure TMVCCustomAuthenticationCore.SendResponse(const AContext: TWebContext; var AHandled: Boolean;
  const AHttpStatus: Word);
var
  lIsPositive: Boolean;
  lMsg: string;
begin
  AContext.LoggedUser.Clear;
  AContext.Response.CustomHeaders.Values['X-LOGIN-URL'] := LoginURL;
  AContext.Response.CustomHeaders.Values['X-LOGIN-METHOD'] := 'POST';
  AContext.Response.StatusCode := AHttpStatus;
  if AContext.Request.ClientPreferHTML then
  begin
    AContext.Response.ContentType := TMVCMediaType.TEXT_HTML;
    AContext.Response.Content :=
      Format(CONTENT_HTML_FORMAT, [IntToStr(AHttpStatus), AContext.Config[TMVCConfigKey.ServerName]]);
  end
  else
  begin
    lIsPositive := (AHttpStatus div 100) = 2;
    lMsg := System.StrUtils.IfThen(lIsPositive, 'OK', 'KO');
    AContext.Response.ContentType := TMVCMediaType.APPLICATION_JSON;
    AContext.Response.Content := '{"status":"' + lMsg + '", "message":"' + IntToStr(AHttpStatus) + '"}';
  end;
  AHandled := True;
end;

end.
