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
// *************************************************************************** }

// ---------------------------------------------------------------------------
// OIDC Authentication Middleware for DMVCFramework
//
// Implements the OpenID Connect Authorization Code flow with JWT-based
// session management via HTTP-only cookies.
//
// This middleware handles:
//   - OIDC discovery, authorization redirect, token exchange
//   - ID token validation (issuer, audience, expiration)
//   - Optional UserInfo endpoint fetching
//   - Local JWT session cookies with automatic refresh
//   - Route-level authentication gating via callback
//
// Usage:
//   Engine.AddMiddleware(
//     UseOIDCAuthentication(
//       procedure(const Ctx: TWebContext; const IDToken, UserInfo: TJsonObject;
//                 const Roles: TList<string>; const Session: TDictionary<string, string>)
//       begin
//         Roles.Add('user');
//         Session.AddOrSetValue('user_id', IDToken.S['sub']);
//       end,
//       procedure(const Ctx: TWebContext; const Controller, Action: string;
//                 var AuthRequired: Boolean)
//       begin
//         AuthRequired := not Action.StartsWith('Public');
//       end,
//       'https://idp.example.com',
//       'my-client-id',
//       'my-client-secret',
//       'https://app.example.com/auth/callback',
//       'my-jwt-signing-secret'
//     )
//   );
// ---------------------------------------------------------------------------

unit MVCFramework.Middleware.OIDC;

{$I dmvcframework.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.NetEncoding,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.JWT,
  MVCFramework.HMAC,
  MVCFramework.Middleware.JWT,
  JsonDataObjects;

type
  /// <summary>
  /// Callback invoked after a successful OIDC authentication. Use this to
  /// populate user roles and session data from the ID token and UserInfo claims.
  /// </summary>
  TMVCOIDCUserAuthenticatedProc = reference to procedure(
    const AContext: TWebContext;
    const AIDTokenClaims: TJsonObject;
    const AUserInfo: TJsonObject;
    const AUserRoles: TList<string>;
    const ASessionData: TDictionary<string, string>
  );

  /// <summary>
  /// Callback invoked before each controller action to determine whether
  /// authentication is required. Set AAuthenticationRequired to False
  /// to allow unauthenticated access.
  /// </summary>
  TMVCOIDCAuthRequiredProc = reference to procedure(
    const AContext: TWebContext;
    const AControllerQualifiedClassName: string;
    const AActionName: string;
    var AAuthenticationRequired: Boolean
  );

  /// <summary>
  /// OIDC Authorization Code flow middleware with JWT cookie sessions.
  /// Handles login redirect, callback token exchange, logout, and
  /// per-request authentication gating with automatic token refresh.
  /// </summary>
  TMVCOIDCAuthenticationMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    { OIDC configuration }
    FOIDCIssuer: string;
    FClientId: string;
    FClientSecret: string;
    FRedirectUri: string;
    FScopes: string;
    { Discovered endpoints }
    FAuthorizationEndpoint: string;
    FTokenEndpoint: string;
    FUserinfoEndpoint: string;
    FEndpointsDiscovered: Boolean;
    FDiscoveryLock: TObject;
    { JWT session }
    FJWTSecret: string;
    FJWTExpirationMinutes: Integer;
    FClaimsToChecks: TJWTCheckableClaims;
    FLeewaySeconds: Cardinal;
    FHMACAlgorithm: string;
    { URL segments }
    FLoginURL: string;
    FCallbackURL: string;
    FLogoutURL: string;
    FPostLoginRedirectURL: string;
    FPostLogoutRedirectURL: string;
    { Cookie settings }
    FCookieName: string;
    FCookieSecure: Boolean;
    FCookieSameSite: TMVCJWTCookieSameSite;
    FCookiePath: string;
    FCookieDomain: string;
    { Feature flags }
    FFetchUserInfo: Boolean;
    { Callbacks }
    FOnUserAuthenticated: TMVCOIDCUserAuthenticatedProc;
    FOnAuthRequired: TMVCOIDCAuthRequiredProc;
    { OIDC operations }
    procedure DiscoverEndpoints;
    function BuildAuthorizationURL(const AState, ANonce: string): string;
    function ExchangeCodeForTokens(const ACode: string): TJsonObject;
    function ValidateIDToken(const AIDToken: string): TJsonObject;
    function GetUserInfo(const AAccessToken: string): TJsonObject;
    function Base64URLDecode(const AInput: string): string;
    { JWT session operations }
    function CreateSessionJWT(const ARoles: TList<string>;
      const ASessionData: TDictionary<string, string>): string;
    procedure SetSessionCookie(AContext: TWebContext; const AToken: string;
      AExpires: TDateTime);
    procedure ClearSessionCookie(AContext: TWebContext);
    function GetTokenFromCookie(AContext: TWebContext): string;
    function NeedsTokenRefresh(const AJWT: TJWT): Boolean;
    { State cookie — stores state|nonce for CSRF and replay protection }
    procedure SetStateCookie(AContext: TWebContext; const AState, ANonce: string);
    procedure GetAndClearStateCookie(AContext: TWebContext;
      out AState, ANonce: string);
    { Handlers }
    procedure HandleLogin(AContext: TWebContext; var AHandled: Boolean);
    procedure HandleCallback(AContext: TWebContext; var AHandled: Boolean);
    procedure HandleLogout(AContext: TWebContext; var AHandled: Boolean);
  protected
    procedure OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
    procedure OnBeforeControllerAction(AContext: TWebContext;
      const AControllerQualifiedClassName: string;
      const AActionName: string; var AHandled: Boolean);
    procedure OnAfterControllerAction(AContext: TWebContext;
      const AControllerQualifiedClassName: string;
      const AActionName: string; const AHandled: Boolean);
    procedure OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
  public
    constructor Create(
      const AOnUserAuthenticated: TMVCOIDCUserAuthenticatedProc;
      const AOnAuthRequired: TMVCOIDCAuthRequiredProc;
      const AOIDCIssuer, AClientId, AClientSecret, ARedirectUri, AJWTSecret: string;
      const ALoginURL, ACallbackURL, ALogoutURL: string;
      const APostLoginRedirectURL, APostLogoutRedirectURL: string;
      const AScopes: string;
      const AJWTExpirationMinutes: Integer;
      const AClaimsToCheck: TJWTCheckableClaims;
      const ALeewaySeconds: Cardinal;
      const AHMACAlgorithm: string
    ); virtual;
    destructor Destroy; override;
    /// <summary>
    /// Sets the session cookie name (default: 'dmvc_oidc_session')
    /// </summary>
    function SetCookieName(const AName: string): TMVCOIDCAuthenticationMiddleware;
    /// <summary>
    /// Sets the Secure flag (default: True). Set to False only for local
    /// development over plain HTTP.
    /// </summary>
    function SetCookieSecure(ASecure: Boolean): TMVCOIDCAuthenticationMiddleware;
    /// <summary>
    /// Sets the SameSite attribute (default: ssLax). Use ssNone only if
    /// absolutely necessary; it requires Secure=True.
    /// </summary>
    function SetCookieSameSite(ASameSite: TMVCJWTCookieSameSite): TMVCOIDCAuthenticationMiddleware;
    /// <summary>
    /// Sets the cookie path (default: '/')
    /// </summary>
    function SetCookiePath(const APath: string): TMVCOIDCAuthenticationMiddleware;
    /// <summary>
    /// Sets the cookie domain (default: '' for current domain)
    /// </summary>
    function SetCookieDomain(const ADomain: string): TMVCOIDCAuthenticationMiddleware;
    /// <summary>
    /// Enables or disables fetching the OIDC UserInfo endpoint after token
    /// exchange (default: True). When disabled, the AUserInfo parameter in
    /// OnUserAuthenticated will be nil.
    /// </summary>
    function SetFetchUserInfo(AFetch: Boolean): TMVCOIDCAuthenticationMiddleware;
  end;

/// <summary>
/// Factory function that creates a fully configured OIDC authentication
/// middleware. Returns the middleware instance for fluent configuration of
/// cookie settings via SetCookie* methods.
/// </summary>
function UseOIDCAuthentication(
  const AOnUserAuthenticated: TMVCOIDCUserAuthenticatedProc;
  const AOnAuthRequired: TMVCOIDCAuthRequiredProc;
  const AOIDCIssuer, AClientId, AClientSecret, ARedirectUri, AJWTSecret: string;
  const ALoginURL: string = '/auth/login';
  const ACallbackURL: string = '/auth/callback';
  const ALogoutURL: string = '/auth/logout';
  const APostLoginRedirectURL: string = '/';
  const APostLogoutRedirectURL: string = '/auth/login';
  const AScopes: string = 'openid email profile';
  const AJWTExpirationMinutes: Integer = 480;
  const AClaimsToCheck: TJWTCheckableClaims = [TJWTCheckableClaim.ExpirationTime];
  const ALeewaySeconds: Cardinal = 300;
  const AHMACAlgorithm: string = HMAC_HS512
): TMVCOIDCAuthenticationMiddleware;

implementation

uses
  System.DateUtils,
  System.Math,
  MVCFramework.RESTClient,
  MVCFramework.RESTClient.Intf,
  MVCFramework.Logger,
  Web.HTTPApp;  // Must be after MVCFramework.RESTClient to resolve TCookie correctly

resourcestring
  SOIDCDiscoveryRequest = 'OIDC: Discovering endpoints from %s';
  SOIDCDiscoverySuccess = 'OIDC: Discovery complete - authorization=%s, token=%s, userinfo=%s';
  SOIDCDiscoveryError = 'OIDC: Discovery failed for %s - %s';
  SOIDCTokenExchangeRequest = 'OIDC: Exchanging authorization code at %s';
  SOIDCTokenExchangeError = 'OIDC: Token exchange failed - %s';
  SOIDCIDTokenValidating = 'OIDC: Validating ID token';
  SOIDCIDTokenInvalidFormat = 'OIDC: ID token has invalid format (expected 3 dot-separated parts)';
  SOIDCIDTokenIssuerMismatch = 'OIDC: ID token issuer mismatch (expected=%s, got=%s)';
  SOIDCIDTokenAudMismatch = 'OIDC: ID token audience mismatch (expected=%s)';
  SOIDCIDTokenExpired = 'OIDC: ID token has expired';
  SOIDCIDTokenNoSigVerify = 'OIDC: ID token signature verification not implemented - accepting token based on TLS trust';
  SOIDCUserInfoRequest = 'OIDC: Fetching user info from %s';
  SOIDCUserInfoError = 'OIDC: UserInfo request failed - %s';
  SOIDCCallbackReceived = 'OIDC: Authorization callback received with state=%s';
  SOIDCCallbackStateMismatch = 'OIDC: State parameter mismatch (expected=%s, got=%s)';
  SOIDCCallbackError = 'OIDC: Callback processing error - %s';
  SOIDCCallbackNonceMismatch = 'OIDC: Nonce mismatch in ID token (expected=%s, got=%s)';
  SOIDCIDTokenMissingExp = 'OIDC: ID token is missing the required exp claim';
  SOIDCUserLoggedIn = 'OIDC: User authenticated and session created';
  SOIDCUserLoggedOut = 'OIDC: User session cleared';

{ Factory function }

function UseOIDCAuthentication(
  const AOnUserAuthenticated: TMVCOIDCUserAuthenticatedProc;
  const AOnAuthRequired: TMVCOIDCAuthRequiredProc;
  const AOIDCIssuer, AClientId, AClientSecret, ARedirectUri, AJWTSecret: string;
  const ALoginURL: string;
  const ACallbackURL: string;
  const ALogoutURL: string;
  const APostLoginRedirectURL: string;
  const APostLogoutRedirectURL: string;
  const AScopes: string;
  const AJWTExpirationMinutes: Integer;
  const AClaimsToCheck: TJWTCheckableClaims;
  const ALeewaySeconds: Cardinal;
  const AHMACAlgorithm: string
): TMVCOIDCAuthenticationMiddleware;
begin
  Result := TMVCOIDCAuthenticationMiddleware.Create(
    AOnUserAuthenticated,
    AOnAuthRequired,
    AOIDCIssuer,
    AClientId,
    AClientSecret,
    ARedirectUri,
    AJWTSecret,
    ALoginURL,
    ACallbackURL,
    ALogoutURL,
    APostLoginRedirectURL,
    APostLogoutRedirectURL,
    AScopes,
    AJWTExpirationMinutes,
    AClaimsToCheck,
    ALeewaySeconds,
    AHMACAlgorithm
  );
end;

{ TMVCOIDCAuthenticationMiddleware }

constructor TMVCOIDCAuthenticationMiddleware.Create(
  const AOnUserAuthenticated: TMVCOIDCUserAuthenticatedProc;
  const AOnAuthRequired: TMVCOIDCAuthRequiredProc;
  const AOIDCIssuer, AClientId, AClientSecret, ARedirectUri, AJWTSecret: string;
  const ALoginURL, ACallbackURL, ALogoutURL: string;
  const APostLoginRedirectURL, APostLogoutRedirectURL: string;
  const AScopes: string;
  const AJWTExpirationMinutes: Integer;
  const AClaimsToCheck: TJWTCheckableClaims;
  const ALeewaySeconds: Cardinal;
  const AHMACAlgorithm: string);
begin
  inherited Create;
  FOnUserAuthenticated := AOnUserAuthenticated;
  FOnAuthRequired := AOnAuthRequired;
  FOIDCIssuer := AOIDCIssuer;
  FClientId := AClientId;
  FClientSecret := AClientSecret;
  FRedirectUri := ARedirectUri;
  FJWTSecret := AJWTSecret;
  FLoginURL := ALoginURL;
  FCallbackURL := ACallbackURL;
  FLogoutURL := ALogoutURL;
  FPostLoginRedirectURL := APostLoginRedirectURL;
  FPostLogoutRedirectURL := APostLogoutRedirectURL;
  FScopes := AScopes;
  FJWTExpirationMinutes := AJWTExpirationMinutes;
  FClaimsToChecks := AClaimsToCheck;
  FLeewaySeconds := ALeewaySeconds;
  FHMACAlgorithm := AHMACAlgorithm;
  FEndpointsDiscovered := False;
  FDiscoveryLock := TObject.Create;
  // Secure cookie defaults
  FCookieName := 'dmvc_oidc_session';
  FCookieSecure := True;
  FCookieSameSite := ssLax;
  FCookiePath := '/';
  FCookieDomain := '';
  FFetchUserInfo := True;
end;

destructor TMVCOIDCAuthenticationMiddleware.Destroy;
begin
  FDiscoveryLock.Free;
  inherited;
end;

{ Fluent cookie setters }

function TMVCOIDCAuthenticationMiddleware.SetCookieName(
  const AName: string): TMVCOIDCAuthenticationMiddleware;
begin
  FCookieName := AName;
  Result := Self;
end;

function TMVCOIDCAuthenticationMiddleware.SetCookieSecure(
  ASecure: Boolean): TMVCOIDCAuthenticationMiddleware;
begin
  if (not ASecure) and (FCookieSameSite = ssNone) then
    raise EMVCException.Create('Cannot set Secure=False when SameSite=None. ' +
      'Browsers require Secure=True for SameSite=None cookies.');
  FCookieSecure := ASecure;
  Result := Self;
end;

function TMVCOIDCAuthenticationMiddleware.SetCookieSameSite(
  ASameSite: TMVCJWTCookieSameSite): TMVCOIDCAuthenticationMiddleware;
begin
  if (ASameSite = ssNone) and (not FCookieSecure) then
    raise EMVCException.Create('SameSite=None requires Secure=True. ' +
      'Call SetCookieSecure(True) before setting SameSite=None.');
  FCookieSameSite := ASameSite;
  Result := Self;
end;

function TMVCOIDCAuthenticationMiddleware.SetCookiePath(
  const APath: string): TMVCOIDCAuthenticationMiddleware;
begin
  FCookiePath := APath;
  Result := Self;
end;

function TMVCOIDCAuthenticationMiddleware.SetCookieDomain(
  const ADomain: string): TMVCOIDCAuthenticationMiddleware;
begin
  FCookieDomain := ADomain;
  Result := Self;
end;

function TMVCOIDCAuthenticationMiddleware.SetFetchUserInfo(
  AFetch: Boolean): TMVCOIDCAuthenticationMiddleware;
begin
  FFetchUserInfo := AFetch;
  Result := Self;
end;

{ OIDC operations }

procedure TMVCOIDCAuthenticationMiddleware.DiscoverEndpoints;
var
  lClient: IMVCRESTClient;
  lResponse: IMVCRESTResponse;
  lJSON: TJsonObject;
  lDiscoveryURL: string;
begin
  // Thread-safe double-checked locking
  if FEndpointsDiscovered then
    Exit;
  TMonitor.Enter(FDiscoveryLock);
  try
    if FEndpointsDiscovered then
      Exit;
    lDiscoveryURL := FOIDCIssuer;
    if not lDiscoveryURL.EndsWith('/') then
      lDiscoveryURL := lDiscoveryURL + '/';
    lDiscoveryURL := lDiscoveryURL + '.well-known/openid-configuration';
    LogI(Format(SOIDCDiscoveryRequest, [lDiscoveryURL]));
    lClient := TMVCRESTClient.New.BaseURL(lDiscoveryURL);
    lResponse := lClient.Get;
    if not lResponse.Success then
      raise EMVCException.CreateFmt(SOIDCDiscoveryError,
        [lDiscoveryURL, lResponse.StatusCode.ToString + ' ' + lResponse.StatusText]);
    lJSON := lResponse.ToJSONObject;
    try
      FAuthorizationEndpoint := lJSON.S['authorization_endpoint'];
      FTokenEndpoint := lJSON.S['token_endpoint'];
      FUserinfoEndpoint := lJSON.S['userinfo_endpoint'];
    finally
      lJSON.Free;
    end;
    FEndpointsDiscovered := True;
    LogI(Format(SOIDCDiscoverySuccess,
      [FAuthorizationEndpoint, FTokenEndpoint, FUserinfoEndpoint]));
  finally
    TMonitor.Exit(FDiscoveryLock);
  end;
end;

function TMVCOIDCAuthenticationMiddleware.BuildAuthorizationURL(
  const AState, ANonce: string): string;
begin
  Result := FAuthorizationEndpoint +
    '?response_type=code' +
    '&client_id=' + TNetEncoding.URL.Encode(FClientId) +
    '&redirect_uri=' + TNetEncoding.URL.Encode(FRedirectUri) +
    '&scope=' + TNetEncoding.URL.Encode(FScopes) +
    '&state=' + TNetEncoding.URL.Encode(AState) +
    '&nonce=' + TNetEncoding.URL.Encode(ANonce);
end;

function TMVCOIDCAuthenticationMiddleware.ExchangeCodeForTokens(
  const ACode: string): TJsonObject;
var
  lClient: IMVCRESTClient;
  lResponse: IMVCRESTResponse;
begin
  LogI(Format(SOIDCTokenExchangeRequest, [FTokenEndpoint]));
  lClient := TMVCRESTClient.New.BaseURL(FTokenEndpoint);
  lClient
    .AddBodyFieldURLEncoded('grant_type', 'authorization_code')
    .AddBodyFieldURLEncoded('code', ACode)
    .AddBodyFieldURLEncoded('redirect_uri', FRedirectUri)
    .AddBodyFieldURLEncoded('client_id', FClientId)
    .AddBodyFieldURLEncoded('client_secret', FClientSecret);
  lResponse := lClient.Post;
  if not lResponse.Success then
    raise EMVCException.CreateFmt(SOIDCTokenExchangeError,
      [lResponse.StatusCode.ToString + ' ' + lResponse.Content]);
  Result := TJsonObject.Parse(lResponse.Content) as TJsonObject;
end;

function TMVCOIDCAuthenticationMiddleware.ValidateIDToken(
  const AIDToken: string): TJsonObject;
var
  lParts: TArray<string>;
  lPayloadJSON: string;
  lClaims: TJsonObject;
  lIssuer: string;
  lExp: Int64;
  lAudValue: string;
  lAudArray: TJsonArray;
  lAudFound: Boolean;
  I: Integer;
begin
  LogD(SOIDCIDTokenValidating);
  lParts := AIDToken.Split(['.']);
  if Length(lParts) <> 3 then
    raise EMVCException.Create(SOIDCIDTokenInvalidFormat);

  // Decode the payload (middle part)
  lPayloadJSON := Base64URLDecode(lParts[1]);
  lClaims := TJsonObject.Parse(lPayloadJSON) as TJsonObject;
  try
    // Validate issuer (exact match per OIDC Core spec Section 3.1.3.7)
    lIssuer := lClaims.S['iss'];
    if lIssuer <> FOIDCIssuer then
      raise EMVCException.CreateFmt(SOIDCIDTokenIssuerMismatch, [FOIDCIssuer, lIssuer]);

    // Validate audience - handle both string and array per OIDC spec
    lAudFound := False;
    case lClaims.Types['aud'] of
      jdtString:
        begin
          lAudValue := lClaims.S['aud'];
          lAudFound := SameText(lAudValue, FClientId);
        end;
      jdtArray:
        begin
          lAudArray := lClaims.A['aud'];
          for I := 0 to lAudArray.Count - 1 do
          begin
            if SameText(lAudArray.S[I], FClientId) then
            begin
              lAudFound := True;
              Break;
            end;
          end;
        end;
    end;
    if not lAudFound then
      raise EMVCException.CreateFmt(SOIDCIDTokenAudMismatch, [FClientId]);

    // Validate expiration (required claim per OIDC Core spec)
    if not lClaims.Contains('exp') then
      raise EMVCException.Create(SOIDCIDTokenMissingExp);
    lExp := lClaims.L['exp'];
    if UnixToDateTime(lExp, False) + (FLeewaySeconds * OneSecond) <= Now then
      raise EMVCException.Create(SOIDCIDTokenExpired);

    // Signature verification is not implemented; we rely on TLS trust
    // to the token endpoint (the ID token was received directly from the
    // provider over HTTPS, not via the browser).
    LogW(SOIDCIDTokenNoSigVerify);

    Result := lClaims;
    lClaims := nil; // prevent freeing, caller owns the result
  finally
    lClaims.Free;
  end;
end;

function TMVCOIDCAuthenticationMiddleware.GetUserInfo(
  const AAccessToken: string): TJsonObject;
var
  lClient: IMVCRESTClient;
  lResponse: IMVCRESTResponse;
begin
  LogI(Format(SOIDCUserInfoRequest, [FUserinfoEndpoint]));
  lClient := TMVCRESTClient.New
    .BaseURL(FUserinfoEndpoint)
    .SetBearerAuthorization(AAccessToken);
  lResponse := lClient.Get;
  if not lResponse.Success then
    raise EMVCException.CreateFmt(SOIDCUserInfoError,
      [lResponse.StatusCode.ToString + ' ' + lResponse.Content]);
  Result := TJsonObject.Parse(lResponse.Content) as TJsonObject;
end;

function TMVCOIDCAuthenticationMiddleware.Base64URLDecode(
  const AInput: string): string;
var
  lBase64: string;
  lBytes: TBytes;
begin
  // Convert Base64URL to standard Base64
  lBase64 := AInput;
  lBase64 := lBase64.Replace('-', '+');
  lBase64 := lBase64.Replace('_', '/');
  // Add padding
  case Length(lBase64) mod 4 of
    2: lBase64 := lBase64 + '==';
    3: lBase64 := lBase64 + '=';
  end;
  lBytes := TNetEncoding.Base64.DecodeStringToBytes(lBase64);
  Result := TEncoding.UTF8.GetString(lBytes);
end;

{ JWT session operations }

function TMVCOIDCAuthenticationMiddleware.CreateSessionJWT(
  const ARoles: TList<string>;
  const ASessionData: TDictionary<string, string>): string;
var
  lJWT: TJWT;
  lPair: TPair<string, string>;
begin
  lJWT := TJWT.Create(FJWTSecret, FLeewaySeconds, FHMACAlgorithm);
  try
    lJWT.RegClaimsToChecks := FClaimsToChecks;
    // Standard registered claims
    lJWT.Claims.Issuer := 'DMVCFramework.OIDC';
    lJWT.Claims.ExpirationTime := Now + (FJWTExpirationMinutes * OneMinute);
    lJWT.Claims.NotBefore := Now - (5 * OneMinute);
    lJWT.Claims.IssuedAt := Now;
    // Mandatory custom claims for the middleware
    if ASessionData.ContainsKey('user_id') then
      lJWT.CustomClaims['username'] := ASessionData['user_id']
    else
      lJWT.CustomClaims['username'] := '';
    lJWT.CustomClaims['roles'] := string.Join(',', ARoles.ToArray);
    // Copy remaining session data as custom claims
    for lPair in ASessionData do
    begin
      if not SameText(lPair.Key, 'user_id') then
        lJWT.CustomClaims[lPair.Key] := lPair.Value;
    end;
    Result := lJWT.GetToken;
  finally
    lJWT.Free;
  end;
end;

procedure TMVCOIDCAuthenticationMiddleware.SetSessionCookie(
  AContext: TWebContext; const AToken: string; AExpires: TDateTime);
var
  lCookie: TCookie;
  lSameSite: string;
begin
  lCookie := AContext.Response.Cookies.Add;
  lCookie.Name := FCookieName;
  lCookie.Value := AToken;
  lCookie.Path := FCookiePath;
  lCookie.Expires := AExpires;
  lCookie.HttpOnly := True;
  lCookie.Secure := FCookieSecure;
  if not FCookieDomain.IsEmpty then
    lCookie.Domain := FCookieDomain;
  case FCookieSameSite of
    ssStrict: lSameSite := 'Strict';
    ssLax: lSameSite := 'Lax';
    ssNone: lSameSite := 'None';
  end;
  {$IF CompilerVersion >= 34.0}  // Delphi 10.4 Sydney and later
  lCookie.SameSite := lSameSite;
  {$ENDIF}
end;

procedure TMVCOIDCAuthenticationMiddleware.ClearSessionCookie(
  AContext: TWebContext);
begin
  SetSessionCookie(AContext, '', EncodeDate(1970, 1, 1));
end;

function TMVCOIDCAuthenticationMiddleware.GetTokenFromCookie(
  AContext: TWebContext): string;
begin
  Result := Trim(TNetEncoding.URL.Decode(AContext.Request.Cookie(FCookieName)));
end;

function TMVCOIDCAuthenticationMiddleware.NeedsTokenRefresh(
  const AJWT: TJWT): Boolean;
var
  lWillExpireIn: Int64;
begin
  lWillExpireIn := SecondsBetween(Now, AJWT.Claims.ExpirationTime);
  Result := lWillExpireIn <= AJWT.LiveValidityWindowInSeconds;
end;

{ State cookie for CSRF protection }

procedure TMVCOIDCAuthenticationMiddleware.SetStateCookie(
  AContext: TWebContext; const AState, ANonce: string);
var
  lCookie: TCookie;
begin
  lCookie := AContext.Response.Cookies.Add;
  lCookie.Name := FCookieName + '_state';
  lCookie.Value := AState + '|' + ANonce;
  lCookie.Path := FCookiePath;
  lCookie.Expires := Now + (10 * OneMinute);
  lCookie.HttpOnly := True;
  lCookie.Secure := FCookieSecure;
  {$IF CompilerVersion >= 34.0}
  lCookie.SameSite := 'Lax';
  {$ENDIF}
end;

procedure TMVCOIDCAuthenticationMiddleware.GetAndClearStateCookie(
  AContext: TWebContext; out AState, ANonce: string);
var
  lCookie: TCookie;
  lValue: string;
  lParts: TArray<string>;
begin
  lValue := Trim(TNetEncoding.URL.Decode(
    AContext.Request.Cookie(FCookieName + '_state')));
  lParts := lValue.Split(['|']);
  if Length(lParts) = 2 then
  begin
    AState := lParts[0];
    ANonce := lParts[1];
  end
  else
  begin
    AState := lValue;
    ANonce := '';
  end;
  // Clear the state cookie by setting it to expired
  lCookie := AContext.Response.Cookies.Add;
  lCookie.Name := FCookieName + '_state';
  lCookie.Value := '';
  lCookie.Path := FCookiePath;
  lCookie.Expires := EncodeDate(1970, 1, 1);
  lCookie.HttpOnly := True;
  lCookie.Secure := FCookieSecure;
  {$IF CompilerVersion >= 34.0}
  lCookie.SameSite := 'Lax';
  {$ENDIF}
end;

{ Route handlers }

procedure TMVCOIDCAuthenticationMiddleware.HandleLogin(
  AContext: TWebContext; var AHandled: Boolean);
var
  lState: string;
  lNonce: string;
  lAuthURL: string;
begin
  DiscoverEndpoints;
  // Generate CSRF state and nonce as stripped GUIDs
  lState := TGUID.NewGuid.ToString.Replace('{', '').Replace('}', '');
  lNonce := TGUID.NewGuid.ToString.Replace('{', '').Replace('}', '');
  SetStateCookie(AContext, lState, lNonce);
  lAuthURL := BuildAuthorizationURL(lState, lNonce);
  AContext.Response.StatusCode := HTTP_STATUS.Found;
  AContext.Response.SetCustomHeader('Location', lAuthURL);
  AHandled := True;
end;

procedure TMVCOIDCAuthenticationMiddleware.HandleCallback(
  AContext: TWebContext; var AHandled: Boolean);
var
  lCode: string;
  lState: string;
  lExpectedState: string;
  lExpectedNonce: string;
  lTokenResponse: TJsonObject;
  lIDTokenClaims: TJsonObject;
  lUserInfo: TJsonObject;
  lRoles: TList<string>;
  lSessionData: TDictionary<string, string>;
  lSessionJWT: string;
  lAccessToken: string;
begin
  lCode := AContext.Request.QueryStringParam('code');
  lState := AContext.Request.QueryStringParam('state');
  LogI(Format(SOIDCCallbackReceived, [lState]));

  // Check for OIDC provider error (no code returned)
  if lCode.IsEmpty then
  begin
    LogE(Format(SOIDCCallbackError,
      [AContext.Request.QueryStringParam('error')]));
    AContext.Response.StatusCode := HTTP_STATUS.Found;
    AContext.Response.SetCustomHeader('Location', FPostLogoutRedirectURL);
    AHandled := True;
    Exit;
  end;

  // Verify CSRF state and extract stored nonce
  GetAndClearStateCookie(AContext, lExpectedState, lExpectedNonce);
  if not SameText(lState, lExpectedState) then
  begin
    LogW(Format(SOIDCCallbackStateMismatch, [lExpectedState, lState]));
    AContext.Response.StatusCode := HTTP_STATUS.Found;
    AContext.Response.SetCustomHeader('Location', FPostLogoutRedirectURL);
    AHandled := True;
    Exit;
  end;

  lTokenResponse := nil;
  lIDTokenClaims := nil;
  lUserInfo := nil;
  lRoles := nil;
  lSessionData := nil;
  try
    try
      // Exchange authorization code for tokens
      DiscoverEndpoints;
      lTokenResponse := ExchangeCodeForTokens(lCode);

      // Validate the ID token
      lIDTokenClaims := ValidateIDToken(lTokenResponse.S['id_token']);

      // Validate nonce to prevent replay attacks (OIDC Core 3.1.3.7)
      if not lExpectedNonce.IsEmpty then
      begin
        if lIDTokenClaims.S['nonce'] <> lExpectedNonce then
          raise EMVCException.CreateFmt(SOIDCCallbackNonceMismatch,
            [lExpectedNonce, lIDTokenClaims.S['nonce']]);
      end;

      // Optionally fetch UserInfo
      lAccessToken := lTokenResponse.S['access_token'];
      if FFetchUserInfo and (not lAccessToken.IsEmpty) and
         (not FUserinfoEndpoint.IsEmpty) then
      begin
        try
          lUserInfo := GetUserInfo(lAccessToken);
        except
          on E: Exception do
          begin
            // UserInfo failure is non-fatal
            LogW(Format(SOIDCUserInfoError, [E.Message]));
            lUserInfo := nil;
          end;
        end;
      end;

      // Let the application populate roles and session data
      lRoles := TList<string>.Create;
      lSessionData := TDictionary<string, string>.Create;
      // Pre-populate user_id with the OIDC subject as default
      lSessionData.Add('user_id', lIDTokenClaims.S['sub']);
      if Assigned(FOnUserAuthenticated) then
        FOnUserAuthenticated(AContext, lIDTokenClaims, lUserInfo,
          lRoles, lSessionData);

      // Create JWT session and set cookie
      lSessionJWT := CreateSessionJWT(lRoles, lSessionData);
      SetSessionCookie(AContext, lSessionJWT,
        Now + (FJWTExpirationMinutes * OneMinute));

      LogI(SOIDCUserLoggedIn);
      AContext.Response.StatusCode := HTTP_STATUS.Found;
      AContext.Response.SetCustomHeader('Location', FPostLoginRedirectURL);
      AHandled := True;
    except
      on E: Exception do
      begin
        LogE(Format(SOIDCCallbackError, [E.Message]));
        ClearSessionCookie(AContext);
        AContext.Response.StatusCode := HTTP_STATUS.Found;
        AContext.Response.SetCustomHeader('Location', FPostLogoutRedirectURL);
        AHandled := True;
      end;
    end;
  finally
    lSessionData.Free;
    lRoles.Free;
    lUserInfo.Free;
    lIDTokenClaims.Free;
    lTokenResponse.Free;
  end;
end;

procedure TMVCOIDCAuthenticationMiddleware.HandleLogout(
  AContext: TWebContext; var AHandled: Boolean);
begin
  ClearSessionCookie(AContext);
  LogI(SOIDCUserLoggedOut);
  AContext.Response.StatusCode := HTTP_STATUS.Found;
  AContext.Response.SetCustomHeader('Location', FPostLogoutRedirectURL);
  AHandled := True;
end;

{ IMVCMiddleware }

procedure TMVCOIDCAuthenticationMiddleware.OnBeforeRouting(
  AContext: TWebContext; var AHandled: Boolean);
begin
  if SameText(AContext.Request.PathInfo, FLoginURL) then
    HandleLogin(AContext, AHandled)
  else if SameText(AContext.Request.PathInfo, FCallbackURL) then
    HandleCallback(AContext, AHandled)
  else if SameText(AContext.Request.PathInfo, FLogoutURL) then
    HandleLogout(AContext, AHandled);
end;

procedure TMVCOIDCAuthenticationMiddleware.OnBeforeControllerAction(
  AContext: TWebContext;
  const AControllerQualifiedClassName, AActionName: string;
  var AHandled: Boolean);
var
  lAuthRequired: Boolean;
  lJWTValue: TJWT;
  lAuthToken: string;
  lErrorMsg: string;
begin
  // Determine whether this action requires authentication
  lAuthRequired := True;
  if Assigned(FOnAuthRequired) then
    FOnAuthRequired(AContext, AControllerQualifiedClassName, AActionName,
      lAuthRequired);

  if not lAuthRequired then
  begin
    AHandled := False;
    // Optionally populate LoggedUser for public routes that check IsValid
    lAuthToken := GetTokenFromCookie(AContext);
    if not lAuthToken.IsEmpty then
    begin
      lJWTValue := TJWT.Create(FJWTSecret, FLeewaySeconds, FHMACAlgorithm);
      try
        lJWTValue.RegClaimsToChecks := FClaimsToChecks;
        if lJWTValue.LoadToken(lAuthToken, lErrorMsg) then
        begin
          AContext.LoggedUser.UserName := lJWTValue.CustomClaims['username'];
          AContext.LoggedUser.Roles.AddRange(
            lJWTValue.CustomClaims['roles'].Split([',']));
          AContext.LoggedUser.LoggedSince := lJWTValue.Claims.IssuedAt;
          AContext.LoggedUser.CustomData := lJWTValue.CustomClaims.AsCustomData;
          // Restore 'sub' - JWT LoadToken classifies it as a registered claim,
          // but the OIDC callback stores it as session data
          if not lJWTValue.Claims.Subject.IsEmpty then
            AContext.LoggedUser.CustomData.AddOrSetValue('sub', lJWTValue.Claims.Subject);
          // Restore 'user_id' - CreateSessionJWT maps it to the mandatory 'username' claim
          if not lJWTValue.CustomClaims['username'].IsEmpty then
            AContext.LoggedUser.CustomData.AddOrSetValue('user_id', lJWTValue.CustomClaims['username']);
        end;
      finally
        lJWTValue.Free;
      end;
    end;
    Exit;
  end;

  // Authentication is required - validate session token
  lJWTValue := TJWT.Create(FJWTSecret, FLeewaySeconds, FHMACAlgorithm);
  try
    lJWTValue.RegClaimsToChecks := FClaimsToChecks;
    lAuthToken := GetTokenFromCookie(AContext);

    if lAuthToken.IsEmpty then
    begin
      // No token: redirect to login (not 401 for browser-based flows)
      AContext.Response.StatusCode := HTTP_STATUS.Found;
      AContext.Response.SetCustomHeader('Location', FLoginURL);
      AHandled := True;
      Exit;
    end;

    if not lJWTValue.LoadToken(lAuthToken, lErrorMsg) then
    begin
      // Invalid or expired token: redirect to login
      ClearSessionCookie(AContext);
      AContext.Response.StatusCode := HTTP_STATUS.Found;
      AContext.Response.SetCustomHeader('Location', FLoginURL);
      AHandled := True;
      Exit;
    end;

    // Verify that the session has a username
    if lJWTValue.CustomClaims['username'].IsEmpty then
    begin
      ClearSessionCookie(AContext);
      AContext.Response.StatusCode := HTTP_STATUS.Found;
      AContext.Response.SetCustomHeader('Location', FLoginURL);
      AHandled := True;
      Exit;
    end;

    // Populate the LoggedUser from the validated JWT
    AContext.LoggedUser.UserName := lJWTValue.CustomClaims['username'];
    AContext.LoggedUser.Roles.AddRange(
      lJWTValue.CustomClaims['roles'].Split([',']));
    AContext.LoggedUser.LoggedSince := lJWTValue.Claims.IssuedAt;
    AContext.LoggedUser.CustomData := lJWTValue.CustomClaims.AsCustomData;
    // Restore 'sub' - JWT LoadToken classifies it as a registered claim,
    // but the OIDC callback stores it as session data
    if not lJWTValue.Claims.Subject.IsEmpty then
      AContext.LoggedUser.CustomData.AddOrSetValue('sub', lJWTValue.Claims.Subject);
    // Restore 'user_id' - CreateSessionJWT maps it to the mandatory 'username' claim
    if not lJWTValue.CustomClaims['username'].IsEmpty then
      AContext.LoggedUser.CustomData.AddOrSetValue('user_id', lJWTValue.CustomClaims['username']);

    // Handle token refresh if LiveValidityWindowInSeconds is configured
    if lJWTValue.LiveValidityWindowInSeconds > 0 then
    begin
      if NeedsTokenRefresh(lJWTValue) then
      begin
        lJWTValue.Claims.ExpirationTime :=
          Max(lJWTValue.Claims.ExpirationTime, Now) +
          (lJWTValue.LeewaySeconds + lJWTValue.LiveValidityWindowInSeconds) * OneSecond;
        SetSessionCookie(AContext, lJWTValue.GetToken,
          lJWTValue.Claims.ExpirationTime);
        AContext.Response.SetCustomHeader('X-JWT-Refreshed', 'true');
      end;
    end;

    AHandled := False;
  finally
    lJWTValue.Free;
  end;
end;

procedure TMVCOIDCAuthenticationMiddleware.OnAfterControllerAction(
  AContext: TWebContext;
  const AControllerQualifiedClassName: string;
  const AActionName: string;
  const AHandled: Boolean);
begin
  // No-op
end;

procedure TMVCOIDCAuthenticationMiddleware.OnAfterRouting(
  AContext: TWebContext; const AHandled: Boolean);
begin
  // No-op
end;

end.
