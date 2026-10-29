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
// INTERNAL - not an API: shared by the OIDC filters and the deprecated middleware; goes away with them in 4.0

unit MVCFramework.Filters.OIDC.Internal;

{$I dmvcframework.inc}

interface

uses
  MVCFramework,
  MVCFramework.JWT,                            // TJWTCheckableClaims, IJWKSProvider, TMVCJWTCookieSameSite
  MVCFramework.Filters.OIDC;                   // TMVCOIDCUserAuthenticatedProc, TMVCOIDCAuthRequiredProc

type
  // The OIDC flow behind the OIDC filters and TMVCOIDCAuthenticationMiddleware.
  // Its settings start as in TMVCOIDCOptions.Default.
  IMVCOIDCAuthenticationFlow = interface
    procedure SetCookieName(const AName: string);
    procedure SetCookieSecure(ASecure: Boolean);                         // raises for False with SameSite=None
    procedure SetCookieSameSite(ASameSite: TMVCJWTCookieSameSite);       // raises for ssNone without Secure
    procedure SetCookiePath(const APath: string);
    procedure SetCookieDomain(const ADomain: string);
    procedure SetFetchUserInfo(AFetch: Boolean);
    procedure SetPKCE(AUse: Boolean);
    procedure SetBaseURL(const AURL: string);
    procedure SetJWKSProvider(const AProvider: IJWKSProvider);
    // answers LoginURL, CallbackURL and LogoutURL (AHandled = True); every other request goes on
    procedure HandleAuthURL(const AContext: TWebContext; var AHandled: Boolean);
    // without a valid session where one is required, redirects to LoginURL (AHandled = True)
    procedure CheckSession(const AContext: TWebContext; const AControllerQualifiedClassName,
      AActionName: string; var AHandled: Boolean);
  end;

// Raises when AJWTSecret, AOIDCIssuer or AClientId is empty.
function NewOIDCAuthenticationFlow(
  const AOnUserAuthenticated: TMVCOIDCUserAuthenticatedProc;
  const AOnAuthRequired: TMVCOIDCAuthRequiredProc;
  const AOIDCIssuer, AClientId, AClientSecret, ARedirectUri, AJWTSecret: string;
  const ALoginURL, ACallbackURL, ALogoutURL: string;
  const APostLoginRedirectURL, APostLogoutRedirectURL: string;
  const AScopes: string;
  const AJWTExpirationMinutes: Integer;
  const AClaimsToCheck: TJWTCheckableClaims;
  const ALeewaySeconds: Cardinal;
  const AHMACAlgorithm: string): IMVCOIDCAuthenticationFlow;

implementation

uses
  System.Generics.Collections,
  JsonDataObjects,
  System.SysUtils,
  System.Classes,
  System.NetEncoding,
  System.DateUtils,
  System.Hash,
  System.Math,
  System.SyncObjs,
  MVCFramework.Commons,
  MVCFramework.HMAC,
  MVCFramework.RESTClient,
  MVCFramework.RESTClient.Intf,
  MVCFramework.Logger,
  Web.HTTPApp;  // Must be after MVCFramework.RESTClient to resolve TCookie correctly

resourcestring
  SOIDCDiscoveryRequest = 'OIDC: Discovering endpoints from %s';
  SOIDCDiscoverySuccess = 'OIDC: Discovery complete - authorization=%s, token=%s, userinfo=%s, end_session=%s';
  SOIDCDiscoveryError = 'OIDC: Discovery failed for %s - %s';
  SOIDCTokenExchangeRequest = 'OIDC: Exchanging authorization code at %s';
  SOIDCTokenExchangeError = 'OIDC: Token exchange failed - %s';
  SOIDCIDTokenValidating = 'OIDC: Validating ID token';
  SOIDCIDTokenInvalidFormat = 'OIDC: ID token has invalid format (expected 3 dot-separated parts)';
  SOIDCIDTokenIssuerMismatch = 'OIDC: ID token issuer mismatch (expected=%s, got=%s)';
  SOIDCIDTokenAudMismatch = 'OIDC: ID token audience mismatch (expected=%s)';
  SOIDCIDTokenAzpMismatch = 'OIDC: ID token azp mismatch (expected=%s, got=%s)';
  SOIDCIDTokenExpired = 'OIDC: ID token has expired';
  SOIDCIDTokenNoSigVerify = 'OIDC: ID token signature verification not configured - accepting token based on TLS trust';
  SOIDCIDTokenSigVerified = 'OIDC: ID token signature verified successfully via JWKS';
  SOIDCIDTokenSigFailed = 'OIDC: ID token signature verification FAILED - token rejected';
  SOIDCIDTokenSigNoKey = 'OIDC: No matching key found in JWKS for ID token - token rejected';
  SOIDCUserInfoRequest = 'OIDC: Fetching user info from %s';
  SOIDCUserInfoError = 'OIDC: UserInfo request failed - %s';
  SOIDCCallbackReceived = 'OIDC: Authorization callback received with state=%s';
  SOIDCCallbackStateMismatch = 'OIDC: State parameter mismatch (expected=%s, got=%s)';
  SOIDCCallbackError = 'OIDC: Callback processing error - %s';
  SOIDCCallbackNonceMismatch = 'OIDC: Nonce mismatch in ID token (expected=%s, got=%s)';
  SOIDCIDTokenMissingExp = 'OIDC: ID token is missing the required exp claim';
  SOIDCIDTokenNotYetValid = 'OIDC: ID token is not yet valid (nbf is in the future)';
  SOIDCIDTokenIssuedInFuture = 'OIDC: ID token was issued in the future (iat beyond allowed clock skew)';
  SOIDCUserLoggedIn = 'OIDC: User authenticated and session created';
  SOIDCUserLoggedOut = 'OIDC: User session cleared';

type
  TMVCOIDCAuthenticationFlow = class(TInterfacedObject, IMVCOIDCAuthenticationFlow)
  private
    { OIDC configuration }
    fOIDCIssuer: string;
    fClientId: string;
    fClientSecret: string;
    fRedirectUri: string;
    fScopes: string;
    { Discovered endpoints }
    fAuthorizationEndpoint: string;
    fTokenEndpoint: string;
    fUserinfoEndpoint: string;
    fEndSessionEndpoint: string;
    fEndpointsDiscovered: Integer;  // 0 = not discovered, 1 = discovered (uses TInterlocked for thread safety)
    fDiscoveryLock: TObject;
    { JWT session }
    fJWTSecret: string;
    fJWTExpirationMinutes: Integer;
    fClaimsToChecks: TJWTCheckableClaims;
    fLeewaySeconds: Cardinal;
    fHMACAlgorithm: string;
    { URL segments }
    fLoginURL: string;
    fCallbackURL: string;
    fLogoutURL: string;
    fPostLoginRedirectURL: string;
    fPostLogoutRedirectURL: string;
    { Cookie settings }
    fCookieName: string;
    fCookieSecure: Boolean;
    fCookieSameSite: TMVCJWTCookieSameSite;
    fCookiePath: string;
    fCookieDomain: string;
    { Feature flags }
    fFetchUserInfo: Boolean;
    fUsePKCE: Boolean;
    fBaseURL: string;
    { JWKS signature verification (optional, requires TaurusTLS) }
    fJWKSProvider: IJWKSProvider;
    { Callbacks }
    fOnUserAuthenticated: TMVCOIDCUserAuthenticatedProc;
    fOnAuthRequired: TMVCOIDCAuthRequiredProc;
    { OIDC operations }
    procedure DiscoverEndpoints;
    function BuildAuthorizationURL(const AState, ANonce, ACodeVerifier: string): string;
    function ExchangeCodeForTokens(const ACode, ACodeVerifier: string): TJsonObject;
    function ValidateIDToken(const AIDToken: string): TJsonObject;
    function GetUserInfo(const AAccessToken: string): TJsonObject;
    function Base64URLDecode(const AInput: string): string;
    { PKCE (RFC 7636) }
    function GenerateCodeVerifier: string;
    function ComputeS256Challenge(const ACodeVerifier: string): string;
    { JWT session operations }
    function CreateSessionJWT(const ARoles: TList<string>;
      const ASessionData: TDictionary<string, string>): string;
    procedure SetSessionCookie(AContext: TWebContext; const AToken: string;
      AExpires: TDateTime);
    procedure ClearSessionCookie(AContext: TWebContext);
    function GetTokenFromCookie(AContext: TWebContext): string;
    function NeedsTokenRefresh(const AJWT: TJWT): Boolean;
    { State cookie: stores state|nonce|code_verifier for CSRF and replay protection }
    procedure SetStateCookie(AContext: TWebContext;
      const AState, ANonce, ACodeVerifier: string);
    procedure GetAndClearStateCookie(AContext: TWebContext;
      out AState, ANonce, ACodeVerifier: string);
    { Handlers }
    procedure HandleLogin(AContext: TWebContext; var AHandled: Boolean);
    procedure HandleCallback(AContext: TWebContext; var AHandled: Boolean);
    procedure HandleLogout(AContext: TWebContext; var AHandled: Boolean);
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
      const AHMACAlgorithm: string);
    destructor Destroy; override;
    { IMVCOIDCAuthenticationFlow }
    procedure SetCookieName(const AName: string);
    procedure SetCookieSecure(ASecure: Boolean);
    procedure SetCookieSameSite(ASameSite: TMVCJWTCookieSameSite);
    procedure SetCookiePath(const APath: string);
    procedure SetCookieDomain(const ADomain: string);
    procedure SetFetchUserInfo(AFetch: Boolean);
    procedure SetPKCE(AUse: Boolean);
    procedure SetBaseURL(const AURL: string);
    procedure SetJWKSProvider(const AProvider: IJWKSProvider);
    procedure HandleAuthURL(const AContext: TWebContext; var AHandled: Boolean);
    procedure CheckSession(const AContext: TWebContext; const AControllerQualifiedClassName,
      AActionName: string; var AHandled: Boolean);
  end;

function NewOIDCAuthenticationFlow(
  const AOnUserAuthenticated: TMVCOIDCUserAuthenticatedProc;
  const AOnAuthRequired: TMVCOIDCAuthRequiredProc;
  const AOIDCIssuer, AClientId, AClientSecret, ARedirectUri, AJWTSecret: string;
  const ALoginURL, ACallbackURL, ALogoutURL: string;
  const APostLoginRedirectURL, APostLogoutRedirectURL: string;
  const AScopes: string;
  const AJWTExpirationMinutes: Integer;
  const AClaimsToCheck: TJWTCheckableClaims;
  const ALeewaySeconds: Cardinal;
  const AHMACAlgorithm: string): IMVCOIDCAuthenticationFlow;
begin
  Result := TMVCOIDCAuthenticationFlow.Create(AOnUserAuthenticated, AOnAuthRequired,
    AOIDCIssuer, AClientId, AClientSecret, ARedirectUri, AJWTSecret,
    ALoginURL, ACallbackURL, ALogoutURL, APostLoginRedirectURL, APostLogoutRedirectURL, AScopes,
    AJWTExpirationMinutes, AClaimsToCheck, ALeewaySeconds, AHMACAlgorithm);
end;

{ TMVCOIDCAuthenticationFlow }

constructor TMVCOIDCAuthenticationFlow.Create(
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
  // Validate required parameters
  if AJWTSecret.Trim.IsEmpty then
    raise EMVCException.Create(
      'OIDC Middleware: JWT secret must not be empty. ' +
      'Set JWT_SECRET to a random string of at least 32 characters.');
  if AOIDCIssuer.Trim.IsEmpty then
    raise EMVCException.Create(
      'OIDC Middleware: OIDC issuer URL must not be empty.');
  if AClientId.Trim.IsEmpty then
    raise EMVCException.Create(
      'OIDC Middleware: OIDC client ID must not be empty.');
  fOnUserAuthenticated := AOnUserAuthenticated;
  fOnAuthRequired := AOnAuthRequired;
  fOIDCIssuer := AOIDCIssuer;
  fClientId := AClientId;
  fClientSecret := AClientSecret;
  fRedirectUri := ARedirectUri;
  fJWTSecret := AJWTSecret;
  fLoginURL := ALoginURL;
  fCallbackURL := ACallbackURL;
  fLogoutURL := ALogoutURL;
  fPostLoginRedirectURL := APostLoginRedirectURL;
  fPostLogoutRedirectURL := APostLogoutRedirectURL;
  fScopes := AScopes;
  fJWTExpirationMinutes := AJWTExpirationMinutes;
  fClaimsToChecks := AClaimsToCheck;
  fLeewaySeconds := ALeewaySeconds;
  fHMACAlgorithm := AHMACAlgorithm;
  fEndpointsDiscovered := 0;
  fDiscoveryLock := TObject.Create;
  // Secure cookie defaults
  fCookieName := 'dmvc_oidc_session';
  fCookieSecure := True;
  fCookieSameSite := ssLax;
  fCookiePath := '/';
  fCookieDomain := '';
  fFetchUserInfo := True;
  fUsePKCE := True;
  // One-time startup notice about signature verification
  fJWKSProvider := nil;
  LogI('OIDC: ID token signature verification not configured. ' +
    'Call SetJWKSProvider() to enable cryptographic verification via JWKS.');
end;

destructor TMVCOIDCAuthenticationFlow.Destroy;
begin
  // Zero sensitive secrets before releasing memory
  if fClientSecret <> '' then
  begin
    UniqueString(fClientSecret);
    FillChar(fClientSecret[1], Length(fClientSecret) * SizeOf(Char), 0);
    fClientSecret := '';
  end;
  if fJWTSecret <> '' then
  begin
    UniqueString(fJWTSecret);
    FillChar(fJWTSecret[1], Length(fJWTSecret) * SizeOf(Char), 0);
    fJWTSecret := '';
  end;
  fDiscoveryLock.Free;
  inherited;
end;

{ Settings }

procedure TMVCOIDCAuthenticationFlow.SetCookieName(const AName: string);
begin
  fCookieName := AName;
end;

procedure TMVCOIDCAuthenticationFlow.SetCookieSecure(ASecure: Boolean);
begin
  if (not ASecure) and (fCookieSameSite = ssNone) then
    raise EMVCException.Create('Cannot set Secure=False when SameSite=None. ' +
      'Browsers require Secure=True for SameSite=None cookies.');
  fCookieSecure := ASecure;
end;

procedure TMVCOIDCAuthenticationFlow.SetCookieSameSite(ASameSite: TMVCJWTCookieSameSite);
begin
  if (ASameSite = ssNone) and (not fCookieSecure) then
    raise EMVCException.Create('SameSite=None requires Secure=True. ' +
      'Call SetCookieSecure(True) before setting SameSite=None.');
  fCookieSameSite := ASameSite;
end;

procedure TMVCOIDCAuthenticationFlow.SetCookiePath(const APath: string);
begin
  fCookiePath := APath;
end;

procedure TMVCOIDCAuthenticationFlow.SetCookieDomain(const ADomain: string);
begin
  fCookieDomain := ADomain;
end;

procedure TMVCOIDCAuthenticationFlow.SetFetchUserInfo(AFetch: Boolean);
begin
  fFetchUserInfo := AFetch;
end;

procedure TMVCOIDCAuthenticationFlow.SetPKCE(AUse: Boolean);
begin
  fUsePKCE := AUse;
end;

procedure TMVCOIDCAuthenticationFlow.SetBaseURL(const AURL: string);
begin
  fBaseURL := AURL;
end;

procedure TMVCOIDCAuthenticationFlow.SetJWKSProvider(const AProvider: IJWKSProvider);
begin
  fJWKSProvider := AProvider;
  if Assigned(AProvider) then
    LogI('OIDC: JWKS provider configured - ID token signatures will be verified')
  else
    LogI('OIDC: JWKS provider cleared - falling back to TLS trust');
end;

{ OIDC operations }

procedure TMVCOIDCAuthenticationFlow.DiscoverEndpoints;
var
  lClient: IMVCRESTClient;
  lResponse: IMVCRESTResponse;
  lJSON: TJsonObject;
  lDiscoveryURL: string;
begin
  // Thread-safe double-checked locking with memory barriers
  if TInterlocked.CompareExchange(fEndpointsDiscovered, 0, 0) = 1 then
    Exit;
  TMonitor.Enter(fDiscoveryLock);
  try
    if TInterlocked.CompareExchange(fEndpointsDiscovered, 0, 0) = 1 then
      Exit;
    lDiscoveryURL := fOIDCIssuer;
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
      fAuthorizationEndpoint := lJSON.S['authorization_endpoint'];
      fTokenEndpoint := lJSON.S['token_endpoint'];
      fUserinfoEndpoint := lJSON.S['userinfo_endpoint'];
      fEndSessionEndpoint := lJSON.S['end_session_endpoint'];
    finally
      lJSON.Free;
    end;
    TInterlocked.Exchange(fEndpointsDiscovered, 1);
    LogI(Format(SOIDCDiscoverySuccess,
      [fAuthorizationEndpoint, fTokenEndpoint, fUserinfoEndpoint, fEndSessionEndpoint]));
  finally
    TMonitor.Exit(fDiscoveryLock);
  end;
end;

function TMVCOIDCAuthenticationFlow.BuildAuthorizationURL(
  const AState, ANonce, ACodeVerifier: string): string;
begin
  Result := fAuthorizationEndpoint +
    '?response_type=code' +
    '&client_id=' + TNetEncoding.URL.Encode(fClientId) +
    '&redirect_uri=' + TNetEncoding.URL.Encode(fRedirectUri) +
    '&scope=' + TNetEncoding.URL.Encode(fScopes) +
    '&state=' + TNetEncoding.URL.Encode(AState) +
    '&nonce=' + TNetEncoding.URL.Encode(ANonce);
  // RFC 7636: bind this authorization request to a secret only this client
  // knows, so a stolen authorization code cannot be redeemed by anyone else.
  if fUsePKCE and (not ACodeVerifier.IsEmpty) then
    Result := Result +
      '&code_challenge=' + ComputeS256Challenge(ACodeVerifier) +
      '&code_challenge_method=S256';
end;

function TMVCOIDCAuthenticationFlow.ExchangeCodeForTokens(
  const ACode, ACodeVerifier: string): TJsonObject;
var
  lClient: IMVCRESTClient;
  lResponse: IMVCRESTResponse;
begin
  LogI(Format(SOIDCTokenExchangeRequest, [fTokenEndpoint]));
  lClient := TMVCRESTClient.New.BaseURL(fTokenEndpoint);
  lClient
    .AddBodyFieldURLEncoded('grant_type', 'authorization_code')
    .AddBodyFieldURLEncoded('code', ACode)
    .AddBodyFieldURLEncoded('redirect_uri', fRedirectUri)
    .AddBodyFieldURLEncoded('client_id', fClientId)
    .AddBodyFieldURLEncoded('client_secret', fClientSecret);
  if fUsePKCE and (not ACodeVerifier.IsEmpty) then
    lClient.AddBodyFieldURLEncoded('code_verifier', ACodeVerifier);
  lResponse := lClient.Post;
  if not lResponse.Success then
  begin
    LogE(Format('OIDC: Token exchange failed - status=%d body=%s',
      [lResponse.StatusCode, lResponse.Content]));
    raise EMVCException.CreateFmt(SOIDCTokenExchangeError,
      [lResponse.StatusCode.ToString]);
  end;
  Result := TJsonObject.Parse(lResponse.Content) as TJsonObject;
end;

function TMVCOIDCAuthenticationFlow.ValidateIDToken(
  const AIDToken: string): TJsonObject;
var
  lParts: TArray<string>;
  lHeaderJSON, lPayloadJSON: string;
  lClaims: TJsonObject;
  lIssuer: string;
  lExp: Int64;
  lNbf, lIat: Int64;
  lAudValue: string;
  lAudArray: TJsonArray;
  lAudFound: Boolean;
  I: Integer;
  lSigner: IJWTSigner;
  lDataToVerify: string;
  lSignatureBytes: TBytes;
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
    if lIssuer <> fOIDCIssuer then
      raise EMVCException.CreateFmt(SOIDCIDTokenIssuerMismatch, [fOIDCIssuer, lIssuer]);

    // Validate audience - handle both string and array per OIDC spec
    lAudFound := False;
    case lClaims.Types['aud'] of
      jdtString:
        begin
          lAudValue := lClaims.S['aud'];
          lAudFound := (lAudValue = fClientId);  // case-sensitive per OIDC spec
        end;
      jdtArray:
        begin
          lAudArray := lClaims.A['aud'];
          for I := 0 to lAudArray.Count - 1 do
          begin
            if lAudArray.S[I] = fClientId then  // case-sensitive per OIDC spec
            begin
              lAudFound := True;
              Break;
            end;
          end;
        end;
    end;
    if not lAudFound then
      raise EMVCException.CreateFmt(SOIDCIDTokenAudMismatch, [fClientId]);

    // OIDC Core 3.1.3.7 steps 4 and 5: when an azp (authorized party) claim
    // is present, it must be this client. It identifies the party the token
    // was issued to, which for a multi-audience token is the only claim that
    // says the token was meant for us and not merely readable by us.
    if lClaims.Contains('azp') and (lClaims.S['azp'] <> fClientId) then
      raise EMVCException.CreateFmt(SOIDCIDTokenAzpMismatch,
        [fClientId, lClaims.S['azp']]);

    // Validate expiration (required claim per OIDC Core spec)
    if not lClaims.Contains('exp') then
      raise EMVCException.Create(SOIDCIDTokenMissingExp);
    lExp := lClaims.L['exp'];
    if UnixToDateTime(lExp, False) + (fLeewaySeconds * OneSecond) <= Now then
      raise EMVCException.Create(SOIDCIDTokenExpired);

    // Validate not-before / issued-at when present (same leeway as exp). Both
    // are optional on an OIDC ID token, so absent claims are simply skipped.
    if lClaims.Contains('nbf') then
    begin
      lNbf := lClaims.L['nbf'];
      if UnixToDateTime(lNbf, False) - (fLeewaySeconds * OneSecond) > Now then
        raise EMVCException.Create(SOIDCIDTokenNotYetValid);
    end;
    if lClaims.Contains('iat') then
    begin
      lIat := lClaims.L['iat'];
      if UnixToDateTime(lIat, False) - (fLeewaySeconds * OneSecond) > Now then
        raise EMVCException.Create(SOIDCIDTokenIssuedInFuture);
    end;

    // ID token signature verification
    if Assigned(fJWKSProvider) then
    begin
      // Decode the header to get kid and alg for key lookup
      lHeaderJSON := Base64URLDecode(lParts[0]);
      lSigner := fJWKSProvider.GetSignerForToken(lHeaderJSON);
      if not Assigned(lSigner) then
        raise EMVCException.Create(SOIDCIDTokenSigNoKey);
      // Verify: signature is over "header.payload" (the first two base64url parts)
      lDataToVerify := lParts[0] + '.' + lParts[1];
      lSignatureBytes := URLSafeB64DecodeBytes(lParts[2]);
      if not lSigner.Verify(lDataToVerify, lSignatureBytes) then
        raise EMVCException.Create(SOIDCIDTokenSigFailed);
      LogD(SOIDCIDTokenSigVerified);
    end
    else
      // No JWKS provider configured - rely on TLS trust to the token endpoint.
      // The token was received directly from the provider over HTTPS, not via the
      // browser. Warn (not debug): cryptographic verification is OFF — configure a
      // JWKS provider via SetJWKSProvider to verify the ID token signature.
      LogW(SOIDCIDTokenNoSigVerify);

    Result := lClaims;
    lClaims := nil; // prevent freeing, caller owns the result
  finally
    lClaims.Free;
  end;
end;

function TMVCOIDCAuthenticationFlow.GetUserInfo(
  const AAccessToken: string): TJsonObject;
var
  lClient: IMVCRESTClient;
  lResponse: IMVCRESTResponse;
begin
  LogI(Format(SOIDCUserInfoRequest, [fUserinfoEndpoint]));
  lClient := TMVCRESTClient.New
    .BaseURL(fUserinfoEndpoint)
    .SetBearerAuthorization(AAccessToken);
  lResponse := lClient.Get;
  if not lResponse.Success then
  begin
    LogE(Format('OIDC: UserInfo request failed - status=%d body=%s',
      [lResponse.StatusCode, lResponse.Content]));
    raise EMVCException.CreateFmt(SOIDCUserInfoError,
      [lResponse.StatusCode.ToString]);
  end;
  Result := TJsonObject.Parse(lResponse.Content) as TJsonObject;
end;

function TMVCOIDCAuthenticationFlow.GenerateCodeVerifier: string;
var
  lBytes: TBytes;
  lGuid: TGUID;
begin
  // RFC 7636 section 4.1 recommends 32 octets from a suitable random number
  // generator. Two GUIDs are exactly 32 bytes and come from the OS CSPRNG.
  // Base64url without padding keeps the result inside the unreserved set and
  // 43 characters long, which is the minimum the ABNF allows.
  SetLength(lBytes, 32);
  lGuid := TGUID.NewGuid;
  Move(lGuid, lBytes[0], 16);
  lGuid := TGUID.NewGuid;
  Move(lGuid, lBytes[16], 16);
  Result := URLSafeB64encode(lBytes, False);
end;

function TMVCOIDCAuthenticationFlow.ComputeS256Challenge(
  const ACodeVerifier: string): string;
begin
  // RFC 7636 section 4.2: BASE64URL-ENCODE(SHA256(ASCII(code_verifier))).
  // The verifier is base64url text, so its UTF-8 and ASCII octets coincide.
  Result := URLSafeB64encode(
    THashSHA2.GetHashBytes(ACodeVerifier, THashSHA2.TSHA2Version.SHA256),
    False);
end;

function TMVCOIDCAuthenticationFlow.Base64URLDecode(
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

function TMVCOIDCAuthenticationFlow.CreateSessionJWT(
  const ARoles: TList<string>;
  const ASessionData: TDictionary<string, string>): string;
var
  lJWT: TJWT;
  lPair: TPair<string, string>;
begin
  lJWT := TJWT.Create(fJWTSecret, fLeewaySeconds, fHMACAlgorithm);
  try
    lJWT.RegClaimsToChecks := fClaimsToChecks;
    // Standard registered claims
    lJWT.Claims.Issuer := 'DMVCFramework.OIDC';
    lJWT.Claims.ExpirationTime := Now + (fJWTExpirationMinutes * OneMinute);
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

procedure TMVCOIDCAuthenticationFlow.SetSessionCookie(
  AContext: TWebContext; const AToken: string; AExpires: TDateTime);
var
  lCookie: TCookie;
  lSameSite: string;
begin
  lCookie := AContext.Response.Cookies.Add;
  lCookie.Name := fCookieName;
  lCookie.Value := AToken;
  lCookie.Path := fCookiePath;
  lCookie.Expires := AExpires;
  lCookie.HttpOnly := True;
  lCookie.Secure := fCookieSecure;
  if not fCookieDomain.IsEmpty then
    lCookie.Domain := fCookieDomain;
  case fCookieSameSite of
    ssStrict: lSameSite := 'Strict';
    ssLax: lSameSite := 'Lax';
    ssNone: lSameSite := 'None';
  end;
  {$IF CompilerVersion >= 35.0}  // TCookie.SameSite exists since 10.4.2, but CompilerVersion is 34.0 for the whole 10.4 line: gate at 11
  lCookie.SameSite := lSameSite;
  {$ENDIF}
end;

procedure TMVCOIDCAuthenticationFlow.ClearSessionCookie(
  AContext: TWebContext);
begin
  SetSessionCookie(AContext, '', EncodeDate(1970, 1, 1));
end;

function TMVCOIDCAuthenticationFlow.GetTokenFromCookie(
  AContext: TWebContext): string;
begin
  Result := Trim(TNetEncoding.URL.Decode(AContext.Request.Cookie(fCookieName)));
end;

function TMVCOIDCAuthenticationFlow.NeedsTokenRefresh(
  const AJWT: TJWT): Boolean;
var
  lWillExpireIn: Int64;
begin
  lWillExpireIn := SecondsBetween(Now, AJWT.Claims.ExpirationTime);
  Result := lWillExpireIn <= AJWT.LiveValidityWindowInSeconds;
end;

{ State cookie for CSRF protection }

procedure TMVCOIDCAuthenticationFlow.SetStateCookie(
  AContext: TWebContext; const AState, ANonce, ACodeVerifier: string);
var
  lCookie: TCookie;
begin
  lCookie := AContext.Response.Cookies.Add;
  lCookie.Name := fCookieName + '_state';
  lCookie.Value := AState + '|' + ANonce + '|' + ACodeVerifier;
  lCookie.Path := fCookiePath;
  lCookie.Expires := Now + (10 * OneMinute);
  lCookie.HttpOnly := True;
  lCookie.Secure := fCookieSecure;
  {$IF CompilerVersion >= 35.0}  // TCookie.SameSite exists since 10.4.2, but CompilerVersion is 34.0 for the whole 10.4 line: gate at 11
  lCookie.SameSite := 'Lax';
  {$ENDIF}
end;

procedure TMVCOIDCAuthenticationFlow.GetAndClearStateCookie(
  AContext: TWebContext; out AState, ANonce, ACodeVerifier: string);
var
  lCookie: TCookie;
  lValue: string;
  lParts: TArray<string>;
begin
  lValue := Trim(TNetEncoding.URL.Decode(
    AContext.Request.Cookie(fCookieName + '_state')));
  lParts := lValue.Split(['|']);
  ACodeVerifier := '';
  if Length(lParts) >= 2 then
  begin
    AState := lParts[0];
    ANonce := lParts[1];
    if Length(lParts) >= 3 then
      ACodeVerifier := lParts[2];
  end
  else
  begin
    // Malformed or missing state cookie: set empty values. The nonce
    // validation in HandleCallback will reject this.
    AState := lValue;
    ANonce := '';
  end;
  // Clear the state cookie by setting it to expired
  lCookie := AContext.Response.Cookies.Add;
  lCookie.Name := fCookieName + '_state';
  lCookie.Value := '';
  lCookie.Path := fCookiePath;
  lCookie.Expires := EncodeDate(1970, 1, 1);
  lCookie.HttpOnly := True;
  lCookie.Secure := fCookieSecure;
  {$IF CompilerVersion >= 35.0}  // TCookie.SameSite exists since 10.4.2, but CompilerVersion is 34.0 for the whole 10.4 line: gate at 11
  lCookie.SameSite := 'Lax';
  {$ENDIF}
end;

{ Route handlers }

procedure TMVCOIDCAuthenticationFlow.HandleLogin(
  AContext: TWebContext; var AHandled: Boolean);
var
  lState: string;
  lNonce: string;
  lCodeVerifier: string;
  lAuthURL: string;
begin
  DiscoverEndpoints;
  // Generate CSRF state and nonce as stripped GUIDs
  lState := TGUID.NewGuid.ToString.Replace('{', '').Replace('}', '');
  lNonce := TGUID.NewGuid.ToString.Replace('{', '').Replace('}', '');
  if fUsePKCE then
    lCodeVerifier := GenerateCodeVerifier
  else
    lCodeVerifier := '';
  SetStateCookie(AContext, lState, lNonce, lCodeVerifier);
  lAuthURL := BuildAuthorizationURL(lState, lNonce, lCodeVerifier);
  AContext.Response.StatusCode := HTTP_STATUS.Found;
  AContext.Response.SetCustomHeader('Location', lAuthURL);
  AHandled := True;
end;

procedure TMVCOIDCAuthenticationFlow.HandleCallback(
  AContext: TWebContext; var AHandled: Boolean);
var
  lCode: string;
  lState: string;
  lExpectedState: string;
  lExpectedNonce: string;
  lCodeVerifier: string;
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
    AContext.Response.SetCustomHeader('Location', fPostLogoutRedirectURL);
    AHandled := True;
    Exit;
  end;

  // Verify CSRF state and extract stored nonce
  GetAndClearStateCookie(AContext, lExpectedState, lExpectedNonce,
    lCodeVerifier);
  if not SameText(lState, lExpectedState) then
  begin
    LogW(Format(SOIDCCallbackStateMismatch, [lExpectedState, lState]));
    AContext.Response.StatusCode := HTTP_STATUS.Found;
    AContext.Response.SetCustomHeader('Location', fPostLogoutRedirectURL);
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
      lTokenResponse := ExchangeCodeForTokens(lCode, lCodeVerifier);

      // Validate the ID token
      lIDTokenClaims := ValidateIDToken(lTokenResponse.S['id_token']);

      // Validate nonce to prevent replay attacks (OIDC Core 3.1.3.7)
      // A missing nonce means the state cookie was lost or tampered with
      if lExpectedNonce.IsEmpty then
        raise EMVCException.Create(
          'OIDC: Missing state cookie nonce - cannot verify replay protection. ' +
          'Possible cookie blocking or replay attack.');
      if lIDTokenClaims.S['nonce'] <> lExpectedNonce then
        raise EMVCException.CreateFmt(SOIDCCallbackNonceMismatch,
          [lExpectedNonce, lIDTokenClaims.S['nonce']]);

      // Optionally fetch UserInfo
      lAccessToken := lTokenResponse.S['access_token'];
      if fFetchUserInfo and (not lAccessToken.IsEmpty) and
         (not fUserinfoEndpoint.IsEmpty) then
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
      if Assigned(fOnUserAuthenticated) then
        fOnUserAuthenticated(AContext, lIDTokenClaims, lUserInfo,
          lRoles, lSessionData);

      // Create JWT session and set cookie
      lSessionJWT := CreateSessionJWT(lRoles, lSessionData);
      SetSessionCookie(AContext, lSessionJWT,
        Now + (fJWTExpirationMinutes * OneMinute));

      LogI(SOIDCUserLoggedIn);
      AContext.Response.StatusCode := HTTP_STATUS.Found;
      AContext.Response.SetCustomHeader('Location', fPostLoginRedirectURL);
      AHandled := True;
    except
      on E: Exception do
      begin
        LogE(Format(SOIDCCallbackError, [E.Message]));
        ClearSessionCookie(AContext);
        AContext.Response.StatusCode := HTTP_STATUS.Found;
        AContext.Response.SetCustomHeader('Location', fPostLogoutRedirectURL);
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

procedure TMVCOIDCAuthenticationFlow.HandleLogout(
  AContext: TWebContext; var AHandled: Boolean);
var
  lRedirectURL: string;
begin
  ClearSessionCookie(AContext);
  LogI(SOIDCUserLoggedOut);
  try
    DiscoverEndpoints;
  except
    on E: Exception do
      LogW('OIDC: Could not discover end_session_endpoint for logout: ' + E.Message);
  end;

  if not fEndSessionEndpoint.IsEmpty then
  begin
    lRedirectURL := fEndSessionEndpoint +
      '?post_logout_redirect_uri=' + TNetEncoding.URL.Encode(
        fBaseURL + fPostLogoutRedirectURL);
    AContext.Response.StatusCode := HTTP_STATUS.Found;
    AContext.Response.SetCustomHeader('Location', lRedirectURL);
  end
  else
  begin
    AContext.Response.StatusCode := HTTP_STATUS.Found;
    AContext.Response.SetCustomHeader('Location', fPostLogoutRedirectURL);
  end;
  AHandled := True;
end;

procedure TMVCOIDCAuthenticationFlow.HandleAuthURL(const AContext: TWebContext; var AHandled: Boolean);
var
  lPathInfo: string;
begin
  lPathInfo := AContext.Request.PathInfo; // read once: on WebBroker hosts every read copies the URL
  if SameText(lPathInfo, fLoginURL) then
    HandleLogin(AContext, AHandled)
  else if SameText(lPathInfo, fCallbackURL) then
    HandleCallback(AContext, AHandled)
  else if SameText(lPathInfo, fLogoutURL) then
    HandleLogout(AContext, AHandled);
end;

procedure TMVCOIDCAuthenticationFlow.CheckSession(const AContext: TWebContext;
  const AControllerQualifiedClassName, AActionName: string; var AHandled: Boolean);
var
  lAuthRequired: Boolean;
  lJWTValue: TJWT;
  lAuthToken: string;
  lErrorMsg: string;
begin
  // Determine whether this action requires authentication
  lAuthRequired := True;
  if Assigned(fOnAuthRequired) then
    fOnAuthRequired(AContext, AControllerQualifiedClassName, AActionName,
      lAuthRequired);

  if not lAuthRequired then
  begin
    AHandled := False;
    // Optionally populate LoggedUser for public routes that check IsValid
    lAuthToken := GetTokenFromCookie(AContext);
    if not lAuthToken.IsEmpty then
    begin
      lJWTValue := TJWT.Create(fJWTSecret, fLeewaySeconds, fHMACAlgorithm);
      try
        lJWTValue.RegClaimsToChecks := fClaimsToChecks;
        if lJWTValue.LoadToken(lAuthToken, lErrorMsg) then
        begin
          AContext.LoggedUser.Clear;
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
  lAuthToken := GetTokenFromCookie(AContext);
  if lAuthToken.IsEmpty then
  begin
    // No token: redirect to login (not 401 for browser-based flows)
    AContext.Response.StatusCode := HTTP_STATUS.Found;
    AContext.Response.SetCustomHeader('Location', fLoginURL);
    AHandled := True;
    Exit;
  end;

  lJWTValue := TJWT.Create(fJWTSecret, fLeewaySeconds, fHMACAlgorithm);
  try
    lJWTValue.RegClaimsToChecks := fClaimsToChecks;
    if not lJWTValue.LoadToken(lAuthToken, lErrorMsg) then
    begin
      // Invalid or expired token: redirect to login
      ClearSessionCookie(AContext);
      AContext.Response.StatusCode := HTTP_STATUS.Found;
      AContext.Response.SetCustomHeader('Location', fLoginURL);
      AHandled := True;
      Exit;
    end;

    // Verify that the session has a username
    if lJWTValue.CustomClaims['username'].IsEmpty then
    begin
      ClearSessionCookie(AContext);
      AContext.Response.StatusCode := HTTP_STATUS.Found;
      AContext.Response.SetCustomHeader('Location', fLoginURL);
      AHandled := True;
      Exit;
    end;

    // Populate the LoggedUser from the validated JWT
    AContext.LoggedUser.Clear;
    AContext.LoggedUser.UserName := lJWTValue.CustomClaims['username'];
    AContext.LoggedUser.Roles.AddRange(
      lJWTValue.CustomClaims['roles'].Split([',']));
    AContext.LoggedUser.LoggedSince := lJWTValue.Claims.IssuedAt;
    AContext.LoggedUser.CustomData := lJWTValue.CustomClaims.AsCustomData;
    // Restore 'sub' - JWT LoadToken classifies it as a registered claim,
    // but the OIDC callback stores it as session data
    if not lJWTValue.Claims.Subject.IsEmpty then
      AContext.LoggedUser.CustomData.AddOrSetValue('sub', lJWTValue.Claims.Subject);
    // Restore 'user_id' - CreateSessionJWT maps it to the mandatory 'username' claim (checked above)
    AContext.LoggedUser.CustomData.AddOrSetValue('user_id', lJWTValue.CustomClaims['username']);

    // Handle token refresh if LiveValidityWindowInSeconds is configured
    if lJWTValue.LiveValidityWindowInSeconds > 0 then
    begin
      if NeedsTokenRefresh(lJWTValue) then
      begin
        // Slide the session by the full configured duration
        lJWTValue.Claims.ExpirationTime := Now + (fJWTExpirationMinutes * OneMinute);
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

end.
