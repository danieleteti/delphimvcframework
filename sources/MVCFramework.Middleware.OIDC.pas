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

{$WARN SYMBOL_DEPRECATED OFF} // the classes are deprecated for their users, not inside their own unit

interface

uses
  MVCFramework,
  MVCFramework.JWT,
  MVCFramework.HMAC,
  MVCFramework.Filters.OIDC,
  MVCFramework.Filters.OIDC.Internal;

type
  // moved to MVCFramework.Filters.OIDC
  TMVCOIDCUserAuthenticatedProc = MVCFramework.Filters.OIDC.TMVCOIDCUserAuthenticatedProc;
  TMVCOIDCAuthRequiredProc = MVCFramework.Filters.OIDC.TMVCOIDCAuthRequiredProc;

  /// <summary>
  /// OIDC Authorization Code flow middleware with JWT cookie sessions.
  /// Handles login redirect, callback token exchange, logout, and
  /// per-request authentication gating with automatic token refresh.
  /// </summary>
  TMVCOIDCAuthenticationMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    fFlow: IMVCOIDCAuthenticationFlow;
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
    /// <summary>
    /// Enables or disables PKCE (RFC 7636, S256) on the authorization code
    /// flow. Default: True. A provider that does not support PKCE ignores the
    /// extra parameters, so there is normally no reason to turn this off.
    /// </summary>
    function SetPKCE(AUse: Boolean): TMVCOIDCAuthenticationMiddleware;
    /// <summary>
    /// Sets the application base URL used to construct absolute redirect URIs
    /// for OIDC provider logout (e.g. 'https://app.example.com').
    /// </summary>
    function SetBaseURL(const AURL: string): TMVCOIDCAuthenticationMiddleware;
    /// <summary>
    /// Sets a JWKS provider for ID token signature verification.
    /// When set, the middleware verifies the cryptographic signature of ID tokens
    /// against the provider's public keys (fetched from the JWKS endpoint).
    /// Without this, the middleware relies on TLS trust to the token endpoint.
    ///
    /// Usage (requires MVCFramework.OIDC.JWKS in your uses clause):
    ///   UseOIDCAuthentication(...)
    ///     .SetJWKSProvider(TMVCJWKSClient.CreateFromIssuer('https://...'));
    /// </summary>
    function SetJWKSProvider(const AProvider: IJWKSProvider): TMVCOIDCAuthenticationMiddleware;
  end deprecated 'Use the OIDC filter bundle (MVCFramework.Filters.OIDC): AEngine.Use(OIDCAuthenticationBundle(...))';

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
      deprecated 'Use the OIDC filter bundle (MVCFramework.Filters.OIDC): AEngine.Use(OIDCAuthenticationBundle(...))';


implementation

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
  fFlow := NewOIDCAuthenticationFlow(AOnUserAuthenticated, AOnAuthRequired,
    AOIDCIssuer, AClientId, AClientSecret, ARedirectUri, AJWTSecret,
    ALoginURL, ACallbackURL, ALogoutURL, APostLoginRedirectURL, APostLogoutRedirectURL, AScopes,
    AJWTExpirationMinutes, AClaimsToCheck, ALeewaySeconds, AHMACAlgorithm);
end;

{ Fluent cookie setters }

function TMVCOIDCAuthenticationMiddleware.SetCookieName(
  const AName: string): TMVCOIDCAuthenticationMiddleware;
begin
  fFlow.SetCookieName(AName);
  Result := Self;
end;

function TMVCOIDCAuthenticationMiddleware.SetCookieSecure(
  ASecure: Boolean): TMVCOIDCAuthenticationMiddleware;
begin
  fFlow.SetCookieSecure(ASecure);
  Result := Self;
end;

function TMVCOIDCAuthenticationMiddleware.SetCookieSameSite(
  ASameSite: TMVCJWTCookieSameSite): TMVCOIDCAuthenticationMiddleware;
begin
  fFlow.SetCookieSameSite(ASameSite);
  Result := Self;
end;

function TMVCOIDCAuthenticationMiddleware.SetCookiePath(
  const APath: string): TMVCOIDCAuthenticationMiddleware;
begin
  fFlow.SetCookiePath(APath);
  Result := Self;
end;

function TMVCOIDCAuthenticationMiddleware.SetCookieDomain(
  const ADomain: string): TMVCOIDCAuthenticationMiddleware;
begin
  fFlow.SetCookieDomain(ADomain);
  Result := Self;
end;

function TMVCOIDCAuthenticationMiddleware.SetFetchUserInfo(
  AFetch: Boolean): TMVCOIDCAuthenticationMiddleware;
begin
  fFlow.SetFetchUserInfo(AFetch);
  Result := Self;
end;

function TMVCOIDCAuthenticationMiddleware.SetPKCE(
  AUse: Boolean): TMVCOIDCAuthenticationMiddleware;
begin
  fFlow.SetPKCE(AUse);
  Result := Self;
end;

function TMVCOIDCAuthenticationMiddleware.SetBaseURL(
  const AURL: string): TMVCOIDCAuthenticationMiddleware;
begin
  fFlow.SetBaseURL(AURL);
  Result := Self;
end;

function TMVCOIDCAuthenticationMiddleware.SetJWKSProvider(
  const AProvider: IJWKSProvider): TMVCOIDCAuthenticationMiddleware;
begin
  fFlow.SetJWKSProvider(AProvider);
  Result := Self;
end;

{ IMVCMiddleware }

procedure TMVCOIDCAuthenticationMiddleware.OnBeforeRouting(
  AContext: TWebContext; var AHandled: Boolean);
begin
  fFlow.HandleAuthURL(AContext, AHandled);
end;

procedure TMVCOIDCAuthenticationMiddleware.OnBeforeControllerAction(
  AContext: TWebContext;
  const AControllerQualifiedClassName, AActionName: string;
  var AHandled: Boolean);
begin
  fFlow.CheckSession(AContext, AControllerQualifiedClassName, AActionName, AHandled);
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
