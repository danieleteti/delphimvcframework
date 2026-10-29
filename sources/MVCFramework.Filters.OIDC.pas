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
// OIDC as a filter bundle. A companion of MVCFramework.Filters, in a unit of
// its own so that the filters do not depend on the REST client OIDC talks to
// the provider with.

unit MVCFramework.Filters.OIDC;

{$I dmvcframework.inc}

interface

uses
  System.Generics.Collections,
  MVCFramework,
  MVCFramework.JWT,                            // TJWTCheckableClaims, IJWKSProvider, TMVCJWTCookieSameSite
  MVCFramework.Filters,                        // TMVCAuthCookieOptions
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

  // Options of OIDCAuthenticationBundle, the parameters of UseOIDCAuthentication
  // and of its SetXxx methods. Start from Default, then set at least Issuer,
  // ClientId, ClientSecret, RedirectUri and JWTSecret (the bundle raises when
  // Issuer, ClientId or JWTSecret are empty).
  TMVCOIDCFlag = (
    ofFetchUserInfo, // the claims of the userinfo endpoint are added to those of the ID token
    ofPKCE           // the code flow uses PKCE (RFC 7636)
  );
  TMVCOIDCFlags = set of TMVCOIDCFlag;

  TMVCOIDCOptions = record
    OnUserAuthenticated: TMVCOIDCUserAuthenticatedProc; // roles and session data from the ID token and UserInfo
    OnAuthRequired: TMVCOIDCAuthRequiredProc;           // per controller action; nil: every action requires authentication
    Issuer: string;
    ClientId: string;
    ClientSecret: string;
    RedirectUri: string;
    JWTSecret: string;                                  // signs the session cookie
    LoginURL: string;                                   // '/auth/login'
    CallbackURL: string;                                // '/auth/callback'
    LogoutURL: string;                                  // '/auth/logout'
    PostLoginRedirectURL: string;                       // '/'
    PostLogoutRedirectURL: string;                      // '/auth/login'
    Scopes: string;                                     // 'openid email profile'
    JWTExpirationMinutes: Integer;                      // 480
    ClaimsToCheck: TJWTCheckableClaims;                 // [ExpirationTime]
    LeewaySeconds: Cardinal;                            // 300
    HMACAlgorithm: string;                              // 'HS512'
    Cookie: TMVCAuthCookieOptions;                      // 'dmvc_oidc_session', Secure, SameSite=Lax, '/'
    Flags: TMVCOIDCFlags;                               // [ofFetchUserInfo, ofPKCE]
    BaseURL: string;                                    // e.g. 'https://app.example.com', for the logout at the provider
    JWKSProvider: IJWKSProvider;                        // nil: the ID token signature is not verified (TLS trust)
    class function Default: TMVCOIDCOptions; static;
  end;

// The OIDC authorization code flow.
//
// HTTP part, on the engine: a request to LoginURL is redirected to the
// provider (state, nonce and PKCE in a short-lived cookie); CallbackURL
// exchanges the code, validates the ID token, calls OnUserAuthenticated and
// sets the session cookie, then redirects to PostLoginRedirectURL; LogoutURL
// clears the cookie and redirects to the provider's end_session_endpoint (or
// to PostLogoutRedirectURL). Every other request goes on.
// Endpoint part: the check. Without a valid session cookie the request is
// redirected (302) to LoginURL; with one, Context.LoggedUser gets the user,
// the roles and the session data. A controller action that OnAuthRequired
// makes public still gets the user when the cookie is there. On a Minimal API
// route OnAuthRequired is not asked (see "Authentication" in
// MVCFramework.Filters). JWTSecret is checked as the JWT bundles check theirs.
//
//   lOIDC := TMVCOIDCOptions.Default;
//   lOIDC.Issuer := 'https://idp.example.com';
//   ...
//   AEngine.Use(OIDCAuthenticationBundle(lOIDC));
function OIDCAuthenticationBundle(const AOptions: TMVCOIDCOptions): IMVCFilterBundle;

implementation

uses
  MVCFramework.HMAC,
  MVCFramework.Filters.OIDC.Internal;          // IMVCOIDCAuthenticationFlow, NewOIDCAuthenticationFlow

class function TMVCOIDCOptions.Default: TMVCOIDCOptions;
begin
  Result.OnUserAuthenticated := nil;
  Result.OnAuthRequired := nil;
  Result.Issuer := '';
  Result.ClientId := '';
  Result.ClientSecret := '';
  Result.RedirectUri := '';
  Result.JWTSecret := '';
  Result.LoginURL := '/auth/login';
  Result.CallbackURL := '/auth/callback';
  Result.LogoutURL := '/auth/logout';
  Result.PostLoginRedirectURL := '/';
  Result.PostLogoutRedirectURL := '/auth/login';
  Result.Scopes := 'openid email profile';
  Result.JWTExpirationMinutes := 480;
  Result.ClaimsToCheck := [TJWTCheckableClaim.ExpirationTime];
  Result.LeewaySeconds := 300;
  Result.HMACAlgorithm := HMAC_HS512;
  Result.Cookie := TMVCAuthCookieOptions.Default;
  Result.Cookie.Name := 'dmvc_oidc_session';
  Result.Cookie.SameSite := ssLax; // the provider comes back with a top-level cross-site redirect
  Result.Flags := [ofFetchUserInfo, ofPKCE];
  Result.BaseURL := '';
  Result.JWKSProvider := nil;
end;

function NewOIDCFlow(const AOptions: TMVCOIDCOptions;
  const AOnAuthRequired: TMVCOIDCAuthRequiredProc): IMVCOIDCAuthenticationFlow;
begin
  // it signs the session cookie: the flow refuses only an empty one
  CheckJWTSecret(AOptions.JWTSecret);
  Result := NewOIDCAuthenticationFlow(AOptions.OnUserAuthenticated, AOnAuthRequired,
    AOptions.Issuer, AOptions.ClientId, AOptions.ClientSecret, AOptions.RedirectUri, AOptions.JWTSecret,
    AOptions.LoginURL, AOptions.CallbackURL, AOptions.LogoutURL,
    AOptions.PostLoginRedirectURL, AOptions.PostLogoutRedirectURL, AOptions.Scopes,
    AOptions.JWTExpirationMinutes, AOptions.ClaimsToCheck, AOptions.LeewaySeconds, AOptions.HMACAlgorithm);
  // the flow refuses SameSite=None without Secure in either order of the two setters
  Result.SetCookieName(AOptions.Cookie.Name);
  Result.SetCookieSameSite(AOptions.Cookie.SameSite);
  Result.SetCookieSecure(AOptions.Cookie.Secure);
  Result.SetCookiePath(AOptions.Cookie.Path);
  Result.SetCookieDomain(AOptions.Cookie.Domain);
  Result.SetFetchUserInfo(ofFetchUserInfo in AOptions.Flags);
  Result.SetPKCE(ofPKCE in AOptions.Flags);
  Result.SetBaseURL(AOptions.BaseURL);
  if Assigned(AOptions.JWKSProvider) then
    Result.SetJWKSProvider(AOptions.JWKSProvider);
end;

function OIDCLoginHTTPFilter(const AOptions: TMVCOIDCOptions): TMVCHTTPFilter;
var
  lFlow: IMVCOIDCAuthenticationFlow;
begin
  lFlow := NewOIDCFlow(AOptions, nil);
  Result :=
    procedure(const AContext: TWebContext; const ANext: TMVCHTTPFilterNext)
    var
      lHandled: Boolean;
    begin
      lHandled := False;
      lFlow.HandleAuthURL(AContext, lHandled);
      if not lHandled then
        ANext();
    end;
end;

// OnAuthRequired for the controllers only. A function of its own: a closure built in OIDCEndpointFilter would
// share the frame of the filter, which holds the flow, which holds the closure, and none of them would be freed.
function ControllersOnly(const AOnAuthRequired: TMVCOIDCAuthRequiredProc): TMVCOIDCAuthRequiredProc;
begin
  Result :=
    procedure(const AContext: TWebContext; const AControllerQualifiedClassName: string;
      const AActionName: string; var AAuthenticationRequired: Boolean)
    begin
      // a Minimal API route has no controller: the filter on its group asks for authentication
      if (AControllerQualifiedClassName <> '') and Assigned(AOnAuthRequired) then
        AOnAuthRequired(AContext, AControllerQualifiedClassName, AActionName, AAuthenticationRequired);
    end;
end;

// a request without the session gets the redirect to the login
function OIDCEndpointFilter(const AOptions: TMVCOIDCOptions): TMVCEndpointFilter;
var
  lFlow: IMVCOIDCAuthenticationFlow;
begin
  lFlow := NewOIDCFlow(AOptions, ControllersOnly(AOptions.OnAuthRequired));
  Result :=
    function(const AContext: TWebContext; const ANext: TMVCEndpointFilterNext): IMVCResponse
    var
      lHandled: Boolean;
    begin
      lHandled := False;
      lFlow.CheckSession(AContext, AContext.ControllerQualifiedClassName, AContext.ActionName, lHandled);
      if lHandled then
        Exit(nil); // the redirect is written
      Result := ANext();
    end;
end;

function OIDCAuthenticationBundle(const AOptions: TMVCOIDCOptions): IMVCFilterBundle;
begin
  Result := TMVCFilterBundle.Create([OIDCLoginHTTPFilter(AOptions)], [OIDCEndpointFilter(AOptions)]);
end;

end.
