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

unit AuthFiltersTestsU;

// The authentication filter bundles of MVCFramework.Filters.Authentication (JWTAuthenticationBundle, with blacklist and
// refresh token, JWTCookieAuthenticationBundle, CustomAuthenticationBundle; BasicAuthenticationBundle is in MultiAuthTestsU)
// and MVCFramework.Filters.OIDC (OIDCAuthenticationBundle) against the classic middleware they replace.
//
// Three engines per group of features, the same controllers in each (AuthFilterSupportU, TestServer folder):
// one with the classic middleware on Indy Direct, one with the bundles on Indy Direct, one with the bundles
// on HTTP.sys. The tests that take a port run against them: the bundle has to answer as the middleware does.
// The engines with the bundles also have Minimal API routes behind bundles on their groups. The TestServer on
// 8888, whatever host the suite runs against (WebBroker, Indy Direct, HTTP.sys, ISAPI, Apache), has the JWT and
// the OIDC bundles too.
//
//   JWT     JWT header + blacklist + JWT cookie        8951 (middleware) 8952 (bundles) 8953 (bundles, HTTP.sys)
//           + refresh token (bundles only)
//   Custom  custom authentication + role based         8954              8955           8956
//   OIDC    OIDC, with a fake provider on the engine   8957              8958           8959
//   Own     an engine-wide JWT bundle without a login                     8946
//           and a login action of the application

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Net.HttpClient,
  System.Net.URLClient,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Server.Intf;

const
  AF_JWT_MW = 8951;
  AF_JWT_F = 8952;
  AF_JWT_HS = 8953;
  AF_CUSTOM_MW = 8954;
  AF_CUSTOM_F = 8955;
  AF_CUSTOM_HS = 8956;
  AF_OIDC_MW = 8957;
  AF_OIDC_F = 8958;
  AF_OIDC_HS = 8959;
  AF_OWN_LOGIN = 8946;

type
  TAFConfigure = procedure(const AEngine: TMVCEngine; const AWithFilters: Boolean);

  [TestFixture]
  TAuthFiltersTests = class
  private
    fServers: TList<IMVCServer>;
    fEngines: TObjectList<TMVCEngine>;
    procedure Start(const APort: Integer; const AWithFilters: Boolean; const AConfigure: TAFConfigure;
      const AHttpSys: Boolean);
  public
    [SetupFixture]
    procedure SetupFixture;
    [TeardownFixture]
    procedure TeardownFixture;

    // --- JWT in the Authorization header
    [Test]
    [TestCase('filters, TestServer', '8888')]
    [TestCase('middleware', '8951')]
    [TestCase('filters', '8952')]
    [TestCase('filters, HTTP.sys', '8953')]
    procedure JWTLoginGivesAToken(const APort: Integer);
    [Test]
    [TestCase('filters, TestServer', '8888')]
    [TestCase('middleware', '8951')]
    [TestCase('filters', '8952')]
    [TestCase('filters, HTTP.sys', '8953')]
    procedure JWTLoginWithWrongCredentialsIs401(const APort: Integer);
    [Test]
    [TestCase('filters, TestServer', '8888')]
    [TestCase('middleware', '8951')]
    [TestCase('filters', '8952')]
    [TestCase('filters, HTTP.sys', '8953')]
    procedure JWTPrivateNeedsTheToken(const APort: Integer);
    [Test]
    [TestCase('filters, TestServer', '8888')]
    [TestCase('middleware', '8951')]
    [TestCase('filters', '8952')]
    [TestCase('filters, HTTP.sys', '8953')]
    procedure JWTAdminIsAuthorizedByAction(const APort: Integer);
    [Test]
    [TestCase('filters, TestServer', '8888')]
    [TestCase('middleware', '8951')]
    [TestCase('filters', '8952')]
    [TestCase('filters, HTTP.sys', '8953')]
    procedure JWTPublicActionKnowsTheUserWhenATokenIsSent(const APort: Integer);
    [Test]
    [TestCase('filters, TestServer', '8888')]
    [TestCase('middleware', '8951')]
    [TestCase('filters', '8952')]
    [TestCase('filters, HTTP.sys', '8953')]
    procedure JWTHandlerGetsTheControllerAndTheAction(const APort: Integer);
    [Test]
    [TestCase('filters, TestServer', '8888')]
    [TestCase('middleware', '8951')]
    [TestCase('filters', '8952')]
    [TestCase('filters, HTTP.sys', '8953')]
    procedure JWTBlackListedTokenIsRefused(const APort: Integer);
    [Test]
    [TestCase('filters, TestServer', '8888')]
    [TestCase('filters', '8952')]
    [TestCase('filters, HTTP.sys', '8953')]
    procedure JWTBlackListReadsTheTokenOfTheParameter(const APort: Integer);
    // the token of one side is accepted by the other: same claims, same signature
    [Test]
    procedure JWTTokensOfMiddlewareAndFilterAreInterchangeable;
    [Test]
    [TestCase('filters, TestServer', '8888')]
    [TestCase('filters', '8952')]
    [TestCase('filters, HTTP.sys', '8953')]
    procedure JWTFilterTakesTheTokenFromTheHeaderOnlyByDefault(const APort: Integer);
    [Test]
    [TestCase('filters, TestServer', '8888')]
    [TestCase('filters', '8952')]
    [TestCase('filters, HTTP.sys', '8953')]
    procedure JWTFilterTakesTheTokenFromTheParameterWhenAsked(const APort: Integer);
    [Test]
    [TestCase('filters, TestServer', '8888')]
    [TestCase('filters', '8952')]
    [TestCase('filters, HTTP.sys', '8953')]
    procedure JWTFilterHonoursTheAlgorithm(const APort: Integer);
    // a Minimal API route behind the filter: OnRequest is not asked, OnAuthorization is
    [Test]
    [TestCase('filters, TestServer', '8888')]
    [TestCase('filters', '8952')]
    [TestCase('filters, HTTP.sys', '8953')]
    procedure JWTMinimalRouteAlwaysNeedsTheToken(const APort: Integer);
    [Test]
    [TestCase('filters, TestServer', '8888')]
    [TestCase('filters', '8952')]
    [TestCase('filters, HTTP.sys', '8953')]
    procedure JWTMinimalRouteIsAuthorizedByOnAuthorization(const APort: Integer);
    [Test]
    procedure JWTBundlesRefuseTheShippedSecretAndAMissingHandler;
    // the login of a bundle on a group answers though no route matches it; the check covers the group only
    [Test]
    [TestCase('filters, TestServer', '8888')]
    [TestCase('filters', '8952')]
    [TestCase('filters, HTTP.sys', '8953')]
    procedure JWTBundleOnAGroupPutsTheLoginOnTheEngine(const APort: Integer);
    // with the refresh token the login answers the pair the refresh URL answers
    [Test]
    [TestCase('filters, TestServer', '8888')]
    [TestCase('filters', '8952')]
    [TestCase('filters, HTTP.sys', '8953')]
    procedure JWTRefreshLoginAnswersBothTokens(const APort: Integer);
    [Test]
    [TestCase('filters, TestServer', '8888')]
    [TestCase('filters', '8952')]
    [TestCase('filters, HTTP.sys', '8953')]
    procedure JWTRefreshTokenIsRotated(const APort: Integer);
    [Test]
    [TestCase('filters, TestServer', '8888')]
    [TestCase('filters', '8952')]
    [TestCase('filters, HTTP.sys', '8953')]
    procedure JWTLogoutRevokesTheRefreshTokenOfItsBody(const APort: Integer);
    // LoginURL '': no login in the bundle, it accepts and refreshes the tokens of a login of the application
    [Test]
    [TestCase('filters, TestServer', '8888')]
    [TestCase('filters', '8952')]
    [TestCase('filters, HTTP.sys', '8953')]
    procedure JWTBundleWithoutALoginAcceptsTheTokensOfYourOwnLogin(const APort: Integer);
    [Test]
    procedure JWTBundleWithoutALoginHasNoLoginFilter;
    // the bundle on the engine, the login a controller action the handler makes public
    [Test]
    procedure JWTEngineWideBundleWithALoginActionOfYourOwn;
    // the bundle gives the refresh token its own signer, whatever the secret
    [Test]
    procedure RefreshTokenSignsWithTheSignerOfItsConfig;
    // the check filters validate "exp" unless told otherwise; the middleware does when told
    [Test]
    [TestCase('middleware', '8951')]
    [TestCase('filters, TestServer', '8888')]
    [TestCase('filters', '8952')]
    [TestCase('filters, HTTP.sys', '8953')]
    procedure JWTExpiredTokenIsRefused(const APort: Integer);
    // a refused login is raised by an HTTP filter: the request still ends (OnAfterRouting, session)
    [Test]
    [TestCase('middleware', '8951')]
    [TestCase('filters, TestServer', '8888')]
    [TestCase('filters', '8952')]
    [TestCase('filters, HTTP.sys', '8953')]
    procedure ARefusedLoginStillEndsTheRequest(const APort: Integer);
    // two JWT filters on one request each give LoggedUser the custom claims: the first are freed
    [Test]
    procedure LoggedUserFreesTheCustomDataItReplaces;
    [Test]
    [TestCase('middleware', '8951')]
    [TestCase('filters, TestServer', '8888')]
    [TestCase('filters', '8952')]
    [TestCase('filters, HTTP.sys', '8953')]
    procedure JWTTokenAboutToExpireIsRenewed(const APort: Integer);
    // a public action reads the token of the parameter too (the middleware dropped it there)
    [Test]
    [TestCase('middleware', '8951')]
    [TestCase('filters, TestServer', '8888')]
    [TestCase('filters', '8952')]
    [TestCase('filters, HTTP.sys', '8953')]
    procedure JWTPublicActionReadsTheTokenOfTheParameter(const APort: Integer);

    // --- JWT in a cookie
    [Test]
    [TestCase('filters, TestServer', '8888')]
    [TestCase('middleware', '8951')]
    [TestCase('filters', '8952')]
    [TestCase('filters, HTTP.sys', '8953')]
    procedure CookieLoginSetsTheCookieAndThePrivateActionReadsIt(const APort: Integer);
    [Test]
    [TestCase('filters, TestServer', '8888')]
    [TestCase('middleware', '8951')]
    [TestCase('filters', '8952')]
    [TestCase('filters, HTTP.sys', '8953')]
    procedure CookieLogoutExpiresTheCookie(const APort: Integer);
    [Test]
    procedure CookieOptionsAreChecked;
    // the middleware with AllowedOriginHTTPFilter in front of it, the bundle with LoginAllowedOrigins
    [Test]
    [TestCase('filters, TestServer', '8888')]
    [TestCase('middleware', '8951')]
    [TestCase('filters', '8952')]
    [TestCase('filters, HTTP.sys', '8953')]
    procedure CookieLoginRefusesAnOriginNotAllowed(const APort: Integer);

    // --- custom authentication (session) and role based handler
    [Test]
    [TestCase('middleware', '8954')]
    [TestCase('filters', '8955')]
    [TestCase('filters, HTTP.sys', '8956')]
    procedure CustomLoginPrivateLogout(const APort: Integer);
    [Test]
    [TestCase('middleware', '8954')]
    [TestCase('filters', '8955')]
    [TestCase('filters, HTTP.sys', '8956')]
    procedure CustomWithoutLoginIs401AsTheMiddlewareAnswers(const APort: Integer);
    [Test]
    [TestCase('middleware', '8954')]
    [TestCase('filters', '8955')]
    [TestCase('filters, HTTP.sys', '8956')]
    procedure CustomAdminIsAuthorizedByAction(const APort: Integer);
    [Test]
    [TestCase('middleware', '8954')]
    [TestCase('filters', '8955')]
    [TestCase('filters, HTTP.sys', '8956')]
    procedure RoleBasedHandlerReadsTheAttributes(const APort: Integer);
    [Test]
    [TestCase('filters', '8955')]
    [TestCase('filters, HTTP.sys', '8956')]
    procedure RoleBasedHandlerOnAMinimalRoute(const APort: Integer);
    // a controller RTTI cannot find by name: its MVCRequiresRole cannot be read, so it is refused
    [Test]
    [TestCase('middleware', '8954')]
    [TestCase('filters', '8955')]
    [TestCase('filters, HTTP.sys', '8956')]
    procedure RoleBasedHandlerRefusesAControllerItCannotFind(const APort: Integer);

    // --- OIDC
    [Test]
    [TestCase('filters, TestServer', '8888')]
    [TestCase('middleware', '8957')]
    [TestCase('filters', '8958')]
    [TestCase('filters, HTTP.sys', '8959')]
    procedure OIDCPrivateWithoutSessionRedirectsToTheLogin(const APort: Integer);
    [Test]
    [TestCase('filters, TestServer', '8888')]
    [TestCase('middleware', '8957')]
    [TestCase('filters', '8958')]
    [TestCase('filters, HTTP.sys', '8959')]
    procedure OIDCLoginCallbackAndPrivate(const APort: Integer);
    [Test]
    [TestCase('filters, TestServer', '8888')]
    [TestCase('middleware', '8957')]
    [TestCase('filters', '8958')]
    [TestCase('filters, HTTP.sys', '8959')]
    procedure OIDCCallbackWithAnotherStateIsRefused(const APort: Integer);
    [Test]
    [TestCase('filters, TestServer', '8888')]
    [TestCase('middleware', '8957')]
    [TestCase('filters', '8958')]
    [TestCase('filters, HTTP.sys', '8959')]
    procedure OIDCLogoutClearsTheSession(const APort: Integer);
    [Test]
    [TestCase('filters, TestServer', '8888')]
    [TestCase('filters', '8958')]
    [TestCase('filters, HTTP.sys', '8959')]
    procedure OIDCMinimalRouteNeedsTheSession(const APort: Integer);
  end;

implementation

uses
  System.NetEncoding,
  System.DateUtils,
  JsonDataObjects,
  MVCFramework.JWT,
  MVCFramework.HMAC,
  MVCFramework.Filters,
  MVCFramework.Filters.Authentication,
  MVCFramework.JWT.RefreshToken,
  MVCFramework.Middleware.JWT,
  MVCFramework.Filters.OIDC,
  MVCFramework.Server.Factory,
  AuthFilterSupportU;

// --- HTTP helpers: no cookie jar and no redirects followed, the tests look at both

function NewClient: THTTPClient;
begin
  Result := THTTPClient.Create;
  Result.HandleRedirects := False;
  Result.AllowCookies := False;
end;

function Get(const APort: Integer; const APath: string; const AHeaders: TNetHeaders = nil): IHTTPResponse;
var
  lClient: THTTPClient;
begin
  lClient := NewClient;
  try
    Result := lClient.Get('http://localhost:' + APort.ToString + APath, nil, AHeaders);
    Result.ContentAsString;
  finally
    lClient.Free;
  end;
end;

function Send(const AMethod: string; const APort: Integer; const APath: string; const AHeaders: TNetHeaders;
  const ABody: string = ''; const AContentType: string = ''): IHTTPResponse;
var
  lClient: THTTPClient;
  lReq: IHTTPRequest;
  lBody: TStringStream;
  lHeader: TNameValuePair;
begin
  lClient := NewClient;
  lBody := TStringStream.Create(ABody, TEncoding.UTF8);
  try
    lReq := lClient.GetRequest(AMethod, 'http://localhost:' + APort.ToString + APath);
    for lHeader in AHeaders do
      lReq.AddHeader(lHeader.Name, lHeader.Value);
    if AContentType <> '' then
      lReq.AddHeader('Content-Type', AContentType);
    lReq.SourceStream := lBody;
    Result := lClient.Execute(lReq);
    Result.ContentAsString;
  finally
    lBody.Free;
    lClient.Free;
  end;
end;

function Header(const AName, AValue: string): TNetHeaders;
begin
  Result := [TNameValuePair.Create(AName, AValue)];
end;

function Bearer(const AToken: string): TNetHeaders;
begin
  Result := Header('Authorization', 'Bearer ' + AToken);
end;

function LoginJWT(const APort: Integer; const AUser: string; const APath: string = '/login'): string;
var
  lRes: IHTTPResponse;
  lJSON: TJsonObject;
begin
  lRes := Send('POST', APort, APath, [TNameValuePair.Create('jwtusername', AUser),
    TNameValuePair.Create('jwtpassword', AUser)]);
  Assert.AreEqual(200, lRes.StatusCode, 'login: ' + lRes.ContentAsString);
  lJSON := StrToJSONObject(lRes.ContentAsString);
  try
    Result := lJSON.S['token'];
  finally
    lJSON.Free;
  end;
  Assert.AreNotEqual('', Result, 'no token in ' + lRes.ContentAsString);
end;

// The cookie ACookieName the response sets (THTTPClient moves the Set-Cookie headers into Cookies):
// False when there is none.
function FindCookie(const AResponse: IHTTPResponse; const ACookieName: string; out ACookie: TCookie): Boolean;
var
  lCookie: TCookie;
begin
  for lCookie in AResponse.Cookies do
    if lCookie.Name = ACookieName then
    begin
      ACookie := lCookie;
      Exit(True);
    end;
  Result := False;
end;

function CookiesOf(const AResponse: IHTTPResponse): string;
var
  lCookie: TCookie;
begin
  Result := '';
  for lCookie in AResponse.Cookies do
    Result := Result + lCookie.Name + '=' + lCookie.Value + ' (expires ' + DateTimeToStr(lCookie.Expires) + ') / ';
end;

// the value of the cookie ACookieName the response sets ('' when it sets none); AExpired: the response deletes it
function SetCookieValue(const AResponse: IHTTPResponse; const ACookieName: string; out AExpired: Boolean): string;
var
  lCookie: TCookie;
begin
  Result := '';
  AExpired := False;
  if FindCookie(AResponse, ACookieName, lCookie) then
  begin
    Result := lCookie.Value;
    AExpired := (lCookie.Expires > 0) and (lCookie.Expires < Now);
  end;
end;

function SetCookieValueOf(const AResponse: IHTTPResponse; const ACookieName: string): string;
var
  lExpired: Boolean;
begin
  Result := SetCookieValue(AResponse, ACookieName, lExpired);
end;

// the "username|roles|IsValid" of an action: a controller answers the text, a Minimal API route {"message": text}
function WhoOf(const AResponse: IHTTPResponse): string;
var
  lJSON: TJsonObject;
begin
  Result := AResponse.ContentAsString;
  if Result.StartsWith('{') then
  begin
    lJSON := StrToJSONObject(Result);
    try
      Result := lJSON.S['message'];
    finally
      lJSON.Free;
    end;
  end;
end;

{ TAuthFiltersTests }

procedure TAuthFiltersTests.Start(const APort: Integer; const AWithFilters: Boolean;
  const AConfigure: TAFConfigure; const AHttpSys: Boolean);
var
  lEngine: TMVCEngine;
  lServer: IMVCServer;
begin
  lEngine := TMVCEngine.Create;
  fEngines.Add(lEngine);
  if Assigned(AConfigure) then
    AConfigure(lEngine, AWithFilters)
  else
    AddOIDCAuthentication(lEngine, AWithFilters, 'http://localhost:' + APort.ToString);
  if AHttpSys then
    lServer := TMVCServerFactory.CreateHttpSys(lEngine)
  else
    lServer := TMVCServerFactory.CreateIndyDirect(lEngine);
  lServer.Listen(APort, 'localhost');
  fServers.Add(lServer);
end;

procedure TAuthFiltersTests.SetupFixture;
begin
  fServers := TList<IMVCServer>.Create;
  fEngines := TObjectList<TMVCEngine>.Create(True);
  Start(AF_JWT_MW, False, AddJWTAuthentication, False);
  Start(AF_JWT_F, True, AddJWTAuthentication, False);
  Start(AF_JWT_HS, True, AddJWTAuthentication, True);
  Start(AF_CUSTOM_MW, False, AddCustomAuthentication, False);
  Start(AF_CUSTOM_F, True, AddCustomAuthentication, False);
  Start(AF_CUSTOM_HS, True, AddCustomAuthentication, True);
  Start(AF_OIDC_MW, False, nil, False);
  Start(AF_OIDC_F, True, nil, False);
  Start(AF_OIDC_HS, True, nil, True);
  Start(AF_OWN_LOGIN, True, AddOwnLoginAuthentication, False);
end;

procedure TAuthFiltersTests.TeardownFixture;
var
  lServer: IMVCServer;
begin
  // the servers do not own the engines
  for lServer in fServers do
    lServer.Stop;
  fServers.Free;
  fEngines.Free;
end;

// --- JWT in the Authorization header

procedure TAuthFiltersTests.JWTLoginGivesAToken(const APort: Integer);
var
  lJWT: TJWT;
  lError: string;
begin
  lJWT := TJWT.Create(AF_SECRET);
  try
    lJWT.RegClaimsToChecks := [TJWTCheckableClaim.ExpirationTime, TJWTCheckableClaim.IssuedAt];
    Assert.IsTrue(lJWT.LoadToken(LoginJWT(APort, 'admin'), lError), lError);
    Assert.AreEqual('admin', lJWT.CustomClaims['username']);
    Assert.AreEqual('user,admin', lJWT.CustomClaims['roles']);
    Assert.AreEqual('af', lJWT.Claims.Issuer, 'the claims setup was applied');
  finally
    lJWT.Free;
  end;
end;

procedure TAuthFiltersTests.JWTLoginWithWrongCredentialsIs401(const APort: Integer);
var
  lRes: IHTTPResponse;
begin
  lRes := Send('POST', APort, '/login', [TNameValuePair.Create('jwtusername', 'user'),
    TNameValuePair.Create('jwtpassword', 'wrong')]);
  Assert.AreEqual(401, lRes.StatusCode, lRes.ContentAsString);
  lRes := Send('POST', APort, '/login', nil);
  Assert.AreEqual(401, lRes.StatusCode, 'no credentials: ' + lRes.ContentAsString);
  // a handler that leaves AIsValid alone when it refuses
  lRes := Send('POST', APort, '/login', [TNameValuePair.Create('jwtusername', 'lazy'),
    TNameValuePair.Create('jwtpassword', 'lazy')]);
  Assert.AreEqual(401, lRes.StatusCode, 'unset IsValid: ' + lRes.ContentAsString);
  lRes := Send('POST', APort, '/cookie/login', [TNameValuePair.Create('jwtusername', 'lazy'),
    TNameValuePair.Create('jwtpassword', 'lazy')]);
  Assert.AreEqual(401, lRes.StatusCode, 'unset IsValid, cookie login: ' + lRes.ContentAsString);
end;

procedure TAuthFiltersTests.JWTPrivateNeedsTheToken(const APort: Integer);
var
  lRes: IHTTPResponse;
begin
  Assert.AreEqual(401, Get(APort, '/jwt/private').StatusCode, 'no token');
  Assert.AreEqual(401, Get(APort, '/jwt/private', Bearer('not.a.token')).StatusCode, 'bad token');
  lRes := Get(APort, '/jwt/private', Bearer(LoginJWT(APort, 'user')));
  Assert.AreEqual(200, lRes.StatusCode, lRes.ContentAsString);
  Assert.AreEqual('user|user|True', WhoOf(lRes));
end;

procedure TAuthFiltersTests.JWTAdminIsAuthorizedByAction(const APort: Integer);
var
  lRes: IHTTPResponse;
begin
  lRes := Get(APort, '/jwt/admin', Bearer(LoginJWT(APort, 'user')));
  Assert.AreEqual(403, lRes.StatusCode, lRes.ContentAsString);
  lRes := Get(APort, '/jwt/admin', Bearer(LoginJWT(APort, 'admin')));
  Assert.AreEqual(200, lRes.StatusCode, lRes.ContentAsString);
  Assert.AreEqual('admin|user,admin|True', WhoOf(lRes));
end;

procedure TAuthFiltersTests.JWTPublicActionKnowsTheUserWhenATokenIsSent(const APort: Integer);
var
  lRes: IHTTPResponse;
begin
  lRes := Get(APort, '/jwt/public');
  Assert.AreEqual(200, lRes.StatusCode, lRes.ContentAsString);
  Assert.AreEqual('||False', WhoOf(lRes), 'anonymous');
  lRes := Get(APort, '/jwt/public', Bearer(LoginJWT(APort, 'user')));
  Assert.AreEqual(200, lRes.StatusCode, lRes.ContentAsString);
  Assert.AreEqual('user|user|True', WhoOf(lRes), 'the token is read on a public action too');
  lRes := Get(APort, '/jwt/public', Bearer('not.a.token'));
  Assert.AreEqual(200, lRes.StatusCode, 'a bad token on a public action: anonymous, not refused');
  Assert.AreEqual('||False', WhoOf(lRes));
end;

procedure TAuthFiltersTests.JWTHandlerGetsTheControllerAndTheAction(const APort: Integer);
var
  lRes: IHTTPResponse;
begin
  lRes := Get(APort, '/jwt/admin', Bearer(LoginJWT(APort, 'admin')));
  Assert.AreEqual(200, lRes.StatusCode, lRes.ContentAsString);
  Assert.AreEqual('AuthFilterSupportU.TAFController|AdminInfo', lRes.HeaderValue['X-AF-OnRequest']);
  Assert.AreEqual('AuthFilterSupportU.TAFController|AdminInfo', lRes.HeaderValue['X-AF-OnAuthorization']);
end;

procedure TAuthFiltersTests.JWTBlackListedTokenIsRefused(const APort: Integer);
var
  lToken: string;
begin
  lToken := LoginJWT(APort, 'user');
  Assert.AreEqual(200, Get(APort, '/jwt/private', Bearer(lToken)).StatusCode);
  Assert.AreEqual(400, Send('POST', APort, '/jwt/logout', nil).StatusCode, 'no token to blacklist');
  Assert.AreEqual(204, Send('POST', APort, '/jwt/logout', Bearer(lToken)).StatusCode);
  Assert.AreEqual(403, Get(APort, '/jwt/private', Bearer(lToken)).StatusCode, 'blacklisted');
  Assert.AreEqual(200, Get(APort, '/jwt/private', Bearer(LoginJWT(APort, 'user'))).StatusCode,
    'another token is fine');
end;

procedure TAuthFiltersTests.JWTBlackListReadsTheTokenOfTheParameter(const APort: Integer);
var
  lToken: string;
begin
  lToken := LoginJWT(APort, 'user');
  Assert.AreEqual(200, Get(APort, '/jwtqs/private?access_token=' + lToken).StatusCode);
  // the logout takes the token of the parameter too
  Assert.AreEqual(204, Send('POST', APort, '/jwtqs/logout?access_token=' + lToken, nil).StatusCode);
  Assert.AreEqual(403, Get(APort, '/jwtqs/private?access_token=' + lToken).StatusCode, 'blacklisted, parameter');
  Assert.AreEqual(403, Get(APort, '/jwtqs/private', Bearer(lToken)).StatusCode, 'blacklisted, header');
end;

procedure TAuthFiltersTests.JWTTokensOfMiddlewareAndFilterAreInterchangeable;
var
  lRes: IHTTPResponse;
begin
  lRes := Get(AF_JWT_F, '/jwt/private', Bearer(LoginJWT(AF_JWT_MW, 'user')));
  Assert.AreEqual(200, lRes.StatusCode, 'middleware token, filter: ' + lRes.ContentAsString);
  lRes := Get(AF_JWT_MW, '/jwt/private', Bearer(LoginJWT(AF_JWT_F, 'user')));
  Assert.AreEqual(200, lRes.StatusCode, 'filter token, middleware: ' + lRes.ContentAsString);
end;

procedure TAuthFiltersTests.JWTFilterTakesTheTokenFromTheHeaderOnlyByDefault(const APort: Integer);
begin
  Assert.AreEqual(401, Get(APort, '/jwt/private?access_token=' + LoginJWT(APort, 'user')).StatusCode);
end;

procedure TAuthFiltersTests.JWTFilterTakesTheTokenFromTheParameterWhenAsked(const APort: Integer);
var
  lRes: IHTTPResponse;
begin
  lRes := Get(APort, '/minjwtqs/private?access_token=' + LoginJWT(APort, 'user'));
  Assert.AreEqual(200, lRes.StatusCode, lRes.ContentAsString);
  Assert.AreEqual('user|user|True', WhoOf(lRes));
end;

procedure TAuthFiltersTests.JWTFilterHonoursTheAlgorithm(const APort: Integer);
var
  lToken: string;
  lJWT: TJWT;
  lError: string;
begin
  lToken := LoginJWT(APort, 'user', '/login256');
  lJWT := TJWT.Create(AF_SECRET, 300, HMAC_HS256);
  try
    lJWT.RegClaimsToChecks := [TJWTCheckableClaim.ExpirationTime];
    Assert.IsTrue(lJWT.LoadToken(lToken, lError), lError);
  finally
    lJWT.Free;
  end;
  Assert.AreEqual(200, Get(APort, '/minjwt256/private', Bearer(lToken)).StatusCode, 'HS256 filter, HS256 token');
  Assert.AreEqual(401, Get(APort, '/jwt/private', Bearer(lToken)).StatusCode, 'HS512 filter, HS256 token');
  Assert.AreEqual(401, Get(APort, '/minjwt256/private', Bearer(LoginJWT(APort, 'user'))).StatusCode,
    'HS256 filter, HS512 token');
end;

procedure TAuthFiltersTests.JWTMinimalRouteAlwaysNeedsTheToken(const APort: Integer);
var
  lRes: IHTTPResponse;
begin
  // the handler of this group says "not required" to everything it is asked
  lRes := Get(APort, '/minjwt/private');
  Assert.AreEqual(401, lRes.StatusCode, lRes.ContentAsString);
  lRes := Get(APort, '/minjwt/private', Bearer(LoginJWT(APort, 'user')));
  Assert.AreEqual(200, lRes.StatusCode, lRes.ContentAsString);
  Assert.AreEqual('user|user|True', WhoOf(lRes));
end;

procedure TAuthFiltersTests.JWTMinimalRouteIsAuthorizedByOnAuthorization(const APort: Integer);
var
  lRes: IHTTPResponse;
begin
  lRes := Get(APort, '/minjwt/admin', Bearer(LoginJWT(APort, 'user')));
  Assert.AreEqual(403, lRes.StatusCode, lRes.ContentAsString);
  lRes := Get(APort, '/minjwt/admin', Bearer(LoginJWT(APort, 'admin')));
  Assert.AreEqual(200, lRes.StatusCode, lRes.ContentAsString);
  Assert.AreEqual('', lRes.HeaderValue['X-AF-OnRequest'], 'OnRequest is not asked for a route');
  Assert.AreEqual('|GET /minjwt/admin', lRes.HeaderValue['X-AF-OnAuthorization']);
end;

procedure TAuthFiltersTests.JWTBundlesRefuseTheShippedSecretAndAMissingHandler;
var
  lHandler: IMVCAuthenticationHandler;
begin
  // in a variable: passed inline to a bundle that raises before keeping it, the handler would never be released
  lHandler := TAFHandler.Create('/x');
  Assert.WillRaise(
    procedure
    var
      lBundle: IMVCFilterBundle;
    begin
      lBundle := JWTAuthenticationBundle(lHandler, ClaimsSetup, MVC_JWT_INSECURE_DEFAULT_SECRET);
    end, EMVCJWTException, 'JWT, the shipped secret');
  Assert.WillRaise(
    procedure
    var
      lBundle: IMVCFilterBundle;
    begin
      lBundle := JWTAuthenticationBundle(lHandler, nil, AF_SECRET);
    end, EMVCException, 'JWT, no claims setup');
  Assert.WillRaise(
    procedure
    var
      lBundle: IMVCFilterBundle;
    begin
      lBundle := JWTCookieAuthenticationBundle(lHandler, nil, AF_SECRET, CookieOptions);
    end, EMVCException, 'JWT cookie, no claims setup');
  Assert.WillRaise(
    procedure
    var
      lBundle: IMVCFilterBundle;
    begin
      lBundle := JWTCookieAuthenticationBundle(lHandler, ClaimsSetup, '', CookieOptions);
    end, EMVCJWTException, 'JWT cookie, empty secret');
  Assert.WillRaise(
    procedure
    var
      lBundle: IMVCFilterBundle;
    begin
      lBundle := JWTAuthenticationBundle(nil, ClaimsSetup, AF_SECRET);
    end, EMVCException, 'JWT without a handler: the login would give a token to anybody');
  Assert.WillRaise(
    procedure
    var
      lBundle: IMVCFilterBundle;
    begin
      lBundle := JWTCookieAuthenticationBundle(nil, ClaimsSetup, AF_SECRET, CookieOptions);
    end, EMVCException, 'JWT cookie without a handler');
  Assert.WillRaise(
    procedure
    var
      lOptions: TMVCJWTAuthenticationOptions;
      lBundle: IMVCFilterBundle;
    begin
      lOptions := TMVCJWTAuthenticationOptions.Default;
      lOptions.Handler := lHandler;
      lOptions.ClaimsSetup := ClaimsSetup;
      lOptions.Secret := AF_SECRET;
      lOptions.OnNewJWTToBlackList :=
        procedure(AContext: TWebContext; AJWTToken: string)
        begin
        end;
      lBundle := JWTAuthenticationBundle(lOptions);
    end, EMVCException, 'a blacklist without OnAcceptToken');
  Assert.WillRaise(
    procedure
    var
      lOptions: TMVCJWTAuthenticationOptions;
      lBundle: IMVCFilterBundle;
    begin
      lOptions := TMVCJWTAuthenticationOptions.Default;
      lOptions.Handler := lHandler;
      lOptions.ClaimsSetup := ClaimsSetup;
      lOptions.Secret := AF_SECRET;
      lOptions.Signer := THMACJWTSigner.Create(HMAC_HS512, AF_SECRET);
      lBundle := JWTAuthenticationBundle(lOptions);
    end, EMVCException, 'Secret and Signer together');
  Assert.WillRaise(
    procedure
    var
      lOptions: TMVCJWTAuthenticationOptions;
      lBundle: IMVCFilterBundle;
    begin
      lOptions := TMVCJWTAuthenticationOptions.Default;
      lOptions.Handler := lHandler;
      lOptions.ClaimsSetup := ClaimsSetup;
      lOptions.Signer := THMACJWTSigner.Create(HMAC_HS512, '');
      lBundle := JWTAuthenticationBundle(lOptions);
    end, EMVCJWTException, 'an HMAC signer with an empty key');
  Assert.WillRaise(
    procedure
    var
      lOptions: TMVCJWTAuthenticationOptions;
      lBundle: IMVCFilterBundle;
    begin
      lOptions := TMVCJWTAuthenticationOptions.Default;
      lOptions.Handler := lHandler;
      lOptions.ClaimsSetup := ClaimsSetup;
      lOptions.Secret := AF_SECRET;
      lOptions.RefreshTokenStore := TMVCInMemoryRefreshTokenStore.Create;
      lBundle := JWTAuthenticationBundle(lOptions);
    end, EMVCException, 'a claims setup the refresh token would ignore');
  Assert.WillRaise(
    procedure
    var
      lBundle: IMVCFilterBundle;
    begin
      lBundle := CustomAuthenticationBundle(nil);
    end, EMVCException, 'CustomAuth without a handler');
  Assert.WillRaise(
    procedure
    var
      lBundle: IMVCFilterBundle;
    begin
      lBundle := BasicAuthenticationBundle(nil);
    end, EMVCException, 'BasicAuth without a handler');
  Assert.WillRaise(
    procedure
    var
      lFilter: TMVCEndpointFilter;
    begin
      lFilter := BasicAuthEndpointFilter(IMVCAuthenticationHandler(nil));
    end, EMVCException, 'BasicAuthEndpointFilter without a handler');
  Assert.WillRaise(
    procedure
    var
      lOptions: TMVCOIDCOptions;
      lBundle: IMVCFilterBundle;
    begin
      lOptions := TMVCOIDCOptions.Default;
      lOptions.Issuer := 'http://localhost:1/idp';
      lOptions.ClientId := AF_CLIENT_ID;
      lOptions.JWTSecret := MVC_JWT_INSECURE_DEFAULT_SECRET;
      lBundle := OIDCAuthenticationBundle(lOptions);
    end, EMVCJWTException, 'OIDC with the shipped secret');
  Assert.WillNotRaiseAny(
    procedure
    var
      lBundle: IMVCFilterBundle;
    begin
      lBundle := JWTAuthenticationBundle(lHandler, ClaimsSetup, AF_SECRET);
    end, 'handler, claims setup and a secret of its own');
end;

procedure TAuthFiltersTests.JWTBundleOnAGroupPutsTheLoginOnTheEngine(const APort: Integer);
var
  lRes: IHTTPResponse;
begin
  lRes := Get(APort, '/minjwt/private', Bearer(LoginJWT(APort, 'user', '/minjwt/login')));
  Assert.AreEqual(200, lRes.StatusCode, lRes.ContentAsString);
  Assert.AreEqual('user|user|True', WhoOf(lRes));
  lRes := Get(APort, '/minjwtopen/private');
  Assert.AreEqual(200, lRes.StatusCode, 'a group without the bundle: ' + lRes.ContentAsString);
  Assert.AreEqual('||False', WhoOf(lRes));
end;

// a login or a refresh with the refresh token: {"access_token", "refresh_token", "token_type", "expires_in"}
procedure TokenPairOf(const AResponse: IHTTPResponse; out AAccess, ARefresh: string);
var
  lJSON: TJsonObject;
begin
  Assert.AreEqual(200, AResponse.StatusCode, AResponse.ContentAsString);
  lJSON := StrToJSONObject(AResponse.ContentAsString);
  try
    Assert.IsNotNull(lJSON, 'not a JSON object: ' + AResponse.ContentAsString);
    AAccess := lJSON.S['access_token'];
    ARefresh := lJSON.S['refresh_token'];
    Assert.AreEqual('bearer', lJSON.S['token_type'], AResponse.ContentAsString);
    Assert.AreEqual(AF_RT_TTL, lJSON.I['expires_in'], AResponse.ContentAsString);
  finally
    lJSON.Free;
  end;
  Assert.AreNotEqual('', AAccess, 'no access_token: ' + AResponse.ContentAsString);
  Assert.AreNotEqual('', ARefresh, 'no refresh_token: ' + AResponse.ContentAsString);
end;

function LoginRT(const APort: Integer; const AUser, APassword: string): IHTTPResponse;
begin
  Result := Send('POST', APort, '/jwtrt/login', [TNameValuePair.Create('jwtusername', AUser),
    TNameValuePair.Create('jwtpassword', APassword)]);
end;

function RefreshRT(const APort: Integer; const ARefreshToken: string): IHTTPResponse;
begin
  Result := Send('POST', APort, '/jwtrt/refresh', nil, '{"refresh_token":"' + ARefreshToken + '"}',
    'application/json');
end;

procedure TAuthFiltersTests.JWTRefreshLoginAnswersBothTokens(const APort: Integer);
var
  lAccess, lRefresh, lError: string;
  lJWT: TJWT;
  lRes: IHTTPResponse;
begin
  TokenPairOf(LoginRT(APort, 'admin', 'admin'), lAccess, lRefresh);
  lJWT := TJWT.Create(AF_SECRET);
  try
    lJWT.RegClaimsToChecks := [TJWTCheckableClaim.ExpirationTime, TJWTCheckableClaim.NotBefore,
      TJWTCheckableClaim.IssuedAt];
    Assert.IsTrue(lJWT.LoadToken(lAccess, lError), lError);
    Assert.AreEqual('admin', lJWT.CustomClaims['username']);
    Assert.AreEqual('user,admin', lJWT.CustomClaims['roles'], 'the roles of the handler');
    Assert.AreEqual('af-rt', lJWT.Claims.Issuer, 'the issuer of the refresh token');
  finally
    lJWT.Free;
  end;
  lRes := Get(APort, '/jwtrt/admin', Bearer(lAccess));
  Assert.AreEqual(200, lRes.StatusCode, lRes.ContentAsString);
  Assert.AreEqual('admin|user,admin|True', WhoOf(lRes));
  lRes := LoginRT(APort, 'user', 'wrong');
  Assert.AreEqual(401, lRes.StatusCode, 'wrong credentials: ' + lRes.ContentAsString);
  Assert.AreEqual(401, Get(APort, '/jwtrt/private').StatusCode, 'no token');
end;

procedure TAuthFiltersTests.JWTRefreshTokenIsRotated(const APort: Integer);
var
  lAccess, lRefresh, lAccess2, lRefresh2: string;
  lRes: IHTTPResponse;
begin
  TokenPairOf(LoginRT(APort, 'user', 'user'), lAccess, lRefresh);
  TokenPairOf(RefreshRT(APort, lRefresh), lAccess2, lRefresh2);
  Assert.AreNotEqual(lRefresh, lRefresh2, 'a new refresh token');
  lRes := Get(APort, '/jwtrt/private', Bearer(lAccess2));
  Assert.AreEqual(200, lRes.StatusCode, lRes.ContentAsString);
  Assert.AreEqual('user|user|True', WhoOf(lRes));
  lRes := RefreshRT(APort, lRefresh);
  Assert.AreEqual(401, lRes.StatusCode, 'the rotated one again is a reuse: ' + lRes.ContentAsString);
  Assert.AreEqual(401, RefreshRT(APort, lRefresh2).StatusCode, 'the reuse revoked the family');
end;

procedure TAuthFiltersTests.JWTLogoutRevokesTheRefreshTokenOfItsBody(const APort: Integer);
var
  lAccess, lRefresh, lAccess2, lRefresh2, lBody: string;
  lRes: IHTTPResponse;
begin
  TokenPairOf(LoginRT(APort, 'user', 'user'), lAccess, lRefresh);
  lRes := Send('POST', APort, '/jwtrt/logout', Bearer(lAccess), '{"refresh_token":"' + lRefresh + '"}',
    'application/json');
  Assert.AreEqual(204, lRes.StatusCode, lRes.ContentAsString);
  Assert.AreEqual(403, Get(APort, '/jwtrt/private', Bearer(lAccess)).StatusCode, 'the access token');
  lRes := RefreshRT(APort, lRefresh);
  Assert.AreEqual(401, lRes.StatusCode, 'the refresh token is revoked: ' + lRes.ContentAsString);
  // no refresh token in the body: the logout of the access token only
  TokenPairOf(LoginRT(APort, 'user', 'user'), lAccess2, lRefresh2);
  Assert.AreEqual(204, Send('POST', APort, '/jwtrt/logout', Bearer(lAccess2)).StatusCode, 'no body');
  Assert.AreEqual(204, Send('POST', APort, '/jwtrt/logout', Bearer(LoginJWT(APort, 'user')), 'not json',
    'application/json').StatusCode, 'a body that is not JSON');
  // a refresh_token that is not a string is not one: the logout of the access token, not a 500 after it
  for lBody in ['{"refresh_token":{}}', '{"refresh_token":[1]}', '{"refresh_token":null}', '{"refresh_token":7}'] do
  begin
    TokenPairOf(LoginRT(APort, 'user', 'user'), lAccess, lRefresh);
    lRes := Send('POST', APort, '/jwtrt/logout', Bearer(lAccess), lBody, 'application/json');
    Assert.AreEqual(204, lRes.StatusCode, lBody + ': ' + lRes.ContentAsString);
  end;
  TokenPairOf(RefreshRT(APort, lRefresh2), lAccess, lRefresh);
end;

procedure TAuthFiltersTests.JWTBundleWithoutALoginAcceptsTheTokensOfYourOwnLogin(const APort: Integer);
var
  lAccess, lRefresh, lAccess2, lRefresh2: string;
  lRes: IHTTPResponse;
begin
  lRes := Send('POST', APort, '/jwtown-login', [TNameValuePair.Create('jwtusername', 'user'),
    TNameValuePair.Create('jwtpassword', 'user')]);
  TokenPairOf(lRes, lAccess, lRefresh);
  lRes := Get(APort, '/jwtown/private', Bearer(lAccess));
  Assert.AreEqual(200, lRes.StatusCode, lRes.ContentAsString);
  Assert.AreEqual('user|user|True', WhoOf(lRes));
  Assert.AreEqual(401, Get(APort, '/jwtown/private').StatusCode, 'no token');
  // the refresh URL of the bundle rotates the pair of the login of the application
  lRes := Send('POST', APort, '/jwtown/refresh', nil, '{"refresh_token":"' + lRefresh + '"}', 'application/json');
  TokenPairOf(lRes, lAccess2, lRefresh2);
  Assert.AreNotEqual(lRefresh, lRefresh2, 'a new refresh token');
  Assert.AreEqual(200, Get(APort, '/jwtown/private', Bearer(lAccess2)).StatusCode, 'the refreshed access token');
  Assert.AreEqual(401, Send('POST', APort, '/jwtown-login', [TNameValuePair.Create('jwtusername', 'user'),
    TNameValuePair.Create('jwtpassword', 'wrong')]).StatusCode, 'wrong credentials');
end;

procedure TAuthFiltersTests.JWTBundleWithoutALoginHasNoLoginFilter;
var
  lHandler: IMVCAuthenticationHandler;
  lOptions: TMVCJWTAuthenticationOptions;
begin
  lHandler := TAFHandler.Create('/x');
  lOptions := TMVCJWTAuthenticationOptions.Default;
  lOptions.Handler := lHandler;
  lOptions.Secret := AF_SECRET;
  lOptions.LoginURL := '';
  // no claims setup: the bundle issues no token
  Assert.AreEqual(0, Integer(Length(JWTAuthenticationBundle(lOptions).HTTPFilters)), 'no login');
  Assert.AreEqual(1, Integer(Length(JWTAuthenticationBundle(lOptions).EndpointFilters)), 'the check');
  lOptions.RefreshTokenStore := TMVCInMemoryRefreshTokenStore.Create;
  Assert.AreEqual(1, Integer(Length(JWTAuthenticationBundle(lOptions).HTTPFilters)), 'the refresh URL, no login');
  // the options in the closure, not captured: ClaimsSetup assigned in this frame would make a cycle with it
  Assert.WillRaise(
    procedure
    var
      lWithClaims: TMVCJWTAuthenticationOptions;
      lBundle: IMVCFilterBundle;
    begin
      lWithClaims := TMVCJWTAuthenticationOptions.Default;
      lWithClaims.Handler := lHandler;
      lWithClaims.ClaimsSetup := ClaimsSetup;
      lWithClaims.Secret := AF_SECRET;
      lWithClaims.LoginURL := '';
      lBundle := JWTAuthenticationBundle(lWithClaims);
    end, EMVCException, 'a claims setup with no token to set up');
end;

procedure TAuthFiltersTests.JWTEngineWideBundleWithALoginActionOfYourOwn;
var
  lRes: IHTTPResponse;
  lToken: string;
begin
  lRes := Send('POST', AF_OWN_LOGIN, '/own/login', [TNameValuePair.Create('jwtusername', 'user'),
    TNameValuePair.Create('jwtpassword', 'user')]);
  Assert.AreEqual(200, lRes.StatusCode, 'the login, public by OnRequest: ' + lRes.ContentAsString);
  lToken := lRes.ContentAsString;
  lRes := Get(AF_OWN_LOGIN, '/own/private', Bearer(lToken));
  Assert.AreEqual(200, lRes.StatusCode, lRes.ContentAsString);
  Assert.AreEqual('user|user|True', WhoOf(lRes));
  Assert.AreEqual(401, Get(AF_OWN_LOGIN, '/own/private').StatusCode, 'no token');
  Assert.AreEqual(401, Send('POST', AF_OWN_LOGIN, '/own/login', [TNameValuePair.Create('jwtusername', 'user'),
    TNameValuePair.Create('jwtpassword', 'wrong')]).StatusCode, 'wrong credentials');
  // a Minimal API route is always checked: a login there could not be reached
  Assert.AreEqual(401, Send('POST', AF_OWN_LOGIN, '/own-route-login', nil).StatusCode, 'a route');
end;

procedure TAuthFiltersTests.RefreshTokenSignsWithTheSignerOfItsConfig;
var
  lConfig: TMVCRefreshTokenConfig;
  lCore: IMVCRefreshTokenCore;
  lJWT: TJWT;
  lError: string;
begin
  lConfig := TMVCRefreshTokenConfig.Default(''); // no secret: the signer signs
  lConfig.Signer := THMACJWTSigner.Create(HMAC_HS256, AF_SECRET);
  lCore := TMVCRefreshTokenCore.Create(lConfig, TMVCInMemoryRefreshTokenStore.Create);
  lJWT := TJWT.Create(AF_SECRET, 300, HMAC_HS256);
  try
    lJWT.RegClaimsToChecks := [TJWTCheckableClaim.ExpirationTime];
    Assert.IsTrue(lJWT.LoadToken(lCore.BuildAccessToken('user', 'user'), lError), lError);
    Assert.AreEqual('user', lJWT.CustomClaims['username']);
  finally
    lJWT.Free;
  end;
end;

function ExpiredToken: string;
var
  lJWT: TJWT;
begin
  lJWT := TJWT.Create(AF_SECRET);
  try
    lJWT.Claims.IssuedAt := Now - 1;
    lJWT.Claims.ExpirationTime := Now - OneHour;
    lJWT.CustomClaims['username'] := 'user';
    lJWT.CustomClaims['roles'] := 'user';
    Result := lJWT.GetToken;
  finally
    lJWT.Free;
  end;
end;

procedure TAuthFiltersTests.JWTExpiredTokenIsRefused(const APort: Integer);
begin
  Assert.AreEqual(401, Get(APort, '/jwt/private', Bearer(ExpiredToken)).StatusCode, 'exp checked');
  if APort <> AF_JWT_MW then
    Assert.AreEqual(401, Get(APort, '/minjwt/private', Bearer(ExpiredToken)).StatusCode,
      'JWTAuthenticationBundle(handler, claims setup, secret): exp checked by default');
end;

// expires in 30 seconds, with a live validity window of 60: the check renews it (the login itself
// moves the expiration past the window, so a fresh login has nothing to renew)
function TokenAboutToExpire: string;
var
  lJWT: TJWT;
begin
  lJWT := TJWT.Create(AF_SECRET);
  try
    lJWT.Claims.IssuedAt := Now;
    lJWT.Claims.ExpirationTime := Now + 30 * OneSecond;
    lJWT.LiveValidityWindowInSeconds := 60;
    lJWT.CustomClaims['username'] := 'user';
    lJWT.CustomClaims['roles'] := 'user';
    Result := lJWT.GetToken;
  finally
    lJWT.Free;
  end;
end;

procedure TAuthFiltersTests.JWTTokenAboutToExpireIsRenewed(const APort: Integer);
var
  lToken: string;
  lRes: IHTTPResponse;
begin
  lToken := TokenAboutToExpire;
  lRes := Get(APort, '/jwt/private', Bearer(lToken));
  Assert.AreEqual(200, lRes.StatusCode, lRes.ContentAsString);
  Assert.StartsWith('Bearer ', lRes.HeaderValue['Authorization'], 'a renewed token');
  Assert.AreNotEqual('Bearer ' + lToken, lRes.HeaderValue['Authorization']);
  Assert.AreEqual(200, Get(APort, '/jwt/private', Header('Authorization', lRes.HeaderValue['Authorization'])).StatusCode,
    'the renewed token is accepted');
end;

procedure TAuthFiltersTests.JWTPublicActionReadsTheTokenOfTheParameter(const APort: Integer);
var
  lPath: string;
  lRes: IHTTPResponse;
begin
  if APort = AF_JWT_MW then
    lPath := '/jwt'
  else
    lPath := '/jwtqs';
  lRes := Get(APort, lPath + '/public?access_token=' + LoginJWT(APort, 'user'));
  Assert.AreEqual(200, lRes.StatusCode, lRes.ContentAsString);
  Assert.AreEqual('user|user|True', WhoOf(lRes));
  lRes := Get(APort, lPath + '/private?access_token=' + LoginJWT(APort, 'user'));
  Assert.AreEqual(200, lRes.StatusCode, 'private: ' + lRes.ContentAsString);
end;

procedure TAuthFiltersTests.ARefusedLoginStillEndsTheRequest(const APort: Integer);
var
  lBefore, lAfter: Integer;
begin
  // each request to /afcount ends too: +1 for it, +1 for the refused login
  lBefore := StrToInt(WhoOf(Get(APort, '/afcount')));
  Assert.AreEqual(401, Send('POST', APort, '/login', [TNameValuePair.Create('jwtusername', 'user'),
    TNameValuePair.Create('jwtpassword', 'wrong')]).StatusCode);
  lAfter := StrToInt(WhoOf(Get(APort, '/afcount')));
  Assert.AreEqual(lBefore + 2, lAfter, 'OnAfterRouting ran for the refused login');
end;

type
  TAFTrackedData = class(TMVCCustomData)
  public
    destructor Destroy; override;
  end;

var
  GTrackedDataFreed: Boolean = False;

destructor TAFTrackedData.Destroy;
begin
  GTrackedDataFreed := True;
  inherited;
end;

procedure TAuthFiltersTests.LoggedUserFreesTheCustomDataItReplaces;
var
  lUser: TMVCUser;
  lData: TMVCCustomData;
begin
  lUser := TMVCUser.Create;
  try
    GTrackedDataFreed := False;
    lUser.CustomData := TAFTrackedData.Create;
    lData := lUser.CustomData;
    lUser.CustomData := lData;
    Assert.IsFalse(GTrackedDataFreed, 'the same object is kept');
    lUser.CustomData := TMVCCustomData.Create;
    Assert.IsTrue(GTrackedDataFreed, 'the one it replaces is freed');
  finally
    lUser.Free;
  end;
end;

// --- JWT in a cookie

procedure TAuthFiltersTests.CookieLoginSetsTheCookieAndThePrivateActionReadsIt(const APort: Integer);
var
  lRes: IHTTPResponse;
  lCookie: string;
  lSetCookie: TCookie;
begin
  Assert.AreEqual(401, Get(APort, '/cookie/private').StatusCode, 'no cookie');
  lRes := Send('POST', APort, '/cookie/login', [TNameValuePair.Create('jwtusername', 'user'),
    TNameValuePair.Create('jwtpassword', 'user')]);
  Assert.AreEqual(200, lRes.StatusCode, lRes.ContentAsString);
  Assert.IsTrue(FindCookie(lRes, AF_COOKIE, lSetCookie), 'the login sets the cookie: ' + CookiesOf(lRes));
  lCookie := lSetCookie.Value;
  Assert.AreNotEqual('', lCookie);
  Assert.IsTrue(lSetCookie.HttpOnly, 'HttpOnly');
  Assert.IsFalse(lSetCookie.Secure, 'Secure as configured');
  Assert.Contains(lRes.ContentAsString, '"token"', 'and answers the token');
  lRes := Get(APort, '/cookie/private', Header('Cookie', AF_COOKIE + '=' + lCookie));
  Assert.AreEqual(200, lRes.StatusCode, lRes.ContentAsString);
  Assert.AreEqual('user|user|True', WhoOf(lRes));
  Assert.AreEqual(401, Get(APort, '/cookie/private', Bearer(lCookie)).StatusCode, 'the header is not read');
  Assert.AreEqual(403, Get(APort, '/cookie/admin', Header('Cookie', AF_COOKIE + '=' + lCookie)).StatusCode);
end;

procedure TAuthFiltersTests.CookieLogoutExpiresTheCookie(const APort: Integer);
var
  lRes: IHTTPResponse;
  lExpired: Boolean;
begin
  lRes := Get(APort, '/cookie/logout');
  Assert.AreEqual(200, lRes.StatusCode, lRes.ContentAsString);
  Assert.AreEqual('', SetCookieValue(lRes, AF_COOKIE, lExpired));
  Assert.IsTrue(lExpired, 'the cookie is expired: ' + CookiesOf(lRes));
end;

procedure TAuthFiltersTests.CookieOptionsAreChecked;
var
  lCookie: TMVCAuthCookieOptions;
  lHandler: IMVCAuthenticationHandler;
begin
  lHandler := TAFHandler.Create('/x');
  lCookie := TMVCAuthCookieOptions.Default;
  Assert.AreEqual('jwt_token', lCookie.Name);
  Assert.IsTrue(lCookie.Secure);
  Assert.IsTrue(lCookie.SameSite = ssStrict);
  lCookie.SameSite := ssNone;
  lCookie.Secure := False;
  Assert.WillRaise(
    procedure
    var
      lBundle: IMVCFilterBundle;
    begin
      lBundle := JWTCookieAuthenticationBundle(lHandler, ClaimsSetup, AF_SECRET, lCookie);
    end, EMVCException, 'SameSite=None without Secure');
  // the origin check on '' would cover every path
  Assert.WillRaise(
    procedure
    var
      lBundle: IMVCFilterBundle;
    begin
      lBundle := JWTCookieAuthenticationBundle(lHandler, ClaimsSetup, AF_SECRET, CookieOptions, '');
    end, EMVCException, 'no login URL');
end;

procedure TAuthFiltersTests.CookieLoginRefusesAnOriginNotAllowed(const APort: Integer);
var
  lCredentials: TNetHeaders;
  lRes: IHTTPResponse;
begin
  lCredentials := [TNameValuePair.Create('jwtusername', 'user'), TNameValuePair.Create('jwtpassword', 'user')];
  lRes := Send('POST', APort, '/cookie/login', lCredentials + Header('Origin', 'http://evil.example'));
  Assert.AreEqual(403, lRes.StatusCode, 'another Origin: ' + lRes.ContentAsString);
  lRes := Send('POST', APort, '/cookie/login', lCredentials + Header('Origin', AF_ALLOWED_ORIGIN));
  Assert.AreEqual(200, lRes.StatusCode, 'the allowed Origin: ' + lRes.ContentAsString);
  lRes := Send('POST', APort, '/cookie/login', lCredentials);
  Assert.AreEqual(200, lRes.StatusCode, 'no Origin, not a cross-origin request: ' + lRes.ContentAsString);
end;

// --- custom authentication

function CustomLogin(const APort: Integer; const AUser: string; const AURL: string = '/system/users/logged'): string;
var
  lRes: IHTTPResponse;
begin
  lRes := Send('POST', APort, AURL, nil, '{"username":"' + AUser + '","password":"' + AUser + '"}',
    'application/json');
  Assert.AreEqual(200, lRes.StatusCode, 'login: ' + lRes.ContentAsString);
  Result := SetCookieValueOf(lRes, TMVCConstants.SESSION_TOKEN_NAME);
  Assert.AreNotEqual('', Result, 'the login opens a session: ' + CookiesOf(lRes));
end;

function SessionCookie(const ASessionId: string): TNetHeaders;
begin
  Result := Header('Cookie', TMVCConstants.SESSION_TOKEN_NAME + '=' + ASessionId);
end;

procedure TAuthFiltersTests.CustomLoginPrivateLogout(const APort: Integer);
var
  lSession: string;
  lRes: IHTTPResponse;
begin
  lSession := CustomLogin(APort, 'user');
  lRes := Get(APort, '/custom/private', SessionCookie(lSession));
  Assert.AreEqual(200, lRes.StatusCode, lRes.ContentAsString);
  Assert.AreEqual('user|user|True', WhoOf(lRes));
  lRes := Send('DELETE', APort, '/system/users/logged', SessionCookie(lSession));
  Assert.AreEqual(200, lRes.StatusCode, 'logout: ' + lRes.ContentAsString);
  Assert.AreEqual(401, Get(APort, '/custom/private', SessionCookie(lSession)).StatusCode, 'after the logout');
end;

procedure TAuthFiltersTests.CustomWithoutLoginIs401AsTheMiddlewareAnswers(const APort: Integer);
var
  lRes: IHTTPResponse;
begin
  lRes := Get(APort, '/custom/private', Header('Accept', 'application/json'));
  Assert.AreEqual(401, lRes.StatusCode, lRes.ContentAsString);
  Assert.StartsWith('application/json', lRes.MimeType, 'Content-Type: ' + lRes.MimeType);
  Assert.AreEqual('{"status":"KO", "message":"401"}', lRes.ContentAsString);
  Assert.AreEqual('/system/users/logged', lRes.HeaderValue['X-LOGIN-URL']);
  lRes := Get(APort, '/custom/public');
  Assert.AreEqual(200, lRes.StatusCode, 'public: ' + lRes.ContentAsString);
end;

procedure TAuthFiltersTests.CustomAdminIsAuthorizedByAction(const APort: Integer);
var
  lRes: IHTTPResponse;
begin
  lRes := Get(APort, '/custom/admin', SessionCookie(CustomLogin(APort, 'user')));
  Assert.AreEqual(403, lRes.StatusCode, lRes.ContentAsString);
  lRes := Get(APort, '/custom/admin', SessionCookie(CustomLogin(APort, 'admin')));
  Assert.AreEqual(200, lRes.StatusCode, lRes.ContentAsString);
  Assert.AreEqual('AuthFilterSupportU.TAFController|AdminInfo', lRes.HeaderValue['X-AF-OnAuthorization']);
end;

procedure TAuthFiltersTests.RoleBasedHandlerReadsTheAttributes(const APort: Integer);
var
  lLoginURL: string;
  lRes: IHTTPResponse;
begin
  // the classic role based middleware has a login URL of its own on that engine
  if APort = AF_CUSTOM_MW then
    lLoginURL := '/system/roles/logged'
  else
    lLoginURL := '/system/users/logged';
  Assert.AreEqual(200, Get(APort, '/role/open').StatusCode, 'no attribute: public');
  Assert.AreEqual(401, Get(APort, '/role/member').StatusCode, 'MVCRequiresAuthentication');
  lRes := Get(APort, '/role/member', SessionCookie(CustomLogin(APort, 'user', lLoginURL)));
  Assert.AreEqual(200, lRes.StatusCode, lRes.ContentAsString);
  lRes := Get(APort, '/role/admin', SessionCookie(CustomLogin(APort, 'user', lLoginURL)));
  Assert.AreEqual(403, lRes.StatusCode, 'MVCRequiresRole(admin), user: ' + lRes.ContentAsString);
  lRes := Get(APort, '/role/admin', SessionCookie(CustomLogin(APort, 'admin', lLoginURL)));
  Assert.AreEqual(200, lRes.StatusCode, lRes.ContentAsString);
end;

procedure TAuthFiltersTests.RoleBasedHandlerRefusesAControllerItCannotFind(const APort: Integer);
var
  lLoginURL: string;
  lRes: IHTTPResponse;
begin
  if APort = AF_CUSTOM_MW then
    lLoginURL := '/system/roles/logged'
  else
    lLoginURL := '/system/users/logged';
  Assert.AreEqual(401, Get(APort, '/hiddenrole/get').StatusCode, 'authentication required');
  lRes := Get(APort, '/hiddenrole/get', SessionCookie(CustomLogin(APort, 'admin', lLoginURL)));
  Assert.AreEqual(403, lRes.StatusCode, 'not even the admin: the roles cannot be read; ' + lRes.ContentAsString);
end;

procedure TAuthFiltersTests.RoleBasedHandlerOnAMinimalRoute(const APort: Integer);
var
  lRes: IHTTPResponse;
begin
  lRes := Get(APort, '/minrole/member');
  Assert.AreEqual(401, lRes.StatusCode, lRes.ContentAsString);
  lRes := Get(APort, '/minrole/member', SessionCookie(CustomLogin(APort, 'user')));
  Assert.AreEqual(200, lRes.StatusCode, lRes.ContentAsString);
  Assert.AreEqual('user|user|True', WhoOf(lRes));
end;

// --- OIDC

function QueryParam(const AURL, AName: string): string;
var
  lPair: string;
begin
  Result := '';
  for lPair in AURL.Substring(AURL.IndexOf('?') + 1).Split(['&']) do
    if lPair.StartsWith(AName + '=') then
      Exit(TNetEncoding.URL.Decode(lPair.Substring(Length(AName) + 1)));
end;

// login at the application, then back from the provider: the session cookie
function OIDCLogin(const APort: Integer; const ATamperState: Boolean; out ACallback: IHTTPResponse): string;
var
  lRes: IHTTPResponse;
  lLocation, lState, lNonce, lStateCookie: string;
begin
  lRes := Get(APort, '/auth/login');
  Assert.AreEqual(302, lRes.StatusCode, lRes.ContentAsString);
  lLocation := lRes.HeaderValue['Location'];
  Assert.StartsWith('http://localhost:' + APort.ToString + '/idp/authorize?', lLocation);
  lState := QueryParam(lLocation, 'state');
  lNonce := QueryParam(lLocation, 'nonce');
  Assert.AreNotEqual('', QueryParam(lLocation, 'code_challenge'), 'PKCE');
  lStateCookie := SetCookieValueOf(lRes, 'dmvc_oidc_session_state');
  Assert.AreNotEqual('', lStateCookie, 'the state cookie: ' + CookiesOf(lRes));
  if ATamperState then
    lState := lState + 'x';
  // the fake provider gives back the nonce as the code
  ACallback := Get(APort, '/auth/callback?code=' + TNetEncoding.URL.Encode(lNonce) + '&state=' +
    TNetEncoding.URL.Encode(lState), Header('Cookie', 'dmvc_oidc_session_state=' + lStateCookie));
  Assert.AreEqual(302, ACallback.StatusCode, ACallback.ContentAsString);
  Result := SetCookieValueOf(ACallback, 'dmvc_oidc_session');
end;

procedure TAuthFiltersTests.OIDCPrivateWithoutSessionRedirectsToTheLogin(const APort: Integer);
var
  lRes: IHTTPResponse;
begin
  lRes := Get(APort, '/oidc/private');
  Assert.AreEqual(302, lRes.StatusCode, lRes.ContentAsString);
  Assert.AreEqual('/auth/login', lRes.HeaderValue['Location']);
  lRes := Get(APort, '/oidc/public');
  Assert.AreEqual(200, lRes.StatusCode, lRes.ContentAsString);
  Assert.AreEqual('||False', WhoOf(lRes));
end;

procedure TAuthFiltersTests.OIDCLoginCallbackAndPrivate(const APort: Integer);
var
  lCallback, lRes: IHTTPResponse;
  lSession: string;
begin
  lSession := OIDCLogin(APort, False, lCallback);
  Assert.AreEqual('/', lCallback.HeaderValue['Location'], 'to the page after the login');
  Assert.AreNotEqual('', lSession, 'the session cookie');
  lRes := Get(APort, '/oidc/private', Header('Cookie', 'dmvc_oidc_session=' + lSession));
  Assert.AreEqual(200, lRes.StatusCode, lRes.ContentAsString);
  Assert.AreEqual('alice|user|True', WhoOf(lRes));
  lRes := Get(APort, '/oidc/public', Header('Cookie', 'dmvc_oidc_session=' + lSession));
  Assert.AreEqual('alice|user|True', WhoOf(lRes), 'a public action knows the user');
end;

procedure TAuthFiltersTests.OIDCCallbackWithAnotherStateIsRefused(const APort: Integer);
var
  lCallback: IHTTPResponse;
begin
  Assert.AreEqual('', OIDCLogin(APort, True, lCallback), 'no session');
  Assert.AreEqual('/auth/login', lCallback.HeaderValue['Location']);
end;

procedure TAuthFiltersTests.OIDCLogoutClearsTheSession(const APort: Integer);
var
  lRes: IHTTPResponse;
  lExpired: Boolean;
begin
  lRes := Get(APort, '/auth/logout');
  Assert.AreEqual(302, lRes.StatusCode, lRes.ContentAsString);
  Assert.AreEqual('/auth/login', lRes.HeaderValue['Location']);
  SetCookieValue(lRes, 'dmvc_oidc_session', lExpired);
  Assert.IsTrue(lExpired, 'the session cookie is expired: ' + CookiesOf(lRes));
end;

procedure TAuthFiltersTests.OIDCMinimalRouteNeedsTheSession(const APort: Integer);
var
  lCallback, lRes: IHTTPResponse;
  lSession: string;
begin
  lRes := Get(APort, '/minoidc/private');
  Assert.AreEqual(302, lRes.StatusCode, lRes.ContentAsString);
  Assert.AreEqual('/auth/login', lRes.HeaderValue['Location']);
  lSession := OIDCLogin(APort, False, lCallback);
  lRes := Get(APort, '/minoidc/private', Header('Cookie', 'dmvc_oidc_session=' + lSession));
  Assert.AreEqual(200, lRes.StatusCode, lRes.ContentAsString);
  Assert.AreEqual('alice|user|True', WhoOf(lRes));
end;

initialization

TDUnitX.RegisterTestFixture(TAuthFiltersTests);

end.
