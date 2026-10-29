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

unit AuthFilterSupportU;

// The server side of the authentication filter tests (AuthFiltersTestsU in TestClient): controllers,
// handlers and the engines with the classic middleware or with the filter bundles that replace them.
// Shared by the in-process servers of the tests and by the TestServer, which every host runs:
// ConfigureAuthFilterSupport puts the JWT (with blacklist, with refresh token), JWT cookie and OIDC bundles
// on it. Every bundle acts only on its own paths (/login, /login256, /jwt*, /cookie, /minjwt*, /auth, /oidc,
// /minoidc, /idp), the other requests of the suite go through untouched. The custom authentication needs a session filter, and
// the TestServer has the classic session middleware: those tests run in-process only.

interface

{$WARN SYMBOL_DEPRECATED OFF} // the middleware are tested on purpose

uses
  System.Generics.Collections,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.JWT,
  MVCFramework.Filters,
  MVCFramework.Middleware.Authentication.RoleBasedAuthHandler; // MVCRequiresRole

const
  AF_SECRET = 'auth_filters_tests_secret_6c1e9d0a';
  AF_OIDC_SECRET = 'auth_filters_tests_oidc_secret_1b7f';
  AF_CLIENT_ID = 'af-client';
  AF_COOKIE = 'af_jwt';
  AF_ALLOWED_ORIGIN = 'http://af.example';  // the only Origin the cookie login accepts
  AF_RT_TTL = 120;                          // lifetime of the access tokens of the refresh token

type
  // Every action answers "username|roles|IsValid" of Context.LoggedUser. Mounted under several prefixes, each
  // protected by a different mechanism.
  TAFController = class(TMVCController)
  public
    [MVCPath('/public')]
    [MVCHTTPMethod([httpGET])]
    function PublicInfo: string;
    [MVCPath('/private')]
    [MVCHTTPMethod([httpGET])]
    function PrivateInfo: string;
    [MVCPath('/admin')]
    [MVCHTTPMethod([httpGET])]
    function AdminInfo: string;
  end;

  [MVCPath('/role')]
  TAFRoleController = class(TMVCController)
  public
    [MVCPath('/open')]
    [MVCHTTPMethod([httpGET])]
    function Open: string;
    [MVCPath('/member')]
    [MVCHTTPMethod([httpGET])]
    [MVCRequiresAuthentication]
    function Member: string;
    [MVCPath('/admin')]
    [MVCHTTPMethod([httpGET])]
    [MVCRequiresRole('admin')]
    function Admin: string;
  end;

  // Protects the requests under fPrefix (everything else is public for it): authentication for every action
  // but Public*, the role admin for Admin* (and for a Minimal API route with /admin in its pattern). What
  // OnRequest and OnAuthorization were given goes into X-AF-OnRequest / X-AF-OnAuthorization.
  TAFHandler = class(TInterfacedObject, IMVCAuthenticationHandler)
  private
    fPrefix: string;
  public
    constructor Create(const APrefix: string);
    procedure OnRequest(const AContext: TWebContext; const AControllerQualifiedClassName,
      AActionName: string; var AAuthenticationRequired: Boolean);
    procedure OnAuthentication(const AContext: TWebContext; const AUserName, APassword: string;
      AUserRoles: TList<string>; var AIsValid: Boolean;
      const ASessionData: TDictionary<string, string>);
    procedure OnAuthorization(const AContext: TWebContext; AUserRoles: TList<string>;
      const AControllerQualifiedClassName: string; const AActionName: string;
      var AIsAuthorized: Boolean);
  end;

  // A login of the application (D1): an action the handler of /own makes public (Public*), the token signed
  // with AF_SECRET. The check is an engine-wide JWT bundle without a login (AddOwnLoginAuthentication).
  TAFOwnLoginController = class(TMVCController)
  public
    [MVCPath('/login')]
    [MVCHTTPMethod([httpPOST])]
    function PublicLogin: string;
  end;

procedure ClaimsSetup(const AJWT: TJWT);
function CookieOptions: TMVCAuthCookieOptions;

// JWT header + blacklist + JWT cookie: the middleware, or the bundles (with Minimal API routes and the refresh token too)
procedure AddJWTAuthentication(const AEngine: TMVCEngine; const AWithFilters: Boolean);
// custom authentication + role based handler; needs a session filter, which it adds
procedure AddCustomAuthentication(const AEngine: TMVCEngine; const AWithFilters: Boolean);
// an engine-wide JWT bundle without a login (no OnlyUnder) and the login of TAFOwnLoginController, under /own;
// AWithFilters is not read (the signature of the configure procedures of the tests)
procedure AddOwnLoginAuthentication(const AEngine: TMVCEngine; const AWithFilters: Boolean);
// OIDC with a fake provider on the same engine; ABaseURL is the URL of the engine, e.g. http://localhost:8957
procedure AddOIDCAuthentication(const AEngine: TMVCEngine; const AWithFilters: Boolean; const ABaseURL: string);

// the filters of AddJWTAuthentication and AddOIDCAuthentication on the TestServer
procedure ConfigureAuthFilterSupport(const AEngine: TMVCEngine; const ABaseURL: string);

implementation

uses
  System.SysUtils,
  System.SyncObjs,
  System.NetEncoding,
  System.DateUtils,
  JsonDataObjects,
  MVCFramework.HMAC,
  MVCFramework.MinimalAPI,
  MVCFramework.Filters.Authentication,
  MVCFramework.Filters.OIDC,
  MVCFramework.JWT.RefreshToken,
  MVCFramework.Middleware.JWT,
  MVCFramework.Middleware.Authentication,
  MVCFramework.Middleware.OIDC;

var
  GBlackList: TList<string>;
  GBlackListLock: TObject;
  // process-wide, as the blacklist: the WebBroker TestServer makes one engine per web module, and the next
  // request of a test can reach a new module when it arrives before the previous module is back in the pool
  GRefreshTokenStore: IMVCRefreshTokenStore;
  GAfterRoutingCount: Integer;

function WhoAmI(const AContext: TWebContext): string;
begin
  Result := AContext.LoggedUser.UserName + '|' + string.Join(',', AContext.LoggedUser.Roles.ToArray) + '|' +
    BoolToStr(AContext.LoggedUser.IsValid, True);
end;

{ TAFController }

function TAFController.PublicInfo: string;
begin
  Result := WhoAmI(Context);
end;

function TAFController.PrivateInfo: string;
begin
  Result := WhoAmI(Context);
end;

function TAFController.AdminInfo: string;
begin
  Result := WhoAmI(Context);
end;

{ TAFRoleController }

function TAFRoleController.Open: string;
begin
  Result := 'open';
end;

function TAFRoleController.Member: string;
begin
  Result := WhoAmI(Context);
end;

function TAFRoleController.Admin: string;
begin
  Result := WhoAmI(Context);
end;

type
  // Declared here, not in the interface: RTTI cannot find it by name, so the role based handler cannot
  // read its attributes and has to refuse it
  [MVCPath('/hiddenrole')]
  [MVCRequiresRole('admin')]
  TAFHiddenRoleController = class(TMVCController)
  public
    [MVCPath('/get')]
    [MVCHTTPMethod([httpGET])]
    function GetIt: string;
  end;

  // counts the requests that reach the end (OnAfterRouting), also those answered by an HTTP filter
  TAFAfterRoutingCounter = class(TInterfacedObject, IMVCMiddleware)
  public
    function Count: Integer;
    procedure OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
    procedure OnBeforeControllerAction(AContext: TWebContext; const AControllerQualifiedClassName: string;
      const AActionName: string; var AHandled: Boolean);
    procedure OnAfterControllerAction(AContext: TWebContext; const AControllerQualifiedClassName: string;
      const AActionName: string; const AHandled: Boolean);
    procedure OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
  end;

function TAFHiddenRoleController.GetIt: string;
begin
  Result := 'hidden';
end;

function TAFAfterRoutingCounter.Count: Integer;
begin
  Result := TInterlocked.CompareExchange(GAfterRoutingCount, 0, 0);
end;

procedure TAFAfterRoutingCounter.OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
begin
end;

procedure TAFAfterRoutingCounter.OnBeforeControllerAction(AContext: TWebContext;
  const AControllerQualifiedClassName, AActionName: string; var AHandled: Boolean);
begin
end;

procedure TAFAfterRoutingCounter.OnAfterControllerAction(AContext: TWebContext;
  const AControllerQualifiedClassName, AActionName: string; const AHandled: Boolean);
begin
end;

procedure TAFAfterRoutingCounter.OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
begin
  TInterlocked.Increment(GAfterRoutingCount);
end;

type
  TAFRoleHandler = class(TRoleBasedAuthHandler)
  protected
    procedure OnAuthentication(const AContext: TWebContext; const UserName: string; const Password: string;
      UserRoles: TList<string>; var IsValid: Boolean; const SessionData: TDictionary<string, string>); override;
  end;

procedure CheckUser(const AUserName, APassword: string; const AUserRoles: TList<string>; var AIsValid: Boolean);
begin
  // user/user: roles user; admin/admin: roles user, admin
  if AUserName = 'lazy' then
    Exit; // a handler that sets AIsValid only to accept
  AIsValid := ((AUserName = 'user') or (AUserName = 'admin')) and (APassword = AUserName);
  if AIsValid then
  begin
    AUserRoles.Add('user');
    if AUserName = 'admin' then
      AUserRoles.Add('admin');
  end;
end;

{ TAFOwnLoginController }

function TAFOwnLoginController.PublicLogin: string;
var
  lRoles: TList<string>;
  lValid: Boolean;
  lJWT: TJWT;
begin
  lRoles := TList<string>.Create;
  try
    lValid := False;
    CheckUser(Context.Request.Headers['jwtusername'], Context.Request.Headers['jwtpassword'], lRoles, lValid);
    if not lValid then
      raise EMVCException.Create(HTTP_STATUS.Unauthorized, 'wrong credentials');
    lJWT := TJWT.Create(AF_SECRET);
    try
      ClaimsSetup(lJWT);
      lJWT.CustomClaims['username'] := Context.Request.Headers['jwtusername'];
      lJWT.CustomClaims['roles'] := string.Join(',', lRoles.ToArray);
      Result := lJWT.GetToken;
    finally
      lJWT.Free;
    end;
  finally
    lRoles.Free;
  end;
end;

{ TAFHandler }

constructor TAFHandler.Create(const APrefix: string);
begin
  inherited Create;
  fPrefix := APrefix;
end;

procedure TAFHandler.OnRequest(const AContext: TWebContext; const AControllerQualifiedClassName,
  AActionName: string; var AAuthenticationRequired: Boolean);
begin
  if not AContext.Request.PathInfo.StartsWith(fPrefix + '/') then
  begin
    AAuthenticationRequired := False;
    Exit;
  end;
  AContext.Response.SetCustomHeader('X-AF-OnRequest', AControllerQualifiedClassName + '|' + AActionName);
  AAuthenticationRequired := not AActionName.StartsWith('Public');
end;

procedure TAFHandler.OnAuthentication(const AContext: TWebContext; const AUserName, APassword: string;
  AUserRoles: TList<string>; var AIsValid: Boolean; const ASessionData: TDictionary<string, string>);
begin
  CheckUser(AUserName, APassword, AUserRoles, AIsValid);
end;

procedure TAFHandler.OnAuthorization(const AContext: TWebContext; AUserRoles: TList<string>;
  const AControllerQualifiedClassName, AActionName: string; var AIsAuthorized: Boolean);
begin
  if AContext.Request.PathInfo.StartsWith(fPrefix + '/') then
    AContext.Response.SetCustomHeader('X-AF-OnAuthorization', AControllerQualifiedClassName + '|' + AActionName);
  if AActionName.StartsWith('Admin') or AActionName.Contains('/admin') then
    AIsAuthorized := AUserRoles.Contains('admin')
  else
    AIsAuthorized := True;
end;

{ TAFRoleHandler }

procedure TAFRoleHandler.OnAuthentication(const AContext: TWebContext; const UserName, Password: string;
  UserRoles: TList<string>; var IsValid: Boolean; const SessionData: TDictionary<string, string>);
begin
  CheckUser(UserName, Password, UserRoles, IsValid);
end;

// AFilter only for the requests whose path starts with one of APrefixes + '/'
function GuardUnder(const APrefixes: TArray<string>; const AFilter: TMVCEndpointFilter): TMVCEndpointFilter;
begin
  Result :=
    function(const AContext: TWebContext; const ANext: TMVCEndpointFilterNext): IMVCResponse
    var
      lPrefix: string;
    begin
      for lPrefix in APrefixes do
        if AContext.Request.PathInfo.StartsWith(lPrefix + '/') then
          Exit(AFilter(AContext, ANext));
      Result := ANext();
    end;
end;

// ABundle with its endpoint part only for the requests under APrefixes: on the TestServer an engine-wide
// check would also cover the Minimal API routes of the other tests. The HTTP part (logins) as it is.
function OnlyUnder(const APrefixes: TArray<string>; const ABundle: IMVCFilterBundle): IMVCFilterBundle;
var
  lFilters: TArray<TMVCEndpointFilter>;
  I: Integer;
begin
  lFilters := Copy(ABundle.EndpointFilters);
  for I := 0 to High(lFilters) do
    lFilters[I] := GuardUnder(APrefixes, lFilters[I]);
  Result := TMVCFilterBundle.Create(ABundle.HTTPFilters, lFilters);
end;

procedure ClaimsSetup(const AJWT: TJWT);
begin
  // two logins in the same second would give the same token: the blacklist of one would refuse the other
  AJWT.Claims.JWT_ID := TGUID.NewGuid.ToString;
  AJWT.Claims.Issuer := 'af';
  AJWT.Claims.ExpirationTime := Now + OneHour;
  AJWT.Claims.IssuedAt := Now;
end;

function AcceptToken: TMVCOnAcceptTokenProc;
begin
  Result :=
    procedure(AContext: TWebContext; AJWTToken: string; var AAccepted: Boolean)
    begin
      TMonitor.Enter(GBlackListLock);
      try
        AAccepted := not GBlackList.Contains(AJWTToken);
      finally
        TMonitor.Exit(GBlackListLock);
      end;
    end;
end;

function BlackListToken: TMVCOnNewJWTToBlackList;
begin
  Result :=
    procedure(AContext: TWebContext; AJWTToken: string)
    begin
      TMonitor.Enter(GBlackListLock);
      try
        GBlackList.Add(AJWTToken);
      finally
        TMonitor.Exit(GBlackListLock);
      end;
    end;
end;

function CookieOptions: TMVCAuthCookieOptions;
begin
  Result := TMVCAuthCookieOptions.Default;
  Result.Name := AF_COOKIE;
  Result.Secure := False; // the tests talk plain HTTP
end;

procedure MapWhoAmI(const AGroup: TMVCRouteGroup<TObject>; const APath: string);
begin
  AGroup.MapGet<TWebContext>(APath,
    function(AContext: TWebContext): IMVCResponse
    begin
      Result := Ok(WhoAmI(AContext));
    end);
end;

procedure AddAfterRoutingCounter(const AEngine: TMVCEngine);
var
  lCounter: TAFAfterRoutingCounter;
  lCounterIntf: IMVCMiddleware;
begin
  lCounter := TAFAfterRoutingCounter.Create;
  lCounterIntf := lCounter;
  AEngine.AddMiddleware(lCounterIntf);
  AEngine.Root.MapGet('/afcount',
    function: IMVCResponse
    begin
      Result := Ok(IntToStr(lCounter.Count));
    end);
end;

// the options of a JWT bundle with the TAFHandler of APrefix
function JWTOptions(const APrefix, ALoginURL: string): TMVCJWTAuthenticationOptions;
begin
  Result := TMVCJWTAuthenticationOptions.Default;
  Result.Handler := TAFHandler.Create(APrefix);
  Result.ClaimsSetup := ClaimsSetup;
  Result.Secret := AF_SECRET;
  Result.LoginURL := ALoginURL;
end;

// the refresh token of the bundle without a login (/jwtown) and of the login of the application
function OwnLoginRefreshConfig: TMVCRefreshTokenConfig;
begin
  Result := TMVCRefreshTokenConfig.Default(AF_SECRET);
  Result.Issuer := 'af-own';
  Result.RefreshURLSegment := '/jwtown/refresh';
  Result.AccessTokenTTLSeconds := AF_RT_TTL;
end;

// a login of the application: the credentials in the jwtusername / jwtpassword headers, the pair from ACore
procedure MapOwnLogin(const AEngine: TMVCEngine; const APath: string; const ACore: IMVCRefreshTokenCore);
begin
  AEngine.Root.MapPost<TWebContext>(APath,
    function(AContext: TWebContext): IMVCResponse
    var
      lUser: string;
      lRoles: TList<string>;
      lValid: Boolean;
    begin
      lUser := AContext.Request.Headers['jwtusername'];
      lRoles := TList<string>.Create;
      try
        lValid := False;
        CheckUser(lUser, AContext.Request.Headers['jwtpassword'], lRoles, lValid);
        if not lValid then
          Exit(Unauthorized);
        ACore.IssueTokenPair(AContext, lUser, string.Join(',', lRoles.ToArray));
        Result := nil; // the answer is written
      finally
        lRoles.Free;
      end;
    end);
end;

procedure AddJWTAuthentication(const AEngine: TMVCEngine; const AWithFilters: Boolean);
var
  lMiddleware: TMVCJWTCookieAuthenticationMiddleware;
  lJWT: TMVCJWTAuthenticationOptions;
  lCookie: TMVCJWTCookieAuthenticationOptions;
begin
  AEngine.AddController(TAFController, '/jwt');
  AEngine.AddController(TAFController, '/cookie');
  AEngine.AddController(TAFController, '/jwtqs');
  AddAfterRoutingCounter(AEngine);
  if AWithFilters then
  begin
    // the token also in the access_token parameter (the middleware reads it by default)
    lJWT := JWTOptions('/jwtqs', '/jwtqs/login');
    lJWT.AccessTokenParamName := 'access_token';
    // its blacklist reads the parameter too: a token logged out is refused there as well
    lJWT.OnAcceptToken := AcceptToken();
    lJWT.OnNewJWTToBlackList := BlackListToken();
    lJWT.BlackListURL := '/jwtqs/logout';
    AEngine.Use(OnlyUnder(['/jwtqs'], JWTAuthenticationBundle(lJWT)));
    lJWT := JWTOptions('/jwt', '/login');
    lJWT.OnAcceptToken := AcceptToken();
    lJWT.OnNewJWTToBlackList := BlackListToken();
    lJWT.BlackListURL := '/jwt/logout';
    AEngine.Use(OnlyUnder(['/jwt'], JWTAuthenticationBundle(lJWT)));
    lCookie := TMVCJWTCookieAuthenticationOptions.Default;
    lCookie.Handler := TAFHandler.Create('/cookie');
    lCookie.ClaimsSetup := ClaimsSetup;
    lCookie.Secret := AF_SECRET;
    lCookie.Cookie := CookieOptions;
    lCookie.LoginURL := '/cookie/login';
    lCookie.LogoutURL := '/cookie/logout';
    lCookie.LoginAllowedOrigins := [AF_ALLOWED_ORIGIN];
    AEngine.Use(OnlyUnder(['/cookie'], JWTCookieAuthenticationBundle(lCookie)));
    // the refresh token: the login answers the pair, the access tokens carry no claims setup; the claims the
    // wizard checks; the logout also revokes the refresh token of its body
    AEngine.AddController(TAFController, '/jwtrt');
    lJWT := JWTOptions('/jwtrt', '/jwtrt/login');
    lJWT.ClaimsSetup := nil;
    lJWT.ClaimsToCheck := [TJWTCheckableClaim.ExpirationTime, TJWTCheckableClaim.NotBefore,
      TJWTCheckableClaim.IssuedAt];
    lJWT.OnAcceptToken := AcceptToken();
    lJWT.OnNewJWTToBlackList := BlackListToken();
    lJWT.BlackListURL := '/jwtrt/logout';
    lJWT.RefreshToken.Issuer := 'af-rt';
    lJWT.RefreshToken.RefreshURLSegment := '/jwtrt/refresh';
    lJWT.RefreshToken.AccessTokenTTLSeconds := AF_RT_TTL;
    lJWT.RefreshTokenStore := GRefreshTokenStore;
    AEngine.Use(OnlyUnder(['/jwtrt'], JWTAuthenticationBundle(lJWT)));
    // no login in the bundle (LoginURL ''): the login is a route of the application, which issues the pair
    // with a refresh core on the same config and store; the bundle has the refresh URL and the check
    AEngine.AddController(TAFController, '/jwtown');
    lJWT := JWTOptions('/jwtown', '');
    lJWT.ClaimsSetup := nil;
    lJWT.RefreshToken := OwnLoginRefreshConfig;
    lJWT.RefreshTokenStore := GRefreshTokenStore;
    AEngine.Use(OnlyUnder(['/jwtown'], JWTAuthenticationBundle(lJWT)));
    MapOwnLogin(AEngine, '/jwtown-login', TMVCRefreshTokenCore.Create(OwnLoginRefreshConfig, GRefreshTokenStore));
    // Minimal API: on a group the login goes on the engine and the check covers the group only. The
    // handler of /minjwt/private would make every route public: the check on the group does not ask it
    MapWhoAmI(AEngine.Root.Prefix('/minjwt').Use(JWTAuthenticationBundle(TAFHandler.Create('/nothing'), ClaimsSetup,
      AF_SECRET, '/minjwt/login')), '/private');
    MapWhoAmI(AEngine.Root.Prefix('/minjwt').Use(JWTAuthenticationBundle(TAFHandler.Create('/minjwt'), ClaimsSetup,
      AF_SECRET, '/minjwt/adminlogin')), '/admin');
    MapWhoAmI(AEngine.Root.Prefix('/minjwtopen'), '/private');
    lJWT := JWTOptions('/nothing', '/minjwtqs/login');
    lJWT.ClaimsToCheck := [];
    lJWT.AccessTokenParamName := 'access_token';
    MapWhoAmI(AEngine.Root.Prefix('/minjwtqs').Use(JWTAuthenticationBundle(lJWT)), '/private');
    // a token signed with HS256: only a bundle given HS256 accepts it
    lJWT := JWTOptions('/minjwt256', '/login256');
    lJWT.ClaimsToCheck := [];
    lJWT.HMACAlgorithm := HMAC_HS256;
    MapWhoAmI(AEngine.Root.Prefix('/minjwt256').Use(JWTAuthenticationBundle(lJWT)), '/private');
  end
  else
  begin
    // the middleware has no check of the Origin: the filter the bundle uses, in front of it
    AEngine.Use(AllowedOriginHTTPFilter([AF_ALLOWED_ORIGIN], '/cookie/login'));
    AEngine.AddMiddleware(TMVCJWTAuthenticationMiddleware.Create(TAFHandler.Create('/jwt'), ClaimsSetup,
      AF_SECRET, '/login', [TJWTCheckableClaim.ExpirationTime]));
    AEngine.AddMiddleware(TMVCJWTBlackListMiddleware.Create(AcceptToken(), BlackListToken(), '/jwt/logout'));
    lMiddleware := TMVCJWTCookieAuthenticationMiddleware.Create(TAFHandler.Create('/cookie'), ClaimsSetup,
      AF_SECRET, '/cookie/login', '/cookie/logout', [TJWTCheckableClaim.ExpirationTime]);
    lMiddleware.SetCookieName(AF_COOKIE).SetCookieSecure(False);
    AEngine.AddMiddleware(lMiddleware);
  end;
end;

procedure AddOwnLoginAuthentication(const AEngine: TMVCEngine; const AWithFilters: Boolean);
var
  lJWT: TMVCJWTAuthenticationOptions;
begin
  AEngine.AddController(TAFController, '/own');
  AEngine.AddController(TAFOwnLoginController, '/own');
  lJWT := JWTOptions('/own', ''); // no login in the bundle
  lJWT.ClaimsSetup := nil;
  AEngine.Use(JWTAuthenticationBundle(lJWT));
  // under an engine-wide bundle a Minimal API route always needs the token: no place for a login
  AEngine.Root.MapPost('/own-route-login',
    function: IMVCResponse
    begin
      Result := Ok('reached');
    end);
end;

procedure AddCustomAuthentication(const AEngine: TMVCEngine; const AWithFilters: Boolean);
begin
  AEngine.AddController(TAFController, '/custom');
  AEngine.AddController(TAFRoleController);
  AEngine.AddController(TAFHiddenRoleController);
  AEngine.Use(MemorySessionHTTPFilter(10));
  if AWithFilters then
  begin
    AEngine.Use(OnlyUnder(['/custom'], CustomAuthenticationBundle(TAFHandler.Create('/custom'))));
    // the role based handler on a login URL of its own, as the classic role based middleware below; the
    // identity is in the session, so the login of /custom serves the checks of these bundles too
    AEngine.Use(OnlyUnder(['/role', '/hiddenrole'], CustomAuthenticationBundle(TAFRoleHandler.Create,
      '/system/roles/logged')));
    // the role based handler has no attribute to read on a route: authentication only, no crash
    MapWhoAmI(AEngine.Root.Prefix('/minrole').Use(CustomAuthenticationBundle(TAFRoleHandler.Create,
      '/system/minrole/logged')), '/member');
  end
  else
  begin
    AEngine.AddMiddleware(TMVCCustomAuthenticationMiddleware.Create(TAFHandler.Create('/custom')));
    // the classic role based middleware, on its own login URL (both middleware answer /system/users/logged)
    AEngine.AddMiddleware(TMVCRoleBasedAuthMiddleware.Create(TAFRoleHandler.Create, '/system/roles/logged'));
  end;
end;

function Base64URL(const AValue: string): string;
begin
  Result := TNetEncoding.Base64URL.Encode(AValue);
end;

// a JSON document as it is, not an object to serialize
function JSONDocument(const AContext: TWebContext; const AJSON: TJsonObject): IMVCResponse;
begin
  try
    AContext.Response.ContentType := TMVCMediaType.APPLICATION_JSON;
    AContext.Response.Content := AJSON.ToJSON;
  finally
    AJSON.Free;
  end;
  Result := nil; // the answer is written
end;

// A provider for the OIDC tests: discovery, and a token endpoint that hands back an ID token for the code.
// The code is the nonce the application sent to the authorization endpoint, so the ID token carries it.
procedure MapFakeProvider(const AEngine: TMVCEngine; const AIssuer: string);
var
  lIdp: TMVCRouteGroup<TObject>;
begin
  lIdp := AEngine.Root.Prefix('/idp');
  lIdp.MapGet<TWebContext>('/.well-known/openid-configuration',
    function(AContext: TWebContext): IMVCResponse
    var
      lJSON: TJsonObject;
    begin
      lJSON := TJsonObject.Create;
      lJSON.S['issuer'] := AIssuer;
      lJSON.S['authorization_endpoint'] := AIssuer + '/authorize';
      lJSON.S['token_endpoint'] := AIssuer + '/token';
      Result := JSONDocument(AContext, lJSON);
    end);
  lIdp.MapPost<TWebContext>('/token',
    function(AContext: TWebContext): IMVCResponse
    var
      lClaims, lJSON: TJsonObject;
      lIDToken: string;
    begin
      lClaims := TJsonObject.Create;
      try
        lClaims.S['iss'] := AIssuer;
        lClaims.S['aud'] := AF_CLIENT_ID;
        lClaims.L['exp'] := DateTimeToUnix(IncHour(Now, 1), False);
        lClaims.L['iat'] := DateTimeToUnix(Now, False);
        lClaims.S['sub'] := 'alice';
        lClaims.S['nonce'] := AContext.Request.ContentParam('code');
        lIDToken := Base64URL('{"alg":"none","typ":"JWT"}') + '.' + Base64URL(lClaims.ToJSON) + '.x';
      finally
        lClaims.Free;
      end;
      lJSON := TJsonObject.Create;
      lJSON.S['id_token'] := lIDToken;
      lJSON.S['access_token'] := 'at';
      lJSON.S['token_type'] := 'Bearer';
      Result := JSONDocument(AContext, lJSON);
    end);
end;

procedure AddOIDCAuthentication(const AEngine: TMVCEngine; const AWithFilters: Boolean; const ABaseURL: string);
var
  lOptions: TMVCOIDCOptions;
begin
  AEngine.AddController(TAFController, '/oidc');
  lOptions := TMVCOIDCOptions.Default;
  lOptions.OnUserAuthenticated :=
    procedure(const AContext: TWebContext; const AIDTokenClaims, AUserInfo: TJsonObject;
      const AUserRoles: TList<string>; const ASessionData: TDictionary<string, string>)
    begin
      AUserRoles.Add('user');
    end;
  lOptions.OnAuthRequired :=
    procedure(const AContext: TWebContext; const AControllerQualifiedClassName, AActionName: string;
      var AAuthenticationRequired: Boolean)
    begin
      AAuthenticationRequired := AContext.Request.PathInfo.StartsWith('/oidc/') and
        not AActionName.StartsWith('Public');
    end;
  lOptions.Issuer := ABaseURL + '/idp';
  lOptions.ClientId := AF_CLIENT_ID;
  lOptions.ClientSecret := 'af-client-secret';
  lOptions.RedirectUri := ABaseURL + '/auth/callback';
  lOptions.JWTSecret := AF_OIDC_SECRET;
  lOptions.Cookie.Secure := False;
  lOptions.Flags := lOptions.Flags - [ofFetchUserInfo];
  if AWithFilters then
  begin
    // the provider is a group of Minimal API routes on this engine: the gate must not cover it
    AEngine.Use(OnlyUnder(['/oidc'], OIDCAuthenticationBundle(lOptions)));
    // the bundle of a group puts its login, callback and logout on the engine too: the bundle above,
    // registered first, answers those URLs
    MapWhoAmI(AEngine.Root.Prefix('/minoidc').Use(OIDCAuthenticationBundle(lOptions)), '/private');
  end
  else
    AEngine.AddMiddleware(UseOIDCAuthentication(lOptions.OnUserAuthenticated, lOptions.OnAuthRequired,
      lOptions.Issuer, lOptions.ClientId, lOptions.ClientSecret, lOptions.RedirectUri, lOptions.JWTSecret)
      .SetCookieSecure(False).SetFetchUserInfo(False));
  MapFakeProvider(AEngine, lOptions.Issuer);
end;

procedure ConfigureAuthFilterSupport(const AEngine: TMVCEngine; const ABaseURL: string);
begin
  AddJWTAuthentication(AEngine, True);
  AddOIDCAuthentication(AEngine, True, ABaseURL);
end;

initialization

GBlackList := TList<string>.Create;
GBlackListLock := TObject.Create;
GRefreshTokenStore := TMVCInMemoryRefreshTokenStore.Create;

finalization

GBlackList.Free;
GBlackListLock.Free;
GRefreshTokenStore := nil;

end.
