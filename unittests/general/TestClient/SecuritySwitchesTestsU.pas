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

unit SecuritySwitchesTestsU;

// End-to-end tests for two switches whose default is the 3.4 behaviour:
// TMVCJWTAuthenticationMiddleware.AuthorizationAccessTokenParamName and the
// CORS credentials rule. A unit test on the property proves the API exists;
// only a request on the wire proves the middleware honours it, and that is
// where the value of these two changes lives.
//
// Three embedded servers, because the settings under test are per-engine:
// one with the default CORS middleware (wildcard origin), one with an explicit
// origin, one with JWT.

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
  MVCFramework.Server.Intf,
  MVCFramework.Middleware.JWT;

const
  CORS_WILDCARD_PORT = 8893;
  CORS_ORIGIN_PORT   = 8894;
  JWT_PORT           = 8895;
  ALLOWED_ORIGIN     = 'https://app.example';
  TEST_SECRET        = 'a-secret-long-enough-not-to-be-refused-at-startup';

type
  [MVCPath('/api')]
  TSecuritySwitchesController = class(TMVCController)
  public
    [MVCPath('/public')]
    [MVCHTTPMethod([httpGET])]
    function Public_: String;
    [MVCPath('/protected')]
    [MVCHTTPMethod([httpGET])]
    function Protected_: String;
  end;

  // Requires a token on /api/protected and nothing else.
  TSwitchesAuthHandler = class(TInterfacedObject, IMVCAuthenticationHandler)
  protected
    procedure OnRequest(const AContext: TWebContext; const AControllerQualifiedClassName: string;
      const AActionName: string; var AAuthenticationRequired: Boolean);
    procedure OnAuthentication(const AContext: TWebContext; const AUserName: string;
      const APassword: string; AUserRoles: TList<string>; var AIsValid: Boolean;
      const ASessionData: TSessionData);
    procedure OnAuthorization(const AContext: TWebContext; AUserRoles: TList<string>;
      const AControllerQualifiedClassName: string; const AActionName: string;
      var AIsAuthorized: Boolean);
  end;

  [TestFixture]
  TSecuritySwitchesTests = class
  private
    FCORSWildcard: IMVCServer;
    FCORSOrigin: IMVCServer;
    FJWTServer: IMVCServer;
    FJWTMiddleware: TMVCJWTAuthenticationMiddleware;
    FClient: THTTPClient;
    function GetWithOrigin(const AURL, AOrigin: string): IHTTPResponse;
    function Login: string;
  public
    [SetupFixture]
    procedure SetupFixture;
    [TeardownFixture]
    procedure TeardownFixture;
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;

    [Test]
    procedure WildcardOriginCarriesNoCredentialsHeader;
    [Test]
    procedure ExplicitOriginKeepsCredentialsAndVariesOnOrigin;
    [Test]
    procedure AccessTokenInTheQueryStringIsAcceptedByDefault;
    [Test]
    procedure AccessTokenInTheQueryStringIsRefusedWhenTheNameIsCleared;
  end;

implementation

uses
  System.JSON.Types,
  JsonDataObjects,
  MVCFramework.Serializer.Commons,
  MVCFramework.Middleware.CORS,
  MVCFramework.JWT,
  MVCFramework.Server.Factory;

{ TSecuritySwitchesController }

function TSecuritySwitchesController.Public_: String;
begin
  Result := 'public';
end;

function TSecuritySwitchesController.Protected_: String;
begin
  Result := 'protected';
end;

{ TSwitchesAuthHandler }

procedure TSwitchesAuthHandler.OnRequest(const AContext: TWebContext;
  const AControllerQualifiedClassName: string; const AActionName: string;
  var AAuthenticationRequired: Boolean);
begin
  AAuthenticationRequired := SameText(AActionName, 'Protected_');
end;

procedure TSwitchesAuthHandler.OnAuthentication(const AContext: TWebContext;
  const AUserName: string; const APassword: string; AUserRoles: TList<string>;
  var AIsValid: Boolean; const ASessionData: TSessionData);
begin
  AIsValid := (AUserName = 'user1') and (APassword = 'user1');
  if AIsValid then
    AUserRoles.Add('user');
end;

procedure TSwitchesAuthHandler.OnAuthorization(const AContext: TWebContext;
  AUserRoles: TList<string>; const AControllerQualifiedClassName: string;
  const AActionName: string; var AIsAuthorized: Boolean);
begin
  AIsAuthorized := True;
end;

{ TSecuritySwitchesTests }

procedure TSecuritySwitchesTests.SetupFixture;
var
  lEngine: TMVCEngine;
  lJWTMiddleware: TMVCJWTAuthenticationMiddleware;
begin
  // 1. CORS with the constructor defaults: origin '*', credentials True.
  lEngine := TMVCEngine.Create;
  lEngine.AddController(TSecuritySwitchesController);
  lEngine.AddMiddleware(TMVCCORSMiddleware.Create);
  FCORSWildcard := TMVCServerFactory.CreateIndyDirect(lEngine);
  FCORSWildcard.Listen(CORS_WILDCARD_PORT);

  // 2. Same, with one explicit origin.
  lEngine := TMVCEngine.Create;
  lEngine.AddController(TSecuritySwitchesController);
  lEngine.AddMiddleware(TMVCCORSMiddleware.Create(ALLOWED_ORIGIN, True));
  FCORSOrigin := TMVCServerFactory.CreateIndyDirect(lEngine);
  FCORSOrigin.Listen(CORS_ORIGIN_PORT);

  // 3. JWT. The middleware instance is kept so a test can clear the parameter
  //    name at runtime, which is the whole point of the property.
  lJWTMiddleware := TMVCJWTAuthenticationMiddleware.Create(
    TSwitchesAuthHandler.Create,
    procedure(const JWT: TJWT)
    begin
      JWT.Claims.Issuer := 'security-switches-tests';
      JWT.Claims.ExpirationTime := Now + 1;
      JWT.Claims.NotBefore := Now - 1;
      JWT.Claims.IssuedAt := Now;
    end,
    TEST_SECRET,
    '/login',
    [TJWTCheckableClaim.ExpirationTime, TJWTCheckableClaim.NotBefore, TJWTCheckableClaim.IssuedAt],
    300);
  FJWTMiddleware := lJWTMiddleware;
  lEngine := TMVCEngine.Create;
  lEngine.AddController(TSecuritySwitchesController);
  lEngine.AddMiddleware(lJWTMiddleware);
  FJWTServer := TMVCServerFactory.CreateIndyDirect(lEngine);
  FJWTServer.Listen(JWT_PORT);
end;

procedure TSecuritySwitchesTests.TeardownFixture;
begin
  FCORSWildcard.Stop;
  FCORSWildcard := nil;
  FCORSOrigin.Stop;
  FCORSOrigin := nil;
  FJWTServer.Stop;
  FJWTServer := nil;
  FJWTMiddleware := nil;
end;

procedure TSecuritySwitchesTests.Setup;
begin
  FClient := THTTPClient.Create;
end;

procedure TSecuritySwitchesTests.Teardown;
begin
  FClient.Free;
end;

function TSecuritySwitchesTests.GetWithOrigin(const AURL, AOrigin: string): IHTTPResponse;
var
  lHeaders: TNetHeaders;
begin
  SetLength(lHeaders, 1);
  lHeaders[0] := TNetHeader.Create('Origin', AOrigin);
  Result := FClient.Get(AURL, nil, lHeaders);
end;

function TSecuritySwitchesTests.Login: string;
var
  lHeaders: TNetHeaders;
  lResp: IHTTPResponse;
  lJSON: TJsonObject;
begin
  SetLength(lHeaders, 2);
  lHeaders[0] := TNetHeader.Create(TMVCJWTDefaults.USERNAME_HEADER, 'user1');
  lHeaders[1] := TNetHeader.Create(TMVCJWTDefaults.PASSWORD_HEADER, 'user1');
  lResp := FClient.Post('http://localhost:' + JWT_PORT.ToString + '/login', TStream(nil), nil, lHeaders);
  Assert.AreEqual(200, lResp.StatusCode, 'login failed: ' + lResp.ContentAsString);
  lJSON := StrToJSONObject(lResp.ContentAsString);
  try
    Result := lJSON.S['token'];
  finally
    lJSON.Free;
  end;
  Assert.IsNotEmpty(Result, 'login returned no token');
end;

procedure TSecuritySwitchesTests.WildcardOriginCarriesNoCredentialsHeader;
var
  lResp: IHTTPResponse;
begin
  { TMVCCORSMiddleware.Create defaults to origin '*' AND credentials True. A
    browser refuses that pair, so the header only ever advertised something no
    client could use - and it became real the day someone replaced '*' with a
    domain. }
  lResp := GetWithOrigin('http://localhost:' + CORS_WILDCARD_PORT.ToString +
    '/api/public', ALLOWED_ORIGIN);
  Assert.AreEqual(200, lResp.StatusCode);
  Assert.AreEqual('*', lResp.HeaderValue['Access-Control-Allow-Origin']);
  Assert.AreEqual('', lResp.HeaderValue['Access-Control-Allow-Credentials'],
    'credentials advertised on a wildcard origin');
end;

procedure TSecuritySwitchesTests.ExplicitOriginKeepsCredentialsAndVariesOnOrigin;
var
  lResp: IHTTPResponse;
begin
  { The other half: with a real origin the header is legitimate and must stay,
    and the response must vary on Origin so a shared cache does not hand one
    origin's response to another. }
  lResp := GetWithOrigin('http://localhost:' + CORS_ORIGIN_PORT.ToString +
    '/api/public', ALLOWED_ORIGIN);
  Assert.AreEqual(200, lResp.StatusCode);
  Assert.AreEqual(ALLOWED_ORIGIN, lResp.HeaderValue['Access-Control-Allow-Origin']);
  Assert.AreEqual('true', lResp.HeaderValue['Access-Control-Allow-Credentials'],
    'credentials must survive on a configured origin');
  Assert.Contains(lResp.HeaderValue['Vary'], 'Origin',
    'a reflected origin must be cached per-origin');
end;

procedure TSecuritySwitchesTests.AccessTokenInTheQueryStringIsAcceptedByDefault;
var
  lToken: string;
  lResp: IHTTPResponse;
begin
  { The 3.4 behaviour, kept on purpose: SSE endpoints, <img> tags and download
    links cannot send an Authorization header. }
  Assert.AreEqual('access_token', FJWTMiddleware.AuthorizationAccessTokenParamName);
  lToken := Login;
  lResp := FClient.Get('http://localhost:' + JWT_PORT.ToString +
    '/api/protected?access_token=' + lToken);
  Assert.AreEqual(200, lResp.StatusCode, lResp.ContentAsString);
end;

procedure TSecuritySwitchesTests.AccessTokenInTheQueryStringIsRefusedWhenTheNameIsCleared;
var
  lToken: string;
  lResp: IHTTPResponse;
  lHeaders: TNetHeaders;
begin
  lToken := Login;
  FJWTMiddleware.AuthorizationAccessTokenParamName := '';
  try
    lResp := FClient.Get('http://localhost:' + JWT_PORT.ToString +
      '/api/protected?access_token=' + lToken);
    Assert.AreEqual(401, lResp.StatusCode,
      'the token was still read from the URL after the parameter name was cleared');

    // The same token in the header is still accepted: only the URL is refused.
    SetLength(lHeaders, 1);
    lHeaders[0] := TNetHeader.Create('Authorization', 'Bearer ' + lToken);
    lResp := FClient.Get('http://localhost:' + JWT_PORT.ToString +
      '/api/protected', nil, lHeaders);
    Assert.AreEqual(200, lResp.StatusCode, lResp.ContentAsString);
  finally
    FJWTMiddleware.AuthorizationAccessTokenParamName := 'access_token';
  end;
end;

initialization

TDUnitX.RegisterTestFixture(TSecuritySwitchesTests);

end.
