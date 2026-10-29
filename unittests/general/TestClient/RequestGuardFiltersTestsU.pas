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

unit RequestGuardFiltersTestsU;

// BearerAuthHTTPFilter, AllowedOriginHTTPFilter, AllowedHostHTTPFilter on the wire: two
// embedded Indy servers, raw requests (TIdHTTP) so Host, Origin and the path
// are exactly what the test writes.

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Server.Intf;

const
  GUARD_AUTH_PORT = 8901;
  GUARD_HOST_PORT = 8902;
  GUARD_NAMELESS_PORT = 8903;
  GUARD_METHODS_PORT = 8904;

type
  [MVCPath('/')]
  TGuardedController = class(TMVCController)
  public
    [MVCPath('/guarded')]
    [MVCHTTPMethod([httpGET, httpPOST])]
    function Guarded: String;
    [MVCPath('/guarded/sub')]
    [MVCHTTPMethod([httpGET])]
    function GuardedSub: String;
    [MVCPath('/open')]
    [MVCHTTPMethod([httpGET])]
    function Open: String;
  end;

  [TestFixture]
  TRequestGuardFiltersTests = class
  private
    fAuthServer: IMVCServer;
    fHostServer: IMVCServer;
    fNamelessServer: IMVCServer;
    fMethodsServer: IMVCServer;
    fEngines: TArray<TMVCEngine>; // the servers do not own their engines
    function RawStatus(const APort: Integer; const ALines: array of string): Integer;
    function Get(const APort: Integer; const APath: string; const AAuthorization: string = '';
      const AOrigin: string = ''; const AHost: string = ''): Integer;
    function GetFull(const APort: Integer; const APath: string; const AAuthorization: string;
      out AWWWAuthenticate, ABody: string): Integer;
  public
    [SetupFixture]
    procedure SetupFixture;
    [TeardownFixture]
    procedure TeardownFixture;

    [Test]
    procedure NormalizePathMatchesTheRouterShape;
    [Test]
    procedure StaticTokensRefuseEmptyAndPaddedTokens;
    [Test]
    procedure BearerRefusesMissingWrongAndMalformedTokens;
    [Test]
    procedure BearerAcceptsTheRightTokenAndSetsTheLoggedUser;
    [Test]
    procedure BearerGuardsTheSubPathsAndTheOtherFormsOfThePath;
    [Test]
    procedure BearerLeavesTheOtherPathsAlone;
    [Test]
    procedure OriginNotInTheListIsRefused;
    [Test]
    procedure OriginInTheListOrAbsentPasses;
    [Test]
    procedure OriginIsCheckedOnlyOnTheGuardedPath;
    [Test]
    procedure HostOutsideTheListIsRefused;
    [Test]
    procedure HostLoopbackAndDeclaredHostsPass;
    [Test]
    procedure HostIPv6AndMissingHost;
    [Test]
    procedure AnAcceptedTokenWithNoUserIsRefused;
    [Test]
    procedure MoreThanOneTokenEachWorksAndIsCaseSensitive;
    [Test]
    procedure TheVerbDoesNotMatterToTheGuards;
    [Test]
    procedure OddPathsNeverReachTheHandlerWithoutAToken;
    [Test]
    procedure MalformedEscapesAreNotAServerError;
    [Test]
    procedure DuplicatedOriginHeadersAreNotAWayAround;
    [Test]
    procedure RefusedRequestsNeverReachTheHandler;
    [Test]
    procedure AllowedMethodsLimitsTheExactPathToTheVerbs;
  end;

implementation

uses
  System.SyncObjs,
  System.Generics.Collections,
  IdHTTP,
  IdTCPClient,
  MVCFramework.Server.Factory,
  MVCFramework.MinimalAPI,
  MVCFramework.Filters;

var
  GuardedHits: Integer = 0;

const
  TOKEN = 'a-token-long-enough-for-the-test';
  TOKEN2 = 'another-token-long-enough-for-the-test';
  TOKEN_USER = 'cli';
  ORIGIN_OK = 'https://app.example';

{ TGuardedController }

function TGuardedController.Guarded: String;
begin
  TInterlocked.Increment(GuardedHits);
  Result := 'user=' + Context.LoggedUser.UserName;
end;

function TGuardedController.GuardedSub: String;
begin
  TInterlocked.Increment(GuardedHits);
  Result := 'sub';
end;

function TGuardedController.Open: String;
begin
  Result := 'open';
end;

{ TRequestGuardFiltersTests }

procedure TRequestGuardFiltersTests.SetupFixture;
var
  lEngine: TMVCEngine;
begin
  // token + origin, on /guarded only
  lEngine := TMVCEngine.Create;
  fEngines := fEngines + [lEngine];
  lEngine.AddController(TGuardedController);
  lEngine.Use(BearerAuthHTTPFilter(
    StaticBearerTokens([TPair<string, string>.Create(TOKEN_USER, TOKEN),
      TPair<string, string>.Create('ci', TOKEN2)]), '/guarded'));
  lEngine.Use(AllowedOriginHTTPFilter([ORIGIN_OK], '/guarded'));
  fAuthServer := TMVCServerFactory.CreateIndyDirect(lEngine);
  fAuthServer.Listen(GUARD_AUTH_PORT);

  // host, on /guarded only
  lEngine := TMVCEngine.Create;
  fEngines := fEngines + [lEngine];
  lEngine.AddController(TGuardedController);
  lEngine.Use(AllowedHostHTTPFilter(['my.example'], '/guarded'));
  fHostServer := TMVCServerFactory.CreateIndyDirect(lEngine);
  fHostServer.Listen(GUARD_HOST_PORT);

  // a validator that accepts the token but names nobody
  lEngine := TMVCEngine.Create;
  fEngines := fEngines + [lEngine];
  lEngine.AddController(TGuardedController);
  lEngine.Use(BearerAuthHTTPFilter(
    function(const AToken: string; out AUserName: string; var ARoles: TArray<string>): Boolean
    begin
      AUserName := '';
      Result := AToken = TOKEN;
    end, '/guarded'));
  fNamelessServer := TMVCServerFactory.CreateIndyDirect(lEngine);
  fNamelessServer.Listen(GUARD_NAMELESS_PORT);

  // verbs, on /guarded only
  lEngine := TMVCEngine.Create;
  fEngines := fEngines + [lEngine];
  lEngine.AddController(TGuardedController);
  lEngine.Use(AllowedMethodsHTTPFilter([httpPOST], '/guarded'));
  fMethodsServer := TMVCServerFactory.CreateIndyDirect(lEngine);
  fMethodsServer.Listen(GUARD_METHODS_PORT);
end;

procedure TRequestGuardFiltersTests.AllowedMethodsLimitsTheExactPathToTheVerbs;
begin
  Assert.AreEqual(200, RawStatus(GUARD_METHODS_PORT, ['POST /guarded HTTP/1.1', 'Host: localhost', 'Content-Length: 0']));
  Assert.AreEqual(405, RawStatus(GUARD_METHODS_PORT, ['GET /guarded HTTP/1.1', 'Host: localhost']), 'verb not allowed');
  Assert.AreEqual(405, RawStatus(GUARD_METHODS_PORT, ['GET /guarded/sub HTTP/1.1', 'Host: localhost']), 'not the exact path');
  Assert.AreEqual(200, RawStatus(GUARD_METHODS_PORT, ['GET /open HTTP/1.1', 'Host: localhost']), 'other paths go on');
  Assert.AreEqual(405, RawStatus(GUARD_METHODS_PORT, ['GET /%67uarded HTTP/1.1', 'Host: localhost']), 'encoded form');
  Assert.WillRaise(
    procedure begin AllowedMethodsHTTPFilter([httpPOST], '/'); end, EMVCException);
end;

procedure TRequestGuardFiltersTests.TeardownFixture;
var
  lEngine: TMVCEngine;
begin
  fMethodsServer.Stop;
  fMethodsServer := nil;
  fAuthServer.Stop;
  fAuthServer := nil;
  fHostServer.Stop;
  fHostServer := nil;
  fNamelessServer.Stop;
  fNamelessServer := nil;
  for lEngine in fEngines do
    lEngine.Free;
  fEngines := nil;
end;

function TRequestGuardFiltersTests.RawStatus(const APort: Integer; const ALines: array of string): Integer;
var
  lClient: TIdTCPClient;
  lLine: string;
  lParts: TArray<string>;
begin
  // written by hand: TIdHTTP rewrites Host from the URL, and Host, verb and duplicates are what is under test
  lClient := TIdTCPClient.Create(nil);
  try
    lClient.Host := '127.0.0.1';
    lClient.Port := APort;
    lClient.ConnectTimeout := 3000;
    lClient.ReadTimeout := 5000;
    lClient.Connect;
    for lLine in ALines do
      lClient.IOHandler.WriteLn(lLine);
    lClient.IOHandler.WriteLn('Connection: close');
    lClient.IOHandler.WriteLn('');
    lParts := lClient.IOHandler.ReadLn.Split([' ']); // HTTP/1.1 403 Forbidden
    Result := StrToInt(lParts[1]);
  finally
    lClient.Free;
  end;
end;

function TRequestGuardFiltersTests.Get(const APort: Integer; const APath, AAuthorization,
  AOrigin, AHost: string): Integer;
var
  lLines: TArray<string>;
begin
  lLines := ['GET ' + APath + ' HTTP/1.1'];
  if AHost <> '' then
    lLines := lLines + ['Host: ' + AHost]
  else
    lLines := lLines + ['Host: localhost'];
  if AAuthorization <> '' then
    lLines := lLines + ['Authorization: ' + AAuthorization];
  if AOrigin <> '' then
    lLines := lLines + ['Origin: ' + AOrigin];
  Result := RawStatus(APort, lLines);
end;

function TRequestGuardFiltersTests.GetFull(const APort: Integer; const APath, AAuthorization: string;
  out AWWWAuthenticate, ABody: string): Integer;
var
  lHTTP: TIdHTTP;
begin
  lHTTP := TIdHTTP.Create(nil);
  try
    lHTTP.HTTPOptions := lHTTP.HTTPOptions + [hoNoProtocolErrorException, hoWantProtocolErrorContent];
    if AAuthorization <> '' then
      lHTTP.Request.CustomHeaders.Values['Authorization'] := AAuthorization;
    ABody := lHTTP.Get('http://127.0.0.1:' + APort.ToString + APath);
    Result := lHTTP.ResponseCode;
    AWWWAuthenticate := lHTTP.Response.WWWAuthenticate.Text;
    if AWWWAuthenticate = '' then
      AWWWAuthenticate := lHTTP.Response.RawHeaders.Values['WWW-Authenticate'];
  finally
    lHTTP.Free;
  end;
end;

procedure TRequestGuardFiltersTests.NormalizePathMatchesTheRouterShape;
begin
  Assert.AreEqual('', MVCNormalizePath(''));
  Assert.AreEqual('', MVCNormalizePath('/'));
  Assert.AreEqual('/mcp', MVCNormalizePath('/mcp/'));
  Assert.AreEqual('/mcp', MVCNormalizePath('MCP'));
  Assert.AreEqual('/mcp', MVCNormalizePath(' /Mcp/?x=1 '));
  Assert.AreEqual('/mcp/a', MVCNormalizePath('/MCP/a//'));
end;

procedure TRequestGuardFiltersTests.StaticTokensRefuseEmptyAndPaddedTokens;
var
  lToken: string;
  lRaised: Boolean;
begin
  for lToken in ['', '   ', ' secret', 'secret '] do
  begin
    lRaised := False;
    try
      StaticBearerTokens([TPair<string, string>.Create('a', lToken)]);
    except
      on EMVCException do
        lRaised := True;
    end;
    Assert.IsTrue(lRaised, 'token "' + lToken + '" must be refused');
  end;
  // a token has to stand for somebody: it becomes the logged user
  lRaised := False;
  try
    StaticBearerTokens([TPair<string, string>.Create('  ', 'secret')]);
  except
    on EMVCException do
      lRaised := True;
  end;
  Assert.IsTrue(lRaised, 'a token with no name must be refused');
end;

procedure TRequestGuardFiltersTests.BearerRefusesMissingWrongAndMalformedTokens;
var
  lWWW, lBody: string;
begin
  Assert.AreEqual(401, GetFull(GUARD_AUTH_PORT, '/guarded', '', lWWW, lBody), 'no Authorization');
  Assert.AreEqual('Bearer', Trim(lWWW), 'a 401 says which scheme to use');
  Assert.AreEqual(401, GetFull(GUARD_AUTH_PORT, '/guarded', 'Bearer wrong-token', lWWW, lBody));
  Assert.AreEqual(401, GetFull(GUARD_AUTH_PORT, '/guarded', 'Bearer ', lWWW, lBody), 'empty token');
  Assert.AreEqual(401, GetFull(GUARD_AUTH_PORT, '/guarded', 'Basic ' + TOKEN, lWWW, lBody), 'wrong scheme');
  Assert.AreEqual(401, GetFull(GUARD_AUTH_PORT, '/guarded', TOKEN, lWWW, lBody), 'no scheme');
  Assert.AreEqual(401, GetFull(GUARD_AUTH_PORT, '/guarded', 'Bearer ' + TOKEN + 'x', lWWW, lBody), 'longer token');
  Assert.AreEqual(401, GetFull(GUARD_AUTH_PORT, '/guarded', 'Bearer ' + Copy(TOKEN, 1, Length(TOKEN) - 1), lWWW, lBody),
    'shorter token');
end;

procedure TRequestGuardFiltersTests.BearerAcceptsTheRightTokenAndSetsTheLoggedUser;
var
  lWWW, lBody: string;
begin
  Assert.AreEqual(200, GetFull(GUARD_AUTH_PORT, '/guarded', 'Bearer ' + TOKEN, lWWW, lBody));
  Assert.AreEqual('user=' + TOKEN_USER, lBody);
  Assert.AreEqual(200, GetFull(GUARD_AUTH_PORT, '/guarded', 'bearer ' + TOKEN, lWWW, lBody), 'the scheme is case-insensitive');
end;

procedure TRequestGuardFiltersTests.BearerGuardsTheSubPathsAndTheOtherFormsOfThePath;
begin
  // the router is case-insensitive and ignores the trailing slash: the guard must too
  Assert.AreEqual(401, Get(GUARD_AUTH_PORT, '/guarded/'));
  Assert.AreEqual(401, Get(GUARD_AUTH_PORT, '/GUARDED'));
  Assert.AreEqual(401, Get(GUARD_AUTH_PORT, '/Guarded/Sub'));
  Assert.AreEqual(401, Get(GUARD_AUTH_PORT, '/guarded/sub'));
  Assert.AreEqual(401, Get(GUARD_AUTH_PORT, '/guarded?x=1'));
  Assert.AreEqual(401, Get(GUARD_AUTH_PORT, '/guarded%2fsub'), 'an encoded slash must not slip past');
  Assert.AreEqual(200, Get(GUARD_AUTH_PORT, '/guarded/sub', 'Bearer ' + TOKEN));
end;

procedure TRequestGuardFiltersTests.BearerLeavesTheOtherPathsAlone;
begin
  Assert.AreEqual(200, Get(GUARD_AUTH_PORT, '/open'));
  // a path that only starts with the prefix is another path
  Assert.AreEqual(404, Get(GUARD_AUTH_PORT, '/guardedx'));
end;

procedure TRequestGuardFiltersTests.OriginNotInTheListIsRefused;
begin
  Assert.AreEqual(403, Get(GUARD_AUTH_PORT, '/guarded', 'Bearer ' + TOKEN, 'https://evil.example'));
  Assert.AreEqual(403, Get(GUARD_AUTH_PORT, '/guarded', 'Bearer ' + TOKEN, ORIGIN_OK + '.evil.example'),
    'an origin is compared whole');
  Assert.AreEqual(403, Get(GUARD_AUTH_PORT, '/guarded', 'Bearer ' + TOKEN, 'null'));
end;

procedure TRequestGuardFiltersTests.OriginInTheListOrAbsentPasses;
begin
  Assert.AreEqual(200, Get(GUARD_AUTH_PORT, '/guarded', 'Bearer ' + TOKEN, ORIGIN_OK));
  Assert.AreEqual(200, Get(GUARD_AUTH_PORT, '/guarded', 'Bearer ' + TOKEN, UpperCase(ORIGIN_OK)));
  Assert.AreEqual(200, Get(GUARD_AUTH_PORT, '/guarded', 'Bearer ' + TOKEN), 'no Origin: not a browser request');
end;

procedure TRequestGuardFiltersTests.OriginIsCheckedOnlyOnTheGuardedPath;
begin
  Assert.AreEqual(200, Get(GUARD_AUTH_PORT, '/open', '', 'https://evil.example'));
end;

procedure TRequestGuardFiltersTests.HostOutsideTheListIsRefused;
begin
  Assert.AreEqual(403, Get(GUARD_HOST_PORT, '/guarded', '', '', 'rebound.evil.example'));
  Assert.AreEqual(403, Get(GUARD_HOST_PORT, '/guarded', '', '', 'rebound.evil.example:' + GUARD_HOST_PORT.ToString));
  Assert.AreEqual(403, Get(GUARD_HOST_PORT, '/guarded/sub', '', '', 'localhost.evil.example'));
  Assert.AreEqual(200, Get(GUARD_HOST_PORT, '/open', '', '', 'rebound.evil.example'), 'only the guarded path');
end;

procedure TRequestGuardFiltersTests.HostLoopbackAndDeclaredHostsPass;
begin
  Assert.AreEqual(200, Get(GUARD_HOST_PORT, '/guarded', '', '', 'localhost:' + GUARD_HOST_PORT.ToString));
  Assert.AreEqual(200, Get(GUARD_HOST_PORT, '/guarded', '', '', '127.0.0.1:' + GUARD_HOST_PORT.ToString));
  Assert.AreEqual(200, Get(GUARD_HOST_PORT, '/guarded', '', '', 'LOCALHOST'));
  Assert.AreEqual(200, Get(GUARD_HOST_PORT, '/guarded', '', '', 'my.example:8080'), 'a declared host, any port');
end;

procedure TRequestGuardFiltersTests.HostIPv6AndMissingHost;
begin
  Assert.AreEqual(200, Get(GUARD_HOST_PORT, '/guarded', '', '', '[::1]'));
  Assert.AreEqual(200, Get(GUARD_HOST_PORT, '/guarded', '', '', '[::1]:' + GUARD_HOST_PORT.ToString));
  Assert.AreEqual(403, Get(GUARD_HOST_PORT, '/guarded', '', '', '[::2]:' + GUARD_HOST_PORT.ToString));
  Assert.AreEqual(403, Get(GUARD_HOST_PORT, '/guarded', '', '', '::1'), 'an IPv6 literal without brackets is not a Host');
  // what follows the host must be a port, nothing else
  Assert.AreEqual(403, Get(GUARD_HOST_PORT, '/guarded', '', '', 'localhost:80@evil.example'));
  Assert.AreEqual(403, Get(GUARD_HOST_PORT, '/guarded', '', '', '[::1]evil'));
  Assert.AreEqual(403, Get(GUARD_HOST_PORT, '/guarded', '', '', '[::1'));
  Assert.AreEqual(403, Get(GUARD_HOST_PORT, '/guarded', '', '', 'localhost:'));
  Assert.AreEqual(403, Get(GUARD_HOST_PORT, '/guarded', '', '', 'localhost:80:80'));
  Assert.AreEqual(403, Get(GUARD_HOST_PORT, '/guarded', '', '', 'localhost:8o'));
  // HTTP/1.0 with no Host header at all: nothing vouches for the name, so refused
  Assert.AreEqual(403, RawStatus(GUARD_HOST_PORT, ['GET /guarded HTTP/1.0']));
  Assert.AreEqual(200, RawStatus(GUARD_HOST_PORT, ['GET /open HTTP/1.0']), 'the other paths do not care');
end;

procedure TRequestGuardFiltersTests.MoreThanOneTokenEachWorksAndIsCaseSensitive;
var
  lWWW, lBody: string;
begin
  Assert.AreEqual(200, GetFull(GUARD_AUTH_PORT, '/guarded', 'Bearer ' + TOKEN2, lWWW, lBody));
  Assert.AreEqual('user=ci', lBody, 'the identity is the one of the token that matched');
  Assert.AreEqual(200, GetFull(GUARD_AUTH_PORT, '/guarded', 'Bearer ' + TOKEN, lWWW, lBody));
  Assert.AreEqual('user=' + TOKEN_USER, lBody);
  Assert.AreEqual(401, Get(GUARD_AUTH_PORT, '/guarded', 'Bearer ' + UpperCase(TOKEN)), 'a token is case-sensitive');
  Assert.AreEqual(401, Get(GUARD_AUTH_PORT, '/guarded', 'Bearer ' + TOKEN + ' ' + TOKEN2), 'two tokens are not a token');
end;

procedure TRequestGuardFiltersTests.TheVerbDoesNotMatterToTheGuards;
var
  lVerb: string;
begin
  for lVerb in ['GET', 'POST', 'PUT', 'PATCH', 'DELETE', 'HEAD', 'OPTIONS', 'QUERY'] do
  begin
    Assert.AreEqual(401, RawStatus(GUARD_AUTH_PORT, [lVerb + ' /guarded HTTP/1.1', 'Host: localhost']),
      lVerb + ' without a token');
    Assert.AreEqual(403, RawStatus(GUARD_AUTH_PORT, [lVerb + ' /guarded HTTP/1.1', 'Host: localhost',
      'Authorization: Bearer ' + TOKEN, 'Origin: https://evil.example']), lVerb + ' with a foreign Origin');
    Assert.AreEqual(403, RawStatus(GUARD_HOST_PORT, [lVerb + ' /guarded HTTP/1.1', 'Host: evil.example']),
      lVerb + ' with a foreign Host');
  end;
end;

procedure TRequestGuardFiltersTests.OddPathsNeverReachTheHandlerWithoutAToken;
var
  lPath: string;
  lStatus, lBefore: Integer;
begin
  lBefore := TInterlocked.CompareExchange(GuardedHits, 0, 0);
  // whatever the router does with these, a request without a token must not run the handler
  for lPath in ['//guarded', '/guarded//', '/guarded//sub', '/./guarded', '/guarded/.', '/guarded/../guarded',
    '/guarded/%2e%2e/guarded', '/guarded%2Fsub', '/guarded%252fsub', '/%67uarded', '/guarded%00', '/guarded;x=1',
    '/guarded/%2e%2e/open'] do
  begin
    lStatus := Get(GUARD_AUTH_PORT, lPath);
    Assert.IsTrue(lStatus <> 200, lPath + ' answered 200 without a token');
    Assert.IsTrue(lStatus < 500, lPath + ' answered ' + lStatus.ToString);
  end;
  Assert.AreEqual(lBefore, TInterlocked.CompareExchange(GuardedHits, 0, 0), 'the handler ran without a token');
  // the ones that stay under the prefix are refused by the guard itself
  Assert.AreEqual(401, Get(GUARD_AUTH_PORT, '/guarded/%2e%2e/guarded'));
  Assert.AreEqual(401, Get(GUARD_AUTH_PORT, '/guarded/%2e%2e/open'));
end;

procedure TRequestGuardFiltersTests.MalformedEscapesAreNotAServerError;
var
  lPath: string;
begin
  for lPath in ['/guarded%', '/guarded%z', '/guarded%zz', '/guarded%2', '/open%zz', '/%', '/%ff%fe'] do
    Assert.IsTrue(Get(GUARD_AUTH_PORT, lPath) < 500, lPath + ' broke the server');
  Assert.AreEqual(200, Get(GUARD_AUTH_PORT, '/guarded', 'Bearer ' + TOKEN), 'and the server is still up');
end;

procedure TRequestGuardFiltersTests.DuplicatedOriginHeadersAreNotAWayAround;
begin
  // a list of origins is not an allowed origin
  Assert.AreEqual(403, RawStatus(GUARD_AUTH_PORT, ['GET /guarded HTTP/1.1', 'Host: localhost',
    'Authorization: Bearer ' + TOKEN, 'Origin: https://evil.example, ' + ORIGIN_OK]));
  // two Origin headers: the one the server reads decides, and a foreign one must not be rescued by the allowed one
  Assert.AreEqual(403, RawStatus(GUARD_AUTH_PORT, ['GET /guarded HTTP/1.1', 'Host: localhost',
    'Authorization: Bearer ' + TOKEN, 'Origin: https://evil.example', 'Origin: ' + ORIGIN_OK]));
end;

procedure TRequestGuardFiltersTests.RefusedRequestsNeverReachTheHandler;
var
  lBefore: Integer;
begin
  lBefore := TInterlocked.CompareExchange(GuardedHits, 0, 0);
  Assert.AreEqual(401, Get(GUARD_AUTH_PORT, '/guarded'));
  Assert.AreEqual(401, Get(GUARD_AUTH_PORT, '/guarded', 'Bearer ' + TOKEN + 'x'));
  Assert.AreEqual(403, Get(GUARD_AUTH_PORT, '/guarded', 'Bearer ' + TOKEN, 'https://evil.example'));
  Assert.AreEqual(403, Get(GUARD_HOST_PORT, '/guarded', '', '', 'evil.example'));
  Assert.AreEqual(lBefore, TInterlocked.CompareExchange(GuardedHits, 0, 0), 'a refused request ran the handler');
  Assert.AreEqual(200, Get(GUARD_AUTH_PORT, '/guarded', 'Bearer ' + TOKEN));
  Assert.AreEqual(lBefore + 1, TInterlocked.CompareExchange(GuardedHits, 0, 0));
end;

procedure TRequestGuardFiltersTests.AnAcceptedTokenWithNoUserIsRefused;
var
  lBefore: Integer;
begin
  // LoggedUser without a name is "nobody": the guard answers 401 instead of letting the handler run as nobody
  lBefore := TInterlocked.CompareExchange(GuardedHits, 0, 0);
  Assert.AreEqual(401, Get(GUARD_NAMELESS_PORT, '/guarded', 'Bearer ' + TOKEN));
  Assert.AreEqual(lBefore, TInterlocked.CompareExchange(GuardedHits, 0, 0), 'the handler ran');
end;

initialization

TDUnitX.RegisterTestFixture(TRequestGuardFiltersTests);

end.
