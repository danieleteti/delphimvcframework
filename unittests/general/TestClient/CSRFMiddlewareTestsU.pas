unit CSRFMiddlewareTestsU;

// End-to-end tests of TMVCCSRFMiddleware: an embedded server, real requests.
// The "session id" the token is bound to is read from the X-Session header.
// TCSRFFilterTests runs the same tests against the CSRF endpoint filter.

interface

{$WARN SYMBOL_DEPRECATED OFF} // the middleware is tested on purpose

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.Net.HttpClient,
  System.Net.URLClient,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Server.Intf;

const
  CSRF_PORT = 8898;
  CSRF_SECRET = 'a-csrf-secret-long-enough-for-the-tests';

type
  [MVCPath('/api')]
  TCSRFTestController = class(TMVCController)
  public
    // every verb the tests send, and a route for /hook-evil: the CSRF endpoint filter checks only
    // the requests that have a route (one without gets the 404 of the router)
    [MVCPath('/thing')]
    [MVCHTTPMethod([httpGET, httpPOST, httpPUT, httpPATCH, httpDELETE, httpQUERY])]
    function Thing: String;
    [MVCPath('/hook-evil')]
    [MVCHTTPMethod([httpPOST])]
    function HookEvil: String;
    [MVCPath('/hook')]
    [MVCHTTPMethod([httpPOST])]
    function HookRoot: String;
    [MVCPath('/hook/sub')]
    [MVCHTTPMethod([httpPOST])]
    function HookSub: String;
    [MVCPath('/hook/ping')]
    [MVCHTTPMethod([httpPOST])]
    function Hook: String;
  end;

  [TestFixture]
  TCSRFMiddlewareTests = class
  private
    FServer: IMVCServer;
    FServerNoHosts: IMVCServer;
    fEngine: TMVCEngine;
    fEngineNoHosts: TMVCEngine;
    FClient: THTTPClient;
    function SendRequest(const AMethod, APath, ASession, AToken, AOrigin: string;
      const AForm: string = ''; APort: Integer = 0): IHTTPResponse;
    function Send(const AMethod, APath, ASession, AToken, AOrigin: string;
      const AForm: string = ''; APort: Integer = 0): Integer;
  protected
    fPort: Integer;
    function BasePort: Integer; virtual;
    // the check under test, on AEngine; with the allowed hosts of the tests or without
    procedure AddCSRF(const AEngine: TMVCEngine; const AWithAllowedHosts: Boolean); virtual;
    // builds the check (and throws it away): for the tests on the arguments
    procedure CreateCSRF(const ASecret: string; const AExemptPaths: TArray<string>;
      const AAllowedHosts: TArray<string>); virtual;
  public
    [SetupFixture]
    procedure SetupFixture;
    [TeardownFixture]
    procedure TeardownFixture;

    [Test]
    procedure Post_WithoutToken_Returns403;
    [Test]
    procedure Post_WithToken_PassesThrough;
    [Test]
    procedure Post_WithTokenInFormField_PassesThrough;
    [Test]
    procedure Post_WithTokenOfOtherSession_Returns403;
    [Test]
    procedure Delete_WithoutToken_Returns403;
    [Test]
    procedure Query_WithoutToken_Returns403;
    [Test]
    procedure Get_NeverRequiresToken;
    [Test]
    procedure ExemptPath_PassesWithoutToken;
    [Test]
    procedure ExemptPath_Exact_Passes;
    [Test]
    procedure ExemptPath_SubPath_Passes;
    [Test]
    procedure ExemptPath_PrefixWithoutBoundary_Returns403;
    [Test]
    procedure ExemptPath_OtherCase_Returns403;
    [Test]
    procedure Put_WithoutToken_Returns403;
    [Test]
    procedure Patch_WithoutToken_Returns403;
    [Test]
    procedure ShortSecret_Raises;
    [Test]
    procedure InvalidExemptPaths_Raise;
    [Test]
    procedure Origin_Mismatch_Returns403;
    [Test]
    procedure Origin_SameHost_PassesThrough;
    [Test]
    procedure Post_WithoutSession_Returns403;
    [Test]
    procedure AllowedHost_Origin_PassesThrough;
    [Test]
    procedure AllowedHost_CaseInsensitive_PassesThrough;
    [Test]
    procedure AllowedHost_WithPort_PassesThrough;
    [Test]
    procedure AllowedHost_NotListed_Returns403;
    [Test]
    procedure AllowedHost_PortMismatch_Returns403;
    [Test]
    procedure AllowedHost_ValidOriginStillNeedsToken;
    [Test]
    procedure NoAllowedHosts_ListedElsewhereHost_Returns403;
    [Test]
    procedure NoAllowedHosts_SameHost_PassesThrough;
    [Test]
    procedure InvalidAllowedHosts_Raise;
    [Test]
    procedure Reject_IsPlainTextAndTheActionDoesNotRun;
  end;

  // the same tests, CSRF endpoint filter (MVCFramework.Filters) instead of the middleware
  [TestFixture]
  TCSRFFilterTests = class(TCSRFMiddlewareTests)
  protected
    function BasePort: Integer; override;
    procedure AddCSRF(const AEngine: TMVCEngine; const AWithAllowedHosts: Boolean); override;
    procedure CreateCSRF(const ASecret: string; const AExemptPaths: TArray<string>;
      const AAllowedHosts: TArray<string>); override;
  end;

  // the same tests, CSRFHTTPFilter (HTTP filter, before routing); it also covers an HTTP filter registered after it
  [TestFixture]
  TCSRFHTTPFilterTests = class(TCSRFMiddlewareTests)
  protected
    function BasePort: Integer; override;
    procedure AddCSRF(const AEngine: TMVCEngine; const AWithAllowedHosts: Boolean); override;
    procedure CreateCSRF(const ASecret: string; const AExemptPaths: TArray<string>;
      const AAllowedHosts: TArray<string>); override;
  public
    [Test]
    procedure APostAnsweredByALaterHTTPFilterIsChecked;
  end;

implementation

uses
  System.NetEncoding,
  System.SyncObjs,
  MVCFramework.Server.Factory,
  MVCFramework.Filters,
  MVCFramework.MinimalAPI,
  MVCFramework.Middleware.CSRF;

var
  GThingRuns: Integer = 0;

function SessionFromHeader(AContext: TWebContext): string;
begin
  Result := AContext.Request.Headers['X-Session'];
end;

{ TCSRFTestController }

function TCSRFTestController.Thing: String;
begin
  TInterlocked.Increment(GThingRuns);
  Result := 'ok';
end;

function TCSRFTestController.HookEvil: String;
begin
  Result := 'evil';
end;

function TCSRFTestController.HookRoot: String;
begin
  Result := 'hook';
end;

function TCSRFTestController.HookSub: String;
begin
  Result := 'hook';
end;

function TCSRFTestController.Hook: String;
begin
  Result := 'hook';
end;

{ TCSRFMiddlewareTests }

function TCSRFMiddlewareTests.BasePort: Integer;
begin
  Result := CSRF_PORT;
end;

procedure TCSRFMiddlewareTests.AddCSRF(const AEngine: TMVCEngine; const AWithAllowedHosts: Boolean);
begin
  if AWithAllowedHosts then
    AEngine.AddMiddleware(TMVCCSRFMiddleware.Create(CSRF_SECRET, SessionFromHeader,
      'X-CSRF-Token', 'csrf_token', ['/api/hook'],
      ['proxy.example.test', 'Other.Example:8443']))
  else
    AEngine.AddMiddleware(TMVCCSRFMiddleware.Create(CSRF_SECRET, SessionFromHeader));
end;

procedure TCSRFMiddlewareTests.CreateCSRF(const ASecret: string; const AExemptPaths,
  AAllowedHosts: TArray<string>);
begin
  TMVCCSRFMiddleware.Create(ASecret, SessionFromHeader, 'X-CSRF-Token', 'csrf_token',
    AExemptPaths, AAllowedHosts).Free;
end;

procedure TCSRFMiddlewareTests.SetupFixture;
var
  lEngine: TMVCEngine;
begin
  fPort := BasePort;
  lEngine := TMVCEngine.Create;
  fEngine := lEngine;
  lEngine.AddController(TCSRFTestController);
  AddCSRF(lEngine, True);
  // after the check: the Minimal API dispatcher comes after the middleware added before the first route
  lEngine.Root.MapPost('/api/($lang)/account/delete',
    function: IMVCResponse
    begin
      Result := Ok('deleted');
    end);
  FServer := TMVCServerFactory.CreateIndyDirect(lEngine);
  FServer.Listen(fPort);
  // same engine setup, no AAllowedHosts: current behaviour
  lEngine := TMVCEngine.Create;
  fEngineNoHosts := lEngine;
  lEngine.AddController(TCSRFTestController);
  AddCSRF(lEngine, False);
  FServerNoHosts := TMVCServerFactory.CreateIndyDirect(lEngine);
  FServerNoHosts.Listen(fPort + 1);
  FClient := THTTPClient.Create;
end;

procedure TCSRFMiddlewareTests.TeardownFixture;
begin
  FClient.Free;
  FServer.Stop;
  FServer := nil;
  FServerNoHosts.Stop;
  FServerNoHosts := nil;
  // the servers do not own the engines
  FreeAndNil(fEngine);
  FreeAndNil(fEngineNoHosts);
end;

function TCSRFMiddlewareTests.Send(const AMethod, APath, ASession, AToken, AOrigin,
  AForm: string; APort: Integer): Integer;
begin
  Result := SendRequest(AMethod, APath, ASession, AToken, AOrigin, AForm, APort).StatusCode;
end;

function TCSRFMiddlewareTests.SendRequest(const AMethod, APath, ASession, AToken, AOrigin,
  AForm: string; APort: Integer): IHTTPResponse;
var
  lReq: IHTTPRequest;
  lBody: TStringStream;
begin
  if APort = 0 then
    APort := fPort;
  lReq := FClient.GetRequest(AMethod, 'http://127.0.0.1:' + APort.ToString + APath);
  if ASession <> '' then
    lReq.AddHeader('X-Session', ASession);
  if (AToken <> '') and (AForm = '') then
    lReq.AddHeader('X-CSRF-Token', AToken);
  if AOrigin <> '' then
    lReq.AddHeader('Origin', AOrigin);
  lBody := TStringStream.Create(AForm);
  try
    if AForm <> '' then
      lReq.AddHeader('Content-Type', 'application/x-www-form-urlencoded');
    lReq.SourceStream := lBody;
    Result := FClient.Execute(lReq);
  finally
    lBody.Free;
  end;
end;

procedure TCSRFMiddlewareTests.Post_WithoutToken_Returns403;
begin
  Assert.AreEqual(403, Send('POST', '/api/thing', 'sess-1', '', ''));
end;

procedure TCSRFMiddlewareTests.Post_WithToken_PassesThrough;
begin
  Assert.AreEqual(200, Send('POST', '/api/thing', 'sess-1',
    CSRFToken(CSRF_SECRET, 'sess-1'), ''));
end;

procedure TCSRFMiddlewareTests.Post_WithTokenInFormField_PassesThrough;
begin
  Assert.AreEqual(200, Send('POST', '/api/thing', 'sess-1',
    CSRFToken(CSRF_SECRET, 'sess-1'), '',
    'csrf_token=' + CSRFToken(CSRF_SECRET, 'sess-1') + '&x=1'));
end;

procedure TCSRFMiddlewareTests.Post_WithTokenOfOtherSession_Returns403;
begin
  Assert.AreEqual(403, Send('POST', '/api/thing', 'sess-1',
    CSRFToken(CSRF_SECRET, 'sess-2'), ''));
end;

procedure TCSRFMiddlewareTests.Delete_WithoutToken_Returns403;
begin
  Assert.AreEqual(403, Send('DELETE', '/api/thing', 'sess-1', '', ''));
end;

procedure TCSRFMiddlewareTests.Query_WithoutToken_Returns403;
begin
  // QUERY carries a body like POST: it is not a safe method for CSRF
  Assert.AreEqual(403, Send('QUERY', '/api/thing', 'sess-1', '', ''));
end;

procedure TCSRFMiddlewareTests.Get_NeverRequiresToken;
begin
  Assert.AreEqual(200, Send('GET', '/api/thing', 'sess-1', '', ''));
end;

procedure TCSRFMiddlewareTests.ExemptPath_PassesWithoutToken;
begin
  Assert.AreEqual(200, Send('POST', '/api/hook/ping', 'sess-1', '', ''));
end;

procedure TCSRFMiddlewareTests.ExemptPath_Exact_Passes;
begin
  Assert.AreEqual(200, Send('POST', '/api/hook', 'sess-1', '', ''));
end;

procedure TCSRFMiddlewareTests.ExemptPath_SubPath_Passes;
begin
  Assert.AreEqual(200, Send('POST', '/api/hook/sub', 'sess-1', '', ''));
end;

procedure TCSRFMiddlewareTests.ExemptPath_PrefixWithoutBoundary_Returns403;
begin
  Assert.AreEqual(403, Send('POST', '/api/hook-evil', 'sess-1', '', ''));
end;

procedure TCSRFMiddlewareTests.ExemptPath_OtherCase_Returns403;
begin
  Assert.AreEqual(403, Send('POST', '/API/HOOK', 'sess-1', '', ''));
end;

procedure TCSRFMiddlewareTests.Put_WithoutToken_Returns403;
begin
  Assert.AreEqual(403, Send('PUT', '/api/thing', 'sess-1', '', ''));
end;

procedure TCSRFMiddlewareTests.Patch_WithoutToken_Returns403;
begin
  Assert.AreEqual(403, Send('PATCH', '/api/thing', 'sess-1', '', ''));
end;

procedure TCSRFMiddlewareTests.ShortSecret_Raises;
begin
  Assert.WillRaise(
    procedure
    begin
      CreateCSRF('short', [], []);
    end, EMVCException);
end;

procedure TCSRFMiddlewareTests.InvalidExemptPaths_Raise;
var
  lBad: string;
begin
  for lBad in ['', '   ', '/', 'api'] do
    Assert.WillRaise(
      procedure
      begin
        CreateCSRF(CSRF_SECRET, [lBad], []);
      end, EMVCException, 'exempt entry [' + lBad + ']');
end;

procedure TCSRFMiddlewareTests.Origin_Mismatch_Returns403;
begin
  Assert.AreEqual(403, Send('POST', '/api/thing', 'sess-1',
    CSRFToken(CSRF_SECRET, 'sess-1'), 'https://evil.example'));
end;

procedure TCSRFMiddlewareTests.Origin_SameHost_PassesThrough;
begin
  Assert.AreEqual(200, Send('POST', '/api/thing', 'sess-1',
    CSRFToken(CSRF_SECRET, 'sess-1'),
    'http://127.0.0.1:' + fPort.ToString));
end;

procedure TCSRFMiddlewareTests.Post_WithoutSession_Returns403;
begin
  // no session: a token derived from '' must not be accepted
  Assert.AreEqual(403, Send('POST', '/api/thing', '',
    CSRFToken(CSRF_SECRET, ''), ''));
end;

procedure TCSRFMiddlewareTests.AllowedHost_Origin_PassesThrough;
begin
  Assert.AreEqual(200, Send('POST', '/api/thing', 'sess-1',
    CSRFToken(CSRF_SECRET, 'sess-1'), 'https://proxy.example.test'));
end;

procedure TCSRFMiddlewareTests.AllowedHost_CaseInsensitive_PassesThrough;
begin
  Assert.AreEqual(200, Send('POST', '/api/thing', 'sess-1',
    CSRFToken(CSRF_SECRET, 'sess-1'), 'https://PROXY.Example.TEST'));
end;

procedure TCSRFMiddlewareTests.AllowedHost_WithPort_PassesThrough;
begin
  Assert.AreEqual(200, Send('POST', '/api/thing', 'sess-1',
    CSRFToken(CSRF_SECRET, 'sess-1'), 'https://other.example:8443'));
end;

procedure TCSRFMiddlewareTests.AllowedHost_NotListed_Returns403;
begin
  Assert.AreEqual(403, Send('POST', '/api/thing', 'sess-1',
    CSRFToken(CSRF_SECRET, 'sess-1'), 'https://evil.example.test'));
  // a suffix/prefix of a listed host is not the host
  Assert.AreEqual(403, Send('POST', '/api/thing', 'sess-1',
    CSRFToken(CSRF_SECRET, 'sess-1'), 'https://x.proxy.example.test'));
end;

procedure TCSRFMiddlewareTests.AllowedHost_PortMismatch_Returns403;
begin
  Assert.AreEqual(403, Send('POST', '/api/thing', 'sess-1',
    CSRFToken(CSRF_SECRET, 'sess-1'), 'https://other.example:9999'));
  Assert.AreEqual(403, Send('POST', '/api/thing', 'sess-1',
    CSRFToken(CSRF_SECRET, 'sess-1'), 'https://other.example'));
end;

procedure TCSRFMiddlewareTests.AllowedHost_ValidOriginStillNeedsToken;
begin
  Assert.AreEqual(403, Send('POST', '/api/thing', 'sess-1', '', 'https://proxy.example.test'));
end;

procedure TCSRFMiddlewareTests.NoAllowedHosts_ListedElsewhereHost_Returns403;
begin
  Assert.AreEqual(403, Send('POST', '/api/thing', 'sess-1',
    CSRFToken(CSRF_SECRET, 'sess-1'), 'https://proxy.example.test', '',
    fPort + 1));
end;

procedure TCSRFMiddlewareTests.NoAllowedHosts_SameHost_PassesThrough;
begin
  Assert.AreEqual(200, Send('POST', '/api/thing', 'sess-1',
    CSRFToken(CSRF_SECRET, 'sess-1'),
    'http://127.0.0.1:' + (fPort + 1).ToString, '', fPort + 1));
end;

procedure TCSRFMiddlewareTests.InvalidAllowedHosts_Raise;
var
  lBad: string;
begin
  for lBad in ['', '   ', 'a b.test', 'https://proxy.test', 'proxy.test/x', '*.proxy.test', '*',
    'proxy.test:', 'proxy.test:0', 'proxy.test:99999', 'proxy.test:8a', 'u@proxy.test', 'a:1:2'] do
    Assert.WillRaise(
      procedure
      begin
        CreateCSRF(CSRF_SECRET, [], [lBad]);
      end, EMVCException, 'allowed host [' + lBad + ']');
end;

procedure TCSRFMiddlewareTests.Reject_IsPlainTextAndTheActionDoesNotRun;
var
  lRes: IHTTPResponse;
  lRuns: Integer;
begin
  lRuns := TInterlocked.CompareExchange(GThingRuns, 0, 0);
  lRes := SendRequest('POST', '/api/thing', 'sess-1', '', '');
  Assert.AreEqual(403, lRes.StatusCode);
  Assert.StartsWith('text/plain', lRes.MimeType, 'Content-Type: ' + lRes.MimeType);
  Assert.AreEqual('CSRF check failed: token', lRes.ContentAsString);
  Assert.AreEqual(lRuns, TInterlocked.CompareExchange(GThingRuns, 0, 0), 'the action did not run');
end;

{ TCSRFFilterTests }

function TCSRFFilterTests.BasePort: Integer;
begin
  Result := CSRF_PORT + 62; // 8960 and 8961
end;

procedure TCSRFFilterTests.AddCSRF(const AEngine: TMVCEngine; const AWithAllowedHosts: Boolean);
begin
  if AWithAllowedHosts then
    AEngine.Use(CSRFEndpointFilter(CSRF_SECRET, SessionFromHeader,
      'X-CSRF-Token', 'csrf_token', ['/api/hook'],
      ['proxy.example.test', 'Other.Example:8443']))
  else
    AEngine.Use(CSRFEndpointFilter(CSRF_SECRET, SessionFromHeader));
end;

procedure TCSRFFilterTests.CreateCSRF(const ASecret: string; const AExemptPaths,
  AAllowedHosts: TArray<string>);
begin
  CSRFEndpointFilter(ASecret, SessionFromHeader, 'X-CSRF-Token', 'csrf_token', AExemptPaths, AAllowedHosts);
end;

{ TCSRFHTTPFilterTests }

function TCSRFHTTPFilterTests.BasePort: Integer;
begin
  Result := CSRF_PORT + 65; // 8963 and 8964
end;

procedure TCSRFHTTPFilterTests.AddCSRF(const AEngine: TMVCEngine; const AWithAllowedHosts: Boolean);
begin
  if AWithAllowedHosts then
    AEngine.Use(CSRFHTTPFilter(CSRF_SECRET, SessionFromHeader,
      'X-CSRF-Token', 'csrf_token', ['/api/hook'],
      ['proxy.example.test', 'Other.Example:8443']))
  else
    AEngine.Use(CSRFHTTPFilter(CSRF_SECRET, SessionFromHeader));
  // an HTTP filter that answers by itself, as a login filter does
  AEngine.Use(
    procedure(const AContext: TWebContext; const ANext: TMVCHTTPFilterNext)
    begin
      if AContext.Request.PathInfo = '/prelogin' then
      begin
        AContext.Response.StatusCode := HTTP_STATUS.OK;
        AContext.Response.Content := 'logged';
      end
      else
        ANext();
    end);
end;

procedure TCSRFHTTPFilterTests.CreateCSRF(const ASecret: string; const AExemptPaths,
  AAllowedHosts: TArray<string>);
begin
  CSRFHTTPFilter(ASecret, SessionFromHeader, 'X-CSRF-Token', 'csrf_token', AExemptPaths, AAllowedHosts);
end;

procedure TCSRFHTTPFilterTests.APostAnsweredByALaterHTTPFilterIsChecked;
begin
  Assert.AreEqual(403, Send('POST', '/prelogin', 'sess-1', '', ''), 'no token');
  Assert.AreEqual(200, Send('POST', '/prelogin', 'sess-1', CSRFToken(CSRF_SECRET, 'sess-1'), ''), 'token');
end;

initialization

TDUnitX.RegisterTestFixture(TCSRFMiddlewareTests);
TDUnitX.RegisterTestFixture(TCSRFFilterTests);
TDUnitX.RegisterTestFixture(TCSRFHTTPFilterTests);

end.
