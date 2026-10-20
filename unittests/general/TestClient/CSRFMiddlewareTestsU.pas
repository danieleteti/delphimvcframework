unit CSRFMiddlewareTestsU;

// End-to-end tests of TMVCCSRFMiddleware: an embedded server, real requests.
// The "session id" the token is bound to is read from the X-Session header.

interface

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
    [MVCPath('/thing')]
    [MVCHTTPMethod([httpGET, httpPOST, httpDELETE])]
    function Thing: String;
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
    FClient: THTTPClient;
    function Send(const AMethod, APath, ASession, AToken, AOrigin: string;
      const AForm: string = ''; APort: Integer = 0): Integer;
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
  end;

implementation

uses
  System.NetEncoding,
  MVCFramework.Server.Factory,
  MVCFramework.Middleware.CSRF;

{ TCSRFTestController }

function TCSRFTestController.Thing: String;
begin
  Result := 'ok';
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

procedure TCSRFMiddlewareTests.SetupFixture;
var
  lEngine: TMVCEngine;
begin
  lEngine := TMVCEngine.Create;
  lEngine.AddController(TCSRFTestController);
  lEngine.AddMiddleware(TMVCCSRFMiddleware.Create(CSRF_SECRET,
    function(AContext: TWebContext): string
    begin
      Result := AContext.Request.Headers['X-Session'];
    end,
    'X-CSRF-Token', 'csrf_token', ['/api/hook'],
    ['proxy.example.test', 'Other.Example:8443']));
  FServer := TMVCServerFactory.CreateIndyDirect(lEngine);
  FServer.Listen(CSRF_PORT);
  // same engine setup, no AAllowedHosts: current behaviour
  lEngine := TMVCEngine.Create;
  lEngine.AddController(TCSRFTestController);
  lEngine.AddMiddleware(TMVCCSRFMiddleware.Create(CSRF_SECRET,
    function(AContext: TWebContext): string
    begin
      Result := AContext.Request.Headers['X-Session'];
    end));
  FServerNoHosts := TMVCServerFactory.CreateIndyDirect(lEngine);
  FServerNoHosts.Listen(CSRF_PORT + 1);
  FClient := THTTPClient.Create;
end;

procedure TCSRFMiddlewareTests.TeardownFixture;
begin
  FClient.Free;
  FServer.Stop;
  FServer := nil;
  FServerNoHosts.Stop;
  FServerNoHosts := nil;
end;

function TCSRFMiddlewareTests.Send(const AMethod, APath, ASession, AToken, AOrigin,
  AForm: string; APort: Integer): Integer;
var
  lReq: IHTTPRequest;
  lBody: TStringStream;
begin
  if APort = 0 then
    APort := CSRF_PORT;
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
    Result := FClient.Execute(lReq).StatusCode;
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
    TMVCCSRFMiddleware.TokenFor(CSRF_SECRET, 'sess-1'), ''));
end;

procedure TCSRFMiddlewareTests.Post_WithTokenInFormField_PassesThrough;
begin
  Assert.AreEqual(200, Send('POST', '/api/thing', 'sess-1',
    TMVCCSRFMiddleware.TokenFor(CSRF_SECRET, 'sess-1'), '',
    'csrf_token=' + TMVCCSRFMiddleware.TokenFor(CSRF_SECRET, 'sess-1') + '&x=1'));
end;

procedure TCSRFMiddlewareTests.Post_WithTokenOfOtherSession_Returns403;
begin
  Assert.AreEqual(403, Send('POST', '/api/thing', 'sess-1',
    TMVCCSRFMiddleware.TokenFor(CSRF_SECRET, 'sess-2'), ''));
end;

procedure TCSRFMiddlewareTests.Delete_WithoutToken_Returns403;
begin
  Assert.AreEqual(403, Send('DELETE', '/api/thing', 'sess-1', '', ''));
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
      TMVCCSRFMiddleware.Create('short', function(C: TWebContext): string begin Result := 'x'; end);
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
        TMVCCSRFMiddleware.Create(CSRF_SECRET, function(C: TWebContext): string begin Result := 'x'; end,
          'X-CSRF-Token', 'csrf_token', [lBad]);
      end, EMVCException, 'exempt entry [' + lBad + ']');
end;

procedure TCSRFMiddlewareTests.Origin_Mismatch_Returns403;
begin
  Assert.AreEqual(403, Send('POST', '/api/thing', 'sess-1',
    TMVCCSRFMiddleware.TokenFor(CSRF_SECRET, 'sess-1'), 'https://evil.example'));
end;

procedure TCSRFMiddlewareTests.Origin_SameHost_PassesThrough;
begin
  Assert.AreEqual(200, Send('POST', '/api/thing', 'sess-1',
    TMVCCSRFMiddleware.TokenFor(CSRF_SECRET, 'sess-1'),
    'http://127.0.0.1:' + CSRF_PORT.ToString));
end;

procedure TCSRFMiddlewareTests.Post_WithoutSession_Returns403;
begin
  // no session: a token derived from '' must not be accepted
  Assert.AreEqual(403, Send('POST', '/api/thing', '',
    TMVCCSRFMiddleware.TokenFor(CSRF_SECRET, ''), ''));
end;

procedure TCSRFMiddlewareTests.AllowedHost_Origin_PassesThrough;
begin
  Assert.AreEqual(200, Send('POST', '/api/thing', 'sess-1',
    TMVCCSRFMiddleware.TokenFor(CSRF_SECRET, 'sess-1'), 'https://proxy.example.test'));
end;

procedure TCSRFMiddlewareTests.AllowedHost_CaseInsensitive_PassesThrough;
begin
  Assert.AreEqual(200, Send('POST', '/api/thing', 'sess-1',
    TMVCCSRFMiddleware.TokenFor(CSRF_SECRET, 'sess-1'), 'https://PROXY.Example.TEST'));
end;

procedure TCSRFMiddlewareTests.AllowedHost_WithPort_PassesThrough;
begin
  Assert.AreEqual(200, Send('POST', '/api/thing', 'sess-1',
    TMVCCSRFMiddleware.TokenFor(CSRF_SECRET, 'sess-1'), 'https://other.example:8443'));
end;

procedure TCSRFMiddlewareTests.AllowedHost_NotListed_Returns403;
begin
  Assert.AreEqual(403, Send('POST', '/api/thing', 'sess-1',
    TMVCCSRFMiddleware.TokenFor(CSRF_SECRET, 'sess-1'), 'https://evil.example.test'));
  // a suffix/prefix of a listed host is not the host
  Assert.AreEqual(403, Send('POST', '/api/thing', 'sess-1',
    TMVCCSRFMiddleware.TokenFor(CSRF_SECRET, 'sess-1'), 'https://x.proxy.example.test'));
end;

procedure TCSRFMiddlewareTests.AllowedHost_PortMismatch_Returns403;
begin
  Assert.AreEqual(403, Send('POST', '/api/thing', 'sess-1',
    TMVCCSRFMiddleware.TokenFor(CSRF_SECRET, 'sess-1'), 'https://other.example:9999'));
  Assert.AreEqual(403, Send('POST', '/api/thing', 'sess-1',
    TMVCCSRFMiddleware.TokenFor(CSRF_SECRET, 'sess-1'), 'https://other.example'));
end;

procedure TCSRFMiddlewareTests.AllowedHost_ValidOriginStillNeedsToken;
begin
  Assert.AreEqual(403, Send('POST', '/api/thing', 'sess-1', '', 'https://proxy.example.test'));
end;

procedure TCSRFMiddlewareTests.NoAllowedHosts_ListedElsewhereHost_Returns403;
begin
  Assert.AreEqual(403, Send('POST', '/api/thing', 'sess-1',
    TMVCCSRFMiddleware.TokenFor(CSRF_SECRET, 'sess-1'), 'https://proxy.example.test', '',
    CSRF_PORT + 1));
end;

procedure TCSRFMiddlewareTests.NoAllowedHosts_SameHost_PassesThrough;
begin
  Assert.AreEqual(200, Send('POST', '/api/thing', 'sess-1',
    TMVCCSRFMiddleware.TokenFor(CSRF_SECRET, 'sess-1'),
    'http://127.0.0.1:' + (CSRF_PORT + 1).ToString, '', CSRF_PORT + 1));
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
        TMVCCSRFMiddleware.Create(CSRF_SECRET, function(C: TWebContext): string begin Result := 'x'; end,
          'X-CSRF-Token', 'csrf_token', [], [lBad]);
      end, EMVCException, 'allowed host [' + lBad + ']');
end;

initialization

TDUnitX.RegisterTestFixture(TCSRFMiddlewareTests);

end.
