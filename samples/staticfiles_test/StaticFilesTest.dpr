program StaticFilesTest;

// Empirical verification for the minimal-API HTTPFilter helpers
// (StaticFilesHTTPFilter, CompressionHTTPFilter, ETagHTTPFilter, IPBlockHTTPFilter,
// RateLimitHTTPFilter, RequestLogHTTPFilter, CORSHTTPFilter). Each scenario spins a fresh engine on its own port so
// filter state cannot bleed across scenarios.
//
// Exit code: 0 = all pass, 1 = at least one failure.

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  System.SyncObjs,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.MinimalAPI,
  MVCFramework.Filters,
  MVCFramework.Filters.ActiveRecord,
  MVCFramework.Server.Intf,
  MVCFramework.Server.Factory,
  MVCFramework.RESTClient,
  MVCFramework.RESTClient.Intf,
  MVCFramework.Middleware.RateLimit,
  MVCFramework.ActiveRecord,
  MVCFramework.Logger,
  FireDAC.Comp.Client,
  FireDAC.ConsoleUI.Wait,
  FireDAC.Phys.SQLite,
{$IF CompilerVersion >= 34} // 10.4+: SQLite linked into the executable, no sqlite3.dll
  FireDAC.Phys.SQLiteWrapper.Stat,
{$ENDIF}
  FireDAC.Stan.Intf;

const
  PORT_BASE             = 8765;
  WWW_DIR               = 'www';
  TEST_HTML             = '<h1>StaticFiles works</h1>';
  INDEX_HTML            = '<h1>Default index</h1>';
  JSON_BODY             = '{"k":"v"}';
  SUB_INDEX_HTML        = '<h1>Sub folder index</h1>';
  MEDIA_SIZE            = 1000;
  COMPRESSION_THRESHOLD = 256;

var
  GFailures: Integer = 0;

procedure Pass(const AName: string);
begin
  WriteLn('[PASS] ', AName);
end;

procedure Fail(const AName, ADetail: string);
begin
  WriteLn('[FAIL] ', AName, ' -- ', ADetail);
  Inc(GFailures);
end;

procedure AssertStatusBody(const AResp: IMVCRESTResponse;
  AExpectedStatus: Integer; const AExpectedBody, AName: string);
begin
  if (AResp.StatusCode = AExpectedStatus) and (AResp.Content = AExpectedBody) then
    Pass(AName)
  else
    Fail(AName, Format('status=%d body=[%s]', [AResp.StatusCode, AResp.Content]));
end;

procedure AssertStatus(const AResp: IMVCRESTResponse;
  AExpectedStatus: Integer; const AName: string);
begin
  if AResp.StatusCode = AExpectedStatus then
    Pass(AName)
  else
    Fail(AName, Format('expected status=%d got=%d body=[%s]',
      [AExpectedStatus, AResp.StatusCode, AResp.Content]));
end;

// Repeating ASCII compresses well — easy to tell from the original bytes.
function MakeBigContent: string;
var
  i: Integer;
  lSB: TStringBuilder;
begin
  lSB := TStringBuilder.Create;
  try
    for i := 1 to 200 do
      lSB.Append('Lorem ipsum dolor sit amet, consectetur adipiscing elit. ');
    Result := lSB.ToString;
  finally
    lSB.Free;
  end;
end;

procedure SetupFixtures;
var
  lDir: string;
begin
  lDir := TPath.Combine(GetCurrentDir, WWW_DIR);
  if not TDirectory.Exists(lDir) then
    TDirectory.CreateDirectory(lDir);
  TFile.WriteAllText(TPath.Combine(lDir, 'test.html'), TEST_HTML);
  TFile.WriteAllText(TPath.Combine(lDir, 'index.html'), INDEX_HTML);
  TFile.WriteAllText(TPath.Combine(lDir, 'data.json'), JSON_BODY);
  TFile.WriteAllText(TPath.Combine(lDir, 'big.html'), MakeBigContent);
  TDirectory.CreateDirectory(TPath.Combine(lDir, 'sub'));
  TFile.WriteAllText(TPath.Combine(TPath.Combine(lDir, 'sub'), 'index.html'), SUB_INDEX_HTML);
  TDirectory.CreateDirectory(TPath.Combine(lDir, 'media'));
  TFile.WriteAllText(TPath.Combine(TPath.Combine(lDir, 'media'), 'clip.txt'), StringOfChar('x', MEDIA_SIZE));
end;

// Runs an isolated scenario: fresh engine + Indy server on APort, runs
// AConfigure to register filters/routes, then ARunTests with an
// IMVCRESTClient pointing at the server. Tears everything down.
procedure WithEngine(APort: Integer;
  const AConfigure: TProc<TMVCEngine>;
  const ARunTests: TProc<IMVCRESTClient>);
var
  lEngine: TMVCEngine;
  lServer: IMVCServer;
  lClient: IMVCRESTClient;
begin
  lEngine := TMVCEngine.Create(
    procedure (AConfig: TMVCConfig)
    begin
      AConfig[TMVCConfigKey.DefaultContentType] := TMVCMediaType.TEXT_HTML;
      AConfig[TMVCConfigKey.AllowUnhandledAction] := 'false';
      AConfig[TMVCConfigKey.LoadSystemControllers] := 'false';
      AConfig[TMVCConfigKey.ExposeServerSignature] := 'false';
    end);
  try
    AConfigure(lEngine);
    lServer := TMVCServerFactory.CreateIndyDirect(lEngine);
    lServer.Listen(APort);
    try
      Sleep(200); // let Indy bind
      lClient := TMVCRESTClient.New.BaseURL('http://localhost:' + APort.ToString);
      ARunTests(lClient);
    finally
      lServer.Stop;
      lServer := nil;
    end;
  finally
    lEngine.Free;
  end;
end;

// -- scenario 1: StaticFiles + Compression + ETag ---------------------------

procedure ConfigStatic(AEngine: TMVCEngine);
begin
  AEngine.Use(ETagHTTPFilter());
  AEngine.Use(CompressionHTTPFilter(COMPRESSION_THRESHOLD));
  AEngine.Use(StaticFilesHTTPFilter('/static', WWW_DIR));
  AEngine.Root.AsWeb.MapGet('/hello',
    function: IMVCResponse
    begin
      Result := Ok('hi');
    end);
end;

procedure TestStatic(AClient: IMVCRESTClient);
var
  lResp: IMVCRESTResponse;
  lEtag: string;
begin
  lResp := AClient.Get('/static/test.html');
  AssertStatusBody(lResp, 200, TEST_HTML, 'GET /static/test.html');

  lResp := AClient.Get('/static/');
  AssertStatusBody(lResp, 200, INDEX_HTML, 'GET /static/  -> index.html');

  lResp := AClient.Get('/static');
  AssertStatusBody(lResp, 200, INDEX_HTML, 'GET /static   -> index.html');

  lResp := AClient.Get('/static/../etc/passwd');
  AssertStatus(lResp, 404, 'GET /static/../etc/passwd -> 404');

  lResp := AClient.Get('/static/missing.html');
  AssertStatus(lResp, 404, 'GET /static/missing.html -> 404');

  lResp := AClient.Get('/hello');
  AssertStatus(lResp, 200, 'GET /hello -> handler runs');

  lResp := AClient.Get('/static/data.json');
  if (lResp.StatusCode = 200) and lResp.ContentType.ToLower.Contains('application/json') then
    Pass('GET /static/data.json -> 200 application/json')
  else
    Fail('GET /static/data.json',
      Format('status=%d ct=%s', [lResp.StatusCode, lResp.ContentType]));

  // Compression
  lResp := AClient.AcceptEncoding('gzip').Get('/static/big.html');
  if (lResp.StatusCode = 200)
     and SameText(lResp.HeaderValue('Content-Encoding'), 'gzip')
     and (lResp.Content = MakeBigContent) then
    Pass('GET /static/big.html (Accept gzip) -> gzip + body matches')
  else
    Fail('GET /static/big.html (Accept gzip)',
      Format('status=%d ce=%s len=%d',
        [lResp.StatusCode, lResp.HeaderValue('Content-Encoding'), Length(lResp.Content)]));

  AClient := TMVCRESTClient.New.BaseURL(AClient.BaseURL);
  lResp := AClient.AcceptEncoding('deflate').Get('/static/big.html');
  if (lResp.StatusCode = 200)
     and SameText(lResp.HeaderValue('Content-Encoding'), 'deflate')
     and (lResp.Content = MakeBigContent) then
    Pass('GET /static/big.html (Accept deflate) -> deflate + body matches')
  else
    Fail('GET /static/big.html (Accept deflate)',
      Format('status=%d ce=%s len=%d',
        [lResp.StatusCode, lResp.HeaderValue('Content-Encoding'), Length(lResp.Content)]));

  AClient := TMVCRESTClient.New.BaseURL(AClient.BaseURL);
  lResp := AClient.AcceptEncoding('deflate, gzip').Get('/static/big.html');
  if (lResp.StatusCode = 200)
     and SameText(lResp.HeaderValue('Content-Encoding'), 'deflate')
     and (lResp.Content = MakeBigContent) then
    Pass('GET /static/big.html (Accept deflate, gzip) -> the first one listed: deflate')
  else
    Fail('GET /static/big.html (Accept deflate, gzip)',
      Format('status=%d ce=%s', [lResp.StatusCode, lResp.HeaderValue('Content-Encoding')]));

  AClient := TMVCRESTClient.New.BaseURL(AClient.BaseURL);
  lResp := AClient.AcceptEncoding('').Get('/static/big.html');
  if (lResp.StatusCode = 200)
     and (lResp.HeaderValue('Content-Encoding') = '')
     and (lResp.Content = MakeBigContent) then
    Pass('GET /static/big.html (no Accept-Encoding) -> uncompressed')
  else
    Fail('GET /static/big.html (no Accept-Encoding)',
      Format('status=%d ce=%s',
        [lResp.StatusCode, lResp.HeaderValue('Content-Encoding')]));

  AClient := TMVCRESTClient.New.BaseURL(AClient.BaseURL);
  lResp := AClient.AcceptEncoding('gzip').Get('/static/test.html');
  if (lResp.StatusCode = 200)
     and (lResp.HeaderValue('Content-Encoding') = '')
     and (lResp.Content = TEST_HTML) then
    Pass('GET /static/test.html (under threshold) -> uncompressed')
  else
    Fail('GET /static/test.html (under threshold)',
      Format('status=%d ce=%s',
        [lResp.StatusCode, lResp.HeaderValue('Content-Encoding')]));

  // ETag
  AClient := TMVCRESTClient.New.BaseURL(AClient.BaseURL);
  lResp := AClient.Get('/static/test.html');
  lEtag := lResp.HeaderValue('ETag');
  if (lResp.StatusCode = 200)
     and lEtag.StartsWith('"') and lEtag.EndsWith('"')
     and (Length(lEtag) >= 6) then
    Pass('GET /static/test.html -> 200 + quoted ETag')
  else
    Fail('GET /static/test.html (ETag stamp)',
      Format('status=%d etag=%s', [lResp.StatusCode, lEtag]));

  AClient := TMVCRESTClient.New.BaseURL(AClient.BaseURL);
  lResp := AClient.Get('/static/test.html');
  if lResp.HeaderValue('ETag') = lEtag then
    Pass('GET /static/test.html (repeat) -> same ETag')
  else
    Fail('GET /static/test.html (repeat ETag)',
      Format('first=%s second=%s', [lEtag, lResp.HeaderValue('ETag')]));

  AClient := TMVCRESTClient.New.BaseURL(AClient.BaseURL);
  lResp := AClient.AddHeader('If-None-Match', lEtag).Get('/static/test.html');
  if (lResp.StatusCode = 304) and (lResp.Content = '') then
    Pass('GET /static/test.html (If-None-Match match) -> 304 empty body')
  else
    Fail('GET /static/test.html (If-None-Match match)',
      Format('status=%d body=[%s]', [lResp.StatusCode, lResp.Content]));

  AClient := TMVCRESTClient.New.BaseURL(AClient.BaseURL);
  lResp := AClient.AddHeader('If-None-Match', '"nope"').Get('/static/test.html');
  if (lResp.StatusCode = 200) and (lResp.Content = TEST_HTML) then
    Pass('GET /static/test.html (If-None-Match miss) -> 200 + body')
  else
    Fail('GET /static/test.html (If-None-Match miss)',
      Format('status=%d body=[%s]', [lResp.StatusCode, lResp.Content]));

  AClient := TMVCRESTClient.New.BaseURL(AClient.BaseURL);
  lResp := AClient.Get('/static/index.html');
  if (lResp.HeaderValue('ETag') <> '') and (lResp.HeaderValue('ETag') <> lEtag) then
    Pass('GET /static/index.html -> different ETag')
  else
    Fail('GET /static/index.html (ETag distinctness)',
      Format('index_etag=%s test_etag=%s', [lResp.HeaderValue('ETag'), lEtag]));
end;

// -- scenario 2: IPBlock blocks local request ------------------------------

procedure ConfigIPBlockBans(AEngine: TMVCEngine);
begin
  // Indy reports the loopback as the fully-expanded IPv6 form when the
  // listener binds to ::; cover both that and the IPv4 form.
  AEngine.Use(IPBlockHTTPFilter(['127.0.0.1', '::1', '0:0:0:0:0:0:0:1']));
  AEngine.Root.AsApi.MapGet('/ping',
    function: IMVCResponse
    begin
      Result := Ok('pong');
    end);
end;

procedure TestIPBlockBans(AClient: IMVCRESTClient);
begin
  AssertStatus(AClient.Get('/ping'), 403, 'IPBlockHTTPFilter(local) -> 403');
end;

// -- scenario 3: IPBlock with non-matching list lets requests through ------

procedure ConfigIPBlockAllows(AEngine: TMVCEngine);
begin
  AEngine.Use(IPBlockHTTPFilter(['10.99.99.99']));
  AEngine.Root.AsApi.MapGet('/ping',
    function: IMVCResponse
    begin
      Result := Ok('pong');
    end);
end;

procedure TestIPBlockAllows(AClient: IMVCRESTClient);
begin
  AssertStatus(AClient.Get('/ping'), 200, 'IPBlockHTTPFilter(other) -> 200');
end;

// -- scenario 4: RateLimit short-circuits the third request ----------------

procedure ConfigRateLimit(AEngine: TMVCEngine);
begin
  AEngine.Use(RateLimitHTTPFilter(2, 60));
  AEngine.Root.AsApi.MapGet('/ping',
    function: IMVCResponse
    begin
      Result := Ok('pong');
    end);
end;

procedure TestRateLimit(AClient: IMVCRESTClient);
var
  lResp: IMVCRESTResponse;
begin
  AssertStatus(AClient.Get('/ping'), 200, 'RateLimit req#1 -> 200');
  AssertStatus(AClient.Get('/ping'), 200, 'RateLimit req#2 -> 200');
  lResp := AClient.Get('/ping');
  if (lResp.StatusCode = 429) and (lResp.HeaderValue('Retry-After') <> '') then
    Pass('RateLimit req#3 -> 429 + Retry-After')
  else
    Fail('RateLimit req#3',
      Format('status=%d retry-after=%s',
        [lResp.StatusCode, lResp.HeaderValue('Retry-After')]));
end;

// -- scenario 5: CORS preflight + simple-header stamping -------------------

procedure ConfigCORS(AEngine: TMVCEngine);
begin
  AEngine.Use(CORSHTTPFilter('https://example.com', True,
    'X-Total-Count', 'Content-Type,Authorization',
    'GET,POST,DELETE,OPTIONS', 600));
  AEngine.Root.AsApi.MapGet('/ping',
    function: IMVCResponse
    begin
      Result := Ok('pong');
    end);
end;

procedure TestCORS(AClient: IMVCRESTClient);
var
  lResp: IMVCRESTResponse;
begin
  // Preflight: OPTIONS short-circuits with 200 + full CORS headers.
  lResp := AClient.Resource('/anything').Get; // dummy noop to reuse client
  // The filter answers with the allowed origin that matches the Origin header of the request.
  AClient := TMVCRESTClient.New.BaseURL(AClient.BaseURL).AddHeader('Origin', 'https://example.com');
  lResp := AClient.Resource('').Options('/anything');
  if (lResp.StatusCode = 200)
     and (lResp.HeaderValue('Access-Control-Allow-Origin') = 'https://example.com')
     and (lResp.HeaderValue('Access-Control-Allow-Credentials') = 'true')
     and (lResp.HeaderValue('Access-Control-Allow-Methods') <> '')
     and (lResp.HeaderValue('Access-Control-Allow-Headers') <> '')
     and (lResp.HeaderValue('Access-Control-Max-Age') = '600') then
    Pass('CORS preflight OPTIONS -> 200 + full header set')
  else
    Fail('CORS preflight',
      Format('status=%d origin=%s methods=%s',
        [lResp.StatusCode,
         lResp.HeaderValue('Access-Control-Allow-Origin'),
         lResp.HeaderValue('Access-Control-Allow-Methods')]));

  // Non-preflight: GET passes through; simple headers stamped.
  AClient := TMVCRESTClient.New.BaseURL(AClient.BaseURL).AddHeader('Origin', 'https://example.com');
  lResp := AClient.Get('/ping');
  if (lResp.StatusCode = 200)
     and (lResp.HeaderValue('Access-Control-Allow-Origin') = 'https://example.com')
     and (lResp.HeaderValue('Access-Control-Allow-Credentials') = 'true')
     and (lResp.HeaderValue('Access-Control-Expose-Headers') = 'X-Total-Count') then
    Pass('CORS GET /ping -> 200 + simple CORS headers')
  else
    Fail('CORS GET /ping',
      Format('status=%d origin=%s expose=%s',
        [lResp.StatusCode,
         lResp.HeaderValue('Access-Control-Allow-Origin'),
         lResp.HeaderValue('Access-Control-Expose-Headers')]));
end;

// -- scenario 6: RequestLog wraps everything and does not break responses --

procedure ConfigRequestLog(AEngine: TMVCEngine);
begin
  AEngine.Use(RequestLogHTTPFilter());
  AEngine.Root.AsApi.MapGet('/ping',
    function: IMVCResponse
    begin
      Result := Ok('pong');
    end);
end;

procedure TestRequestLog(AClient: IMVCRESTClient);
begin
  // The point of this test is just that RequestLog does not corrupt the
  // response. The log emission itself is visible in the console output.
  AssertStatus(AClient.Get('/ping'), 200, 'RequestLog passthrough -> 200');
end;

// -- scenario 7: Trace -- pre + post-Next debug logging --------------------

procedure ConfigTrace(AEngine: TMVCEngine);
begin
  AEngine.Use(TraceHTTPFilter(2048, 'trace_test'));
  AEngine.Root.AsApi.MapGet('/echo',
    function: IMVCResponse
    begin
      Result := Ok('echoed');
    end);
end;

procedure TestTrace(AClient: IMVCRESTClient);
var
  lResp: IMVCRESTResponse;
begin
  // Smoke: response not corrupted by Trace's pre/post hooks.
  lResp := AClient.Get('/echo');
  AssertStatus(lResp, 200, 'Trace passthrough GET -> 200');

  // Correlation id: the one of the caller is honored and echoed, otherwise one is generated
  lResp := TMVCRESTClient.New.BaseURL(AClient.BaseURL).AddHeader('X-Request-ID', 'abc-123').Get('/echo');
  if lResp.HeaderValue('X-Request-ID') = 'abc-123' then
    Pass('Trace echoes the X-Request-ID of the caller')
  else
    Fail('Trace correlation id (given)', 'X-Request-ID=' + lResp.HeaderValue('X-Request-ID'));
  lResp := TMVCRESTClient.New.BaseURL(AClient.BaseURL).Get('/echo');
  if lResp.HeaderValue('X-Request-ID') <> '' then
    Pass('Trace generates an X-Request-ID when the caller sends none')
  else
    Fail('Trace correlation id (generated)', 'no X-Request-ID in the response');

  // POST with JSON body -- the body should be readable post-filter by the
  // engine even though Trace called ReadTotalContent pre-Next.
  lResp := AClient.AddHeader('Content-Type', TMVCMediaType.APPLICATION_JSON)
                  .Post('/echo', '{"hello":"world"}');
  // /echo is GET only -> 404/405 expected; we only check Trace didn't crash.
  if (lResp.StatusCode >= 400) and (lResp.StatusCode < 500) then
    Pass('Trace POST body read -> 4xx (route mismatch, no crash)')
  else
    Fail('Trace POST', Format('unexpected status %d', [lResp.StatusCode]));
end;

// -- scenario 8: Analytics -- post-Next CSV observer -----------------------

procedure ConfigAnalytics(AEngine: TMVCEngine);
begin
  AEngine.Use(AnalyticsHTTPFilter()); // default CSV logger under bin\analytics
  AEngine.Root.AsApi.MapGet('/ping',
    function: IMVCResponse
    begin
      Result := Ok('pong');
    end);
end;

procedure TestAnalytics(AClient: IMVCRESTClient);
begin
  // Smoke: filter must not corrupt the response. CSV emission is async via
  // LoggerPro and depends on filesystem permissions, so we only assert the
  // pass-through behavior here.
  AssertStatus(AClient.Get('/ping'), 200, 'Analytics passthrough -> 200');
end;

// -- scenario 9: Redirect -- pattern match + 301 ---------------------------

procedure ConfigRedirect(AEngine: TMVCEngine);
begin
  AEngine.Use(RedirectHTTPFilter(['/old', '/legacy'], '/new', rdkPermanent));
  AEngine.Root.AsApi.MapGet('/new',
    function: IMVCResponse
    begin
      Result := Ok('arrived');
    end);
end;

procedure TestRedirect(AClient: IMVCRESTClient);
var
  lResp: IMVCRESTResponse;
begin
  // IMVCRESTClient follows 3xx by default; disable so we can observe the
  // redirect response directly.
  AClient := TMVCRESTClient.New.BaseURL(AClient.BaseURL).HandleRedirects(False);
  lResp := AClient.Get('/old');
  if (lResp.StatusCode = 301)
     and (lResp.HeaderValue('Location') = '/new') then
    Pass('Redirect /old -> 301 + Location /new')
  else
    Fail('Redirect /old', Format('status=%d location=%s',
      [lResp.StatusCode, lResp.HeaderValue('Location')]));

  AClient := TMVCRESTClient.New.BaseURL(AClient.BaseURL).HandleRedirects(False);
  lResp := AClient.AddQueryStringParam('x', '1').AddQueryStringParam('y', '2').Get('/old');
  if (lResp.StatusCode = 301)
     and lResp.HeaderValue('Location').StartsWith('/new?') then
    Pass('Redirect /old?x=1&y=2 -> 301 + Location preserves query: ' + lResp.HeaderValue('Location'))
  else
    Fail('Redirect with QS', Format('status=%d location=%s',
      [lResp.StatusCode, lResp.HeaderValue('Location')]));

  AClient := TMVCRESTClient.New.BaseURL(AClient.BaseURL).HandleRedirects(False);
  lResp := AClient.Get('/new');
  AssertStatus(lResp, 200, 'Redirect: non-matching /new passes through');
end;

// -- scenario 10: BasicAuth (callback overload) ----------------------------

procedure ConfigBasicAuth(AEngine: TMVCEngine);
begin
  AEngine.Root.Use(BasicAuthEndpointFilter(
    function (const U, P: string; var R: TArray<string>): Boolean
    begin
      Result := (U = 'alice') and (P = 's3cret');
      if Result then
        R := TArray<string>.Create('user');
    end, 'MyTestRealm'))
    .MapGet('/secure',
      function: IMVCResponse
      begin
        Result := Ok('welcome');
      end);
end;

procedure TestBasicAuth(AClient: IMVCRESTClient);
var
  lResp: IMVCRESTResponse;
  lAuthHeader: string;
begin
  AClient := TMVCRESTClient.New.BaseURL(AClient.BaseURL);
  lResp := AClient.Get('/secure');
  if (lResp.StatusCode = 401)
     and lResp.HeaderValue('WWW-Authenticate').StartsWith('Basic realm=', True) then
    Pass('BasicAuth no creds -> 401 + WWW-Authenticate')
  else
    Fail('BasicAuth no creds', Format('status=%d auth=%s',
      [lResp.StatusCode, lResp.HeaderValue('WWW-Authenticate')]));

  // alice:s3cret -> "YWxpY2U6czNjcmV0"
  lAuthHeader := 'Basic YWxpY2U6czNjcmV0';
  AClient := TMVCRESTClient.New.BaseURL(AClient.BaseURL);
  lResp := AClient.AddHeader('Authorization', lAuthHeader).Get('/secure');
  AssertStatus(lResp, 200, 'BasicAuth valid creds -> 200');

  // alice:wrong -> "YWxpY2U6d3Jvbmc="
  AClient := TMVCRESTClient.New.BaseURL(AClient.BaseURL);
  lResp := AClient.AddHeader('Authorization', 'Basic YWxpY2U6d3Jvbmc=').Get('/secure');
  AssertStatus(lResp, 401, 'BasicAuth wrong password -> 401');
end;

// -- scenario 11: FileSession persists across requests ---------------------

var
  GSessionTmpFolder: string;

procedure ConfigFileSession(AEngine: TMVCEngine);
begin
  GSessionTmpFolder := TPath.Combine(TPath.GetTempPath,
    'dmvc_filtertest_sess_' + TGUID.NewGuid.ToString.Replace('{', '').Replace('}', ''));
  TDirectory.CreateDirectory(GSessionTmpFolder);

  AEngine.Root.AsWeb.Use(FileSessionEndpointFilter(10, GSessionTmpFolder))
    .MapGet<TWebContext>('/sess/set',
      function (Ctx: TWebContext): IMVCResponse
      begin
        Ctx.Session['k'] := Ctx.Request.QueryStringParam('v');
        Result := Ok('set:' + Ctx.Request.QueryStringParam('v'));
      end);
  AEngine.Root.AsWeb.Use(FileSessionEndpointFilter(10, GSessionTmpFolder))
    .MapGet<TWebContext>('/sess/get',
      function (Ctx: TWebContext): IMVCResponse
      begin
        Result := Ok(Ctx.Session['k']);
      end);
end;

procedure TestFileSession(AClient: IMVCRESTClient);
var
  lResp: IMVCRESTResponse;
begin
  // Same client across calls so the session cookie sticks.
  lResp := AClient.AddQueryStringParam('v', 'foo').Get('/sess/set');
  AssertStatus(lResp, 200, 'FileSession set -> 200');

  lResp := AClient.Get('/sess/get');
  // Ok('foo') wraps the value in a {"message":"foo"} JSON envelope.
  if (lResp.StatusCode = 200) and lResp.Content.Contains('"foo"') then
    Pass('FileSession get -> 200 body contains "foo" (persisted across requests)')
  else
    Fail('FileSession get', Format('status=%d body=%s',
      [lResp.StatusCode, lResp.Content]));

  // Cleanup: best-effort, ignore failures.
  try
    TDirectory.Delete(GSessionTmpFolder, True);
  except
  end;
end;

// -- scenarios 12-13: the session HTTPFilters (controllers use them too) ---

procedure MapSessionRoutes(AEngine: TMVCEngine);
begin
  AEngine.Root.AsWeb
    .MapGet<TWebContext>('/sess/set',
      function (Ctx: TWebContext): IMVCResponse
      begin
        Ctx.Session['k'] := Ctx.Request.QueryStringParam('v');
        Result := Ok('set:' + Ctx.Request.QueryStringParam('v'));
      end);
  AEngine.Root.AsWeb
    .MapGet<TWebContext>('/sess/get',
      function (Ctx: TWebContext): IMVCResponse
      begin
        Result := Ok(Ctx.Session['k']);
      end);
end;

procedure TestSessionFilter(AClient: IMVCRESTClient; const AName: string);
var
  lResp: IMVCRESTResponse;
begin
  // Same client across calls so the session cookie sticks.
  AssertStatus(AClient.AddQueryStringParam('v', 'foo').Get('/sess/set'), 200, AName + ' set -> 200');
  lResp := AClient.Get('/sess/get');
  if (lResp.StatusCode = 200) and lResp.Content.Contains('"foo"') then
    Pass(AName + ' get -> 200 body contains "foo" (persisted across requests)')
  else
    Fail(AName + ' get', Format('status=%d body=%s', [lResp.StatusCode, lResp.Content]));
end;

procedure ConfigMemorySessionFilter(AEngine: TMVCEngine);
begin
  AEngine.Use(MemorySessionHTTPFilter(10));
  MapSessionRoutes(AEngine);
end;

procedure TestMemorySessionFilter(AClient: IMVCRESTClient);
begin
  TestSessionFilter(AClient, 'MemorySessionHTTPFilter');
end;

procedure ConfigFileSessionFilter(AEngine: TMVCEngine);
begin
  GSessionTmpFolder := TPath.Combine(TPath.GetTempPath,
    'dmvc_filtertest_sessf_' + TGUID.NewGuid.ToString.Replace('{', '').Replace('}', ''));
  TDirectory.CreateDirectory(GSessionTmpFolder);
  AEngine.Use(FileSessionHTTPFilter(10, GSessionTmpFolder));
  MapSessionRoutes(AEngine);
end;

procedure TestFileSessionFilter(AClient: IMVCRESTClient);
var
  lFiles: Integer;
begin
  TestSessionFilter(AClient, 'FileSessionHTTPFilter');
  lFiles := Length(TDirectory.GetFiles(GSessionTmpFolder));
  if lFiles > 0 then
    Pass('FileSessionHTTPFilter wrote the session under the folder')
  else
    Fail('FileSessionHTTPFilter session file', 'no file under ' + GSessionTmpFolder);
  try
    TDirectory.Delete(GSessionTmpFolder, True);
  except
  end;
end;

// -- scenario 19: the simple StaticFiles form is the options form with defaults ----

procedure ConfigStaticSimple(AEngine: TMVCEngine);
begin
  AEngine.Use(StaticFilesHTTPFilter('/files/', WWW_DIR)); // a trailing / in the prefix is fine
end;

procedure TestStaticSimple(AClient: IMVCRESTClient);
var
  lResp: IMVCRESTResponse;
  lLastModified: string;
begin
  AssertStatusBody(AClient.Get('/files/test.html'), 200, TEST_HTML, 'prefix with a trailing /: file served');
  AssertStatusBody(AClient.Get('/FILES/test.html'), 200, TEST_HTML, 'the prefix is case-insensitive');
  AssertStatusBody(AClient.Get('/files/sub/'), 200, SUB_INDEX_HTML, 'index.html of a sub folder');
  AssertStatus(AClient.Get('/files/../etc/passwd'), 404, 'a path out of the root -> 404');
  AssertStatus(AClient.Get('/files/missing.html'), 404, 'a missing file -> 404, not index.html (no SPA unless asked)');

  lResp := TMVCRESTClient.New.BaseURL(AClient.BaseURL).HandleRedirects(False).Get('/files/sub');
  if (lResp.StatusCode = 301) and (lResp.HeaderValue('Location') = '/files/sub/') then
    Pass('a folder without the trailing / -> 301 /files/sub/')
  else
    Fail('folder redirect', Format('status=%d location=%s', [lResp.StatusCode, lResp.HeaderValue('Location')]));

  lResp := AClient.Get('/files/test.html');
  lLastModified := lResp.HeaderValue('Last-Modified');
  if lLastModified = '' then
    Fail('Last-Modified', 'the header is missing')
  else
  begin
    Pass('Last-Modified is sent');
    lResp := TMVCRESTClient.New.BaseURL(AClient.BaseURL)
      .AddHeader('If-Modified-Since', lLastModified).Get('/files/test.html');
    AssertStatus(lResp, 304, 'If-Modified-Since -> 304');
  end;
end;

// -- scenario 20: single-page application, asked for in the simple form -------

procedure MapAPINextToTheSPA(AEngine: TMVCEngine);
begin
  AEngine.Root.AsApi.MapGet('/api/ping',
    function: IMVCResponse
    begin
      Result := Ok('pong');
    end);
  AEngine.Root.AsApi.MapGet('/api/gone',
    function: IMVCResponse
    begin
      Result := Status(http_status.NotFound, 'gone');
    end);
end;

procedure ConfigStaticSPA(AEngine: TMVCEngine);
begin
  // the application at / and an API next to it: the router goes first
  AEngine.Use(StaticFilesHTTPFilter('/', WWW_DIR, sfmSPA));
  MapAPINextToTheSPA(AEngine);
end;

procedure TestStaticSPA(AClient: IMVCRESTClient);
var
  lResp: IMVCRESTResponse;
begin
  AssertStatusBody(AClient.Get('/test.html'), 200, TEST_HTML, 'SPA: an existing file is served as it is');
  AssertStatusBody(AClient.Get('/people/12/edit'), 200, INDEX_HTML, 'SPA: a client-side route -> index.html');
  AssertStatusBody(AClient.Get('/sub/deep/route'), 200, SUB_INDEX_HTML, 'SPA: the nearest folder with an index.html');

  lResp := AClient.Get('/api/ping');
  if (lResp.StatusCode = 200) and lResp.Content.Contains('pong') then
    Pass('SPA at /: a route of the API is not hidden by index.html')
  else
    Fail('SPA at /: API route', Format('status=%d body=%s', [lResp.StatusCode, lResp.Content]));

  lResp := AClient.Get('/api/gone');
  if (lResp.StatusCode = 404) and (not lResp.Content.Contains(INDEX_HTML)) then
    Pass('SPA at /: the 404 of an action stays a 404')
  else
    Fail('SPA at /: 404 of an action', Format('status=%d body=%s', [lResp.StatusCode, lResp.Content]));

  AssertStatus(AClient.Post('/nothing-here', ''), 404, 'SPA: a POST without a route stays a 404');
end;

// -- scenario 21: RangeMedia with a root written with / ----------------------

procedure ConfigRangeMedia(AEngine: TMVCEngine);
var
  lRoot: string;
begin
  // the folder of the fixtures, spelled with / : on Windows that is a mix of
  // separators once joined, and the filter must still find the files
  lRoot := StringReplace(TPath.Combine(TPath.Combine(GetCurrentDir, WWW_DIR), 'media'),
    PathDelim, '/', [rfReplaceAll]);
  AEngine.Use(RangeMediaHTTPFilter('/media', lRoot));
end;

procedure TestRangeMedia(AClient: IMVCRESTClient);
var
  lResp: IMVCRESTResponse;
begin
  lResp := AClient.AddHeader('Range', 'bytes=0-9').Get('/media/clip.txt');
  if (lResp.StatusCode = 206) and (Length(lResp.ContentRawBytes) = 10)
     and (lResp.HeaderValue('Content-Range') = Format('bytes 0-9/%d', [MEDIA_SIZE])) then
    Pass('RangeMedia, root spelled with /: bytes=0-9 -> 206 and the 10 bytes')
  else
    Fail('RangeMedia, root spelled with /', Format('status=%d len=%d range=%s',
      [lResp.StatusCode, Length(lResp.ContentRawBytes), lResp.HeaderValue('Content-Range')]));
  AssertStatus(TMVCRESTClient.New.BaseURL(AClient.BaseURL).Get('/media/clip.txt'), 200,
    'RangeMedia, root spelled with /: no Range header -> 200');
end;

// -- scenario 23: FileServerBundle = scenarios 20 and 21 with one Use ---------

procedure ConfigFileServer(AEngine: TMVCEngine);
begin
  // files + Range (206) + SPA fallback on the same folder and prefix
  AEngine.Use(FileServerBundle('/', WWW_DIR, fsmSPA));
  MapAPINextToTheSPA(AEngine);
end;

procedure TestFileServer(AClient: IMVCRESTClient);
var
  lResp: IMVCRESTResponse;
begin
  WriteLn('FileServerBundle(''/'', ''www'', fsmSPA):');
  TestStaticSPA(AClient);
  lResp := TMVCRESTClient.New.BaseURL(AClient.BaseURL).AddHeader('Range', 'bytes=0-9').Get('/media/clip.txt');
  if (lResp.StatusCode = 206) and (Length(lResp.ContentRawBytes) = 10)
     and (lResp.HeaderValue('Content-Range') = Format('bytes 0-9/%d', [MEDIA_SIZE])) then
    Pass('FileServerBundle: bytes=0-9 -> 206 and the 10 bytes')
  else
    Fail('FileServerBundle, Range', Format('status=%d len=%d range=%s',
      [lResp.StatusCode, Length(lResp.ContentRawBytes), lResp.HeaderValue('Content-Range')]));
  lResp := TMVCRESTClient.New.BaseURL(AClient.BaseURL).Get('/media/clip.txt');
  if (lResp.StatusCode = 200) and (Length(lResp.ContentRawBytes) = MEDIA_SIZE) then
    Pass('FileServerBundle: no Range header -> 200 and the whole file')
  else
    Fail('FileServerBundle, no Range', Format('status=%d len=%d', [lResp.StatusCode, Length(lResp.ContentRawBytes)]));
end;

// -- scenario 22: exceptions and the filters ---------------------------------

procedure ConfigExceptions(AEngine: TMVCEngine);
begin
  AEngine.Use(CORSHTTPFilter('*'));
  AEngine.Use(
    procedure (const AContext: TWebContext; const ANext: TMVCHTTPFilterNext)
    begin
      if AContext.Request.PathInfo = '/filter-raises' then
        raise EMVCException.Create(http_status.Forbidden, 'no way');
      ANext();
    end);
  AEngine.Root.AsApi.MapGet('/boom',
    function: IMVCResponse
    begin
      raise Exception.Create('boom');
    end);
end;

procedure TestExceptions(AClient: IMVCRESTClient);
var
  lResp: IMVCRESTResponse;
begin
  // the action raises: the engine turns it into a 500 inside the dispatcher, so the
  // code of CORS after ANext() still runs and the browser can read the error
  lResp := TMVCRESTClient.New.BaseURL(AClient.BaseURL).AddHeader('Origin', 'http://example.com').Get('/boom');
  if (lResp.StatusCode = 500) and (lResp.HeaderValue('Access-Control-Allow-Origin') = '*') then
    Pass('an action that raises: 500, and CORS stamps the answer')
  else
    Fail('action raises', Format('status=%d ACAO=%s', [lResp.StatusCode, lResp.HeaderValue('Access-Control-Allow-Origin')]));

  // a filter raises: nothing inside the dispatcher can catch it, the engine answers
  lResp := TMVCRESTClient.New.BaseURL(AClient.BaseURL).AddHeader('Origin', 'http://example.com').Get('/filter-raises');
  if lResp.StatusCode = 403 then
    Pass('a filter that raises EMVCException(403): the answer is a 403')
  else
    Fail('filter raises', Format('status=%d', [lResp.StatusCode]));
  WriteLn('[INFO] the 403 of the filter that raises carries Access-Control-Allow-Origin="',
    lResp.HeaderValue('Access-Control-Allow-Origin'), '"');
end;

// -- scenario 17/18: the cookie flags of the session filters ------------------

procedure ConfigSessionCookieFlags(AEngine: TMVCEngine);
begin
  AEngine.Use(MemorySessionHTTPFilter(10, [scfHttpOnly, scfSecure]));
  MapSessionRoutes(AEngine);
end;

procedure TestSessionCookieFlags(AClient: IMVCRESTClient);
var
  lCookies: TCookies;
begin
  lCookies := AClient.AddQueryStringParam('v', 'x').Get('/sess/set').Cookies;
  if (lCookies.Count > 0) and lCookies[0].HttpOnly and lCookies[0].Secure then
    Pass('[scfHttpOnly, scfSecure] -> the session cookie is HttpOnly and Secure')
  else
    Fail('session cookie flags', Format('cookies=%d', [lCookies.Count]));
end;

procedure ConfigSessionCookieDefault(AEngine: TMVCEngine);
begin
  AEngine.Use(MemorySessionHTTPFilter(10));
  MapSessionRoutes(AEngine);
end;

procedure TestSessionCookieDefault(AClient: IMVCRESTClient);
var
  lCookies: TCookies;
begin
  lCookies := AClient.AddQueryStringParam('v', 'x').Get('/sess/set').Cookies;
  if (lCookies.Count > 0) and lCookies[0].HttpOnly and (not lCookies[0].Secure) then
    Pass('default flags -> the session cookie is HttpOnly and not Secure')
  else
    Fail('session cookie default flags', Format('cookies=%d', [lCookies.Count]));
end;

// -- scenario 14: ActiveRecordHTTPFilter gives the action a connection and gives it back
// even when the action raises -------------------------------------------------

const
  AR_CONDEF = 'filtertest_ar';

procedure ConfigActiveRecordFilter(AEngine: TMVCEngine);
var
  lParams: TStringList;
begin
  lParams := TStringList.Create;
  try
    lParams.Add('Database=:memory:');
    FDManager.AddConnectionDef(AR_CONDEF, 'SQLite', lParams);
  finally
    lParams.Free;
  end;
  // empty file name: the definition above is already in the manager
  AEngine.Use(ActiveRecordHTTPFilter(AR_CONDEF, ''));
  AEngine.Root.AsApi.MapGet('/ar/has',
    function: IMVCResponse
    begin
      Result := Ok(BoolToStr(ActiveRecordConnectionsRegistry.GetCurrent(False) <> nil, True));
    end);
  AEngine.Root.AsApi.MapGet('/ar/boom',
    function: IMVCResponse
    begin
      raise Exception.Create('boom');
    end);
end;

procedure TestActiveRecordFilter(AClient: IMVCRESTClient);
var
  lResp: IMVCRESTResponse;
begin
  lResp := AClient.Get('/ar/has');
  if (lResp.StatusCode = 200) and lResp.Content.Contains('True') then
    Pass('ActiveRecordHTTPFilter: the action has the default connection')
  else
    Fail('ActiveRecordHTTPFilter has', Format('status=%d body=%s', [lResp.StatusCode, lResp.Content]));

  AssertStatus(AClient.Get('/ar/boom'), 500, 'ActiveRecordHTTPFilter: a raising action answers 500');

  // the leaked connection of a skipped release would make this call raise "already registered"
  lResp := AClient.Get('/ar/has');
  if (lResp.StatusCode = 200) and lResp.Content.Contains('True') then
    Pass('ActiveRecordHTTPFilter: the connection is given back when the action raised')
  else
    Fail('ActiveRecordHTTPFilter after boom', Format('status=%d body=%s', [lResp.StatusCode, lResp.Content]));
end;

// -- scenario 15: RateLimit with options -----------------------------------

var
  GRateLimitExceeded: Integer = 0;

procedure ConfigRateLimitOptions(AEngine: TMVCEngine);
var
  lOptions: TMVCRateLimitOptions;
begin
  lOptions := TMVCRateLimitOptions.Default;
  lOptions.MaxRequests := 2;
  lOptions.WindowSeconds := 60;
  lOptions.HeaderPrefix := 'X-Quota-';
  lOptions.ExcludedPaths := ['/health'];
  lOptions.OnExceeded :=
    procedure (const AContext: TWebContext; const AKey: string;
      const ALimit: Integer; const AWindowSeconds: Integer)
    begin
      Inc(GRateLimitExceeded);
    end;
  AEngine.Use(RateLimitHTTPFilter(lOptions));
  AEngine.Root.AsApi.MapGet('/ping',
    function: IMVCResponse
    begin
      Result := Ok('pong');
    end);
  AEngine.Root.AsApi.MapGet('/health',
    function: IMVCResponse
    begin
      Result := Ok('up');
    end);
end;

procedure TestRateLimitOptions(AClient: IMVCRESTClient);
var
  lResp: IMVCRESTResponse;
  I: Integer;
begin
  lResp := AClient.Get('/ping');
  if (lResp.StatusCode = 200) and (lResp.HeaderValue('X-Quota-Limit') = '2') and
    (lResp.HeaderValue('X-Quota-Remaining') <> '') then
    Pass('RateLimit options: custom header prefix on the answer')
  else
    Fail('RateLimit options header', Format('status=%d limit=%s',
      [lResp.StatusCode, lResp.HeaderValue('X-Quota-Limit')]));
  AssertStatus(AClient.Get('/ping'), 200, 'RateLimit options req#2 -> 200');

  lResp := AClient.Get('/ping');
  if (lResp.StatusCode = 429) and (lResp.HeaderValue('Retry-After') <> '') and
    lResp.Content.Contains('Rate limit exceeded') then
    Pass('RateLimit options req#3 -> 429 + Retry-After + JSON body')
  else
    Fail('RateLimit options req#3', Format('status=%d body=%s', [lResp.StatusCode, lResp.Content]));

  if GRateLimitExceeded = 1 then
    Pass('RateLimit options: OnExceeded called once')
  else
    Fail('RateLimit options OnExceeded', 'called ' + GRateLimitExceeded.ToString + ' times');

  for I := 1 to 4 do
    if AClient.Get('/health').StatusCode <> 200 then
    begin
      Fail('RateLimit options excluded path', 'request ' + I.ToString + ' was limited');
      Exit;
    end;
  Pass('RateLimit options: an excluded path is never limited');
end;

// -- scenario 16: engine-wide endpoint filters (post-routing) around controller actions and Minimal API routes --

type
  [MVCPath('/ctl')]
  TFilterTestController = class(TMVCController)
  public
    [MVCPath('/ok')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(TMVCMediaType.APPLICATION_JSON)] // the bench engine defaults to text/html
    function OkAction: IMVCResponse;
    // the legacy shape (procedure + Render) has to go through the filters as well
    [MVCPath('/legacy')]
    [MVCHTTPMethod([httpGET])]
    procedure LegacyAction;
    [MVCPath('/boom')]
    [MVCHTTPMethod([httpGET])]
    function BoomAction: IMVCResponse;
  end;

var
  GActionRuns: Integer = 0;
  GFilterTrace: string = '';
  GFilterController: string = '';
  GFilterAction: string = '';

function TFilterTestController.OkAction: IMVCResponse;
begin
  Inc(GActionRuns);
  Result := Ok('from the action');
end;

procedure TFilterTestController.LegacyAction;
begin
  Inc(GActionRuns);
  Render('from the legacy action');
end;

function TFilterTestController.BoomAction: IMVCResponse;
begin
  raise Exception.Create('boom');
end;

procedure ConfigEndpointFilters(AEngine: TMVCEngine);
begin
  AEngine.AddController(TFilterTestController);
  // the first registered is the outermost: A is entered first and left last
  AEngine.Use(
    function (const AContext: TWebContext; const ANext: TMVCEndpointFilterNext): IMVCResponse
    begin
      GFilterTrace := GFilterTrace + 'A>';
      GFilterController := AContext.ControllerQualifiedClassName;
      GFilterAction := AContext.ActionName;
      Result := ANext();
      GFilterTrace := GFilterTrace + '<A';
      AContext.Response.SetCustomHeader('X-Endpoint-Filter', 'A');
    end);
  AEngine.Use(
    function (const AContext: TWebContext; const ANext: TMVCEndpointFilterNext): IMVCResponse
    begin
      if AContext.Request.Headers['X-Deny'] <> '' then
        Exit(Forbidden('denied by the filter')); // answers by itself: the action does not run
      GFilterTrace := GFilterTrace + 'B>';
      Result := ANext();
      GFilterTrace := GFilterTrace + '<B';
    end);
  AEngine.Root.AsApi.MapGet('/min',
    function: IMVCResponse
    begin
      Result := Ok('from the route');
    end);
end;

procedure TestEndpointFilters(AClient: IMVCRESTClient);
var
  lResp: IMVCRESTResponse;
begin
  // controller action, functional
  GFilterTrace := '';
  lResp := AClient.Get('/ctl/ok');
  if (lResp.StatusCode = 200) and lResp.Content.Contains('from the action') and
    (lResp.HeaderValue('X-Endpoint-Filter') = 'A') and (GFilterTrace = 'A>B><B<A') then
    Pass('EndpointFilter around a controller action: answer intact, filters nested A>B><B<A')
  else
    Fail('EndpointFilter controller action', Format('status=%d body=%s header=%s trace=%s',
      [lResp.StatusCode, lResp.Content, lResp.HeaderValue('X-Endpoint-Filter'), GFilterTrace]));

  if GFilterController.EndsWith('TFilterTestController') and (GFilterAction = 'OkAction') then
    Pass('EndpointFilter sees the controller and the action in the context')
  else
    Fail('EndpointFilter context', 'controller=' + GFilterController + ' action=' + GFilterAction);

  // controller action, legacy procedure + Render
  lResp := AClient.Get('/ctl/legacy');
  if (lResp.StatusCode = 200) and lResp.Content.Contains('from the legacy action') and
    (lResp.HeaderValue('X-Endpoint-Filter') = 'A') then
    Pass('EndpointFilter around a legacy procedure action')
  else
    Fail('EndpointFilter legacy action', Format('status=%d body=%s', [lResp.StatusCode, lResp.Content]));

  // a filter that answers by itself: 403, and the action does not run
  GActionRuns := 0;
  lResp := AClient.AddHeader('X-Deny', '1').Get('/ctl/ok');
  if (lResp.StatusCode = 403) and lResp.Content.Contains('denied by the filter') and (GActionRuns = 0) then
    Pass('EndpointFilter that answers by itself: 403 and the action did not run')
  else
    Fail('EndpointFilter short-circuit', Format('status=%d body=%s runs=%d',
      [lResp.StatusCode, lResp.Content, GActionRuns]));

  // an exception of the action goes through the filters to the engine
  AClient := TMVCRESTClient.New.BaseURL(AClient.BaseURL);
  AssertStatus(AClient.Get('/ctl/boom'), 500, 'EndpointFilter: an action that raises answers 500');
  lResp := AClient.Get('/ctl/ok');
  AssertStatus(lResp, 200, 'EndpointFilter: the next request is fine after an exception');

  // Minimal API route: the same filters, an empty class name and "verb pattern" as the action
  GFilterTrace := '';
  lResp := AClient.Get('/min');
  if (lResp.StatusCode = 200) and lResp.Content.Contains('from the route') and
    (lResp.HeaderValue('X-Endpoint-Filter') = 'A') and (GFilterTrace = 'A>B><B<A') then
    Pass('EndpointFilter around a Minimal API route')
  else
    Fail('EndpointFilter Minimal API', Format('status=%d body=%s trace=%s',
      [lResp.StatusCode, lResp.Content, GFilterTrace]));
  if (GFilterController = '') and (GFilterAction = 'GET /min') then
    Pass('EndpointFilter on a route: no class name, "GET /min" as the action')
  else
    Fail('EndpointFilter route context', 'controller=' + GFilterController + ' action=' + GFilterAction);
end;

// -- scenario 17: the code of an HTTP filter after ANext() runs after the action, for a controller too -----

type
  [MVCPath('/hf')]
  THttpFilterOrderController = class(TMVCController)
  public
    [MVCPath('/order')]
    [MVCHTTPMethod([httpGET])]
    function Order: string;
  end;

var
  GHttpOrder: string = '';
  GHttpPostSawBody: string = '';

function THttpFilterOrderController.Order: string;
begin
  GHttpOrder := GHttpOrder + 'action ';
  Result := 'the-body';
end;

procedure ConfigHttpFilterOrder(AEngine: TMVCEngine);
begin
  AEngine.AddController(THttpFilterOrderController);
  AEngine.Use(
    procedure(const AContext: TWebContext; const ANext: TMVCHTTPFilterNext)
    begin
      if not (AContext.Request.PathInfo.StartsWith('/hf') or AContext.Request.PathInfo.StartsWith('/hfmin')) then
      begin
        ANext();
        Exit;
      end;
      GHttpOrder := GHttpOrder + 'pre ';
      ANext();
      GHttpOrder := GHttpOrder + 'post';
      // what the code after ANext() can see of the answer: the text, or the size of the stream
      if AContext.Response.ContentStream <> nil then
        GHttpPostSawBody := 'stream:' + IntToStr(AContext.Response.ContentStream.Size)
      else
        GHttpPostSawBody := AContext.Response.Content;
    end);
  AEngine.Root.AsApi.MapGet('/hfmin',
    function: IMVCResponse
    begin
      GHttpOrder := GHttpOrder + 'action ';
      Result := Ok('the-route-body');
    end);
end;

procedure TestHttpFilterOrder(AClient: IMVCRESTClient);
var
  lResp: IMVCRESTResponse;
begin
  GHttpOrder := '';
  GHttpPostSawBody := '';
  lResp := AClient.Get('/hfmin');
  if (lResp.StatusCode = 200) and (GHttpOrder = 'pre action post') and (GHttpPostSawBody <> '') then
    Pass('HTTP filter around a Minimal API route: pre, action, post and the post sees the body')
  else
    Fail('HTTP filter order, Minimal API', Format('status=%d order=[%s] body seen=[%s]',
      [lResp.StatusCode, GHttpOrder, GHttpPostSawBody]));

  GHttpOrder := '';
  GHttpPostSawBody := '';
  lResp := AClient.Get('/hf/order');
  if (lResp.StatusCode = 200) and (GHttpOrder = 'pre action post') and (GHttpPostSawBody <> '') then
    Pass('HTTP filter around a controller action: pre, action, post and the post sees the body')
  else
    Fail('HTTP filter order, controller', Format('status=%d order=[%s] body seen=[%s]',
      [lResp.StatusCode, GHttpOrder, GHttpPostSawBody]));
end;

begin
  IsMultiThread := True;
  try
    SetupFixtures;

    WithEngine(PORT_BASE,     ConfigStatic,         TestStatic);
    WithEngine(PORT_BASE + 1, ConfigIPBlockBans,    TestIPBlockBans);
    WithEngine(PORT_BASE + 2, ConfigIPBlockAllows,  TestIPBlockAllows);
    WithEngine(PORT_BASE + 3, ConfigRateLimit,      TestRateLimit);
    WithEngine(PORT_BASE + 4, ConfigCORS,           TestCORS);
    WithEngine(PORT_BASE + 5, ConfigRequestLog,     TestRequestLog);
    WithEngine(PORT_BASE + 6, ConfigTrace,          TestTrace);
    WithEngine(PORT_BASE + 7, ConfigAnalytics,      TestAnalytics);
    WithEngine(PORT_BASE + 8, ConfigRedirect,       TestRedirect);
    WithEngine(PORT_BASE + 9, ConfigBasicAuth,      TestBasicAuth);
    WithEngine(PORT_BASE + 10, ConfigFileSession,   TestFileSession);
    WithEngine(PORT_BASE + 11, ConfigMemorySessionFilter, TestMemorySessionFilter);
    WithEngine(PORT_BASE + 12, ConfigFileSessionFilter,   TestFileSessionFilter);
    WithEngine(PORT_BASE + 13, ConfigActiveRecordFilter,  TestActiveRecordFilter);
    WithEngine(PORT_BASE + 14, ConfigRateLimitOptions,    TestRateLimitOptions);
    WithEngine(PORT_BASE + 15, ConfigEndpointFilters,     TestEndpointFilters);
    WithEngine(PORT_BASE + 16, ConfigHttpFilterOrder,     TestHttpFilterOrder);
    WithEngine(PORT_BASE + 17, ConfigSessionCookieFlags,   TestSessionCookieFlags);
    WithEngine(PORT_BASE + 18, ConfigSessionCookieDefault, TestSessionCookieDefault);
    WithEngine(PORT_BASE + 19, ConfigStaticSimple,         TestStaticSimple);
    WithEngine(PORT_BASE + 20, ConfigStaticSPA,            TestStaticSPA);
    WithEngine(PORT_BASE + 21, ConfigRangeMedia,           TestRangeMedia);
    WithEngine(PORT_BASE + 22, ConfigExceptions,           TestExceptions);
    WithEngine(PORT_BASE + 23, ConfigFileServer,           TestFileServer);
  except
    on E: Exception do
    begin
      WriteLn('[CRASH] ', E.ClassName, ': ', E.Message);
      Inc(GFailures);
    end;
  end;

  WriteLn;
  if GFailures = 0 then
  begin
    WriteLn('ALL PASS');
    ExitCode := 0;
  end
  else
  begin
    WriteLn('FAILURES: ', GFailures);
    ExitCode := 1;
  end;
end.
