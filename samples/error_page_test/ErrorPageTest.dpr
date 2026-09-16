program ErrorPageTest;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.IOUtils,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.MinimalAPI,
  MVCFramework.View.Renderers.TemplatePro,
  MVCFramework.Server.Intf,
  MVCFramework.Server.Factory,
  MVCFramework.RESTClient,
  MVCFramework.RESTClient.Intf;

const
  PORT_BASE = 8811;
  VIEW_DIR  = 'templates_errtest';
  EMPTY_DIR = 'templates_empty';
  ERROR_TPL = '<h1>{{:status}} {{:statustext}}</h1><p>{{:error}}</p>';

type
  [MVCPath('/ctrl')]
  TBoomController = class(TMVCController)
  public
    [MVCPath('/boom')]
    [MVCHTTPMethod([httpGET])]
    procedure Boom;
    [MVCPath('/notfound')]
    [MVCHTTPMethod([httpGET])]
    procedure NotFound;
  end;

var
  GFailures: Integer = 0;

procedure TBoomController.Boom;
begin
  raise Exception.Create('classic-kaboom');
end;

procedure TBoomController.NotFound;
begin
  raise EMVCException.Create(404, 'classic-gone');
end;

procedure Pass(const AName: string);
begin
  WriteLn('[PASS] ', AName);
end;

procedure Fail(const AName, ADetail: string);
begin
  WriteLn('[FAIL] ', AName, ' -- ', ADetail);
  Inc(GFailures);
end;

procedure SetupFixtures;
var
  lDir: string;
begin
  lDir := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), VIEW_DIR);
  if not TDirectory.Exists(lDir) then
    TDirectory.CreateDirectory(lDir);
  TFile.WriteAllText(TPath.Combine(lDir, 'error.html'), ERROR_TPL, TEncoding.UTF8);
  lDir := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), EMPTY_DIR);
  if not TDirectory.Exists(lDir) then
    TDirectory.CreateDirectory(lDir);
end;

procedure WithEngine(APort: Integer; const AViewDir: string;
  const AShowDetails: Boolean;
  const AConfigure: TProc<TMVCEngine>; const ARunTests: TProc<IMVCRESTClient>);
var
  lEngine: TMVCEngine;
  lServer: IMVCServer;
  lClient: IMVCRESTClient;
  lOpts: TMVCExceptionHandlerOptions;
begin
  lEngine := TMVCEngine.Create(
    procedure (AConfig: TMVCConfig)
    begin
      AConfig[TMVCConfigKey.DefaultContentType] := TMVCMediaType.TEXT_HTML;
      AConfig[TMVCConfigKey.AllowUnhandledAction] := 'false';
      AConfig[TMVCConfigKey.LoadSystemControllers] := 'false';
      AConfig[TMVCConfigKey.ExposeServerSignature] := 'false';
      AConfig[TMVCConfigKey.ViewPath] := AViewDir;
      AConfig[TMVCConfigKey.DefaultViewFileExtension] := 'html';
    end);
  try
    lEngine.SetViewEngine(TMVCTemplateProViewEngine);
    if AShowDetails then
      lOpts := [ehShowDetails]
    else
      lOpts := [];
    lEngine.UseExceptionHandler('error', 'ErrTestApp', lOpts);
    AConfigure(lEngine);
    lServer := TMVCServerFactory.CreateIndyDirect(lEngine);
    lServer.Listen(APort);
    try
      Sleep(200);
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

procedure ConfigClassic(AEngine: TMVCEngine);
begin
  AEngine.AddController(TBoomController);
end;

// --- HTML path (view present, browser request) ---

procedure TestClassicHTML(AClient: IMVCRESTClient);
var
  lResp: IMVCRESTResponse;
begin
  // Classic controller raises + Accept:text/html -> handler fires, returns 500 HTML page
  lResp := AClient.AddHeader('Accept', 'text/html').Get('/ctrl/boom');
  if (lResp.StatusCode = 500) and lResp.Content.Contains('Internal Server Error')
     and lResp.ContentType.ToLower.Contains('text/html') then
    Pass('classic raise + html -> 500 HTML page')
  else
    Fail('classic + html', Format('status=%d ct=%s body=[%s]',
      [lResp.StatusCode, lResp.ContentType, lResp.Content]));

  // EMVCException(404) + text/html -> handler fires, returns 404 HTML page
  AClient := TMVCRESTClient.New.BaseURL(AClient.BaseURL);
  lResp := AClient.AddHeader('Accept', 'text/html').Get('/ctrl/notfound');
  if (lResp.StatusCode = 404) and lResp.Content.Contains('Not Found')
     and lResp.Content.Contains('<h1>') then
    Pass('classic EMVCException(404) + html -> 404 HTML page')
  else
    Fail('classic 404 + html', Format('status=%d body=[%s]',
      [lResp.StatusCode, lResp.Content]));
end;

// --- JSON path (view present, non-browser request) ---
// Handler must NOT fire: it must leave Handled=False and the framework legacy
// path (TMVCErrorResponse) must render instead.  The legacy JSON format uses
// "message" (not "type"/"about:blank").

procedure TestClassicJSON(AClient: IMVCRESTClient);
var
  lResp: IMVCRESTResponse;
begin
  // Handler returns Handled=False -> classic controller Render(E) -> legacy TMVCErrorResponse
  lResp := AClient.AddHeader('Accept', 'application/json').Get('/ctrl/boom');
  if (lResp.StatusCode = 500)
     and lResp.ContentType.ToLower.Contains('json')
     and lResp.Content.Contains('"message"')
     and (not lResp.Content.Contains('about:blank'))
     and (not lResp.Content.Contains('<h1>')) then
    Pass('classic raise + json -> legacy TMVCErrorResponse (handler did NOT intercept)')
  else
    Fail('classic + json (legacy expected)', Format('status=%d ct=%s body=[%s]',
      [lResp.StatusCode, lResp.ContentType, lResp.Content]));
end;

// --- No-match ---

procedure ConfigNoMatch(AEngine: TMVCEngine);
begin
end;

procedure TestNoMatch(AClient: IMVCRESTClient);
var
  lResp: IMVCRESTResponse;
begin
  // No-match + text/html + view present -> handler fires (E=nil, status=404) -> 404 HTML
  lResp := AClient.AddHeader('Accept', 'text/html').Get('/nope');
  if (lResp.StatusCode = 404) and lResp.Content.Contains('<h1>404 Not Found') then
    Pass('no-match + html -> 404 HTML page')
  else
    Fail('no-match + html', Format('status=%d body=[%s]',
      [lResp.StatusCode, lResp.Content]));

  // No-match + application/json + view present -> handler returns Handled=False ->
  // SendHTTPStatus runs, produces legacy TMVCErrorResponse JSON (no about:blank, no <h1>)
  AClient := TMVCRESTClient.New.BaseURL(AClient.BaseURL);
  lResp := AClient.AddHeader('Accept', 'application/json').Get('/nope');
  if (lResp.StatusCode = 404)
     and lResp.ContentType.ToLower.Contains('json')
     and lResp.Content.Contains('"message"')
     and (not lResp.Content.Contains('about:blank'))
     and (not lResp.Content.Contains('<h1>')) then
    Pass('no-match + json -> legacy SendHTTPStatus (handler did NOT intercept)')
  else
    Fail('no-match + json (legacy expected)', Format('status=%d ct=%s body=[%s]',
      [lResp.StatusCode, lResp.ContentType, lResp.Content]));
end;

// --- No view (EMPTY_DIR) --- handler installed but no error view file ---
// Handler MUST leave Handled=False in every case.

procedure ConfigMissingTpl(AEngine: TMVCEngine);
begin
  AEngine.AddController(TBoomController);
end;

procedure TestMissingTpl(AClient: IMVCRESTClient);
var
  lResp: IMVCRESTResponse;
begin
  // text/html + no view -> handler returns Handled=False -> legacy path, NOT our HTML page
  lResp := AClient.AddHeader('Accept', 'text/html').Get('/ctrl/boom');
  if (lResp.StatusCode = 500)
     and (not lResp.Content.Contains('<h1>500')) then
    Pass('no view + html -> framework default (no error HTML page rendered)')
  else
    Fail('no view + html', Format('status=%d ct=%s body=[%s]',
      [lResp.StatusCode, lResp.ContentType, lResp.Content]));
end;

// --- TestNoMatch with EMPTY_DIR (handler installed, no view) ---
// The /pippo regression: a REST project (no view) + UseExceptionHandler +
// no-match + text/html must NOT return about:blank RFC 7807.

procedure TestNoMatchEmptyDir(AClient: IMVCRESTClient);
var
  lResp: IMVCRESTResponse;
begin
  // Handler present, no view -> Handled=False -> SendHTTPStatus legacy (no about:blank).
  // The framework's built-in HTML error page is returned (DMVCFramework-styled
  // status-code page, NOT our TemplatePro '<h1>NNN...' template, NOT RFC 7807).
  lResp := AClient.AddHeader('Accept', 'text/html').Get('/pippo');
  if (lResp.StatusCode = 404)
     and (not lResp.Content.Contains('about:blank'))
     and (not lResp.Content.Contains('<h1>')) then
    Pass('/pippo regression: no-match + html + no view -> legacy (no about:blank, no HTML page)')
  else
    Fail('/pippo regression', Format('status=%d ct=%s body=[%s]',
      [lResp.StatusCode, lResp.ContentType, lResp.Content]));
end;

// --- ShowDetails ---

procedure TestShowDetails(AClient: IMVCRESTClient);
var
  lResp: IMVCRESTResponse;
begin
  lResp := AClient.AddHeader('Accept', 'text/html').Get('/ctrl/boom');
  if (lResp.StatusCode = 500) and lResp.Content.Contains('classic-kaboom') then
    Pass('ehShowDetails ON -> message shown in HTML page')
  else
    Fail('ehShowDetails ON', Format('body=[%s]', [lResp.Content]));
end;

// --- Minimal API routes ---

procedure ConfigMinimal(AEngine: TMVCEngine);
begin
  AEngine.Root.AsWeb.MapGet('/m/boom',
    function: IMVCResponse
    begin
      raise Exception.Create('minimal-kaboom');
    end);
  AEngine.Root.AsWeb.MapGet('/m/gone',
    function: IMVCResponse
    begin
      raise EMVCException.Create(404, 'minimal-gone');
    end);
end;

procedure TestMinimal(AClient: IMVCRESTClient);
var
  lResp: IMVCRESTResponse;
begin
  // Minimal + text/html + view present -> handler fires via HandleException -> 500 HTML
  lResp := AClient.AddHeader('Accept', 'text/html').Get('/m/boom');
  if (lResp.StatusCode = 500) and lResp.Content.Contains('Internal Server Error')
     and lResp.ContentType.ToLower.Contains('text/html') then
    Pass('minimal raise + html -> 500 HTML page (delegation works)')
  else
    Fail('minimal + html', Format('status=%d ct=%s body=[%s]',
      [lResp.StatusCode, lResp.ContentType, lResp.Content]));

  // Minimal + json -> handler leaves Handled=False -> minimal's RenderExceptionAsProblem
  // (this was already problem+json in the framework default before our handler; unchanged)
  AClient := TMVCRESTClient.New.BaseURL(AClient.BaseURL);
  lResp := AClient.AddHeader('Accept', 'application/json').Get('/m/boom');
  if (lResp.StatusCode = 500) and lResp.ContentType.ToLower.Contains('json')
     and (not lResp.Content.Contains('<h1>')) then
    Pass('minimal raise + json -> framework problem+json (handler did NOT intercept)')
  else
    Fail('minimal + json', Format('status=%d ct=%s body=[%s]',
      [lResp.StatusCode, lResp.ContentType, lResp.Content]));

  // Minimal EMVCException(404) + text/html -> handler fires -> 404 HTML
  AClient := TMVCRESTClient.New.BaseURL(AClient.BaseURL);
  lResp := AClient.AddHeader('Accept', 'text/html').Get('/m/gone');
  if (lResp.StatusCode = 404) and lResp.Content.Contains('Not Found') then
    Pass('minimal EMVCException(404) + html -> 404 HTML page')
  else
    Fail('minimal 404 + html', Format('status=%d body=[%s]',
      [lResp.StatusCode, lResp.Content]));
end;

// --- WithEngineNoHandler ---

procedure WithEngineNoHandler(APort: Integer; const AViewDir: string;
  const AConfigure: TProc<TMVCEngine>; const ARunTests: TProc<IMVCRESTClient>);
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
      AConfig[TMVCConfigKey.ViewPath] := AViewDir;
      AConfig[TMVCConfigKey.DefaultViewFileExtension] := 'html';
    end);
  try
    lEngine.SetViewEngine(TMVCTemplateProViewEngine);
    // NOTE: UseExceptionHandler is intentionally NOT called here
    AConfigure(lEngine);
    lServer := TMVCServerFactory.CreateIndyDirect(lEngine);
    lServer.Listen(APort);
    try
      Sleep(200);
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

// --- ehShowDetails OFF ---

procedure TestNoDetails(AClient: IMVCRESTClient);
var
  lResp: IMVCRESTResponse;
begin
  // HTML: message must NOT leak; status + reason shown in the HTML page
  lResp := AClient.AddHeader('Accept', 'text/html').Get('/ctrl/boom');
  if (lResp.StatusCode = 500)
     and (not lResp.Content.Contains('classic-kaboom'))
     and lResp.Content.Contains('Internal Server Error') then
    Pass('ehShowDetails OFF -> message absent (HTML page)')
  else
    Fail('ehShowDetails OFF (HTML)', Format('body=[%s]', [lResp.Content]));

  // JSON: handler leaves Handled=False -> legacy TMVCErrorResponse; message hidden
  // by the handler not being invoked.  The legacy error is whatever the framework
  // produces; we just check status and that the exception message did not leak
  // (it appears in the legacy "message" field when there is no handler suppressing
  // it, but we confirm the handler did NOT produce our RFC7807 format).
  AClient := TMVCRESTClient.New.BaseURL(AClient.BaseURL);
  lResp := AClient.AddHeader('Accept', 'application/json').Get('/ctrl/boom');
  if (lResp.StatusCode = 500)
     and lResp.ContentType.ToLower.Contains('json')
     and (not lResp.Content.Contains('about:blank')) then
    Pass('ehShowDetails OFF -> legacy JSON (handler did NOT intercept; no about:blank)')
  else
    Fail('ehShowDetails OFF (json)', Format('body=[%s]', [lResp.Content]));
end;

procedure TestNoHandlerLegacy(AClient: IMVCRESTClient);
var
  lResp: IMVCRESTResponse;
begin
  // No handler registered -> classic exception takes the LEGACY path
  // (TMVCErrorResponse), NOT UseExceptionHandler's output.
  lResp := AClient.AddHeader('Accept', 'application/json').Get('/ctrl/boom');
  if (lResp.StatusCode = 500)
     and (not lResp.Content.Contains('about:blank')) then
    Pass('no handler -> legacy classic error (not UseExceptionHandler output)')
  else
    Fail('no handler legacy (classic json)', Format('body=[%s]', [lResp.Content]));

  // Browser: no handler -> no error.html template rendered (no '<h1>500' from it)
  AClient := TMVCRESTClient.New.BaseURL(AClient.BaseURL);
  lResp := AClient.AddHeader('Accept', 'text/html').Get('/ctrl/boom');
  if (lResp.StatusCode = 500) and (not lResp.Content.Contains('<h1>500')) then
    Pass('no handler -> no error.html rendered (legacy path)')
  else
    Fail('no handler legacy (classic html)', Format('body=[%s]', [lResp.Content]));
end;

begin
  IsMultiThread := True;
  try
    SetupFixtures;
    // Suite A: handler + VIEW_DIR (error view present)
    WithEngine(PORT_BASE,     VIEW_DIR,  False, ConfigClassic,    TestClassicHTML);
    WithEngine(PORT_BASE + 1, VIEW_DIR,  False, ConfigClassic,    TestClassicJSON);
    WithEngine(PORT_BASE + 2, VIEW_DIR,  False, ConfigNoMatch,    TestNoMatch);
    WithEngine(PORT_BASE + 3, VIEW_DIR,  True,  ConfigClassic,    TestShowDetails);
    WithEngine(PORT_BASE + 4, VIEW_DIR,  False, ConfigMinimal,    TestMinimal);
    WithEngine(PORT_BASE + 5, VIEW_DIR,  False, ConfigClassic,    TestNoDetails);
    // Suite B: handler + EMPTY_DIR (no error view — simulates pure REST project)
    WithEngine(PORT_BASE + 6, EMPTY_DIR, False, ConfigMissingTpl, TestMissingTpl);
    WithEngine(PORT_BASE + 7, EMPTY_DIR, False, ConfigNoMatch,    TestNoMatchEmptyDir);
    // Suite C: no handler at all
    WithEngineNoHandler(PORT_BASE + 8, VIEW_DIR, ConfigClassic, TestNoHandlerLegacy);
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
