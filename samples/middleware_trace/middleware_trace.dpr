program middleware_trace;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Console,
  MVCFramework.Signal,
  MVCFramework.Logger,
  MVCFramework.Server.Intf,
  MVCFramework.Server.Factory,
  MVCFramework.Middleware.Trace,
  MainControllerU in 'MainControllerU.pas';

{$R *.res}

const
  PORT = 8080;

procedure RunServer;
var
  lEngine: TMVCEngine;
  lServer: IMVCServer;
begin
  WriteLn('** DMVCFramework Trace Middleware Sample ** build ' + DMVCFRAMEWORK_VERSION);
  WriteLn;

  lEngine := TMVCEngine.Create(
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.DefaultContentType] := TMVCConstants.DEFAULT_CONTENT_TYPE;
      Config[TMVCConfigKey.DefaultContentCharset] := TMVCConstants.DEFAULT_CONTENT_CHARSET;
      Config[TMVCConfigKey.AllowUnhandledAction] := 'false';
      Config[TMVCConfigKey.ExposeServerSignature] := 'false';
    end);
  try
    lEngine.AddController(TMainController);

    // The Trace middleware logs the full request/response lifecycle and the
    // time spent serving each request. Here we log up to 2 KB of body per
    // request; every line is tagged with a per-request correlation id which is
    // also echoed back on the 'X-Request-ID' response header.
    lEngine.AddMiddleware(TMVCTraceMiddleware.Create(2048));

    lServer := TMVCServerFactory.CreateIndyDirect(lEngine);
    lServer.Listen(PORT);
    try
      WriteLn(Format('Server started on http://localhost:%d (Indy Direct)', [PORT]));
      WriteLn;
      WriteLn('Try these and watch the [trace] lines in this console:');
      WriteLn(Format('  curl http://localhost:%d/api', [PORT]));
      WriteLn(Format('  curl http://localhost:%d/api/customers', [PORT]));
      WriteLn(Format('  curl http://localhost:%d/api/customers/1', [PORT]));
      WriteLn(Format('  curl -X POST http://localhost:%d/api/customers \', [PORT]));
      WriteLn('       -H "Content-Type: application/json" \');
      WriteLn('       -d "{""name"":""Acme"",""city"":""Milan""}"');
      WriteLn;
      WriteLn('Each response carries an X-Request-ID header matching the [rid:...] log prefix.');
      WriteLn('Press Ctrl+C to stop.');
      WaitForTerminationSignal;
      WriteLn('Shutting down...');
    finally
      lServer.Stop;
      lServer := nil;
    end;
  finally
    lEngine.Free;
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  IsMultiThread := True;
  try
    RunServer;
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;
end.
