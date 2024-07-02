program htmx_website;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Web.ReqMulti,
  Web.WebReq,
  Web.WebBroker,
  IdContext,
  IdHTTPWebBrokerBridge,
  MVCFramework,
  MVCFramework.Logger,
  MVCFramework.DotEnv,
  MVCFramework.Commons,
  MVCFramework.View.Renderers.Mustache,
  mormot.core.mustache,
  MVCFramework.Signal,
  ControllerU in 'ControllerU.pas',
  HelpersU in 'HelpersU.pas',
  WebModuleU in 'WebModuleU.pas' {MyWebModule: TWebModule};

{$R *.res}

procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.OnParseAuthentication := TMVCParseAuthentication.OnParseAuthentication;
    LServer.DefaultPort := APort;
    LServer.KeepAlive := dotEnv.Env('dmvc.indy.keep_alive', True);
    LServer.MaxConnections := dotEnv.Env('dmvc.webbroker.max_connections', 0);
    LServer.ListenQueue := dotEnv.Env('dmvc.indy.listen_queue', 500);
    LServer.Active := True;
    LogI('Listening on port ' + APort.ToString);
    LogI('Application started. Press Ctrl+C to shut down.');
    WaitForTerminationSignal;
    EnterInShutdownState;
    LServer.Active := False;
  finally
    LServer.Free;
  end;
end;

begin
  { Enable ReportMemoryLeaksOnShutdown during debug }
  // ReportMemoryLeaksOnShutdown := True;
  IsMultiThread := True;
  // DMVCFramework Specific Configuration
  // When MVCSerializeNulls = True empty nullables and nil are serialized as json null.
  // When MVCSerializeNulls = False empty nullables and nil are not serialized at all.
  MVCSerializeNulls := True;
  UseConsoleLogger := True;

  LogI('** DMVCFramework Server ** build ' + DMVCFRAMEWORK_VERSION);

  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;

    WebRequestHandlerProc.MaxConnections := dotEnv.Env('dmvc.handler.max_connections', 1024);

{$IF CompilerVersion >= 34} //SYDNEY+
    if dotEnv.Env('dmvc.profiler.enabled', false) then
    begin
      Profiler.ProfileLogger := Log;
      Profiler.WarningThreshold := dotEnv.Env('dmvc.profiler.warning_threshold', 2000);
    end;
{$ENDIF}

  // Project specific Mustache helpers
  TMVCMustacheHelpers.OnLoadCustomHelpers := procedure(var MustacheHelpers: TSynMustacheHelpers)
  begin
    TSynMustache.HelperAdd(MustacheHelpers, 'MyHelper1', TMyMustacheHelpers.MyHelper1);
    TSynMustache.HelperAdd(MustacheHelpers, 'MyHelper2', TMyMustacheHelpers.MyHelper2);
  end;

    RunServer(dotEnv.Env('dmvc.server.port', 8080));
  except
    on E: Exception do
      LogF(E.ClassName + ': ' + E.Message);
  end;
end.
