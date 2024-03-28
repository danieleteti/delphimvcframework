program services_injection;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MVCFramework,
  MVCFramework.Logger,
  MVCFramework.DotEnv,
  MVCFramework.Commons,
  MVCFramework.Signal,
  Web.ReqMulti,
  Web.WebReq,
  Web.WebBroker,
  IdContext,
  IdHTTPWebBrokerBridge,
  MainControllerU in 'MainControllerU.pas',
  WebModuleU in 'WebModuleU.pas' {MyWebModule: TWebModule},
  Services.PeopleU in 'Services.PeopleU.pas',
  Services.InterfacesU in 'Services.InterfacesU.pas',
  Entities in '..\commons\Entities.pas',
  MVCFramework.Router in '..\..\sources\MVCFramework.Router.pas',
  MVCFramework.Container in '..\..\sources\MVCFramework.Container.pas',
  Services.ConnectionU in 'Services.ConnectionU.pas',
  Services.RegistrationU in 'Services.RegistrationU.pas';

{$R *.res}


procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin

  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.OnParseAuthentication := TMVCParseAuthentication.OnParseAuthentication;
    LServer.DefaultPort := APort;
    LServer.KeepAlive := True;
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
  ReportMemoryLeaksOnShutdown := True;
  IsMultiThread := True;

  // DMVCFramework Specific Configuration
  // When MVCSerializeNulls = True empty nullables and nil are serialized as json null.
  // When MVCSerializeNulls = False empty nullables and nil are not serialized at all.
  MVCSerializeNulls := True;
  UseConsoleLogger := True;

  LogI('** DMVCFramework Server ** build ' + DMVCFRAMEWORK_VERSION);
  try
    dotEnvConfigure(
      function: IMVCDotEnv
      begin
        Result := NewDotEnv
                 .UseStrategy(TMVCDotEnvPriority.FileThenEnv)
                 .UseLogger(procedure(LogItem: String)
                            begin
                              LogD('dotEnv: ' + LogItem);
                            end)
                 .Build();
      end);

    RegisterServices(DefaultMVCServiceContainer);
    DefaultMVCServiceContainer.Build;

    if dotEnv.Env('dmvc.profiler.enabled', false) then
    begin
      Profiler.ProfileLogger := Log;
      Profiler.WarningThreshold := dotEnv.Env('dmvc.profiler.warning_threshold', 2000);
    end;

    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    WebRequestHandlerProc.MaxConnections := dotEnv.Env('dmvc.handler.max_connections', 1024);

    RunServer(dotEnv.Env('dmvc.server.port', 8080));
  except
    on E: Exception do
      LogF(E.ClassName + ': ' + E.Message);
  end;
end.
