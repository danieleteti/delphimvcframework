program HTMX_Sample;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MVCFramework,
  MVCFramework.Logger,
  MVCFramework.Commons,
  MVCFramework.Signal,
  Web.ReqMulti,
  Web.WebReq,
  Web.WebBroker,
  IdContext,
  IdHTTPWebBrokerBridge,
  uConfig.Module in 'uConfig.Module.pas' {ConfigModule: TWebModule},
  uBase.Controller in 'Controllers\uBase.Controller.pas',
  uMovie.Controller in 'Controllers\uMovie.Controller.pas',
  uData.Model in 'Classes\uData.Model.pas',
  uServices in 'Classes\uServices.pas',
  MVCFramework.HTMX in 'MVCFramework.HTMX.pas';

{$R *.res}


procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  LogI('** DMVCFramework Server ** build ' + DMVCFRAMEWORK_VERSION);
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.OnParseAuthentication := TMVCParseAuthentication.OnParseAuthentication;
    LServer.DefaultPort := APort;
    LServer.KeepAlive := True;
    LServer.Active := True;
    LogI('Listening on port ' + APort.ToString);
    LogI('CTRL+C to shutdown the server');
    WaitForTerminationSignal;
    EnterInShutdownState;
    LServer.Active := False;
  finally
    LServer.Free;
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  IsMultiThread := True;
  // DMVCFramework Specific Configuration
  // When MVCSerializeNulls = True empty nullables and nil are serialized as json null.
  // When MVCSerializeNulls = False empty nullables and nil are not serialized at all.
  MVCSerializeNulls := True;
  UseConsoleLogger := True;
  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    WebRequestHandlerProc.MaxConnections := 1024;
    RunServer(dotEnv.Env('dmvc.server.port', 8080));
  except
    on E: Exception do
    begin
      LogI(E.ClassName + ': ' + E.Message);
    end;
  end;
end.
