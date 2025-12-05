program routingsample;

{$APPTYPE CONSOLE}


uses
  System.SysUtils,
  IdHTTPWebBrokerBridge,
  MVCFramework,
  MVCFramework.Logger,
  MVCFramework.Signal,
  Web.WebReq,
  Web.WebBroker,
  WebModuleU in 'WebModuleU.pas' {WebModule1: TWebModule} ,
  RoutingSampleControllerU in 'RoutingSampleControllerU.pas',
  BusinessObjectsU in 'BusinessObjectsU.pas';

{$R *.res}


procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  LogI(Format('Starting HTTP Server or port %d', [APort]));
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;
    LServer.Active := True;
    LServer.ServerSoftware := 'DMVCFramework';
    LogI('Press CTRL+C to stop the server');
    WaitForTerminationSignal;
    LogW('Shutting down...');
  finally
    LServer.Free;
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  UseConsoleLogger := True;
  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    RunServer(8080);
  except
    on E: Exception do
      LogException(E, E.Message);
  end

end.
