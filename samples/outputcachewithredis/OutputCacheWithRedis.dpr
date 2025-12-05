program OutputCacheWithRedis;
{$APPTYPE CONSOLE}


uses
  System.SysUtils,
  MVCFramework,
  MVCFramework.Signal,
  MVCFramework.Logger,
  MVCFramework.Commons,
  IdHTTPWebBrokerBridge,
  Web.WebReq,
  Web.WebBroker,
  WebModuleU in 'WebModuleU.pas' {wmMain: TWebModule},
  PeopleControllerU in 'Controllers\PeopleControllerU.pas',
  PeopleModuleU in 'Modules\PeopleModuleU.pas' {PeopleModule: TDataModule},
  PersonBO in 'BusinessObjects\PersonBO.pas',
  CommonsU in 'CommonsU.pas';

{$R *.res}


procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  LogI(Format('Starting "OutputCacheWithRedis" HTTP Server or port %d', [APort]));
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;
    LServer.ListenQueue := 200;
    LServer.MaxConnections := 5000;
    LServer.Active := True;
    LogI('Listening on http://localhost:' + APort.ToString + '/people/tests/many');
    LogW('Start from route "/people/tests/many" then inspect all other features');
    LogI('Application started. Press Ctrl+C to shut down.');
    LogI('Press CTRL+C to stop the server');
    WaitForTerminationSignal;
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
      Writeln(E.ClassName, ': ', E.Message);
  end

end.
