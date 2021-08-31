program OutputCacheWithRedis;
{$APPTYPE CONSOLE}


uses
  System.SysUtils,
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
  Writeln(Format('Starting "OutputCacheWithRedis" HTTP Server or port %d', [APort]));
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;
    LServer.ListenQueue := 200;
    LServer.MaxConnections := 5000;
    LServer.Active := True;
    Writeln('Press RETURN to stop the server');
    ReadLn;
  finally
    LServer.Free;
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    RunServer(8080);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end

end.
