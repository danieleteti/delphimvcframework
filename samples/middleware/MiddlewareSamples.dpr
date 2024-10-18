program MiddlewareSamples;

{$APPTYPE CONSOLE}


uses
  System.SysUtils,
  Web.WebReq,
  Web.WebBroker,
  MVCFramework.Logger,
  IdHTTPWebBrokerBridge,
  WebModuleUnit1 in 'WebModuleUnit1.pas' {WebModule1: TWebModule} ,
  AppControllerU in 'AppControllerU.pas',
  MiddlewareSample1 in 'MiddlewareSample1.pas';

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
    LogI('Press RETURN to stop the server');
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
    WebRequestHandlerProc.MaxConnections := 1024;
    RunServer(8080);
  except
    on E: Exception do
      LogException(E, 'Shutting Down');
  end

end.
