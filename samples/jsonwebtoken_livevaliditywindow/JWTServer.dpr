program JWTServer;

{$APPTYPE CONSOLE}


uses
  System.SysUtils,
  Winapi.Windows,
  Winapi.ShellAPI,
  MVCFramework.Commons,
  Web.WebReq,
  Web.WebBroker,
  IdHTTPWebBrokerBridge,
  WebModuleUnit1 in 'WebModuleUnit1.pas' {WebModule1: TWebModule},
  AppControllerU in 'AppControllerU.pas',
  AuthenticationU in 'AuthenticationU.pas';

{$R *.res}


procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  Writeln(Format('Starting HTTP Server or port %d', [APort]));
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.OnParseAuthentication := TMVCParseAuthentication.OnParseAuthentication;
    LServer.DefaultPort := APort;
    LServer.Active := True;
    Writeln('THIS SERVER USES JWT WITH LIVEVALIDITYWINDOWS FEATURE');
    Writeln('Press RETURN to stop the server');
    // ShellExecute(0, 'open', PChar('http://localhost:' + IntToStr(APort)), nil, nil, SW_SHOW);
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
      Writeln(E.ClassName, ': ', E.Message);
  end

end.
