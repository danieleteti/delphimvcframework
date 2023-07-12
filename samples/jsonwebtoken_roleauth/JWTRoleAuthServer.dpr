program JWTRoleAuthServer;

{$APPTYPE CONSOLE}


uses
  System.SysUtils,
  Winapi.Windows,
  Winapi.ShellAPI,
  Web.WebReq,
  Web.WebBroker,
  IdHTTPWebBrokerBridge,
  MVCFramework,
  MVCFramework.Signal,
  MVCFramework.Commons,
  IdContext,
  WebModuleUnit1 in 'WebModuleUnit1.pas' {WebModule1: TWebModule},
  AppControllerU in 'AppControllerU.pas',
  AuthenticationU in 'AuthenticationU.pas';

{$R *.res}

procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  Writeln('Role Based Authentication and Authorization');
  Writeln(Format('Starting HTTP Server or port %d', [APort]));
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.OnParseAuthentication :=  TMVCParseAuthentication.OnParseAuthentication;
    LServer.DefaultPort := APort;
    LServer.Active := True;
    Writeln('CTRL+C to stop the server');
    WaitForTerminationSignal;
    EnterInShutdownState;
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
