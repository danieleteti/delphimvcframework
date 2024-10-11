program CustomRoleAuthServer;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Web.WebReq,
  Web.WebBroker,
  IdHTTPWebBrokerBridge,
  MVCFramework,
  MVCFramework.Signal,
  MyWebModuleU in 'MyWebModuleU.pas' {MyWebModule: TWebModule},
  PrivateControllerU in 'PrivateControllerU.pas',
  MVCFramework.Middleware.Authentication.RoleBasedAuthHandler in '..\..\sources\MVCFramework.Middleware.Authentication.RoleBasedAuthHandler.pas',
  RoleAuthHandlerU in 'RoleAuthHandlerU.pas';

{$R *.res}

procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  Writeln('** DMVCFramework Server **');
  Writeln(Format('Starting HTTP Server on port %d', [APort]));
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;
    LServer.Active := True;
    // ShellExecute(0, 'open', pChar('http://localhost:' + inttostr(APort)), nil, nil, SW_SHOWMAXIMIZED);
    Writeln('CTRL+C to stop server');
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
  end;

end.
