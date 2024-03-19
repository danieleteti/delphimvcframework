program BasicDemo;

{$APPTYPE CONSOLE}


uses
  System.SysUtils,
  Web.WebReq,
  Web.WebBroker,
  IdHTTPWebBrokerBridge,
  MVCFramework,
  MVCFramework.Signal,
  MVCFramework.Logger,
  WebModuleUnit1 in 'WebModuleUnit1.pas' {WebModule1: TWebModule},
  App1MainControllerU in 'App1MainControllerU.pas',
  MVCFramework.Middleware.StaticFiles in '..\..\sources\MVCFramework.Middleware.StaticFiles.pas';

{$R *.res}


procedure RunServer(APort: Integer);
var
  lServer: TIdHTTPWebBrokerBridge;
begin
  // Writeln(Format('Starting HTTP Server or port %d', [APort]));
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;
    LogI(Format('Server started on port %d', [APort]));

    { more info about MaxConnections
      http://www.indyproject.org/docsite/html/frames.html?frmname=topic&frmfile=TIdCustomTCPServer_MaxConnections.html }
    LServer.MaxConnections := 0;

    { more info about ListenQueue
      http://www.indyproject.org/docsite/html/frames.html?frmname=topic&frmfile=TIdCustomTCPServer_ListenQueue.html }
    LServer.ListenQueue := 200;
    LServer.Active := True;
    WriteLn('CTRL+C to shutdown the server');
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
