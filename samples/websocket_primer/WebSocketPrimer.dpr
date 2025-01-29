program WebSocketPrimer;

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
  MVCFramework.WebSocketServer,
  IdContext,
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  Winapi.ShellAPI,
  {$ENDIF }
  IdHTTPWebBrokerBridge,
  MainControllerU in 'MainControllerU.pas',
  WebModuleU in 'WebModuleU.pas' {MyWebModule: TWebModule},
  WebSocketHandlerU in 'WebSocketHandlerU.pas';

{$R *.res}

procedure RunServer(APort: Integer; AWSPort: Integer);
var
  lServer: TIdHTTPWebBrokerBridge;
  lWSHandler: TWebSocketHandler;
  lWebSocketServer: TMVCWebSocketServer;
begin
  Writeln('** DMVCFramework Server ** build ' + DMVCFRAMEWORK_VERSION);
  lServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    lServer.OnParseAuthentication := TMVCParseAuthentication.OnParseAuthentication;
    lServer.DefaultPort := APort;
    lServer.KeepAlive := True;

    { more info about MaxConnections
      http://ww2.indyproject.org/docsite/html/frames.html?frmname=topic&frmfile=index.html }
    lServer.MaxConnections := 0;

    { more info about ListenQueue
      http://ww2.indyproject.org/docsite/html/frames.html?frmname=topic&frmfile=index.html }
    lServer.ListenQueue := 200;
    lServer.Active := True;

    // WS Server
    lWebSocketServer := TMVCWebSocketServer.Create(AWSPort);
    try
      lWSHandler := TWebSocketHandler.Create;
      try
        lWebSocketServer.OnConnect := lWSHandler.Connect;
        lWebSocketServer.OnDisconnect := lWSHandler.Disconnect;
        lWebSocketServer.OnMessageArrived := lWSHandler.MessageArrived;
        lWebSocketServer.Active := True;
  {$IFDEF MSWINDOWS}
        { Comment the next line to avoid the default browser startup }
        ShellExecute(0, 'open', PChar('http://localhost:' + IntToStr(APort) + '/static'), nil, nil, SW_SHOWMAXIMIZED);
  {$ENDIF}
        Writeln('HTTP Server is listening on port ', APort);
        Writeln('WebSocket Server is running on port ', AWSPort);
        Write('CTRL+C to shutdown the server');
        WaitForTerminationSignal;
        EnterInShutdownState;
      finally
        lWSHandler.Free;
      end;
    finally
      lWebSocketServer.Free;
    end;
    lServer.Active := False;
  finally
    lServer.Free;
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  IsMultiThread := True;
  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    WebRequestHandlerProc.MaxConnections := 1024;
    RunServer(8080, 9090);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
