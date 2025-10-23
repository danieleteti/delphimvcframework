program WebSocketChatServer;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Web.WebReq,
  Web.WebBroker,
  IdHTTPWebBrokerBridge,
  MVCFramework.Signal,
  WebModuleU in 'WebModuleU.pas' {WebModule1: TWebModule},
  ChatControllerU in 'ChatControllerU.pas';

{$R *.res}

procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  Writeln('** Delphi MVC Framework WebSocket Chat Server **');
  Writeln(Format('Starting HTTP/WebSocket server on port %d', [APort]));

  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;
    LServer.KeepAlive := True;
    LServer.MaxConnections := 1000;

    LServer.Active := True;

    Writeln('Server started successfully!');
    Writeln('');
    Writeln('Available endpoints:');
    Writeln(Format('  WebSocket Chat:    ws://localhost:%d/chat', [APort]));
    Writeln(Format('  Chat Web Page:     http://localhost:%d/chat/page', [APort]));
    Writeln(Format('  Chat Statistics:   http://localhost:%d/chat/stats', [APort]));
    Writeln('');
    Writeln('Press Ctrl+C to stop the server');

    // Handle console signals
    WaitForTerminationSignal;

    Writeln('Shutting down server...');
  finally
    LServer.Free;
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  IsMultiThread := True;

  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;

    WebRequestHandlerProc.MaxConnections := 1024;

    RunServer(9090);
  except
    on E: Exception do
    begin
      Writeln('ERROR: ' + E.ClassName + ': ' + E.Message);
      ExitCode := 1;
    end;
  end;
end.
