program WebSocketServerU;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MVCFramework,
  MVCFramework.Logger,
  MVCFramework.Commons,
  MVCFramework.Signal,
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  Winapi.ShellAPI,
  {$ENDIF }
  Web.WebReq,
  Web.WebBroker,
  IdContext,
  IdHTTPWebBrokerBridge,
  WebSocketWebModuleU in 'WebSocketWebModuleU.pas' {WebSocketWebModule: TWebModule},
  WebSocketControllerU in 'WebSocketControllerU.pas';

{$R *.res}

procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  Writeln('** DelphiMVCFramework WebSocket Sample **');
  Writeln(Format('Starting HTTP Server on port %d', [APort]));

  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;
    LServer.KeepAlive := True;
    LServer.MaxConnections := 1024;

    LServer.Active := True;

    Writeln('Server is running on port ' + IntToStr(LServer.DefaultPort));
    Writeln('');
    Writeln('WebSocket Endpoints:');
    Writeln('  ws://localhost:' + IntToStr(APort) + '/ws/echo  - Echo server');
    Writeln('  ws://localhost:' + IntToStr(APort) + '/ws/chat  - Chat room');
    Writeln('');
    Writeln('Press Ctrl+C to stop the server');

    WaitForTerminationSignal;

    Writeln('Shutting down...');
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

    RunServer(8080);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
