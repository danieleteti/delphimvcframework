program SSEChatServer;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MVCFramework,
  MVCFramework.Signal,
  MVCFramework.Logger,
  MVCFramework.Commons,
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}
  ReqMulti,
  Web.WebReq,
  Web.WebBroker,
  IdHTTPWebBrokerBridge,
  MVCFramework.SSE in '..\..\sources\MVCFramework.SSE.pas',
  ChatSSEControllerU in 'ChatSSEControllerU.pas',
  ChatApiControllerU in 'ChatApiControllerU.pas',
  ChatRoomU in 'ChatRoomU.pas',
  WebModuleU in 'WebModuleU.pas' {MyWebModule: TWebModule};

{$R *.res}

procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  LogI('** DMVCFramework SSE Chat Server ** build ' + DMVCFRAMEWORK_VERSION);
  LogI(Format('Starting HTTP Server on port %d', [APort]));
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.KeepAlive := True;
    LServer.DefaultPort := APort;
    LServer.MaxConnections := 0;
    LServer.ListenQueue := 200;
    LServer.Active := True;
    LogI('Server started. Press CTRL+C to stop.');
    WaitForTerminationSignal;
    EnterInShutdownState;
  finally
    LServer.Free;
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  IsMultiThread := True;
  UseConsoleLogger := True;
  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    WebRequestHandlerProc.MaxConnections := 1024;
    RunServer(8080);
  except
    on E: Exception do
      LogException(E, E.Message);
  end;
end.
