program IOCPBasicDemo;

{$APPTYPE CONSOLE}


uses
  System.SysUtils,
  Winapi.Windows,
  Winapi.ShellAPI,
  Web.WebReq,
  Web.WebBroker,
  WebModuleUnit1 in 'WebModuleUnit1.pas' {WebModule1: TWebModule} ,
  App1MainControllerU in 'App1MainControllerU.pas',
{$IFDEF IOCP}Iocp.DSHTTPWebBroker {$ELSE} IdHTTPWebBrokerBridge{$ENDIF};

{$R *.res}


procedure RunServer(APort: Integer);
var
  LInputRecord: TInputRecord;
  LEvent: DWord;
  LHandle: THandle;
  LServer: {$IFDEF IOCP}TIocpWebBrokerBridge{$ELSE}TIdHTTPWebBrokerBridge{$ENDIF};
begin
  Writeln(Format('Starting HTTP Server or port %d', [APort]));
  Writeln(
{$IFDEF IOCP}
    'IOCP Version'
{$ELSE}
    'INDY Version'
{$ENDIF}
    );
  LServer := {$IFDEF IOCP}TIocpWebBrokerBridge{$ELSE}TIdHTTPWebBrokerBridge{$ENDIF}.Create(nil);
  try
{$IFDEF IOCP}
    LServer.Port := APort;
{$ELSE}
    LServer.DefaultPort := APort;
{$ENDIF}
    LServer.Active := True;
    ShellExecute(0, 'open', pChar('http://localhost:' + inttostr(APort) +
      '/div/10/20'), nil, nil,
      SW_SHOWMAXIMIZED);
    Writeln('Press ESC to stop the server');
    LHandle := GetStdHandle(STD_INPUT_HANDLE);
    while True do
    begin
      Win32Check(ReadConsoleInput(LHandle, LInputRecord, 1, LEvent));
      if (LInputRecord.EventType = KEY_EVENT) and
        LInputRecord.Event.KeyEvent.bKeyDown and
        (LInputRecord.Event.KeyEvent.wVirtualKeyCode = VK_ESCAPE) then
        break;
    end;
  finally
    LServer.Free;
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  try
    // WebRequestHandlerProc := Iocp.DSHTTPWebBroker.
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    WebRequestHandlerProc.MaxConnections := 1024;
    RunServer(3000);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end

end.
