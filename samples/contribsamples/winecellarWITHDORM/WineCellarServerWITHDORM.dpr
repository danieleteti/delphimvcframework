program WineCellarServerWITHDORM;

{$APPTYPE CONSOLE}


uses
  WinApi.ShellAPI,
  System.SysUtils,
  WinApi.Windows,
  Web.WebReq,
  Web.WebBroker,
  MainWebModuleUnit in 'MainWebModuleUnit.pas' {wm: TWebModule},
  WineCellarAppControllerU in 'WineCellarAppControllerU.pas',
  WinesBO in 'WinesBO.pas',
  IdHTTPWebBrokerBridge;

{$R *.res}


procedure RunServer(APort: Integer);
var
  LInputRecord: TInputRecord;
  LEvent: DWord;
  LHandle: THandle;
  LServer: TIdHTTPWebBrokerBridge;
begin
  Writeln(Format('Starting HTTP Server or port %d', [APort]));
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;
    LServer.Active := True;
    Writeln('Press ESC to stop the server');
    // Just to start the WEB client
    ShellExecute(0, 'open', 'http://localhost:3000', nil, nil, SW_SHOW);
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
  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    WebRequestHandlerProc.MaxConnections := 1024; // daniele teti
    RunServer(3000);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end

end.
