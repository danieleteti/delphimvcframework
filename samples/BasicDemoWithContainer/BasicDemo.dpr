program BasicDemo;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Winapi.Windows,
  Winapi.ShellAPI,
  Web.WebReq,
  Web.WebBroker,
  MVCFramework.Server,
  WebModuleUnit1 in 'WebModuleUnit1.pas' {WebModule1: TWebModule} ,
  App1MainControllerU in 'App1MainControllerU.pas';

{$R *.res}

procedure RunServer(APort: Integer);
var
  LServerInfo: IMVCServerInfo;
  LInputRecord: TInputRecord;
  LEvent: DWord;
  LHandle: THandle;
begin
  Writeln(Format('Starting HTTP Server or port %d', [APort]));

  LServerInfo := TMVCServerInfoFactory.Build;
  LServerInfo.ServerName := 'BasicDemo';
  LServerInfo.Port := APort;
  LServerInfo.MaxConnections := 1024;
  LServerInfo.WebModuleClass := WebModuleClass;

  MVCServerDefault.Container.CreateServer(LServerInfo);
  MVCServerDefault.Container.StartServers;

  ShellExecute(0, 'open', pChar('http://localhost:' + inttostr(APort) +
    '/div/10/20'), nil, nil, SW_SHOWMAXIMIZED);
  Writeln('Press ESC to stop the server');
  LHandle := GetStdHandle(STD_INPUT_HANDLE);
  while True do
  begin
    Win32Check(ReadConsoleInput(LHandle, LInputRecord, 1, LEvent));
    if (LInputRecord.EventType = KEY_EVENT) and
      LInputRecord.Event.KeyEvent.bKeyDown and
      (LInputRecord.Event.KeyEvent.wVirtualKeyCode = VK_ESCAPE) then
      Break;
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  try
    RunServer(3000);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
