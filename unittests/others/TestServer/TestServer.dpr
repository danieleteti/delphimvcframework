program TestServer;

{$APPTYPE CONSOLE}


uses
  System.SysUtils,
  Winapi.Windows,
  IdHTTPWebBrokerBridge,
  Web.WebReq,
  Web.WebBroker,
  MVCFramework.Commons,
  WebModuleUnit in 'WebModuleUnit.pas' {bas: TWebModule},
  TestServerControllerU in 'TestServerControllerU.pas',
  TestServerControllerExceptionU in 'TestServerControllerExceptionU.pas',
  SpeedMiddlewareU in 'SpeedMiddlewareU.pas',
  TestServerControllerPrivateU in 'TestServerControllerPrivateU.pas',
  AuthHandlersU in 'AuthHandlersU.pas',
  BusinessObjectsU in '..\..\..\samples\commons\BusinessObjectsU.pas';

{$R *.res}


procedure Logo;
begin
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_RED or FOREGROUND_INTENSITY);
  WriteLn(' ██████╗ ███╗   ███╗██╗   ██╗ ██████╗    ███████╗███████╗██████╗ ██╗   ██╗███████╗██████╗');
  WriteLn(' ██╔══██╗████╗ ████║██║   ██║██╔════╝    ██╔════╝██╔════╝██╔══██╗██║   ██║██╔════╝██╔══██╗');
  WriteLn(' ██║  ██║██╔████╔██║██║   ██║██║         ███████╗█████╗  ██████╔╝██║   ██║█████╗  ██████╔╝');
  WriteLn(' ██║  ██║██║╚██╔╝██║╚██╗ ██╔╝██║         ╚════██║██╔══╝  ██╔══██╗╚██╗ ██╔╝██╔══╝  ██╔══██╗');
  WriteLn(' ██████╔╝██║ ╚═╝ ██║ ╚████╔╝ ╚██████╗    ███████║███████╗██║  ██║ ╚████╔╝ ███████╗██║  ██║');
  WriteLn(' ╚═════╝ ╚═╝     ╚═╝  ╚═══╝   ╚═════╝    ╚══════╝╚══════╝╚═╝  ╚═╝  ╚═══╝  ╚══════╝╚═╝  ╚═╝');
  WriteLn(' ');
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_INTENSITY);
  WriteLn('DMVCFRAMEWORK VERSION: ', DMVCFRAMEWORK_VERSION);
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_BLUE or FOREGROUND_GREEN or
    FOREGROUND_RED);
end;

procedure RunServer(APort: Integer);
var
  LInputRecord: TInputRecord;
  LEvent: DWord;
  LHandle: THandle;
  LServer: TIdHTTPWebBrokerBridge;
begin
  Logo;
  WriteLn(Format('Starting HTTP Server or port %d', [APort]));
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;
    LServer.Active := True;
    { more info about MaxConnections
      http://www.indyproject.org/docsite/html/frames.html?frmname=topic&frmfile=TIdCustomTCPServer_MaxConnections.html }
    LServer.MaxConnections := 0;
    { more info about ListenQueue
      http://www.indyproject.org/docsite/html/frames.html?frmname=topic&frmfile=TIdCustomTCPServer_ListenQueue.html }
    LServer.ListenQueue := 200;
    WriteLn('Press ESC to stop the server');
    LHandle := GetStdHandle(STD_INPUT_HANDLE);
    while True do
    begin
{$WARN SYMBOL_PLATFORM OFF}
      Win32Check(ReadConsoleInput(LHandle, LInputRecord, 1, LEvent));
{$WARN SYMBOL_PLATFORM ON}
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
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    RunServer(9999);
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end

end.
