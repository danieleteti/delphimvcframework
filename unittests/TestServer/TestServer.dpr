program TestServer;

{$APPTYPE CONSOLE}


uses
  System.SysUtils,
  Winapi.Windows,
  IdHTTPWebBrokerBridge,
  Web.WebReq,
  Web.WebBroker,
  WebModuleUnit in 'WebModuleUnit.pas' {bas: TWebModule},
  TestServerControllerU in 'TestServerControllerU.pas',
  BusinessObjectsU in '..\..\samples\commons\BusinessObjectsU.pas',
  TestServerControllerExceptionU in 'TestServerControllerExceptionU.pas',
  SpeedMiddlewareU in 'SpeedMiddlewareU.pas',
  MVCFramework in '..\..\sources\MVCFramework.pas',
  MVCFramework.RESTAdapter in '..\..\sources\MVCFramework.RESTAdapter.pas',
  MVCFramework.ApplicationSession in '..\..\sources\MVCFramework.ApplicationSession.pas',
  MVCFramework.Commons in '..\..\sources\MVCFramework.Commons.pas',
  MVCFramework.HMAC in '..\..\sources\MVCFramework.HMAC.pas',
  MVCFramework.JWT in '..\..\sources\MVCFramework.JWT.pas',
  MVCFramework.Logger in '..\..\sources\MVCFramework.Logger.pas',
  MVCFramework.MessagingController in '..\..\sources\MVCFramework.MessagingController.pas',
  MVCFramework.Middleware.Authentication in '..\..\sources\MVCFramework.Middleware.Authentication.pas',
  MVCFramework.Middleware.CORS in '..\..\sources\MVCFramework.Middleware.CORS.pas',
  MVCFramework.Middleware.JWT in '..\..\sources\MVCFramework.Middleware.JWT.pas',
  MVCFramework.Router in '..\..\sources\MVCFramework.Router.pas',
  MVCFramework.SysControllers in '..\..\sources\MVCFramework.SysControllers.pas',
  MVCFramework.Session in '..\..\sources\MVCFramework.Session.pas',
  MVCFramework.Server in '..\..\sources\MVCFramework.Server.pas',
  MVCFramework.Server.Impl in '..\..\sources\MVCFramework.Server.Impl.pas',
  MVCFramework.View.Cache in '..\..\sources\MVCFramework.View.Cache.pas',
  MVCFramework.View in '..\..\sources\MVCFramework.View.pas',
  RTTIUtilsU in '..\..\sources\RTTIUtilsU.pas',
  DuckListU in '..\..\sources\DuckListU.pas',
  TestServerControllerPrivateU in 'TestServerControllerPrivateU.pas',
  AuthHandlersU in 'AuthHandlersU.pas';

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
