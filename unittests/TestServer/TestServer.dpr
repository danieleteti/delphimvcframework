program TestServer;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Winapi.Windows,
  IdHTTPWebBrokerBridge,
  Web.WebReq,
  Web.WebBroker,
  WebModuleUnit in 'WebModuleUnit.pas' {wm: TWebModule},
  MVCFramework.Commons in '..\..\sources\MVCFramework.Commons.pas',
  MVCFramework in '..\..\sources\MVCFramework.pas',
  MVCFramework.RESTClient in '..\..\sources\MVCFramework.RESTClient.pas',
  MVCFramework.Router in '..\..\sources\MVCFramework.Router.pas',
  MVCFramework.View.Cache in '..\..\sources\MVCFramework.View.Cache.pas',
  MVCFramework.View in '..\..\sources\MVCFramework.View.pas',
  TestServerControllerU in 'TestServerControllerU.pas',
  MVCFramework.ApplicationSession in '..\..\sources\MVCFramework.ApplicationSession.pas',
  MVCFramework.Session in '..\..\sources\MVCFramework.Session.pas',
  MVCFramework.Logger in '..\..\sources\MVCFramework.Logger.pas',
  StompClient in '..\..\lib\delphistompclient\StompClient.pas',
  StompTypes in '..\..\lib\delphistompclient\StompTypes.pas',
  RTTIUtilsU in '..\..\sources\RTTIUtilsU.pas',
  ObjectsMappers in '..\..\sources\ObjectsMappers.pas',
  DuckListU in '..\..\sources\DuckListU.pas',
  MVCFramework.BUSController in '..\..\sources\MVCFramework.BUSController.pas',
  BusinessObjectsU in '..\..\samples\commons\BusinessObjectsU.pas',
  TestServerControllerExceptionU in 'TestServerControllerExceptionU.pas',
  SpeedMiddlewareU in 'SpeedMiddlewareU.pas';

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
    RunServer(9999);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end

end.
