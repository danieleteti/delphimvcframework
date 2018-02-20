program CallbackDemo;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Winapi.Windows,
  Winapi.ShellAPI,
  IdHTTPWebBrokerBridge,
  Web.WebReq,
  Web.WebBroker,
  MVCFramework,
  WebModuleUnit1 in 'WebModuleUnit1.pas' {WebModule1: TWebModule},
  CallbackControllerU in 'CallbackControllerU.pas',
  StompClient in '..\..\lib\delphistompclient\StompClient.pas',
  MVCFramework.MessagingController in '..\..\sources\MVCFramework.MessagingController.pas';

{$R *.res}

procedure RunServer(APort: Integer);
var
  LInputRecord: TInputRecord;
  LEvent      : DWord;
  LHandle     : THandle;
  LServer     : TIdHTTPWebBrokerBridge;
begin
  Writeln(Format('Starting HTTP Server or port %d', [APort]));
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;
    LServer.Active := True;
    Writeln('Press ESC to stop the server');
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
    EnterInShutdownState; // required for a "clean" stop of messaging extensions
  finally
    LServer.Free;
  end;
end;

begin
  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    RunServer(3000);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end

end.
