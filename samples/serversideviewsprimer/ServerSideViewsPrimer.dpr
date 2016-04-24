program ServerSideViewsPrimer;
{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Winapi.ShellAPI,
  Winapi.Windows,
  IdHTTPWebBrokerBridge,
  Web.WebReq,
  Web.WebBroker,
  WebModuleU in 'WebModuleU.pas' {WebModule1: TWebModule} ,
  WebSiteControllerU in 'WebSiteControllerU.pas',
  DAL in 'DAL.pas';

{$R *.res}

procedure RunServer(APort: Integer);
var
  LInputRecord: TInputRecord;
  LEvent: DWord;
  LHandle: THandle;
  LServer: TIdHTTPWebBrokerBridge;
begin
  ReportMemoryLeaksOnShutdown := True;
  Writeln(Format('Starting HTTP Server on port %d', [APort]));
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;
    LServer.Active := True;
    Writeln('Press ESC to stop the server');
    ShellExecute(0, 'open', 'http://localhost:8080', nil, nil, SW_SHOW);
    LHandle := GetStdHandle(STD_INPUT_HANDLE);
    while True do
    begin
      ReadConsoleInput(LHandle, LInputRecord, 1, LEvent);
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
    RunServer(8080);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end

end.
