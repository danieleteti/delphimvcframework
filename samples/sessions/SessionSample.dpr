program SessionSample;

{$APPTYPE CONSOLE}


uses
  System.SysUtils,
  MVCFramework,
  MVCFramework.Signal,
  MVCFramework.Logger,
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  Winapi.ShellAPI,
  {$ENDIF}
  Web.WebReq,
  Web.WebBroker,
  IdHTTPWebBrokerBridge,
  WebModuleUnit1 in 'WebModuleUnit1.pas' {WebModule1: TWebModule} ,
  AppControllerU in 'AppControllerU.pas';

{$R *.res}


procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  LogI(Format('Starting HTTP Server or port %d', [APort]));
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;
    LServer.Active := True;
    {$IFDEF MSWINDOWS}
    ShellExecute(0, 'open', PChar('http://localhost:' + IntToStr(APort) + '/login/john'), nil, nil, SW_SHOW);
    {$ENDIF}
    LogI('CTRL+C to stop the server');
    WaitForTerminationSignal;
    EnterInShutdownState;
  finally
    LServer.Free;
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    WebRequestHandlerProc.MaxConnections := 1024;
    RunServer(8080);
  except
    on E: Exception do
      LogI(E.ClassName + ': ' + E.Message);
  end

end.
