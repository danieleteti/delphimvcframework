program jsonwriterrenders;

 {$APPTYPE CONSOLE}

uses
  MVCFramework,
  MVCFramework.Signal,
  System.SysUtils,
  Winapi.Windows,
  Winapi.ShellAPI,
  Web.WebReq,
  Web.WebBroker,
  IdHTTPWebBrokerBridge,
  JSONSampleController in 'JSONSampleController.pas',
  WebModuleU in 'WebModuleU.pas' {MyWebModule: TWebModule};

{$R *.res}

procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  Writeln('** DMVCFramework Server **');
  Writeln(Format('Starting HTTP Server on port %d', [APort]));
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;
    LServer.Active := True;
    ShellExecute(0, 'open', pChar('http://localhost:' + inttostr(APort)), nil, nil, SW_SHOWMAXIMIZED);
    Writeln('CTRL+C to stop the server');
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
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
