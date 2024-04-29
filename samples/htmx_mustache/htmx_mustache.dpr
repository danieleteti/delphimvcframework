program htmx_mustache;

{$APPTYPE CONSOLE}


uses
  System.SysUtils,
  MVCFramework,
  MVCFramework.Signal,
  MVCFramework.Logger,
  MVCFramework.Console,
  {$IFDEF MSWINDOWS}
  Winapi.ShellAPI,
  Winapi.Windows,
  {$ENDIF }
  IdHTTPWebBrokerBridge,
  Web.WebReq,
  Web.WebBroker,
  WebModuleU in 'WebModuleU.pas' {WebModule1: TWebModule},
  WebSiteControllerU in 'WebSiteControllerU.pas',
  DAL in 'DAL.pas',
  MyDataModuleU in '..\renders\MyDataModuleU.pas' {MyDataModule: TDataModule},
  MVCFramework.HTMX in '..\htmx\MVCFramework.HTMX.pas', MVCFramework.Commons,
  MVCFramework.DotEnv;

{$R *.res}


procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  ReportMemoryLeaksOnShutdown := True;
  LogI('HTMX DMVCFramework Sample');
  LogI(Format('Starting HTTP Server on port %d', [APort]));
  ResetConsole;
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;
    LServer.Active := True;
    {$IFDEF MSWINDOWS}
    //ShellExecute(0, 'open', 'http://localhost:8080', nil, nil, SW_SHOW);
    {$ENDIF}
    LogI('HTMX DMVCFramework Sample');
    LogI('Ctrl+C to stop the server');
    WaitForTerminationSignal;
    EnterInShutdownState;
    ResetConsole;
    LServer.Active := False;
  finally
    LServer.Free;
  end;
end;

begin
  { Enable ReportMemoryLeaksOnShutdown during debug }
  // ReportMemoryLeaksOnShutdown := True;
  IsMultiThread := True;
  UseConsoleLogger := True;
  // DMVCFramework Specific Configuration
  // When MVCSerializeNulls = True empty nullables and nil are serialized as json null.
  // When MVCSerializeNulls = False empty nullables and nil are not serialized at all.
  MVCSerializeNulls := True;

  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    WebRequestHandlerProc.MaxConnections := dotEnv.Env('dmvc.handler.max_connections', 1024);
    RunServer(dotEnv.Env('dmvc.server.port', 8080));
  except
    on E: Exception do
      LogE(E.ClassName +  ': ' + E.Message);
  end;

end.
