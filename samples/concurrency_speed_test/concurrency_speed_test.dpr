program concurrency_speed_test;

{$APPTYPE CONSOLE}


uses
//  RDPMM64,
  System.SysUtils,
  MVCFramework,
  MVCFramework.Logger,
  MVCFramework.DotEnv,
  MVCFramework.Commons,
  MVCFramework.Signal,
  Web.ReqMulti,
  Web.WebReq,
  Web.WebBroker,
  IdHTTPWebBrokerBridge,
  MainControllerU in 'MainControllerU.pas',
  WebModuleU in 'WebModuleU.pas' {MyWebModule: TWebModule};

{$R *.res}


procedure RunServer(APort: Integer);
var
  lServer: TIdHTTPWebBrokerBridge;
begin
  Writeln('** DMVCFramework Server ** build ' + DMVCFRAMEWORK_VERSION);
  Writeln('Listening on port: ', APort);
  lServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    lServer.DefaultPort := APort;
    lServer.MaxConnections := dotEnv.Env('dmvc.webbroker.max_connections', 0);
    lServer.ListenQueue := dotEnv.Env('dmvc.indy.listen_queue', 500);
    lServer.Active := True;
    WriteLn('CTRL+C to shutdown the server');
    WaitForTerminationSignal;
    EnterInShutdownState;
  finally
    lServer.Free;
  end;
end;

begin
  //ReportMemoryLeaksOnShutdown := True;
  IsMultiThread := True;
  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;

    dotEnvConfigure(
      function : IMVCDotEnv
      begin
        Result := NewDotEnv
          .WithStrategy(TMVCDotEnvPriority.FileThenEnv)
                              //if available, by default, loads default environment (.env)
          .UseProfile('test') //if available loads the test environment (.env.test)
          .UseProfile('prod') //if available loads the prod environment (.env.prod)
          .Build();           //uses the executable folder to look for .env* files
      end);

    WebRequestHandlerProc.MaxConnections := dotEnv.Env('dmvc.handler.max_connections', 1024);
    RunServer(dotEnv.Env('dmvc.server.port', 9999));
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
