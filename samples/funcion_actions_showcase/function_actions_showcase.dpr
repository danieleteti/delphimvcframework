program function_actions_showcase;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MVCFramework,
  MVCFramework.Logger,
  MVCFramework.DotEnv,
  MVCFramework.Commons,
  MVCFramework.Signal,
  Web.ReqMulti,
  Web.WebReq,
  Web.WebBroker,
  IdContext,
  IdHTTPWebBrokerBridge,
  ControllerU in 'ControllerU.pas',
  WebModuleU in 'WebModuleU.pas' {MyWebModule: TWebModule},
  MainDMU in 'MainDMU.pas' {dmMain: TDataModule};

{$R *.res}


procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  Writeln('** DMVCFramework Server ** build ' + DMVCFRAMEWORK_VERSION);
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.OnParseAuthentication := TMVCParseAuthentication.OnParseAuthentication;
    LServer.DefaultPort := APort;
    LServer.KeepAlive := True;
    LServer.MaxConnections := dotEnv.Env('dmvc.webbroker.max_connections', 0);
    LServer.ListenQueue := dotEnv.Env('dmvc.indy.listen_queue', 500);

    LServer.Active := True;
    WriteLn('Listening on port ', APort);
    Write('CTRL+C to shutdown the server');
    WaitForTerminationSignal;
    EnterInShutdownState;
    LServer.Active := False;
  finally
    LServer.Free;
  end;
end;

begin
  { Enable ReportMemoryLeaksOnShutdown during debug }
  // ReportMemoryLeaksOnShutdown := True;
  IsMultiThread := True;

  // DMVCFramework Specific Configuration
  // When MVCSerializeNulls = True empty nullables and nil are serialized as json null.
  // When MVCSerializeNulls = False empty nullables and nil are not serialized at all.
  MVCSerializeNulls := True;

  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;

    dotEnvConfigure(
      function: IMVCDotEnv
      begin
        Result := NewDotEnv
                 .WithStrategy(TMVCDotEnvPriority.FileThenEnv)
                                       //if available, by default, loads default environment (.env)
                 .UseProfile('test') //if available loads the test environment (.env.test)
                 .UseProfile('prod') //if available loads the prod environment (.env.prod)
                 .UseLogger(procedure(LogItem: String)
                            begin
                              LogW('dotEnv: ' + LogItem);
                            end)
                 .Build();             //uses the executable folder to look for .env* files
      end);

    WebRequestHandlerProc.MaxConnections := dotEnv.Env('dmvc.handler.max_connections', 1024);
    RunServer(dotEnv.Env('dmvc.server.port', 8080));
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
