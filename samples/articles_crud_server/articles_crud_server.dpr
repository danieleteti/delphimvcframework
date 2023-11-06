program articles_crud_server;

{$APPTYPE CONSOLE}


uses
  System.SysUtils,
  IdHTTPWebBrokerBridge,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Signal,
  MVCFramework.Logger,
  MVCFramework.dotEnv,
  Web.WebReq,
  Web.WebBroker,
  WebModuleUnit1 in 'WebModuleUnit1.pas' {WebModule1: TWebModule},
  Controllers.Base in 'Controllers.Base.pas',
  Controllers.Articles in 'Controllers.Articles.pas',
  Services in 'Services.pas',
  BusinessObjects in 'BusinessObjects.pas',
  MainDM in 'MainDM.pas' {dmMain: TDataModule},
  Commons in 'Commons.pas',
  MVCFramework.ActiveRecord in '..\..\sources\MVCFramework.ActiveRecord.pas',
  MVCFramework.Serializer.JsonDataObjects in '..\..\sources\MVCFramework.Serializer.JsonDataObjects.pas';

{$R *.res}


procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  WriteLn('ARTICLES CRUD Sample. Use articles_crud_vcl_client.dproj to manage data');
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
  ReportMemoryLeaksOnShutdown := True;
  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;

    dotEnvConfigure(
      function: IMVCDotEnv
      begin
        Result := NewDotEnv
                   .UseStrategy(TMVCDotEnvPriority.FileThenEnv)
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
      WriteLn(E.ClassName, ': ', E.Message);
  end

end.
