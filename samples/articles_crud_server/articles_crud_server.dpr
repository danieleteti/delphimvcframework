program articles_crud_server;

{$APPTYPE CONSOLE}
{$DEFINE USE_DB_LOGGER}

uses
  System.SysUtils,
  IdHTTPWebBrokerBridge,
  {$IF Defined(USE_DB_LOGGER)}
  CustomLoggerConfigU,
  {$ENDIF }
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Signal,
  MVCFramework.Logger,
  MVCFramework.Container,
  MVCFramework.dotEnv,
  Web.WebReq,
  Web.WebBroker,
  WebModuleUnit1 in 'WebModuleUnit1.pas' {WebModule1: TWebModule},
  Controllers.Base in 'Controllers.Base.pas',
  Controllers.Articles in 'Controllers.Articles.pas',
  Services in 'Services.pas',
  BusinessObjects in 'BusinessObjects.pas',
  Commons in 'Commons.pas',
  FDConnectionConfigU in 'FDConnectionConfigU.pas';

{$R *.res}


procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  LogI('** DMVCFramework Server ** build ' + DMVCFRAMEWORK_VERSION);
  LogW('ARTICLES CRUD Sample. Use articles_crud_vcl_client.dproj to manage data');
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.OnParseAuthentication := TMVCParseAuthentication.OnParseAuthentication;
    LServer.DefaultPort := APort;
    LServer.KeepAlive := True;
    LServer.MaxConnections := dotEnv.Env('dmvc.webbroker.max_connections', 0);
    LServer.ListenQueue := dotEnv.Env('dmvc.indy.listen_queue', 500);
    LServer.Active := True;
    LogI('Listening on port ' + APort.ToString);
    LogI('CTRL+C to shutdown the server');
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

    {$IF Defined(USE_DB_LOGGER)}
      MVCFramework.Logger.SetDefaultLogger(CustomLoggerConfigU.GetLogger);
    {$ENDIF}

    CreateFirebirdPrivateConnDef(True);
    DefaultMVCServiceContainer
      .RegisterType(TArticlesService, IArticlesService, TRegistrationType.SingletonPerRequest)
      .Build;

    WebRequestHandlerProc.MaxConnections := dotEnv.Env('dmvc.handler.max_connections', 1024);
    RunServer(dotEnv.Env('dmvc.server.port', 8080));
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end

end.
