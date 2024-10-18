program activerecord_restful_crud;

{$APPTYPE CONSOLE}

uses
  FireDAC.Phys.FB,
  FireDAC.Phys.PG,
  FireDAC.Phys.MySQL,
  FireDAC.Phys.SQLite,
  System.SysUtils,
  MVCFramework,
  MVCFramework.Commons,
  Web.ReqMulti,
  Web.WebReq,
  Web.WebBroker,
  IdHTTPWebBrokerBridge,
  MVCFramework.Signal,
  MVCFramework.Logger,
  MVCFramework.SQLGenerators.PostgreSQL,
  MVCFramework.SQLGenerators.Firebird,
  MVCFramework.SQLGenerators.Interbase,
  MVCFramework.SQLGenerators.MSSQL,
  MVCFramework.SQLGenerators.MySQL,
  WebModuleU in 'WebModuleU.pas' {MyWebModule: TWebModule},
  Entities in 'Entities.pas',
  EntitiesProcessors in 'EntitiesProcessors.pas',
  FDConnectionConfigU in '..\activerecord_showcase\FDConnectionConfigU.pas',
  OtherControllerU in 'OtherControllerU.pas';

{$R *.res}

procedure RunServer(APort: Integer);
var
  lServer: TIdHTTPWebBrokerBridge;
  lCmd: string;
begin
  LogI('** DMVCFramework Server ** build ' + DMVCFRAMEWORK_VERSION);
  if ParamCount >= 1 then
    lCmd := ParamStr(1)
  else
    lCmd := '/postgresql';

  if (lCmd = '/firebird') then
  begin
    CreateFirebirdPrivateConnDef(True);
  end
  else if (lCmd = '/mysql') then
  begin
    CreateMySQLPrivateConnDef(True);
  end
  else
  begin
    lCmd := '/postgresql';
    CreatePostgreSQLPrivateConnDef(True);
  end;

  LogI('Using ' + lCmd.Substring(1));

  lServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    lServer.KeepAlive := True;
    lServer.DefaultPort := APort;
    lServer.MaxConnections := 0;
    lServer.ListenQueue := 200;
    lServer.Active := True;
    LogI('Running on port ' + APort.ToString);
    LogI('CTRL+C to Quit');
    WaitForTerminationSignal;
    EnterInShutdownState;
  finally
    lServer.Free;
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  IsMultiThread := True;
  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    WebRequestHandlerProc.MaxConnections := 1024;
    RunServer(dotEnv.Env('dmvc.server.port', 8080));
  except
    on E: Exception do
      LogE(E.ClassName + ': ' + E.Message);
  end;

end.
