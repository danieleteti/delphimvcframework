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
  MVCFramework.SQLGenerators.PostgreSQL,
  MVCFramework.SQLGenerators.Firebird,
  MVCFramework.SQLGenerators.Interbase,
  MVCFramework.SQLGenerators.MSSQL,
  MVCFramework.SQLGenerators.MySQL,
  WebModuleU in 'WebModuleU.pas' {MyWebModule: TWebModule} ,
  Entities in 'Entities.pas',
  MVCFramework.ActiveRecordController in '..\..\sources\MVCFramework.ActiveRecordController.pas',
  MVCFramework.ActiveRecord in '..\..\sources\MVCFramework.ActiveRecord.pas',
  EntitiesProcessors in 'EntitiesProcessors.pas',
  FDConnectionConfigU in '..\activerecord_showcase\FDConnectionConfigU.pas',
  OtherControllerU in 'OtherControllerU.pas',
  MVCFramework.SysControllers in '..\..\sources\MVCFramework.SysControllers.pas';

{$R *.res}

procedure RunServer(APort: Integer);
var
  lServer: TIdHTTPWebBrokerBridge;
  lCmd: string;
begin
  ConnectionDefinitionName := CON_DEF_NAME;
  Writeln('** DMVCFramework Server ** build ' + DMVCFRAMEWORK_VERSION);
  if ParamCount >= 1 then
    lCmd := ParamStr(1)
  else
    lCmd := '/firebird';

  if (lCmd = '/firebird') then
  begin
    CreateFirebirdPrivateConnDef(True);
  end
  else if (lCmd = '/mysql') then
  begin
    CreateMySQLPrivateConnDef(True);
  end
  else if (lCmd = '/postgresql') then
  begin
    CreatePostgreSQLPrivateConnDef(True);
  end
  else
  begin
    CreateFirebirdPrivateConnDef(True);
  end;

  lServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    lServer.KeepAlive := True;
    lServer.DefaultPort := APort;
    lServer.MaxConnections := 0;
    lServer.ListenQueue := 200;
    lServer.Active := True;
    WriteLn('Running on port ', APort);
    Write('CTRL+C to Quit');
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
    RunServer(8080);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
