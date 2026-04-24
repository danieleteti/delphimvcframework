program ValidationVsStorageDemo;

// ===========================================================================
//  Minimal demo: two validation layers on one POST endpoint.
//
//  Server: Indy Direct, port 8989.
//  DB:     SQLite (people.db in the exe folder, created on first run).
//  Route:  POST /people
//
//  See test_01_*.bat / test_02_*.bat / test_03_*.bat for the three scenarios.
// ===========================================================================

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  FireDAC.Phys.SQLite,
  FireDAC.Stan.Def,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.Comp.Client,
  FireDAC.DApt,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Signal,
  MVCFramework.Logger,
  MVCFramework.Server.Intf,
  MVCFramework.Server.Indy,
  MVCFramework.Server.Factory,
  MVCFramework.ActiveRecord,
  MVCFramework.SQLGenerators.Sqlite,
  MVCFramework.Middleware.ActiveRecord,
  ModelsU in 'ModelsU.pas',
  ControllerU in 'ControllerU.pas';

const
  CON_DEF_NAME = 'VALIDATION_VS_STORAGE_DEMO';
  DB_FILENAME = 'people.db';
  PORT = 8989;

procedure RegisterSQLiteConnectionDef;
var
  LParams: TStringList;
  LDBPath: string;
begin
  LDBPath := TPath.Combine(
    TPath.GetDirectoryName(ParamStr(0)), DB_FILENAME);
  LParams := TStringList.Create;
  try
    LParams.Add('Database=' + LDBPath);
    LParams.Add('StringFormat=Unicode');
    LParams.Add('Pooled=True');
    LParams.Add('POOL_MaximumItems=50');
    FDManager.AddConnectionDef(CON_DEF_NAME, 'SQLite', LParams);
  finally
    LParams.Free;
  end;
  LogI('SQLite database: ' + LDBPath);
end;

procedure CreateSchema;
var
  LConn: TFDConnection;
begin
  LConn := TFDConnection.Create(nil);
  try
    LConn.ConnectionDefName := CON_DEF_NAME;
    LConn.Open;
    LConn.ExecSQL(
      'CREATE TABLE IF NOT EXISTS people (' +
      '  id    integer primary key autoincrement,' +
      '  name  varchar(100),' +
      '  email varchar(100)' +
      ')');
  finally
    LConn.Free;
  end;
end;

procedure RunServer;
var
  LEngine: TMVCEngine;
  LServer: IMVCServer;
begin
  LEngine := TMVCEngine.Create(
    procedure(AConfig: TMVCConfig)
    begin
      AConfig[TMVCConfigKey.DefaultContentType] := TMVCConstants.DEFAULT_CONTENT_TYPE;
      AConfig[TMVCConfigKey.DefaultContentCharset] := TMVCConstants.DEFAULT_CONTENT_CHARSET;
      AConfig[TMVCConfigKey.ExposeServerSignature] := 'false';
    end);
  try
    LEngine.AddController(TPeopleController);
    LEngine.AddMiddleware(TMVCActiveRecordMiddleware.Create(CON_DEF_NAME));

    LServer := TMVCServerFactory.CreateIndyDirect(LEngine);
    LServer.Listen(PORT);

    LogI(Format('Server listening on http://localhost:%d', [PORT]));
    LogI('POST /people  { "name": "...", "email": "..." }');
    LogI('CTRL+C to shutdown.');

    WaitForTerminationSignal;
    EnterInShutdownState;
    LServer.Stop;
    LServer := nil;
  finally
    LEngine.Free;
  end;
end;

begin
  IsMultiThread := True;
  try
    LogI('** DMVCFramework Validation vs StorageValidation Demo **');
    RegisterSQLiteConnectionDef;
    CreateSchema;
    RunServer;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
