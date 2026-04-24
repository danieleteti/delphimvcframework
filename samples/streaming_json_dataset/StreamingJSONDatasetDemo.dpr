program StreamingJSONDatasetDemo;

// ===========================================================================
//  Minimal demo: transparent streaming of a large JSON array via the 3.5.0-
//  silicon streaming serializer. Two functional actions, zero boilerplate.
//
//  Server: Indy Direct, port 8990.
//  DB:     SQLite (people.db), populated at startup with 200k rows.
//
//  Routes:
//    GET /people/dataset       return TDataSet -> raw [...] JSON array
//    GET /people/okresponse    OKResponse(TDataSet) -> {"data":[...]}
//
//  Verify with:
//     curl -s http://localhost:8990/people/dataset > out.json
//     curl -s http://localhost:8990/people/okresponse > out.json
//
//  Both responses are valid JSON. Peak DB-side RAM stays bounded to
//  RowsetSize rows (SelectUnidirectionalDataSet => fmOnDemand), peak
//  framework-side RAM = size of the final JSON payload (no DOM, no
//  intermediate string).
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
  ControllerU in 'ControllerU.pas';

const
  CON_DEF_NAME = 'STREAMING_JSON_DEMO';
  DB_FILENAME = 'people.db';
  PORT = 8990;
  SEED_COUNT = 200000;  // 200k rows - big enough that "materialise in RAM" is expensive

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
    LParams.Add('LockingMode=Normal');
    FDManager.AddConnectionDef(CON_DEF_NAME, 'SQLite', LParams);
  finally
    LParams.Free;
  end;
  LogI('SQLite database: ' + LDBPath);
end;

procedure SeedDataset;
var
  LConn: TFDConnection;
  LCount: Integer;
  I: Integer;
  LTxn: Boolean;
const
  FIRST: array [0 .. 9] of string = ('Alice', 'Bob', 'Carol', 'Daniel', 'Eve',
    'Frank', 'Grace', 'Hannah', 'Ivan', 'Julia');
  LAST: array [0 .. 9] of string = ('Bianchi', 'Rossi', 'Verdi', 'Neri', 'Gialli',
    'Bruni', 'Romano', 'Ricci', 'Greco', 'Conti');
  COUNTRIES: array [0 .. 5] of string = ('IT', 'FR', 'DE', 'ES', 'NL', 'PT');
begin
  LConn := TFDConnection.Create(nil);
  try
    LConn.ConnectionDefName := CON_DEF_NAME;
    LConn.Open;
    LConn.ExecSQL(
      'CREATE TABLE IF NOT EXISTS people (' +
      '  id         integer primary key autoincrement,' +
      '  full_name  varchar(120) not null,' +
      '  email      varchar(200) not null,' +
      '  country    varchar(2)   not null,' +
      '  age        integer      not null,' +
      '  joined_at  varchar(20)  not null)');

    LCount := LConn.ExecSQLScalar('SELECT COUNT(*) FROM people');
    if LCount >= SEED_COUNT then
    begin
      LogI('people table already seeded (%d rows); skipping', [LCount]);
      Exit;
    end;

    LogI('Seeding %d rows into people ...', [SEED_COUNT]);
    LTxn := False;
    try
      LConn.StartTransaction;
      LTxn := True;
      // Single parameterised prepared insert reused across the loop keeps
      // seeding time reasonable (a few seconds instead of minutes).
      for I := 1 to SEED_COUNT do
      begin
        LConn.ExecSQL(
          'INSERT INTO people(full_name, email, country, age, joined_at) ' +
          'VALUES(?, ?, ?, ?, ?)',
          [FIRST[Random(10)] + ' ' + LAST[Random(10)],
           Format('user%d@example.com', [I]),
           COUNTRIES[Random(6)],
           18 + Random(62),
           FormatDateTime('yyyy-mm-dd', Now - Random(3000))]);
      end;
      LConn.Commit;
      LTxn := False;
    except
      if LTxn then
        LConn.Rollback;
      raise;
    end;
    LogI('Seeding done: %d rows', [SEED_COUNT]);
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

    LogI('Server listening on http://localhost:%d', [PORT]);
    LogI('GET /people/dataset       - return TDataSet -> streaming serializer');
    LogI('GET /people/okresponse    - OKResponse(TDataSet) -> streaming serializer');
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
    LogI('** DMVCFramework Streaming JSON Dataset Demo **');
    RegisterSQLiteConnectionDef;
    SeedDataset;
    RunServer;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
