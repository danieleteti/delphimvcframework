unit StreamedArrayWriterConfigU;

interface

const
  CON_DEF_NAME = 'STREAMED_ARRAY_WRITER_DEMO';
  DB_FILENAME = 'people.db';
  FEED_FILENAME = 'people_feed.csv';
  SEED_COUNT = 200000; // big enough that "materialise in RAM" is clearly wasteful

function GetPeopleFeedPath: string;
procedure RegisterSQLiteConnectionDef;
procedure SeedDataset;
procedure SeedFileFeed;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  FireDAC.Stan.Intf,
  FireDAC.Comp.Client,
  MVCFramework.Logger;

const
  FIRST: array [0 .. 9] of string = ('Alice', 'Bob', 'Carol', 'Daniel', 'Eve',
    'Frank', 'Grace', 'Hannah', 'Ivan', 'Julia');
  LAST: array [0 .. 9] of string = ('Bianchi', 'Rossi', 'Verdi', 'Neri',
    'Gialli', 'Bruni', 'Romano', 'Ricci', 'Greco', 'Conti');
  COUNTRIES: array [0 .. 5] of string = ('IT', 'FR', 'DE', 'ES', 'NL', 'PT');

function GetPeopleFeedPath: string;
begin
  Result := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), FEED_FILENAME);
end;

procedure RegisterSQLiteConnectionDef;
var
  LParams: TStringList;
  LDBPath: string;
begin
  LDBPath := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), DB_FILENAME);
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
  LTxn: Boolean;
  I: Integer;
begin
  LConn := TFDConnection.Create(nil);
  try
    LConn.ConnectionDefName := CON_DEF_NAME;
    LConn.Open;
    LConn.ExecSQL(
      'CREATE TABLE IF NOT EXISTS people (' +
      '  id        integer primary key autoincrement,' +
      '  full_name varchar(120) not null,' +
      '  country   varchar(2)   not null,' +
      '  age       integer      not null)');

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
      for I := 1 to SEED_COUNT do
        LConn.ExecSQL(
          'INSERT INTO people(full_name, country, age) VALUES(?, ?, ?)',
          [FIRST[Random(10)] + ' ' + LAST[Random(10)],
           COUNTRIES[Random(6)],
           18 + Random(62)]);
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

procedure SeedFileFeed;
var
  LWriter: TStreamWriter;
  LPath: string;
  I: Integer;
begin
  LPath := GetPeopleFeedPath;
  if TFile.Exists(LPath) then
  begin
    LogI('feed file already present (%s); skipping', [LPath]);
    Exit;
  end;
  LogI('Writing %d-line feed file %s ...', [SEED_COUNT, LPath]);
  LWriter := TStreamWriter.Create(LPath, False, TEncoding.UTF8);
  try
    for I := 1 to SEED_COUNT do
      LWriter.WriteLine(Format('%d;%s;%s;%d',
        [I, FIRST[Random(10)] + ' ' + LAST[Random(10)],
         COUNTRIES[Random(6)], 18 + Random(62)]));
  finally
    LWriter.Free;
  end;
  LogI('Feed file done: %d lines', [SEED_COUNT]);
end;

end.
