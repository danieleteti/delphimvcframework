unit UniDACConnectionConfigU;

interface

const
  CON_DEF_NAME = 'MyConnX';

procedure CreateFirebirdPrivateConnDef(AIsPooled: boolean);
procedure CreateInterbasePrivateConnDef(AIsPooled: boolean);
procedure CreateMySQLPrivateConnDef(AIsPooled: boolean);
procedure CreateMSSQLServerPrivateConnDef(AIsPooled: boolean);
procedure CreatePostgresqlPrivateConnDef(AIsPooled: boolean);
procedure CreateSqlitePrivateConnDef(AIsPooled: boolean);

implementation

uses
  System.Classes,
  System.IOUtils,
  UniProvider,
  DBAccess;

procedure CreateMySQLPrivateConnDef(AIsPooled: boolean);
var
  LParams: TStringList;
begin
  LParams := TStringList.Create;
  try
    LParams.Add('Database=activerecorddb');
    LParams.Add('Server=localhost');
    LParams.Add('Username=root');
    LParams.Add('Password=root');
    LParams.Add('SpecificOptions.MySQL.TinyIntFormat=Boolean');
    LParams.Add('CharSet=utf8mb4'); // not utf8!!
    if AIsPooled then
    begin
      LParams.Add('Pooling=True');
      LParams.Add('Pool.MaxPoolSize=100');
    end
    else
    begin
      LParams.Add('Pooling=False');
    end;
    UniProviderManager.GetProvider('MySQL').AddConnection(LParams, True, CON_DEF_NAME);
  finally
    LParams.Free;
  end;
end;

procedure CreateMSSQLServerPrivateConnDef(AIsPooled: boolean);
var
  LParams: TStringList;
begin
  LParams := TStringList.Create;
  try
    LParams.Add('Database=activerecorddb');
    LParams.Add('Server=localhost');
    LParams.Add('Username=sa');
    LParams.Add('Password=Daniele123!');
    if AIsPooled then
    begin
      LParams.Add('Pooling=True');
      LParams.Add('Pool.MaxPoolSize=100');
    end
    else
    begin
      LParams.Add('Pooling=False');
    end;
    UniProviderManager.GetProvider('SQLServer').AddConnection(LParams, True, CON_DEF_NAME);
  finally
    LParams.Free;
  end;
end;

procedure CreateFirebirdPrivateConnDef(AIsPooled: boolean);
var
  LParams: TStringList;
begin
  LParams := TStringList.Create;
  try
    LParams.Add('Database=' + TPath.GetFullPath(TPath.Combine('..\..',
      'data\ACTIVERECORDDB.FDB')));
    LParams.Add('Server=localhost');
    LParams.Add('Username=sysdba');
    LParams.Add('Password=masterkey');
    LParams.Add('CharSet=UTF8');
    if AIsPooled then
    begin
      LParams.Add('Pooling=True');
      LParams.Add('Pool.MaxPoolSize=100');
    end
    else
    begin
      LParams.Add('Pooling=False');
    end;
    UniProviderManager.GetProvider('Firebird').AddConnection(LParams, True, CON_DEF_NAME);
  finally
    LParams.Free;
  end;
end;

procedure CreateInterbasePrivateConnDef(AIsPooled: boolean);
var
  LParams: TStringList;
begin
  LParams := TStringList.Create;
  try
    LParams.Add('Database=' + TPath.GetFullPath(TPath.Combine('..\..',
      'data\ACTIVERECORDDB.IB')));
    LParams.Add('Server=localhost');
    LParams.Add('Username=sysdba');
    LParams.Add('Password=masterkey');
    LParams.Add('CharSet=UTF8');
    if AIsPooled then
    begin
      LParams.Add('Pooling=True');
      LParams.Add('Pool.MaxPoolSize=100');
    end
    else
    begin
      LParams.Add('Pooling=False');
    end;
    UniProviderManager.GetProvider('Interbase').AddConnection(LParams, True, CON_DEF_NAME);
  finally
    LParams.Free;
  end;
end;

procedure CreatePostgresqlPrivateConnDef(AIsPooled: boolean);
var
  LParams: TStringList;
begin
  LParams := TStringList.Create;
  try
    LParams.Add('Database=activerecorddb');
    LParams.Add('Server=localhost');
    LParams.Add('Username=postgres');
    LParams.Add('Password=postgres');
    if AIsPooled then
    begin
      LParams.Add('Pooling=True');
      LParams.Add('Pool.MaxPoolSize=100');
    end
    else
    begin
      LParams.Add('Pooling=False');
    end;
    UniProviderManager.GetProvider('PostgreSQL').AddConnection(LParams, True, CON_DEF_NAME);
  finally
    LParams.Free;
  end;
end;

procedure CreateSqlitePrivateConnDef(AIsPooled: boolean);
var
  LParams: TStringList;
  lFName: string;
begin
  LParams := TStringList.Create;
  try
    lFName := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)),
      '..\..\data\activerecorddb.db');
    LParams.Add('Database=' + lFName);
    if AIsPooled then
    begin
      LParams.Add('Pooling=True');
      LParams.Add('Pool.MaxPoolSize=100');
    end
    else
    begin
      LParams.Add('Pooling=False');
    end;
    UniProviderManager.GetProvider('SQLite').AddConnection(LParams, True, CON_DEF_NAME);
  finally
    LParams.Free;
  end;
end;

end.
