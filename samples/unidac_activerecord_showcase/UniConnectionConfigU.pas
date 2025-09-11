unit UniConnectionConfigU;

interface

uses
  Uni,
  DBAccess;

const
  CON_DEF_NAME = 'MyConnX';

procedure CreateFirebirdPrivateConn(AConnection: TUniConnection; AIsPooled: boolean);
procedure CreateInterbasePrivateConn(AConnection: TUniConnection; AIsPooled: boolean);
procedure CreateMySQLPrivateConn(AConnection: TUniConnection; AIsPooled: boolean);
procedure CreateMSSQLServerPrivateConn(AConnection: TUniConnection; AIsPooled: boolean);
procedure CreatePostgresqlPrivateConn(AConnection: TUniConnection; AIsPooled: boolean);
procedure CreateSqlitePrivateConn(AConnection: TUniConnection; AIsPooled: boolean);

implementation

uses
  System.Classes,
  System.IOUtils,
  SysUtils;

procedure CreateMySQLPrivateConn(AConnection: TUniConnection; AIsPooled: boolean);
begin
  AConnection.ProviderName := 'MySQL';
  AConnection.Database := 'activerecorddb';
  AConnection.Server := 'localhost';
  AConnection.Username := 'root';
  AConnection.Password := 'root';
  AConnection.SpecificOptions.Values['TinyIntFormat'] := 'Boolean';
  AConnection.SpecificOptions.Values['CharacterSet'] := 'utf8mb4';
  AConnection.Pooling := AIsPooled;
  if AIsPooled then
  begin
    AConnection.PoolMaxSize := 100;
  end;
end;

procedure CreateMSSQLServerPrivateConn(AConnection: TUniConnection; AIsPooled: boolean);
begin
  AConnection.ProviderName := 'SQLServer';
  AConnection.Database := 'activerecorddb';
  AConnection.Server := 'localhost';
  AConnection.Username := 'sa';
  AConnection.Password := 'Daniele123!';
  AConnection.Pooling := AIsPooled;
  if AIsPooled then
  begin
    AConnection.PoolMaxSize := 100;
  end;
end;

procedure CreateFirebirdPrivateConn(AConnection: TUniConnection; AIsPooled: boolean);
begin
  AConnection.ProviderName := 'InterBase';
  AConnection.Database := TPath.GetFullPath(TPath.Combine('..\..', 'data\ACTIVERECORDDB.FDB'));
  AConnection.Server := 'localhost';
  AConnection.Username := 'sysdba';
  AConnection.Password := 'masterkey';
  AConnection.SpecificOptions.Values['CharacterSet'] := 'UTF8';
  AConnection.Pooling := AIsPooled;
  if AIsPooled then
  begin
    AConnection.PoolMaxSize := 100;
  end;
end;

procedure CreateInterbasePrivateConn(AConnection: TUniConnection; AIsPooled: boolean);
begin
  AConnection.ProviderName := 'InterBase';
  AConnection.Database := TPath.GetFullPath(TPath.Combine('..\..', 'data\ACTIVERECORDDB.IB'));
  AConnection.Server := 'localhost';
  AConnection.Username := 'sysdba';
  AConnection.Password := 'masterkey';
  AConnection.SpecificOptions.Values['CharacterSet'] := 'UTF8';
  AConnection.Pooling := AIsPooled;
  if AIsPooled then
  begin
    AConnection.PoolMaxSize := 100;
  end;
end;

procedure CreatePostgresqlPrivateConn(AConnection: TUniConnection; AIsPooled: boolean);
begin
  AConnection.ProviderName := 'PostgreSQL';
  AConnection.Database := 'activerecorddb';
  AConnection.Server := 'localhost';
  AConnection.Username := 'postgres';
  AConnection.Password := 'postgres';
  AConnection.Pooling := AIsPooled;
  if AIsPooled then
  begin
    AConnection.PoolMaxSize := 100;
  end;
end;

procedure CreateSqlitePrivateConn(AConnection: TUniConnection; AIsPooled: boolean);
var
  lFName: string;
begin
  lFName := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), '..\..\data\activerecorddb.db');
  AConnection.ProviderName := 'SQLite';
  AConnection.Database := lFName;
  AConnection.Pooling := AIsPooled;
  if AIsPooled then
  begin
    AConnection.PoolMaxSize := 100;
  end;
end;

end.
