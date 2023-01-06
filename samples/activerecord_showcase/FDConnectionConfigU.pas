unit FDConnectionConfigU;

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
  FireDAC.Comp.Client,
  FireDAC.Moni.Base,
  FireDAC.Moni.FlatFile,
  FireDAC.Stan.Intf
  ;


var
  gFlatFileMonitor: TFDMoniFlatFileClientLink = nil;

procedure CreateMySQLPrivateConnDef(AIsPooled: boolean);
var
  LParams: TStringList;
begin
  {
    docker run --detach --env MARIADB_USER=example-user --env MARIADB_PASSWORD=my_cool_secret --env MARIADB_ROOT_PASSWORD=root  -p 3306:3306 mariadb:latest
  }

  LParams := TStringList.Create;
  try
    LParams.Add('Database=activerecorddb');
    LParams.Add('Protocol=TCPIP');
    LParams.Add('Server=localhost');
    LParams.Add('User_Name=root');
    LParams.Add('Password=root');
    LParams.Add('TinyIntFormat=Boolean'); { it's the default }
    LParams.Add('CharacterSet=utf8mb4'); // not utf8!!
    LParams.Add('MonitorBy=FlatFile');
    if AIsPooled then
    begin
      LParams.Add('Pooled=True');
      LParams.Add('POOL_MaximumItems=100');
    end
    else
    begin
      LParams.Add('Pooled=False');
    end;
    FDManager.AddConnectionDef(CON_DEF_NAME, 'MySQL', LParams);
  finally
    LParams.Free;
  end;
end;

procedure CreateMSSQLServerPrivateConnDef(AIsPooled: boolean);
var
  LParams: TStringList;
begin
  {
    docker run -e "ACCEPT_EULA=Y" -e "SA_PASSWORD=!SA_password!" -p 1433:1433 -d mcr.microsoft.com/mssql/server:2019-latest
  }

  // [ACTIVERECORDB_SQLSERVER]
  // Database=activerecorddb
  // OSAuthent=Yes
  // Server=DANIELETETI\SQLEXPRESS
  // DriverID=MSSQL
  //

  LParams := TStringList.Create;
  try
    LParams.Add('Database=activerecorddb');
    LParams.Add('OSAuthent=Yes');
    LParams.Add('Server=DANIELETETI\SQLEXPRESS');
    if AIsPooled then
    begin
      LParams.Add('Pooled=True');
      LParams.Add('POOL_MaximumItems=100');
    end
    else
    begin
      LParams.Add('Pooled=False');
    end;
    FDManager.AddConnectionDef(CON_DEF_NAME, 'MSSQL', LParams);
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
    LParams.Add('Protocol=TCPIP');
    LParams.Add('Server=localhost');
    LParams.Add('User_Name=sysdba');
    LParams.Add('Password=masterkey');
    LParams.Add('CharacterSet=UTF8');
    if AIsPooled then
    begin
      LParams.Add('Pooled=True');
      LParams.Add('POOL_MaximumItems=100');
    end
    else
    begin
      LParams.Add('Pooled=False');
    end;
    FDManager.AddConnectionDef(CON_DEF_NAME, 'FB', LParams);
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
    LParams.Add('Protocol=TCPIP');
    LParams.Add('Server=localhost');
    LParams.Add('User_Name=sysdba');
    LParams.Add('Password=masterkey');
    LParams.Add('CharacterSet=UTF8');
    if AIsPooled then
    begin
      LParams.Add('Pooled=True');
      LParams.Add('POOL_MaximumItems=100');
    end
    else
    begin
      LParams.Add('Pooled=False');
    end;
    FDManager.AddConnectionDef(CON_DEF_NAME, 'IB', LParams);
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
    LParams.Add('Protocol=TCPIP');
    LParams.Add('Server=localhost');
    LParams.Add('User_Name=postgres');
    LParams.Add('Password=postgres');
    LParams.Add('MonitorBy=FlatFile');

    // https://quality.embarcadero.com/browse/RSP-19755?jql=text%20~%20%22firedac%20guid%22
    LParams.Add('GUIDEndian=Big');
    if AIsPooled then
    begin
      LParams.Add('Pooled=True');
      LParams.Add('POOL_MaximumItems=100');
    end
    else
    begin
      LParams.Add('Pooled=False');
    end;
    FDManager.AddConnectionDef(CON_DEF_NAME, 'PG', LParams);
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
    LParams.Add('StringFormat=Unicode');
    if AIsPooled then
    begin
      LParams.Add('Pooled=True');
      LParams.Add('POOL_MaximumItems=100');
    end
    else
    begin
      LParams.Add('Pooled=False');
    end;
    FDManager.AddConnectionDef(CON_DEF_NAME, 'SQLite', LParams);
  finally
    LParams.Free;
  end;
end;

initialization

gFlatFileMonitor := TFDMoniFlatFileClientLink.Create(nil);
gFlatFileMonitor.FileColumns := [tiRefNo, tiTime, tiThreadID, tiClassName, tiObjID, tiMsgText];
gFlatFileMonitor.EventKinds := [
    ekVendor, ekConnConnect, ekLiveCycle, ekError, ekConnTransact,
    ekCmdPrepare, ekCmdExecute, ekCmdDataIn, ekCmdDataOut];
gFlatFileMonitor.ShowTraces := False;
gFlatFileMonitor.FileAppend := False;
gFlatFileMonitor.FileName := TPath.ChangeExtension(ParamStr(0), '.trace.log');
gFlatFileMonitor.Tracing := True;

finalization

gFlatFileMonitor.Free;

end.
