unit FDConnectionConfigU;

interface

const
  CON_DEF_NAME_FIREBIRD = 'MyConnFB';
  CON_DEF_NAME_MYSQL = 'MyConnMYSQL';

procedure CreateFirebirdPrivateConnDef(AIsPooled: boolean);
procedure CreateMySQLPrivateConnDef(AIsPooled: boolean);

implementation

uses
  System.Classes,
  System.IOUtils,
  FireDAC.Comp.Client;

procedure CreateMySQLPrivateConnDef(AIsPooled: boolean);
var
  LParams: TStringList;
begin
  LParams := TStringList.Create;
  try
    LParams.Add('Database=activerecorddb');
    LParams.Add('Protocol=TCPIP');
    LParams.Add('Server=localhost');
    LParams.Add('User_Name=root');
    LParams.Add('Password=root');
    LParams.Add('TinyIntFormat=Boolean'); { it's the default }
    if AIsPooled then
    begin
      LParams.Add('Pooled=True');
      LParams.Add('POOL_MaximumItems=100');
    end
    else
    begin
      LParams.Add('Pooled=False');
    end;
    FDManager.AddConnectionDef(CON_DEF_NAME_MYSQL, 'MySQL', LParams);
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
    LParams.Add('Database=' + TPath.GetFullPath(TPath.Combine('..\..', 'data\ORDERSMANAGER_FB30.FDB')));
    LParams.Add('Protocol=TCPIP');
    LParams.Add('Server=localhost');
    LParams.Add('User_Name=sysdba');
    LParams.Add('Password=masterkey');
    if AIsPooled then
    begin
      LParams.Add('Pooled=True');
      LParams.Add('POOL_MaximumItems=100');
    end
    else
    begin
      LParams.Add('Pooled=False');
    end;
    FDManager.AddConnectionDef(CON_DEF_NAME_FIREBIRD, 'FB', LParams);
  finally
    LParams.Free;
  end;
end;

end.
