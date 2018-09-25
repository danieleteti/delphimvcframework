unit FDConnectionConfigU;

interface

const
  CON_DEF_NAME = 'MyPrivConn';

function CreatePrivateConnDef(AIsPooled: boolean = True): string;

implementation

uses
  System.Classes,
  FireDAC.Comp.Client;

function CreatePrivateConnDef(AIsPooled: boolean): string;
var
  LParams: TStringList;
  LConnName: string;
begin
  LParams := TStringList.Create;
  try
    LParams.Add('Database=C:\DEV\dmvcframework\samples\data\ACTIVERECORDDB.FDB');
    LParams.Add('Protocol=TCPIP');
    LParams.Add('Server=localhost');
    LParams.Add('User_Name=sysdba');
    LParams.Add('Password=masterkey');
    if AIsPooled then
    begin
      LParams.Add('Pooled=True');
    end
    else
    begin
      LParams.Add('Pooled=False');
    end;
    FDManager.AddConnectionDef(CON_DEF_NAME, 'FB', LParams);
  finally
    LParams.Free;
  end;
  Result := LConnName;
end;

end.
