unit CustomersTDGU;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.VCLUI.Wait,
  Data.DB, FireDAC.Comp.Client, FireDAC.Comp.UI, FireDAC.Phys.FBDef,
  FireDAC.Phys.IBBase, FireDAC.Phys.FB, FireDAC.DApt;

type
  TCustomersTDG = class(TDataModule)
    FDConnection1: TFDConnection;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    function GetCustomers: TDataSet;
    function GetCustomerById(const ID: UInt64): TDataSet;
  end;

procedure SetupFireDACConnection;

implementation

uses
  FireDAC.Stan.Param;

const
  PRIVATE_CONN_DEF_NAME = 'MYCONNECTION';

  { %CLASSGROUP 'Vcl.Controls.TControl' }

  {$R *.dfm}

  { TDataModule1 }

procedure TCustomersTDG.DataModuleCreate(Sender: TObject);
begin
  FDConnection1.ConnectionDefName := PRIVATE_CONN_DEF_NAME;
  FDConnection1.Connected := True;
end;

function TCustomersTDG.GetCustomerById(const ID: UInt64): TDataSet;
var
  lParams: TFDParams;
  lParam: TFDParam;
begin
  lParams := TFDParams.Create;
  try
    lParam := lParams.Add;
    lParam.AsLargeInt := ID;
    lParam.Name := 'ID';
    FDConnection1.ExecSQL('SELECT * FROM CUSTOMERS WHERE ID = :ID', lParams, Result);
  finally
    lParams.Free;
  end;
end;

function TCustomersTDG.GetCustomers: TDataSet;
begin
  FDConnection1.ExecSQL('SELECT * FROM CUSTOMERS ORDER BY ID', Result);
end;

procedure SetupFireDACConnection;
var
  lParams: TStrings;
begin
  lParams := TStringList.Create;
  try
    // Use CUSTOMERS_FB25.FDB is you are using firebird 2.5
    lParams.Add('Database=' + ExtractFilePath(GetModuleName(HInstance)) + '\CUSTOMERS_FB30.FDB');
    lParams.Add('Protocol=TCPIP');
    lParams.Add('Server=localhost');
    lParams.Add('User_Name=sysdba');
    lParams.Add('Password=masterkey');
    lParams.Add('Pooled=true');
    FDManager.AddConnectionDef(PRIVATE_CONN_DEF_NAME, 'FB', lParams);
  finally
    lParams.Free;
  end;
end;

end.
