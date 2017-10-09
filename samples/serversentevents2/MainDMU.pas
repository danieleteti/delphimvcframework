unit MainDMU;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.VCLUI.Wait,
  Data.DB, FireDAC.Comp.Client, FireDAC.Phys.MSSQL, FireDAC.Phys.MSSQLDef,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.Comp.DataSet, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs;

type
  TdmMain = class(TDataModule)
    Connection: TFDConnection;
    qryInsertNotification: TFDQuery;
    procedure ConnectionAfterConnect(Sender: TObject);
    procedure ConnectionBeforeConnect(Sender: TObject);
  private
    { Private declarations }
  public

  end;

implementation

uses
  SysConstantsU;

{ %CLASSGROUP 'Vcl.Controls.TControl' }

{$R *.dfm}

procedure TdmMain.ConnectionAfterConnect(Sender: TObject);
begin
  Connection.ExecSQL(SQL_CREATE_TABLE);
end;

procedure TdmMain.ConnectionBeforeConnect(Sender: TObject);
begin
  Connection.Params.Database := 'serversentevent.db';
end;

end.
