unit MainDM;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.FB, Data.DB,
  FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, FireDAC.Comp.DataSet, FireDAC.Phys.FBDef, FireDAC.VCLUI.Wait;

type
  TdmMain = class(TDataModule)
    Connection: TFDConnection;
    dsArticles: TFDQuery;
    updArticles: TFDUpdateSQL;
    procedure ConnectionBeforeConnect(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}
{$R *.dfm}

procedure TdmMain.ConnectionBeforeConnect(Sender: TObject);
begin
  // currently, this demo uses firebird 2.5
  // if you want to use firebird 2.5, you can use the file ORDERSMANAGER_FB25.FDB
  // Connection.Params.Values['Database'] := '..\..\data\ORDERSMANAGER_FB30.FDB';
  Connection.Params.Values['Database'] := '..\..\data\ORDERSMANAGER_FB25.FDB';
end;

end.
