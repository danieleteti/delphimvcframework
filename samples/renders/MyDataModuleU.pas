unit MyDataModuleU;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.FB,
  FireDAC.Phys.FBDef, FireDAC.VCLUI.Wait, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Phys.IBBase, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type
  TMyDataModule = class(TDataModule)
    FDConnection1: TFDConnection;
    qryCustomers: TFDQuery;
    FDPhysFBDriverLink1: TFDPhysFBDriverLink;
    qryCountry: TFDQuery;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MyDataModule: TMyDataModule;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.
