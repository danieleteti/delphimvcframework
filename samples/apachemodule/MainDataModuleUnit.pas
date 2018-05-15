unit MainDataModuleUnit;

interface

uses
  System.SysUtils,
  System.Classes,
  Data.DB, System.IOUtils,
  MVCFramework.TypesAliases,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Comp.Client,
  FireDAC.Stan.Param, WinesBO,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Comp.DataSet, FireDAC.Phys.IBBase,
  FireDAC.Phys.FB, FireDAC.Phys.FBDef, FireDAC.VCLUI.Wait;

type
  TWineCellarDataModule = class(TDataModule)
    Connection: TFDConnection;
    qryWines: TFDQuery;
    updWines: TFDUpdateSQL;
    FDPhysFBDriverLink1: TFDPhysFBDriverLink;
    procedure ConnectionBeforeConnect(Sender: TObject);

  public
    function GetWineById(id: Integer): TDataSet;
    function FindWines(Search: string): TDataSet;
    procedure AddWine(AWine: TWine);
    procedure UpdateWine(Wine: TWine);
    procedure DeleteWine(id: Integer);
  end;

implementation

uses
  MVCFramework.Commons,
  MVCFramework.DataSet.Utils,
  MVCFramework.SystemJSONUtils,
  System.StrUtils,
  MVCFramework.FireDAC.Utils;

{$R *.dfm}


{ TCellarSM }

procedure TWineCellarDataModule.AddWine(AWine: TWine);
begin
  TFireDACUtils.ObjectToParameters(updWines.Commands[arInsert].Params, AWine, 'NEW_');
  updWines.Commands[arInsert].Execute;
end;

procedure TWineCellarDataModule.DeleteWine(id: Integer);
begin
  updWines.Commands[arDelete].ParamByName('OLD_ID').AsInteger := id;
  updWines.Commands[arDelete].Execute;
end;

procedure TWineCellarDataModule.ConnectionBeforeConnect(Sender: TObject);
begin
  Connection.Params.Values['Database'] :=
  { change this path to be compliant with your system }
    TPath.Combine(AppPath, '..\..\..\winecellarserver\WINES_FB30.FDB');
//    'D:\DEV\dmvcframework\samples\winecellarserver\WINES.FDB';
end;

function TWineCellarDataModule.FindWines(Search: string): TDataSet;
begin
  if Search.IsEmpty then
    qryWines.Open('SELECT * FROM wine')
  else
    qryWines.Open('SELECT * FROM wine where NAME CONTAINING ?', [Search]);
  Result := qryWines;
end;

function TWineCellarDataModule.GetWineById(id: Integer): TDataSet;
begin
  qryWines.Open('SELECT * FROM wine where id = ?', [id]);
  Result := qryWines;
end;

procedure TWineCellarDataModule.UpdateWine(Wine: TWine);
begin
  TFireDACUtils.ObjectToParameters(updWines.Commands[arUpdate].Params, Wine, 'NEW_');
  updWines.Commands[arUpdate].Params.ParamByName('OLD_ID').AsInteger := Wine.id;
  updWines.Commands[arUpdate].Execute;
end;

end.
