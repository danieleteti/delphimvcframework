unit MainDataModuleUnit;

interface

uses System.SysUtils,
  System.Classes,
  Data.DBXFirebird,
  Data.DB,
  Data.SqlExpr
{$IFDEF VER270}
    , System.JSON
{$ELSE}
    , Data.DBXJSON
{$ENDIF}
    , FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Comp.Client, FireDAC.Stan.Param,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Comp.DataSet, FireDAC.Phys.IBBase, FireDAC.Phys.FB;

type
  TWineCellarDataModule = class(TDataModule)
    Connection: TFDConnection;
    qryWines: TFDQuery;
    updWines: TFDUpdateSQL;
    FDPhysFBDriverLink1: TFDPhysFBDriverLink;
    procedure ConnectionBeforeConnect(Sender: TObject);

  public
    function GetWineById(id: Integer): TJSONObject;
    function FindWines(Search: string): TJSONArray;
    function AddWine(AWine: TJSONObject): TJSONObject;
    function UpdateWine(Wine: TJSONObject): TJSONObject;
    function DeleteWine(id: Integer): TJSONObject;
  end;

implementation

{$R *.dfm}


uses System.StrUtils,
  Data.DBXCommon,
  ObjectsMappers,
  WinesBO;

{ TCellarSM }

function TWineCellarDataModule.AddWine(AWine: TJSONObject): TJSONObject;
var
  Wine: TWine;
begin
  Wine := Mapper.JSONObjectToObject<TWine>(AWine);
  try
    Mapper.ObjectToFDParameters(
      updWines.Commands[arInsert].Params,
      Wine,
      'NEW_');
    updWines.Commands[arInsert].Execute;
  finally
    Wine.Free;
  end;
end;

function TWineCellarDataModule.DeleteWine(id: Integer): TJSONObject;
begin
  updWines.Commands[arDelete].ParamByName('OLD_ID').AsInteger := id;
  updWines.Commands[arDelete].Execute;
end;

procedure TWineCellarDataModule.ConnectionBeforeConnect(Sender: TObject);
begin
  Connection.Params.Values['Database'] :=
  { change this path to be compliant with your system }
    'C:\DEV\DMVCFramework\samples\winecellar\WINES.FDB';
end;

function TWineCellarDataModule.FindWines(Search: string): TJSONArray;
begin
  if Search.IsEmpty then
    qryWines.Open('SELECT * FROM wine')
  else
    qryWines.Open('SELECT * FROM wine where NAME CONTAINING ?', [Search]);
  Result := qryWines.AsJSONArray;
end;

function TWineCellarDataModule.GetWineById(id: Integer): TJSONObject;
begin
  qryWines.Open('SELECT * FROM wine where id = ?', [id]);
  Result := qryWines.AsJSONObject;
end;

function TWineCellarDataModule.UpdateWine(Wine: TJSONObject): TJSONObject;
var
  w: TWine;
begin
  w := Mapper.JSONObjectToObject<TWine>(Wine);
  try
    Mapper.ObjectToFDParameters(updWines.Commands[arUpdate].Params, w, 'NEW_');
    updWines.Commands[arUpdate].Params.ParamByName('OLD_ID').AsInteger := w.id;
    updWines.Commands[arUpdate].Execute;
  finally
    w.Free;
  end;
  Result := TJSONObject.Create;
end;

end.
