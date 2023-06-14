unit MainDataModuleUnit;

interface

uses
  System.SysUtils,
  System.Classes,
  MVCFramework.Commons,
  Data.DB,
  Data.SqlExpr,

{$IF CompilerVersion <= 27}
  Data.DBXJSON,

{$ELSE}
  System.JSON,

{$ENDIF}
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys,
  FireDAC.Comp.Client,
  FireDAC.Stan.Param,
  FireDAC.DatS,
  FireDAC.DApt.Intf,
  FireDAC.DApt,
  FireDAC.Comp.DataSet,
  FireDAC.Phys.IBBase,
  FireDAC.Phys.FB,
  WinesBO,
  FireDAC.Phys.FBDef,
  FireDAC.VCLUI.Wait;

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
    function GetAllWines: TDataSet;
    procedure AddWine(AWine: TWine);
    procedure UpdateWine(AWine: TWine);
    procedure DeleteWine(id: Integer);
  end;

implementation

{$R *.dfm}

uses
  System.StrUtils,
  MVCFramework.FireDAC.Utils;

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
var
  lDBPath: string;
begin
  // if database is defined in .env use that, otherwise try to
  // understand where the database is
  if not dotEnv.Env('database.path').IsEmpty then
  begin
    Connection.Params.Values['Database'] := dotEnv.Env('database.path');
  end
  else
  begin
    // if you are using firebird 2.5, uses the file WINES_FB25.FDB
    if not IsLibrary then
    begin
      // Is compiled as EXE
      Connection.Params.Values['Database'] := ExtractFilePath(ParamStr(0)) + '..\..\WINES_FB30.FDB';
    end
    else
    begin
      // compiled as apache module or isapi
      lDBPath := ExtractFilePath(GetModuleName(HInstance)) + '..\..\..\WineCellarSample\winecellarserver\WINES_FB30.FDB';
      if lDBPath.StartsWith('\\?\') then
        lDBPath := lDBPath.Remove(0, 4);
      Connection.Params.Values['Database'] := lDBPath;
    end;
  end;
end;

function TWineCellarDataModule.FindWines(Search: string): TDataSet;
begin
  if Search.IsEmpty then
    qryWines.Open('SELECT * FROM wine')
  else
    qryWines.Open('SELECT * FROM wine where NAME CONTAINING ?', [Search]);
  Result := qryWines;
end;

function TWineCellarDataModule.GetAllWines: TDataSet;
begin
  Result := FindWines('');
end;

function TWineCellarDataModule.GetWineById(id: Integer): TDataSet;
begin
  qryWines.Open('SELECT * FROM wine where id = ?', [id]);
  Result := qryWines;
end;

procedure TWineCellarDataModule.UpdateWine(AWine: TWine);
begin
  TFireDACUtils.ObjectToParameters(updWines.Commands[arUpdate].Params, AWine, 'NEW_');
  updWines.Commands[arUpdate].Params.ParamByName('OLD_ID').AsInteger := AWine.id;
  updWines.Commands[arUpdate].Execute;
end;

end.
