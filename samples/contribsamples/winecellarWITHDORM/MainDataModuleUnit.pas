unit MainDataModuleUnit;

interface

uses System.SysUtils,
  System.Classes,
  Data.DBXFirebird,
  Data.DB,
  Data.SqlExpr,
  Data.DBXJSON;

type
  TWineCellarDataModule = class(TDataModule)
    wines: TSQLConnection;
    procedure DataModuleCreate(Sender: TObject);

  public
    function GetWineById(id: Integer): TJSONObject;
    function FindWines(Search: string): TJSONArray;
    function AddWine(Wine: TJSONObject): TJSONObject;
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

function TWineCellarDataModule.AddWine(Wine: TJSONObject): TJSONObject;
var
  cmd: TDBXCommand;
  w  : TWine;
  qry: TSQLQuery;
const
  SQL = 'INSERT INTO wine (name, grapes, country, region, "YEAR", description) VALUES (:name, :grapes, :country, :region, :year, :description)';
begin
  w := Mapper.JSONObjectToObject<TWine>(Wine);
  try
    qry := Mapper.CreateQuery(wines, SQL);
    try
      Mapper.ExecuteSQLQueryNoResult(qry, w);
    finally
      qry.free;
    end;
  finally
    w.free;
  end;
  Result := TJSONObject.Create;
end;

procedure TWineCellarDataModule.DataModuleCreate(Sender: TObject);
begin
  wines.Params.Values[TDBXPropertyNames.Database] := ExtractFilePath(ParamStr(0)
    ) + '..\..\WINES.FDB';
  wines.Open;
end;

function TWineCellarDataModule.DeleteWine(id: Integer): TJSONObject;
var
  cmd: TDBXCommand;
begin
  cmd := wines.DBXConnection.CreateCommand;
  try
    cmd.Text := 'DELETE FROM WINE WHERE ID = ' + inttostr(id);
    cmd.ExecuteUpdate;
    Result := TJSONObject.Create;
  finally
    cmd.free;
  end;
end;

function TWineCellarDataModule.FindWines(Search: string): TJSONArray;
var
  obj: TJSONObject;
  cmd: TDBXCommand;
begin
  cmd := wines.DBXConnection.CreateCommand;
  try
    if Search.IsEmpty then
      cmd.Text := 'SELECT * FROM wine'
    else
      cmd.Text := 'SELECT * FROM wine where NAME CONTAINING ''' + Search + '''';
    cmd.Text := cmd.Text + ' order by name';
    Result := TJSONArray.Create;
    Mapper.ReaderToJSONArray(cmd.ExecuteQuery, Result);
  finally
    cmd.free;
  end;
end;

function TWineCellarDataModule.GetWineById(id: Integer): TJSONObject;
var
  cmd: TDBXCommand;
  rdr: TDBXReader;
begin
  Result := nil;
  cmd := wines.DBXConnection.CreateCommand;
  try
    cmd.Text := 'SELECT * FROM wine where id = ' + inttostr(id);
    Result := TJSONObject.Create;
    rdr := cmd.ExecuteQuery;
    if rdr.Next then
      Mapper.ReaderToJSONObject(rdr, Result);
  finally
    cmd.free;
  end;
end;

function TWineCellarDataModule.UpdateWine(Wine: TJSONObject): TJSONObject;
var
  cmd: TDBXCommand;
  w  : TWine;
  qry: TSQLQuery;
const
  SQL = 'UPDATE WINE SET name = :name, grapes = :grapes, country = :country, region = :region, "YEAR" = :year, description = :description WHERE ID = :id';
begin
  w := Mapper.JSONObjectToObject<TWine>(Wine);
  try
    qry := Mapper.CreateQuery(wines, SQL);
    try
      Mapper.ExecuteSQLQueryNoResult(qry, w);
    finally
      qry.free;
    end;
  finally
    w.free;
  end;
  Result := TJSONObject.Create;
end;

end.
