unit LuaBind.CustomType.DataSet;

interface

uses
  Data.DB,
  LuaBind;

type
  TLuaDataSetExposerLibraries = class(TInterfacedObject, ILuaLibraryLoader)

  public
    procedure Execute(ALuaEngine: TLuaEngine);
  end;

procedure ExposeDataSet(ALuaEngine: TLuaEngine; ADataSet: TDataSet; AVariableName: AnsiString);

implementation

uses LuaBind.Intf;

function _dataset_eof(L: plua_state): Integer; cdecl;
var
  ds: TDataSet;
  s : ansistring;
begin
  s := lua_typename(L, lua_type(L, - 1));
  assert(lua_islightuserdata(L, - 1));
  ds := TDataSet(lua_topointer(L, - 1));
  if ds.Eof then
    lua_pushboolean(L, 1)
  else
    lua_pushboolean(L, 0);
  Result := 1;
end;

function _dataset_move_next(L: plua_state): Integer; cdecl;
var
  ds: TDataSet;
begin
  assert(lua_islightuserdata(L, - 1));
  ds := TDataSet(lua_topointer(L, - 1));
  ds.Next;
  Result := 0;
end;

function _dataset_field_by_name(L: plua_state): Integer; cdecl;
var
  fieldname: AnsiString;
  ds       : TDataSet;
  sdata    : AnsiString;
begin
  assert(lua_isstring(L, - 1) = 1);
  assert(lua_islightuserdata(L, - 2));
  fieldname := lua_tostring(L, - 1);
  lua_pop(L, 1);
  ds := TDataSet(lua_topointer(L, - 1));
  lua_pop(L, 1);

  sdata := ds.FieldByName(String(fieldname)).AsAnsiString;
  lua_pushstring(L, pansichar(sdata));
  Result := 1;
end;

function _dataset_close(L: plua_state): Integer; cdecl;
var
  ds: TDataSet;
begin
  assert(lua_islightuserdata(L, - 1));
  ds := TDataSet(lua_topointer(L, - 1));
  ds.Close;
  Result := 0;
end;

function _dataset_first(L: plua_state): Integer; cdecl;
var
  ds: TDataSet;
begin
  assert(lua_islightuserdata(L, - 1));
  ds := TDataSet(lua_topointer(L, - 1));
  ds.First;
  Result := 0;
end;

function _dataset_is_open(L: plua_state): Integer; cdecl;
var
  ds: TDataSet;
  o : Integer;
begin
  assert(lua_islightuserdata(L, - 1));
  ds := TDataSet(lua_topointer(L, - 1));
  if ds.State = dsInactive then
    o := 0
  else
    o := 1;
  lua_pushboolean(L, o);
  Result := 1;
end;

function _dataset_open(L: plua_state): Integer; cdecl;
var
  ds: TDataSet;
begin
  assert(lua_islightuserdata(L, - 1));
  ds := TDataSet(lua_topointer(L, - 1));
  ds.Open;
  Result := 0;
end;

procedure ExposeDataSet(ALuaEngine: TLuaEngine; ADataSet: TDataSet; AVariableName: AnsiString);
begin
  ALuaEngine.DeclareGlobalLightUserData(AVariableName, ADataSet);
end;

{ TLuaDataSetExportlibraries }

procedure TLuaDataSetExposerLibraries.Execute(ALuaEngine: TLuaEngine);
begin
  ALuaEngine.
    DeclareGlobalFunction('_dataset_eof', @_dataset_eof).
    DeclareGlobalFunction('_dataset_move_next', @_dataset_move_next).
    DeclareGlobalFunction('_dataset_field_by_name', @_dataset_field_by_name).
    DeclareGlobalFunction('_dataset_first', @_dataset_first).
    DeclareGlobalFunction('_dataset_is_open', @_dataset_is_open).
    DeclareGlobalFunction('_dataset_open', @_dataset_open).
    DeclareGlobalFunction('_dataset_close', @_dataset_close);
end;

end.
