unit MainFormU;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Data.DB,
  Datasnap.DBClient,
  Vcl.ExtCtrls,
  Vcl.DBCtrls,
  Vcl.Grids,
  Vcl.DBGrids;

type
  TMainForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button5Click(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TPersona = class
  private
    FAge      : Integer;
    FLastName : string;
    FFirstName: string;
    FBornDate : TDateTime;
    FChild    : TPersona;
    procedure SetAge(const Value: Integer);
    procedure SetFirstName(const Value: string);
    procedure SetLastName(const Value: string);
    procedure SetBornDate(const Value: TDateTime);
    procedure SetChild(const Value: TPersona);

  public
    class function NewPersona: TPersona;
    property FirstName: string read FFirstName write SetFirstName;
    property LastName: string read FLastName write SetLastName;
    property Age: Integer read FAge write SetAge;
    property BornDate: TDateTime read FBornDate write SetBornDate;
    property Child: TPersona read FChild write SetChild;
  end;

var
  MainForm: TMainForm;

implementation

uses
  rtti,
  System.TypInfo,
  LuaBind.Intf,
  LuaBind;

{$R *.dfm}


procedure TMainForm.Button1Click(Sender: TObject);
var
  state: Plua_State;
begin
  state := lua_open;
  if not assigned(state) then
    raise Exception.Create('Cannot initialize Lua');
  try
    luaL_openlibs(state);
    if luaL_loadstring(state,
      'days = {"Sunday", "Monday", "Tuesday", "Wednesday"}; ' +
      'print(days[2]);') = 0 then
    begin
      if lua_pcall(state, 0, 1, 0) <> 0 then
      begin
        ShowMessage(lua_tostring(state, - 1));
        lua_pop(state, 1);
      end
      else
      begin
        lua_pop(state, lua_gettop(state));
      end;
    end
    else
    begin
      ShowMessage(lua_tostring(state, - 1));
      lua_pop(state, 1);
    end;
  finally
    lua_close(state);
  end;
end;

function MyFunctionForLua(L: Plua_State): Integer; cdecl;
var
  v: pansichar;
  s: ansistring;
begin
  s := uppercase(lua_tostring(L, 1));
  lua_pushstring(L, pansichar(s));
  Result := 1; // * number of results * /
end;

function myfullname(L: Plua_State): Integer; cdecl;
var
  v: pansichar;
  s: ansistring;
begin
  assert(lua_istable(L, - 1));
  lua_getfield(L, - 1, pansichar('nome'));
  s := lua_tostring(L, - 1);
  lua_pushstring(L, pansichar(s));
  Result := 1; // * number of results * /
end;

function _setcaption(L: Plua_State): Integer; cdecl;
var
  form      : TForm;
  newcaption: ansistring;
begin
  assert(lua_type(L, - 1) = LUA_TSTRING);
  assert(lua_type(L, - 2) = LUA_TLIGHTUSERDATA);

  newcaption := lua_tostring(L, - 1);
  lua_pop(L, 1);

  form := TForm(lua_topointer(L, - 1));
  lua_pop(L, 1);

  form.Caption := newcaption;
  Result := 0;
end;

function doSomethingOnDelphiObject(L: Plua_State): Integer; cdecl;
var
  v   : pansichar;
  s   : ansistring;
  p   : Pointer;
  form: TMainForm;
begin

  // TFileStream.Create('', fmCreate);

  assert(lua_islightuserdata(L, - 1));
  // lua_pop(L, );
  p := lua_topointer(L, - 1);
  lua_pop(L, 1);
  form := TMainForm(p);
  form.Caption := 'Cambiato da Lua';

  // lua_getfield(L, -1, pansichar('nome'));
  // s := lua_tostring(L, -1);
  // lua_pushstring(L, pansichar(s));
  //
  Result := 0; // * number of results * /
end;

procedure SetSomeTables(L: Plua_State);
begin
  lua_newtable(L);
  lua_pushstring(L, 'nome');
  lua_pushstring(L, 'Daniele');
  lua_settable(L, - 3);

  lua_pushstring(L, 'cognome');
  lua_pushstring(L, 'Teti');
  lua_settable(L, - 3);

  lua_pushstring(L, 'eta');
  lua_pushnumber(L, 3);
  lua_settable(L, - 3);

  lua_pushstring(L, 'fullname');
  lua_pushcfunction(L, @myfullname);
  lua_settable(L, - 3);

  lua_setglobal(L, 'daniele');
end;

// procedure SetSomeTables(L: Plua_State);
// begin
// lua_newtable(L);
// lua_pushstring(L, 'nome');
// lua_pushstring(L, 'Daniele');
// lua_pushstring(L, 'cognome');
// lua_pushnumber(L, 2);
// lua_pushstring(L, 'eta');
// lua_pushnumber(L, 3);
// lua_pushstring(L, 'fullname');
// lua_pushcfunction(L, @myfullname);
// lua_settable(L, -9);
// lua_settable(L, -7);
// lua_settable(L, -5);
// lua_settable(L, -3);
// lua_setglobal(L, 'daniele');
// end;

procedure SetSomeVariables(L: Plua_State);
begin
  lua_pushnumber(L, 11);
  lua_pushnumber(L, 22);
  lua_pushnumber(L, 33);
  lua_setglobal(L, 'x3');
  lua_setglobal(L, 'x2');
  lua_setglobal(L, 'x1');
end;

function Load(filename: ansistring; out AWidth: Integer;
  out AHeight: Integer): string;
var
  L       : Plua_State;
  AStringa: string;
begin
  L := lua_open();
  luaL_openlibs(L);
  // luaopen_base(L);
  // luaopen_io(L);
  // luaopen_string(L);
  // luaopen_math(L);

  if (luaL_loadfile(L, pansichar(filename)) > 0) then
  begin
    Exit('Cannot run configuration file ' + lua_tostring(L, - 1));
  end;

  SetSomeVariables(L);
  SetSomeTables(L);

  lua_pushcfunction(L, @MyFunctionForLua);
  lua_setglobal(L, 'delphiuppercase');

  lua_pushcfunction(L, @doSomethingOnDelphiObject);
  lua_setglobal(L, 'dosomething');

  lua_pushcfunction(L, @_setcaption);
  lua_setglobal(L, '_setcaption');

  lua_pushlightuserdata(L, MainForm);
  lua_setglobal(L, 'form');

  if lua_pcall(L, 0, 0, 0) > 0 then
  begin
    Exit('Cannot exec configuration file ' + lua_tostring(L, - 1));
  end;

  lua_getglobal(L, 'width');
  lua_getglobal(L, 'height');
  lua_getglobal(L, 'stringa');

  if (lua_isnumber(L, - 3) = 0) then
    Exit('Width should be a number');

  if (lua_isnumber(L, - 2) = 0) then
    Exit('Height should be a number');

  if (lua_isstring(L, - 1) = 0) then
    Exit('stringa should be a string');

  AWidth := lua_tointeger(L, - 3);
  AHeight := lua_tointeger(L, - 2);
  AStringa := lua_tostring(L, - 1);
  lua_close(L);
  Result := Format('Width: %d, Height: %d, Stringa: %s',
    [AWidth, AHeight, AStringa]);
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  w: Integer;
  h: Integer;
begin
  ShowMessage(Load('config.lua', w, h));
end;

procedure push_object_properties(L: Plua_State; AObject: TObject;
  AName: string);
var
  prop      : TRttiProperty;
  ctx       : TRttiContext;
  properties: TArray<TRttiProperty>;
  k         : ansistring;
  Value     : TValue;
  v         : ansistring;
begin
  ctx := TRttiContext.Create;
  try
    lua_newtable(L);
    properties := ctx.GetType(AObject.ClassType).GetProperties;
    for prop in properties do
    begin
      k := prop.Name;
      lua_pushstring(L, pansichar(k));
      Value := prop.GetValue(AObject);
      if Value.TypeInfo^.Kind in [tkString, tkUString] then
      begin
        v := Value.AsString;
        lua_pushstring(L, pansichar(v));
      end
      else if Value.TypeInfo^.Kind = tkInteger then
      begin
        lua_pushnumber(L, Value.AsInteger);
      end
      else if Value.TypeInfo^.Kind = tkFloat then
      begin
        lua_pushnumber(L, Value.AsExtended);
      end
      else if Value.TypeInfo^.Kind = tkClass then
      begin
        if Value.IsEmpty then
          lua_pushnil(L)
        else
          push_object_properties(L, Value.AsObject, '');
      end;

      lua_settable(L, - 3);
    end;
  finally
    ctx.Free;
  end;
end;

procedure push_table(L: Plua_State; Keys: array of ansistring;
  Values: array of ansistring);
var
  key         : TObject;
  I           : Integer;
  k           : ansistring;
  v           : ansistring;
  pvalue, pkey: pansichar;
  le          : Cardinal;
begin
  // lua_newtable(L);
  // pkey := GetMemory(Length('firstname') + 1);
  // pkey := StrCopy(pkey, pansichar('firstname'));
  // lua_pushstring(L, pkey);
  // //lua_pushstring(L, 'firstname');
  // lua_pushstring(L, 'Daniele');
  // lua_settable(L, -3);
  // lua_pushstring(L, 'lastname');
  // lua_pushstring(L, 'Teti');
  // lua_settable(L, -3);

  lua_newtable(L);
  for I := 0 to high(Keys) do
  begin
    k := Keys[I];
    v := Values[I];
    lua_pushstring(L, pansichar(k));
    lua_pushstring(L, pansichar(v));
    lua_settable(L, - 3);
  end;


  // k := Keys[I];
  // pkey := GetMemory(Length(k) + 1);
  // ZeroMemory(pkey, Length(k) + 1);
  // MoveMemory(pkey, pansichar(k), Length(k));
  /// /    pkey := StrPCopy(pkey, pansichar(k));
  //
  // v := Values[I];
  // pvalue := GetMemory(Length(v) + 1);
  // pvalue := StrCopy(pvalue, pansichar(v));
  //
  // lua_pushstring(L, pkey);
  // lua_pushstring(L, pvalue);
  // lua_settable(L, -3);

end;

procedure TMainForm.Button3Click(Sender: TObject);
var
  L      : Plua_State;
  persona: TPersona;
begin
  persona := TPersona.Create;
  try
    persona.FirstName := 'Daniele';
    persona.LastName := 'Teti';
    persona.Age := 32;
    persona.BornDate := date;
    persona.Child := TPersona.NewPersona;

    L := lua_open;
    try
      luaL_openlibs(L);
      if luaL_loadfile(L, 'test01.html') > 0 then
        raise Exception.Create('Cannot run configuration file ' +
          lua_tostring(L, - 1));

      // push_table(L, ['firstname', 'lastname'], ['Daniele', 'Teti']);
      push_object_properties(L, persona, 'persona');
      lua_setglobal(L, 'persona');
      if lua_pcall(L, 0, 0, 0) > 0 then
      begin
        raise Exception.Create('Cannot exec configuration file ' +
          lua_tostring(L, - 1));
      end;
      lua_getglobal(L, 'p');
      ShowMessage(lua_tostring(L, - 1));
    finally
      lua_close(L);
    end;
  finally
    persona.Child.Free;
    persona.Free;
  end;
end;

procedure TMainForm.Button4Click(Sender: TObject);
var
  lua: TLuaEngine;
  v  : ILuaValue;
begin
  lua := TLuaEngine.Create;
  try
    lua.LoadScript(
      'local z = y + 2;' + sLineBreak +
      'x = y + 1 + z;');
    lua.DeclareGlobalInteger('y', 4);
    lua.Execute;
    v := lua.GetGlobal('x');
    if v.IsNumber then
      ShowMessage(FloatToStr(v.GetAsNumber));
  finally
    lua.Free;
  end;
end;

procedure TMainForm.Button5Click(Sender: TObject);
begin
  // ClientDataSet1.SaveToFile(ClientDataSet1.filename);
  // ClientDataSet1.Close;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // ClientDataSet1.Close;
  // ClientDataSet1.filename := '..\..\..\unittests\win32\debug\samplecds.xml';
  // ClientDataSet1.Open;
end;

{ TPersona }

class function TPersona.NewPersona: TPersona;
begin
  Result := TPersona.Create;
  Result.FirstName := 'Mattia';
  Result.LastName := 'Teti';
  Result.Age := 1;
  Result.BornDate := EncodeDate(2011, 11, 17);
end;

procedure TPersona.SetAge(const Value: Integer);
begin
  FAge := Value;
end;

procedure TPersona.SetBornDate(const Value: TDateTime);
begin
  FBornDate := Value;
end;

procedure TPersona.SetChild(const Value: TPersona);
begin
  FChild := Value;
end;

procedure TPersona.SetFirstName(const Value: string);
begin
  FFirstName := Value;
end;

procedure TPersona.SetLastName(const Value: string);
begin
  FLastName := Value;
end;

end.
