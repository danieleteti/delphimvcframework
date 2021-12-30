unit LuaBind.CustomType.UserType;

interface

uses
  LuaBind;

type
  TLuaMyTest = class(TInterfacedObject, ILuaLibraryLoader)
  public
    procedure Execute(ALuaEngine: TLuaEngine);
  end;

procedure ExposeMyTest(ALuaEngine: TLuaEngine; AObject: TObject;
  AVariableName: AnsiString);

implementation

uses
  LuaBind.Intf,
  System.Classes;

var
  MyTest: TArray<luaL_Reg>;

  { TLuaMyTest }

type
  PStringList = ^TStringlist;

function _NewObject(L: Plua_State): Integer; cdecl;
var
  psl: PStringList;
begin
  if lua_gettop(L) <> 0 then
    Exit(luaL_error(L, PAnsiChar('Got arguments expected no arguments')));
  psl := lua_newuserdata(L, SizeOf(PStringList));
  psl^ := TStringlist.Create;
  lua_getglobal(L, PAnsiChar('TStringList'));
  lua_setmetatable(L, - 2);
  Result := 1;
end;

function _TStringList_free(L: Plua_State): Integer; cdecl;
var
  s: string;
  p: PStringList;
begin
  if (lua_gettop(L) <> 1) or (lua_isuserdata(L, - 1) = 0) then
    Exit(luaL_error(L, PAnsiChar('No params allowed or no valid userdata')));
  p := PStringList(lua_touserdata(L, - 1));
  lua_pop(L, 1);
  p^.Free;
  p^ := nil;
  Result := 0;
end;

function _TStringList_add(L: Plua_State): Integer; cdecl;
var
  s: string;
  p: PStringList;
begin
  if lua_gettop(L) <> 2 then
    Exit(luaL_error(L, PAnsiChar('Wrong number of parameters: 2 expected')));
  s := TLuaValue.PopTValueFromStack(L).AsString;
  p := PStringList(lua_touserdata(L, - 1));
  lua_pop(L, 1);
  p^.Add(s);
  Result := 0;
end;

procedure TLuaMyTest.Execute(ALuaEngine: TLuaEngine);
begin
  SetLength(MyTest, 4);
  MyTest[0].name := PAnsiChar('new');
  MyTest[0].func := @_NewObject;
  MyTest[1].name := PAnsiChar('add');
  MyTest[1].func := @_TStringList_add;
  MyTest[2].name := PAnsiChar('__gc');
  MyTest[2].func := @_TStringList_free;
  MyTest[3].name := nil;
  MyTest[3].func := nil;

  luaL_register(ALuaEngine.GetRawLuaState, 'TStringList', @MyTest[0]);
  lua_pushvalue(ALuaEngine.GetRawLuaState, - 1);
  lua_setfield(ALuaEngine.GetRawLuaState, - 2, '__index');
end;

procedure ExposeMyTest(ALuaEngine: TLuaEngine; AObject: TObject;
  AVariableName: AnsiString);
begin

end;

end.
