unit LuaWrapper;

interface

uses dorLua,
  System.Classes,
  System.SysUtils,
  System.Rtti;

type
  ELuaException = class(Exception)

  end;

  ILuaValue = interface
    ['{8621BAD3-B847-4176-937F-2CD016BBE13E}']
    /// ///
    function IsNil: boolean;
    function IsNumber: boolean;
    function IsInteger: boolean;
    function IsString: boolean;
    function IsBoolean: boolean;
    function IsLightUserData: boolean;
    /// ///
    function GetAsNumber: Double;
    function GetAsInteger: Integer;
    function GetAsString: AnsiString;
    function GetAsBoolean: boolean;
    function GetAsLightUserData: Pointer;
  end;

  TLuaValueType = (lvtNil, lvtNumber, lvtInteger, lvtString, lvtBoolean, lvtLightUserData,
    lvtTable, lvtFunction, lvtUserData, lvtThread);

  TLuaValue = class(TInterfacedObject, ILuaValue)
  strict private
    FLuaValueType: TLuaValueType;
  strict
    private

  protected
    /// ///
    function IsNil: boolean;
    function IsNumber: boolean;
    function IsInteger: boolean;
    function IsString: boolean;
    function IsBoolean: boolean;
    function IsLightUserData: boolean;
    /// ///
    function GetAsNumber: Double;
    function GetAsInteger: Integer;
    function GetAsString: AnsiString;
    function GetAsBoolean: boolean;
    function GetAsLightUserData: Pointer;
  private
    procedure CheckType(LuaValueType: TLuaValueType);
  protected
    FValue: TValue;
    constructor Create(LuaState: Plua_State; StackIndex: Integer);
    { todo }
    function GetAsLuaTable(LuaState: Plua_State; StackIndex: Integer): TValue;
  public
    class function GetLuaValueType(LuaState: Plua_State; StackIndex: Integer): TLuaValueType;
  end;

  TLuaEngine = class
  strict protected
    LState: Plua_State;
  public
    constructor Create; overload; virtual;
    constructor Create(const AScript: String); overload; virtual;
    destructor Destroy; override;
    procedure LoadScript(const AScript: AnsiString); overload;
    procedure LoadScript(const AStream: TStream; AOwnStream: boolean = true); overload;
    procedure LoadFromFile(const AFileName: String);
    procedure Execute;

    // PUSH methods for simple types
    function DeclareGlobalNil(AName: AnsiString): TLuaEngine;
    function DeclareGlobalNumber(AName: AnsiString; AValue: Double): TLuaEngine;
    function DeclareGlobalInteger(AName: AnsiString; AValue: Integer): TLuaEngine;
    function DeclareGlobalString(AName: AnsiString; AValue: AnsiString): TLuaEngine;
    function DeclareGlobalBoolean(AName: AnsiString; AValue: boolean): TLuaEngine;
    function DeclareGlobalLightUserData(AName: AnsiString; AValue: Pointer): TLuaEngine;

    // GET methods for simple types
    function GetGlobal(AName: AnsiString): ILuaValue;
  end;

implementation

{ TLuaEngine }

constructor TLuaEngine.Create;
begin
  inherited Create;
  LState := lua_open;
  if not assigned(LState) then
    raise ELuaException.Create('Cannot initialize Lua');
  luaL_openlibs(LState);
end;

constructor TLuaEngine.Create(const AScript: String);
begin
  Create;
  LoadScript(AScript);
end;

function TLuaEngine.DeclareGlobalBoolean(AName: AnsiString; AValue: boolean): TLuaEngine;
var
  b: Integer;
begin
  if AValue then
    b := 1
  else
    b := 0;
  lua_pushboolean(LState, b);
  lua_setglobal(LState, PAnsiChar(AName));
end;

function TLuaEngine.DeclareGlobalInteger(AName: AnsiString; AValue: Integer): TLuaEngine;
begin
  lua_pushinteger(LState, AValue);
  lua_setglobal(LState, PAnsiChar(AName));
end;

function TLuaEngine.DeclareGlobalLightUserData(AName: AnsiString; AValue: Pointer): TLuaEngine;
begin
  lua_pushlightuserdata(LState, AValue);
  lua_setglobal(LState, PAnsiChar(AName));
end;

function TLuaEngine.DeclareGlobalNil(AName: AnsiString): TLuaEngine;
begin
  lua_pushnil(LState);
  lua_setglobal(LState, PAnsiChar(AName));
end;

function TLuaEngine.DeclareGlobalNumber(AName: AnsiString; AValue: Double): TLuaEngine;
begin
  lua_pushnumber(LState, AValue);
  lua_setglobal(LState, PAnsiChar(AName));
end;

function TLuaEngine.DeclareGlobalString(AName: AnsiString; AValue: AnsiString): TLuaEngine;
begin
  lua_pushstring(LState, PAnsiChar(AValue));
  lua_setglobal(LState, PAnsiChar(AName));
end;

destructor TLuaEngine.Destroy;
begin
  lua_close(LState);
  inherited;
end;

procedure TLuaEngine.Execute;
var
  err: PAnsiChar;
  r: Integer;
begin
  r := lua_pcall(LState, 0, 0, 0);
  case r of
    // success
    0:
      begin
        lua_pop(LState, lua_gettop(LState));
      end;
    // a runtime error.
    LUA_ERRRUN:
      begin
        err := lua_tostring(LState, -1);
        raise ELuaException.CreateFmt('Runtime error [%s]', [err]);
        lua_pop(LState, 1);
      end;
    // memory allocation error. For such errors, Lua does not call the error handler function.
    LUA_ERRMEM:
      begin
        err := lua_tostring(LState, -1);
        raise ELuaException.CreateFmt('Memory allocation error [%s]', [err]);
        lua_pop(LState, 1);
      end;
    // error while running the error handler function.
    LUA_ERRERR:
      begin
        err := lua_tostring(LState, -1);
        raise ELuaException.CreateFmt('Error while running the error handler function [%s]', [err]);
        lua_pop(LState, 1);
      end;
    else
      begin
        err := lua_tostring(LState, -1);
        raise ELuaException.CreateFmt('Unknown Error [%s]', [err]);
        lua_pop(LState, 1);
      end;
  end;
end;

function TLuaEngine.GetGlobal(AName: AnsiString): ILuaValue;
begin
  lua_getglobal(LState, PAnsiChar(AName));
  Result := TLuaValue.Create(LState, -1);
end;

procedure TLuaEngine.LoadFromFile(const AFileName: String);
var
  err: PAnsiChar;
begin
  if luaL_loadfile(LState, PAnsiChar(AFileName)) <> 0 then
  begin
    err := lua_tostring(LState, -1);
    lua_pop(LState, 1);
    raise ELuaException.Create(err);
  end;
end;

procedure TLuaEngine.LoadScript(const AStream: TStream; AOwnStream: boolean);
var
  sr: TStreamReader;
  s: string;
begin
  sr := TStreamReader.Create(AStream);
  try
    if AOwnStream then
      sr.OwnStream;
    s := sr.ReadToEnd;
    LoadScript(s);
  finally
    sr.Free;
  end;
end;

procedure TLuaEngine.LoadScript(const AScript: AnsiString);
var
  err: PAnsiChar;
begin
  if luaL_loadstring(LState,
    PAnsiChar(AScript)) <> 0 then
  begin
    err := lua_tostring(LState, -1);
    lua_pop(LState, 1);
    raise ELuaException.Create(err);
  end;
end;

{ TLuaValue }

constructor TLuaValue.Create(LuaState: Plua_State; StackIndex: Integer);
var
  a: AnsiString;
  _lua_CFunction: lua_CFunction;
  _pluastate: Plua_State;
begin
  inherited Create;
  FLuaValueType := GetLuaValueType(LuaState, StackIndex);

  case FLuaValueType of
    lvtNil:
      FValue := nil;

    lvtNumber:
      FValue := lua_tonumber(LuaState, StackIndex);

    lvtInteger:
      FValue := lua_tointeger(LuaState, StackIndex);

    lvtString:
      begin
        a := lua_tostring(LuaState, StackIndex);
        FValue := a;
      end;

    lvtBoolean:
      FValue := lua_toboolean(LuaState, StackIndex) = 1;

    lvtLightUserData:
      FValue := lua_topointer(LuaState, StackIndex);

    lvtTable:
      begin
        FValue := GetAsLuaTable(LuaState, StackIndex);
      end;

    lvtFunction:
      begin
        _lua_CFunction := lua_tocfunction(LuaState, StackIndex);
        FValue := nil; { todo }
      end;

    lvtUserData:
      FValue := lua_touserdata(LuaState, StackIndex);

    lvtThread:
      begin
        _pluastate := lua_tothread(LuaState, StackIndex);
        FValue := nil; { todo }
      end;
  end;

end;

class function TLuaValue.GetLuaValueType(LuaState: Plua_State; StackIndex: Integer):
  TLuaValueType;
begin
  case lua_type(LuaState, StackIndex) of
    LUA_TNIL:
      Result := lvtNil;
    LUA_TNUMBER:
      Result := lvtNumber;
    LUA_TBOOLEAN:
      Result := lvtBoolean;
    LUA_TSTRING:
      Result := lvtString;
    LUA_TTABLE:
      Result := lvtTable;
    LUA_TFUNCTION:
      Result := lvtFunction;
    LUA_TUSERDATA:
      Result := lvtUserData;
    LUA_TTHREAD:
      Result := lvtThread;
    LUA_TLIGHTUSERDATA:
      Result := lvtLightUserData;
    LUA_TNONE:
      raise ELuaException.Create('Invalid stack index location');
  end;
end;

procedure TLuaValue.CheckType(LuaValueType: TLuaValueType);
begin
  if FLuaValueType <> LuaValueType then
    raise ELuaException.Create('Cannot access value as ' + inttostr(Integer(LuaValueType)));
end;

function TLuaValue.GetAsBoolean:
  boolean;
begin
  CheckType(lvtBoolean);
  Result := FValue.AsBoolean;
end;

function TLuaValue.GetAsInteger: Integer;
begin
  CheckType(lvtInteger);
  Result := FValue.AsInteger;
end;

function TLuaValue.GetAsLightUserData: Pointer;
begin
  CheckType(lvtLightUserData);
  Result := FValue.AsType<Pointer>;
end;

function TLuaValue.GetAsLuaTable(LuaState: Plua_State; StackIndex: Integer): TValue;
begin
  { todo }
end;

function TLuaValue.GetAsNumber: Double;
begin
  CheckType(lvtNumber);
  Result := FValue.AsExtended;
end;

function TLuaValue.GetAsString: AnsiString;
begin
  CheckType(lvtString);
  Result := FValue.AsString;
end;

function TLuaValue.IsBoolean: boolean;
begin
  Result := FLuaValueType = lvtBoolean;
end;

function TLuaValue.IsInteger: boolean;
begin
  Result := FLuaValueType = lvtInteger;
end;

function TLuaValue.IsLightUserData: boolean;
begin
  Result := FLuaValueType = lvtLightUserData;
end;

function TLuaValue.IsNil: boolean;
begin
  Result := FLuaValueType = lvtNil;
end;

function TLuaValue.IsNumber: boolean;
begin
  Result := FLuaValueType = lvtNumber;
end;

function TLuaValue.IsString: boolean;
begin
  Result := FLuaValueType = lvtString;
end;

end.
