{$WARNINGS OFF}
unit LuaBind;

interface

uses LuaBind.Intf,
  System.Classes,
  System.SysUtils,
  System.Rtti,
  System.Generics.Collections;

type
  ELuaException = class(Exception)

  end;

  ELuaRuntimeException = class(ELuaException)

  end;

  ELuaConstructorException = class(ELuaException)

  end;

  ELuaSyntaxError = class(ELuaException)

  end;

  ELuaFilterException = class(ELuaSyntaxError)

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

  TLuaValueType = (lvtNil, lvtNumber, lvtInteger, lvtString, lvtBoolean,
    lvtLightUserData, lvtTable, lvtFunction, lvtUserData, lvtThread);

  TLuaValue = class(TInterfacedObject, ILuaValue)
  strict private
    FLuaValueType: TLuaValueType;

  strict private

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

  public
    class function GetAsLuaTable(LuaState: Plua_State;
      StackIndex: Integer): TValue;
    class function GetLuaValueType(LuaState: Plua_State; StackIndex: Integer)
      : TLuaValueType;
    class function GetTValueFromLuaValueType(LuaValueType: TLuaValueType;
      LuaState: Plua_State; StackIndex: Integer): TValue; static;
    class function PopTValueFromStack(LuaState: Plua_State): TValue; static;
  end;

  ILuaLibraryLoader = interface;

  TLuaEngine = class
  strict private
    procedure CheckLuaError(const r: Integer);

  private
    procedure InternalDeclareTable(AObject: TObject);

  protected
    LState: Plua_State;
    procedure InitLua;
    procedure CloseLua;

  public
    constructor Create; overload; virtual;
    constructor Create(const AScript: string); overload; virtual;
    destructor Destroy; override;
    function GetRawLuaState: Plua_State;

    procedure Reset;
    procedure LoadScript(const AScript: AnsiString); overload;
    procedure LoadScript(const AStream: TStream;
      AOwnStream: boolean = true); overload;
    procedure LoadFromFile(const AFileName: AnsiString);
    procedure Execute;
    procedure ExecuteFile(const AFileName: AnsiString);
    procedure ExecuteScript(const AScript: AnsiString);

    // PUSH methods for simple types
    function DeclareGlobalNil(AName: AnsiString): TLuaEngine;
    function DeclareGlobalNumber(AName: AnsiString; AValue: Double): TLuaEngine;
    function DeclareGlobalInteger(AName: AnsiString; AValue: Integer)
      : TLuaEngine;
    function DeclareGlobalString(AName: AnsiString; AValue: AnsiString)
      : TLuaEngine;
    function DeclareGlobalBoolean(AName: AnsiString; AValue: boolean)
      : TLuaEngine;
    function DeclareGlobalLightUserData(AName: AnsiString; AValue: Pointer)
      : TLuaEngine;
    function DeclareGlobalUserData(AName: AnsiString; AValue: Pointer)
      : TLuaEngine;

    // PUSH complex types
    function DeclareGlobalFunction(AName: AnsiString; AFunction: lua_CFunction)
      : TLuaEngine;

    function DeclareGlobalDelphiObjectAsTable(AObject: TObject;
      AVariableName: string): TLuaEngine;

    // Helpers to PUSH specific types coping properties or data into Lua tables
    function DeclareTable(const ATableName: AnsiString; AKeys: array of string;
      AValues: array of string): TLuaEngine; overload;
    function DeclareTable(const ATableName: AnsiString; AKeys: array of Integer;
      AValues: array of string): TLuaEngine; overload;
    function DeclareTable(const ATableName: AnsiString;
      ADictionary: TDictionary<TValue, TValue>): TLuaEngine; overload;
    function DeclareTable(const ATableName: AnsiString;
      ADictionary: TDictionary<string, string>): TLuaEngine; overload;
    function DeclareTable(const ATableName: AnsiString; AObject: TObject)
      : TLuaEngine; overload;

    function DeclareTable(const ATableName: AnsiString; __self: Pointer;
      AFunctions: TDictionary<string, lua_CFunction>;
      AData: TDictionary<string, TValue>
      ): TLuaEngine; overload;

    // GET methods for simple types
    function GetGlobal(AName: AnsiString): ILuaValue;

    // External load libraries
    procedure LoadExternalLibraries(ALuaLibraryLoader: ILuaLibraryLoader);

    // function
    function ExecuteFunction(FunctionName: AnsiString;
      const Params: array of TValue): TValue;

    // helpers
    class function ExecuteWithResult(AScript: AnsiString;
      const ParamNames: array of string;
      const ParamValues: array of string): string;
  end;

  ILuaLibraryLoader = interface
    ['{46A7894C-DDA8-4E15-95EB-B52463341FF1}']
    procedure Execute(ALuaEngine: TLuaEngine);
  end;

  TLuaUtils = class sealed
  public
    class procedure PushTValue(L: Plua_State; Value: TValue); static;
  end;

implementation

uses
  typinfo,
  LuaBind.DelphiObjects;

function internal_call_method(LState: Plua_State): Integer; cdecl;
begin
  // raise Exception.Create('Error Message');
  lua_pop(LState, 1);
  lua_pop(LState, 1);
  lua_pushcfunction(LState, @_delphi_call_method);
  Result := 1;
  // Result := lua_gettop(LState);
  // for I := 1 to Result do
  // begin
  // v := TLuaValue.PopTValueFromStack(LState);
  // end
  // // Result := _delphi_call_method(LState)
end;

class procedure TLuaUtils.PushTValue(L: Plua_State; Value: TValue);
var
  utf8s: RawByteString;
begin
  case Value.Kind of
    tkUnknown, tkChar, tkSet, tkMethod, tkVariant, tkArray, tkProcedure,
      tkRecord, tkInterface, tkDynArray, tkClassRef:
      begin
        lua_pushnil(L);
        // raise Exception.Create('Unsupported return type: ' + Value.ToString);
      end;
    tkInteger:
      lua_pushinteger(L, Value.AsInteger);
    tkEnumeration:
      begin
        if Value.IsType<boolean> then
        begin
          if Value.AsBoolean then
            lua_pushboolean(L, 1)
          else
            lua_pushboolean(L, 0);
        end
        else
          lua_pushinteger(L, Value.AsInteger);
      end;
    tkFloat:
      lua_pushnumber(L, Value.AsExtended);
    tkString, tkWChar, tkLString, tkWString, tkUString:
      begin
        utf8s := UTF8Encode(Value.AsString);
        lua_pushstring(L, PAnsiChar(utf8s));
      end;
    tkClass:
      lua_pushlightuserdata(L, Pointer(Value.AsObject));
    tkInt64:
      lua_pushnumber(L, Value.AsInt64);
    tkPointer:
      lua_pushlightuserdata(L, Pointer(Value.AsObject));
  end;
end;

{ TLuaEngine }

constructor TLuaEngine.Create;
begin
  inherited Create;
  InitLua;
end;

procedure TLuaEngine.CloseLua;
begin
  lua_close(LState);
end;

constructor TLuaEngine.Create(const AScript: string);
begin
  Create;
  LoadScript(AScript);
end;

function TLuaEngine.DeclareGlobalBoolean(AName: AnsiString; AValue: boolean)
  : TLuaEngine;
var
  b: Integer;
begin
  if AValue then
    b := 1
  else
    b := 0;
  lua_pushboolean(LState, b);
  lua_setglobal(LState, PAnsiChar(AName));
  Result := Self;
end;

function TLuaEngine.DeclareGlobalDelphiObjectAsTable(AObject: TObject;
  AVariableName: string): TLuaEngine;
begin
  // lua_createtable(L, 0, 0);
  // lua_createtable(L, 0, 1);
  assert(lua_isnil(LState, -1));
  lua_newtable(LState);
  assert(lua_istable(LState, -1));
  lua_newtable(LState);
  lua_pushcfunction(LState, @internal_call_method);
  lua_setfield(LState, -2, '__index');
  lua_setmetatable(LState, -2);
  lua_setglobal(LState, PAnsiChar(AnsiString(AVariableName)));

  // http://stackoverflow.com/questions/3449759/lua-c-api-and-metatable-functions
  // lua_newtable(LState);
  // lua_pushstring(LState, PAnsiChar('__handle__'));
  // lua_pushlightuserdata(LState, Pointer(AObject));
  // lua_settable(LState, - 3);
  // {
  // for prop in ctx.GetType(AObject.ClassInfo).GetProperties do
  // begin
  // v := prop.GetValue(AObject);
  // if v.Kind = tkUnknown then
  // continue;
  // k := prop.Name;
  // TLuaUtils.PushTValue(LState, k);
  // TLuaUtils.PushTValue(LState, v);
  // lua_settable(LState, - 3);
  // end;
  //
  // for method in ctx.GetType(AObject.ClassInfo).GetMethods
  // begin
  // if not (method.Visibility in [mvPublic, mvPublished]) then
  // continue;
  // k := method.Name;
  // TLuaUtils.PushTValue(LState, k);
  // lua_pushcfunction(LState, @internal_call_method);
  // lua_settable(LState, - 3);
  // end;
  // }
  //
  // lua_setglobal(LState, PAnsiChar(AnsiString(AVariableName)));
  //
  // // define metatable
  // // lua_createtable(LState, 0, 0);
  // lua_createtable(LState, 0, 1);
  // lua_pushcfunction(LState, @internal_call_method);
  // lua_setfield(LState, - 2, AnsiString('__index'));
  // lua_setmetatable(LState, - 2);
  // lua_setglobal(LState, PAnsiChar(AnsiString(AVariableName)));
end;

function TLuaEngine.DeclareGlobalFunction(AName: AnsiString;
  AFunction: lua_CFunction): TLuaEngine;
begin
  lua_pushcfunction(LState, AFunction);
  lua_setglobal(LState, PAnsiChar(AName));
  Result := Self;
end;

function TLuaEngine.DeclareGlobalInteger(AName: AnsiString; AValue: Integer)
  : TLuaEngine;
begin
  lua_pushinteger(LState, AValue);
  lua_setglobal(LState, PAnsiChar(AName));
  Result := Self;
end;

function TLuaEngine.DeclareGlobalLightUserData(AName: AnsiString;
  AValue: Pointer): TLuaEngine;
begin
  lua_pushlightuserdata(LState, AValue);
  lua_setglobal(LState, PAnsiChar(AName));
  Result := Self;
end;

function TLuaEngine.DeclareGlobalNil(AName: AnsiString): TLuaEngine;
begin
  lua_pushnil(LState);
  lua_setglobal(LState, PAnsiChar(AName));
  Result := Self;
end;

function TLuaEngine.DeclareGlobalNumber(AName: AnsiString; AValue: Double)
  : TLuaEngine;
begin
  lua_pushnumber(LState, AValue);
  lua_setglobal(LState, PAnsiChar(AName));
  Result := Self;
end;

function TLuaEngine.DeclareGlobalString(AName: AnsiString; AValue: AnsiString)
  : TLuaEngine;
begin
  lua_pushstring(LState, PAnsiChar(AValue));
  lua_setglobal(LState, PAnsiChar(AName));
  Result := Self;
end;

function TLuaEngine.DeclareGlobalUserData(AName: AnsiString; AValue: Pointer): TLuaEngine;
begin
  //raise ELuaException.Create('Not implemented');
  //Result := Self;
end;

function TLuaEngine.DeclareTable(const ATableName: AnsiString;
  AKeys: array of Integer; AValues: array of string): TLuaEngine;
var
  I: Integer;
  k: Integer;
  v: string;
begin
  lua_newtable(LState);
  for I := 0 to high(AKeys) do
  begin
    k := AKeys[I];
    v := AValues[I];
    TLuaUtils.PushTValue(LState, k);
    TLuaUtils.PushTValue(LState, v);
    lua_settable(LState, -3);
  end;
  lua_setglobal(LState, PAnsiChar(ATableName));
  Result := Self;
end;

function TLuaEngine.DeclareTable(const ATableName: AnsiString;
  ADictionary: TDictionary<TValue, TValue>): TLuaEngine;
var
  key: TValue;
begin
  lua_newtable(LState);
  for key in ADictionary.Keys do
  begin
    TLuaUtils.PushTValue(LState, key);
    TLuaUtils.PushTValue(LState, ADictionary.Items[key]);
    lua_settable(LState, -3);
  end;
  lua_setglobal(LState, PAnsiChar(ATableName));
  Result := Self;
end;

function TLuaEngine.DeclareTable(const ATableName: AnsiString;
  ADictionary: TDictionary<string, string>): TLuaEngine;
var
  key: string;
begin
  lua_newtable(LState);
  for key in ADictionary.Keys do
  begin
    TLuaUtils.PushTValue(LState, key);
    TLuaUtils.PushTValue(LState, ADictionary.Items[key]);
    lua_settable(LState, -3);
  end;
  lua_setglobal(LState, PAnsiChar(ATableName));
end;

function TLuaEngine.DeclareTable(const ATableName: AnsiString;
  AKeys: array of string; AValues: array of string): TLuaEngine;
var
  I: Integer;
  k: string;
  v: string;
begin
  lua_newtable(LState);
  for I := 0 to high(AKeys) do
  begin
    k := AKeys[I];
    v := AValues[I];
    TLuaUtils.PushTValue(LState, k);
    TLuaUtils.PushTValue(LState, v);
    lua_settable(LState, -3);
  end;
  lua_setglobal(LState, PAnsiChar(ATableName));
end;

destructor TLuaEngine.Destroy;
begin
  CloseLua;
  inherited;
end;

procedure TLuaEngine.Execute;
var
  r: Integer;
begin
  r := lua_pcall(LState, 0, 0, 0);
  CheckLuaError(r);
end;

procedure TLuaEngine.CheckLuaError(const r: Integer);
var
  err: PAnsiChar;
begin
  case r of
    // success
    0:
      begin

      end;
    // a runtime error.
    LUA_ERRRUN:
      begin
        err := lua_tostring(LState, -1);
        lua_pop(LState, 1);
        raise ELuaRuntimeException.CreateFmt('Runtime error [%s]', [err]);
      end;
    // memory allocation error. For such errors, Lua does not call the error handler function.
    LUA_ERRMEM:
      begin
        err := lua_tostring(LState, -1);
        lua_pop(LState, 1);
        raise ELuaException.CreateFmt('Memory allocation error [%s]', [err]);
      end;
    // error while running the error handler function.
    LUA_ERRERR:
      begin
        err := lua_tostring(LState, -1);
        lua_pop(LState, 1);
        raise ELuaException.CreateFmt
          ('Error while running the error handler function [%s]', [err]);
      end;
    LUA_ERRSYNTAX:
      begin
        err := lua_tostring(LState, -1);
        lua_pop(LState, 1);
        raise ELuaSyntaxError.CreateFmt('Syntax Error [%s]', [err]);
      end
  else
    begin
      err := lua_tostring(LState, -1);
      lua_pop(LState, 1);
      raise ELuaException.CreateFmt('Unknown Error [%s]', [err]);
    end;
  end;
end;

procedure TLuaEngine.ExecuteFile(const AFileName: AnsiString);
begin
  LoadFromFile(AFileName);
  Execute;
end;

function TLuaEngine.ExecuteFunction(FunctionName: AnsiString;
  const Params: array of TValue): TValue;
var
  p: TValue;
  r: Integer;
begin
  lua_getglobal(LState, PAnsiChar(FunctionName));
  for p in Params do
    TLuaUtils.PushTValue(LState, p);
  r := lua_pcall(LState, Length(Params), 1, 0);
  CheckLuaError(r);
  Result := TLuaValue.PopTValueFromStack(LState);
end;

function TLuaEngine.GetGlobal(AName: AnsiString): ILuaValue;
begin
  lua_getglobal(LState, PAnsiChar(AName));
  Result := TLuaValue.Create(LState, -1);
end;

function TLuaEngine.GetRawLuaState: Plua_State;
begin
  Result := LState;
end;

procedure TLuaEngine.InitLua;
begin
  LState := lua_open;
  if not assigned(LState) then
    raise ELuaException.Create('Cannot initialize Lua');
  luaL_openlibs(LState);
end;

procedure TLuaEngine.LoadExternalLibraries(ALuaLibraryLoader
  : ILuaLibraryLoader);
begin
  ALuaLibraryLoader.Execute(Self);
end;

procedure TLuaEngine.LoadFromFile(const AFileName: AnsiString);
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

procedure TLuaEngine.Reset;
begin
  CloseLua;
  InitLua;
end;

procedure TLuaEngine.ExecuteScript(const AScript: AnsiString);
var
  err: PAnsiChar;
begin
  if luaL_dostring(LState, PAnsiChar(AScript)) then
  begin
    err := lua_tostring(LState, -1);
    lua_pop(LState, 1);
    raise ELuaException.Create(err);
  end;
end;

class function TLuaEngine.ExecuteWithResult(AScript: AnsiString;
  const ParamNames, ParamValues: array of string): string;
var
  L: TLuaEngine;
  I: Integer;
begin
  L := TLuaEngine.Create;
  try
    L.LoadScript(AScript);
    if Length(ParamNames) <> Length(ParamValues) then
      raise ELuaRuntimeException.Create
        ('Number of params names and param values is not equals');
    for I := 0 to Length(ParamNames) - 1 do
      L.DeclareGlobalString(ParamNames[I], ParamValues[I]);
    L.Execute;
  finally
    L.Free;
  end;
end;

procedure TLuaEngine.LoadScript(const AScript: AnsiString);
var
  err: PAnsiChar;
begin
  if luaL_loadstring(LState, PAnsiChar(AScript)) <> 0 then
  begin
    err := lua_tostring(LState, -1);
    lua_pop(LState, 1);
    raise ELuaException.Create(err);
  end;
end;

procedure TLuaEngine.InternalDeclareTable(AObject: TObject);
var
  prop: TRTTIProperty;
  ctx: TRTTIContext;
  properties: TArray<TRTTIProperty>;
  k: AnsiString;
  Value: TValue;
  o: TObject;
begin
  ctx := TRTTIContext.Create;
  try
    lua_newtable(LState);
    properties := ctx.GetType(AObject.ClassType).GetProperties;
    for prop in properties do
    begin
      if not(prop.Visibility in [mvPublic, mvPublished]) then
        continue;
      k := prop.Name;
      TLuaUtils.PushTValue(LState, k);
      Value := prop.GetValue(AObject);
      if Value.TypeInfo^.Kind = tkClass then
      begin
        o := Value.AsObject;
        if not assigned(o) then
          lua_pushnil(LState)
        else
          InternalDeclareTable(Value.AsObject);
      end
      else
      begin
        if Value.Kind = tkEnumeration then
        begin
          Value := prop.GetValue(AObject).AsOrdinal;
        end;
        TLuaUtils.PushTValue(LState, Value)
      end;
      lua_settable(LState, -3);
    end;
  finally
    ctx.Free;
  end;
end;

function TLuaEngine.DeclareTable(const ATableName: AnsiString; AObject: TObject)
  : TLuaEngine;
begin
  InternalDeclareTable(AObject);
  lua_setglobal(LState, PAnsiChar(ATableName));
end;

function TLuaEngine.DeclareTable(const ATableName: AnsiString; __self: Pointer;
  AFunctions: TDictionary<string, lua_CFunction>;
  AData: TDictionary<string, TValue>): TLuaEngine;
var
  key: string;
begin
  lua_newtable(LState);
  TLuaUtils.PushTValue(LState, '__self');
  lua_pushlightuserdata(LState, __self);
  lua_settable(LState, -3);
  for key in AFunctions.Keys do
  begin
    TLuaUtils.PushTValue(LState, key);
    lua_pushcfunction(LState, AFunctions[key]);
    lua_settable(LState, -3);
  end;

  if Assigned(AData) then
  begin
    for key in AData.Keys do
    begin
      TLuaUtils.PushTValue(LState, key);
      TLuaUtils.PushTValue(LState, AData[key]);
      lua_settable(LState, -3);
    end;
  end;

  lua_setglobal(LState, PAnsiChar(ATableName));
end;

{ TLuaValue }

class function TLuaValue.GetTValueFromLuaValueType(LuaValueType: TLuaValueType;
  LuaState: Plua_State; StackIndex: Integer): TValue;
var
  a: AnsiString;
begin
  case LuaValueType of
    lvtNil:
      Result := nil;

    lvtNumber:
      Result := lua_tonumber(LuaState, StackIndex);

    lvtInteger:
      Result := lua_tointeger(LuaState, StackIndex);

    lvtString:
      begin
        a := lua_tostring(LuaState, StackIndex);
        Result := a;
      end;

    lvtBoolean:
      Result := lua_toboolean(LuaState, StackIndex) = 1;

    lvtLightUserData:
      Result := TObject(lua_topointer(LuaState, StackIndex));

    lvtTable:
      begin
        Result := GetAsLuaTable(LuaState, StackIndex);
      end;

    lvtFunction:
      begin
        raise ELuaException.Create('Not implemented');
        // _lua_CFunction := lua_tocfunction(LuaState, StackIndex);
        Result := nil; { todo }
      end;

    lvtUserData:
      raise ELuaException.Create('UserData not allowed here');

    lvtThread:
      begin
        raise ELuaException.Create('Not implemented');
        // _pluastate := lua_tothread(LuaState, StackIndex);
        Result := nil; { todo }
      end;
  end;
end;

constructor TLuaValue.Create(LuaState: Plua_State; StackIndex: Integer);
begin
  inherited Create;
  FLuaValueType := GetLuaValueType(LuaState, StackIndex);
  FValue := GetTValueFromLuaValueType(FLuaValueType, LuaState, StackIndex);
end;

class function TLuaValue.GetLuaValueType(LuaState: Plua_State;
  StackIndex: Integer): TLuaValueType;
begin
  Result := lvtNil;
  case lua_type(LuaState, StackIndex) of
    lua_tnil:
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
    raise ELuaException.Create('Cannot access value as ' +
      GetEnumName(TypeInfo(TLuaValueType), Ord(LuaValueType)) + ' while it is '
      + GetEnumName(TypeInfo(TLuaValueType), Ord(FLuaValueType)));
end;

function TLuaValue.GetAsBoolean: boolean;
begin
  CheckType(lvtBoolean);
  Result := FValue.AsBoolean;
end;

function TLuaValue.GetAsInteger: Integer;
begin
  Result := 0;
  if Self.FLuaValueType = lvtNumber then
  begin
    if GetAsNumber = Trunc(GetAsNumber) then
      Result := Trunc(GetAsNumber)
    else
      CheckType(lvtInteger);
  end
  else
  begin
    CheckType(lvtInteger);
    Result := FValue.AsInteger;
  end;
end;

function TLuaValue.GetAsLightUserData: Pointer;
begin
  CheckType(lvtLightUserData);
  Result := Pointer(FValue.AsObject);
end;

class function TLuaValue.GetAsLuaTable(LuaState: Plua_State;
  StackIndex: Integer): TValue;
begin
  raise ELuaException.Create('Not implemented');
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

class function TLuaValue.PopTValueFromStack(LuaState: Plua_State): TValue;
var
  lvt: TLuaValueType;
begin
  lvt := TLuaValue.GetLuaValueType(LuaState, -1);
  Result := TLuaValue.GetTValueFromLuaValueType(lvt, LuaState, -1);
  lua_pop(LuaState, 1);
end;

end.
