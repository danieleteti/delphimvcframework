unit LuaBind.CustomType.PODO;

interface

uses
  LuaBind;

type
  TLuaDelphiObjectExposerLibraries = class(TInterfacedObject, ILuaLibraryLoader)
  public
    procedure Execute(ALuaEngine: TLuaEngine);
  end;

procedure ExposeDelphiObject(ALuaEngine: TLuaEngine; AObject: TObject;
  AVariableName: AnsiString);

implementation

uses
  LuaBind.Intf,
  System.Rtti,
  System.SysUtils,
  System.TypInfo;

function InvokeMethod(Obj: TObject; MethodName: AnsiString; params: TArray<TValue>): TValue;
var
  ctx  : TRTTIContext;
  _type: TRttiType;
  _m   : TRttiMethod;
  _p   : TRttiProperty;
  _pars: TArray<TRttiParameter>;
  par  : TRttiParameter;
  i    : Integer;
begin
  ctx := TRTTIContext.Create;
  try
    _type := ctx.GetType(Obj.ClassInfo);
    _m := _type.GetMethod(MethodName);
    if _m = nil then
    begin
      _p := _type.GetProperty(MethodName);
      if _p = nil then
        raise Exception.CreateFmt('Method or property [%s] not found', [MethodName]);
      Result := _p.GetValue(Obj);
    end
    else
    begin
      _pars := _m.GetParameters;
      i := 0;
      for par in _pars do
      begin
        if par.ParamType.TypeKind in [tkInteger, tkInt64] then
        begin
          if params[i].Kind in [tkFloat] then
            params[i] := Trunc(params[i].AsExtended);
        end;
      end;
      Result := _m.Invoke(Obj, params);
    end;
  finally
    ctx.Free;
  end;
end;

procedure SetProperty(Obj: TObject; const PropertyName: string;
  const Value: TValue);
var
  Prop     : TRttiProperty;
  ARttiType: TRttiType;
  ctx      : TRTTIContext;
begin
  ARttiType := ctx.GetType(Obj.ClassType);
  if not Assigned(ARttiType) then
    raise Exception.CreateFmt('Cannot get RTTI for type [%s]',
      [ARttiType.ToString]);
  Prop := ARttiType.GetProperty(PropertyName);
  if not Assigned(Prop) then
    raise Exception.CreateFmt('Cannot get RTTI for property [%s.%s]',
      [ARttiType.ToString, PropertyName]);
  if Prop.IsWritable then
  begin
    if Prop.PropertyType.TypeKind in [tkInteger, tkInt64] then
      Prop.SetValue(Obj, Trunc(Value.AsExtended))
    else
      Prop.SetValue(Obj, Value);
  end
  else
    raise Exception.CreateFmt('Property is not writeable [%s.%s]',
      [ARttiType.ToString, PropertyName]);
end;

function _delphi_is_a_property(L: Plua_State): Integer; cdecl;
var
  lvt         : TLuaValueType;
  PropertyName: string;
  Obj         : TObject;
  propname    : TValue;
  ctx         : TRTTIContext;
  _rttitype   : TRttiType;
begin
  Result := 0;
  try
    assert(lua_isstring(L, - 1) = 1);
    assert(lua_islightuserdata(L, - 2));
    PropertyName := TLuaValue.PopTValueFromStack(L).AsString;
    Obj := TLuaValue.PopTValueFromStack(L).AsObject;
    ctx := TRTTIContext.Create;
    try
      _rttitype := ctx.GetType(Obj.ClassType);
      if Assigned(_rttitype.GetProperty(PropertyName)) then
        TLuaUtils.PushTValue(L, true)
      else
        TLuaUtils.PushTValue(L, false);
    finally
      ctx.Free;
    end;
    Result := 1;
  except
    on E: Exception do
    begin
      lua_pushstring(L, PAnsiChar(UTF8Encode(E.ClassName + ' ' + E.Message)));
      lua_error(L);
    end;
  end;
end;

// allows to create a delphi object using a parametersless constructor
function _delphi_create_object(L: Plua_State): Integer; cdecl;
var
  ParCount  : Integer;
  _classname: string;
  ctx       : TRTTIContext;
  _type     : TRttiType;
  Obj       : TObject;
  _m        : TRttiMethod;
  _mm       : TRttiMethod;
begin
  Result := 0;
  try
    ParCount := lua_gettop(L) - 1;
    if ParCount > 0 then
      raise ELuaConstructorException.Create('Constructor parameters are not supported');

    // Create the object
    _classname := TLuaValue.PopTValueFromStack(L).AsString;
    _type := ctx.FindType(_classname);
    if _type = nil then
      raise ELuaException.Create('Cannot find type ' + _classname +
        '. Is it a Fully Qualified Name?');

    _m := nil;
    for _mm in _type.AsInstance.GetDeclaredMethods do
      if _mm.IsConstructor and (Length(_mm.GetParameters) = 0) then
      begin
        _m := _mm;
        Break;
      end;

    if Assigned(_m) then
      Obj := _m.Invoke(_type.AsInstance.MetaclassType, []).AsObject
    else
      raise ELuaConstructorException.Create
        ('Cannot find a suitable constructor for ' + _type.AsInstance.QualifiedName);
    TLuaUtils.PushTValue(L, Obj);
    Result := 1;
  except
    on E: Exception do
    begin
      lua_pushstring(L, PAnsiChar(UTF8Encode(E.ClassName + ' ' + E.Message)));
      lua_error(L);
    end;
  end;
end;

function _delphi_set_property(L: Plua_State): Integer; cdecl;
var
  Value       : TValue;
  PropertyName: AnsiString;
  Obj         : TObject;
begin
  Result := 0;
  try
    assert(lua_isstring(L, - 2) = 1);
    assert(lua_islightuserdata(L, - 3));
    Value := TLuaValue.PopTValueFromStack(L);
    PropertyName := TLuaValue.PopTValueFromStack(L).AsString;
    Obj := TLuaValue.PopTValueFromStack(L).AsObject;
    SetProperty(Obj, PropertyName, Value);
    Result := 0;
  except
    on E: Exception do
    begin
      lua_pushstring(L, PAnsiChar(UTF8Encode(E.ClassName + ' ' + E.Message)));
      lua_error(L);
    end;
  end;
end;

function _delphi_call_method(L: Plua_State): Integer; cdecl;
var
  s           : AnsiString;
  params      : TArray<TValue>;
  maxindex    : Integer;
  i           : Integer;
  LuaValueType: TLuaValueType;
  MethodName  : AnsiString;
  Obj         : TObject;
  v           : TValue;
begin
  Result := 0;
  try
    assert(lua_istable(L, - 1));
    assert(lua_isstring(L, - 2) = 1);
    assert(lua_islightuserdata(L, - 3));

    lua_getfield(L, - 1, 'maxindex');
    maxindex := lua_tointeger(L, - 1);
    lua_pop(L, 1);
    if maxindex > - 1 then
    begin
      SetLength(params, maxindex + 1);
      for i := 0 to maxindex do
      begin
        s := '_' + IntToStr(i);
        lua_getfield(L, - 1, PAnsiChar(s));
        LuaValueType := TLuaValue.GetLuaValueType(L, - 1);
        params[i] := TLuaValue.GetTValueFromLuaValueType(LuaValueType, L, - 1);
        lua_pop(L, 1);
      end;
    end;
    lua_pop(L, 1); // pop the table

    // read the method name
    LuaValueType := TLuaValue.GetLuaValueType(L, - 1);
    if LuaValueType <> lvtString then
      raise ELuaException.Create('Bad error! Cannot find the method name');
    MethodName := TLuaValue.GetTValueFromLuaValueType(LuaValueType, L, - 1).AsString;
    lua_pop(L, 1); // remove the methodname

    // read the pointer
    LuaValueType := TLuaValue.GetLuaValueType(L, - 1);
    if LuaValueType <> lvtLightUserData then
      raise ELuaException.Create('Bad error! Cannot find the object ref');
    Obj := TLuaValue.GetTValueFromLuaValueType(LuaValueType, L, - 1).AsObject;
    lua_pop(L, 1); // remove the lightuserdata

    v := InvokeMethod(Obj, MethodName, params);
    if v.IsEmpty then
      Result := 0
    else
    begin
      Result := 1;
      TLuaUtils.PushTValue(L, v);
    end;
  except
    on E: Exception do
    begin
      lua_pushstring(L, PAnsiChar(UTF8Encode(E.ClassName + ' ' + E.Message)));
      lua_error(L);
    end;
  end;
end;

procedure ExposeDelphiObject(ALuaEngine: TLuaEngine;
  AObject:
  TObject;
  AVariableName:
  AnsiString);
begin
  ALuaEngine.DeclareGlobalLightUserData(AVariableName, AObject);
end;

{ TLuaDelphiObjectExposerLibraries }

procedure TLuaDelphiObjectExposerLibraries.Execute(ALuaEngine: TLuaEngine);
begin
  inherited;
  ALuaEngine.
    DeclareGlobalFunction('_delphi_call_method', @_delphi_call_method).
    DeclareGlobalFunction('_delphi_create_object', @_delphi_create_object).
    DeclareGlobalFunction('_delphi_is_a_property', @_delphi_is_a_property).
    DeclareGlobalFunction('_delphi_set_property', @_delphi_set_property);
end;

end.
