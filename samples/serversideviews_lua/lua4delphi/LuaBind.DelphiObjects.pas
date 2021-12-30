unit LuaBind.DelphiObjects;

interface

uses
  LuaBind.Intf;

function _delphi_call_method(const L: Plua_State): Integer; cdecl;

implementation

uses
  System.Rtti,
  LuaBind,
  System.SysUtils,
  System.TypInfo;

function InvokeMethod(Obj: TObject; MethodName: AnsiString; params: TArray<TValue>): TValue;
var
  ctx: TRTTIContext;
  _type: TRttiType;
  _m: TRttiMethod;
  _p: TRttiProperty;
  _pars: TArray<TRttiParameter>;
  par: TRttiParameter;
  i: Integer;
  LMethodName: String;
begin
  LMethodName := String(MethodName);
  ctx := TRTTIContext.Create;
  try
    _type := ctx.GetType(Obj.ClassInfo);
    _m := _type.GetMethod(LMethodName);
    if _m = nil then
    begin
      _p := _type.GetProperty(LMethodName);
      if _p = nil then
        raise Exception.CreateFmt('Method or property [%s] not found', [LMethodName]);
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

function _delphi_call_method(const L: Plua_State): Integer; cdecl;
var
  s: AnsiString;
  params: TArray<TValue>;
  maxindex: Integer;
  i: Integer;
  LuaValueType: TLuaValueType;
  MethodName: AnsiString;
  Obj: TObject;
  v: TValue;
begin
  Result := 0;
  try
    Result := lua_gettop(L);
    assert(lua_istable(L, 1));
    assert(lua_isstring(L, 2) = 1);

    MethodName := lua_tostring(L, 2);

    // assert(lua_islightuserdata(L, - 3));

    // lua_getfield(L, - 1, 'maxindex');
    // maxindex := lua_tointeger(L, - 1); //decomment this?7
    maxindex := 0; { TODO -oOwner -cGeneral : this method is in alpha stage }

    lua_pop(L, 1);
    if maxindex > - 1 then
    begin
      SetLength(params, maxindex + 1);
      for i := 0 to maxindex do
      begin
        s := '_' + AnsiString(IntToStr(i));
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
    MethodName := AnsiString(TLuaValue.GetTValueFromLuaValueType(LuaValueType, L, - 1).AsString);
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

end.
