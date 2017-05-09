unit MVCFramework.SystemJSONUtils;

{$I dmvcframework.inc}

interface

uses
  MVCFramework.TypesAliases, MVCFramework.RESTClient, System.Rtti;

type
  TSystemJSON = class sealed
  private
    class var CTX: TRttiContext;
    class function GetProperty(Obj: TObject;
      const PropertyName: string): TValue; static;
  public
    class function BodyAsJSONValue(const RESTResponse: IRESTResponse): TJSONValue;
    class function BodyAsJSONObject(const RESTResponse: IRESTResponse): TJSONObject;
    class function BodyAsJSONArray(const RESTResponse: IRESTResponse): TJSONArray;
    class function JSONValueToString(JSONValue: TJSONValue; const Owns: Boolean = true): String;
    class function GetPair(JSONObject: TJSONObject; PropertyName: string)
      : TJSONPair;
    class function GetStringDef(JSONObject: TJSONObject; PropertyName: string;
      DefaultValue: string = ''): string;
    class function GetNumberDef(JSONObject: TJSONObject; PropertyName: string;
      DefaultValue: Extended = 0): Extended;
    class function GetJSONObj(JSONObject: TJSONObject; PropertyName: string)
      : TJSONObject;
    class function GetJSONArray(JSONObject: TJSONObject; PropertyName: string)
      : TJSONArray;
    class function GetIntegerDef(JSONObject: TJSONObject; PropertyName: string;
      DefaultValue: Integer = 0): Integer;
    class function GetInt64Def(JSONObject: TJSONObject; PropertyName: string;
      DefaultValue: Int64 = 0): Int64;
    class function GetBooleanDef(JSONObject: TJSONObject; PropertyName: string;
      DefaultValue: boolean = false): boolean;
    class function PropertyExists(JSONObject: TJSONObject;
      PropertyName: string): boolean;
  end;

implementation

uses
  MVCFramework.Commons, System.SysUtils;

class function TSystemJSON.BodyAsJSONArray(const RESTResponse: IRESTResponse): TJSONArray;
begin
  Result := TSystemJSON.BodyAsJSONValue(RESTResponse) as TJSONArray;
end;

class function TSystemJSON.BodyAsJSONObject(const RESTResponse: IRESTResponse): TJSONObject;
begin
  Result := TSystemJSON.BodyAsJSONValue(RESTResponse) as TJSONObject;
end;

class function TSystemJSON.BodyAsJSONValue(const RESTResponse: IRESTResponse): TJSONValue;
var
  lBodyAsJSONValue: TJSONValue;
begin
  lBodyAsJSONValue := TJSONObject.ParseJSONValue(RESTResponse.BodyAsString);
  if lBodyAsJSONValue = nil then
    raise EMVCException.Create('Invalid JSON');
  Result := lBodyAsJSONValue;
end;

class function TSystemJSON.JSONValueToString(JSONValue: TJSONValue;
  const Owns: Boolean): String;
begin
  Result := JSONValue.ToJSON;
  if Owns then
  begin
    JSONValue.Free;
  end;
end;

class function TSystemJSON.GetBooleanDef(JSONObject: TJSONObject;
  PropertyName: string; DefaultValue: boolean): boolean;
var
  pair: TJSONPair;
begin
  pair := GetPair(JSONObject, PropertyName);
  if pair = nil then
    Exit(DefaultValue);
  if pair.JsonValue is TJSONFalse then
    Exit(false)
  else if pair.JsonValue is TJSONTrue then
    Exit(True)
  else
    raise EMVCException.CreateFmt('Property %s is not a Boolean Property',
      [PropertyName]);
end;

class function TSystemJSON.GetInt64Def(JSONObject: TJSONObject;
  PropertyName: string; DefaultValue: Int64): Int64;
var
  pair: TJSONPair;
begin
  pair := GetPair(JSONObject, PropertyName);
  if pair = nil then
    Exit(DefaultValue);
  if pair.JsonValue is TJSONNumber then
    Exit(TJSONNumber(pair.JsonValue).AsInt64)
  else
    raise EMVCException.CreateFmt('Property %s is not a Int64 Property',
      [PropertyName]);
end;

class function TSystemJSON.GetIntegerDef(JSONObject: TJSONObject;
  PropertyName: string; DefaultValue: Integer): Integer;
var
  pair: TJSONPair;
begin
  pair := GetPair(JSONObject, PropertyName);
  if pair = nil then
    Exit(DefaultValue);
  if pair.JsonValue is TJSONNumber then
    Exit(TJSONNumber(pair.JsonValue).AsInt)
  else
    raise EMVCException.CreateFmt('Property %s is not an Integer Property',
      [PropertyName]);
end;

class function TSystemJSON.GetJSONArray(JSONObject: TJSONObject;
  PropertyName: string): TJSONArray;
var
  pair: TJSONPair;
begin
  pair := GetPair(JSONObject, PropertyName);
  if pair = nil then
    Exit(nil);
  if pair.JsonValue is TJSONArray then
    Exit(TJSONArray(pair.JsonValue))
  else
    raise EMVCException.Create('Property is not a JSONArray');
end;

class function TSystemJSON.GetJSONObj(JSONObject: TJSONObject;
  PropertyName: string): TJSONObject;
var
  pair: TJSONPair;
begin
  pair := GetPair(JSONObject, PropertyName);
  if pair = nil then
    Exit(nil);
  if pair.JsonValue is TJSONObject then
    Exit(TJSONObject(pair.JsonValue))
  else
    raise EMVCException.Create('Property is not a JSONObject');
end;

class function TSystemJSON.PropertyExists(JSONObject: TJSONObject;
  PropertyName: string): boolean;
begin
  Result := Assigned(GetPair(JSONObject, PropertyName));
end;

class function TSystemJSON.GetNumberDef(JSONObject: TJSONObject;
  PropertyName: string; DefaultValue: Extended): Extended;
var
  pair: TJSONPair;
begin
  pair := GetPair(JSONObject, PropertyName);
  if pair = nil then
    Exit(DefaultValue);
  if pair.JsonValue is TJSONNumber then
    Exit(TJSONNumber(pair.JsonValue).AsDouble)
  else
    raise EMVCException.Create('Property is not a Number Property');
end;

class function TSystemJSON.GetPair(JSONObject: TJSONObject; PropertyName: string)
  : TJSONPair;
var
  pair: TJSONPair;
begin
  if not Assigned(JSONObject) then
    raise EMVCException.Create('JSONObject is nil');
  pair := JSONObject.Get(PropertyName);
  Result := pair;
end;

class function TSystemJSON.GetProperty(Obj: TObject;
  const PropertyName: string): TValue;
var
  Prop: TRttiProperty;
  ARTTIType: TRttiType;
begin
  ARTTIType := ctx.GetType(Obj.ClassType);
  if not Assigned(ARTTIType) then
    raise EMVCException.CreateFmt('Cannot get RTTI for type [%s]',
      [ARTTIType.ToString]);
  Prop := ARTTIType.GetProperty(PropertyName);
  if not Assigned(Prop) then
    raise EMVCException.CreateFmt('Cannot get RTTI for property [%s.%s]',
      [ARTTIType.ToString, PropertyName]);
  if Prop.IsReadable then
    Result := Prop.GetValue(Obj)
  else
    raise EMVCException.CreateFmt('Property is not readable [%s.%s]',
      [ARTTIType.ToString, PropertyName]);
end;

class function TSystemJSON.GetStringDef(JSONObject: TJSONObject;
  PropertyName, DefaultValue: string): string;
var
  pair: TJSONPair;
begin
  pair := GetPair(JSONObject, PropertyName);
  if pair = nil then
    Exit(DefaultValue);
  if pair.JsonValue is TJSONString then
    Exit(TJSONString(pair.JsonValue).Value)
  else
    raise EMVCException.Create('Property is not a String Property');
end;

end.
