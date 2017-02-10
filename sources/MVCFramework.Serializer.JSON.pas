// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2017 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// *************************************************************************** }

unit MVCFramework.Serializer.JSON;

interface

{$I dmvcframework.inc}


uses MVCFramework.Serializer.Intf
    , Data.DB
    , System.Rtti
    , System.SysUtils
    , System.Classes
    , MVCFramework.TypesAliases, MVCFramework.DuckTyping, System.TypInfo
    ;

type
  TMVCJSONSerializer = class(TInterfacedObject, IMVCSerializer)
  private
    class var CTX: TRTTIContext;
    function PropertyMustBeSkipped(const ARTTIProperty: TRTTIProperty): Boolean;
    function SerializeFloatProperty(AObject: TObject;
      ARTTIProperty: TRTTIProperty): TJSONValue; overload; deprecated;
    function SerializeFloatProperty(AElementType: TRTTIType; AValue: TValue): TJSONValue; overload;
    // function SerializeFloatField(AObject: TObject; ARttiField: TRttiField): TJSONValue;
    // function SerializeEnumerationProperty(AObject: TObject;
    // ARTTIProperty: TRttiProperty): TJSONValue; overload; deprecated;
    function SerializeEnumerationProperty(AElementType: TRTTIType; AValue: TValue): TJSONValue; overload;
    function SerializeTValue(AElementType: TRTTIType; AValue: TValue; AAttributes: TArray<TCustomAttribute>)
      : TJSONValue;
    function SerializeRecord(AElementType: TRTTIType; AValue: TValue; AAttributes: TArray<TCustomAttribute>)
      : TJSONValue;
    function SerializeEnumerationField(AObject: TObject;
      ARttiField: TRttiField): TJSONValue;
    function DeserializeFloat(ARTTIType: TRTTIType; AJSONValue: TJSONValue; APropName: String): TValue;
    function DeserializeEnumeration(ARTTIType: TRTTIType; AJSONValue: TJSONValue; AItemName: String): TValue;
    function DeserializeRecord(ARTTIType: TRTTIType; AJSONValue: TJSONValue;
      AAttributes: TArray<TCustomAttribute>; AItemName: String): TValue;
    function DeserializeArray(ARTTIType: TRTTIType; AJSONValue: TJSONValue;
      AAttributes: TArray<TCustomAttribute>; AItemName: String): TValue;
    function DeserializeTValue(AJValue: TJSONValue; AAttributes: TArray<TCustomAttribute>; AItemName: String): TValue;
    function DeserializeTValueWithDynamicType(AJValue: TJSONValue; AItemName: String): TValue;
    procedure DeSerializeBase64StringStream(aStream: TStream;
      const aBase64SerializedString: string);
    function ObjectToJSONValue(AObject: TObject;
      AIgnoredProperties: array of string): TJSONValue;
    function ObjectToJSONObjectFields(AObject: TObject): TJSONObject;
    function PropertyExists(JSONObject: TJSONObject;
      PropertyName: string): Boolean;
    function GetPair(JSONObject: TJSONObject; PropertyName: string): TJSONPair;
    function JSONObjectToObject(Clazz: TClass; AJSONObject: TJSONObject): TObject;
    function SerializeRTTIElement(ElementType: TRTTIType;
      ElementAttributes: TArray<TCustomAttribute>; Value: TValue; out OutputValue: TJSONValue): Boolean;
    procedure DeserializeRTTIElement(ElementType: TRTTIType;
      ElementAttributes: TArray<TCustomAttribute>; JSONValue: TJSONValue; APropName: String; var Value: TValue);
    procedure InternalJSONValueToObject(AJSONValue: TJSONValue; AObject: TObject);
    function SerializeTValueAsFixedNullableType(AValue: TValue;
      AValueTypeInfo: PTypeInfo): TJSONValue;
    procedure InternalDeserializeObject(ASerializedObject: string; AObject: TObject; AStrict: Boolean);
  protected
    { IMVCSerializer }
    function SerializeObject(AObject: TObject;
      AIgnoredProperties: array of string): string;
    function SerializeObjectStrict(AObject: TObject): String;
    function SerializeDataSet(ADataSet: TDataSet;
      AIgnoredFields: array of string): string;
    function SerializeCollection(AList: TObject;
      AIgnoredProperties: array of string): String;
    function SerializeCollectionStrict(AList: TObject): String;
    procedure DeserializeObject(ASerializedObject: string; AObject: TObject);
    procedure DeserializeObjectStrict(ASerializedObject: String; AObject: TObject);
    procedure DeserializeCollection(ASerializedObjectList: string; AList: IMVCList; AClazz: TClass);
    procedure DeserializeDataSet(ASerializedObject: String; const ADataSet: TDataSet);
  public
    const
    SERIALIZER_NAME = 'DELPHIJSON';
  end;

implementation

uses
  ObjectsMappers, MVCFramework.Patches, MVCFramework.RTTIUtils,
  MVCFramework.Serializer.Commons, Winapi.Windows;

{ TMVCJSONSerializer }

function TMVCJSONSerializer.DeserializeTValue(AJValue: TJSONValue; AAttributes: TArray<TCustomAttribute>;
  AItemName: String): TValue;
var
  lAttr: TValueAsType;
begin
  if TSerializerHelpers.AttributeExists<TValueAsType>(AAttributes, lAttr) then
  begin
    case lAttr.TValueTypeInfo.Kind of
      tkUString, tkString, tkLString, tkWString:
        begin
          Result := (AJValue as TJSONString).Value;
        end;
      tkInteger:
        begin
          Result := (AJValue as TJSONNumber).AsInt;
        end;
      tkInt64:
        begin
          Result := (AJValue as TJSONNumber).AsInt64;
        end;
      tkEnumeration:
        begin
          raise EMVCDeserializationException.Create('Booleans and enumerations are not supported');
        end;
    else
      raise EMVCDeserializationException.CreateFmt('Type non supported for TValue at item: ', [AItemName]);
    end;
  end
  else
  begin
    Result := DeserializeTValueWithDynamicType(AJValue, AItemName);
  end;
end;

function TMVCJSONSerializer.DeserializeTValueWithDynamicType(
  AJValue: TJSONValue; AItemName: String): TValue;
var
  lJTValueValue: TJSONValue;
  lTypeKind: TTypeKind;
  lStrType: string;
begin
  lStrType := AJValue.GetValue<TJSONString>('type').Value;
  lJTValueValue := AJValue.GetValue<TJSONValue>('value');
  lTypeKind := TSerializerHelpers.StringToTypeKind(lStrType);
  case lTypeKind of
    tkInteger:
      begin
        Result := (lJTValueValue as TJSONNumber).AsInt;
      end;
    tkEnumeration:
      begin
        Result := lJTValueValue is TJSONTrue;
      end;
    tkFloat:
      begin
        Result := (lJTValueValue as TJSONNumber).AsDouble;
      end;
    tkString, tkLString, tkWString, tkUString:
      begin
        Result := lJTValueValue.Value;
      end;
    tkInt64:
      begin
        Result := (lJTValueValue as TJSONNumber).AsInt64;
      end;
  else
    raise EMVCDeserializationException.CreateFmt('Type non supported for TValue %s at: ', [lStrType, AItemName]);
  end;
end;

function TMVCJSONSerializer.GetPair(JSONObject: TJSONObject; PropertyName: string): TJSONPair;
var
  pair: TJSONPair;
begin
  if not Assigned(JSONObject) then
    raise EMapperException.Create('JSONObject is nil');
  pair := JSONObject.Get(PropertyName);
  Result := pair;
end;

procedure TMVCJSONSerializer.InternalDeserializeObject(ASerializedObject: string;
  AObject: TObject; AStrict: Boolean);
var
  lJSON: TJSONValue;
begin
  lJSON := TJSONObject.ParseJSONValue(ASerializedObject);
  try
    if lJSON <> nil then
    begin
      if not AStrict then
      begin
        InternalJSONValueToObject(TJSONObject(lJSON), AObject)
      end
      else
        // InternalJSONObjectToObjectFields(TJSONObject(lJSON), AObject)
    end
    else
    begin
      raise EMVCDeserializationException.Create('Serialized string is not a valid JSON');
    end;
  finally
    lJSON.Free;
  end;
end;

procedure TMVCJSONSerializer.InternalJSONValueToObject(AJSONValue: TJSONValue; AObject: TObject);
var
  lType: TRTTIType;
  lProperties: TArray<TRTTIProperty>;
  lProperty: TRTTIProperty;
  lJSONObjKey: string;
  lJSONValue: TJSONValue;
  v: TValue;
  o: TObject;
  list: IWrappedList;
  I: Integer;
  cref: TClass;
  attr: MapperItemsClassType;
  Arr: TJSONArray;
  n: TJSONNumber;
  SerStreamASString: string;
  _attrser: MapperSerializeAsString;
  ListMethod: TRttiMethod;
  ListItem: TValue;
  ListParam: TRttiParameter;
  lPropName: string;
  lTypeSerializer: IMVCTypeSerializer;
  lInstanceField: TValue;
  lJSONObject: TJSONObject;
begin
  { TODO -oDaniele -cGeneral : Refactor this method }
  if not Assigned(AJSONValue) then
    raise EMapperException.Create('JSON cannot be nil');
  lType := CTX.GetType(AObject.ClassInfo);

  // has main object a custom serializer?
  lTypeSerializer := TMVCSerializersRegistry.GetTypeSerializer(
    SERIALIZER_NAME,
    lType.Handle);
  if lTypeSerializer <> nil then
  begin
    // deserialize object using custom serializer...
    lInstanceField := AObject;
    lTypeSerializer.DeserializeInstance(
      lType, lType.GetAttributes, AJSONValue, lInstanceField);
  end
  else
  begin
    // deserialize object using standard process...
    if not(AJSONValue is TJSONObject) then
    begin
      raise EMVCDeserializationException.CreateFmt('Expected TJSONObject, actual %s', [AJSONValue.ClassName]);
    end;
    lJSONObject := TJSONObject(AJSONValue);
    lProperties := lType.GetProperties;
    for lProperty in lProperties do
    begin
      if PropertyMustBeSkipped(lProperty) then
        continue;
      lPropName := lProperty.Name;
      lJSONObjKey := TSerializerHelpers.GetKeyName(lProperty, lType);

      if not lJSONObject.TryGetValue<TJSONValue>(lJSONObjKey, lJSONValue) then
        continue;
      lInstanceField := lProperty.GetValue(TObject(AObject));

      // check is exists a custom serializer for the current type
      lTypeSerializer := TMVCSerializersRegistry.GetTypeSerializer(SERIALIZER_NAME, lProperty.PropertyType.Handle);
      if lTypeSerializer <> nil then
      begin
        // go with custom deserializer
        lTypeSerializer.DeserializeInstance(
          lProperty.PropertyType, lProperty.GetAttributes, TObject(lJSONValue), lInstanceField);
      end
      else
      begin
        // go with standard deserializer
        DeserializeRTTIElement(lProperty.PropertyType, lProperty.GetAttributes, lJSONValue, lProperty.Name,
          lInstanceField);
      end;
      { Reference types MUST use the internal "AsObject" wghile value types can directly assign to InstanceField }
      if not lInstanceField.IsObject then
        lProperty.SetValue(TObject(AObject), lInstanceField);
    end;
  end;
end;

function TMVCJSONSerializer.JSONObjectToObject(Clazz: TClass; AJSONObject: TJSONObject): TObject;
var
  AObject: TObject;
begin
  AObject := TRTTIUtils.CreateObject(Clazz.QualifiedClassName);
  try
    InternalJSONValueToObject(AJSONObject, AObject);
    Result := AObject;
  except
    on E: Exception do
    begin
      FreeAndNil(AObject);
      raise EMVCDeserializationException.Create(E.Message);
    end;
  end;
end;

function TMVCJSONSerializer.ObjectToJSONValue(AObject: TObject;
  AIgnoredProperties: array of string): TJSONValue;
var
  lType: TRTTIType;
  lProperties: TArray<TRTTIProperty>;
  lProperty: TRTTIProperty;
  f: string;
  lOutputJSONObject: TJSONObject;
  lOutputJsonValue: TJSONValue;
  Arr: TJSONArray;
  list: IMVCList;
  Obj, o: TObject;
  ThereAreIgnoredProperties: Boolean;
  ts: TTimeStamp;
  sr: TStringStream;
  SS: TStringStream;
  _attrser: MapperSerializeAsString;
  lTypeSerializer: IMVCTypeSerializer;
  lJSONValue: TJSONValue;
  lSerializedJValue: TJSONValue;
  function MustBeSerialized(AKeyName: String): Boolean;
  var
    I: Integer;
  begin
    if not ThereAreIgnoredProperties then
      Exit(True);
    for I := low(AIgnoredProperties) to high(AIgnoredProperties) do
    begin
      if SameText(f, AIgnoredProperties[I]) then
      begin
        Exit(false);
      end;
    end;
  end;

begin
  ThereAreIgnoredProperties := Length(AIgnoredProperties) > 0;
  lType := CTX.GetType(AObject.ClassInfo);

  // main object has a custom serializer?
  lTypeSerializer := TMVCSerializersRegistry.GetTypeSerializer(
    SERIALIZER_NAME,
    lType.Handle);
  if lTypeSerializer <> nil then
  begin
    lJSONValue := nil;
    lTypeSerializer.SerializeInstance(
      lType, lType.GetAttributes, AObject, TObject(lJSONValue));
    Result := lJSONValue; // as TJSONObject;
  end
  else
  begin
    lOutputJSONObject := TJSONObject.Create;

    lProperties := lType.GetProperties;
    for lProperty in lProperties do
    begin
      f := TSerializerHelpers.GetKeyName(lProperty, lType);
      if not MustBeSerialized(f) then
        continue;
      if TSerializerHelpers.HasAttribute<DoNotSerializeAttribute>(lProperty) then
        continue;
      lTypeSerializer := TMVCSerializersRegistry.GetTypeSerializer(
        SERIALIZER_NAME,
        lProperty.PropertyType.Handle);
      if lTypeSerializer <> nil then
      begin
        lJSONValue := nil;
        lTypeSerializer.SerializeInstance(
          lProperty.PropertyType, lProperty.GetAttributes, lProperty.GetValue(AObject), TObject(lJSONValue));
        lOutputJSONObject.AddPair(f, lJSONValue);
      end
      else
      begin
        { if serializable then serialize, otherwise ignore it }
        if SerializeRTTIElement(lProperty.PropertyType, lProperty.GetAttributes,
          lProperty.GetValue(AObject), lSerializedJValue) then
          lOutputJSONObject.AddPair(f, lSerializedJValue);
      end;
    end;
    Result := lOutputJSONObject;
  end;
end;

function TMVCJSONSerializer.ObjectToJSONObjectFields(AObject: TObject): TJSONObject;
var
  _type: TRTTIType;
  _fields: TArray<TRttiField>;
  _field: TRttiField;
  f: string;
  JSONObject: TJSONObject;
  Obj, o: TObject;
  DoNotSerializeThis: Boolean;
  I: Integer;
  JObj: TJSONObject;
  lSerializedJValue: TJSONValue;
begin
  JSONObject := TJSONObject.Create;
  try
    // add the $dmvc.classname property to allows a strict deserialization
    JSONObject.AddPair(DMVC_CLASSNAME, AObject.QualifiedClassName);
    _type := CTX.GetType(AObject.ClassInfo);
    _fields := _type.GetFields;
    for _field in _fields do
    begin
      f := TSerializerHelpers.GetKeyName(_field, _type);
      if SerializeRTTIElement(_field.FieldType, _field.GetAttributes, _field.GetValue(AObject), lSerializedJValue) then
        JSONObject.AddPair(f, lSerializedJValue);
    end;
    Result := JSONObject;
  except
    FreeAndNil(JSONObject);
    raise;
  end;
end;

function TMVCJSONSerializer.SerializeFloatProperty(AObject: TObject;
  ARTTIProperty: TRTTIProperty): TJSONValue;
begin
  if ARTTIProperty.PropertyType.QualifiedName = 'System.TDate' then
  begin
    if ARTTIProperty.GetValue(AObject).AsExtended = 0 then
      Result := TJSONNull.Create
    else
      Result := TJSONString.Create
        (ISODateToString(ARTTIProperty.GetValue(AObject).AsExtended))
  end
  else if ARTTIProperty.PropertyType.QualifiedName = 'System.TDateTime' then
  begin
    if ARTTIProperty.GetValue(AObject).AsExtended = 0 then
      Result := TJSONNull.Create
    else
      Result := TJSONString.Create
        (ISODateTimeToString(ARTTIProperty.GetValue(AObject).AsExtended))
  end
  else if ARTTIProperty.PropertyType.QualifiedName = 'System.TTime' then
    Result := TJSONString.Create(ISOTimeToString(ARTTIProperty.GetValue(AObject)
      .AsExtended))
  else
    Result := TJSONNumber.Create(ARTTIProperty.GetValue(AObject).AsExtended);
end;

function TMVCJSONSerializer.SerializeObject(AObject: TObject;
  AIgnoredProperties: array of string): string;
var
  lJSON: TJSONValue;
begin
  if AObject is TJSONValue then
    Exit(TJSONValue(AObject).ToJson);

  lJSON := ObjectToJSONValue(AObject, AIgnoredProperties);
  try
    Result := lJSON.ToJson;
  finally
    lJSON.Free;
  end;
end;

function TMVCJSONSerializer.SerializeObjectStrict(AObject: TObject): String;
begin
  raise EMVCSerializationException.Create('Not implemented');
end;

function TMVCJSONSerializer.SerializeRecord(AElementType: TRTTIType;
  AValue: TValue; AAttributes: TArray<TCustomAttribute>): TJSONValue;
var
  lTimeStamp: TTimeStamp;
begin
  if AElementType.QualifiedName = 'System.Rtti.TValue' then
  begin
    Result := SerializeTValue(AElementType, AValue, AAttributes);
  end
  else if AElementType.QualifiedName = 'System.SysUtils.TTimeStamp' then
  begin
    lTimeStamp := AValue.AsType<System.SysUtils.TTimeStamp>;
    Result := TJSONNumber.Create(TimeStampToMsecs(lTimeStamp));
  end
  else
    raise EMVCSerializationException.CreateFmt('Cannot serialize record: %s', [AElementType.ToString]);
end;

function TMVCJSONSerializer.SerializeRTTIElement(ElementType: TRTTIType;
  ElementAttributes: TArray<TCustomAttribute>; Value: TValue; out OutputValue: TJSONValue): Boolean;
var
  ts: TTimeStamp;
  o: TObject;
  list: IMVCList;
  Arr: TJSONArray;
  Obj: TObject;
  _attrser: MapperSerializeAsString;
  SerEnc: TEncoding;
  sr: TStringStream;
  lEncodingName: string;
  buff: TBytes;
  lStreamAsString: string;
begin
  OutputValue := nil;
  Result := false;
  case ElementType.TypeKind of
    tkInteger, tkInt64:
      begin
        OutputValue := TJSONNumber.Create(Value.AsInteger);
      end;
    tkFloat:
      begin
        OutputValue := SerializeFloatProperty(ElementType, Value);
      end;
    tkString, tkLString, tkWString, tkUString:
      begin
        OutputValue := TJSONString.Create(Value.AsString);
      end;
    tkEnumeration:
      begin
        OutputValue := SerializeEnumerationProperty(ElementType, Value);
      end;
    tkRecord:
      begin
        OutputValue := SerializeRecord(ElementType, Value, ElementAttributes);
      end;
    tkClass:
      begin
        o := Value.AsObject;
        if Assigned(o) then
        begin
          list := TDuckTypedList.Wrap(o);
          if Assigned(list) then
          begin
            OutputValue := TJSONArray.Create;
            for Obj in list do
              if Assigned(Obj) then
                // nil element into the list are not serialized
                TJSONArray(OutputValue).AddElement(ObjectToJSONValue(Obj, []));
          end
          else
          begin
            OutputValue := ObjectToJSONValue(Value.AsObject, []);
          end;
        end
        else
        begin
          if TSerializerHelpers.HasAttribute<MapperSerializeAsString>(ElementType) then
            OutputValue := TJSONString.Create('')
          else
            OutputValue := TJSONNull.Create;
        end;
      end; // tkClass
  end;
  Result := OutputValue <> nil;
end;

function TMVCJSONSerializer.SerializeTValueAsFixedNullableType(AValue: TValue;
  AValueTypeInfo: PTypeInfo): TJSONValue;
begin
  // supports nulls
  if AValue.IsEmpty then
    Exit(TJSONNull.Create);

  // serialize the TValue internal value as specific type
  case AValueTypeInfo.Kind of
    tkString, tkUString, tkLString, tkWString:
      begin
        Result := TJSONString.Create(AValue.AsString);
      end;
    tkInteger:
      begin
        Result := TJSONNumber.Create(AValue.AsInteger);
      end;
    tkInt64:
      begin
        Result := TJSONNumber.Create(AValue.AsInt64);
      end;
  else
    raise EMVCSerializationException.Create('Unsupported type in SerializeTValueAsFixedType');
  end;
end;

function TMVCJSONSerializer.SerializeTValue(AElementType: TRTTIType; AValue: TValue;
  AAttributes: TArray<TCustomAttribute>)
  : TJSONValue;
var
  lTValueDataRTTIType: TRTTIType;
  lValue: TValue;
  lAtt: TValueAsType;
  lJSONValue: TJSONValue;
begin
  lValue := AValue.AsType<TValue>;
  if TSerializerHelpers.AttributeExists<TValueAsType>(AAttributes, lAtt) then
  begin
    Result := SerializeTValueAsFixedNullableType(lValue, lAtt.TValueTypeInfo)
  end
  else
  begin
    Result := TJSONObject.Create;
    try
      if lValue.IsEmpty then
      begin
        lJSONValue := TJSONNull.Create;
        TJSONObject(Result).AddPair('type', TJSONNull.Create);
      end
      else
      begin
        lTValueDataRTTIType := CTX.GetType(lValue.TypeInfo);
        if not SerializeRTTIElement(lTValueDataRTTIType, [], lValue, lJSONValue) then
          raise EMVCSerializationException.Create('Cannot serialize TValue');
        TJSONObject(Result).AddPair('type', TSerializerHelpers.GetTypeKindAsString(lValue.TypeInfo.Kind));
      end;
      TJSONObject(Result).AddPair('value', lJSONValue);
    except
      Result.Free;
      raise;
    end;
  end;
end;

function TMVCJSONSerializer.SerializeCollection(AList: TObject;
  AIgnoredProperties: array of string): String;
var
  I: Integer;
  JV: TJSONValue;
  lList: IMVCList;
  lJArr: TJSONArray;
begin
  if Assigned(AList) then
  begin
    lList := WrapAsList(AList);
    lJArr := TJSONArray.Create;
    try
      // AList.OwnsObjects := AOwnsChildObjects;
      for I := 0 to lList.Count - 1 do
      begin
        JV := ObjectToJSONValue(lList.GetItem(I), AIgnoredProperties);
        // if Assigned(AForEach) then
        // AForEach(JV);
        lJArr.AddElement(JV);
      end;
      Result := lJArr.ToJson;
    finally
      lJArr.Free;
    end;
  end
  else
  begin
    raise EMVCSerializationException.Create('List is nil');
  end;
end;

function TMVCJSONSerializer.SerializeCollectionStrict(AList: TObject): String;
var
  I: Integer;
  JV: TJSONObject;
  lList: IMVCList;
  lJArr: TJSONArray;
begin
  if Assigned(AList) then
  begin
    lList := WrapAsList(AList);
    lJArr := TJSONArray.Create;
    try
      for I := 0 to lList.Count - 1 do
      begin
        JV := ObjectToJSONObjectFields(lList.GetItem(I));
        // if Assigned(AForEach) then
        // AForEach(JV);
        lJArr.AddElement(JV);
      end;
      Result := lJArr.ToJson;
    finally
      lJArr.Free;
    end;
  end
  else
  begin
    raise EMVCSerializationException.Create('List is nil');
  end;
end;

function TMVCJSONSerializer.PropertyExists(JSONObject: TJSONObject;
  PropertyName: string): Boolean;
begin
  Result := Assigned(GetPair(JSONObject, PropertyName));
end;

function TMVCJSONSerializer.PropertyMustBeSkipped(
  const ARTTIProperty: TRTTIProperty): Boolean;
begin
  Result :=
    ((not ARTTIProperty.IsWritable) and (ARTTIProperty.PropertyType.TypeKind <> tkClass))
    or (TSerializerHelpers.HasAttribute<MapperTransientAttribute>(ARTTIProperty));
end;

function TMVCJSONSerializer.SerializeDataSet(ADataSet: TDataSet;
  AIgnoredFields: array of string): string;
begin
  raise EMVCSerializationException.Create('Not implemented');
end;

function TMVCJSONSerializer.SerializeEnumerationField(AObject: TObject;
  ARttiField: TRttiField): TJSONValue;
begin
  if ARttiField.FieldType.QualifiedName = 'System.Boolean' then
  begin
    if ARttiField.GetValue(AObject).AsBoolean then
      Result := TJSONTrue.Create
    else
      Result := TJSONFalse.Create;
  end
  else
  begin
    Result := TJSONNumber.Create(ARttiField.GetValue(AObject).AsOrdinal);
  end;
end;

function TMVCJSONSerializer.SerializeEnumerationProperty(AElementType: TRTTIType;
  AValue: TValue): TJSONValue;
begin
  if AElementType.QualifiedName = 'System.Boolean' then
  begin
    if AValue.AsBoolean then
      Result := TJSONTrue.Create
    else
      Result := TJSONFalse.Create;
  end
  else
  begin
    Result := TJSONNumber.Create(AValue.AsOrdinal);
  end;
end;

// function TMVCJSONSerializer.SerializeEnumerationProperty(AObject: TObject;
// ARTTIProperty: TRttiProperty): TJSONValue;
// begin
// if ARTTIProperty.PropertyType.QualifiedName = 'System.Boolean' then
// begin
// if ARTTIProperty.GetValue(AObject).AsBoolean then
// Result := TJSONTrue.Create
// else
// Result := TJSONFalse.Create;
// end
// else
// begin
// Result := TJSONNumber.Create(ARTTIProperty.GetValue(AObject).AsOrdinal);
// end;
// end;

// function TMVCJSONSerializer.SerializeFloatField(AObject: TObject;
// ARttiField: TRttiField): TJSONValue;
// begin
// if ARttiField.FieldType.QualifiedName = 'System.TDate' then
// begin
// if ARttiField.GetValue(AObject).AsExtended = 0 then
// Result := TJSONNull.Create
// else
// Result := TJSONString.Create(ISODateToString(ARttiField.GetValue(AObject)
// .AsExtended))
// end
// else if ARttiField.FieldType.QualifiedName = 'System.TDateTime' then
// begin
// if ARttiField.GetValue(AObject).AsExtended = 0 then
// Result := TJSONNull.Create
// else
// Result := TJSONString.Create
// (ISODateTimeToString(ARttiField.GetValue(AObject).AsExtended))
// end
// else if ARttiField.FieldType.QualifiedName = 'System.TTime' then
// Result := TJSONString.Create(ISOTimeToString(ARttiField.GetValue(AObject)
// .AsExtended))
// else
// Result := TJSONNumber.Create(ARttiField.GetValue(AObject).AsExtended);
// end;

function TMVCJSONSerializer.SerializeFloatProperty(AElementType: TRTTIType;
  AValue: TValue): TJSONValue;
begin
  if AElementType.QualifiedName = 'System.TDate' then
  begin
    if AValue.AsExtended = 0 then
      Result := TJSONNull.Create
    else
      Result := TJSONString.Create
        (ISODateToString(AValue.AsExtended))
  end
  else if AElementType.QualifiedName = 'System.TDateTime' then
  begin
    if AValue.AsExtended = 0 then
      Result := TJSONNull.Create
    else
      Result := TJSONString.Create
        (ISODateTimeToString(AValue.AsExtended))
  end
  else if AElementType.QualifiedName = 'System.TTime' then
    Result := TJSONString.Create(ISOTimeToString(AValue.AsExtended))
  else
    Result := TJSONNumber.Create(AValue.AsExtended);
end;

{ TMVCJSONDeserializer }

procedure TMVCJSONSerializer.DeserializeCollection(ASerializedObjectList: string; AList: IMVCList;
  AClazz: TClass);
var
  I: Integer;
  lJArr: TJSONArray;
  lJValue: TJSONValue;
begin
  if Trim(ASerializedObjectList) = '' then
    raise EMVCDeserializationException.Create('Invalid serialized data');
  lJValue := TJSONObject.ParseJSONValue(ASerializedObjectList);
  try
    if (lJValue = nil) or (not(lJValue is TJSONArray)) then
      raise EMVCDeserializationException.Create('Serialized data is not a valid JSON Array');
    lJArr := TJSONArray(lJValue);
    for I := 0 to lJArr.Count - 1 do
    begin
      AList.Add(JSONObjectToObject(AClazz, lJArr.Items[I] as TJSONObject));
    end;
  finally
    lJValue.Free;
  end;
end;

procedure TMVCJSONSerializer.DeserializeDataSet(ASerializedObject: String;
  const ADataSet: TDataSet);
begin
  raise Exception.Create('Not implemented');
end;

function TMVCJSONSerializer.DeserializeEnumeration(ARTTIType: TRTTIType; AJSONValue: TJSONValue;
  AItemName: String): TValue;
var
  lOutputValue: TValue;
begin
  if ARTTIType.QualifiedName = 'System.Boolean' then
  begin
    if AJSONValue is TJSONTrue then
      Result := True
    else if AJSONValue is TJSONFalse then
      Result := false
    else
      raise EMapperException.CreateFmt('Invalid value for property %s', [AItemName]);
  end
  else // it is an enumerated value but it's not a boolean.
  begin
    TValue.Make((AJSONValue as TJSONNumber).AsInt, ARTTIType.Handle, lOutputValue);
    Result := lOutputValue;
  end;
end;

function TMVCJSONSerializer.DeserializeFloat(ARTTIType: TRTTIType; AJSONValue: TJSONValue; APropName: String): TValue;
begin
  if ARTTIType.QualifiedName = 'System.TDate' then
  begin
    if AJSONValue is TJSONNull then
      Result := 0
    else
      Result := ISOStrToDateTime(AJSONValue.Value + ' 00:00:00');
  end
  else if ARTTIType.QualifiedName = 'System.TDateTime' then
  begin
    if AJSONValue is TJSONNull then
      Result := 0
    else
      Result := ISOStrToDateTime(AJSONValue.Value);
  end
  else if ARTTIType.QualifiedName = 'System.TTime' then
  begin
    if not(AJSONValue is TJSONNull) then
      if AJSONValue is TJSONString then
        Result := ISOStrToTime(AJSONValue.Value)
      else
        raise EMVCDeserializationException.CreateFmt
          ('Cannot deserialize [%s], expected [%s] got [%s]',
          [APropName, 'TJSONString', AJSONValue.ClassName]);
  end
  else { if _field.PropertyType.QualifiedName = 'System.Currency' then }
  begin
    if not(AJSONValue is TJSONNull) then
      if AJSONValue is TJSONNumber then
        Result := TJSONNumber(AJSONValue).AsDouble
      else
        raise EMVCDeserializationException.CreateFmt
          ('Cannot deserialize [%s], expected [%s] got [%s]',
          [APropName, 'TJSONNumber', AJSONValue.ClassName]);
  end;
end;

function TMVCJSONSerializer.DeserializeArray(ARTTIType: TRTTIType;
  AJSONValue: TJSONValue; AAttributes: TArray<TCustomAttribute>;
  AItemName: String): TValue;
begin

end;

procedure TMVCJSONSerializer.DeSerializeBase64StringStream(aStream: TStream;
  const aBase64SerializedString: string);
begin

end;

procedure TMVCJSONSerializer.DeserializeObject(ASerializedObject: string; AObject: TObject);
begin
  InternalDeserializeObject(ASerializedObject, AObject, false);
end;

procedure TMVCJSONSerializer.DeserializeObjectStrict(ASerializedObject: String;
  AObject: TObject);
begin
  InternalDeserializeObject(ASerializedObject, AObject, True);
end;

function TMVCJSONSerializer.DeserializeRecord(ARTTIType: TRTTIType; AJSONValue: TJSONValue;
  AAttributes: TArray<TCustomAttribute>; AItemName: String): TValue;
var
  lJNumber: TJSONNumber;
begin
  if ARTTIType.QualifiedName = 'System.Rtti.TValue' then
  begin
    Result := DeserializeTValue(AJSONValue, AAttributes, AItemName);
  end
  else if ARTTIType.QualifiedName = 'System.SysUtils.TTimeStamp'
  then
  begin
    lJNumber := AJSONValue as TJSONNumber;
    Result := TValue.From<TTimeStamp>(MSecsToTimeStamp(lJNumber.AsInt64));
  end
  else
    raise EMVCDeserializationException.CreateFmt('Type %s not supported for %s',
      [ARTTIType.QualifiedName, AItemName]);
end;

procedure TMVCJSONSerializer.DeserializeRTTIElement(ElementType: TRTTIType;
  ElementAttributes: TArray<TCustomAttribute>; JSONValue: TJSONValue; APropName: String; var Value: TValue);
var
  lInnerObj: TObject;
  lJSONArr: TJSONArray;
  lAttr: MapperItemsClassType;
  lClassRef: TClass;
  list: IMVCList;
  I: Integer;
begin
  case ElementType.TypeKind of
    tkEnumeration:
      begin
        Value := DeserializeEnumeration(ElementType, JSONValue, APropName);
      end;
    tkInteger, tkInt64:
      Value := StrToIntDef(JSONValue.Value, 0);
    tkFloat:
      begin
        Value := DeserializeFloat(ElementType, JSONValue, APropName);
      end;
    tkString, tkLString, tkWString, tkUString:
      begin
        Value := JSONValue.Value;
      end;
    tkRecord:
      begin
        Value := DeserializeRecord(ElementType, JSONValue, ElementAttributes, APropName);
      end;
    tkArray:
      begin
        Value := DeserializeArray(ElementType, JSONValue, ElementAttributes, APropName);
      end;
    tkClass: // try to restore child properties... but only if the collection is not nil!!!
      begin
        lInnerObj := Value.AsObject;
        if Assigned(lInnerObj) then
        begin
          list := TDuckTypedList.Wrap(lInnerObj);
          if list <> nil then
          begin // restore collection
            if JSONValue is TJSONArray then
            begin
              lJSONArr := TJSONArray(JSONValue);
              // look for the MapperItemsClassType on the property itself or on the property type
              if TSerializerHelpers.AttributeExists<MapperItemsClassType>(ElementAttributes, lAttr) or
                TSerializerHelpers.AttributeExists<MapperItemsClassType>(ElementType.GetAttributes, lAttr) then
              begin
                lClassRef := lAttr.Value;
                for I := 0 to lJSONArr.Count - 1 do
                begin
                  list.Add(JSONObjectToObject(lClassRef, lJSONArr.Items[I] as TJSONObject));
                end;
              end
              else
                raise EMVCDeserializationException.CreateFmt
                  ('Cannot deserialize %s because no MapperItemsClassType is defined on property or property type',
                  [APropName]);
            end
            else
              raise EMapperException.CreateFmt('Expected TJSONArray, actual %s while deserialize property %s',
                [JSONValue.ClassName, APropName]);
          end
          else // try to deserialize into the property... but the json MUST be an object
          begin
            if JSONValue is TJSONObject then
            begin
              InternalJSONValueToObject(TJSONObject(JSONValue), lInnerObj);
            end
            else
              raise EMapperException.CreateFmt('Cannot deserialize property %s', [APropName]);
          end;
        end;
      end;
  end; // case
end;

initialization

TMVCSerializersRegistry.RegisterSerializer('application/json', TMVCJSONSerializer.Create);

finalization

TMVCSerializersRegistry.UnRegisterSerializer('application/json');

end.
