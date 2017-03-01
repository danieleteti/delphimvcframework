// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2017 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// Collaborators with this file: Ezequiel Juliano Müller (ezequieljuliano@gmail.com)
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
// ***************************************************************************

unit MVCFramework.Serializer.JsonDataObjects;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Rtti,
  System.TypInfo,
  System.Variants,
  System.Generics.Collections,
  Data.DB,
  MVCFramework.Serializer.Intf,
  MVCFramework.Serializer.Commons,
  MVCFramework.DuckTyping,
  JsonDataObjects;

type

  TJsonValue = class
  private
    FValue: string;
  protected
    { protected declarations }
  public
    constructor Create; overload;
    constructor Create(const AValue: string); overload;

    property Value: string read FValue write FValue;
  end;

  TMVCJsonDataObjectsSerializer = class(TInterfacedObject, IMVCSerializer)
  private
    FRttiContext: TRttiContext;
    FTypeSerializers: TDictionary<PTypeInfo, IMVCTypeSerializer>;
    procedure ObjectToJsonObject(
      const AObject: TObject;
      const AJsonObject: TJsonObject;
      const AType: TMVCSerializationType;
      const AIgnoredAttributes: array of string
      );
    procedure AttributeToJsonDataValue(
      const AJsonObject: TJsonObject;
      const AName: string;
      const AValue: TValue;
      const AType: TMVCSerializationType;
      const AIgnored: array of string;
      const ACustomAttributes: TArray<TCustomAttribute>
      );
    procedure JsonObjectToObject(
      const AJsonObject: TJsonObject;
      const AObject: TObject;
      const AType: TMVCSerializationType;
      const AIgnoredAttributes: array of string
      );
    procedure JsonDataValueToAttribute(
      const AJsonObject: TJsonObject;
      const AName: string;
      var AValue: TValue;
      const AType: TMVCSerializationType;
      const AIgnored: array of string;
      const ACustomAttributes: TArray<TCustomAttribute>
      );
    procedure JsonArrayToList(
      const AJsonArray: TJsonArray;
      const AList: IMVCList;
      const AClazz: TClass;
      const AType: TMVCSerializationType;
      const AIgnoredAttributes: array of string
      );
    function IsIgnoredAttribute(const AAttributes: array of string; const AName: string): Boolean;
  protected
    procedure RegisterTypeSerializer(const ATypeInfo: PTypeInfo; AInstance: IMVCTypeSerializer);

    function SerializeObject(const AObject: TObject): string; overload;
    function SerializeObject(const AObject: TObject; const AType: TMVCSerializationType): string; overload;
    function SerializeObject(const AObject: TObject; const AType: TMVCSerializationType; const AIgnoredAttributes: array of string): string; overload;

    function SerializeCollection(const AList: TObject): string; overload;
    function SerializeCollection(const AList: TObject; const AType: TMVCSerializationType): string; overload;
    function SerializeCollection(const AList: TObject; const AType: TMVCSerializationType; const AIgnoredAttributes: array of string): string; overload;

    function SerializeDataSet(const ADataSet: TDataSet): string; overload;
    function SerializeDataSet(const ADataSet: TDataSet; const AIgnoredFields: array of string): string; overload;

    procedure DeserializeObject(const ASerializedObject: string; const AObject: TObject); overload;
    procedure DeserializeObject(const ASerializedObject: string; const AObject: TObject; const AType: TMVCSerializationType); overload;
    procedure DeserializeObject(const ASerializedObject: string; const AObject: TObject; const AType: TMVCSerializationType; const AIgnoredAttributes: array of string); overload;

    procedure DeserializeCollection(const ASerializedList: string; const AList: TObject; const AClazz: TClass); overload;
    procedure DeserializeCollection(const ASerializedList: string; const AList: TObject; const AClazz: TClass; const AType: TMVCSerializationType); overload;
    procedure DeserializeCollection(const ASerializedList: string; const AList: TObject; const AClazz: TClass; const AType: TMVCSerializationType; const AIgnoredAttributes: array of string); overload;

    procedure DeserializeDataSet(const ASerializedDataSet: string; const ADataSet: TDataSet);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  MVCFramework.Serializer.JsonDataObjects.CustomTypes;

{ TMVCJsonDataObjectsSerializer }

procedure TMVCJsonDataObjectsSerializer.AttributeToJsonDataValue(
  const AJsonObject: TJsonObject;
  const AName: string;
  const AValue: TValue;
  const AType: TMVCSerializationType;
  const AIgnored: array of string;
  const ACustomAttributes: TArray<TCustomAttribute>);
var
  ChildJsonObject: TJsonObject;
  ChildJsonArray: TJsonArray;
  ChildValue: TValue;
  ChildObject, Obj: TObject;
  ChildList: IMVCList;
  ChildJsonValue: TObject;
  ValueTypeAtt: MVCValueAsTypeAttribute;
  CastValue, CastedValue: TValue;
begin
  if AValue.IsEmpty then
  begin
    AJsonObject[AName] := Null;
    Exit;
  end;

  if FTypeSerializers.ContainsKey(AValue.TypeInfo) then
  begin
    ChildJsonValue := nil;
    FTypeSerializers.Items[AValue.TypeInfo].Serialize(AValue, ChildJsonValue, ACustomAttributes);
    if Assigned(ChildJsonValue) then
    begin
      if ChildJsonValue is TJsonObject then
        AJsonObject.O[AName] := ChildJsonValue as TJsonObject
      else if ChildJsonValue is TJsonArray then
        AJsonObject.A[AName] := ChildJsonValue as TJsonArray
      else if ChildJsonValue is TJsonBaseObject then
        AJsonObject[AName] := ChildJsonValue
      else if ChildJsonValue is TJsonValue then
      begin
        AJsonObject[AName] := TJsonValue(ChildJsonValue).Value;
        ChildJsonValue.Free;
      end
      else
        raise EMVCSerializationException.CreateFmt('Can not serialize %s the serializer does not have a valid TJsonBaseObject type.', [AName]);
    end;
    Exit;
  end;

  case AValue.Kind of
    tkInteger:
      AJsonObject.I[AName] := AValue.AsInteger;

    tkInt64:
      AJsonObject.L[AName] := AValue.AsInt64;

    tkChar, tkString, tkWChar, tkLString, tkWString, tkUString:
      AJsonObject.S[AName] := AValue.AsString;

    tkFloat:
      begin
        if (AValue.TypeInfo = System.TypeInfo(TDate)) then
        begin
          if (AValue.AsExtended = 0) then
            AJsonObject[AName] := Null
          else
            AJsonObject.S[AName] := DateToISODate(AValue.AsExtended);
        end
        else if (AValue.TypeInfo = System.TypeInfo(TDateTime)) then
        begin
          if (AValue.AsExtended = 0) then
            AJsonObject[AName] := Null
          else
            AJsonObject.S[AName] := DateTimeToISOTimeStamp(AValue.AsExtended);
        end
        else if (AValue.TypeInfo = System.TypeInfo(TTime)) then
        begin
          if (AValue.AsExtended = 0) then
            AJsonObject[AName] := Null
          else
            AJsonObject.S[AName] := TimeToISOTime(AValue.AsExtended);
        end
        else
          AJsonObject.F[AName] := AValue.AsExtended;
      end;

    tkVariant:
      AJsonObject[AName] := AValue.AsVariant;

    tkEnumeration:
      begin
        if (AValue.TypeInfo = System.TypeInfo(Boolean)) then
        begin
          if AValue.AsBoolean then
            AJsonObject.B[AName] := True
          else
            AJsonObject.B[AName] := False
        end
        else
          AJsonObject.L[AName] := AValue.AsOrdinal;
      end;

    tkClass:
      begin
        ChildObject := AValue.AsObject;
        if Assigned(ChildObject) then
        begin
          ChildList := TDuckTypedList.Wrap(ChildObject);
          if Assigned(ChildList) then
          begin
            ChildJsonArray := AJsonObject.A[AName];
            for Obj in ChildList do
              if Assigned(Obj) then
                ObjectToJsonObject(Obj, ChildJsonArray.AddObject, AType, AIgnored);
          end
          else
          begin
            ChildJsonObject := AJsonObject.O[AName];
            ObjectToJsonObject(ChildObject, ChildJsonObject, AType, AIgnored);
          end;
        end
        else
        begin
          if TMVCSerializerHelpful.AttributeExists<MVCSerializeAsStringAttribute>(ACustomAttributes) then
            AJsonObject.S[AName] := EmptyStr
          else
            AJsonObject[AName] := Null;
        end;
      end;

    tkRecord:
      begin
        if (AValue.TypeInfo = System.TypeInfo(TTimeStamp)) then
        begin
          AJsonObject.F[AName] := TimeStampToMsecs(AValue.AsType<TTimeStamp>);
        end
        else if (AValue.TypeInfo = System.TypeInfo(TValue)) then
        begin
          if TMVCSerializerHelpful.AttributeExists<MVCValueAsTypeAttribute>(ACustomAttributes, ValueTypeAtt) then
          begin
            CastValue := AValue.AsType<TValue>;
            if CastValue.TryCast(ValueTypeAtt.ValueTypeInfo, CastedValue) then
              AttributeToJsonDataValue(AJsonObject, AName, CastedValue, stDefault, [], [])
            else
              raise EMVCSerializationException.CreateFmt('Can not serialize %s of TypeKind tkRecord (TValue with MVCValueAsTypeAttribute).', [AName]);
          end
          else
          begin
            ChildValue := AValue.AsType<TValue>;
            ChildJsonObject := AJsonObject.O[AName];
            ChildJsonObject.S['type'] := TMVCSerializerHelpful.GetTypeKindAsString(ChildValue.TypeInfo.Kind);
            AttributeToJsonDataValue(ChildJsonObject, 'value', ChildValue, stDefault, [], []);
          end;
        end
        else
          raise EMVCSerializationException.CreateFmt('Can not serialize %s of TypeKind tkRecord.', [AName]);
      end;

    tkSet:
      raise EMVCSerializationException.CreateFmt('Can not serialize %s of TypeKind tkSet.', [AName]);

    tkArray:
      raise EMVCSerializationException.CreateFmt('Can not serialize %s of TypeKind tkArray.', [AName]);

    tkUnknown:
      raise EMVCSerializationException.CreateFmt('Can not serialize %s of TypeKind tkUnknown.', [AName]);
  end;
end;

constructor TMVCJsonDataObjectsSerializer.Create;
begin
  inherited Create;
  FRttiContext := TRttiContext.Create;
  FTypeSerializers := TDictionary<PTypeInfo, IMVCTypeSerializer>.Create;
  FTypeSerializers.Add(System.TypeInfo(TStream), TStreamSerializerJsonDataObject.Create);
  FTypeSerializers.Add(System.TypeInfo(TStringStream), TStreamSerializerJsonDataObject.Create);
  FTypeSerializers.Add(System.TypeInfo(TMemoryStream), TStreamSerializerJsonDataObject.Create);
end;

procedure TMVCJsonDataObjectsSerializer.DeserializeCollection(
  const ASerializedList: string;
  const AList: TObject;
  const AClazz: TClass;
  const AType: TMVCSerializationType);
begin
  DeserializeCollection(ASerializedList, AList, AClazz, AType, []);
end;

procedure TMVCJsonDataObjectsSerializer.DeserializeCollection(
  const ASerializedList: string;
  const AList: TObject;
  const AClazz: TClass);
begin
  DeserializeCollection(ASerializedList, AList, AClazz, stDefault);
end;

procedure TMVCJsonDataObjectsSerializer.DeserializeCollection(
  const ASerializedList: string;
  const AList: TObject;
  const AClazz: TClass;
  const AType: TMVCSerializationType;
  const AIgnoredAttributes: array of string);
var
  JsonArray: TJsonArray;
  ObjList: IMVCList;
begin
  if (ASerializedList = EmptyStr) then
    Exit;

  if not Assigned(AList) then
    Exit;

  ObjList := TDuckTypedList.Wrap(AList);
  if Assigned(ObjList) then
  begin
    JsonArray := TJsonArray.Parse(ASerializedList) as TJsonArray;
    try
      JsonArrayToList(JsonArray, ObjList, AClazz, AType, AIgnoredAttributes);
    finally
      JsonArray.Free;
    end;
  end;
end;

procedure TMVCJsonDataObjectsSerializer.DeserializeDataSet(
  const ASerializedDataSet: string; const ADataSet: TDataSet);
begin
  raise EMVCSerializationException.Create('Method TMVCJsonDataObjectsSerializer.DeserializeDataSet not implemented.');
end;

procedure TMVCJsonDataObjectsSerializer.DeserializeObject(
  const ASerializedObject: string; const AObject: TObject;
  const AType: TMVCSerializationType);
begin
  DeserializeObject(ASerializedObject, AObject, AType, []);
end;

procedure TMVCJsonDataObjectsSerializer.DeserializeObject(
  const ASerializedObject: string; const AObject: TObject);
begin
  DeserializeObject(ASerializedObject, AObject, stDefault);
end;

destructor TMVCJsonDataObjectsSerializer.Destroy;
begin
  FTypeSerializers.Free;
  FRttiContext.Free;
  inherited Destroy;
end;

function TMVCJsonDataObjectsSerializer.IsIgnoredAttribute(
  const AAttributes: array of string; const AName: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Low(AAttributes) to High(AAttributes) do
    if (AAttributes[I] = AName) then
      Exit(True);
end;

procedure TMVCJsonDataObjectsSerializer.JsonArrayToList(
  const AJsonArray: TJsonArray; const AList: IMVCList;
  const AClazz: TClass; const AType: TMVCSerializationType;
  const AIgnoredAttributes: array of string);
var
  I: Integer;
  Obj: TObject;
begin
  for I := 0 to Pred(AJsonArray.Count) do
  begin
    Obj := TMVCSerializerHelpful.CreateObject(AClazz.QualifiedClassName);
    JsonObjectToObject(AJsonArray.Items[I].ObjectValue, Obj, AType, AIgnoredAttributes);
    AList.Add(Obj);
  end;
end;

procedure TMVCJsonDataObjectsSerializer.JsonDataValueToAttribute(
  const AJsonObject: TJsonObject;
  const AName: string;
  var AValue: TValue;
  const AType: TMVCSerializationType;
  const AIgnored: array of string;
  const ACustomAttributes: TArray<TCustomAttribute>);
var
  ChildObject: TObject;
  ChildList: IMVCList;
  ChildListOfAtt: MVCListOfAttribute;
  ChildJsonValue: TJsonValue;
begin
  if FTypeSerializers.ContainsKey(AValue.TypeInfo) then
  begin
    case AJsonObject[AName].Typ of
      jdtNone:
        Exit;
      jdtObject:
        FTypeSerializers.Items[AValue.TypeInfo].Deserialize(AJsonObject[AName].ObjectValue, AValue, ACustomAttributes);
      jdtArray:
        FTypeSerializers.Items[AValue.TypeInfo].Deserialize(AJsonObject[AName].ArrayValue, AValue, ACustomAttributes);
    else
      begin
        ChildJsonValue := TJsonValue.Create;
        try
          ChildJsonValue.Value := AJsonObject[AName].Value;
          FTypeSerializers.Items[AValue.TypeInfo].Deserialize(ChildJsonValue, AValue, ACustomAttributes);
        finally
          ChildJsonValue.Free;
        end;
      end;
    end;
    Exit;
  end;

  case AJsonObject[AName].Typ of
    jdtNone:
      Exit;

    jdtString:
      begin
        if (AValue.TypeInfo = System.TypeInfo(TDate)) then
          AValue := TValue.From<TDate>(ISODateToDate(AJsonObject[AName].Value))

        else if (AValue.TypeInfo = System.TypeInfo(TDateTime)) then
          AValue := TValue.From<TDateTime>(ISOTimeStampToDateTime(AJsonObject[AName].Value))

        else if (AValue.TypeInfo = System.TypeInfo(TTime)) then
          AValue := TValue.From<TTime>(ISOTimeToTime(AJsonObject[AName].Value))

        else
          AValue := TValue.From<string>(AJsonObject[AName].Value);
      end;

    jdtInt:
      begin
        if (AValue.Kind = tkEnumeration) then
          TValue.Make(AJsonObject[AName].IntValue, AValue.TypeInfo, AValue)
        else
          AValue := TValue.From<Integer>(AJsonObject[AName].IntValue);
      end;

    jdtLong, jdtULong:
      begin
        if (AValue.TypeInfo = System.TypeInfo(TTimeStamp)) then
          AValue := TValue.From<TTimeStamp>(MSecsToTimeStamp(AJsonObject[AName].LongValue))
        else
          AValue := TValue.From<Int64>(AJsonObject[AName].LongValue);
      end;

    jdtFloat:
      AValue := TValue.From<Double>(AJsonObject[AName].FloatValue);

    jdtDateTime:
      AValue := TValue.From<TDateTime>(AJsonObject[AName].DateTimeValue);

    jdtBool:
      AValue := TValue.From<Boolean>(AJsonObject[AName].BoolValue);

    jdtObject:
      begin
        if (AValue.TypeInfo = System.TypeInfo(TValue)) then
          AValue := TValue.FromVariant(AJsonObject[AName].O['value'].VariantValue)
        else
        begin
          ChildObject := AValue.AsObject;
          if Assigned(ChildObject) then
            JsonObjectToObject(AJsonObject.O[AName], ChildObject, AType, AIgnored);
        end;
      end;

    jdtArray:
      begin
        ChildObject := AValue.AsObject;
        if Assigned(ChildObject) then
        begin
          ChildList := TDuckTypedList.Wrap(ChildObject);
          if TMVCSerializerHelpful.AttributeExists<MVCListOfAttribute>(ACustomAttributes, ChildListOfAtt) then
            JsonArrayToList(AJsonObject.A[AName], ChildList, ChildListOfAtt.Value, AType, AIgnored)
          else
            raise EMVCDeserializationException.CreateFmt('You can not deserialize a list %s without the attribute MVCListClassTypeAttribute.', [AName]);
        end;
      end;
  end;
end;

procedure TMVCJsonDataObjectsSerializer.JsonObjectToObject(
  const AJsonObject: TJsonObject;
  const AObject: TObject;
  const AType: TMVCSerializationType;
  const AIgnoredAttributes: array of string);
var
  ObjType: TRttiType;
  Prop: TRttiProperty;
  Fld: TRttiField;
  AttributeValue: TValue;
begin
  ObjType := FRttiContext.GetType(AObject.ClassType);
  case AType of
    stDefault, stProperties:
      begin
        for Prop in ObjType.GetProperties do
          if (Prop.IsWritable or Prop.GetValue(AObject).IsObject) and (not TMVCSerializerHelpful.HasAttribute<MVCDoNotSerializeAttribute>(Prop)) and (not IsIgnoredAttribute(AIgnoredAttributes, Prop.Name)) then
          begin
            AttributeValue := Prop.GetValue(AObject);
            JsonDataValueToAttribute(AJsonObject, TMVCSerializerHelpful.GetKeyName(Prop, ObjType), AttributeValue, AType, AIgnoredAttributes, Prop.GetAttributes);
            if (not AttributeValue.IsEmpty) and Prop.IsWritable then
              Prop.SetValue(AObject, AttributeValue);
          end;
      end;
    stFields:
      begin
        for Fld in ObjType.GetFields do
          if (not TMVCSerializerHelpful.HasAttribute<MVCDoNotSerializeAttribute>(Fld)) and (not IsIgnoredAttribute(AIgnoredAttributes, Fld.Name)) then
          begin
            AttributeValue := Fld.GetValue(AObject);
            JsonDataValueToAttribute(AJsonObject, TMVCSerializerHelpful.GetKeyName(Fld, ObjType), AttributeValue, AType, AIgnoredAttributes, Fld.GetAttributes);
            if not AttributeValue.IsEmpty then
              Fld.SetValue(AObject, AttributeValue);
          end;
      end;
  end;
end;

procedure TMVCJsonDataObjectsSerializer.ObjectToJsonObject(
  const AObject: TObject;
  const AJsonObject: TJsonObject;
  const AType: TMVCSerializationType;
  const AIgnoredAttributes: array of string);
var
  ObjType: TRttiType;
  Prop: TRttiProperty;
  Fld: TRttiField;
begin
  ObjType := FRttiContext.GetType(AObject.ClassType);
  case AType of
    stDefault, stProperties:
      begin
        for Prop in ObjType.GetProperties do
          if (not TMVCSerializerHelpful.HasAttribute<MVCDoNotSerializeAttribute>(Prop)) and (not IsIgnoredAttribute(AIgnoredAttributes, Prop.Name)) then
            AttributeToJsonDataValue(AJsonObject, TMVCSerializerHelpful.GetKeyName(Prop, ObjType), Prop.GetValue(AObject), AType, AIgnoredAttributes, Prop.GetAttributes);
      end;
    stFields:
      begin
        for Fld in ObjType.GetFields do
          if (not TMVCSerializerHelpful.HasAttribute<MVCDoNotSerializeAttribute>(Fld)) and (not IsIgnoredAttribute(AIgnoredAttributes, Fld.Name)) then
            AttributeToJsonDataValue(AJsonObject, TMVCSerializerHelpful.GetKeyName(Fld, ObjType), Fld.GetValue(AObject), AType, AIgnoredAttributes, Fld.GetAttributes);
      end;
  end;
end;

procedure TMVCJsonDataObjectsSerializer.RegisterTypeSerializer(
  const ATypeInfo: PTypeInfo; AInstance: IMVCTypeSerializer);
begin
  FTypeSerializers.AddOrSetValue(ATypeInfo, AInstance);
end;

function TMVCJsonDataObjectsSerializer.SerializeCollection(
  const AList: TObject; const AType: TMVCSerializationType;
  const AIgnoredAttributes: array of string): string;
var
  JsonArray: TJsonArray;
  ObjList: IMVCList;
  Obj: TObject;
begin
  Result := EmptyStr;

  if not Assigned(AList) then
    Exit;

  if AList is TJsonBaseObject then
    Exit(TJsonBaseObject(AList).ToJSON(False));

  ObjList := TDuckTypedList.Wrap(AList);
  if Assigned(ObjList) then
  begin
    JsonArray := TJsonArray.Create;
    try
      for Obj in ObjList do
        if Assigned(Obj) then
          ObjectToJsonObject(Obj, JsonArray.AddObject, AType, AIgnoredAttributes);
      Result := JsonArray.ToJSON(False);
    finally
      JsonArray.Free;
    end;
  end;
end;

function TMVCJsonDataObjectsSerializer.SerializeCollection(
  const AList: TObject): string;
begin
  Result := SerializeCollection(AList, stDefault);
end;

function TMVCJsonDataObjectsSerializer.SerializeCollection(
  const AList: TObject; const AType: TMVCSerializationType): string;
begin
  Result := SerializeCollection(AList, AType, []);
end;

function TMVCJsonDataObjectsSerializer.SerializeDataSet(
  const ADataSet: TDataSet; const AIgnoredFields: array of string): string;
begin
  raise EMVCSerializationException.Create('Method TMVCJsonDataObjectsSerializer.SerializeDataSet not implemented.');
end;

function TMVCJsonDataObjectsSerializer.SerializeDataSet(
  const ADataSet: TDataSet): string;
begin
  Result := SerializeDataSet(ADataSet, []);
end;

function TMVCJsonDataObjectsSerializer.SerializeObject(
  const AObject: TObject; const AType: TMVCSerializationType;
  const AIgnoredAttributes: array of string): string;
var
  JsonObject: TJsonObject;
  ChildJsonValue: TJsonBaseObject;
  ObjType: TRttiType;
begin
  Result := EmptyStr;

  if not Assigned(AObject) then
    Exit;

  if AObject is TJsonBaseObject then
    Exit(TJsonBaseObject(AObject).ToJSON(False));

  ObjType := FRttiContext.GetType(AObject.ClassType);
  if FTypeSerializers.ContainsKey(ObjType.Handle) then
  begin
    ChildJsonValue := nil;
    FTypeSerializers.Items[ObjType.Handle].Serialize(AObject, TObject(ChildJsonValue), []);
    if Assigned(ChildJsonValue) then
    begin
      try
        if ChildJsonValue is TJsonBaseObject then
          Result := ChildJsonValue.ToJSON(False)
        else
          raise EMVCSerializationException.Create('Can not serialize the serializer does not have a valid TJsonBaseObject type.');
      finally
        ChildJsonValue.Free;
      end;
    end;
    Exit;
  end;

  JsonObject := TJsonObject.Create;
  try
    ObjectToJsonObject(AObject, JsonObject, AType, AIgnoredAttributes);
    Result := JsonObject.ToJSON(False);
  finally
    JsonObject.Free;
  end;
end;

function TMVCJsonDataObjectsSerializer.SerializeObject(
  const AObject: TObject; const AType: TMVCSerializationType): string;
begin
  Result := SerializeObject(AObject, AType, []);
end;

function TMVCJsonDataObjectsSerializer.SerializeObject(
  const AObject: TObject): string;
begin
  Result := SerializeObject(AObject, stDefault);
end;

procedure TMVCJsonDataObjectsSerializer.DeserializeObject(
  const ASerializedObject: string;
  const AObject: TObject;
  const AType: TMVCSerializationType;
  const AIgnoredAttributes: array of string);
var
  JsonObject: TJsonObject;
  ObjType: TRttiType;
  ObjValue: TValue;
begin
  if (ASerializedObject = EmptyStr) then
    Exit;

  if not Assigned(AObject) then
    Exit;

  JsonObject := TJsonObject.Parse(ASerializedObject) as TJsonObject;
  try
    ObjType := FRttiContext.GetType(AObject.ClassType);
    if FTypeSerializers.ContainsKey(ObjType.Handle) then
    begin
      ObjValue := TValue.From<TObject>(AObject);
      FTypeSerializers.Items[ObjType.Handle].Deserialize(JsonObject, ObjValue, []);
      Exit;
    end;
    JsonObjectToObject(JsonObject, AObject, AType, AIgnoredAttributes);
  finally
    JsonObject.Free;
  end;
end;

{ TJsonValue }

constructor TJsonValue.Create;
begin
  inherited Create;
  FValue := EmptyStr;
end;

constructor TJsonValue.Create(const AValue: string);
begin
  Create;
  FValue := AValue;
end;

end.
