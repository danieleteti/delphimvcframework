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

unit MVCFramework.Serializer.JSON;

{$I dmvcframework.inc}

interface

uses
  System.JSON,
  System.SysUtils,
  System.Classes,
  System.Rtti,
  System.TypInfo,
  System.Variants,
  System.Generics.Collections,
  System.StrUtils,
  Data.DB,
  MVCFramework.Serializer.Intf,
  MVCFramework.Serializer.Commons,
  MVCFramework.DuckTyping;

type

  TMVCJSONSerializer = class(TMVCAbstractSerializer, IMVCSerializer)
  private
    procedure ObjectToJSONObject(
      const AObject: TObject;
      const AJSONObject: TJSONObject;
      const AType: TMVCSerializationType;
      const AIgnoredAttributes: array of string
      );
    procedure AttributeToJSONDataValue(
      const AJSONObject: TJSONObject;
      const AName: string;
      const AValue: TValue;
      const AType: TMVCSerializationType;
      const AIgnored: array of string;
      const ACustomAttributes: TArray<TCustomAttribute>
      );
    procedure JSONObjectToObject(
      const AJSONObject: TJSONObject;
      const AObject: TObject;
      const AType: TMVCSerializationType;
      const AIgnoredAttributes: array of string
      );
    procedure JSONDataValueToAttribute(
      const AJSONObject: TJSONObject;
      const AName: string;
      var AValue: TValue;
      const AType: TMVCSerializationType;
      const AIgnored: array of string;
      const ACustomAttributes: TArray<TCustomAttribute>
      );
    procedure JSONArrayToList(
      const AJSONArray: TJSONArray;
      const AList: IMVCList;
      const AClazz: TClass;
      const AType: TMVCSerializationType;
      const AIgnoredAttributes: array of string
      );
  protected
    function SerializeObject(const AObject: TObject): string; overload;
    function SerializeObject(const AObject: TObject; const AType: TMVCSerializationType): string; overload;
    function SerializeObject(const AObject: TObject; const AType: TMVCSerializationType; const AIgnoredAttributes: array of string): string; overload;

    function SerializeCollection(const AList: TObject): string; overload;
    function SerializeCollection(const AList: TObject; const AType: TMVCSerializationType): string; overload;
    function SerializeCollection(const AList: TObject; const AType: TMVCSerializationType; const AIgnoredAttributes: array of string): string; overload;

    function SerializeDataSet(const ADataSet: TDataSet): string; overload;
    function SerializeDataSet(const ADataSet: TDataSet; const AIgnoredFields: array of string): string; overload;

    function SerializeDataSetRecord(const ADataSet: TDataSet): string; overload;
    function SerializeDataSetRecord(const ADataSet: TDataSet; const AIgnoredFields: array of string): string; overload;

    procedure DeserializeObject(const ASerializedObject: string; const AObject: TObject); overload;
    procedure DeserializeObject(const ASerializedObject: string; const AObject: TObject; const AType: TMVCSerializationType); overload;
    procedure DeserializeObject(const ASerializedObject: string; const AObject: TObject; const AType: TMVCSerializationType; const AIgnoredAttributes: array of string); overload;

    procedure DeserializeCollection(const ASerializedList: string; const AList: TObject; const AClazz: TClass); overload;
    procedure DeserializeCollection(const ASerializedList: string; const AList: TObject; const AClazz: TClass; const AType: TMVCSerializationType); overload;
    procedure DeserializeCollection(const ASerializedList: string; const AList: TObject; const AClazz: TClass; const AType: TMVCSerializationType; const AIgnoredAttributes: array of string); overload;

    procedure DeserializeDataSet(const ASerializedDataSet: string; const ADataSet: TDataSet);
    procedure DeserializeDataSetRecord(const ASerializedDataSetRecord: string; const ADataSet: TDataSet);
  public
    procedure AfterConstruction; override;
  end;

implementation

uses
  MVCFramework.Serializer.JSON.CustomTypes;

{ TMVCJSONSerializer }

procedure TMVCJSONSerializer.AfterConstruction;
begin
  inherited AfterConstruction;
  GetTypeSerializers.Add(System.TypeInfo(TStream), TStreamSerializerJSON.Create);
  GetTypeSerializers.Add(System.TypeInfo(TStringStream), TStreamSerializerJSON.Create);
  GetTypeSerializers.Add(System.TypeInfo(TMemoryStream), TStreamSerializerJSON.Create);
end;

procedure TMVCJSONSerializer.AttributeToJSONDataValue(
  const AJSONObject: TJSONObject; const AName: string;
  const AValue: TValue; const AType: TMVCSerializationType;
  const AIgnored: array of string;
  const ACustomAttributes: TArray<TCustomAttribute>);
var
  ChildJSONObject: TJSONObject;
  ChildJSONArray: TJSONArray;
  ChildValue: TValue;
  ChildObject, Obj: TObject;
  ChildList: IMVCList;
  ChildJSONValue: TJSONValue;
  ValueTypeAtt: MVCValueAsTypeAttribute;
  CastValue, CastedValue: TValue;
begin
  if AValue.IsEmpty then
  begin
    AJSONObject.AddPair(AName, TJSONNull.Create);
    Exit;
  end;

  if GetTypeSerializers.ContainsKey(AValue.TypeInfo) then
  begin
    ChildJSONValue := nil;
    GetTypeSerializers.Items[AValue.TypeInfo].Serialize(AValue, TObject(ChildJSONValue), ACustomAttributes);
    if Assigned(ChildJSONValue) then
    begin
      if ChildJSONValue is TJSONValue then
        AJSONObject.AddPair(AName, ChildJSONValue)
      else
        raise EMVCSerializationException.CreateFmt('Can not serialize %s the serializer does not have a valid TJSONValue type.', [AName]);
    end;
    Exit;
  end;

  case AValue.Kind of
    tkInteger:
      AJSONObject.AddPair(AName, TJSONNumber.Create(AValue.AsInteger));

    tkInt64:
      AJSONObject.AddPair(AName, TJSONNumber.Create(AValue.AsInt64));

    tkChar, tkString, tkWChar, tkLString, tkWString, tkUString:
      AJSONObject.AddPair(AName, TJSONString.Create(AValue.AsString));

    tkFloat:
      begin
        if (AValue.TypeInfo = System.TypeInfo(TDate)) then
        begin
          if (AValue.AsExtended = 0) then
            AJSONObject.AddPair(AName, TJSONNull.Create)
          else
            AJSONObject.AddPair(AName, TJSONString.Create(DateToISODate(AValue.AsExtended)));
        end
        else if (AValue.TypeInfo = System.TypeInfo(TDateTime)) then
        begin
          if (AValue.AsExtended = 0) then
            AJSONObject.AddPair(AName, TJSONNull.Create)
          else
            AJSONObject.AddPair(AName, TJSONString.Create(DateTimeToISOTimeStamp(AValue.AsExtended)));
        end
        else if (AValue.TypeInfo = System.TypeInfo(TTime)) then
        begin
          if (AValue.AsExtended = 0) then
            AJSONObject.AddPair(AName, TJSONNull.Create)
          else
            AJSONObject.AddPair(AName, TJSONString.Create(TimeToISOTime(AValue.AsExtended)));
        end
        else
          AJSONObject.AddPair(AName, TJSONNumber.Create(AValue.AsExtended));
      end;

    tkVariant:
      AJSONObject.AddPair(AName, AValue.AsVariant);

    tkEnumeration:
      begin
        if (AValue.TypeInfo = System.TypeInfo(Boolean)) then
        begin
          if AValue.AsBoolean then
            AJSONObject.AddPair(AName, TJSONBool.Create(True))
          else
            AJSONObject.AddPair(AName, TJSONBool.Create(False));
        end
        else
          AJSONObject.AddPair(AName, TJSONNumber.Create(AValue.AsOrdinal));
      end;

    tkClass:
      begin
        ChildObject := AValue.AsObject;
        if Assigned(ChildObject) then
        begin
          ChildList := TDuckTypedList.Wrap(ChildObject);
          if Assigned(ChildList) then
          begin
            ChildJSONArray := TJSONArray.Create;
            for Obj in ChildList do
              if Assigned(Obj) then
              begin
                ChildJSONObject := TJSONObject.Create;
                ObjectToJSONObject(Obj, ChildJSONObject, AType, AIgnored);
                ChildJSONArray.AddElement(ChildJSONObject);
              end;
            AJSONObject.AddPair(AName, ChildJSONArray);
          end
          else
          begin
            ChildJSONObject := TJSONObject.Create;
            ObjectToJSONObject(ChildObject, ChildJSONObject, AType, AIgnored);
            AJSONObject.AddPair(AName, ChildJSONObject);
          end;
        end
        else
        begin
          if TMVCSerializerHelpful.AttributeExists<MVCSerializeAsStringAttribute>(ACustomAttributes) then
            AJSONObject.AddPair(AName, TJSONString.Create(EmptyStr))
          else
            AJSONObject.AddPair(AName, TJSONNull.Create);
        end;
      end;

    tkRecord:
      begin
        if (AValue.TypeInfo = System.TypeInfo(TTimeStamp)) then
        begin
          AJSONObject.AddPair(AName, TJSONNumber.Create(TimeStampToMsecs(AValue.AsType<TTimeStamp>)));
        end
        else if (AValue.TypeInfo = System.TypeInfo(TValue)) then
        begin
          if TMVCSerializerHelpful.AttributeExists<MVCValueAsTypeAttribute>(ACustomAttributes, ValueTypeAtt) then
          begin
            CastValue := AValue.AsType<TValue>;
            if CastValue.TryCast(ValueTypeAtt.ValueTypeInfo, CastedValue) then
              AttributeToJSONDataValue(AJSONObject, AName, CastedValue, stDefault, [], [])
            else
              raise EMVCSerializationException.CreateFmt('Can not serialize %s of TypeKind tkRecord (TValue with MVCValueAsTypeAttribute).', [AName]);
          end
          else
          begin
            ChildValue := AValue.AsType<TValue>;
            ChildJSONObject := TJSONObject.Create;
            ChildJSONObject.AddPair('type', TMVCSerializerHelpful.GetTypeKindAsString(ChildValue.TypeInfo.Kind));
            AttributeToJSONDataValue(ChildJSONObject, 'value', ChildValue, stDefault, [], []);
            AJSONObject.AddPair(AName, ChildJSONObject);
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

procedure TMVCJSONSerializer.DeserializeCollection(
  const ASerializedList: string; const AList: TObject;
  const AClazz: TClass; const AType: TMVCSerializationType);
begin
  DeserializeCollection(ASerializedList, AList, AClazz, AType, []);
end;

procedure TMVCJSONSerializer.DeserializeCollection(
  const ASerializedList: string; const AList: TObject;
  const AClazz: TClass; const AType: TMVCSerializationType;
  const AIgnoredAttributes: array of string);
var
  JSONArray: TJSONArray;
  ObjList: IMVCList;
begin
  if (ASerializedList = EmptyStr) then
    Exit;

  if not Assigned(AList) then
    Exit;

  ObjList := TDuckTypedList.Wrap(AList);
  if Assigned(ObjList) then
  begin
    JSONArray := TJSONObject.ParseJSONValue(ASerializedList) as TJSONArray;
    try
      JSONArrayToList(JSONArray, ObjList, AClazz, AType, AIgnoredAttributes);
    finally
      JSONArray.Free;
    end;
  end;
end;

procedure TMVCJSONSerializer.DeserializeCollection(
  const ASerializedList: string; const AList: TObject;
  const AClazz: TClass);
begin
  DeserializeCollection(ASerializedList, AList, AClazz, stDefault);
end;

procedure TMVCJSONSerializer.DeserializeDataSet(
  const ASerializedDataSet: string; const ADataSet: TDataSet);
begin
  raise EMVCSerializationException.Create('Method TMVCJSONSerializer.DeserializeDataSet not implemented.');
end;

procedure TMVCJSONSerializer.DeserializeDataSetRecord(const ASerializedDataSetRecord: string; const ADataSet: TDataSet);
begin
  raise EMVCSerializationException.Create('Method TMVCJSONSerializer.DeserializeDataSetRecord not implemented.');
end;

procedure TMVCJSONSerializer.DeserializeObject(
  const ASerializedObject: string; const AObject: TObject;
  const AType: TMVCSerializationType);
begin
  DeserializeObject(ASerializedObject, AObject, AType, []);
end;

procedure TMVCJSONSerializer.DeserializeObject(
  const ASerializedObject: string; const AObject: TObject;
  const AType: TMVCSerializationType;
  const AIgnoredAttributes: array of string);
var
  JSONObject: TJSONObject;
  ObjType: TRttiType;
  ObjValue: TValue;
begin
  if (ASerializedObject = EmptyStr) then
    Exit;

  if not Assigned(AObject) then
    Exit;

  JSONObject := TJSONObject.ParseJSONValue(ASerializedObject) as TJSONObject;
  try
    ObjType := GetRttiContext.GetType(AObject.ClassType);
    if GetTypeSerializers.ContainsKey(ObjType.Handle) then
    begin
      ObjValue := TValue.From<TObject>(AObject);
      GetTypeSerializers.Items[ObjType.Handle].Deserialize(JSONObject, ObjValue, []);
      Exit;
    end;
    JSONObjectToObject(JSONObject, AObject, AType, AIgnoredAttributes);
  finally
    JSONObject.Free;
  end;
end;

procedure TMVCJSONSerializer.DeserializeObject(
  const ASerializedObject: string; const AObject: TObject);
begin
  DeserializeObject(ASerializedObject, AObject, stDefault);
end;

procedure TMVCJSONSerializer.JSONArrayToList(const AJSONArray: TJSONArray;
  const AList: IMVCList; const AClazz: TClass;
  const AType: TMVCSerializationType;
  const AIgnoredAttributes: array of string);
var
  I: Integer;
  Obj: TObject;
begin
  for I := 0 to Pred(AJSONArray.Count) do
  begin
    Obj := TMVCSerializerHelpful.CreateObject(AClazz.QualifiedClassName);
    JSONObjectToObject(AJSONArray.Items[I] as TJSONObject, Obj, AType, AIgnoredAttributes);
    AList.Add(Obj);
  end;
end;

procedure TMVCJSONSerializer.JSONDataValueToAttribute(
  const AJSONObject: TJSONObject; const AName: string; var AValue: TValue;
  const AType: TMVCSerializationType; const AIgnored: array of string;
  const ACustomAttributes: TArray<TCustomAttribute>);
var
  ChildObject: TObject;
  ChildList: IMVCList;
  ChildListOfAtt: MVCListOfAttribute;
begin
  if not Assigned(AJSONObject.Values[AName]) then
    Exit;

  if GetTypeSerializers.ContainsKey(AValue.TypeInfo) then
  begin
    GetTypeSerializers.Items[AValue.TypeInfo].Deserialize(AJSONObject.Values[AName], AValue, ACustomAttributes);
    Exit;
  end;

  case AnsiIndexStr(AJSONObject.Values[AName].ClassName, ['TJSONString', 'TJSONNumber', 'TJSONBool', 'TJSONTrue', 'TJSONFalse', 'TJSONObject', 'TJSONArray']) of
    0 { TJSONString } :
      begin
        if (AValue.TypeInfo = System.TypeInfo(TDate)) then
          AValue := TValue.From<TDate>(ISODateToDate(AJSONObject.Values[AName].Value))

        else if (AValue.TypeInfo = System.TypeInfo(TDateTime)) then
          AValue := TValue.From<TDateTime>(ISOTimeStampToDateTime(AJSONObject.Values[AName].Value))

        else if (AValue.TypeInfo = System.TypeInfo(TTime)) then
          AValue := TValue.From<TTime>(ISOTimeToTime(AJSONObject.Values[AName].Value))

        else
          AValue := TValue.From<string>(AJSONObject.Values[AName].Value);
      end;
    1 { TJSONNumber } :
      begin
        if (AValue.Kind = tkEnumeration) then
          TValue.Make(TJSONNumber(AJSONObject.Values[AName]).AsInt64, AValue.TypeInfo, AValue)

        else if (AValue.TypeInfo = System.TypeInfo(TTimeStamp)) then
          AValue := TValue.From<TTimeStamp>(MSecsToTimeStamp(TJSONNumber(AJSONObject.Values[AName]).AsInt64))

        else if (AValue.Kind = tkFloat) then
          AValue := TValue.From<Double>(TJSONNumber(AJSONObject.Values[AName]).AsDouble)

        else
          AValue := TValue.From<Int64>(TJSONNumber(AJSONObject.Values[AName]).AsInt64);
      end;
    2 { TJSONBool } , 3 { TJSONTrue } , 4 { TJSONFalse } :
      begin
        AValue := TValue.From<Boolean>(TJSONBool(AJSONObject.Values[AName]).AsBoolean);
      end;
    5 { TJSONObject } :
      begin
        if (AValue.TypeInfo = System.TypeInfo(TValue)) then
          AValue := TValue.FromVariant(TJSONObject(AJSONObject.Values[AName]).Values['value'].Value)
        else
        begin
          ChildObject := AValue.AsObject;
          if Assigned(ChildObject) then
            JSONObjectToObject(AJSONObject.Values[AName] as TJSONObject, ChildObject, AType, AIgnored);
        end;
      end;
    6 { TJSONArray } :
      begin
        ChildObject := AValue.AsObject;
        if Assigned(ChildObject) then
        begin
          ChildList := TDuckTypedList.Wrap(ChildObject);
          if TMVCSerializerHelpful.AttributeExists<MVCListOfAttribute>(ACustomAttributes, ChildListOfAtt) then
            JSONArrayToList(AJSONObject.Values[AName] as TJSONArray, ChildList, ChildListOfAtt.Value, AType, AIgnored)
          else
            raise EMVCDeserializationException.CreateFmt('You can not deserialize a list %s without the attribute MVCListClassTypeAttribute.', [AName]);
        end;
      end;
    -1: Exit;
  end;
end;

procedure TMVCJSONSerializer.JSONObjectToObject(
  const AJSONObject: TJSONObject; const AObject: TObject;
  const AType: TMVCSerializationType;
  const AIgnoredAttributes: array of string);
var
  ObjType: TRttiType;
  Prop: TRttiProperty;
  Fld: TRttiField;
  AttributeValue: TValue;
begin
  ObjType := GetRttiContext.GetType(AObject.ClassType);
  case AType of
    stDefault, stProperties:
      begin
        for Prop in ObjType.GetProperties do
          if (Prop.IsWritable or Prop.GetValue(AObject).IsObject) and (not TMVCSerializerHelpful.HasAttribute<MVCDoNotSerializeAttribute>(Prop)) and (not IsIgnoredAttribute(AIgnoredAttributes, Prop.Name)) then
          begin
            AttributeValue := Prop.GetValue(AObject);
            JSONDataValueToAttribute(AJsonObject, TMVCSerializerHelpful.GetKeyName(Prop, ObjType), AttributeValue, AType, AIgnoredAttributes, Prop.GetAttributes);
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
            JSONDataValueToAttribute(AJsonObject, TMVCSerializerHelpful.GetKeyName(Fld, ObjType), AttributeValue, AType, AIgnoredAttributes, Fld.GetAttributes);
            if not AttributeValue.IsEmpty then
              Fld.SetValue(AObject, AttributeValue);
          end;
      end;
  end;
end;

procedure TMVCJSONSerializer.ObjectToJSONObject(const AObject: TObject;
  const AJSONObject: TJSONObject; const AType: TMVCSerializationType;
  const AIgnoredAttributes: array of string);
var
  ObjType: TRttiType;
  Prop: TRttiProperty;
  Fld: TRttiField;
begin
  ObjType := GetRttiContext.GetType(AObject.ClassType);
  case AType of
    stDefault, stProperties:
      begin
        for Prop in ObjType.GetProperties do
          if (not TMVCSerializerHelpful.HasAttribute<MVCDoNotSerializeAttribute>(Prop)) and (not IsIgnoredAttribute(AIgnoredAttributes, Prop.Name)) then
            AttributeToJSONDataValue(AJsonObject, TMVCSerializerHelpful.GetKeyName(Prop, ObjType), Prop.GetValue(AObject), AType, AIgnoredAttributes, Prop.GetAttributes);
      end;
    stFields:
      begin
        for Fld in ObjType.GetFields do
          if (not TMVCSerializerHelpful.HasAttribute<MVCDoNotSerializeAttribute>(Fld)) and (not IsIgnoredAttribute(AIgnoredAttributes, Fld.Name)) then
            AttributeToJSONDataValue(AJsonObject, TMVCSerializerHelpful.GetKeyName(Fld, ObjType), Fld.GetValue(AObject), AType, AIgnoredAttributes, Fld.GetAttributes);
      end;
  end;
end;

function TMVCJSONSerializer.SerializeCollection(
  const AList: TObject): string;
begin
  Result := SerializeCollection(AList, stDefault);
end;

function TMVCJSONSerializer.SerializeCollection(const AList: TObject;
  const AType: TMVCSerializationType): string;
begin
  Result := SerializeCollection(AList, AType, []);
end;

function TMVCJSONSerializer.SerializeCollection(const AList: TObject;
  const AType: TMVCSerializationType;
  const AIgnoredAttributes: array of string): string;
var
  JSONArray: TJSONArray;
  JSONObject: TJSONObject;
  ObjList: IMVCList;
  Obj: TObject;
begin
  Result := EmptyStr;

  if not Assigned(AList) then
    Exit;

  if AList is TJSONValue then
    Exit(TJSONValue(AList).ToJSON);

  ObjList := TDuckTypedList.Wrap(AList);
  if Assigned(ObjList) then
  begin
    JSONArray := TJSONArray.Create;
    try
      for Obj in ObjList do
        if Assigned(Obj) then
        begin
          JSONObject := TJSONObject.Create;
          ObjectToJsonObject(Obj, JSONObject, AType, AIgnoredAttributes);
          JSONArray.AddElement(JSONObject);
        end;
      Result := JSONArray.ToJSON;
    finally
      JSONArray.Free;
    end;
  end;
end;

function TMVCJSONSerializer.SerializeDataSet(const ADataSet: TDataSet;
  const AIgnoredFields: array of string): string;
begin
  raise EMVCSerializationException.Create('Method TMVCJSONSerializer.SerializeDataSet not implemented.');
end;

function TMVCJSONSerializer.SerializeDataSetRecord(const ADataSet: TDataSet): string;
begin
  Result := SerializeDataSetRecord(ADataSet, []);
end;

function TMVCJSONSerializer.SerializeDataSetRecord(const ADataSet: TDataSet; const AIgnoredFields: array of string): string;
begin
  raise EMVCSerializationException.Create('Method TMVCJSONSerializer.SerializeDataSetRecord not implemented.');
end;

function TMVCJSONSerializer.SerializeDataSet(
  const ADataSet: TDataSet): string;
begin
  Result := SerializeDataSet(ADataSet, []);
end;

function TMVCJSONSerializer.SerializeObject(const AObject: TObject;
  const AType: TMVCSerializationType;
  const AIgnoredAttributes: array of string): string;
var
  JSONObject: TJSONObject;
  ChildJSONValue: TJSONValue;
  ObjType: TRttiType;
begin
  Result := EmptyStr;

  if not Assigned(AObject) then
    Exit;

  if AObject is TJSONValue then
    Exit(TJSONValue(AObject).ToJSON);

  ObjType := GetRttiContext.GetType(AObject.ClassType);
  if GetTypeSerializers.ContainsKey(ObjType.Handle) then
  begin
    ChildJSONValue := nil;
    GetTypeSerializers.Items[ObjType.Handle].Serialize(AObject, TObject(ChildJSONValue), []);
    if Assigned(ChildJSONValue) then
    begin
      try
        if ChildJSONValue is TJSONValue then
          Result := ChildJSONValue.ToJSON
        else
          raise EMVCSerializationException.Create('Can not serialize the serializer does not have a valid TJSONValue type.');
      finally
        ChildJSONValue.Free;
      end;
    end;
    Exit;
  end;

  JSONObject := TJSONObject.Create;
  try
    ObjectToJSONObject(AObject, JSONObject, AType, AIgnoredAttributes);
    Result := JSONObject.ToJSON;
  finally
    JSONObject.Free;
  end;
end;

function TMVCJSONSerializer.SerializeObject(const AObject: TObject;
  const AType: TMVCSerializationType): string;
begin
  Result := SerializeObject(AObject, AType, []);
end;

function TMVCJSONSerializer.SerializeObject(
  const AObject: TObject): string;
begin
  Result := SerializeObject(AObject, stDefault);
end;

end.
