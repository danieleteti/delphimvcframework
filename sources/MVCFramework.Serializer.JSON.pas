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
  Data.SqlTimSt,
  Data.FmtBcd,
  Data.DB,
  MVCFramework.Serializer.Intf,
  MVCFramework.Serializer.Abstract,
  MVCFramework.Serializer.Commons,
  MVCFramework.DuckTyping;

type

  TMVCJSONSerializer = class(TMVCAbstractSerializer, IMVCSerializer)
  private
    procedure ObjectToJSONObject(
      const AObject: TObject;
      const AJSONObject: TJSONObject;
      const AType: TMVCSerializationType;
      const AIgnoredAttributes: TMVCIgnoredList
      );
    procedure AttributeToJSONDataValue(
      const AJSONObject: TJSONObject;
      const AName: string;
      const AValue: TValue;
      const AType: TMVCSerializationType;
      const AIgnored: TMVCIgnoredList;
      const ACustomAttributes: TArray<TCustomAttribute>
      );
    procedure JSONObjectToObject(
      const AJSONObject: TJSONObject;
      const AObject: TObject;
      const AType: TMVCSerializationType;
      const AIgnoredAttributes: TMVCIgnoredList
      );
    procedure JSONDataValueToAttribute(
      const AJSONObject: TJSONObject;
      const AName: string;
      var AValue: TValue;
      const AType: TMVCSerializationType;
      const AIgnored: TMVCIgnoredList;
      const ACustomAttributes: TArray<TCustomAttribute>
      );
    procedure JSONArrayToList(
      const AJSONArray: TJSONArray;
      const AList: IMVCList;
      const AClazz: TClass;
      const AType: TMVCSerializationType;
      const AIgnoredAttributes: TMVCIgnoredList
      );
    procedure DataSetToJSONObject(
      const ADataSet: TDataSet;
      const AJSONObject: TJSONObject;
      const ANameCase: TMVCNameCase;
      const AIgnoredFields: TMVCIgnoredList
      );
    procedure JSONObjectToDataSet(
      const AJSONObject: TJSONObject;
      const ADataSet: TDataSet;
      const AIgnoredFields: TMVCIgnoredList;
      const ANameCase: TMVCNameCase
      );
    procedure JSONArrayToDataSet(
      const AJSONArray: TJSONArray;
      const ADataSet: TDataSet;
      const AIgnoredFields: TMVCIgnoredList;
      const ANameCase: TMVCNameCase
      );
  protected
    function SerializeObject(
      const AObject: TObject;
      const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = []
      ): string;

    function SerializeCollection(
      const AList: TObject;
      const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = []
      ): string;

    function SerializeDataSet(
      const ADataSet: TDataSet;
      const AIgnoredFields: TMVCIgnoredList = [];
      const ANameCase: TMVCNameCase = ncAsIs
      ): string;

    function SerializeDataSetRecord(
      const ADataSet: TDataSet;
      const AIgnoredFields: TMVCIgnoredList = [];
      const ANameCase: TMVCNameCase = ncAsIs
      ): string;

    procedure DeserializeObject(
      const ASerializedObject: string;
      const AObject: TObject;
      const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = []
      );

    procedure DeserializeCollection(
      const ASerializedList: string;
      const AList: TObject;
      const AClazz: TClass;
      const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = []
      );

    procedure DeserializeDataSet(
      const ASerializedDataSet: string;
      const ADataSet: TDataSet;
      const AIgnoredFields: TMVCIgnoredList = [];
      const ANameCase: TMVCNameCase = ncAsIs
      );

    procedure DeserializeDataSetRecord(
      const ASerializedDataSetRecord: string;
      const ADataSet: TDataSet;
      const AIgnoredFields: TMVCIgnoredList = [];
      const ANameCase: TMVCNameCase = ncAsIs
      );
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
  const AIgnored: TMVCIgnoredList;
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
                ObjectToJSONObject(Obj, ChildJSONObject, GetSerializationType(Obj, AType), AIgnored);
                ChildJSONArray.AddElement(ChildJSONObject);
              end;
            AJSONObject.AddPair(AName, ChildJSONArray);
          end
          else
          begin
            ChildJSONObject := TJSONObject.Create;
            ObjectToJSONObject(ChildObject, ChildJSONObject, GetSerializationType(ChildObject, AType), AIgnored);
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

procedure TMVCJSONSerializer.DataSetToJSONObject(
  const ADataSet: TDataSet; const AJSONObject: TJSONObject;
  const ANameCase: TMVCNameCase; const AIgnoredFields: TMVCIgnoredList);
var
  I: Integer;
  FieldName: string;
  MS: TMemoryStream;
  SS: TStringStream;
  NestedDataSet: TDataSet;
  ChildJSONArray: TJSONArray;
  ChildJSONObject: TJSONObject;
begin
  for I := 0 to ADataSet.FieldCount - 1 do
  begin
    FieldName := GetNameAs(ADataSet.Owner, ADataSet.Fields[I].Name, ADataSet.Fields[I].FieldName);
    if (not IsIgnoredAttribute(AIgnoredFields, FieldName)) and (not IsIgnoredComponent(ADataSet.Owner, ADataSet.Fields[I].Name)) then
    begin
      case ANameCase of
        ncUpperCase: FieldName := UpperCase(ADataSet.Fields[I].FieldName);
        ncLowerCase: FieldName := LowerCase(ADataSet.Fields[I].FieldName);
      end;
      if ADataSet.Fields[I].IsNull then
        AJSONObject.AddPair(FieldName, TJSONNull.Create)
      else
      begin
        case ADataSet.Fields[I].DataType of
          ftBoolean:
            AJSONObject.AddPair(FieldName, TJSONBool.Create(ADataSet.Fields[I].AsBoolean));

          ftInteger, ftSmallint, ftShortint:
            AJSONObject.AddPair(FieldName, TJSONNumber.Create(ADataSet.Fields[I].AsInteger));

          ftLargeint:
            AJSONObject.AddPair(FieldName, TJSONNumber.Create(ADataSet.Fields[I].AsLargeInt));

          ftSingle, ftFloat:
            AJSONObject.AddPair(FieldName, TJSONNumber.Create(ADataSet.Fields[I].AsFloat));

          ftString, ftWideString, ftMemo, ftWideMemo:
            AJSONObject.AddPair(FieldName, TJSONString.Create(ADataSet.Fields[I].AsWideString));

          ftDate:
            AJSONObject.AddPair(FieldName, TJSONString.Create(DateToISODate(ADataSet.Fields[I].AsDateTime)));

          ftDateTime:
            AJSONObject.AddPair(FieldName, TJSONString.Create(DateTimeToISOTimeStamp(ADataSet.Fields[I].AsDateTime)));

          ftTime, ftTimeStamp:
            AJSONObject.AddPair(FieldName, TJSONString.Create(SQLTimeStampToStr('hh:nn:ss', ADataSet.Fields[I].AsSQLTimeStamp)));

          ftCurrency:
            AJSONObject.AddPair(FieldName, TJSONNumber.Create(ADataSet.Fields[I].AsCurrency));

          ftFMTBcd, ftBCD:
            AJSONObject.AddPair(FieldName, TJSONNumber.Create(BcdToDouble(ADataSet.Fields[I].AsBcd)));

          ftGraphic, ftBlob, ftStream:
            begin
              MS := TMemoryStream.Create;
              try
                TBlobField(ADataSet.Fields[I]).SaveToStream(MS);
                MS.Position := 0;
                SS := TStringStream.Create;
                try
                  TMVCSerializerHelpful.EncodeStream(MS, SS);
                  AJSONObject.AddPair(FieldName, TJSONString.Create(SS.DataString));
                finally
                  SS.Free;
                end;
              finally
                MS.Free;
              end;
            end;

          ftDataSet:
            begin
              NestedDataSet := TDataSetField(ADataSet.Fields[I]).NestedDataSet;
              case GetDataType(ADataSet.Owner, ADataSet.Fields[I].Name, dtArray) of
                dtArray:
                  begin
                    ChildJSONArray := TJSONArray.Create;
                    NestedDataSet.First;
                    while not NestedDataSet.Eof do
                    begin
                      ChildJSONObject := TJSONObject.Create;
                      DataSetToJSONObject(NestedDataSet, ChildJSONObject, GetNameCase(NestedDataSet, ANameCase), AIgnoredFields);
                      ChildJSONArray.AddElement(ChildJSONObject);
                      NestedDataSet.Next;
                    end;
                    AJSONObject.AddPair(FieldName, ChildJSONArray);
                  end;
                dtObject:
                  begin
                    ChildJSONObject := TJSONObject.Create;
                    DataSetToJSONObject(NestedDataSet, ChildJSONObject, GetNameCase(NestedDataSet, ANameCase), AIgnoredFields);
                    AJSONObject.AddPair(FieldName, ChildJSONObject);
                  end;
              end;
            end;
        else
          raise EMVCSerializationException.CreateFmt('Cannot find type for field "%s"', [FieldName]);
        end;
      end;
    end;
  end;
end;

procedure TMVCJSONSerializer.DeserializeCollection(
  const ASerializedList: string; const AList: TObject;
  const AClazz: TClass; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList);
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

procedure TMVCJSONSerializer.DeserializeDataSet(
  const ASerializedDataSet: string;
  const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList;
  const ANameCase: TMVCNameCase);
var
  JSONArray: TJSONArray;
begin
  if (ASerializedDataSet = EmptyStr) or (not Assigned(ADataSet)) then
    Exit;

  JSONArray := TJSONObject.ParseJSONValue(ASerializedDataSet) as TJSONArray;
  try
    JSONArrayToDataSet(JSONArray, ADataSet, AIgnoredFields, ANameCase);
  finally
    JSONArray.Free;
  end;
end;

procedure TMVCJSONSerializer.DeserializeDataSetRecord(
  const ASerializedDataSetRecord: string;
  const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList;
  const ANameCase: TMVCNameCase);
var
  JSONObject: TJSONObject;
begin
  if (ASerializedDataSetRecord = EmptyStr) or (not Assigned(ADataSet)) then
    Exit;

  JSONObject := TJSONObject.ParseJSONValue(ASerializedDataSetRecord) as TJSONObject;
  try
    ADataSet.Edit;
    JSONObjectToDataSet(JSONObject, ADataSet, AIgnoredFields, ANameCase);
    ADataSet.Post;
  finally
    JSONObject.Free;
  end;
end;

procedure TMVCJSONSerializer.DeserializeObject(
  const ASerializedObject: string; const AObject: TObject;
  const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList);
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
    JSONObjectToObject(JSONObject, AObject, GetSerializationType(AObject, AType), AIgnoredAttributes);
  finally
    JSONObject.Free;
  end;
end;

procedure TMVCJSONSerializer.JSONArrayToDataSet(
  const AJSONArray: TJSONArray; const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase);
var
  I: Integer;
begin
  for I := 0 to Pred(AJSONArray.Count) do
  begin
    ADataSet.Append;
    JSONObjectToDataSet(AJSONArray.Items[I] as TJSONObject, ADataSet, AIgnoredFields, ANameCase);
    ADataSet.Post;
  end;
end;

procedure TMVCJSONSerializer.JSONArrayToList(const AJSONArray: TJSONArray;
  const AList: IMVCList; const AClazz: TClass;
  const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList);
var
  I: Integer;
  Obj: TObject;
begin
  for I := 0 to Pred(AJSONArray.Count) do
  begin
    Obj := TMVCSerializerHelpful.CreateObject(AClazz.QualifiedClassName);
    JSONObjectToObject(AJSONArray.Items[I] as TJSONObject, Obj, GetSerializationType(Obj, AType), AIgnoredAttributes);
    AList.Add(Obj);
  end;
end;

procedure TMVCJSONSerializer.JSONDataValueToAttribute(
  const AJSONObject: TJSONObject; const AName: string; var AValue: TValue;
  const AType: TMVCSerializationType; const AIgnored: TMVCIgnoredList;
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
            JSONObjectToObject(AJSONObject.Values[AName] as TJSONObject, ChildObject, GetSerializationType(ChildObject, AType), AIgnored);
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

procedure TMVCJSONSerializer.JSONObjectToDataSet(
  const AJSONObject: TJSONObject; const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase);
var
  Field: TField;
  Name: string;
  Jv: TJSONValue;
  SS: TStringStream;
  SM: TMemoryStream;
  NestedDataSet: TDataSet;
begin
  if (ADataSet.State in [dsInsert, dsEdit]) then
  begin
    for Field in ADataSet.Fields do
    begin
      Name := GetNameAs(ADataSet.Owner, Field.Name, Field.FieldName);

      if (IsIgnoredAttribute(AIgnoredFields, Name)) or (IsIgnoredComponent(ADataSet.Owner, Field.Name)) then
        Continue;

      case GetNameCase(ADataSet, ANameCase) of
        ncLowerCase: Name := LowerCase(Field.FieldName);
        ncUpperCase: Name := UpperCase(Field.FieldName);
      end;

      Jv := AJSONObject.Get(Name).JsonValue;
      if not Assigned(Jv) then
        Continue;

      if Jv is TJSONNull then
      begin
        Field.Clear;
        Continue;
      end;

      case field.DataType of
        TFieldType.ftBoolean:
          Field.AsBoolean := (Jv as TJSONBool).AsBoolean;

        TFieldType.ftInteger, TFieldType.ftSmallint, TFieldType.ftShortint:
          Field.AsInteger := (Jv as TJSONNumber).AsInt;

        TFieldType.ftLargeint:
          Field.AsLargeInt := (Jv as TJSONNumber).AsInt64;

        TFieldType.ftCurrency:
          Field.AsCurrency := (Jv as TJSONNumber).AsDouble;

        TFieldType.ftSingle:
          Field.AsSingle := (Jv as TJSONNumber).AsDouble;

        TFieldType.ftFloat, TFieldType.ftFMTBcd, TFieldType.ftBCD:
          Field.AsFloat := (Jv as TJSONNumber).AsDouble;

        ftString, ftWideString, ftMemo, ftWideMemo:
          Field.AsWideString := (Jv as TJSONString).Value;

        TFieldType.ftDate:
          Field.AsDateTime := ISODateToDate((Jv as TJSONString).Value);

        TFieldType.ftDateTime:
          Field.AsDateTime := ISOTimeStampToDateTime((Jv as TJSONString).Value);

        TFieldType.ftTimeStamp, TFieldType.ftTime:
          Field.AsDateTime := ISOTimeToTime((Jv as TJSONString).Value);

        TFieldType.ftGraphic, TFieldType.ftBlob, TFieldType.ftStream:
          begin
            SS := TStringStream.Create((Jv as TJSONString).Value);
            try
              SS.Position := 0;
              SM := TMemoryStream.Create;
              try
                TMVCSerializerHelpful.DecodeStream(SS, SM);
                TBlobField(Field).LoadFromStream(SM);
              finally
                SM.Free;
              end;
            finally
              SS.Free;
            end;
          end;

        TFieldType.ftDataSet:
          begin
            NestedDataSet := TDataSetField(Field).NestedDataSet;

            NestedDataSet.First;
            while not NestedDataSet.Eof do
              NestedDataSet.Delete;

            case GetDataType(ADataSet.Owner, Field.Name, dtArray) of
              dtArray:
                begin
                  JsonArrayToDataSet(Jv as TJSONArray, NestedDataSet, AIgnoredFields, ANameCase);
                end;
              dtObject:
                begin
                  NestedDataSet.Edit;
                  JsonObjectToDataSet(Jv as TJSONObject, NestedDataSet, AIgnoredFields, ANameCase);
                  NestedDataSet.Post;
                end;
            end;
          end;
      else
        raise EMVCDeserializationException.CreateFmt('Cannot find type for field "%s"', [Field.FieldName]);
      end;
    end;
  end;
end;

procedure TMVCJSONSerializer.JSONObjectToObject(
  const AJSONObject: TJSONObject; const AObject: TObject;
  const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList);
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
  const AIgnoredAttributes: TMVCIgnoredList);
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
  const AList: TObject;
  const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList): string;
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
          ObjectToJsonObject(Obj, JSONObject, GetSerializationType(Obj, AType), AIgnoredAttributes);
          JSONArray.AddElement(JSONObject);
        end;
      Result := JSONArray.ToJSON;
    finally
      JSONArray.Free;
    end;
  end;
end;

function TMVCJSONSerializer.SerializeDataSet(
  const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList;
  const ANameCase: TMVCNameCase): string;
var
  JSONArray: TJSONArray;
  JSONObject: TJSONObject;
  BookMark: TBookmark;
begin
  Result := EmptyStr;

  if (not Assigned(ADataSet)) or (ADataSet.IsEmpty) then
    Exit;

  JSONArray := TJSONArray.Create;
  try
    BookMark := ADataSet.Bookmark;
    ADataSet.First;
    while not ADataSet.Eof do
    begin
      JSONObject := TJSONObject.Create;
      DataSetToJSONObject(ADataSet, JSONObject, GetNameCase(ADataSet, ANameCase), AIgnoredFields);
      JSONArray.AddElement(JSONObject);
      ADataSet.Next;
    end;
    Result := JSONArray.ToJSON;
  finally
    JSONArray.Free;
    if ADataSet.BookmarkValid(BookMark) then
      ADataSet.GotoBookmark(BookMark);
    ADataSet.FreeBookmark(BookMark);
  end;
end;

function TMVCJSONSerializer.SerializeDataSetRecord(
  const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList;
  const ANameCase: TMVCNameCase): string;
var
  JSONObject: TJSONObject;
begin
  Result := EmptyStr;

  if (not Assigned(ADataSet)) or (ADataSet.IsEmpty) then
    Exit;

  JSONObject := TJSONObject.Create;
  try
    DataSetToJSONObject(ADataSet, JSONObject, GetNameCase(ADataSet, ANameCase), AIgnoredFields);
    Result := JSONObject.ToJSON;
  finally
    JSONObject.Free;
  end;
end;

function TMVCJSONSerializer.SerializeObject(
  const AObject: TObject;
  const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList): string;
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
    ObjectToJSONObject(AObject, JSONObject, GetSerializationType(AObject, AType), AIgnoredAttributes);
    Result := JSONObject.ToJSON;
  finally
    JSONObject.Free;
  end;
end;

end.
