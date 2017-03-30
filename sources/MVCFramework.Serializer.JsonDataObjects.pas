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

{$I dmvcframework.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Rtti,
  System.TypInfo,
  System.Variants,
  System.Generics.Collections,
  Data.SqlTimSt,
  Data.FmtBcd,
  Data.DB,
  MVCFramework.Serializer.Intf,
  MVCFramework.Serializer.Abstract,
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

  TMVCJsonDataObjectsSerializer = class(TMVCAbstractSerializer, IMVCSerializer)
  private
    procedure ObjectToJsonObject(
      const AObject: TObject;
      const AJsonObject: TJsonObject;
      const AType: TMVCSerializationType;
      const AIgnoredAttributes: TMVCIgnoredList
      );
    procedure AttributeToJsonDataValue(
      const AJsonObject: TJsonObject;
      const AName: string;
      const AValue: TValue;
      const AType: TMVCSerializationType;
      const AIgnored: TMVCIgnoredList;
      const ACustomAttributes: TArray<TCustomAttribute>
      );
    procedure JsonObjectToObject(
      const AJsonObject: TJsonObject;
      const AObject: TObject;
      const AType: TMVCSerializationType;
      const AIgnoredAttributes: TMVCIgnoredList
      );
    procedure JsonDataValueToAttribute(
      const AJsonObject: TJsonObject;
      const AName: string;
      var AValue: TValue;
      const AType: TMVCSerializationType;
      const AIgnored: TMVCIgnoredList;
      const ACustomAttributes: TArray<TCustomAttribute>
      );
    procedure JsonArrayToList(
      const AJsonArray: TJsonArray;
      const AList: IMVCList;
      const AClazz: TClass;
      const AType: TMVCSerializationType;
      const AIgnoredAttributes: TMVCIgnoredList
      );
    procedure DataSetToJsonObject(
      const ADataSet: TDataSet;
      const AJsonObject: TJsonObject;
      const ANameCase: TMVCNameCase;
      const AIgnoredFields: TMVCIgnoredList
      );
    procedure JsonObjectToDataSet(
      const AJsonObject: TJsonObject;
      const ADataSet: TDataSet;
      const AIgnoredFields: TMVCIgnoredList;
      const ANameCase: TMVCNameCase
      );
    procedure JsonArrayToDataSet(
      const AJsonArray: TJsonArray;
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
  MVCFramework.Serializer.JsonDataObjects.CustomTypes;

{ TMVCJsonDataObjectsSerializer }

procedure TMVCJsonDataObjectsSerializer.AfterConstruction;
begin
  inherited AfterConstruction;
  GetTypeSerializers.Add(System.TypeInfo(TStream), TStreamSerializerJsonDataObject.Create);
  GetTypeSerializers.Add(System.TypeInfo(TStringStream), TStreamSerializerJsonDataObject.Create);
  GetTypeSerializers.Add(System.TypeInfo(TMemoryStream), TStreamSerializerJsonDataObject.Create);
end;

procedure TMVCJsonDataObjectsSerializer.AttributeToJsonDataValue(
  const AJsonObject: TJsonObject;
  const AName: string;
  const AValue: TValue;
  const AType: TMVCSerializationType;
  const AIgnored: TMVCIgnoredList;
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

  if GetTypeSerializers.ContainsKey(AValue.TypeInfo) then
  begin
    ChildJsonValue := nil;
    GetTypeSerializers.Items[AValue.TypeInfo].Serialize(AValue, ChildJsonValue, ACustomAttributes);
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
                ObjectToJsonObject(Obj, ChildJsonArray.AddObject, GetSerializationType(Obj, AType), AIgnored);
          end
          else
          begin
            ChildJsonObject := AJsonObject.O[AName];
            ObjectToJsonObject(ChildObject, ChildJsonObject, GetSerializationType(ChildObject, AType), AIgnored);
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

procedure TMVCJsonDataObjectsSerializer.DataSetToJsonObject(
  const ADataSet: TDataSet; const AJsonObject: TJsonObject;
  const ANameCase: TMVCNameCase; const AIgnoredFields: TMVCIgnoredList);
var
  I: Integer;
  FieldName: string;
  MS: TMemoryStream;
  SS: TStringStream;
  NestedDataSet: TDataSet;
  ChildJsonArray: TJsonArray;
  ChildJsonObject: TJsonObject;
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
        AJsonObject[FieldName] := Null
      else
      begin
        case ADataSet.Fields[I].DataType of
          ftBoolean:
            AJsonObject.B[FieldName] := ADataSet.Fields[I].AsBoolean;

          ftInteger, ftSmallint, ftShortint:
            AJsonObject.I[FieldName] := ADataSet.Fields[I].AsInteger;

          ftLargeint:
            AJsonObject.L[FieldName] := ADataSet.Fields[I].AsLargeInt;

          ftSingle, ftFloat:
            AJsonObject.F[FieldName] := ADataSet.Fields[I].AsFloat;

          ftString, ftWideString, ftMemo, ftWideMemo:
            AJsonObject.S[FieldName] := ADataSet.Fields[I].AsWideString;

          ftDate:
            AJsonObject.S[FieldName] := DateToISODate(ADataSet.Fields[I].AsDateTime);

          ftDateTime:
            AJsonObject.S[FieldName] := DateTimeToISOTimeStamp(ADataSet.Fields[I].AsDateTime);

          ftTime, ftTimeStamp:
            AJsonObject.S[FieldName] := SQLTimeStampToStr('hh:nn:ss', ADataSet.Fields[I].AsSQLTimeStamp);

          ftCurrency:
            AJsonObject.F[FieldName] := ADataSet.Fields[I].AsCurrency;

          ftFMTBcd, ftBCD:
            AJsonObject.F[FieldName] := BcdToDouble(ADataSet.Fields[I].AsBcd);

          ftGraphic, ftBlob, ftStream:
            begin
              MS := TMemoryStream.Create;
              try
                TBlobField(ADataSet.Fields[I]).SaveToStream(MS);
                MS.Position := 0;
                SS := TStringStream.Create;
                try
                  TMVCSerializerHelpful.EncodeStream(MS, SS);
                  AJsonObject.S[FieldName] := SS.DataString;
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
                    ChildJsonArray := AJsonObject.A[FieldName];
                    NestedDataSet.First;
                    while not NestedDataSet.Eof do
                    begin
                      DataSetToJsonObject(NestedDataSet, ChildJsonArray.AddObject, GetNameCase(NestedDataSet, ANameCase), AIgnoredFields);
                      NestedDataSet.Next;
                    end;
                  end;
                dtObject:
                  begin
                    ChildJsonObject := AJsonObject.O[FieldName];
                    DataSetToJsonObject(NestedDataSet, ChildJsonObject, GetNameCase(NestedDataSet, ANameCase), AIgnoredFields);
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

procedure TMVCJsonDataObjectsSerializer.DeserializeCollection(
  const ASerializedList: string;
  const AList: TObject;
  const AClazz: TClass;
  const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList);
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
  const ASerializedDataSet: string;
  const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList;
  const ANameCase: TMVCNameCase);
var
  JsonArray: TJsonArray;
begin
  if (ASerializedDataSet = EmptyStr) or (not Assigned(ADataSet)) then
    Exit;

  JsonArray := TJsonArray.Parse(ASerializedDataSet) as TJsonArray;
  try
    JsonArrayToDataSet(JsonArray, ADataSet, AIgnoredFields, ANameCase);
  finally
    JsonArray.Free;
  end;
end;

procedure TMVCJsonDataObjectsSerializer.DeserializeDataSetRecord(
  const ASerializedDataSetRecord: string;
  const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList;
  const ANameCase: TMVCNameCase);
var
  JsonObject: TJsonObject;
begin
  if (ASerializedDataSetRecord = EmptyStr) or (not Assigned(ADataSet)) then
    Exit;

  JsonObject := TJsonObject.Parse(ASerializedDataSetRecord) as TJsonObject;
  try
    ADataSet.Edit;
    JsonObjectToDataSet(JsonObject, ADataSet, AIgnoredFields, ANameCase);
    ADataSet.Post;
  finally
    JsonObject.Free;
  end;
end;

procedure TMVCJsonDataObjectsSerializer.JsonArrayToDataSet(
  const AJsonArray: TJsonArray; const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase);
var
  I: Integer;
begin
  for I := 0 to Pred(AJsonArray.Count) do
  begin
    ADataSet.Append;
    JsonObjectToDataSet(AJsonArray.Items[I].ObjectValue, ADataSet, AIgnoredFields, ANameCase);
    ADataSet.Post;
  end;
end;

procedure TMVCJsonDataObjectsSerializer.JsonArrayToList(
  const AJsonArray: TJsonArray; const AList: IMVCList;
  const AClazz: TClass; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList);
var
  I: Integer;
  Obj: TObject;
begin
  for I := 0 to Pred(AJsonArray.Count) do
  begin
    Obj := TMVCSerializerHelpful.CreateObject(AClazz.QualifiedClassName);
    JsonObjectToObject(AJsonArray.Items[I].ObjectValue, Obj, GetSerializationType(Obj, AType), AIgnoredAttributes);
    AList.Add(Obj);
  end;
end;

procedure TMVCJsonDataObjectsSerializer.JsonDataValueToAttribute(
  const AJsonObject: TJsonObject;
  const AName: string;
  var AValue: TValue;
  const AType: TMVCSerializationType;
  const AIgnored: TMVCIgnoredList;
  const ACustomAttributes: TArray<TCustomAttribute>);
var
  ChildObject: TObject;
  ChildList: IMVCList;
  ChildListOfAtt: MVCListOfAttribute;
  ChildJsonValue: TJsonValue;
begin
  if GetTypeSerializers.ContainsKey(AValue.TypeInfo) then
  begin
    case AJsonObject[AName].Typ of
      jdtNone:
        Exit;
      jdtObject:
        GetTypeSerializers.Items[AValue.TypeInfo].Deserialize(AJsonObject[AName].ObjectValue, AValue, ACustomAttributes);
      jdtArray:
        GetTypeSerializers.Items[AValue.TypeInfo].Deserialize(AJsonObject[AName].ArrayValue, AValue, ACustomAttributes);
    else
      begin
        ChildJsonValue := TJsonValue.Create;
        try
          ChildJsonValue.Value := AJsonObject[AName].Value;
          GetTypeSerializers.Items[AValue.TypeInfo].Deserialize(ChildJsonValue, AValue, ACustomAttributes);
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
            JsonObjectToObject(AJsonObject.O[AName], ChildObject, GetSerializationType(ChildObject, AType), AIgnored);
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

procedure TMVCJsonDataObjectsSerializer.JsonObjectToDataSet(
  const AJsonObject: TJsonObject; const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase);
var
  Field: TField;
  Name: string;
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

      if not AJsonObject.Contains(Name) then
        Continue;

      if (AJsonObject[Name].Typ = jdtObject) and (AJsonObject.Values[Name].ObjectValue = nil) then // Nullable Type
      begin
        Field.Clear;
        Continue;
      end;

      case field.DataType of
        TFieldType.ftBoolean:
          Field.AsBoolean := AJsonObject.B[Name];

        TFieldType.ftInteger, TFieldType.ftSmallint, TFieldType.ftShortint:
          Field.AsInteger := AJsonObject.I[Name];

        TFieldType.ftLargeint:
          Field.AsLargeInt := AJsonObject.L[Name];

        TFieldType.ftCurrency:
          Field.AsCurrency := AJsonObject.F[Name];

        TFieldType.ftSingle:
          Field.AsSingle := AJsonObject.F[Name];

        TFieldType.ftFloat, TFieldType.ftFMTBcd, TFieldType.ftBCD:
          Field.AsFloat := AJsonObject.F[Name];

        ftString, ftWideString, ftMemo, ftWideMemo:
          Field.AsWideString := AJsonObject.S[Name];

        TFieldType.ftDate:
          Field.AsDateTime := ISODateToDate(AJsonObject.S[Name]);

        TFieldType.ftDateTime:
          Field.AsDateTime := ISOTimeStampToDateTime(AJsonObject.S[Name]);

        TFieldType.ftTimeStamp, TFieldType.ftTime:
          Field.AsDateTime := ISOTimeToTime(AJsonObject.S[Name]);

        TFieldType.ftGraphic, TFieldType.ftBlob, TFieldType.ftStream:
          begin
            SS := TStringStream.Create(AJsonObject.S[Name]);
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
                  JsonArrayToDataSet(AJsonObject.A[Name], NestedDataSet, AIgnoredFields, ANameCase);
                end;
              dtObject:
                begin
                  NestedDataSet.Edit;
                  JsonObjectToDataSet(AJsonObject.O[Name], NestedDataSet, AIgnoredFields, ANameCase);
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

procedure TMVCJsonDataObjectsSerializer.JsonObjectToObject(
  const AJsonObject: TJsonObject;
  const AObject: TObject;
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

function TMVCJsonDataObjectsSerializer.SerializeCollection(
  const AList: TObject; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList): string;
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
          ObjectToJsonObject(Obj, JsonArray.AddObject, GetSerializationType(Obj, AType), AIgnoredAttributes);
      Result := JsonArray.ToJSON(False);
    finally
      JsonArray.Free;
    end;
  end;
end;

function TMVCJsonDataObjectsSerializer.SerializeDataSet(
  const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList;
  const ANameCase: TMVCNameCase): string;
var
  JsonArray: TJsonArray;
  BookMark: TBookmark;
begin
  Result := EmptyStr;

  if (not Assigned(ADataSet)) or (ADataSet.IsEmpty) then
    Exit;

  JsonArray := TJsonArray.Create;
  try
    BookMark := ADataSet.Bookmark;
    ADataSet.First;
    while not ADataSet.Eof do
    begin
      DataSetToJsonObject(ADataSet, JsonArray.AddObject, GetNameCase(ADataSet, ANameCase), AIgnoredFields);
      ADataSet.Next;
    end;
    Result := JsonArray.ToJSON(False);
  finally
    JsonArray.Free;
    if ADataSet.BookmarkValid(BookMark) then
      ADataSet.GotoBookmark(BookMark);
    ADataSet.FreeBookmark(BookMark);
  end;
end;

function TMVCJsonDataObjectsSerializer.SerializeDataSetRecord(
  const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList;
  const ANameCase: TMVCNameCase): string;
var
  JsonObject: TJsonObject;
begin
  Result := EmptyStr;

  if (not Assigned(ADataSet)) or (ADataSet.IsEmpty) then
    Exit;

  JsonObject := TJsonObject.Create;
  try
    DataSetToJsonObject(ADataSet, JsonObject, GetNameCase(ADataSet, ANameCase), AIgnoredFields);
    Result := JsonObject.ToJSON(False);
  finally
    JsonObject.Free;
  end;
end;

function TMVCJsonDataObjectsSerializer.SerializeObject(
  const AObject: TObject; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList): string;
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

  ObjType := GetRttiContext.GetType(AObject.ClassType);
  if GetTypeSerializers.ContainsKey(ObjType.Handle) then
  begin
    ChildJsonValue := nil;
    GetTypeSerializers.Items[ObjType.Handle].Serialize(AObject, TObject(ChildJsonValue), []);
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
    ObjectToJsonObject(AObject, JsonObject, GetSerializationType(AObject, AType), AIgnoredAttributes);
    Result := JsonObject.ToJSON(False);
  finally
    JsonObject.Free;
  end;
end;

procedure TMVCJsonDataObjectsSerializer.DeserializeObject(
  const ASerializedObject: string;
  const AObject: TObject;
  const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList);
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
    ObjType := GetRttiContext.GetType(AObject.ClassType);
    if GetTypeSerializers.ContainsKey(ObjType.Handle) then
    begin
      ObjValue := TValue.From<TObject>(AObject);
      GetTypeSerializers.Items[ObjType.Handle].Deserialize(JsonObject, ObjValue, []);
      Exit;
    end;
    JsonObjectToObject(JsonObject, AObject, GetSerializationType(AObject, AType), AIgnoredAttributes);
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
