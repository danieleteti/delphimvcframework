// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2018 Daniele Teti and the DMVCFramework Team
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
  System.JSON,
  JsonDataObjects;

type
  TMVCJsonDataObjectsSerializer = class(TMVCAbstractSerializer, IMVCSerializer)
  public
    procedure ObjectToJsonObject(const AObject: TObject; const AJsonObject: TJsonObject;
      const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);
    procedure ListToJsonArray(const AList: IMVCList; const AJsonArray: TJsonArray; const AType: TMVCSerializationType;
      const AIgnoredAttributes: TMVCIgnoredList);
    procedure AttributeToJsonDataValue(const AJsonObject: TJsonObject; const AName: string; const AValue: TValue;
      const AType: TMVCSerializationType; const AIgnored: TMVCIgnoredList;
      const ACustomAttributes: TArray<TCustomAttribute>);
    procedure JsonObjectToObject(const AJsonObject: TJsonObject; const AObject: TObject;
      const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);
    procedure JsonDataValueToAttribute(const AJsonObject: TJsonObject; const AName: string; var AValue: TValue;
      const AType: TMVCSerializationType; const AIgnored: TMVCIgnoredList;
      const ACustomAttributes: TArray<TCustomAttribute>);
    procedure JsonArrayToList(const AJsonArray: TJsonArray; const AList: IMVCList; const AClazz: TClass;
      const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);
    procedure DataSetToJsonObject(const ADataSet: TDataSet; const AJsonObject: TJsonObject;
      const ANameCase: TMVCNameCase; const AIgnoredFields: TMVCIgnoredList);
    procedure DataSetToJsonArray(const ADataSet: TDataSet; const AJsonArray: TJsonArray; const ANameCase: TMVCNameCase;
      const AIgnoredFields: TMVCIgnoredList);
    procedure JsonObjectToDataSet(const AJsonObject: TJsonObject; const ADataSet: TDataSet;
      const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase);
    procedure JsonArrayToDataSet(const AJsonArray: TJsonArray; const ADataSet: TDataSet;
      const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase);
    { IMVCSerializer }
    function SerializeObject(const AObject: TObject; const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = [];
      const ASerializationAction: TMVCSerializationAction = nil): string;

    function SerializeObjectToJSON(const AObject: TObject; const AType: TMVCSerializationType;
      const AIgnoredAttributes: TMVCIgnoredList; const ASerializationAction: TMVCSerializationAction): TJsonObject;

    function SerializeCollection(const AList: TObject; const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = []): string;

    function SerializeDataSet(const ADataSet: TDataSet; const AIgnoredFields: TMVCIgnoredList = [];
      const ANameCase: TMVCNameCase = ncAsIs): string;

    function SerializeDataSetRecord(const ADataSet: TDataSet; const AIgnoredFields: TMVCIgnoredList = [];
      const ANameCase: TMVCNameCase = ncAsIs): string;

    procedure DeserializeObject(const ASerializedObject: string; const AObject: TObject;
      const AType: TMVCSerializationType = stDefault; const AIgnoredAttributes: TMVCIgnoredList = []);

    procedure DeserializeCollection(const ASerializedList: string; const AList: TObject; const AClazz: TClass;
      const AType: TMVCSerializationType = stDefault; const AIgnoredAttributes: TMVCIgnoredList = []);

    procedure DeserializeDataSet(const ASerializedDataSet: string; const ADataSet: TDataSet;
      const AIgnoredFields: TMVCIgnoredList = []; const ANameCase: TMVCNameCase = ncAsIs);

    procedure DeserializeDataSetRecord(const ASerializedDataSetRecord: string; const ADataSet: TDataSet;
      const AIgnoredFields: TMVCIgnoredList = []; const ANameCase: TMVCNameCase = ncAsIs);
    class function ParseObject(const AString: string): TJsonObject;
    class function ParseArray(const AString: string): TJsonArray;
    class function Parse<T: TJsonBaseObject>(const AString: string): T;
  public
    procedure AfterConstruction; override;
  end;

implementation

uses
  MVCFramework.Serializer.JsonDataObjects.CustomTypes,
  MVCFramework.Commons,
  MVCFramework.Logger,
  System.SysUtils;

{ TMVCJsonDataObjectsSerializer }

procedure TMVCJsonDataObjectsSerializer.AfterConstruction;
begin
  inherited AfterConstruction;
  GetTypeSerializers.Add(TypeInfo(TStream), TMVCStreamSerializerJsonDataObject.Create);
  GetTypeSerializers.Add(TypeInfo(TStringStream), TMVCStreamSerializerJsonDataObject.Create);
  GetTypeSerializers.Add(TypeInfo(TMemoryStream), TMVCStreamSerializerJsonDataObject.Create);
  GetTypeSerializers.Add(TypeInfo(TMVCStringDictionary), TMVCStringDictionarySerializer.Create);
end;

procedure TMVCJsonDataObjectsSerializer.AttributeToJsonDataValue(const AJsonObject: TJsonObject; const AName: string;
  const AValue: TValue; const AType: TMVCSerializationType; const AIgnored: TMVCIgnoredList;
  const ACustomAttributes: TArray<TCustomAttribute>);
var
  ChildJsonObject: TJsonObject;
  ChildJsonArray: TJsonArray;
  ChildValue: TValue;
  ChildObject, Obj: TObject;
  ChildList: IMVCList;
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
    GetTypeSerializers.Items[AValue.TypeInfo].SerializeAttribute(AValue, AName, AJsonObject, ACustomAttributes);
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
          AJsonObject.S[AName] := GetEnumName(AValue.TypeInfo, AValue.AsOrdinal);
      end;

    tkClass:
      begin
        ChildObject := AValue.AsObject;
        if Assigned(ChildObject) then
        begin
          if ChildObject is TDataSet then
          begin
            ChildJsonArray := AJsonObject.A[AName];
            DataSetToJsonArray(TDataSet(ChildObject), ChildJsonArray, TMVCNameCase.ncLowerCase, []);
          end
          else
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
          end;
        end
        else
        begin
          if TMVCSerializerHelper.AttributeExists<MVCSerializeAsStringAttribute>(ACustomAttributes) then
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
          if TMVCSerializerHelper.AttributeExists<MVCValueAsTypeAttribute>(ACustomAttributes, ValueTypeAtt) then
          begin
            CastValue := AValue.AsType<TValue>;
            if CastValue.TryCast(ValueTypeAtt.ValueTypeInfo, CastedValue) then
              AttributeToJsonDataValue(AJsonObject, AName, CastedValue, stDefault, [], [])
            else
              raise EMVCSerializationException.CreateFmt
                ('Cannot serialize %s of TypeKind tkRecord (TValue with MVCValueAsTypeAttribute).', [AName]);
          end
          else
          begin
            ChildValue := AValue.AsType<TValue>;
            ChildJsonObject := AJsonObject.O[AName];
            ChildJsonObject.S['type'] := TMVCSerializerHelper.GetTypeKindAsString(ChildValue.TypeInfo.Kind);
            AttributeToJsonDataValue(ChildJsonObject, 'value', ChildValue, stDefault, [], []);
          end;
        end
        else
          raise EMVCSerializationException.CreateFmt('Cannot serialize %s of TypeKind tkRecord.', [AName]);
      end;

    tkSet:
      raise EMVCSerializationException.CreateFmt('Cannot serialize %s of TypeKind tkSet.', [AName]);

    tkArray, tkDynArray:
      begin
        raise EMVCSerializationException.CreateFmt('Cannot serialize %s of TypeKind tkArray or tkDynArray.', [AName]);
      end;

    tkUnknown:
      raise EMVCSerializationException.CreateFmt('Cannot serialize %s of TypeKind tkUnknown.', [AName]);
  end;
end;

procedure TMVCJsonDataObjectsSerializer.DataSetToJsonArray(const ADataSet: TDataSet; const AJsonArray: TJsonArray;
  const ANameCase: TMVCNameCase; const AIgnoredFields: TMVCIgnoredList);
var
  LJObj: TJsonObject;
begin
  while not ADataSet.Eof do
  begin
    LJObj := AJsonArray.AddObject;
    DataSetToJsonObject(ADataSet, LJObj, ANameCase, AIgnoredFields);
    ADataSet.Next;
  end;
end;

procedure TMVCJsonDataObjectsSerializer.DataSetToJsonObject(const ADataSet: TDataSet; const AJsonObject: TJsonObject;
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
    case ANameCase of
      ncUpperCase:
        FieldName := UpperCase(ADataSet.Fields[I].FieldName);
      ncLowerCase:
        FieldName := LowerCase(ADataSet.Fields[I].FieldName);
    end;
    if (not IsIgnoredAttribute(AIgnoredFields, FieldName)) and
      (not IsIgnoredComponent(ADataSet.Owner, ADataSet.Fields[I].Name)) then
    begin
      if ADataSet.Fields[I].IsNull then
        AJsonObject[FieldName] := Null
      else
      begin
        case ADataSet.Fields[I].DataType of
          ftBoolean:
            AJsonObject.B[FieldName] := ADataSet.Fields[I].AsBoolean;

          ftInteger, ftSmallint, ftShortint, ftByte:
            AJsonObject.I[FieldName] := ADataSet.Fields[I].AsInteger;

          ftLargeint, ftAutoInc:
            AJsonObject.L[FieldName] := ADataSet.Fields[I].AsLargeInt;
{$IFDEF TOKYOORBETTER}
          ftGuid:
            AJsonObject.S[FieldName] := GUIDToString(ADataSet.Fields[I].AsGuid);
{$ENDIF}
          ftSingle, ftFloat:
            AJsonObject.F[FieldName] := ADataSet.Fields[I].AsFloat;

          ftString, ftMemo:
            AJsonObject.S[FieldName] := ADataSet.Fields[I].AsString;

          ftWideString, ftWideMemo:
            AJsonObject.S[FieldName] := ADataSet.Fields[I].AsWideString;

          ftDate:
            AJsonObject.S[FieldName] := DateToISODate(ADataSet.Fields[I].AsDateTime);

          ftDateTime:
            AJsonObject.S[FieldName] := DateTimeToISOTimeStamp(ADataSet.Fields[I].AsDateTime);

          ftTime:
            AJsonObject.S[FieldName] := SQLTimeStampToStr('hh:nn:ss', ADataSet.Fields[I].AsSQLTimeStamp);

          ftTimeStamp:
            AJsonObject.S[FieldName] :=
              DateTimeToISOTimeStamp(SQLTimeStampToDateTime(ADataSet.Fields[I].AsSQLTimeStamp));

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
                  TMVCSerializerHelper.EncodeStream(MS, SS);
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
                      DataSetToJsonObject(NestedDataSet, ChildJsonArray.AddObject,
                        GetNameCase(NestedDataSet, ANameCase), AIgnoredFields);
                      NestedDataSet.Next;
                    end;
                  end;
                dtObject:
                  begin
                    ChildJsonObject := AJsonObject.O[FieldName];
                    DataSetToJsonObject(NestedDataSet, ChildJsonObject, GetNameCase(NestedDataSet, ANameCase),
                      AIgnoredFields);
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

procedure TMVCJsonDataObjectsSerializer.DeserializeCollection(const ASerializedList: string; const AList: TObject;
  const AClazz: TClass; const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);
var
  JsonArray: TJsonArray;
  ObjList: IMVCList;
begin
  if (ASerializedList = EmptyStr) then
    raise EMVCException.Create(http_status.BadRequest, 'Invalid body');

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

procedure TMVCJsonDataObjectsSerializer.DeserializeDataSet(const ASerializedDataSet: string; const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase);
var
  lJsonArray: TJsonArray;
begin
  if (ASerializedDataSet = EmptyStr) then
    raise EMVCException.Create(http_status.BadRequest, 'Invalid body');

  if not Assigned(ADataSet) then
    Exit;

  try
    lJsonArray := TJsonArray.Parse(ASerializedDataSet) as TJsonArray;
  except
    on E: EJsonParserException do
    begin
      raise EMVCException.Create(http_status.BadRequest, 'Invalid body');
    end;
  end;
  try
    JsonArrayToDataSet(lJsonArray, ADataSet, AIgnoredFields, ANameCase);
  finally
    lJsonArray.Free;
  end;
end;

procedure TMVCJsonDataObjectsSerializer.DeserializeDataSetRecord(const ASerializedDataSetRecord: string;
  const ADataSet: TDataSet; const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase);
var
  lJsonObject: TJsonObject;
begin
  if (ASerializedDataSetRecord = EmptyStr) or (not Assigned(ADataSet)) then
    Exit;

  try
    lJsonObject := TJsonObject.Parse(ASerializedDataSetRecord) as TJsonObject;
  except
    on E: EJsonParserException do
    begin
      raise EMVCException.Create(http_status.BadRequest, 'Invalid body');
    end;
  end;
  try
    ADataSet.Edit;
    JsonObjectToDataSet(lJsonObject, ADataSet, AIgnoredFields, ANameCase);
    ADataSet.Post;
  finally
    lJsonObject.Free;
  end;
end;

procedure TMVCJsonDataObjectsSerializer.JsonArrayToDataSet(const AJsonArray: TJsonArray; const ADataSet: TDataSet;
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

procedure TMVCJsonDataObjectsSerializer.JsonArrayToList(const AJsonArray: TJsonArray; const AList: IMVCList;
  const AClazz: TClass; const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);
var
  I: Integer;
  Obj: TObject;
begin
  for I := 0 to Pred(AJsonArray.Count) do
  begin
    Obj := TMVCSerializerHelper.CreateObject(AClazz.QualifiedClassName);
    JsonObjectToObject(AJsonArray.Items[I].ObjectValue, Obj, GetSerializationType(Obj, AType), AIgnoredAttributes);
    AList.Add(Obj);
  end;
end;

procedure TMVCJsonDataObjectsSerializer.JsonDataValueToAttribute(const AJsonObject: TJsonObject; const AName: string;
  var AValue: TValue; const AType: TMVCSerializationType; const AIgnored: TMVCIgnoredList;
  const ACustomAttributes: TArray<TCustomAttribute>);
var
  ChildObject: TObject;
  ChildList: IMVCList;
  ChildListOfAtt: MVCListOfAttribute;
begin
  if GetTypeSerializers.ContainsKey(AValue.TypeInfo) then
  begin
    case AJsonObject[AName].Typ of
      jdtNone:
        Exit;
      jdtObject:
        GetTypeSerializers.Items[AValue.TypeInfo].DeserializeAttribute(AValue, AName, AJsonObject[AName].ObjectValue,
          ACustomAttributes);
      jdtArray:
        GetTypeSerializers.Items[AValue.TypeInfo].DeserializeAttribute(AValue, AName, AJsonObject[AName].ArrayValue,
          ACustomAttributes);
    else
      GetTypeSerializers.Items[AValue.TypeInfo].DeserializeAttribute(AValue, AName, AJsonObject, ACustomAttributes);
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

        else if (AValue.Kind = tkEnumeration) then
          TValue.Make(GetEnumValue(AValue.TypeInfo, AJsonObject[AName].Value), AValue.TypeInfo, AValue)

        else
          AValue := TValue.From<string>(AJsonObject[AName].Value);
      end;

    jdtInt:
      begin
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
          // dt: if a key is null, jsondataobjects assign it the type jdtObject
          if AJsonObject[AName].ObjectValue <> nil then
          begin
            ChildObject := AValue.AsObject;
            JsonObjectToObject(AJsonObject.O[AName], ChildObject, GetSerializationType(ChildObject, AType), AIgnored);
          end;
        end;
      end;

    jdtArray:
      begin
        ChildObject := AValue.AsObject;
        if Assigned(ChildObject) then
        begin
          ChildList := TDuckTypedList.Wrap(ChildObject);
          if TMVCSerializerHelper.AttributeExists<MVCListOfAttribute>(ACustomAttributes, ChildListOfAtt) then
            JsonArrayToList(AJsonObject.A[AName], ChildList, ChildListOfAtt.Value, AType, AIgnored)
          else
            raise EMVCDeserializationException.CreateFmt
              ('You can not deserialize a list %s without the attribute MVCListClassTypeAttribute.', [AName]);
        end;
      end;
  end;
end;

procedure TMVCJsonDataObjectsSerializer.JsonObjectToDataSet(const AJsonObject: TJsonObject; const ADataSet: TDataSet;
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
      name := GetNameAs(ADataSet.Owner, Field.Name, Field.FieldName);

      if (IsIgnoredAttribute(AIgnoredFields, name)) or (IsIgnoredComponent(ADataSet.Owner, Field.Name)) then
        Continue;

      case GetNameCase(ADataSet, ANameCase) of
        ncLowerCase:
          name := LowerCase(Field.FieldName);
        ncUpperCase:
          name := UpperCase(Field.FieldName);
      end;

      if not AJsonObject.Contains(name) then
        Continue;

      if (AJsonObject[name].Typ = jdtObject) and (AJsonObject.Values[name].ObjectValue = nil) then // Nullable Type
      begin
        Field.Clear;
        Continue;
      end;

      case Field.DataType of
        TFieldType.ftBoolean:
          Field.AsBoolean := AJsonObject.B[name];

        TFieldType.ftInteger, TFieldType.ftSmallint, TFieldType.ftShortint, TFieldType.ftByte:
          Field.AsInteger := AJsonObject.I[name];

        TFieldType.ftLargeint:
          Field.AsLargeInt := AJsonObject.L[name];

        TFieldType.ftCurrency:
          Field.AsCurrency := AJsonObject.F[name];

        TFieldType.ftSingle:
          Field.AsSingle := AJsonObject.F[name];

        TFieldType.ftFloat, TFieldType.ftFMTBcd, TFieldType.ftBCD:
          Field.AsFloat := AJsonObject.F[name];

        ftString, ftWideString, ftMemo, ftWideMemo:
          Field.AsWideString := AJsonObject.S[name];

        TFieldType.ftDate:
          Field.AsDateTime := ISODateToDate(AJsonObject.S[name]);

        TFieldType.ftDateTime:
          Field.AsDateTime := ISOTimeStampToDateTime(AJsonObject.S[name]);

        TFieldType.ftTimeStamp, TFieldType.ftTime:
          Field.AsDateTime := ISOTimeToTime(AJsonObject.S[name]);

{$IFDEF TOKYOORBETTER}
        TFieldType.ftGuid:
          Field.AsGuid := StringToGUID(AJsonObject.S[name]);
{$ENDIF}
        TFieldType.ftGraphic, TFieldType.ftBlob, TFieldType.ftStream:
          begin
            SS := TStringStream.Create(AJsonObject.S[name]);
            try
              SS.Position := 0;
              SM := TMemoryStream.Create;
              try
                TMVCSerializerHelper.DecodeStream(SS, SM);
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
                  JsonArrayToDataSet(AJsonObject.A[name], NestedDataSet, AIgnoredFields, ANameCase);
                end;
              dtObject:
                begin
                  NestedDataSet.Edit;
                  JsonObjectToDataSet(AJsonObject.O[name], NestedDataSet, AIgnoredFields, ANameCase);
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

procedure TMVCJsonDataObjectsSerializer.JsonObjectToObject(const AJsonObject: TJsonObject; const AObject: TObject;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);
var
  ObjType: TRttiType;
  Prop: TRttiProperty;
  Fld: TRttiField;
  AttributeValue: TValue;
  lKeyName: string;
begin
  ObjType := GetRttiContext.GetType(AObject.ClassType);
  case AType of
    stDefault, stProperties:
      begin
        try
          for Prop in ObjType.GetProperties do
          begin

{$IFDEF AUTOREFCOUNT}
            if TMVCSerializerHelper.IsAPropertyToSkip(Prop.Name) then
              Continue;

{$ENDIF}
            if (Prop.IsWritable or Prop.GetValue(AObject).IsObject) and
              (not TMVCSerializerHelper.HasAttribute<MVCDoNotSerializeAttribute>(Prop)) and
              (not IsIgnoredAttribute(AIgnoredAttributes, Prop.Name)) then
            begin
              AttributeValue := Prop.GetValue(AObject);
              lKeyName := TMVCSerializerHelper.GetKeyName(Prop, ObjType);
              JsonDataValueToAttribute(AJsonObject, lKeyName, AttributeValue, AType, AIgnoredAttributes,
                Prop.GetAttributes);
              if (not AttributeValue.IsEmpty) and Prop.IsWritable then
                Prop.SetValue(AObject, AttributeValue);
            end;
          end;
        except
          on E: EInvalidCast do
          begin
            raise EMVCException.CreateFmt('Invalid class typecast for property "%s"', [lKeyName]);
          end;
        end;
      end;
    stFields:
      begin
        try
          for Fld in ObjType.GetFields do
            if (not TMVCSerializerHelper.HasAttribute<MVCDoNotSerializeAttribute>(Fld)) and
              (not IsIgnoredAttribute(AIgnoredAttributes, Fld.Name)) then
            begin
              AttributeValue := Fld.GetValue(AObject);
              lKeyName := TMVCSerializerHelper.GetKeyName(Fld, ObjType);
              JsonDataValueToAttribute(AJsonObject, lKeyName, AttributeValue, AType, AIgnoredAttributes,
                Fld.GetAttributes);
              if not AttributeValue.IsEmpty then
                Fld.SetValue(AObject, AttributeValue);
            end;
        except
          on E: EInvalidCast do
          begin
            raise EMVCException.CreateFmt('Invalid class typecast for field "%s"', [lKeyName]);
          end;
        end;
      end;
  end;
end;

procedure TMVCJsonDataObjectsSerializer.ListToJsonArray(const AList: IMVCList; const AJsonArray: TJsonArray;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);
var
  I: Integer;
begin
  if not Assigned(AList) then
    raise EMVCSerializationException.Create('List not assigned');
  for I := 0 to Pred(AList.Count) do
  begin
    ObjectToJsonObject(AList.GetItem(I), AJsonArray.AddObject, AType, AIgnoredAttributes);
  end;
end;

procedure TMVCJsonDataObjectsSerializer.ObjectToJsonObject(const AObject: TObject; const AJsonObject: TJsonObject;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);
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
        begin

{$IFDEF AUTOREFCOUNT}
          if TMVCSerializerHelper.IsAPropertyToSkip(Prop.Name) then
            Continue;

{$ENDIF}
          if (not TMVCSerializerHelper.HasAttribute<MVCDoNotSerializeAttribute>(Prop)) and
            (not IsIgnoredAttribute(AIgnoredAttributes, Prop.Name)) then
            AttributeToJsonDataValue(AJsonObject, TMVCSerializerHelper.GetKeyName(Prop, ObjType),
              Prop.GetValue(AObject), AType, AIgnoredAttributes, Prop.GetAttributes);
        end;
      end;
    stFields:
      begin
        for Fld in ObjType.GetFields do
          if (not TMVCSerializerHelper.HasAttribute<MVCDoNotSerializeAttribute>(Fld)) and
            (not IsIgnoredAttribute(AIgnoredAttributes, Fld.Name)) then
            AttributeToJsonDataValue(AJsonObject, TMVCSerializerHelper.GetKeyName(Fld, ObjType), Fld.GetValue(AObject),
              AType, AIgnoredAttributes, Fld.GetAttributes);
      end;
  end;
end;

class function TMVCJsonDataObjectsSerializer.Parse<T>(const AString: string): T;
begin
  Result := TJsonObject.Parse(AString) as T;
  if not Assigned(Result) then
    raise EMVCDeserializationException.Create('Cannot parse string as ' + T.ClassName);
end;

class function TMVCJsonDataObjectsSerializer.ParseArray(const AString: string): TJsonArray;
begin
  Result := Parse<TJsonArray>(AString);
end;

class function TMVCJsonDataObjectsSerializer.ParseObject(const AString: string): TJsonObject;
begin
  Result := Parse<TJsonObject>(AString);
end;

function TMVCJsonDataObjectsSerializer.SerializeCollection(const AList: TObject; const AType: TMVCSerializationType;
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
    Exit(TJsonBaseObject(AList).ToJSON(True));

  ObjList := TDuckTypedList.Wrap(AList);
  if Assigned(ObjList) then
  begin
    JsonArray := TJsonArray.Create;
    try
      for Obj in ObjList do
      begin
        if Assigned(Obj) then
        begin
          ObjectToJsonObject(Obj, JsonArray.AddObject, GetSerializationType(Obj, AType), AIgnoredAttributes);
        end;
      end;
      Result := JsonArray.ToJSON(True);
    finally
      JsonArray.Free;
    end;
  end;
end;

function TMVCJsonDataObjectsSerializer.SerializeDataSet(const ADataSet: TDataSet; const AIgnoredFields: TMVCIgnoredList;
  const ANameCase: TMVCNameCase): string;
var
  JsonArray: TJsonArray;
  BookMark: TBookmark;
  lNameCase: TMVCNameCase;
begin
  Result := EmptyStr;

  if (not Assigned(ADataSet)) or (ADataSet.IsEmpty) then
    Exit;

  JsonArray := TJsonArray.Create;
  try
    BookMark := ADataSet.BookMark;
    lNameCase := GetNameCase(ADataSet, ANameCase);
    ADataSet.First;
    while not ADataSet.Eof do
    begin
      DataSetToJsonObject(ADataSet, JsonArray.AddObject, lNameCase, AIgnoredFields);
      ADataSet.Next;
    end;
    Result := JsonArray.ToJSON(True);
  finally
    JsonArray.Free;
    if ADataSet.BookmarkValid(BookMark) then
      ADataSet.GotoBookmark(BookMark);
    ADataSet.FreeBookmark(BookMark);
  end;
end;

function TMVCJsonDataObjectsSerializer.SerializeDataSetRecord(const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase): string;
var
  JsonObject: TJsonObject;
begin
  Result := EmptyStr;

  if (not Assigned(ADataSet)) or (ADataSet.IsEmpty) then
    Exit;

  JsonObject := TJsonObject.Create;
  try
    DataSetToJsonObject(ADataSet, JsonObject, GetNameCase(ADataSet, ANameCase), AIgnoredFields);
    Result := JsonObject.ToJSON(True);
  finally
    JsonObject.Free;
  end;
end;

function TMVCJsonDataObjectsSerializer.SerializeObject(const AObject: TObject; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList; const ASerializationAction: TMVCSerializationAction): string;
var
  JsonObject: TJsonObject;
  ObjType: TRttiType;
begin
  Result := EmptyStr;

  if not Assigned(AObject) then
    Exit('null');

  if AObject is TJsonBaseObject then
    Exit(TJsonBaseObject(AObject).ToJSON(True));

  if AObject is TDataSet then
    Exit(self.SerializeDataSet(TDataSet(AObject)));

  if AObject is TJsonValue then
    Exit(TJsonValue(AObject).ToJSON);

  ObjType := GetRttiContext.GetType(AObject.ClassType);

  if GetTypeSerializers.ContainsKey(ObjType.Handle) then
  begin
    GetTypeSerializers.Items[ObjType.Handle].SerializeRoot(AObject, TObject(JsonObject), []);
    try
      Result := JsonObject.ToJSON(True);
    finally
      JsonObject.Free;
    end;
    Exit;
  end;

  JsonObject := TJsonObject.Create;
  try
    ObjectToJsonObject(AObject, JsonObject, GetSerializationType(AObject, AType), AIgnoredAttributes);
    Result := JsonObject.ToJSON(True);
  finally
    JsonObject.Free;
  end;
end;

function TMVCJsonDataObjectsSerializer.SerializeObjectToJSON(const AObject: TObject; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList; const ASerializationAction: TMVCSerializationAction): TJsonObject;
var
  JsonObject: TJsonObject;
  ObjType: TRttiType;
begin
  if not Assigned(AObject) then
    Exit(TJsonObject.Create);

  if AObject is TJsonBaseObject then
    Exit(TJsonBaseObject(AObject).Clone as TJsonObject);

  if AObject is TDataSet then
  begin
    raise Exception.Create('Not supported yet');
    //Exit(self.SerializeDataSet(TDataSet(AObject)));
  end;

  if AObject is TJsonValue then
    Exit(TJSONObject.Parse(TJsonValue(AObject).ToJSON) as TJsonObject);

  ObjType := GetRttiContext.GetType(AObject.ClassType);

  if GetTypeSerializers.ContainsKey(ObjType.Handle) then
  begin
    GetTypeSerializers.Items[ObjType.Handle].SerializeRoot(AObject, TObject(JsonObject), []);
    try
      Result := JsonObject;
    except
      JsonObject.Free;
      raise;
    end;
    Exit;
  end;

  Result := TJsonObject.Create;
  try
    ObjectToJsonObject(AObject, Result, GetSerializationType(AObject, AType), AIgnoredAttributes);
  except
    Result.Free;
    raise;
  end;
end;

procedure TMVCJsonDataObjectsSerializer.DeserializeObject(const ASerializedObject: string; const AObject: TObject;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);
var
  JsonObject: TJsonObject;
begin
  if (ASerializedObject = EmptyStr) then
    raise EMVCException.Create(http_status.BadRequest, 'Invalid body');

  if not Assigned(AObject) then
    Exit;

  try
    JsonObject := TJsonObject.Parse(ASerializedObject) as TJsonObject;
  except
    on E: EJsonParserException do
    begin
      raise EMVCException.Create(http_status.BadRequest, E.Message);
    end;
  end;
  try
    if GetTypeSerializers.ContainsKey(AObject.ClassInfo) then
    begin
      GetTypeSerializers.Items[AObject.ClassInfo].DeserializeRoot(JsonObject, AObject, []);
    end
    else
    begin
      JsonObjectToObject(JsonObject, AObject, GetSerializationType(AObject, AType), AIgnoredAttributes);
    end;
  finally
    JsonObject.Free;
  end;
end;

end.
