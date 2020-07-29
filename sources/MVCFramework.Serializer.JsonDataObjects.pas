// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2020 Daniele Teti and the DMVCFramework Team
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
{$WARN SYMBOL_DEPRECATED OFF}

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
  MVCFramework.Commons,
  MVCFramework.Serializer.Intf,
  MVCFramework.Serializer.Abstract,
  MVCFramework.DuckTyping,
  MVCFramework.Serializer.Commons,
  System.JSON,
  JsonDataObjects,
  System.SysUtils;

type

  TMVCDataSetField = record
    FieldName: string;
    DataType: TFieldType;
    I: Integer;
  end;

  TMVCDataSetFields = TList<TMVCDataSetField>;

  TJSONObjectHelper = class helper for TJsonObject
  public
    procedure LoadFromString(const Value: string; Encoding: TEncoding = nil; Utf8WithoutBOM: Boolean = True);
  end;

  TMVCJsonDataObjectsSerializer = class(TMVCAbstractSerializer, IMVCSerializer)
  private
    fStringDictionarySerializer: IMVCTypeSerializer;
    function TryMapNullableFloat(var Value: TValue;
      const JSONDataObject: TJsonObject; const AttribName: string): Boolean;
  public
    function GetDataSetFields(const ADataSet: TDataSet; const AIgnoredFields: TMVCIgnoredList;
      const ANameCase: TMVCNameCase = ncAsIs): TMVCDataSetFields;
    procedure ObjectToJsonObject(const AObject: TObject; const AJsonObject: TJDOJsonObject;
      const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);
    procedure InternalObjectToJsonObject(const AObject: TObject; const AJsonObject: TJDOJsonObject;
      const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList;
      const ASerializationAction: TMVCSerializationAction; const Links: IMVCLinks;
      const Serializer: IMVCTypeSerializer);
    procedure ListToJsonArray(const AList: IMVCList; const AJsonArray: TJDOJsonArray;
      const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList;
      const ASerializationAction: TMVCSerializationAction = nil);
    procedure AttributeToJsonDataValue(const AJsonObject: TJDOJsonObject; const AName: string; const AValue: TValue;
      const AType: TMVCSerializationType; const AIgnored: TMVCIgnoredList;
      const ACustomAttributes: TArray<TCustomAttribute>);
    function TryNullableToJSON(const AValue: TValue; const AJsonObject: TJDOJsonObject; const AName: string): Boolean;
    procedure JsonObjectToObject(const AJsonObject: TJDOJsonObject; const AObject: TObject;
      const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);
    procedure JsonDataValueToAttribute(const AJsonObject: TJDOJsonObject; const AName: string; var AValue: TValue;
      const AType: TMVCSerializationType; const AIgnored: TMVCIgnoredList;
      const ACustomAttributes: TArray<TCustomAttribute>);
    procedure JsonArrayToList(const AJsonArray: TJDOJsonArray; const AList: IMVCList; const AClazz: TClass;
      const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);
    procedure DataSetToJsonObject(const ADataSet: TDataSet; const AJsonObject: TJDOJsonObject;
      const ANameCase: TMVCNameCase; const AIgnoredFields: TMVCIgnoredList; const ADataSetFields: TMVCDataSetFields;
      const ASerializationCallback: TMVCDataSetFieldSerializationAction = nil);
    procedure DataSetRowToJsonArrayOfValues(const ADataSet: TDataSet; const AJsonArray: TJDOJsonArray;
      const AIgnoredFields: TMVCIgnoredList; const ADataSetFields: TMVCDataSetFields);
    procedure DataSetToJsonArray(const ADataSet: TDataSet; const AJsonArray: TJDOJsonArray;
      const ANameCase: TMVCNameCase; const AIgnoredFields: TMVCIgnoredList;
      const ASerializationCallback: TMVCDataSetFieldSerializationAction = nil);
    procedure DataSetToJsonArrayOfValues(const ADataSet: TDataSet; const AJsonArray: TJDOJsonArray;
      const AIgnoredFields: TMVCIgnoredList);
    procedure JsonObjectToDataSet(const AJsonObject: TJDOJsonObject; const ADataSet: TDataSet;
      const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase);
    procedure JsonArrayToDataSet(const AJsonArray: TJDOJsonArray; const ADataSet: TDataSet;
      const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase);
    function JsonArrayToArray(const AJsonArray: TJDOJsonArray): TValue;
    { IMVCSerializer }
    function SerializeObject(const AObject: TObject; const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = []; const ASerializationAction: TMVCSerializationAction = nil)
      : string; overload;

    function SerializeObject(const AObject: IInterface; const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = []; const ASerializationAction: TMVCSerializationAction = nil)
      : string; overload;

    function SerializeObjectToJSON(const AObject: TObject; const AType: TMVCSerializationType;
      const AIgnoredAttributes: TMVCIgnoredList; const ASerializationAction: TMVCSerializationAction): TJDOJsonObject;

    function SerializeCollection(const AList: TObject; const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = []; const ASerializationAction: TMVCSerializationAction = nil)
      : string; overload;

    function SerializeCollection(const AList: IInterface; const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = []; const ASerializationAction: TMVCSerializationAction = nil)
      : string; overload;

    function SerializeDataSet(const ADataSet: TDataSet; const AIgnoredFields: TMVCIgnoredList;
      const ANameCase: TMVCNameCase = ncAsIs; const ASerializationAction: TMVCDatasetSerializationAction = nil): string;

    function SerializeDataSetRecord(const DataSet: TDataSet;
      const IgnoredFields: TMVCIgnoredList; const NameCase: TMVCNameCase = ncAsIs;
      const SerializationAction: TMVCDatasetSerializationAction = nil): string;

    procedure DeserializeObject(const ASerializedObject: string; const AObject: TObject;
      const AType: TMVCSerializationType = stDefault; const AIgnoredAttributes: TMVCIgnoredList = []); overload;

    procedure DeserializeObject(const ASerializedObject: string; const AObject: IInterface;
      const AType: TMVCSerializationType = stDefault; const AIgnoredAttributes: TMVCIgnoredList = []); overload;

    procedure DeserializeCollection(const ASerializedList: string; const AList: TObject; const AClazz: TClass;
      const AType: TMVCSerializationType = stDefault; const AIgnoredAttributes: TMVCIgnoredList = []); overload;

    procedure DeserializeCollection(const ASerializedList: string; const AList: IInterface; const AClazz: TClass;
      const AType: TMVCSerializationType = stDefault; const AIgnoredAttributes: TMVCIgnoredList = []); overload;

    procedure DeserializeDataSet(const ASerializedDataSet: string; const ADataSet: TDataSet;
      const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase = ncAsIs);

    procedure InternalSerializeDataSet(
      const ADataSet: TDataSet; const AJsonArray: TJsonArray; const AIgnoredFields: TMVCIgnoredList;
      const ANameCase: TMVCNameCase;
      const ASerializationAction: TMVCDatasetSerializationAction);

    procedure InternalSerializeDataSetRecord(
      const DataSet: TDataSet; const JSONObject: TJsonObject;
      const IgnoredFields: TMVCIgnoredList; const NameCase: TMVCNameCase;
      const SerializationAction: TMVCDatasetSerializationAction);

    procedure DeserializeDataSetRecord(const ASerializedDataSetRecord: string; const ADataSet: TDataSet;
      const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase = ncAsIs);
    class function ParseObject(const AString: string): TJDOJsonObject;
    class function ParseArray(const AString: string): TJDOJsonArray;
    class function Parse<T: TJsonBaseObject>(const AString: string): T;
  public
    procedure AfterConstruction; override;
  end;

  TJDOLinks = class(TMVCLinks)
  public
    procedure FillJSONArray(const AJsonArray: TJsonArray);
  end;

procedure TValueToJsonElement(const Value: TValue; const JSON: TJDOJsonObject; const KeyName: string);
function StrToJSONObject(const AValue: string): TJDOJsonObject;
function StrToJSONArray(const AValue: string): TJDOJsonArray;
procedure JsonObjectToObject(const AJsonObject: TJDOJsonObject; const AObject: TObject;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);

const
  JDO_TYPE_DESC: array [
    TJsonDataType.jdtNone .. TJsonDataType.jdtObject
    ] of string = (
    'None', 'String', 'Int', 'Long', 'ULong', 'Float', 'DateTime', 'UtcDateTime', 'Bool', 'Array', 'Object'
    );

implementation

uses
  MVCFramework.Serializer.JsonDataObjects.CustomTypes,
  MVCFramework.Logger,
  MVCFramework.DataSet.Utils,
  MVCFramework.Nullables;

{ TMVCJsonDataObjectsSerializer }

procedure TMVCJsonDataObjectsSerializer.AfterConstruction;
var
  lStreamSerializer: IMVCTypeSerializer;
  lDataSetHolderSerializer: TMVCDataSetHolderSerializer;
  fObjectDictionarySerializer: TMVCObjectDictionarySerializer;
begin
  inherited AfterConstruction;
  lDataSetHolderSerializer := TMVCDataSetHolderSerializer.Create;
  GetTypeSerializers.Add(TypeInfo(TDataSetHolder), lDataSetHolderSerializer);
  lStreamSerializer := TMVCStreamSerializerJsonDataObject.Create;
  GetTypeSerializers.Add(TypeInfo(TStream), lStreamSerializer);
  GetTypeSerializers.Add(TypeInfo(TStringStream), lStreamSerializer);
  GetTypeSerializers.Add(TypeInfo(TFileStream), lStreamSerializer);
  GetTypeSerializers.Add(TypeInfo(TMemoryStream), lStreamSerializer);
  fStringDictionarySerializer := TMVCStringDictionarySerializer.Create;
  GetTypeSerializers.Add(TypeInfo(TMVCStringDictionary), fStringDictionarySerializer);
  GetTypeSerializers.Add(TypeInfo(TGUID), TMVCGUIDSerializer.Create);
  fObjectDictionarySerializer := TMVCObjectDictionarySerializer.Create(self);
  GetTypeSerializers.Add(TypeInfo(TMVCObjectDictionary), fObjectDictionarySerializer);
end;

procedure TMVCJsonDataObjectsSerializer.AttributeToJsonDataValue(const AJsonObject: TJDOJsonObject; const AName: string;
  const AValue: TValue; const AType: TMVCSerializationType; const AIgnored: TMVCIgnoredList;
  const ACustomAttributes: TArray<TCustomAttribute>);
var
  ChildJsonObject: TJDOJsonObject;
  ChildJsonArray: TJDOJsonArray;
  ChildValue: TValue;
  ChildObject, Obj: TObject;
  ChildList: IMVCList;
  ValueTypeAtt: MVCValueAsTypeAttribute;
  CastValue, CastedValue: TValue;
  I: Integer;
  LEnumAsAttr: MVCEnumSerializationAttribute;
  LEnumSerType: TMVCEnumSerializationType;
  LEnumMappedValues: TList<string>;
  LEnumName: string;
begin
  if SameText(AName, 'RefCount') then
  begin
    Exit;
  end;

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
        begin
          LEnumSerType := estEnumName;
          LEnumMappedValues := nil;
          if TMVCSerializerHelper.AttributeExists<MVCEnumSerializationAttribute>(ACustomAttributes, LEnumAsAttr)
          then
          begin
            LEnumSerType := LEnumAsAttr.SerializationType;
            LEnumMappedValues := LEnumAsAttr.MappedValues;
          end;

          case LEnumSerType of
            estEnumName:
              begin
                LEnumName := GetEnumName(AValue.TypeInfo, AValue.AsOrdinal);

                AJsonObject.S[AName] := LEnumName;
              end;
            estEnumOrd:
              begin
                AJsonObject.I[AName] := AValue.AsOrdinal;
              end;
            estEnumMappedValues:
              begin
                if (LEnumMappedValues.Count - 1) < AValue.AsOrdinal then
                  raise EMVCException.Create('Enumerator value is not mapped in MappedValues');

                AJsonObject.S[AName] := LEnumMappedValues[AValue.AsOrdinal];
              end;
          end;
        end;
      end;

    tkClass, tkInterface:
      begin
        ChildObject := nil;
        if not AValue.IsEmpty and (AValue.Kind = tkInterface) then
          ChildObject := TObject(AValue.AsInterface)
        else if AValue.Kind = tkClass then
          ChildObject := AValue.AsObject;

        if Assigned(ChildObject) then
        begin
          if ChildObject is TDataSet then
          begin
            ChildJsonArray := AJsonObject.A[AName];
            DataSetToJsonArray(TDataSet(ChildObject), ChildJsonArray, TMVCNameCase.ncLowerCase, []);
          end
          else if ChildObject is TJsonObject then
          begin
            AJsonObject.O[AName] := TJsonObject(ChildObject).Clone as TJsonObject;
          end
          else
          begin
            ChildList := TDuckTypedList.Wrap(ChildObject);
            if Assigned(ChildList) then
            begin
              ChildJsonArray := AJsonObject.A[AName];
              for Obj in ChildList do
              begin
                if Assigned(Obj) then
                begin
                  ObjectToJsonObject(Obj, ChildJsonArray.AddObject, GetSerializationType(Obj, AType), AIgnored);
                end;
              end;
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
        if AValue.TypeInfo.NameFld.ToString.StartsWith('Nullable') then
        begin
          if TryNullableToJSON(AValue, AJsonObject, AName) then
          begin
            Exit;
          end;
        end;

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
                ('Cannot serialize property or field "%s" of TypeKind tkRecord (TValue with MVCValueAsTypeAttribute).',
                [AName]);
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
          raise EMVCSerializationException.CreateFmt
            ('Cannot serialize property or field "%s" of TypeKind tkRecord.', [AName]);
      end;

    tkSet:
      raise EMVCSerializationException.CreateFmt('Cannot serialize property or field "%s" of TypeKind tkSet.', [AName]);

    tkArray, tkDynArray:
      begin
        if AValue.GetArrayLength > 0 then
        begin
          for I := 0 to AValue.GetArrayLength - 1 do
          begin
            case AValue.GetArrayElement(I).Kind of
              tkChar, tkString, tkWChar, tkLString, tkWString, tkUString:
                AJsonObject.A[AName].Add(AValue.GetArrayElement(I).AsString);
              tkInteger:
                AJsonObject.A[AName].Add(AValue.GetArrayElement(I).AsInteger);
              tkInt64:
                AJsonObject.A[AName].Add(AValue.GetArrayElement(I).AsInt64);
              tkFloat:
                AJsonObject.A[AName].Add(AValue.GetArrayElement(I).AsExtended);
            else
              raise EMVCSerializationException.CreateFmt
                ('Cannot serialize property or field "%s" of TypeKind tkArray or tkDynArray.', [AName]);
            end;
          end;
        end;
      end;

    tkUnknown:
      raise EMVCSerializationException.CreateFmt
        ('Cannot serialize property or field "%s" of TypeKind tkUnknown.', [AName]);
  end;
end;

procedure TMVCJsonDataObjectsSerializer.DataSetRowToJsonArrayOfValues(const ADataSet: TDataSet;
  const AJsonArray: TJDOJsonArray; const AIgnoredFields: TMVCIgnoredList; const ADataSetFields: TMVCDataSetFields);
var
  lMS: TMemoryStream;
  lSS: TStringStream;
  lNestedDataSet: TDataSet;
  lChildJsonArray: TJDOJsonArray;
  lField: TMVCDataSetField;
  lDataSetFieldsDetail: TMVCDataSetFields;
begin
  Assert(Assigned(ADataSetFields));
  for lField in ADataSetFields do
  begin
    begin
      if ADataSet.Fields[lField.I].IsNull then
      begin
        AJsonArray.Add(TJsonObject(nil));
      end
      else
      begin
        case lField.DataType of
          ftBoolean:
            AJsonArray.Add(ADataSet.Fields[lField.I].AsBoolean);

          ftInteger, ftSmallint, ftShortint, ftByte:
            AJsonArray.Add(ADataSet.Fields[lField.I].AsInteger);

          ftLargeint, ftAutoInc, ftLongword:
            AJsonArray.Add(ADataSet.Fields[lField.I].AsLargeInt);
{$IFDEF TOKYOORBETTER}
          ftGuid:
            AJsonArray.Add(GUIDToString(ADataSet.Fields[lField.I].AsGuid));
{$ENDIF}
          ftSingle, ftFloat:
            AJsonArray.Add(ADataSet.Fields[lField.I].AsFloat);

          ftString, ftMemo:
            AJsonArray.Add(ADataSet.Fields[lField.I].AsString);

          ftWideString, ftWideMemo:
            AJsonArray.Add(ADataSet.Fields[lField.I].AsWideString);

          ftDate:
            AJsonArray.Add(DateToISODate(ADataSet.Fields[lField.I].AsDateTime));

          ftDateTime:
            AJsonArray.Add(DateTimeToISOTimeStamp(ADataSet.Fields[lField.I].AsDateTime));

          ftTime:
            AJsonArray.Add(SQLTimeStampToStr('hh:nn:ss', ADataSet.Fields[lField.I].AsSQLTimeStamp));

          ftTimeStamp:
            AJsonArray.Add(DateTimeToISOTimeStamp(SQLTimeStampToDateTime(ADataSet.Fields[lField.I].AsSQLTimeStamp)));

          ftCurrency:
            AJsonArray.Add(ADataSet.Fields[lField.I].AsCurrency);

          ftFMTBcd, ftBCD:
            AJsonArray.Add(BcdToDouble(ADataSet.Fields[lField.I].AsBcd));

          ftGraphic, ftBlob, ftStream, ftOraBlob:
            begin
              lMS := TMemoryStream.Create;
              try
                TBlobField(ADataSet.Fields[lField.I]).SaveToStream(lMS);
                lMS.Position := 0;
                lSS := TStringStream.Create;
                try
                  TMVCSerializerHelper.EncodeStream(lMS, lSS);
                  AJsonArray.Add(lSS.DataString);
                finally
                  lSS.Free;
                end;
              finally
                lMS.Free;
              end;
            end;

          ftDataSet:
            begin
              lNestedDataSet := TDataSetField(ADataSet.Fields[lField.I]).NestedDataSet;
              lDataSetFieldsDetail := GetDataSetFields(lNestedDataSet, AIgnoredFields,
                GetNameCase(lNestedDataSet, ncAsIs));
              try
                case GetDataType(ADataSet.Owner, ADataSet.Fields[lField.I].Name, dtArray) of
                  dtArray:
                    begin
                      lChildJsonArray := AJsonArray.AddArray;
                      lNestedDataSet.First;
                      while not lNestedDataSet.Eof do
                      begin
                        DataSetRowToJsonArrayOfValues(lNestedDataSet, lChildJsonArray, AIgnoredFields,
                          lDataSetFieldsDetail);
                        lNestedDataSet.Next;
                      end;
                    end;
                  dtObject:
                    begin
                      lChildJsonArray := AJsonArray.AddArray;
                      DataSetRowToJsonArrayOfValues(lNestedDataSet, lChildJsonArray, AIgnoredFields,
                        lDataSetFieldsDetail);
                    end;
                end;
              finally
                lDataSetFieldsDetail.Free;
              end;
            end;
        else
          raise EMVCSerializationException.CreateFmt('Cannot find type for field "%s"', [lField.FieldName]);
        end;
      end;
    end;
  end;
end;

procedure TMVCJsonDataObjectsSerializer.DataSetToJsonArray(const ADataSet: TDataSet; const AJsonArray: TJDOJsonArray;
  const ANameCase: TMVCNameCase; const AIgnoredFields: TMVCIgnoredList;
  const ASerializationCallback: TMVCDataSetFieldSerializationAction);
var
  LJObj: TJDOJsonObject;
  lDataSetFields: TMVCDataSetFields;
begin
  lDataSetFields := GetDataSetFields(ADataSet, AIgnoredFields, ANameCase);
  try
    while not ADataSet.Eof do
    begin
      LJObj := AJsonArray.AddObject;
      DataSetToJsonObject(ADataSet, LJObj, ANameCase, AIgnoredFields, lDataSetFields, ASerializationCallback);
      ADataSet.Next;
    end;
  finally
    lDataSetFields.Free;
  end;
end;

procedure TMVCJsonDataObjectsSerializer.DataSetToJsonArrayOfValues(const ADataSet: TDataSet;
  const AJsonArray: TJDOJsonArray; const AIgnoredFields: TMVCIgnoredList);
var
  LJArr: TJDOJsonArray;
  lDataSetFields: TMVCDataSetFields;
begin
  lDataSetFields := GetDataSetFields(ADataSet, AIgnoredFields, ncAsIs);
  try
    while not ADataSet.Eof do
    begin
      LJArr := AJsonArray.AddArray;
      DataSetRowToJsonArrayOfValues(ADataSet, LJArr, AIgnoredFields, lDataSetFields);
      ADataSet.Next;
    end;
  finally
    lDataSetFields.Free;
  end;
end;

procedure TMVCJsonDataObjectsSerializer.DataSetToJsonObject(const ADataSet: TDataSet; const AJsonObject: TJDOJsonObject;
  const ANameCase: TMVCNameCase; const AIgnoredFields: TMVCIgnoredList; const ADataSetFields: TMVCDataSetFields;
  const ASerializationCallback: TMVCDataSetFieldSerializationAction);
var
  lMS: TMemoryStream;
  lSS: TStringStream;
  lNestedDataSet: TDataSet;
  lChildJsonArray: TJDOJsonArray;
  lChildJsonObject: TJDOJsonObject;
  lField: TMVCDataSetField;
  lDataSetFieldsDetail: TMVCDataSetFields;
  lHandled: Boolean;
  lFName: string;
begin
  Assert(Assigned(ADataSetFields));
  for lField in ADataSetFields do
  begin
    begin
      if Assigned(ASerializationCallback) then
      begin
        lHandled := False;
        ASerializationCallback(ADataSet.Fields[lField.I], AJsonObject, lHandled);
        if lHandled then
        begin
          continue;
        end;
      end;

      lFName := TMVCSerializerHelper.ApplyNameCase(ANameCase, lField.FieldName);

      if ADataSet.Fields[lField.I].IsNull then
        AJsonObject[lFName] := Null
      else
      begin
        case lField.DataType of
          ftBoolean:
            AJsonObject.B[lFName] := ADataSet.Fields[lField.I].AsBoolean;

          ftInteger, ftSmallint, ftShortint, ftByte, ftWord:
            AJsonObject.I[lFName] := ADataSet.Fields[lField.I].AsInteger;

          ftLargeint, ftAutoInc, ftLongword:
            AJsonObject.L[lFName] := ADataSet.Fields[lField.I].AsLargeInt;
{$IFDEF TOKYOORBETTER}
          ftGuid:
            AJsonObject.S[lFName] := GUIDToString(ADataSet.Fields[lField.I].AsGuid);
{$ENDIF}
          ftSingle, ftFloat:
            AJsonObject.F[lFName] := ADataSet.Fields[lField.I].AsFloat;

          ftString, ftMemo:
            AJsonObject.S[lFName] := ADataSet.Fields[lField.I].AsString;

          ftWideString, ftWideMemo:
            AJsonObject.S[lFName] := ADataSet.Fields[lField.I].AsWideString;

          ftDate:
            AJsonObject.S[lFName] := DateToISODate(ADataSet.Fields[lField.I].AsDateTime);

          ftDateTime:
            AJsonObject.S[lFName] := DateTimeToISOTimeStamp(ADataSet.Fields[lField.I].AsDateTime);

          ftTime:
            AJsonObject.S[lFName] := SQLTimeStampToStr('hh:nn:ss', ADataSet.Fields[lField.I].AsSQLTimeStamp);

          ftTimeStamp:
            AJsonObject.S[lFName] :=
              DateTimeToISOTimeStamp(SQLTimeStampToDateTime(ADataSet.Fields[lField.I].AsSQLTimeStamp));

          ftCurrency:
            AJsonObject.F[lFName] := ADataSet.Fields[lField.I].AsCurrency;

          ftFMTBcd, ftBCD:
            AJsonObject.F[lFName] := BcdToDouble(ADataSet.Fields[lField.I].AsBcd);

          ftGraphic, ftBlob, ftStream, ftOraBlob:
            begin
              lMS := TMemoryStream.Create;
              try
                TBlobField(ADataSet.Fields[lField.I]).SaveToStream(lMS);
                lMS.Position := 0;
                lSS := TStringStream.Create;
                try
                  TMVCSerializerHelper.EncodeStream(lMS, lSS);
                  AJsonObject.S[lFName] := lSS.DataString;
                finally
                  lSS.Free;
                end;
              finally
                lMS.Free;
              end;
            end;

          ftDataSet:
            begin
              lNestedDataSet := TDataSetField(ADataSet.Fields[lField.I]).NestedDataSet;
              lDataSetFieldsDetail := GetDataSetFields(lNestedDataSet, AIgnoredFields,
                GetNameCase(lNestedDataSet, ANameCase));
              try
                case GetDataType(ADataSet.Owner, ADataSet.Fields[lField.I].Name, dtArray) of
                  dtArray:
                    begin
                      lChildJsonArray := AJsonObject.A[lField.FieldName];
                      lNestedDataSet.First;
                      while not lNestedDataSet.Eof do
                      begin
                        DataSetToJsonObject(lNestedDataSet, lChildJsonArray.AddObject,
                          GetNameCase(lNestedDataSet, ANameCase), AIgnoredFields, lDataSetFieldsDetail,
                          ASerializationCallback);
                        lNestedDataSet.Next;
                      end;
                    end;
                  dtObject:
                    begin
                      lChildJsonObject := AJsonObject.O[lField.FieldName];
                      DataSetToJsonObject(lNestedDataSet, lChildJsonObject, GetNameCase(lNestedDataSet, ANameCase),
                        AIgnoredFields, lDataSetFieldsDetail, ASerializationCallback);
                    end;
                end;
              finally
                lDataSetFieldsDetail.Free;
              end;
            end;
        else
          raise EMVCSerializationException.CreateFmt('Cannot find type for field "%s"', [lField.FieldName]);
        end;
      end;
    end;
  end;
end;

procedure TMVCJsonDataObjectsSerializer.DeserializeCollection(const ASerializedList: string; const AList: TObject;
  const AClazz: TClass; const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);
var
  JsonArray: TJDOJsonArray;
  ObjList: IMVCList;
begin
  if (ASerializedList = EmptyStr) then
    raise EMVCException.Create(HTTP_STATUS.BadRequest, 'Invalid body');

  if not Assigned(AList) then
    Exit;

  ObjList := TDuckTypedList.Wrap(AList);
  if Assigned(ObjList) then
  begin
    JsonArray := TJDOJsonArray.Parse(ASerializedList) as TJDOJsonArray;
    try
      JsonArrayToList(JsonArray, ObjList, AClazz, AType, AIgnoredAttributes);
    finally
      JsonArray.Free;
    end;
  end;
end;

procedure TMVCJsonDataObjectsSerializer.DeserializeCollection(const ASerializedList: string; const AList: IInterface;
  const AClazz: TClass; const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);
begin
  DeserializeCollection(ASerializedList, TObject(AList), AClazz, AType, AIgnoredAttributes);
end;

procedure TMVCJsonDataObjectsSerializer.DeserializeDataSet(const ASerializedDataSet: string; const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase);
var
  lJsonArray: TJDOJsonArray;
begin
  if (ASerializedDataSet = EmptyStr) then
    raise EMVCException.Create(HTTP_STATUS.BadRequest, 'Invalid body');

  if not Assigned(ADataSet) then
    Exit;

  try
    lJsonArray := TJDOJsonArray.Parse(ASerializedDataSet) as TJDOJsonArray;
  except
    on E: EJsonParserException do
    begin
      raise EMVCException.Create(HTTP_STATUS.BadRequest, 'Invalid body');
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
  lJsonBase: TJDOJsonBaseObject;
begin
  if (ASerializedDataSetRecord = EmptyStr) or (not Assigned(ADataSet)) then
    Exit;

  lJsonBase := TJDOJsonObject.Parse(ASerializedDataSetRecord);
  try
    if lJsonBase is TJsonObject then
    begin
      if not(ADataSet.State in [dsInsert, dsEdit]) then
      begin
        ADataSet.Edit;
      end;
      JsonObjectToDataSet(TJsonObject(lJsonBase), ADataSet, AIgnoredFields, ANameCase);
      ADataSet.Post;
    end
    else
    begin
      raise EMVCSerializationException.Create('Cannot deserialize, expected json object');
    end;
  finally
    lJsonBase.Free;
  end;
end;

procedure TMVCJsonDataObjectsSerializer.DeserializeObject(const ASerializedObject: string; const AObject: IInterface;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);
begin
  DeserializeObject(ASerializedObject, TObject(AObject), AType, AIgnoredAttributes);
end;

function TMVCJsonDataObjectsSerializer.JsonArrayToArray(const AJsonArray: TJDOJsonArray): TValue;
var
  I: Integer;
  lStrArr: TArray<string>;
  lIntArr: TArray<Integer>;
  lLongArr: TArray<Int64>;
  lDoubleArr: TArray<Double>;
begin
  for I := 0 to Pred(AJsonArray.Count) do
  begin
    case AJsonArray.types[0] of
      jdtString:
        lStrArr := lStrArr + [AJsonArray.Items[I].Value];
      jdtInt:
        lIntArr := lIntArr + [AJsonArray.Items[I].IntValue];
      jdtLong:
        lLongArr := lLongArr + [AJsonArray.Items[I].LongValue];
      jdtFloat:
        lDoubleArr := lDoubleArr + [AJsonArray.Items[I].FloatValue];
    end;
  end;

  if Length(lStrArr) > 0 then
    Result := TValue.From < TArray < string >> (lStrArr)
  else if Length(lIntArr) > 0 then
    Result := TValue.From < TArray < Integer >> (lIntArr)
  else if Length(lLongArr) > 0 then
    Result := TValue.From < TArray < Int64 >> (lLongArr)
  else
    Result := TValue.From < TArray < Double >> (lDoubleArr);
end;

procedure TMVCJsonDataObjectsSerializer.JsonArrayToDataSet(const AJsonArray: TJDOJsonArray; const ADataSet: TDataSet;
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

procedure TMVCJsonDataObjectsSerializer.JsonArrayToList(const AJsonArray: TJDOJsonArray; const AList: IMVCList;
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

procedure TMVCJsonDataObjectsSerializer.JsonDataValueToAttribute(const AJsonObject: TJDOJsonObject; const AName: string;
  var AValue: TValue; const AType: TMVCSerializationType; const AIgnored: TMVCIgnoredList;
  const ACustomAttributes: TArray<TCustomAttribute>);
var
  ChildObject: TObject;
  ChildList: IMVCList;
  ChildListOfAtt: MVCListOfAttribute;
  LEnumAsAttr: MVCEnumSerializationAttribute;
  LEnumMappedValues: TList<string>;
  LEnumSerType: TMVCEnumSerializationType;
  LClazz: TClass;
  LMappedValueIndex: Integer;
  lOutInteger: Integer;
  lOutInteger64: Int64;
begin
  if GetTypeSerializers.ContainsKey(AValue.TypeInfo) then
  begin
    case AJsonObject[AName].Typ of
      jdtNone:
        Exit;
      jdtObject:
        begin
          /// <summary>JsonDataObjects assumes values null as jdtObject</summary>
          if AJsonObject[AName].ObjectValue <> nil then
            GetTypeSerializers.Items[AValue.TypeInfo].DeserializeAttribute(AValue, AName,
              AJsonObject[AName].ObjectValue, ACustomAttributes);
        end;
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

        else if (AValue.Kind = tkRecord) and (AValue.TypeInfo <> TypeInfo(TValue)) then { nullables }
        begin
          if AValue.TypeInfo = TypeInfo(NullableString) then
          begin
            AValue := TValue.From<NullableString>(NullableString(AJsonObject[AName].Value))
          end
          else if AValue.TypeInfo = TypeInfo(NullableTDate) then
          begin
            AValue := TValue.From<NullableTDate>(NullableTDate(ISODateToDate(AJsonObject[AName].Value)))
          end
          else if AValue.TypeInfo = TypeInfo(NullableTDateTime) then
          begin
            AValue := TValue.From<NullableTDateTime>
              (NullableTDateTime(ISOTimeStampToDateTime(AJsonObject[AName].Value)))
          end
          else if AValue.TypeInfo = TypeInfo(NullableTTime) then
          begin
            AValue := TValue.From<NullableTTime>(NullableTTime(ISOTimeToTime(AJsonObject[AName].Value)))
          end
          else
            raise EMVCSerializationException.CreateFmt('Cannot deserialize property "%s" from string', [AName]);
        end
        else if (AValue.Kind = tkEnumeration) then
        begin
          LEnumSerType := estEnumName;
          LEnumMappedValues := nil;
          if TMVCSerializerHelper.AttributeExists<MVCEnumSerializationAttribute>(ACustomAttributes, LEnumAsAttr) then
          begin
            LEnumSerType := LEnumAsAttr.SerializationType;
            LEnumMappedValues := LEnumAsAttr.MappedValues;
          end;

          if LEnumSerType = estEnumName then
          begin
            TValue.Make(GetEnumValue(AValue.TypeInfo, AJsonObject[AName].Value), AValue.TypeInfo, AValue)
          end
          else
          begin
            LMappedValueIndex := LEnumMappedValues.IndexOf(AJsonObject[AName].Value);
            if LMappedValueIndex < 0 then
              raise EMVCSerializationException.CreateFmt('Cannot deserialize property "%s" from mapped values',
                [AName]);

            TValue.Make(GetEnumValue(AValue.TypeInfo, GetEnumName(AValue.TypeInfo, LMappedValueIndex)),
              AValue.TypeInfo, AValue)
          end;
        end
        else if (AValue.Kind = tkInteger) and (TryStrToInt(AJsonObject[AName].Value, lOutInteger)) then
        begin
          AValue := lOutInteger;
        end
        else if (AValue.Kind = tkInt64) and (TryStrToInt64(AJsonObject[AName].Value, lOutInteger64)) then
        begin
          AValue := lOutInteger64;
        end
        else
          AValue := TValue.From<string>(AJsonObject[AName].Value);
      end;

    jdtInt:
      begin
        if (AValue.Kind = tkEnumeration) then
        begin
          TValue.Make(GetEnumValue(AValue.TypeInfo, GetEnumName(AValue.TypeInfo, AJsonObject[AName].IntValue)),
            AValue.TypeInfo, AValue)
        end
        else if (AValue.Kind <> tkRecord) then { nullables }
        begin
          AValue := TValue.From<Integer>(AJsonObject[AName].IntValue);
        end
        else
        begin
          if AValue.TypeInfo = TypeInfo(NullableInt32) then
            AValue := TValue.From<NullableInt32>(NullableInt32(AJsonObject[AName].IntValue))
          else if AValue.TypeInfo = TypeInfo(NullableUInt32) then
            AValue := TValue.From<NullableUInt32>(NullableUInt32(AJsonObject[AName].IntValue))
          else if AValue.TypeInfo = TypeInfo(NullableInt16) then
            AValue := TValue.From<NullableInt16>(NullableInt16(AJsonObject[AName].IntValue))
          else if AValue.TypeInfo = TypeInfo(NullableUInt16) then
            AValue := TValue.From<NullableUInt16>(NullableUInt16(AJsonObject[AName].IntValue))
          else if AValue.TypeInfo = TypeInfo(NullableInt64) then
            AValue := TValue.From<NullableInt64>(NullableInt64(AJsonObject[AName].LongValue))
          else if AValue.TypeInfo = TypeInfo(NullableUInt64) then
            AValue := TValue.From<NullableUInt64>(NullableUInt64(AJsonObject[AName].LongValue))
          else if not TryMapNullableFloat(AValue, AJsonObject, AName) then
            raise EMVCDeserializationException.CreateFmt('Cannot deserialize integer value for "%s"', [AName]);
        end;
      end;

    jdtLong, jdtULong:
      begin
        if (AValue.TypeInfo = System.TypeInfo(TTimeStamp)) then
        begin
          AValue := TValue.From<TTimeStamp>(MSecsToTimeStamp(AJsonObject[AName].LongValue))
        end
        else if (AValue.Kind <> tkRecord) then { nullables }
        begin
          AValue := TValue.From<Int64>(AJsonObject[AName].LongValue);
        end
        else
        begin
          if AValue.TypeInfo = TypeInfo(NullableInt64) then
            AValue := TValue.From<NullableInt64>(NullableInt64(AJsonObject[AName].LongValue))
          else if AValue.TypeInfo = TypeInfo(NullableUInt64) then
            AValue := TValue.From<NullableUInt64>(NullableUInt64(AJsonObject[AName].LongValue))
          else if not TryMapNullableFloat(AValue, AJsonObject, AName) then
            raise EMVCDeserializationException.CreateFmt('Cannot deserialize long integer value for "%s"', [AName]);
        end;
      end;

    jdtFloat:
      if (AValue.Kind <> tkRecord) then { nullables }
      begin
        AValue := TValue.From<Double>(AJsonObject[AName].FloatValue);
      end
      else
      begin
        if not TryMapNullableFloat(AValue, AJsonObject, AName) then
          raise EMVCDeserializationException.CreateFmt('Cannot deserialize floating-point value for "%s"', [AName]);
      end;

    jdtDateTime:
      if (AValue.Kind <> tkRecord) then { nullables }
      begin
        AValue := TValue.From<TDateTime>(AJsonObject[AName].DateTimeValue);
      end
      else
      begin
        if AValue.TypeInfo = TypeInfo(NullableTDate) then
          AValue := TValue.From<NullableTDate>(NullableTDate(AJsonObject[AName].DateTimeValue))
        else if AValue.TypeInfo = TypeInfo(NullableTDateTime) then
          AValue := TValue.From<NullableTDateTime>(NullableTDateTime(AJsonObject[AName].DateTimeValue))
        else if AValue.TypeInfo = TypeInfo(NullableTTime) then
          AValue := TValue.From<NullableTTime>(NullableTTime(AJsonObject[AName].DateTimeValue))
        else
          raise EMVCDeserializationException.CreateFmt('Cannot deserialize date or time value for "%s"', [AName]);
      end;

    jdtBool:
      if (AValue.Kind <> tkRecord) then { nullables }
      begin
        AValue := TValue.From<Boolean>(AJsonObject[AName].BoolValue);
      end
      else
      begin
        if AValue.TypeInfo = TypeInfo(NullableBoolean) then
          AValue := TValue.From<NullableBoolean>(NullableBoolean(AJsonObject[AName].BoolValue))
        else
          raise EMVCDeserializationException.CreateFmt('Cannot deserialize boolean value for "%s"', [AName]);
      end;

    jdtObject:
      begin
        if (AValue.TypeInfo = System.TypeInfo(TValue)) then
          AValue := TValue.FromVariant(AJsonObject[AName].O['value'].VariantValue)
        else
        begin
          // dt: if a key is null, jsondataobjects assign it the type jdtObject
          if AJsonObject[AName].ObjectValue <> nil then
          begin
            case AValue.Kind of
              tkInterface:
                begin
                  ChildObject := TObject(AValue.AsInterface);
                  JsonObjectToObject(AJsonObject.O[AName], ChildObject, GetSerializationType(ChildObject, AType),
                    AIgnored);
                end;
              tkClass:
                begin
                  ChildObject := AValue.AsObject;
                  JsonObjectToObject(AJsonObject.O[AName], ChildObject, GetSerializationType(ChildObject, AType),
                    AIgnored);
                end;
              tkString, tkUString:
                begin
                  AValue := AJsonObject.O[AName].ToJSON();
                end;
              tkRecord:
                begin
                  if AValue.TypeInfo = TypeInfo(NullableString) then
                  begin
                    AValue := TValue.From<NullableString>(NullableString(AJsonObject.O[AName].ToJSON()));
                  end
                  else
                  begin
                    raise EMVCDeserializationException.CreateFmt('Cannot deserialize object value for "%s"', [AName]);
                  end;
                end
            end;
          end;
        end;
      end;

    jdtArray:
      begin
        if AValue.Kind = tkInterface then
          ChildObject := TObject(AValue.AsInterface)
        else
          ChildObject := AValue.AsObject;
        if Assigned(ChildObject) then
        begin
          if ChildObject is TDataSet then
            JsonArrayToDataSet(AJsonObject.A[AName], ChildObject as TDataSet, AIgnored, ncLowerCase)
          else
          begin
            ChildList := TDuckTypedList.Wrap(ChildObject);

            if TMVCSerializerHelper.AttributeExists<MVCListOfAttribute>(ACustomAttributes, ChildListOfAtt) then
              LClazz := ChildListOfAtt.Value
            else
              LClazz := GetObjectTypeOfGenericList(AValue.TypeInfo);

            if Assigned(LClazz) then
              JsonArrayToList(AJsonObject.A[AName], ChildList, LClazz, AType, AIgnored)
            else
              raise EMVCDeserializationException.CreateFmt
                ('You can not deserialize a list "%s" without the MVCListOf attribute.', [AName]);
          end;
        end
        else if AValue.isArray then
        begin
          AValue := JsonArrayToArray(AJsonObject.A[AName]);
        end;
      end;
  end;
end;

procedure TMVCJsonDataObjectsSerializer.JsonObjectToDataSet(const AJsonObject: TJDOJsonObject;
  const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase);
var
  Field: TField;
  lName: string;
  SS: TStringStream;
  SM: TMemoryStream;
  NestedDataSet: TDataSet;
begin
  if (ADataSet.State in [dsInsert, dsEdit]) then
  begin
    for Field in ADataSet.Fields do
    begin
      lName := GetNameAs(ADataSet.Owner, Field.Name, Field.FieldName);

      if (IsIgnoredAttribute(AIgnoredFields, lName)) or (IsIgnoredComponent(ADataSet.Owner, Field.Name)) then
        continue;

      lName := TMVCSerializerHelper.ApplyNameCase(GetNameCase(ADataSet, ANameCase), lName { Field.FieldName } );
      // case GetNameCase(ADataSet, ANameCase) of
      // ncLowerCase:
      // name := LowerCase(Field.FieldName);
      // ncUpperCase:
      // name := UpperCase(Field.FieldName);
      // end;

      if not AJsonObject.Contains(lName) then
        continue;

      if (AJsonObject[lName].Typ = jdtObject) and (AJsonObject.Values[lName].ObjectValue = nil) then
      // Nullable Type
      begin
        Field.Clear;
        continue;
      end;

      case Field.DataType of
        TFieldType.ftBoolean:
          Field.AsBoolean := AJsonObject.B[lName];

        TFieldType.ftInteger, TFieldType.ftSmallint, TFieldType.ftShortint, TFieldType.ftByte, TFieldType.ftLongword,
          TFieldType.ftWord, TFieldType.ftAutoInc:
          Field.AsInteger := AJsonObject.I[lName];

        TFieldType.ftLargeint:
          Field.AsLargeInt := AJsonObject.L[lName];

        TFieldType.ftCurrency:
          Field.AsCurrency := AJsonObject.F[lName];

        TFieldType.ftSingle:
          Field.AsSingle := AJsonObject.F[lName];

        TFieldType.ftFloat, TFieldType.ftFMTBcd, TFieldType.ftBCD:
          Field.AsFloat := AJsonObject.F[lName];

        ftString, ftWideString, ftMemo, ftWideMemo:
          Field.AsWideString := AJsonObject.S[lName];

        TFieldType.ftDate:
          Field.AsDateTime := ISODateToDate(AJsonObject.S[lName]);

        TFieldType.ftDateTime:
          Field.AsDateTime := ISOTimeStampToDateTime(AJsonObject.S[lName]);

        TFieldType.ftTimeStamp, TFieldType.ftTime:
          Field.AsDateTime := ISOTimeToTime(AJsonObject.S[lName]);

{$IFDEF TOKYOORBETTER}
        TFieldType.ftGuid:
          Field.AsGuid := StringToGUID(AJsonObject.S[lName]);
{$ENDIF}
        TFieldType.ftGraphic, TFieldType.ftBlob, TFieldType.ftStream:
          begin
            SS := TStringStream.Create(AJsonObject.S[lName]);
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
                  JsonArrayToDataSet(AJsonObject.A[lName], NestedDataSet, AIgnoredFields, ANameCase);
                end;
              dtObject:
                begin
                  NestedDataSet.Edit;
                  JsonObjectToDataSet(AJsonObject.O[lName], NestedDataSet, AIgnoredFields, ANameCase);
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

procedure TMVCJsonDataObjectsSerializer.JsonObjectToObject(const AJsonObject: TJDOJsonObject; const AObject: TObject;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);
var
  lObjType: TRttiType;
  lProp: TRttiProperty;
  lFld: TRttiField;
  lAttributeValue: TValue;
  lKeyName: string;
  lErrMsg: string;
begin
  if AObject is TJsonObject then
  begin
    if not Assigned(AObject) then
    begin
      raise EMVCDeserializationException.Create(AObject.ClassName + ' is not assigned');
    end;
    TJsonObject(AObject).Assign(AJsonObject);
    Exit;
  end;

  lProp := nil;
  lFld := nil;

  lObjType := GetRttiContext.GetType(AObject.ClassType);
  case AType of
    stDefault, stProperties:
      begin
        try
          for lProp in lObjType.GetProperties do
          begin

{$IFDEF AUTOREFCOUNT}
            if TMVCSerializerHelper.IsAPropertyToSkip(lProp.Name) then
              continue;

{$ENDIF}
            if ((not TMVCSerializerHelper.HasAttribute<MVCDoNotDeserializeAttribute>(lProp)) and
              (not IsIgnoredAttribute(AIgnoredAttributes, lProp.Name)) and
              (lProp.IsWritable or lProp.GetValue(AObject).IsObject)) then
            begin
              lAttributeValue := lProp.GetValue(AObject);
              lKeyName := TMVCSerializerHelper.GetKeyName(lProp, lObjType);
              JsonDataValueToAttribute(AJsonObject, lKeyName, lAttributeValue, AType, AIgnoredAttributes,
                lProp.GetAttributes);
              if (not lAttributeValue.IsEmpty) and (not lAttributeValue.IsObject) and lProp.IsWritable then
              begin
                lProp.SetValue(AObject, lAttributeValue);
              end;
            end;
          end;
        except
          on E: EInvalidCast do
          begin
            if lProp <> nil then
            begin
              lErrMsg := Format('Invalid class typecast for property "%s" [Expected: %s, Actual: %s]',
                [
                lKeyName, lProp.PropertyType.ToString(),
                JDO_TYPE_DESC[AJsonObject[lKeyName].Typ]
                ]);
            end
            else
            begin
              lErrMsg := Format('Invalid class typecast for property "%s" [Actual: %s]',
                [
                lKeyName, JDO_TYPE_DESC[AJsonObject[lKeyName].Typ]
                ]);
            end;
            raise EMVCException.Create(HTTP_STATUS.BadRequest, lErrMsg);
          end;
        end;
      end;
    stFields:
      begin
        try
          for lFld in lObjType.GetFields do
            if (not TMVCSerializerHelper.HasAttribute<MVCDoNotDeserializeAttribute>(lFld)) and
              (not IsIgnoredAttribute(AIgnoredAttributes, lFld.Name)) then
            begin
              lAttributeValue := lFld.GetValue(AObject);
              lKeyName := TMVCSerializerHelper.GetKeyName(lFld, lObjType);
              JsonDataValueToAttribute(AJsonObject, lKeyName, lAttributeValue, AType, AIgnoredAttributes,
                lFld.GetAttributes);
              if not lAttributeValue.IsEmpty then
                lFld.SetValue(AObject, lAttributeValue);
            end;
        except
          on E: EInvalidCast do
          begin
            if lFld <> nil then
            begin
              lErrMsg := Format('Invalid class typecast for field "%s" [Expected: %s, Actual: %s]',
                [
                lKeyName, lFld.FieldType.ToString(),
                JDO_TYPE_DESC[AJsonObject[lKeyName].Typ]
                ]);
            end
            else
            begin
              lErrMsg := Format('Invalid class typecast for field "%s" [Actual: %s]',
                [
                lKeyName, JDO_TYPE_DESC[AJsonObject[lKeyName].Typ]
                ]);
            end;
            raise EMVCException.Create(HTTP_STATUS.BadRequest, lErrMsg);
          end;
        end;
      end;
  end;
end;

procedure TMVCJsonDataObjectsSerializer.ListToJsonArray(const AList: IMVCList;
  const AJsonArray: TJDOJsonArray;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList;
  const ASerializationAction: TMVCSerializationAction);
var
  I: Integer;
  lDict: IMVCLinks;
  lSer: IMVCTypeSerializer;
begin
  if not Assigned(AList) then
    raise EMVCSerializationException.Create('List not assigned');
  if Assigned(ASerializationAction) then
  begin
    lDict := TJDOLinks.Create;
    for I := 0 to Pred(AList.Count) do
    begin
      lDict.Clear;
      InternalObjectToJsonObject(AList.GetItem(I), AJsonArray.AddObject, AType, AIgnoredAttributes,
        ASerializationAction, lDict, lSer);
    end;
  end
  else
  begin
    for I := 0 to Pred(AList.Count) do
    begin
      InternalObjectToJsonObject(AList.GetItem(I), AJsonArray.AddObject, AType, AIgnoredAttributes, nil, nil,
        nil);
    end;
  end;
end;

procedure TMVCJsonDataObjectsSerializer.ObjectToJsonObject(const AObject: TObject;
  const AJsonObject: TJDOJsonObject;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);
begin
  InternalObjectToJsonObject(AObject, AJsonObject, AType, AIgnoredAttributes, nil, nil, nil);
end;

procedure TMVCJsonDataObjectsSerializer.InternalObjectToJsonObject(const AObject: TObject;
  const AJsonObject: TJDOJsonObject; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList;
  const ASerializationAction: TMVCSerializationAction; const Links: IMVCLinks;
  const Serializer: IMVCTypeSerializer);
var
  ObjType: TRttiType;
  Prop: TRttiProperty;
  Fld: TRttiField;
begin
  { TODO -oDanieleT -cGeneral : Find a way to automatically add HATEOS }
  ObjType := GetRttiContext.GetType(AObject.ClassType);
  case AType of
    stDefault, stProperties:
      begin
        for Prop in ObjType.GetProperties do
        begin

{$IFDEF AUTOREFCOUNT}
          if TMVCSerializerHelper.IsAPropertyToSkip(Prop.Name) then
            continue;

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
        begin
          if (not TMVCSerializerHelper.HasAttribute<MVCDoNotSerializeAttribute>(Fld)) and
            (not IsIgnoredAttribute(AIgnoredAttributes, Fld.Name)) then
            AttributeToJsonDataValue(AJsonObject, TMVCSerializerHelper.GetKeyName(Fld, ObjType),
              Fld.GetValue(AObject),
              AType, AIgnoredAttributes, Fld.GetAttributes);
        end;
      end;
  end;

  if Assigned(ASerializationAction) then
  begin
    ASerializationAction(AObject, Links);
    TJDOLinks(Links).FillJSONArray(AJsonObject.A[TMVCConstants.HATEOAS_PROP_NAME]);
  end;
end;

procedure TMVCJsonDataObjectsSerializer.InternalSerializeDataSet(
  const ADataSet: TDataSet; const AJsonArray: TJsonArray; const AIgnoredFields: TMVCIgnoredList;
  const ANameCase: TMVCNameCase;
  const ASerializationAction: TMVCDatasetSerializationAction);
var
  BookMark: TBookmark;
  lLinks: IMVCLinks;
  LJObj: TJsonObject;
  lDataSetFields: TMVCDataSetFields;
begin
  lLinks := nil;
  if Assigned(ASerializationAction) then
  begin
    lLinks := TJDOLinks.Create;
  end;
  lDataSetFields := GetDataSetFields(ADataSet, AIgnoredFields, ANameCase);
  try
    BookMark := ADataSet.BookMark;
    try
      ADataSet.First;
      while not ADataSet.Eof do
      begin
        LJObj := AJsonArray.AddObject;
        DataSetToJsonObject(ADataSet, LJObj, ncAsIs { already applied } , AIgnoredFields, lDataSetFields);
        if Assigned(ASerializationAction) then
        begin
          lLinks.Clear;
          ASerializationAction(ADataSet, lLinks);
          TJDOLinks(lLinks).FillJSONArray(LJObj.A[TMVCConstants.HATEOAS_PROP_NAME]);
        end;
        ADataSet.Next;
      end;
    finally
      if ADataSet.BookmarkValid(BookMark) then
        ADataSet.GotoBookmark(BookMark);
      ADataSet.FreeBookmark(BookMark);
    end;
  finally
    lDataSetFields.Free;
  end;
end;

procedure TMVCJsonDataObjectsSerializer.InternalSerializeDataSetRecord(
  const DataSet: TDataSet; const JSONObject: TJsonObject;
  const IgnoredFields: TMVCIgnoredList; const NameCase: TMVCNameCase;
  const SerializationAction: TMVCDatasetSerializationAction);
var
  lNameCase: TMVCNameCase;
  lDataSetFields: TList<TMVCDataSetField>;
  lLinks: IMVCLinks;
begin
  lNameCase := GetNameCase(DataSet, NameCase);
  lDataSetFields := GetDataSetFields(DataSet, IgnoredFields, lNameCase);
  try
    DataSetToJsonObject(DataSet, JSONObject, ncAsIs { lNameCase } , IgnoredFields, lDataSetFields);
    lLinks := TJDOLinks.Create;
    if Assigned(SerializationAction) then
    begin
      SerializationAction(DataSet, lLinks);
      TJDOLinks(lLinks).FillJSONArray(JSONObject.A[
        TMVCSerializerHelper.ApplyNameCase(lNameCase, TMVCConstants.HATEOAS_PROP_NAME)
        ]);
    end;
  finally
    lDataSetFields.Free;
  end;
end;

class
  function TMVCJsonDataObjectsSerializer.Parse<T>(const AString: string): T;
begin
  Result := TJDOJsonObject.Parse(AString) as T;
  if not Assigned(Result) then
    raise EMVCDeserializationException.Create('Cannot parse string as ' + T.ClassName);
end;

class
  function TMVCJsonDataObjectsSerializer.ParseArray(const AString: string): TJDOJsonArray;
begin
  Result := Parse<TJDOJsonArray>(AString);
end;

class
  function TMVCJsonDataObjectsSerializer.ParseObject(const AString: string): TJDOJsonObject;
begin
  Result := Parse<TJDOJsonObject>(AString);
end;

function TMVCJsonDataObjectsSerializer.SerializeCollection(const AList: TObject;
  const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList; const ASerializationAction: TMVCSerializationAction): string;
var
  JsonArray: TJDOJsonArray;
  ObjList: IMVCList;
  Obj: TObject;
  lLinks: IMVCLinks;
  lSer: IMVCTypeSerializer;
begin
  Result := EmptyStr;

  if not Assigned(AList) then
    Exit;

  if AList is TJsonBaseObject then
    Exit(TJsonBaseObject(AList).ToJSON(True));

  ObjList := TDuckTypedList.Wrap(AList);
  if Assigned(ObjList) then
  begin
    JsonArray := TJDOJsonArray.Create;
    try
      if Assigned(ASerializationAction) then
      begin
        if not GetTypeSerializers.TryGetValue(TypeInfo(TMVCStringDictionary), lSer) then
        begin
          raise EMVCSerializationException.Create
            ('Cannot serialize _links without TMVCStringDictionary custom serializer');
        end;

        lLinks := TJDOLinks.Create;
        for Obj in ObjList do
        begin
          lLinks.Clear;
          InternalObjectToJsonObject(Obj, JsonArray.AddObject, GetSerializationType(Obj, AType), AIgnoredAttributes,
            ASerializationAction, lLinks, lSer);
        end;
      end
      else
      begin
        for Obj in ObjList do
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

function TMVCJsonDataObjectsSerializer.SerializeCollection(const AList: IInterface;
  const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList; const ASerializationAction: TMVCSerializationAction): string;
begin
  Result := SerializeCollection(TObject(AList), AType, AIgnoredAttributes, ASerializationAction);
end;

function TMVCJsonDataObjectsSerializer.SerializeDataSet(const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase = ncAsIs;
  const ASerializationAction: TMVCDatasetSerializationAction = nil): string;
var
  JsonArray: TJDOJsonArray;
begin
  Result := EmptyStr;

  if (not Assigned(ADataSet)) then
    Exit('null');
  if ADataSet.IsEmpty then
    Exit('[]'); // https://github.com/danieleteti/delphimvcframework/issues/219

  JsonArray := TJsonArray.Create;
  try
    InternalSerializeDataSet(ADataSet, JsonArray, AIgnoredFields, ANameCase, ASerializationAction);
    Result := JsonArray.ToJSON(True);
  finally
    JsonArray.Free;
  end;
end;

function TMVCJsonDataObjectsSerializer.SerializeDataSetRecord(const DataSet: TDataSet;
  const IgnoredFields: TMVCIgnoredList; const NameCase: TMVCNameCase = ncAsIs;
  const SerializationAction: TMVCDatasetSerializationAction = nil): string;
var
  lJSONObject: TJDOJsonObject;
begin
  Result := EmptyStr;
  if (not Assigned(DataSet)) or DataSet.IsEmpty then
    Exit('null');

  lJSONObject := TJDOJsonObject.Create;
  try
    InternalSerializeDataSetRecord(DataSet, lJSONObject, IgnoredFields, NameCase, SerializationAction);
    Result := lJSONObject.ToJSON(True);
  finally
    lJSONObject.Free;
  end;
end;

function TMVCJsonDataObjectsSerializer.SerializeObject(const AObject: TObject;
  const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList; const ASerializationAction: TMVCSerializationAction): string;
var
  LJObj: TJDOJsonObject;
  lObjType: TRttiType;
  lDict: IMVCLinks;
begin
  Result := EmptyStr;

  if not Assigned(AObject) then
    Exit('null');

  if AObject is TJsonBaseObject then
    Exit(TJsonBaseObject(AObject).ToJSON(True));

  if AObject is TDataSet then
    Exit(self.SerializeDataSet(TDataSet(AObject), AIgnoredAttributes));

  if AObject is System.JSON.TJsonValue then
    Exit(System.JSON.TJsonValue(AObject).ToJSON);

  lObjType := GetRttiContext.GetType(AObject.ClassType);

  if GetTypeSerializers.ContainsKey(lObjType.Handle) then
  begin
    GetTypeSerializers.Items[lObjType.Handle].SerializeRoot(AObject, TObject(LJObj), []);
    try
      Result := LJObj.ToJSON(True);
    finally
      LJObj.Free;
    end;
    Exit;
  end;

  LJObj := TJDOJsonObject.Create;
  try
    if Assigned(ASerializationAction) then
    begin
      lDict := TJDOLinks.Create;
      InternalObjectToJsonObject(AObject, LJObj, GetSerializationType(AObject, AType), AIgnoredAttributes,
        ASerializationAction, lDict, fStringDictionarySerializer);
    end
    else
    begin
      InternalObjectToJsonObject(AObject, LJObj, GetSerializationType(AObject, AType), AIgnoredAttributes, nil,
        nil, nil);
    end;
    Result := LJObj.ToJSON(True);
  finally
    LJObj.Free;
  end;
end;

function TMVCJsonDataObjectsSerializer.SerializeObject(const AObject: IInterface;
  const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList; const ASerializationAction: TMVCSerializationAction): string;
var
  LIgnoredAttrs: TList<string>;
begin
  if not Assigned(AObject) then
    Exit('null');

  LIgnoredAttrs := TList<string>.Create;
  try
    LIgnoredAttrs.AddRange(AIgnoredAttributes);
    // if Assigned(GetRttiContext.GetType(TObject(AObject).ClassType).GetProperty('RefCount')) then
    // LIgnoredAttrs.Add('RefCount');
    Result := SerializeObject(TObject(AObject), AType, TMVCIgnoredList(LIgnoredAttrs.ToArray),
      ASerializationAction);
  finally
    LIgnoredAttrs.Free;
  end;
end;

function TMVCJsonDataObjectsSerializer.SerializeObjectToJSON(const AObject: TObject;
  const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList; const ASerializationAction: TMVCSerializationAction)
  : TJDOJsonObject;
var
  JSONObject: TJDOJsonObject;
  ObjType: TRttiType;
begin
  if not Assigned(AObject) then
    Exit(TJDOJsonObject.Create);

  if AObject is TJsonBaseObject then
    Exit(TJsonBaseObject(AObject).Clone as TJDOJsonObject);

  if AObject is TDataSet then
  begin
    raise Exception.Create('Not supported yet');
  end;

  if AObject is TJsonValue then
  begin
    Exit(TJDOJsonObject.Parse(TJsonValue(AObject).ToJSON) as TJDOJsonObject);
  end;

  ObjType := GetRttiContext.GetType(AObject.ClassType);

  if GetTypeSerializers.ContainsKey(ObjType.Handle) then
  begin
    GetTypeSerializers.Items[ObjType.Handle].SerializeRoot(AObject, TObject(JSONObject), []);
    try
      Result := JSONObject;
    except
      JSONObject.Free;
      raise;
    end;
    Exit;
  end;

  Result := TJDOJsonObject.Create;
  try
    ObjectToJsonObject(AObject, Result, GetSerializationType(AObject, AType), AIgnoredAttributes);
  except
    Result.Free;
    raise;
  end;
end;

function TMVCJsonDataObjectsSerializer.TryMapNullableFloat(var Value: TValue;
  const JSONDataObject: TJsonObject; const AttribName: string): Boolean;
begin
  Result := True;
  if Value.TypeInfo = TypeInfo(NullableSingle) then
    Value := TValue.From<NullableSingle>(NullableSingle(JSONDataObject[AttribName].FloatValue))
  else if Value.TypeInfo = TypeInfo(NullableCurrency) then
    Value := TValue.From<NullableCurrency>(NullableCurrency(JSONDataObject[AttribName].FloatValue))
  else if Value.TypeInfo = TypeInfo(NullableDouble) then
    Value := TValue.From<NullableDouble>(NullableDouble(JSONDataObject[AttribName].FloatValue))
  else if Value.TypeInfo = TypeInfo(NullableExtended) then
    Value := TValue.From<NullableExtended>(NullableExtended(JSONDataObject[AttribName].FloatValue))
  else
    Result := False;
end;

function TMVCJsonDataObjectsSerializer.TryNullableToJSON(const AValue: TValue;
  const AJsonObject: TJDOJsonObject;
  const AName: string): Boolean;
begin
  Result := False;
  if (AValue.TypeInfo = System.TypeInfo(NullableString)) then
  begin
    if AValue.AsType<NullableString>().HasValue then
    begin
      AJsonObject.S[AName] := AValue.AsType<NullableString>().Value;
    end
    else
    begin
      AJsonObject.Values[AName] := nil;
    end;
    Exit(True);
  end;

  if (AValue.TypeInfo = System.TypeInfo(NullableInt32)) then
  begin
    if AValue.AsType<NullableInt32>().HasValue then
    begin
      AJsonObject.I[AName] := AValue.AsType<NullableInt32>().Value;
    end
    else
    begin
      AJsonObject.Values[AName] := nil;
    end;
    Exit(True);
  end;

  if (AValue.TypeInfo = System.TypeInfo(NullableInt64)) then
  begin
    if AValue.AsType<NullableInt64>().HasValue then
    begin
      AJsonObject.L[AName] := AValue.AsType<NullableInt64>().Value;
    end
    else
    begin
      AJsonObject.Values[AName] := nil;
    end;
    Exit(True);
  end;

  if (AValue.TypeInfo = System.TypeInfo(NullableInt16)) then
  begin
    if AValue.AsType<NullableInt16>().HasValue then
    begin
      AJsonObject.I[AName] := AValue.AsType<NullableInt16>().Value;
    end
    else
    begin
      AJsonObject.Values[AName] := nil;
    end;
    Exit(True);
  end;

  if (AValue.TypeInfo = System.TypeInfo(NullableTDate)) then
  begin
    if AValue.AsType<NullableTDate>().HasValue then
    begin
      AJsonObject.S[AName] := DateToISODate(AValue.AsType<NullableTDate>().Value);
    end
    else
    begin
      AJsonObject.Values[AName] := nil;
    end;
    Exit(True);
  end;

  if (AValue.TypeInfo = System.TypeInfo(NullableTDateTime)) then
  begin
    if AValue.AsType<NullableTDateTime>().HasValue then
    begin
      AJsonObject.S[AName] := DateTimeToISOTimeStamp(AValue.AsType<NullableTDateTime>().Value);
    end
    else
    begin
      AJsonObject.Values[AName] := nil;
    end;
    Exit(True);
  end;

  if (AValue.TypeInfo = System.TypeInfo(NullableTTime)) then
  begin
    if AValue.AsType<NullableTTime>().HasValue then
    begin
      AJsonObject.S[AName] := TimeToISOTime(AValue.AsType<NullableTTime>().Value);
    end
    else
    begin
      AJsonObject.Values[AName] := nil;
    end;
    Exit(True);
  end;

  if (AValue.TypeInfo = System.TypeInfo(NullableBoolean)) then
  begin
    if AValue.AsType<NullableBoolean>().HasValue then
    begin
      AJsonObject.B[AName] := AValue.AsType<NullableBoolean>().Value;
    end
    else
    begin
      AJsonObject.Values[AName] := nil;
    end;
    Exit(True);
  end;

  if (AValue.TypeInfo = System.TypeInfo(NullableCurrency)) then
  begin
    if AValue.AsType<NullableCurrency>().HasValue then
    begin
      AJsonObject.F[AName] := AValue.AsType<NullableCurrency>().Value;
    end
    else
    begin
      AJsonObject.Values[AName] := nil;
    end;
    Exit(True);
  end;

  if (AValue.TypeInfo = System.TypeInfo(NullableSingle)) then
  begin
    if AValue.AsType<NullableSingle>().HasValue then
    begin
      AJsonObject.F[AName] := AValue.AsType<NullableSingle>().Value;
    end
    else
    begin
      AJsonObject.Values[AName] := nil;
    end;
    Exit(True);
  end;

  if (AValue.TypeInfo = System.TypeInfo(NullableDouble)) then
  begin
    if AValue.AsType<NullableDouble>().HasValue then
    begin
      AJsonObject.F[AName] := AValue.AsType<NullableDouble>().Value;
    end
    else
    begin
      AJsonObject.Values[AName] := nil;
    end;
    Exit(True);
  end;

  if (AValue.TypeInfo = System.TypeInfo(NullableExtended)) then
  begin
    if AValue.AsType<NullableExtended>().HasValue then
    begin
      AJsonObject.F[AName] := AValue.AsType<NullableExtended>().Value;
    end
    else
    begin
      AJsonObject.Values[AName] := nil;
    end;
    Exit(True);
  end;

  { from here all nullable integers }
  if (AValue.TypeInfo = System.TypeInfo(NullableUInt16)) then
  begin
    if AValue.AsType<NullableUInt16>().HasValue then
    begin
      AJsonObject.I[AName] := AValue.AsType<NullableUInt16>().Value;
    end
    else
    begin
      AJsonObject.Values[AName] := nil;
    end;
    Exit(True);
  end;

  if (AValue.TypeInfo = System.TypeInfo(NullableUInt32)) then
  begin
    if AValue.AsType<NullableUInt32>().HasValue then
    begin
      AJsonObject.I[AName] := AValue.AsType<NullableUInt32>().Value;
    end
    else
    begin
      AJsonObject.Values[AName] := nil;
    end;
    Exit(True);
  end;

  if (AValue.TypeInfo = System.TypeInfo(NullableUInt64)) then
  begin
    if AValue.AsType<NullableUInt64>().HasValue then
    begin
      AJsonObject.I[AName] := AValue.AsType<NullableUInt64>().Value;
    end
    else
    begin
      AJsonObject.Values[AName] := nil;
    end;
    Exit(True);
  end;

end;

procedure TMVCJsonDataObjectsSerializer.DeserializeObject(const ASerializedObject: string;
  const AObject: TObject;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);
var
  JSONObject: TJDOJsonObject;
  JsonBase: TJsonBaseObject;
begin
  if (ASerializedObject = EmptyStr) then
    raise EMVCException.Create(HTTP_STATUS.BadRequest, 'Invalid body');

  if not Assigned(AObject) then
    Exit;

  try
    JsonBase := TJDOJsonObject.Parse(ASerializedObject);
    if not(JsonBase is TJDOJsonObject) then
    begin
      raise EMVCSerializationException.CreateFmt('Invalid JSON. Expected %s got %s',
        [TJDOJsonObject.ClassName, JsonBase.ClassName]);
    end;
    JSONObject := TJDOJsonObject(JsonBase);
  except
    on E: EJsonParserException do
    begin
      raise EMVCException.Create(HTTP_STATUS.BadRequest, E.Message);
    end;
  end;
  try
    if GetTypeSerializers.ContainsKey(AObject.ClassInfo) then
    begin
      GetTypeSerializers.Items[AObject.ClassInfo].DeserializeRoot(JSONObject, AObject, []);
    end
    else
    begin
      JsonObjectToObject(JSONObject, AObject, GetSerializationType(AObject, AType), AIgnoredAttributes);
    end;
  finally
    JSONObject.Free;
  end;
end;

function TMVCJsonDataObjectsSerializer.GetDataSetFields(const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase = ncAsIs): TMVCDataSetFields;
var
  I: Integer;
  lField: TMVCDataSetField;
begin
  Result := TMVCDataSetFields.Create;
  for I := 0 to ADataSet.Fields.Count - 1 do
  begin
    { gets the name as defined by NameAs attribute }
    lField.FieldName := GetNameAs(ADataSet.Owner, ADataSet.Fields[I].Name, ADataSet.Fields[I].FieldName);
    { apply the name case to the field name }
    lField.FieldName := TMVCSerializerHelper.ApplyNameCase(ANameCase, lField.FieldName);
    lField.DataType := ADataSet.Fields[I].DataType;
    lField.I := I;
    if (not IsIgnoredAttribute(AIgnoredFields, ADataSet.Fields[I].FieldName)) and
      (not IsIgnoredComponent(ADataSet.Owner, ADataSet.Fields[I].Name)) then
      Result.Add(lField);
  end;
end;

procedure TValueToJsonElement(const Value: TValue; const JSON: TJDOJsonObject; const KeyName: string);
var
  lSer: TMVCJsonDataObjectsSerializer;
  lMVCList: IMVCList;
  lOrdinalValue: Int64;
  lValueAsObj: TObject;
  lValueAsObjQualifClassName, lTypeName: string;
begin
  if Value.IsEmpty then
  begin
    JSON.Values[KeyName] := nil;
    Exit;
  end;

  case Value.Kind of
    tkInteger:
      begin
        JSON.I[KeyName] := Value.AsInteger;
      end;
    tkFloat:
      begin
{$IFDEF NEXTGEN}
        lTypeName := PChar(Pointer(Value.TypeInfo.Name))
{$ELSE}
        lTypeName := String(Value.TypeInfo.Name);
{$ENDIF}
        if (lTypeName = 'TDate') or (lTypeName = 'TDateTime')  or (lTypeName = 'TTime') then
        begin
          JSON.D[KeyName] := Value.AsExtended;
        end
        else
        begin
          JSON.F[KeyName] := Value.AsExtended;
        end;
      end;
    tkString, tkUString, tkWChar, tkLString, tkWString:
      begin
        JSON.S[KeyName] := Value.AsString;
      end;
    tkInt64:
      begin
        JSON.I[KeyName] := Value.AsInt64;
      end;
    tkEnumeration:
      begin
        Value.TryAsOrdinal(lOrdinalValue);
        JSON.I[KeyName] := lOrdinalValue;
      end;
    tkClass, tkInterface:
      begin
        if Value.Kind = tkInterface then
          lValueAsObj := TObject(Value.AsInterface)
        else
          lValueAsObj := Value.AsObject;
        lValueAsObjQualifClassName := lValueAsObj.QualifiedClassName.ToLower;
        if (lValueAsObj is TJDOJsonObject) or (lValueAsObj is TJsonObject)
{$IFDEF RIOORBETTER} or
        { this is for a bug in delphi103rio }
          (lValueAsObjQualifClassName = 'jsondataobjects.tjsonobject') or
        { this is for a bug in delphi103rio }
          (lValueAsObj.QualifiedClassName = 'jsondataobjects.tjdojsonobject')
{$ENDIF}
        then
        begin
          JSON.O[KeyName] := TJDOJsonObject.Create;
          JSON.O[KeyName].Assign(TJDOJsonObject(Value.AsObject));
        end
        else if (lValueAsObj is TJDOJsonArray) or (lValueAsObj is TJsonArray)
{$IFDEF RIOORBETTER} or
        { this is for a bug in delphi103rio }
          (lValueAsObj.QualifiedClassName = 'jsondataobjects.tjsonarray') or
        { this is for a bug in delphi103rio }
          (lValueAsObj.QualifiedClassName = 'jsondataobjects.tjdojsonarray')
{$ENDIF}
        then
        begin
          JSON.A[KeyName] := TJDOJsonArray.Create;
          JSON.A[KeyName].Assign(TJDOJsonArray(Value.AsObject));
        end
        else if lValueAsObj is TDataSet then
        begin
          lSer := TMVCJsonDataObjectsSerializer.Create;
          try
            JSON.A[KeyName] := TJDOJsonArray.Create;
            lSer.DataSetToJsonArray(TDataSet(lValueAsObj), JSON.A[KeyName], TMVCNameCase.ncLowerCase, []);
          finally
            lSer.Free;
          end;
        end
        else if TDuckTypedList.CanBeWrappedAsList(lValueAsObj, lMVCList) then
        begin
          lSer := TMVCJsonDataObjectsSerializer.Create;
          try
            JSON.A[KeyName] := TJDOJsonArray.Create;
            lSer.ListToJsonArray(lMVCList, JSON.A[KeyName], TMVCSerializationType.stDefault, nil);
          finally
            lSer.Free;
          end;
        end
        else
        begin
          lSer := TMVCJsonDataObjectsSerializer.Create;
          try
            JSON.O[KeyName] := lSer.SerializeObjectToJSON(lValueAsObj, TMVCSerializationType.stProperties, [], nil);
          finally
            lSer.Free;
          end;
        end;
      end;
  else
    raise EMVCException.Create('Invalid type');
  end;
end;

function StrToJSONObject(const AValue: string): TJDOJsonObject;
var
  lJSON: TJDOJsonObject;
begin
  lJSON := nil;
  try
    lJSON := TJDOJsonObject.Parse(AValue) as TJDOJsonObject;
    Result := lJSON;
  except
    on E: Exception do
    begin
      lJSON.Free;
      raise EMVCDeserializationException.Create('Invalid JSON Object');
    end;
  end;
end;

function StrToJSONArray(const AValue: string): TJDOJsonArray;
var
  lJSON: TJDOJsonArray;
begin
  lJSON := nil;
  try
    lJSON := TJDOJsonObject.Parse(AValue) as TJDOJsonArray;
    Result := lJSON;
  except
    on E: Exception do
    begin
      lJSON.Free;
      raise EMVCDeserializationException.Create('Invalid JSON Array');
    end;
  end;
end;

procedure JsonObjectToObject(const AJsonObject: TJDOJsonObject; const AObject: TObject;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);
var
  lSer: TMVCJsonDataObjectsSerializer;
begin
  lSer := TMVCJsonDataObjectsSerializer.Create;
  try
    lSer.JsonObjectToObject(AJsonObject, AObject, AType, AIgnoredAttributes);
  finally
    lSer.Free;
  end;
end;

procedure MVCStringDictionaryListToJSONArray(const aStringDictionaryList: TMVCStringDictionaryList;
  const AJsonArray: TJsonArray);
var
  lStringDictionary: TMVCStringDictionary;
begin
  if aStringDictionaryList = nil then
    Exit;
  for lStringDictionary in aStringDictionaryList do
  begin
    TMVCStringDictionarySerializer.Serialize(lStringDictionary, AJsonArray.AddObject);
  end;
end;

{ TJDOLinks }

procedure TJDOLinks.FillJSONArray(const AJsonArray: TJsonArray);
begin
  MVCStringDictionaryListToJSONArray(LinksData, AJsonArray);
end;

{ TJSONObjectHelper }

procedure TJSONObjectHelper.LoadFromString(const Value: string; Encoding: TEncoding;
  Utf8WithoutBOM: Boolean);
var
  lSS: TStringStream;
begin
  if Assigned(Encoding) then
  begin
    lSS := TStringStream.Create(Value, Encoding);
  end
  else
  begin
    lSS := TStringStream.Create(Value);
  end;
  try
    lSS.Position := 0;
    LoadFromStream(lSS, Encoding, Utf8WithoutBOM);
  finally
    lSS.Free;
  end;
end;

end.
