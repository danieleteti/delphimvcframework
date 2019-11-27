// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2019 Daniele Teti and the DMVCFramework Team
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
  MVCFramework.Commons,
  MVCFramework.Serializer.Intf,
  MVCFramework.Serializer.Abstract,
  MVCFramework.Serializer.Commons,
  MVCFramework.DuckTyping,
  System.JSON,
  JsonDataObjects, System.SysUtils;

type

  TMVCDataSetField = record
    FieldName: string;
    DataType: TFieldType;
    I: Integer;
  end;

  TMVCDataSetFieldSerializationAction = reference to procedure(const AField: TField; const AJsonObject: TJsonObject;
    var Handled: Boolean);

  TMVCDataSetFields = TList<TMVCDataSetField>;

  TJSONObjectHelper = class helper for TJsonObject
  public
    procedure LoadFromString(const Value: String; Encoding: TEncoding = nil; Utf8WithoutBOM: Boolean = True);
  end;

  TMVCJsonDataObjectsSerializer = class(TMVCAbstractSerializer, IMVCSerializer)
  private
    fStringDictionarySerializer: IMVCTypeSerializer;
  public
    function GetDataSetFields(const ADataSet: TDataSet; const AIgnoredFields: TMVCIgnoredList = [];
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

    function SerializeDataSet(const ADataSet: TDataSet; const AIgnoredFields: TMVCIgnoredList = [];
      const ANameCase: TMVCNameCase = ncAsIs; const ASerializationAction: TMVCDatasetSerializationAction = nil): string;

    function SerializeDataSetRecord(const ADataSet: TDataSet; const AIgnoredFields: TMVCIgnoredList = [];
      const ANameCase: TMVCNameCase = ncAsIs; const ASerializationAction: TMVCDatasetSerializationAction = nil): string;

    procedure DeserializeObject(const ASerializedObject: string; const AObject: TObject;
      const AType: TMVCSerializationType = stDefault; const AIgnoredAttributes: TMVCIgnoredList = []); overload;

    procedure DeserializeObject(const ASerializedObject: string; const AObject: IInterface;
      const AType: TMVCSerializationType = stDefault; const AIgnoredAttributes: TMVCIgnoredList = []); overload;

    procedure DeserializeCollection(const ASerializedList: string; const AList: TObject; const AClazz: TClass;
      const AType: TMVCSerializationType = stDefault; const AIgnoredAttributes: TMVCIgnoredList = []); overload;

    procedure DeserializeCollection(const ASerializedList: string; const AList: IInterface; const AClazz: TClass;
      const AType: TMVCSerializationType = stDefault; const AIgnoredAttributes: TMVCIgnoredList = []); overload;

    procedure DeserializeDataSet(const ASerializedDataSet: string; const ADataSet: TDataSet;
      const AIgnoredFields: TMVCIgnoredList = []; const ANameCase: TMVCNameCase = ncAsIs);

    procedure DeserializeDataSetRecord(const ASerializedDataSetRecord: string; const ADataSet: TDataSet;
      const AIgnoredFields: TMVCIgnoredList = []; const ANameCase: TMVCNameCase = ncAsIs);
    class function ParseObject(const AString: string): TJDOJsonObject;
    class function ParseArray(const AString: string): TJDOJsonArray;
    class function Parse<T: TJsonBaseObject>(const AString: string): T;
  public
    procedure AfterConstruction; override;
  end;

procedure TValueToJsonElement(const Value: TValue; const JSON: TJDOJsonObject; const KeyName: string);
function StrToJSONObject(const AValue: string): TJDOJsonObject;
procedure JsonObjectToObject(const AJsonObject: TJDOJsonObject; const AObject: TObject;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);

implementation

uses
  MVCFramework.Serializer.JsonDataObjects.CustomTypes,
  MVCFramework.Logger, MVCFramework.DataSet.Utils;

type
  TJDOLinks = class(TMVCLinks)
  public
    procedure FillJSONArray(const AJsonArray: TJsonArray);
  end;

  { TMVCJsonDataObjectsSerializer }

procedure TMVCJsonDataObjectsSerializer.AfterConstruction;
var
  lStreamSerializer: IMVCTypeSerializer;
  lDataSetHolderSerializer: TMVCDataSetHolderSerializer;
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
  GetTypeSerializers.Add(TypeInfo(TMVCStringDictionary), TMVCStringDictionarySerializer.Create);
  GetTypeSerializers.Add(TypeInfo(TGUID), TMVCGUIDSerializer.Create);
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
  LEnumAsAttr: MVCEnumSerializationTypeAttribute;
  LEnumSerType: TMVCEnumSerializationType;
  LEnumPrefix: string;
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
          LEnumPrefix := '';
          if TMVCSerializerHelper.AttributeExists<MVCEnumSerializationTypeAttribute>(ACustomAttributes, LEnumAsAttr)
          then
          begin
            LEnumSerType := LEnumAsAttr.EnumSerializationType;
            LEnumPrefix := LEnumAsAttr.EnumPrefix;
          end;

          case LEnumSerType of
            estEnumName:
              begin
                LEnumName := GetEnumName(AValue.TypeInfo, AValue.AsOrdinal);
                if not LEnumPrefix.IsEmpty and LEnumName.StartsWith(LEnumPrefix) then
                  LEnumName := LEnumName.Remove(0, LEnumPrefix.Length);

                AJsonObject.S[AName] := LEnumName;
              end;
            estEnumOrd:
              begin
                AJsonObject.I[AName] := AValue.AsOrdinal;
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
        Begin
          for I := 0 to AValue.GetArrayLength - 1 do
          Begin
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
          End;
        End;
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

      if ADataSet.Fields[lField.I].IsNull then
        AJsonObject[lField.FieldName] := Null
      else
      begin
        case lField.DataType of
          ftBoolean:
            AJsonObject.B[lField.FieldName] := ADataSet.Fields[lField.I].AsBoolean;

          ftInteger, ftSmallint, ftShortint, ftByte:
            AJsonObject.I[lField.FieldName] := ADataSet.Fields[lField.I].AsInteger;

          ftLargeint, ftAutoInc, ftLongword:
            AJsonObject.L[lField.FieldName] := ADataSet.Fields[lField.I].AsLargeInt;
{$IFDEF TOKYOORBETTER}
          ftGuid:
            AJsonObject.S[lField.FieldName] := GUIDToString(ADataSet.Fields[lField.I].AsGuid);
{$ENDIF}
          ftSingle, ftFloat:
            AJsonObject.F[lField.FieldName] := ADataSet.Fields[lField.I].AsFloat;

          ftString, ftMemo:
            AJsonObject.S[lField.FieldName] := ADataSet.Fields[lField.I].AsString;

          ftWideString, ftWideMemo:
            AJsonObject.S[lField.FieldName] := ADataSet.Fields[lField.I].AsWideString;

          ftDate:
            AJsonObject.S[lField.FieldName] := DateToISODate(ADataSet.Fields[lField.I].AsDateTime);

          ftDateTime:
            AJsonObject.S[lField.FieldName] := DateTimeToISOTimeStamp(ADataSet.Fields[lField.I].AsDateTime);

          ftTime:
            AJsonObject.S[lField.FieldName] := SQLTimeStampToStr('hh:nn:ss', ADataSet.Fields[lField.I].AsSQLTimeStamp);

          ftTimeStamp:
            AJsonObject.S[lField.FieldName] :=
              DateTimeToISOTimeStamp(SQLTimeStampToDateTime(ADataSet.Fields[lField.I].AsSQLTimeStamp));

          ftCurrency:
            AJsonObject.F[lField.FieldName] := ADataSet.Fields[lField.I].AsCurrency;

          ftFMTBcd, ftBCD:
            AJsonObject.F[lField.FieldName] := BcdToDouble(ADataSet.Fields[lField.I].AsBcd);

          ftGraphic, ftBlob, ftStream, ftOraBlob:
            begin
              lMS := TMemoryStream.Create;
              try
                TBlobField(ADataSet.Fields[lField.I]).SaveToStream(lMS);
                lMS.Position := 0;
                lSS := TStringStream.Create;
                try
                  TMVCSerializerHelper.EncodeStream(lMS, lSS);
                  AJsonObject.S[lField.FieldName] := lSS.DataString;
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
    raise EMVCException.Create(http_status.BadRequest, 'Invalid body');

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
    raise EMVCException.Create(http_status.BadRequest, 'Invalid body');

  if not Assigned(ADataSet) then
    Exit;

  try
    lJsonArray := TJDOJsonArray.Parse(ASerializedDataSet) as TJDOJsonArray;
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
  lJsonBase: TJDOJsonBaseObject;
begin
  if (ASerializedDataSetRecord = EmptyStr) or (not Assigned(ADataSet)) then
    Exit;

  lJsonBase := TJDOJsonObject.Parse(ASerializedDataSetRecord);
  try
    if lJsonBase is TJsonObject then
    begin
      ADataSet.Edit;
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
  lStrArr: TArray<String>;
  lIntArr: TArray<Integer>;
begin
  for I := 0 to Pred(AJsonArray.Count) do
    case AJsonArray.types[0] of
      jdtString:
        lStrArr := lStrArr + [AJsonArray.Items[I].Value];
      jdtInt:
        lIntArr := lIntArr + [AJsonArray.Items[I].Value.ToInteger];
    end;

  if Length(lStrArr) > 0 then
    result := TValue.From < TArray < String >> (lStrArr)
  else
    result := TValue.From < TArray < Integer >> (lIntArr);
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
  LEnumAsAttr: MVCEnumSerializationTypeAttribute;
  LEnumPrefix: string;
  LClazz: TClass;
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

        else if (AValue.Kind = tkEnumeration) then
        begin
          LEnumPrefix := '';
          if TMVCSerializerHelper.AttributeExists<MVCEnumSerializationTypeAttribute>(ACustomAttributes, LEnumAsAttr)
          then
            LEnumPrefix := LEnumAsAttr.EnumPrefix;

          TValue.Make(GetEnumValue(AValue.TypeInfo, LEnumPrefix + AJsonObject[AName].Value), AValue.TypeInfo, AValue)
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
        else
        begin
          AValue := TValue.From<Integer>(AJsonObject[AName].IntValue);
        end;
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
            if AValue.Kind = tkInterface then
              ChildObject := TObject(AValue.AsInterface)
            else
              ChildObject := AValue.AsObject;
            JsonObjectToObject(AJsonObject.O[AName], ChildObject, GetSerializationType(ChildObject, AType), AIgnored);
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
                ('You can not deserialize a list %s without the MVCListOf attribute.', [AName]);
          end;
        end
        else if AValue.isArray then
        begin
          AValue := JsonArrayToArray(AJsonObject.A[AName]);
        end;
      end;
  end;
end;

procedure TMVCJsonDataObjectsSerializer.JsonObjectToDataSet(const AJsonObject: TJDOJsonObject; const ADataSet: TDataSet;
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
        continue;

      case GetNameCase(ADataSet, ANameCase) of
        ncLowerCase:
          name := LowerCase(Field.FieldName);
        ncUpperCase:
          name := UpperCase(Field.FieldName);
      end;

      if not AJsonObject.Contains(name) then
        continue;

      if (AJsonObject[name].Typ = jdtObject) and (AJsonObject.Values[name].ObjectValue = nil) then
      // Nullable Type
      begin
        Field.Clear;
        continue;
      end;

      case Field.DataType of
        TFieldType.ftBoolean:
          Field.AsBoolean := AJsonObject.B[name];

        TFieldType.ftInteger, TFieldType.ftSmallint, TFieldType.ftShortint, TFieldType.ftByte, TFieldType.ftLongWord, TFieldType.ftWord, TFieldType.ftAutoInc:
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

procedure TMVCJsonDataObjectsSerializer.JsonObjectToObject(const AJsonObject: TJDOJsonObject; const AObject: TObject;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);
var
  ObjType: TRttiType;
  Prop: TRttiProperty;
  Fld: TRttiField;
  AttributeValue: TValue;
  lKeyName: string;
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

  ObjType := GetRttiContext.GetType(AObject.ClassType);
  case AType of
    stDefault, stProperties:
      begin
        try
          for Prop in ObjType.GetProperties do
          begin

{$IFDEF AUTOREFCOUNT}
            if TMVCSerializerHelper.IsAPropertyToSkip(Prop.Name) then
              continue;

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

procedure TMVCJsonDataObjectsSerializer.ListToJsonArray(const AList: IMVCList; const AJsonArray: TJDOJsonArray;
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
      InternalObjectToJsonObject(AList.GetItem(I), AJsonArray.AddObject, AType, AIgnoredAttributes, nil, nil, nil);
    end;
  end;
end;

procedure TMVCJsonDataObjectsSerializer.ObjectToJsonObject(const AObject: TObject; const AJsonObject: TJDOJsonObject;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);
begin
  InternalObjectToJsonObject(AObject, AJsonObject, AType, AIgnoredAttributes, nil, nil, nil);
end;

procedure TMVCJsonDataObjectsSerializer.InternalObjectToJsonObject(const AObject: TObject;
  const AJsonObject: TJDOJsonObject; const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList;
  const ASerializationAction: TMVCSerializationAction; const Links: IMVCLinks; const Serializer: IMVCTypeSerializer);
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
            AttributeToJsonDataValue(AJsonObject, TMVCSerializerHelper.GetKeyName(Fld, ObjType), Fld.GetValue(AObject),
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

class function TMVCJsonDataObjectsSerializer.Parse<T>(const AString: string): T;
begin
  result := TJDOJsonObject.Parse(AString) as T;
  if not Assigned(result) then
    raise EMVCDeserializationException.Create('Cannot parse string as ' + T.ClassName);
end;

class function TMVCJsonDataObjectsSerializer.ParseArray(const AString: string): TJDOJsonArray;
begin
  result := Parse<TJDOJsonArray>(AString);
end;

class function TMVCJsonDataObjectsSerializer.ParseObject(const AString: string): TJDOJsonObject;
begin
  result := Parse<TJDOJsonObject>(AString);
end;

function TMVCJsonDataObjectsSerializer.SerializeCollection(const AList: TObject; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList; const ASerializationAction: TMVCSerializationAction): string;
var
  JsonArray: TJDOJsonArray;
  ObjList: IMVCList;
  Obj: TObject;
  lLinks: IMVCLinks;
  lSer: IMVCTypeSerializer;
begin
  result := EmptyStr;

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
      result := JsonArray.ToJSON(True);
    finally
      JsonArray.Free;
    end;
  end;
end;

function TMVCJsonDataObjectsSerializer.SerializeCollection(const AList: IInterface; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList; const ASerializationAction: TMVCSerializationAction): string;
begin
  result := SerializeCollection(TObject(AList), AType, AIgnoredAttributes, ASerializationAction);
end;

function TMVCJsonDataObjectsSerializer.SerializeDataSet(const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList = []; const ANameCase: TMVCNameCase = ncAsIs;
  const ASerializationAction: TMVCDatasetSerializationAction = nil): string;
var
  JsonArray: TJDOJsonArray;
  BookMark: TBookmark;
  lNameCase: TMVCNameCase;
  lLinks: IMVCLinks;
  LJObj: TJsonObject;
  lDataSetFields: TMVCDataSetFields;
begin
  result := EmptyStr;

  if (not Assigned(ADataSet)) then
    Exit('null');
  if ADataSet.IsEmpty then
    Exit('[]'); // https://github.com/danieleteti/delphimvcframework/issues/219

  lLinks := nil;
  if Assigned(ASerializationAction) then
  begin
    lLinks := TJDOLinks.Create;
  end;
  lDataSetFields := GetDataSetFields(ADataSet, AIgnoredFields, ANameCase);
  try
    JsonArray := TJDOJsonArray.Create;
    try
      BookMark := ADataSet.BookMark;
      lNameCase := GetNameCase(ADataSet, ANameCase);
      ADataSet.First;
      while not ADataSet.Eof do
      begin
        LJObj := JsonArray.AddObject;
        DataSetToJsonObject(ADataSet, LJObj, lNameCase, AIgnoredFields, lDataSetFields);
        if Assigned(ASerializationAction) then
        begin
          lLinks.Clear;
          ASerializationAction(ADataSet, lLinks);
          TJDOLinks(lLinks).FillJSONArray(LJObj.A[TMVCConstants.HATEOAS_PROP_NAME]);
        end;
        ADataSet.Next;
      end;
      result := JsonArray.ToJSON(True);
    finally
      JsonArray.Free;
      if ADataSet.BookmarkValid(BookMark) then
        ADataSet.GotoBookmark(BookMark);
      ADataSet.FreeBookmark(BookMark);
    end;
  finally
    lDataSetFields.Free;
  end;
end;

function TMVCJsonDataObjectsSerializer.SerializeDataSetRecord(const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList = []; const ANameCase: TMVCNameCase = ncAsIs;
  const ASerializationAction: TMVCDatasetSerializationAction = nil): string;
var
  JsonObject: TJDOJsonObject;
  lDataSetFields: TMVCDataSetFields;
  lLinks: IMVCLinks;
begin
  result := EmptyStr;
  if (not Assigned(ADataSet)) or ADataSet.IsEmpty then
    Exit('null');

  lDataSetFields := GetDataSetFields(ADataSet, AIgnoredFields, GetNameCase(ADataSet, ANameCase));
  try
    JsonObject := TJDOJsonObject.Create;
    try
      DataSetToJsonObject(ADataSet, JsonObject, GetNameCase(ADataSet, ANameCase), AIgnoredFields, lDataSetFields);
      lLinks := TJDOLinks.Create;
      if Assigned(ASerializationAction) then
      begin
        ASerializationAction(ADataSet, lLinks);
        TJDOLinks(lLinks).FillJSONArray(JsonObject.A[TMVCConstants.HATEOAS_PROP_NAME]);
      end;
      result := JsonObject.ToJSON(True);
    finally
      JsonObject.Free;
    end;
  finally
    lDataSetFields.Free;
  end;
end;

function TMVCJsonDataObjectsSerializer.SerializeObject(const AObject: TObject; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList; const ASerializationAction: TMVCSerializationAction): string;
var
  LJObj: TJDOJsonObject;
  lObjType: TRttiType;
  lDict: IMVCLinks;
begin
  result := EmptyStr;

  if not Assigned(AObject) then
    Exit('null');

  if AObject is TJsonBaseObject then
    Exit(TJsonBaseObject(AObject).ToJSON(True));

  if AObject is TDataSet then
    Exit(self.SerializeDataSet(TDataSet(AObject)));

  if AObject is System.JSON.TJsonValue then
    Exit(System.JSON.TJsonValue(AObject).ToJSON);

  lObjType := GetRttiContext.GetType(AObject.ClassType);

  if GetTypeSerializers.ContainsKey(lObjType.Handle) then
  begin
    GetTypeSerializers.Items[lObjType.Handle].SerializeRoot(AObject, TObject(LJObj), []);
    try
      result := LJObj.ToJSON(True);
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
    result := LJObj.ToJSON(True);
  finally
    LJObj.Free;
  end;
end;

function TMVCJsonDataObjectsSerializer.SerializeObject(const AObject: IInterface; const AType: TMVCSerializationType;
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
    result := SerializeObject(TObject(AObject), AType, TMVCIgnoredList(LIgnoredAttrs.ToArray), ASerializationAction);
  finally
    LIgnoredAttrs.Free;
  end;
end;

function TMVCJsonDataObjectsSerializer.SerializeObjectToJSON(const AObject: TObject; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList; const ASerializationAction: TMVCSerializationAction): TJDOJsonObject;
var
  JsonObject: TJDOJsonObject;
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
    GetTypeSerializers.Items[ObjType.Handle].SerializeRoot(AObject, TObject(JsonObject), []);
    try
      result := JsonObject;
    except
      JsonObject.Free;
      raise;
    end;
    Exit;
  end;

  result := TJDOJsonObject.Create;
  try
    ObjectToJsonObject(AObject, result, GetSerializationType(AObject, AType), AIgnoredAttributes);
  except
    result.Free;
    raise;
  end;
end;

procedure TMVCJsonDataObjectsSerializer.DeserializeObject(const ASerializedObject: string; const AObject: TObject;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);
var
  JsonObject: TJDOJsonObject;
  JsonBase: TJsonBaseObject;
begin
  if (ASerializedObject = EmptyStr) then
    raise EMVCException.Create(http_status.BadRequest, 'Invalid body');

  if not Assigned(AObject) then
    Exit;

  try
    JsonBase := TJDOJsonObject.Parse(ASerializedObject);
    if not(JsonBase is TJDOJsonObject) then
    begin
      raise EMVCSerializationException.CreateFmt('Invalid JSON. Expected %s got %s',
        [TJDOJsonObject.ClassName, JsonBase.ClassName]);
    end;
    JsonObject := TJDOJsonObject(JsonBase);
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

function TMVCJsonDataObjectsSerializer.GetDataSetFields(const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList = []; const ANameCase: TMVCNameCase = ncAsIs): TMVCDataSetFields;
var
  I: Integer;
  lField: TMVCDataSetField;
begin
  result := TMVCDataSetFields.Create;
  for I := 0 to ADataSet.Fields.Count - 1 do
  Begin
    lField.FieldName := GetNameAs(ADataSet.Owner, ADataSet.Fields[I].Name, ADataSet.Fields[I].FieldName);
    lField.DataType := ADataSet.Fields[I].DataType;
    lField.I := I;
    case ANameCase of
      ncUpperCase:
        lField.FieldName := UpperCase(ADataSet.Fields[I].FieldName);
      ncLowerCase:
        lField.FieldName := LowerCase(ADataSet.Fields[I].FieldName);
    end;
    if (not IsIgnoredAttribute(AIgnoredFields, lField.FieldName)) and
      (not IsIgnoredComponent(ADataSet.Owner, ADataSet.Fields[I].Name)) then
      result.Add(lField);
  End;
end;

procedure TValueToJsonElement(const Value: TValue; const JSON: TJDOJsonObject; const KeyName: string);
var
  lSer: TMVCJsonDataObjectsSerializer;
  lMVCList: IMVCList;
  lOrdinalValue: Int64;
  lValueAsObj: TObject;
  lValueAsObjQualifClassName: string;
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
        if PChar(Pointer(Value.TypeInfo.Name)) = 'TDate' then
{$ELSE}
        if Value.TypeInfo.Name = 'TDate' then
{$ENDIF}
        begin
          JSON.DUtc[KeyName] := Value.AsExtended;
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
    result := lJSON;
  except
    on E: Exception do
    begin
      lJSON.Free;
      raise EMVCDeserializationException.Create('Invalid JSON');
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

procedure TJSONObjectHelper.LoadFromString(const Value: String; Encoding: TEncoding; Utf8WithoutBOM: Boolean);
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
