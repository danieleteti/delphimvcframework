// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
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

  TMVCJsonDataObjectsSerializer = class(TMVCAbstractSerializer, IMVCSerializer, IMVCJSONSerializer)
  private
    fStringDictionarySerializer: IMVCTypeSerializer;
    function TryMapNullableFloat(var Value: TValue; const JSONDataObject: TJsonObject;
      const AttribName: string): Boolean;
    type
      TFieldMetaInfo = record
        NameAs: String;
        Ignored: Boolean;
      end;
      TSerializationMetaInfo = record
        FieldsMetaInfo: TArray<TFieldMetaInfo>;
        IgnoredFields: TMVCIgnoredList;
        NameCase: TMVCNameCase;
        class function CreateFieldsMetaInfo(
          const ADataSet: TDataSet;
          const ANameCase: TMVCNameCase;
          const AIgnoredFields: TMVCIgnoredList): TSerializationMetaInfo; static;
      end;
  public
    procedure ParseStringAsTValueUsingMetadata(
      const AStringValue: String;
      const DestinationTypeInfo: PTypeInfo;
      const ExceptionHintString: String;
      const AAttributes: TArray<TCustomAttribute>;
      var AValue: TValue);
    function JSONObjectToRecord<T: record >(const JSONObject: TJsonObject): T; overload;
    function StrToRecord<T: record >(const AJSONString: String): T;
    procedure JSONObjectToNestedRecordField(const JSONObject: TJsonObject; RecordFieldRTTIType: TRttiField;
      const TypeOffset: Integer; var Buffer: PByte);
    procedure JSONObjectToNestedRecordFieldStatic(const JSONObject: TJsonObject; RecordFieldRTTIType: TRttiField;
      const TypeOffset: Integer; var Buffer: PByte);
    procedure JSONObjectPropertyToTValueForRecord(AJSONObject: TJsonObject; const APropertyName: String;
      const AType: TMVCSerializationType; const AIgnored: TMVCIgnoredList; var AValue: TValue;
      const ACustomAttributes: TArray<TCustomAttribute>; const ARTTIField: TRttiField);
    function GetDataSetFields(const ADataSet: TDataSet; const AIgnoredFields: TMVCIgnoredList;
      const ANameCase: TMVCNameCase = ncAsIs): TMVCDataSetFields;
    procedure ObjectToJsonObject(const AObject: TObject; const AJSONObject: TJDOJsonObject;
      const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);
    procedure InternalObjectToJsonObject(const AObject: TObject; const AJSONObject: TJDOJsonObject;
      const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList;
      const ASerializationAction: TMVCSerializationAction; const Links: IMVCLinks;
      const Serializer: IMVCTypeSerializer);
    procedure InternalRecordToJsonObject(const ARecord: Pointer; const ARecordTypeInfo: PTypeInfo;
      const AJSONObject: TJDOJsonObject; const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList;
      const ASerializationAction: TMVCSerializationAction; const Links: IMVCLinks;
      const Serializer: IMVCTypeSerializer);
    procedure InternalTValueToJsonObject(const AValue: TValue; const AJSONObject: TJDOJsonObject;
      const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList;
      const ASerializationAction: TMVCSerializationAction; const Links: IMVCLinks;
      const Serializer: IMVCTypeSerializer);
    function ConvertObjectToJsonValue(const AObject: TObject; const AType: TMVCSerializationType;
      const AIgnoredFields: TMVCIgnoredList;
      const ADataSetSerializationCallback: TMVCDataSetFieldSerializationAction;
      const ASerializationAction: TMVCSerializationAction; out AJsonDataType: TJsonDataType): TJsonBaseObject;
    function ConvertRecordToJsonValue(const ARecord: Pointer; const ARecordTypeInfo: PTypeInfo;
      const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList;
      const ADataSetSerializationCallback: TMVCDataSetFieldSerializationAction;
      const ASerializationAction: TMVCSerializationAction; out AJsonDataType: TJsonDataType): TJsonBaseObject;
    procedure AddTValueToJsonArray(const Value: TValue; const JSON: TJDOJsonArray);
    procedure ListToJsonArray(const AList: IMVCList; const AJsonArray: TJDOJsonArray;
      const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList;
      const ASerializationAction: TMVCSerializationAction = nil);
    procedure TValueToJSONObjectProperty(const AJSONObject: TJDOJsonObject; const AName: string; const AValue: TValue;
      const AType: TMVCSerializationType; const AIgnored: TMVCIgnoredList;
      const ACustomAttributes: TArray<TCustomAttribute>);
    function TryNullableToJSON(const AValue: TValue; const AJSONObject: TJDOJsonObject; const AName: string;
      const ACustomAttributes: TArray<TCustomAttribute>): Boolean;
    procedure JsonObjectToObject(const AJSONObject: TJDOJsonObject; const AObject: TObject;
      const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);
    procedure JSONObjectToRecord(const JSONObject: TJsonObject; RTTIType: TRttiRecordType; out Buffer: PByte); overload;
    procedure JSONObjectToRecordStatic(const JSONObject: TJsonObject; RTTIType: TRttiRecordType; var Buffer: PByte);
    procedure JSONObjectPropertyToTValue(AJSONObject: TJsonObject; const APropertyName: String;
      const AType: TMVCSerializationType; const AIgnored: TMVCIgnoredList; var ChildObject: TObject; var AValue: TValue;
      const ACustomAttributes: TArray<TCustomAttribute>);
    procedure JsonDataValueToAttribute(const AObject: TObject; const ARttiMember: TRttiMember;
      const AJSONObject: TJDOJsonObject; const AName: string; var AValue: TValue; const AType: TMVCSerializationType;
      const AIgnored: TMVCIgnoredList; const ACustomAttributes: TArray<TCustomAttribute>);
    procedure JsonArrayToList(const AJsonArray: TJDOJsonArray; const AList: IMVCList; const AClazz: TClass;
      const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);
    procedure DataSetToJsonObject(const ADataSet: TDataSet; const AJSONObject: TJDOJsonObject;
      const ANameCase: TMVCNameCase; const AIgnoredFields: TMVCIgnoredList; const ADataSetFields: TMVCDataSetFields;
      const ASerializationCallback: TMVCDataSetFieldSerializationAction = nil);
    procedure DataSetRowToJsonArrayOfValues(const ADataSet: TDataSet; const AJsonArray: TJDOJsonArray;
      const AIgnoredFields: TMVCIgnoredList; const ADataSetFields: TMVCDataSetFields);
    procedure DataSetToJsonArray(const ADataSet: TDataSet; const AJsonArray: TJDOJsonArray;
      const ANameCase: TMVCNameCase; const AIgnoredFields: TMVCIgnoredList;
      const ASerializationCallback: TMVCDataSetFieldSerializationAction = nil);
    procedure DataSetToJsonArrayOfValues(const ADataSet: TDataSet; const AJsonArray: TJDOJsonArray;
      const AIgnoredFields: TMVCIgnoredList);
    procedure JsonObjectToDataSet(const AJSONObject: TJDOJsonObject; const ADataSet: TDataSet;
      const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase); overload;
    procedure JsonObjectToDataSet(const AJSONObject: TJDOJsonObject; const ADataSet: TDataSet;
      const SerializationMetaInfo: TSerializationMetaInfo); overload;
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

    function SerializeRecord(const ARecord: Pointer; const ARecordTypeInfo: PTypeInfo;
      const AType: TMVCSerializationType = stDefault; const AIgnoredAttributes: TMVCIgnoredList = nil;
      const ASerializationAction: TMVCSerializationAction = nil): string; overload;

    function SerializeArrayOfRecord(
      var ATValueContainingAnArray: TValue;
      const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = nil;
      const ASerializationAction: TMVCSerializationAction = nil
      ): string; overload;

    procedure RecordToJsonObject(const ARecord: Pointer; const ARecordTypeInfo: PTypeInfo;
      const AJSONObject: TJDOJsonObject; const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);

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

    function SerializeDataSetRecord(const DataSet: TDataSet; const IgnoredFields: TMVCIgnoredList;
      const NameCase: TMVCNameCase = ncAsIs; const SerializationAction: TMVCDatasetSerializationAction = nil): string;

    procedure DeserializeObject(const ASerializedObject: string; const AObject: TObject;
      const AType: TMVCSerializationType = stDefault; const AIgnoredAttributes: TMVCIgnoredList = [];
      const ARootNode: string = ''); overload;

    procedure DeserializeObject(const ASerializedObject: string; const AObject: IInterface;
      const AType: TMVCSerializationType = stDefault; const AIgnoredAttributes: TMVCIgnoredList = []); overload;

    procedure DeserializeCollection(const ASerializedList: string; const AList: TObject; const AClazz: TClass;
      const AType: TMVCSerializationType = stDefault; const AIgnoredAttributes: TMVCIgnoredList = [];
      const ARootNode: string = ''); overload;

    procedure DeserializeCollection(const ASerializedList: string; const AList: IInterface; const AClazz: TClass;
      const AType: TMVCSerializationType = stDefault; const AIgnoredAttributes: TMVCIgnoredList = []); overload;

    procedure DeserializeDataSet(const ASerializedDataSet: string; const ADataSet: TDataSet;
      const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase = ncAsIs);

    procedure InternalSerializeDataSet(const ADataSet: TDataSet; const AJsonArray: TJsonArray;
      const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase;
      const ASerializationAction: TMVCDatasetSerializationAction);

    procedure InternalSerializeDataSetRecord(const DataSet: TDataSet; const JSONObject: TJsonObject;
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

  TJSONUtils = record
  private
    class function JSONObjectToRecord<T: record >(const JSONObject: TJsonObject;
      const Serializer: TMVCJsonDataObjectsSerializer): T; overload; static; inline;
  public
    // records
    class function JSONObjectToRecord<T: record >(const JSONObject: TJsonObject): T; overload; static;
    class function JSONArrayToArrayOfRecord<T: record >(const JSONArray: TJsonArray): TArray<T>; overload; static;
    // objects
    class function JsonObjectToObject<T: class, constructor>(const JSONObject: TJsonObject): T; overload; static;
    class function JSONArrayToListOf<T: class, constructor>(const JSONArray: TJsonArray): TObjectList<T>;
      overload; static;
  end;

procedure TValueToJSONObjectPropertyEx(const Value: TValue; const JSON: TJDOJsonObject; const KeyName: string);
function StrToJSONObject(const AValue: string; ARaiseExceptionOnError: Boolean = False): TJDOJsonObject; inline;
function StrToJSONArray(const AValue: string; ARaiseExceptionOnError: Boolean = False): TJDOJsonArray; inline;
procedure JsonObjectToObject(const AJSONObject: TJDOJsonObject; const AObject: TObject;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList); overload;
procedure JsonObjectToObject(const AJSONObject: TJDOJsonObject; const AObject: TObject); overload;
procedure JsonArrayToList(const AJsonArray: TJDOJsonArray; const AList: IMVCList; const AClazz: TClass;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);

const
  JDO_TYPE_DESC: array [TJsonDataType.jdtNone .. TJsonDataType.jdtObject] of string = ('None', 'String', 'Int', 'Long',
    'ULong', 'Float', 'DateTime', 'UtcDateTime', 'Bool', 'Array', 'Object');

implementation

uses
  MVCFramework.Serializer.JsonDataObjects.CustomTypes,
  MVCFramework.Logger,
  MVCFramework.DataSet.Utils,
  MVCFramework.Nullables;

function SelectRootNodeOrWholeObject(const RootNode: string; const JSONObject: TJsonObject): TJsonObject; inline;
begin
  if RootNode.IsEmpty then
  begin
    Result := JSONObject
  end
  else
  begin
    Result := JSONObject.O[RootNode];
  end;
end;

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
  GetTypeSerializers.Add(TypeInfo(TMVCListOfString { TList<string> } ), TMVCListOfStringSerializer.Create);
  GetTypeSerializers.Add(TypeInfo(TMVCListOfInteger { TList<Integer> } ), TMVCListOfIntegerSerializer.Create);
  GetTypeSerializers.Add(TypeInfo(TMVCListOfBoolean { TList<Boolean> } ), TMVCListOfBooleanSerializer.Create);
  GetTypeSerializers.Add(TypeInfo(TMVCListOfDouble { TList<Double> } ), TMVCListOfDoubleSerializer.Create);
end;

procedure TMVCJsonDataObjectsSerializer.TValueToJSONObjectProperty(const AJSONObject: TJDOJsonObject;
  const AName: string; const AValue: TValue; const AType: TMVCSerializationType; const AIgnored: TMVCIgnoredList;
  const ACustomAttributes: TArray<TCustomAttribute>);
var
  ChildJsonObject: TJDOJsonObject;
  ChildValue: TValue;
  ChildObject, Obj: TObject;
  ValueTypeAtt: MVCValueAsTypeAttribute;
  CastValue, CastedValue: TValue;
  I: Integer;
  LEnumAsAttr: MVCEnumSerializationAttribute;
  LEnumSerType: TMVCEnumSerializationType;
  LEnumMappedValues: TList<string>;
  LEnumName: string;
  lJSONValue: TJsonBaseObject;
  lJsonDataType: TJsonDataType;
  lTypeInfo: PTypeInfo;
  lBuffer: Pointer;
  lCurrentArrayItem: TValue;
begin
  if SameText(AName, 'RefCount') then
  begin
    Exit;
  end;

  if AValue.IsEmpty then
  begin
    if AValue.IsArray then
    begin
      AJSONObject.A[AName] := TJDOJsonArray.Create;
    end
    else
    begin
      if MVCSerializeNulls then
      begin
        AJSONObject[AName] := Null;
      end;
    end;
    Exit;
  end;



  lTypeInfo := AValue.TypeInfo;
  // AValue.TypeInfo does not show the correct TypeInfo of the class instantiated for the object or interface
  ChildObject := nil;
  if AValue.Kind in [tkClass, tkInterface] then
  begin
    if not AValue.IsEmpty and (AValue.Kind = tkInterface) then
      ChildObject := TObject(AValue.AsInterface)
    else if AValue.Kind = tkClass then
      ChildObject := AValue.AsObject;
    if Assigned(ChildObject) then
      lTypeInfo := ChildObject.ClassInfo;
  end;

  if GetTypeSerializers.ContainsKey(lTypeInfo) then
  begin
    GetTypeSerializers.Items[lTypeInfo].SerializeAttribute(AValue, AName, AJSONObject, ACustomAttributes);
    Exit;
  end;

  case AValue.Kind of
    tkInteger:
      AJSONObject.I[AName] := AValue.AsInteger;

    tkInt64:
      AJSONObject.L[AName] := AValue.AsInt64;

    tkChar, tkString, tkWChar, tkLString, tkWString, tkUString:
      AJSONObject.S[AName] := AValue.AsString;

    tkFloat:
      begin
        if (AValue.TypeInfo = System.TypeInfo(TDate)) then
        begin
          if (AValue.AsExtended = 0) then
            AJSONObject[AName] := Null
          else
            AJSONObject.S[AName] := DateToISODate(AValue.AsExtended);
        end
        else if (AValue.TypeInfo = System.TypeInfo(TDateTime)) then
        begin
          if (AValue.AsExtended = 0) then
            AJSONObject[AName] := Null
          else
            AJSONObject.S[AName] := DateTimeToISOTimeStamp(AValue.AsExtended);
        end
        else if (AValue.TypeInfo = System.TypeInfo(TTime)) then
        begin
          if (AValue.AsExtended = 0) then
            AJSONObject[AName] := Null
          else
            AJSONObject.S[AName] := TimeToISOTime(AValue.AsExtended);
        end
        else
          AJSONObject.F[AName] := AValue.AsExtended;
      end;

    tkVariant:
      AJSONObject[AName] := AValue.AsVariant;

    tkEnumeration:
      begin
        if (AValue.TypeInfo = System.TypeInfo(Boolean)) then
        begin
          if AValue.AsBoolean then
            AJSONObject.B[AName] := True
          else
            AJSONObject.B[AName] := False
        end
        else
        begin
          LEnumSerType := estEnumName;
          LEnumMappedValues := nil;
          if TMVCSerializerHelper.AttributeExists<MVCEnumSerializationAttribute>(ACustomAttributes, LEnumAsAttr) then
          begin
            LEnumSerType := LEnumAsAttr.SerializationType;
            LEnumMappedValues := LEnumAsAttr.MappedValues;
          end;

          case LEnumSerType of
            estEnumName:
              begin
                LEnumName := GetEnumName(AValue.TypeInfo, AValue.AsOrdinal);
                AJSONObject.S[AName] := LEnumName;
              end;
            estEnumOrd:
              begin
                AJSONObject.I[AName] := AValue.AsOrdinal;
              end;
            estEnumMappedValues:
              begin
                if (LEnumMappedValues.Count - 1) < AValue.AsOrdinal then
                  raise EMVCException.Create('Enumerator value is not mapped in MappedValues');

                AJSONObject.S[AName] := LEnumMappedValues[AValue.AsOrdinal];
              end;
          end;
        end;
      end;

    tkClass, tkInterface:
      begin
        if Assigned(ChildObject) then
        begin
          lJSONValue := ConvertObjectToJsonValue(ChildObject, GetSerializationType(ChildObject, AType), AIgnored, nil,
            nil, lJsonDataType);
          case lJsonDataType of
            jdtArray:
              begin
                AJSONObject.A[AName] := TJsonArray(lJSONValue);
              end;
            jdtObject:
              begin
                AJSONObject.O[AName] := TJsonObject(lJSONValue);
              end
          else
            begin
              lJSONValue.Free;
              RaiseSerializationError('Invalid JSON Data Type');
            end;
          end;
        end
        else
        begin
          if TMVCSerializerHelper.AttributeExists<MVCSerializeAsStringAttribute>(ACustomAttributes) then
            AJSONObject.S[AName] := EmptyStr
          else
            AJSONObject[AName] := Null;
        end;
      end;

    tkRecord:
      begin
        if AValue.TypeInfo.NameFld.ToString.StartsWith('Nullable') then
        begin
          if TryNullableToJSON(AValue, AJSONObject, AName, ACustomAttributes) then
          begin
            Exit;
          end;
        end;

        if (AValue.TypeInfo = System.TypeInfo(TTimeStamp)) then
        begin
          AJSONObject.F[AName] := TimeStampToMsecs(AValue.AsType<TTimeStamp>);
        end
        else if (AValue.TypeInfo = System.TypeInfo(TValue)) then
        begin
          if TMVCSerializerHelper.AttributeExists<MVCValueAsTypeAttribute>(ACustomAttributes, ValueTypeAtt) then
          begin
            CastValue := AValue.AsType<TValue>;
            if CastValue.TryCast(ValueTypeAtt.ValueTypeInfo, CastedValue) then
            begin
              TValueToJSONObjectProperty(AJSONObject, AName, CastedValue, stDefault, [], [])
            end
            else
            begin
              RaiseSerializationError
                (Format('Cannot serialize property or field "%s" of TypeKind tkRecord (TValue with MVCValueAsTypeAttribute)',
                [AName]));
            end;
          end
          else
          begin
            ChildValue := AValue.AsType<TValue>;
            ChildJsonObject := AJSONObject.O[AName];
            ChildJsonObject.S['type'] := TMVCSerializerHelper.GetTypeKindAsString(ChildValue.TypeInfo.Kind);
            TValueToJSONObjectProperty(ChildJsonObject, 'value', ChildValue, stDefault, [], []);
          end;
        end
        else
        begin
          lJSONValue := ConvertRecordToJsonValue(AValue.GetReferenceToRawData, AValue.TypeInfo, stFields, AIgnored, nil,
            nil, lJsonDataType);
          case lJsonDataType of
            jdtArray:
              begin
                AJSONObject.A[AName] := TJsonArray(lJSONValue);
              end;
            jdtObject:
              begin
                AJSONObject.O[AName] := TJsonObject(lJSONValue);
              end
          else
            begin
              lJSONValue.Free;
              RaiseSerializationError('Invalid JSON Data Type');
            end;
          end;
        end;
      end;

    tkSet:
      begin
{$IF defined(BERLINORBETTER)}
        lBuffer := AllocMem(AValue.DataSize);
        try
          AValue.ExtractRawDataNoCopy(lBuffer);
          AJSONObject.S[AName] := SetToString(AValue.TypeInfo, lBuffer);
        finally
          FreeMem(lBuffer)
        end;
{$ELSE}
        raise EMVCSerializationException.CreateFmt
          ('Cannot serialize property or field "%s" of TypeKind tkSet in this Delphi version.', [AName]);
{$ENDIF}
      end;

    tkArray, tkDynArray:
      begin
        if AValue.GetArrayLength > 0 then
        begin
          for I := 0 to AValue.GetArrayLength - 1 do
          begin
            lCurrentArrayItem := AValue.GetArrayElement(I);
            case lCurrentArrayItem.Kind of
              tkChar, tkString, tkWChar, tkLString, tkWString, tkUString:
                AJSONObject.A[AName].Add(lCurrentArrayItem.AsString);
              tkInteger:
                AJSONObject.A[AName].Add(lCurrentArrayItem.AsInteger);
              tkInt64:
                AJSONObject.A[AName].Add(lCurrentArrayItem.AsInt64);
              tkFloat:
                begin
                  if lCurrentArrayItem.TypeInfo = TypeInfo(TDate) then
                  begin
                    AJSONObject.A[AName].Add(DateToISODate(lCurrentArrayItem.AsExtended));
                  end
                  else if lCurrentArrayItem.TypeInfo = TypeInfo(TTime) then
                  begin
                    AJSONObject.A[AName].Add(TimeToISOTime(lCurrentArrayItem.AsExtended));
                  end
                  else if lCurrentArrayItem.TypeInfo = TypeInfo(TDateTime) then
                  begin
                    AJSONObject.A[AName].Add(DateTimeToISOTimeStamp(lCurrentArrayItem.AsExtended));
                  end
                  else
                  begin
                    AJSONObject.A[AName].Add(lCurrentArrayItem.AsExtended);
                  end;
                end;
              tkEnumeration:
                AJSONObject.A[AName].Add(lCurrentArrayItem.AsBoolean);
              tkClass:
                begin
                  Obj := lCurrentArrayItem.AsObject;
                  if Obj = nil then
                  begin
                    AJSONObject.A[AName].Add(TJsonObject(nil));
                  end
                  else
                  begin
                    lJSONValue := ConvertObjectToJsonValue(Obj, GetSerializationType(Obj), [], nil, nil, lJsonDataType);
                    case lJsonDataType of
                      jdtArray:
                        begin
                          AJSONObject.A[AName].Add(TJsonArray(lJSONValue));
                        end;
                      jdtObject:
                        begin
                          AJSONObject.A[AName].Add(TJsonObject(lJSONValue));
                        end;
                    else
                      begin
                        lJSONValue.Free;
                        RaiseSerializationError('Invalid JSON Type for ' + AName);
                      end;
                    end;
                  end;
                end;
              tkRecord:
                begin
                  if lCurrentArrayItem.IsEmpty then
                  begin
                    AJSONObject.A[AName].Add(TJsonObject(nil));
                  end
                  else
                  begin
                    lJSONValue := ConvertRecordToJsonValue(AValue.GetReferenceToRawArrayElement(I),
                      lCurrentArrayItem.TypeInfo, stFields, [], nil, nil, lJsonDataType);
                    case lJsonDataType of
                      jdtArray:
                        begin
                          AJSONObject.A[AName].Add(TJsonArray(lJSONValue));
                        end;
                      jdtObject:
                        begin
                          AJSONObject.A[AName].Add(TJsonObject(lJSONValue));
                        end;
                    else
                      begin
                        lJSONValue.Free;
                        RaiseSerializationError('Invalid JSON Type for ' + AName);
                      end;
                    end;
                  end;
                end;
            else
              begin
                raise EMVCSerializationException.CreateFmt('Cannot serialize property or field "%s"', [AName]);
              end;
            end;
          end;
        end;
      end;

    tkUnknown:
      raise EMVCSerializationException.CreateFmt
        ('Cannot serialize property or field "%s" of TypeKind tkUnknown.', [AName]);
  end;
end;

function TMVCJsonDataObjectsSerializer.ConvertObjectToJsonValue(const AObject: TObject;
  const AType: TMVCSerializationType; const AIgnoredFields: TMVCIgnoredList;
  const ADataSetSerializationCallback: TMVCDataSetFieldSerializationAction;
  const ASerializationAction: TMVCSerializationAction; out AJsonDataType: TJsonDataType): TJsonBaseObject;
var
  lList: IMVCList;
  I: Integer;
  lValue: TValue;
  lObj: TObject;
  lJSONValue: TJsonBaseObject;
  lJsonDataType: TJsonDataType;
  lLinks: IMVCLinks;
begin
  Result := nil;
  try
    if AObject is TDataSet then
    begin
      Result := TJsonArray.Create;
      AJsonDataType := jdtArray;
      DataSetToJsonArray(TDataSet(AObject), TJsonArray(Result), TMVCNameCase.ncLowerCase, [],
        ADataSetSerializationCallback);
    end
    else if AObject is TJsonObject then
    begin
      AJsonDataType := jdtObject;
      Result := TJsonObject(TJsonObject(AObject).Clone);
    end
    else if AObject is TJsonArray then
    begin
      AJsonDataType := jdtArray;
      Result := TJsonArray(TJsonArray(AObject).Clone);
    end
    else if AObject = nil then
    begin
      AJsonDataType := jdtObject;
      Result := nil;
    end
    else
    begin
      lList := TDuckTypedList.Wrap(AObject);
      if Assigned(lList) then
      begin
        Result := TJsonArray.Create; // ChildJsonArray := AJsonObject.A[AName];
        AJsonDataType := jdtArray;
        for I := 0 to lList.Count - 1 do
        begin
          if lList.ItemIsObject(I, lValue) then
          begin
            lObj := lValue.AsObject; // ChildList.GetItem(I);
            if Assigned(lObj) then
            begin
              lJSONValue := ConvertObjectToJsonValue(lObj, GetSerializationType(lObj, AType), AIgnoredFields, nil,
                ASerializationAction, lJsonDataType);
              case lJsonDataType of
                jdtObject:
                  begin
                    TJsonArray(Result).Add(TJsonObject(lJSONValue));
                  end;
                jdtArray:
                  begin
                    TJsonArray(Result).Add(TJsonArray(lJSONValue));
                  end;
              else
                begin
                  RaiseSerializationError('Invalid JSON type');
                end;
              end;
            end
            else
            begin
              TJsonArray(Result).Add(TJsonObject(nil));
            end;
          end
          else
          begin
            AddTValueToJsonArray(lValue, TJsonArray(Result));
          end;
        end;
      end
      else
      begin
        Result := TJsonObject.Create;
        AJsonDataType := jdtObject;
        lLinks := TMVCLinks.Create;
        InternalObjectToJsonObject(AObject, TJsonObject(Result), GetSerializationType(AObject, AType),
          AIgnoredFields, ASerializationAction, lLinks, nil);
      end;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TMVCJsonDataObjectsSerializer.ConvertRecordToJsonValue(const ARecord: Pointer;
  const ARecordTypeInfo: PTypeInfo; const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList;
  const ADataSetSerializationCallback: TMVCDataSetFieldSerializationAction;
  const ASerializationAction: TMVCSerializationAction; out AJsonDataType: TJsonDataType): TJsonBaseObject;
var
  lLinks: IMVCLinks;
begin
  Result := nil;
  try
    if ARecord = nil then
    begin
      AJsonDataType := jdtObject;
      Result := nil;
    end
    else
    begin
      Result := TJsonObject.Create;
      AJsonDataType := jdtObject;
      lLinks := TMVCLinks.Create;
      InternalRecordToJsonObject(ARecord, ARecordTypeInfo, TJsonObject(Result), stFields, AIgnoredAttributes,
        ASerializationAction, lLinks, nil);
    end;
  except
    FreeAndNil(Result);
    raise;
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

procedure TMVCJsonDataObjectsSerializer.DataSetToJsonObject(const ADataSet: TDataSet; const AJSONObject: TJDOJsonObject;
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
        ASerializationCallback(ADataSet.Fields[lField.I], AJSONObject, lHandled);
        if lHandled then
        begin
          continue;
        end;
      end;

      lFName := TMVCSerializerHelper.ApplyNameCase(ANameCase, lField.FieldName);

      if ADataSet.Fields[lField.I].IsNull then
        AJSONObject[lFName] := Null
      else
      begin
        case lField.DataType of
          ftBoolean:
            AJSONObject.B[lFName] := ADataSet.Fields[lField.I].AsBoolean;

          ftInteger, ftSmallint, ftShortint, ftByte, ftWord:
            AJSONObject.I[lFName] := ADataSet.Fields[lField.I].AsInteger;

          ftLargeint, ftAutoInc, ftLongword:
            AJSONObject.L[lFName] := ADataSet.Fields[lField.I].AsLargeInt;
{$IFDEF TOKYOORBETTER}
          ftGuid:
            AJSONObject.S[lFName] := GUIDToString(ADataSet.Fields[lField.I].AsGuid);
{$ENDIF}
          ftSingle, ftFloat:
            AJSONObject.F[lFName] := ADataSet.Fields[lField.I].AsFloat;

          ftString, ftMemo:
            AJSONObject.S[lFName] := ADataSet.Fields[lField.I].AsString;

          ftWideString, ftWideMemo:
            AJSONObject.S[lFName] := ADataSet.Fields[lField.I].AsWideString;

          ftDate:
            AJSONObject.S[lFName] := DateToISODate(ADataSet.Fields[lField.I].AsDateTime);

          ftDateTime:
            AJSONObject.S[lFName] := DateTimeToISOTimeStamp(ADataSet.Fields[lField.I].AsDateTime);

          ftTime:
            AJSONObject.S[lFName] := SQLTimeStampToStr('hh:nn:ss', ADataSet.Fields[lField.I].AsSQLTimeStamp);

          ftTimeStamp:
            AJSONObject.S[lFName] := DateTimeToISOTimeStamp
              (SQLTimeStampToDateTime(ADataSet.Fields[lField.I].AsSQLTimeStamp));

          ftCurrency:
            AJSONObject.F[lFName] := ADataSet.Fields[lField.I].AsCurrency;

          ftFMTBcd, ftBCD:
            AJSONObject.F[lFName] := BcdToDouble(ADataSet.Fields[lField.I].AsBcd);

          ftGraphic, ftBlob, ftStream, ftOraBlob:
            begin
              lMS := TMemoryStream.Create;
              try
                TBlobField(ADataSet.Fields[lField.I]).SaveToStream(lMS);
                lMS.Position := 0;
                lSS := TStringStream.Create;
                try
                  TMVCSerializerHelper.EncodeStream(lMS, lSS);
                  AJSONObject.S[lFName] := lSS.DataString;
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
                      lChildJsonArray := AJSONObject.A[lField.FieldName];
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
                      lChildJsonObject := AJSONObject.O[lField.FieldName];
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
  const AClazz: TClass; const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList;
  const ARootNode: string);
var
  JSONArray: TJDOJsonArray;
  JsonBase: TJDOJsonBaseObject;
  JSONObject: TJDOJsonObject;
  ObjList: IMVCList;
begin
  if (ASerializedList = EmptyStr) then
    raise EMVCException.Create(HTTP_STATUS.BadRequest, 'Invalid body');

  if not Assigned(AList) then
    Exit;

  if GetTypeSerializers.ContainsKey(AList.ClassInfo) then
  begin
    if ARootNode.IsEmpty then
    begin
      JSONArray := TJDOJsonArray.Parse(ASerializedList) as TJDOJsonArray;
    end
    else
    begin
      try
        JsonBase := TJDOJsonObject.Parse(ASerializedList);
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
      JSONArray := JSONObject.A[ARootNode] as TJDOJsonArray;
    end;
    try
      GetTypeSerializers.Items[AList.ClassInfo].DeserializeRoot(JSONArray, AList, []);
      Exit;
    finally
      JSONArray.Free;
    end;
  end;

  ObjList := TDuckTypedList.Wrap(AList);
  if Assigned(ObjList) then
  begin
    JsonBase := TJDOJsonObject.Parse(ASerializedList);
    try
      try
        if ARootNode.IsEmpty then
        begin
          if not(JsonBase is TJDOJsonArray) then
          begin
            raise EMVCSerializationException.CreateFmt('Invalid JSON. Expected %s got %s',
              [TJDOJsonArray.ClassName, JsonBase.ClassName]);
          end;
          JSONArray := TJDOJsonArray(JsonBase);
        end
        else
        begin
          if not(JsonBase is TJDOJsonObject) then
          begin
            raise EMVCSerializationException.CreateFmt('Invalid JSON. Expected %s got %s',
              [TJDOJsonObject.ClassName, JsonBase.ClassName]);
          end;
          JSONObject := TJDOJsonObject(JsonBase);
          JSONArray := JSONObject.A[ARootNode] as TJDOJsonArray;
        end;
      except
        on E: EJsonParserException do
        begin
          raise EMVCException.Create(HTTP_STATUS.BadRequest, E.Message);
        end;
      end;
      JsonArrayToList(JSONArray, ObjList, AClazz, AType, AIgnoredAttributes);
    finally
      JsonBase.Free;
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
type
  TSetOfTypeElement = (xString, xInt, xLong, xFloat, xBool);
  TSetOfType = set of TSetOfTypeElement;
var
  I: Integer;
  lStrArr: TArray<string>;
  lIntArr: TArray<Integer>;
  lLongArr: TArray<Int64>;
  lDoubleArr: TArray<Double>;
  lBoolArr: TArray<Boolean>;
  lSetOfType: TSetOfType;
  lEl: TSetOfTypeElement;
begin
  lSetOfType := [];
  { TODO -oDanieleT -cGeneral : I dont like this... }
  for I := 0 to Pred(AJsonArray.Count) do
  begin
    case AJsonArray.types[0] of
      jdtString:
        begin
          Include(lSetOfType, xString);
          lStrArr := lStrArr + [AJsonArray.Items[I].Value];
        end;
      jdtInt:
        begin
          Include(lSetOfType, xInt);
          lIntArr := lIntArr + [AJsonArray.Items[I].IntValue];
        end;
      jdtLong:
        begin
          Include(lSetOfType, xLong);
          lLongArr := lLongArr + [AJsonArray.Items[I].LongValue];
        end;
      jdtFloat:
        begin
          Include(lSetOfType, xFloat);
          lDoubleArr := lDoubleArr + [AJsonArray.Items[I].FloatValue];
        end;
      jdtBool:
        begin
          Include(lSetOfType, xBool);
          lBoolArr := lBoolArr + [AJsonArray.Items[I].BoolValue];
        end;
    end;
  end;

  I := 0;
  for lEl in lSetOfType do
  begin
    Inc(I);
    if I > 1 then
    begin
      raise EMVCDeserializationException.Create('Types in the array must be homogeneous');
    end;
  end;

  if Length(lStrArr) > 0 then
    Exit(TValue.From < TArray < string >> (lStrArr));
  if Length(lIntArr) > 0 then
    Exit(TValue.From < TArray < Integer >> (lIntArr));
  if Length(lLongArr) > 0 then
    Exit(TValue.From < TArray < Int64 >> (lLongArr));
  if Length(lBoolArr) > 0 then
    Exit(TValue.From < TArray < Boolean >> (lBoolArr));
  if Length(lDoubleArr) > 0 then
    Exit(TValue.From < TArray < Double >> (lDoubleArr));
  Result := TValue.From < TArray < String >> ([]);
end;

procedure TMVCJsonDataObjectsSerializer.JsonArrayToDataSet(const AJsonArray: TJDOJsonArray; const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase);
var
  I: Integer;
  lSerializationMetaInfo: TSerializationMetaInfo;
begin
  if AJsonArray.Count > 0 then
  begin
    lSerializationMetaInfo := TSerializationMetaInfo.CreateFieldsMetaInfo(
      ADataSet,
      ANameCase,
      AIgnoredFields);
    for I := 0 to Pred(AJsonArray.Count) do
    begin
      ADataSet.Append;
      JsonObjectToDataSet(
        AJsonArray.Items[I].ObjectValue,
        ADataSet,
        lSerializationMetaInfo);
      ADataSet.Post;
    end;
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
    Assert(AJsonArray.Items[I].Typ = jdtObject, 'Cannot deserialize non object type in ' + AClazz.QualifiedClassName +
      '. [HINT] Move data structure to objects or use manual deserialization.');
    JsonObjectToObject(AJsonArray.Items[I].ObjectValue, Obj, GetSerializationType(Obj, AType), AIgnoredAttributes);
    AList.Add(Obj);
  end;
end;

procedure TMVCJsonDataObjectsSerializer.JsonDataValueToAttribute(const AObject: TObject; const ARttiMember: TRttiMember;
  const AJSONObject: TJDOJsonObject; const AName: string; var AValue: TValue; const AType: TMVCSerializationType;
  const AIgnored: TMVCIgnoredList; const ACustomAttributes: TArray<TCustomAttribute>);
var
  ChildObject: TObject;
  lOwnedAttribute: MVCOwnedAttribute;
  lTypeInfo: PTypeInfo;
  lJSONExists: Boolean;
  lJSONIsNull: Boolean;
  lChildObjectAssigned: Boolean;
begin
  ChildObject := nil;
  lTypeInfo := AValue.TypeInfo;
  if AValue.Kind in [tkClass, tkInterface] then
  begin
    if not AValue.IsEmpty then
    begin
      if AValue.Kind = tkInterface then
        ChildObject := TObject(AValue.AsInterface)
      else
        ChildObject := AValue.AsObject;
    end;

    if Assigned(ChildObject) then
    begin
      lTypeInfo := ChildObject.ClassInfo
    end;

    if TMVCSerializerHelper.AttributeExists<MVCOwnedAttribute>(ACustomAttributes, lOwnedAttribute) then
    begin
      {

        Now, can happens the following situations:

        ChildObject   JSON        Outcome
        -----------   ---------   ----------------------------------------------------
        1) Created       Exists      The JSON is loaded in the object (default)
        2) Created       NotExists   Leave unchanged
        3) Created       is Null     If ChildObject is Owned must be destroyed
        4) nil           Exists      If ChildObject is Owned, create it and load the json
        5) nil           NotExists   Leave unchanged
        6) nil           is Null     Leave unchanged


        --> So, we'll manage only case 3 and 4 <--

      }

      lJSONExists := AJSONObject.Contains(AName);
      lJSONIsNull := lJSONExists and AJSONObject.IsNull(AName);
      lChildObjectAssigned := ChildObject <> nil;

      // case 3
      if lChildObjectAssigned and lJSONIsNull then
      begin
        ChildObject.Free;
        case AType of
          stUnknown, stDefault, stProperties:
            TRttiProperty(ARttiMember).SetValue(AObject, nil);
          stFields:
            TRttiField(ARttiMember).SetValue(AObject, nil);
        end;
      end
      // case 4
      else if (not lChildObjectAssigned) and lJSONExists and (not lJSONIsNull) then
      begin
        if lOwnedAttribute.ClassRef <> nil then
        begin
          ChildObject := TMVCSerializerHelper.CreateObject(lOwnedAttribute.ClassRef.QualifiedClassName);
          AValue := ChildObject; //dt20221006
        end
        else
        begin
          case AType of
            stUnknown, stDefault, stProperties:
            begin
              ChildObject := TMVCSerializerHelper.CreateObject(TRttiProperty(ARttiMember).PropertyType);
              AValue := ChildObject; //dt20221006
            end;
            stFields:
            begin
              ChildObject := TMVCSerializerHelper.CreateObject(TRttiField(ARttiMember).FieldType);
              AValue := ChildObject; //dt20221006
            end;
          end;
        end;
        lTypeInfo := ChildObject.ClassInfo;
        case AType of
          stUnknown, stDefault, stProperties:
            TRttiProperty(ARttiMember).SetValue(AObject, ChildObject);
          stFields:
            TRttiField(ARttiMember).SetValue(AObject, ChildObject);
        end;
      end; // end cases
    end;
  end;

  if GetTypeSerializers.ContainsKey(lTypeInfo) then
  begin
    case AJSONObject[AName].Typ of
      jdtNone:
        Exit;
      jdtObject:
        begin
          /// <summary>JsonDataObjects assumes values null as jdtObject</summary>
          if AJSONObject[AName].ObjectValue <> nil then
            GetTypeSerializers.Items[lTypeInfo].DeserializeAttribute(AValue, AName, AJSONObject[AName].ObjectValue,
              ACustomAttributes);
        end;
      jdtArray:
        GetTypeSerializers.Items[lTypeInfo].DeserializeAttribute(AValue, AName, AJSONObject[AName].ArrayValue,
          ACustomAttributes);
    else
      GetTypeSerializers.Items[lTypeInfo].DeserializeAttribute(AValue, AName, AJSONObject, ACustomAttributes);
    end;
    Exit;
  end;
  JSONObjectPropertyToTValue(AJSONObject, AName, AType, AIgnored, ChildObject, AValue, ACustomAttributes);
end;

procedure TMVCJsonDataObjectsSerializer.JSONObjectPropertyToTValue(AJSONObject: TJsonObject;
  const APropertyName: String; const AType: TMVCSerializationType; const AIgnored: TMVCIgnoredList;
  var ChildObject: TObject; var AValue: TValue; const ACustomAttributes: TArray<TCustomAttribute>);
var
  ChildList: IMVCList;
  ChildListOfAtt: MVCListOfAttribute;
  LClazz: TClass;
  lValueTypeInfo: PTypeInfo;
begin
  case AJSONObject[APropertyName].Typ of
    jdtNone:
      Exit;

    jdtString:
      begin
        ParseStringAsTValueUsingMetadata(
          AJSONObject[APropertyName].Value,
          AValue.TypeInfo,
          'property ' + APropertyName,
          ACustomAttributes,
          AValue);
      end;

    jdtInt:
      begin
        if (AValue.Kind = tkEnumeration) then
        begin
          TValue.Make(GetEnumValue(AValue.TypeInfo, GetEnumName(AValue.TypeInfo, AJSONObject[APropertyName].IntValue)),
            AValue.TypeInfo, AValue)
        end
        else if (AValue.Kind <> tkRecord) then { nullables }
        begin
          AValue := TValue.From<Integer>(AJSONObject[APropertyName].IntValue);
        end
        else
        begin
          lValueTypeInfo := AValue.TypeInfo;
          if lValueTypeInfo = TypeInfo(NullableInt32) then
            AValue := TValue.From<NullableInt32>(NullableInt32(AJSONObject[APropertyName].IntValue))
          else if lValueTypeInfo = TypeInfo(NullableUInt32) then
            AValue := TValue.From<NullableUInt32>(NullableUInt32(AJSONObject[APropertyName].IntValue))
          else if lValueTypeInfo = TypeInfo(NullableInt16) then
            AValue := TValue.From<NullableInt16>(NullableInt16(AJSONObject[APropertyName].IntValue))
          else if lValueTypeInfo = TypeInfo(NullableUInt16) then
            AValue := TValue.From<NullableUInt16>(NullableUInt16(AJSONObject[APropertyName].IntValue))
          else if lValueTypeInfo = TypeInfo(NullableInt64) then
            AValue := TValue.From<NullableInt64>(NullableInt64(AJSONObject[APropertyName].LongValue))
          else if lValueTypeInfo = TypeInfo(NullableUInt64) then
            AValue := TValue.From<NullableUInt64>(NullableUInt64(AJSONObject[APropertyName].LongValue))
          else if not TryMapNullableFloat(AValue, AJSONObject, APropertyName) then
            raise EMVCDeserializationException.CreateFmt('Cannot deserialize integer value for "%s"', [APropertyName]);
        end;
      end;

    jdtLong, jdtULong:
      begin
        lValueTypeInfo := AValue.TypeInfo;
        if (lValueTypeInfo = System.TypeInfo(TTimeStamp)) then
        begin
          AValue := TValue.From<TTimeStamp>(MSecsToTimeStamp(AJSONObject[APropertyName].LongValue))
        end
        else if (AValue.Kind <> tkRecord) then { nullables }
        begin
          AValue := TValue.From<Int64>(AJSONObject[APropertyName].LongValue);
        end
        else
        begin
          if lValueTypeInfo = TypeInfo(NullableInt64) then
            AValue := TValue.From<NullableInt64>(NullableInt64(AJSONObject[APropertyName].LongValue))
          else if lValueTypeInfo = TypeInfo(NullableUInt64) then
            AValue := TValue.From<NullableUInt64>(NullableUInt64(AJSONObject[APropertyName].LongValue))
          else if not TryMapNullableFloat(AValue, AJSONObject, APropertyName) then
            raise EMVCDeserializationException.CreateFmt('Cannot deserialize long integer value for "%s"',
              [APropertyName]);
        end;
      end;

    jdtFloat:
      if (AValue.Kind <> tkRecord) then { nullables }
      begin
        AValue := TValue.From<Double>(AJSONObject[APropertyName].FloatValue);
      end
      else
      begin
        if not TryMapNullableFloat(AValue, AJSONObject, APropertyName) then
          raise EMVCDeserializationException.CreateFmt('Cannot deserialize floating-point value for "%s"',
            [APropertyName]);
      end;

    jdtDateTime:
      if (AValue.Kind <> tkRecord) then { nullables }
      begin
        AValue := TValue.From<TDateTime>(AJSONObject[APropertyName].DateTimeValue);
      end
      else
      begin
        if AValue.TypeInfo = TypeInfo(NullableTDate) then
          AValue := TValue.From<NullableTDate>(NullableTDate(AJSONObject[APropertyName].DateTimeValue))
        else if AValue.TypeInfo = TypeInfo(NullableTDateTime) then
          AValue := TValue.From<NullableTDateTime>(NullableTDateTime(AJSONObject[APropertyName].DateTimeValue))
        else if AValue.TypeInfo = TypeInfo(NullableTTime) then
          AValue := TValue.From<NullableTTime>(NullableTTime(AJSONObject[APropertyName].DateTimeValue))
        else
          raise EMVCDeserializationException.CreateFmt('Cannot deserialize date or time value for "%s"',
            [APropertyName]);
      end;

    jdtBool:
      if (AValue.Kind <> tkRecord) then { nullables }
      begin
        AValue := TValue.From<Boolean>(AJSONObject[APropertyName].BoolValue);
      end
      else
      begin
        if AValue.TypeInfo = TypeInfo(NullableBoolean) then
          AValue := TValue.From<NullableBoolean>(NullableBoolean(AJSONObject[APropertyName].BoolValue))
        else
          raise EMVCDeserializationException.CreateFmt('Cannot deserialize boolean value for "%s"', [APropertyName]);
      end;

    jdtObject:
      begin
        if (AValue.TypeInfo = System.TypeInfo(TValue)) then
        begin
          AValue := TValue.FromVariant(AJSONObject[APropertyName].O['value'].VariantValue)
        end
        else
        begin
          // dt: if a key is null, jsondataobjects assign it the type jdtObject
          if AJSONObject[APropertyName].ObjectValue <> nil then
          begin
            case AValue.Kind of
              tkInterface:
                begin
                  JsonObjectToObject(AJSONObject.O[APropertyName], ChildObject,
                    GetSerializationType(ChildObject, AType), AIgnored);
                end;
              tkClass:
                begin
                  JsonObjectToObject(AJSONObject.O[APropertyName], ChildObject,
                    GetSerializationType(ChildObject, AType), AIgnored);
                end;
              tkString, tkUString:
                begin
                  AValue := AJSONObject.O[APropertyName].ToJSON();
                end;
              tkRecord:
                begin
                  if AValue.TypeInfo = TypeInfo(NullableString) then
                  begin
                    AValue := TValue.From<NullableString>(NullableString(AJSONObject.O[APropertyName].ToJSON()));
                  end
                  else
                  begin
                    raise EMVCDeserializationException.CreateFmt('Cannot deserialize object value for "%s"',
                      [APropertyName]);
                  end;
                end
            end;
          end
          else if AValue.Kind = tkRecord then
          begin
            if String(AValue.TypeInfo.Name).StartsWith('Nullable') then
            begin
              case GetNullableType(AValue.TypeInfo) of
                ntNullableString: NullableString(AValue.GetReferenceToRawData^).SetNull;
                ntNullableCurrency: NullableCurrency(AValue.GetReferenceToRawData^).SetNull;
                ntNullableBoolean: NullableBoolean(AValue.GetReferenceToRawData^).SetNull;
                ntNullableTDate: NullableTDate(AValue.GetReferenceToRawData^).SetNull;
                ntNullableTTime: NullableTTime(AValue.GetReferenceToRawData^).SetNull;
                ntNullableTDateTime: NullableTDateTime(AValue.GetReferenceToRawData^).SetNull;
                ntNullableSingle: NullableSingle(AValue.GetReferenceToRawData^).SetNull;
                ntNullableDouble: NullableDouble(AValue.GetReferenceToRawData^).SetNull;
                ntNullableExtended: NullableExtended(AValue.GetReferenceToRawData^).SetNull;
                ntNullableInt16: NullableInt16(AValue.GetReferenceToRawData^).SetNull;
                ntNullableUInt16: NullableUInt16(AValue.GetReferenceToRawData^).SetNull;
                ntNullableInt32: NullableInt32(AValue.GetReferenceToRawData^).SetNull;
                ntNullableUInt32: NullableUInt32(AValue.GetReferenceToRawData^).SetNull;
                ntNullableInt64: NullableInt64(AValue.GetReferenceToRawData^).SetNull;
                ntNullableUInt64: NullableUInt64(AValue.GetReferenceToRawData^).SetNull;
                ntNullableTGUID: NullableTGUID(AValue.GetReferenceToRawData^).SetNull;
                else
                  raise EMVCNullable.Create('Invalid Nullable Type: ' + String(AValue.TypeInfo.Name));
              end;
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
            JsonArrayToDataSet(AJSONObject.A[APropertyName], ChildObject as TDataSet, AIgnored, ncLowerCase)
          else if GetTypeSerializers.ContainsKey(ChildObject.ClassInfo) then
          begin
            GetTypeSerializers.Items[ChildObject.ClassInfo].DeserializeAttribute(AValue, APropertyName, AJSONObject,
              ACustomAttributes);
          end
          else
          begin
            ChildList := TDuckTypedList.Wrap(ChildObject);

            if TMVCSerializerHelper.AttributeExists<MVCListOfAttribute>(ACustomAttributes, ChildListOfAtt) then
              LClazz := ChildListOfAtt.Value
            else
              LClazz := GetObjectTypeOfGenericList(AValue.TypeInfo);

            if Assigned(LClazz) then
              JsonArrayToList(AJSONObject.A[APropertyName], ChildList, LClazz, AType, AIgnored)
            else
              raise EMVCDeserializationException.CreateFmt
                ('You can not deserialize a list "%s" without the MVCListOf attribute.', [APropertyName]);
          end;
        end
        else if AValue.isArray then
        begin
          AValue := JsonArrayToArray(AJSONObject.A[APropertyName]);
        end;
      end;
  end;
end;

procedure TMVCJsonDataObjectsSerializer.JSONObjectPropertyToTValueForRecord(AJSONObject: TJsonObject;
  const APropertyName: String; const AType: TMVCSerializationType; const AIgnored: TMVCIgnoredList; var AValue: TValue;
  const ACustomAttributes: TArray<TCustomAttribute>; const ARTTIField: TRttiField);
var
  LEnumAsAttr: MVCEnumSerializationAttribute;
  LEnumMappedValues: TList<string>;
  LEnumSerType: TMVCEnumSerializationType;
  LMappedValueIndex: Integer;
  lOutInteger: Integer;
  lInt: Integer;
  lOutInteger64: Int64;
  lChildObject: TObject;
  lRef: PByte;
  lInnerType: TRttiType;
  lCtx: TRttiContext;
  lArr: TArray<TValue>;
  lBuff: PByte;
  lInnerTypeAsRecord: TRttiRecordType;
  lJItem: TJsonObject;

  procedure BuildATValueArrayFromJSONArrayOfJSONObject;
  var
    I: Integer;
  begin
    lInnerTypeAsRecord := lInnerType.AsRecord;
    for I := 0 to Length(lArr) - 1 do
    begin
      lBuff := AValue.GetReferenceToRawArrayElement(I);
      lJItem := AJSONObject.A[APropertyName].Items[I].ObjectValue;
      JSONObjectToRecord(lJItem, lInnerTypeAsRecord, lBuff);
      TValue.MakeWithoutCopy(lBuff, lInnerType.Handle, lArr[I]);
      FreeMem(lBuff, lInnerType.TypeSize);
    end;
  end;

  procedure BuildATValueArrayFromJSONArrayOfSimpleType;
  type
    TSetOfTypeElement = (xString, xInt, xLong, xFloat, xBool);
    TSetOfType = set of TSetOfTypeElement;
  var
    I: Integer;
    LJArr: TJsonArray;
    lArrayItemType: TJsonDataType;
  begin
    LJArr := AJSONObject.A[APropertyName];
    if LJArr.Count = 0 then
    begin
      SetLength(lArr, 0);
      Exit;
    end;
    lArrayItemType := LJArr.types[0];

    for I := 0 to Pred(LJArr.Count) do
    begin
      case lArrayItemType of
        jdtString:
          begin
            if lInnerType.Handle = TypeInfo(TDate) then
            begin
              lArr[I] := ISODateToDate(LJArr.Items[I].Value);
            end
            else if lInnerType.Handle = TypeInfo(TTime) then
            begin
              lArr[I] := ISOTimeToTime(LJArr.Items[I].Value);
            end
            else if lInnerType.Handle = TypeInfo(TDateTime) then
            begin
              lArr[I] := ISOTimeStampToDateTime(LJArr.Items[I].Value);
            end
            else
            begin
              lArr[I] := LJArr.Items[I].Value;
            end;
          end;
        jdtInt:
          begin
            lArr[I] := LJArr.Items[I].IntValue;
          end;
        jdtLong:
          begin
            lArr[I] := LJArr.Items[I].LongValue;
          end;
        jdtFloat:
          begin
            lArr[I] := LJArr.Items[I].FloatValue;
          end;
        jdtBool:
          begin
            lArr[I] := LJArr.Items[I].BoolValue;
          end;
      else
        raise EMVCDeserializationException.Create('Invalid element in array at property ' + APropertyName);
      end;
    end;
  end;

begin
  lChildObject := nil;
  case AJSONObject[APropertyName].Typ of
    jdtNone:
      Exit;

    jdtString:
      begin
        if (AValue.TypeInfo = System.TypeInfo(TDate)) then
          AValue := TValue.From<TDate>(ISODateToDate(AJSONObject[APropertyName].Value))

        else if (AValue.TypeInfo = System.TypeInfo(TDateTime)) then
          AValue := TValue.From<TDateTime>(ISOTimeStampToDateTime(AJSONObject[APropertyName].Value))

        else if (AValue.TypeInfo = System.TypeInfo(TTime)) then
          AValue := TValue.From<TTime>(ISOTimeToTime(AJSONObject[APropertyName].Value))
        else if (AValue.Kind = tkRecord) and (AValue.TypeInfo <> TypeInfo(TValue)) then { nullables }
        begin
          if AValue.TypeInfo = TypeInfo(NullableString) then
          begin
            AValue := TValue.From<NullableString>(NullableString(AJSONObject[APropertyName].Value))
          end
          else if AValue.TypeInfo = TypeInfo(NullableTDate) then
          begin
            AValue := TValue.From<NullableTDate>(NullableTDate(ISODateToDate(AJSONObject[APropertyName].Value)))
          end
          else if AValue.TypeInfo = TypeInfo(NullableTDateTime) then
          begin
            AValue := TValue.From<NullableTDateTime>
              (NullableTDateTime(ISOTimeStampToDateTime(AJSONObject[APropertyName].Value)))
          end
          else if AValue.TypeInfo = TypeInfo(NullableTTime) then
          begin
            AValue := TValue.From<NullableTTime>(NullableTTime(ISOTimeToTime(AJSONObject[APropertyName].Value)))
          end
          else
            raise EMVCSerializationException.CreateFmt('Cannot deserialize property "%s" from string', [APropertyName]);
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
            TValue.Make(GetEnumValue(AValue.TypeInfo, AJSONObject[APropertyName].Value), AValue.TypeInfo, AValue)
          end
          else
          begin
            LMappedValueIndex := LEnumMappedValues.IndexOf(AJSONObject[APropertyName].Value);
            if LMappedValueIndex < 0 then
              raise EMVCSerializationException.CreateFmt('Cannot deserialize property "%s" from mapped values',
                [APropertyName]);

            TValue.Make(GetEnumValue(AValue.TypeInfo, GetEnumName(AValue.TypeInfo, LMappedValueIndex)),
              AValue.TypeInfo, AValue)
          end;
        end
        else if (AValue.Kind = tkInteger) and (TryStrToInt(AJSONObject[APropertyName].Value, lOutInteger)) then
        begin
          AValue := lOutInteger;
        end
        else if (AValue.Kind = tkInt64) and (TryStrToInt64(AJSONObject[APropertyName].Value, lOutInteger64)) then
        begin
          AValue := lOutInteger64;
        end
        else if AValue.TypeInfo.Kind = tkSet then
        begin
          lInt := StringToSet(AValue.TypeInfo, StringReplace(AJSONObject[APropertyName].Value, ' ', '',
            [rfReplaceAll]));
          TValue.Make(lInt, AValue.TypeInfo, AValue);
        end
        else
        begin
          AValue := TValue.From<string>(AJSONObject[APropertyName].Value);
        end;
      end;

    jdtInt:
      begin
        if (AValue.Kind = tkEnumeration) then
        begin
          TValue.Make(GetEnumValue(AValue.TypeInfo, GetEnumName(AValue.TypeInfo, AJSONObject[APropertyName].IntValue)),
            AValue.TypeInfo, AValue)
        end
        else if (AValue.Kind <> tkRecord) then { nullables }
        begin
          AValue := TValue.From<Integer>(AJSONObject[APropertyName].IntValue);
        end
        else
        begin
          if AValue.TypeInfo = TypeInfo(NullableInt32) then
            AValue := TValue.From<NullableInt32>(NullableInt32(AJSONObject[APropertyName].IntValue))
          else if AValue.TypeInfo = TypeInfo(NullableUInt32) then
            AValue := TValue.From<NullableUInt32>(NullableUInt32(AJSONObject[APropertyName].IntValue))
          else if AValue.TypeInfo = TypeInfo(NullableInt16) then
            AValue := TValue.From<NullableInt16>(NullableInt16(AJSONObject[APropertyName].IntValue))
          else if AValue.TypeInfo = TypeInfo(NullableUInt16) then
            AValue := TValue.From<NullableUInt16>(NullableUInt16(AJSONObject[APropertyName].IntValue))
          else if AValue.TypeInfo = TypeInfo(NullableInt64) then
            AValue := TValue.From<NullableInt64>(NullableInt64(AJSONObject[APropertyName].LongValue))
          else if AValue.TypeInfo = TypeInfo(NullableUInt64) then
            AValue := TValue.From<NullableUInt64>(NullableUInt64(AJSONObject[APropertyName].LongValue))
          else if not TryMapNullableFloat(AValue, AJSONObject, APropertyName) then
            raise EMVCDeserializationException.CreateFmt('Cannot deserialize integer value for "%s"', [APropertyName]);
        end;
      end;

    jdtLong, jdtULong:
      begin
        if (AValue.TypeInfo = System.TypeInfo(TTimeStamp)) then
        begin
          AValue := TValue.From<TTimeStamp>(MSecsToTimeStamp(AJSONObject[APropertyName].LongValue))
        end
        else if (AValue.Kind <> tkRecord) then { nullables }
        begin
          AValue := TValue.From<Int64>(AJSONObject[APropertyName].LongValue);
        end
        else
        begin
          if AValue.TypeInfo = TypeInfo(NullableInt64) then
            AValue := TValue.From<NullableInt64>(NullableInt64(AJSONObject[APropertyName].LongValue))
          else if AValue.TypeInfo = TypeInfo(NullableUInt64) then
            AValue := TValue.From<NullableUInt64>(NullableUInt64(AJSONObject[APropertyName].LongValue))
          else if not TryMapNullableFloat(AValue, AJSONObject, APropertyName) then
            raise EMVCDeserializationException.CreateFmt('Cannot deserialize long integer value for "%s"',
              [APropertyName]);
        end;
      end;

    jdtFloat:
      if (AValue.Kind <> tkRecord) then { nullables }
      begin
        AValue := TValue.From<Double>(AJSONObject[APropertyName].FloatValue);
      end
      else
      begin
        if not TryMapNullableFloat(AValue, AJSONObject, APropertyName) then
          raise EMVCDeserializationException.CreateFmt('Cannot deserialize floating-point value for "%s"',
            [APropertyName]);
      end;

    jdtDateTime:
      if (AValue.Kind <> tkRecord) then { nullables }
      begin
        AValue := TValue.From<TDateTime>(AJSONObject[APropertyName].DateTimeValue);
      end
      else
      begin
        if AValue.TypeInfo = TypeInfo(NullableTDate) then
          AValue := TValue.From<NullableTDate>(NullableTDate(AJSONObject[APropertyName].DateTimeValue))
        else if AValue.TypeInfo = TypeInfo(NullableTDateTime) then
          AValue := TValue.From<NullableTDateTime>(NullableTDateTime(AJSONObject[APropertyName].DateTimeValue))
        else if AValue.TypeInfo = TypeInfo(NullableTTime) then
          AValue := TValue.From<NullableTTime>(NullableTTime(AJSONObject[APropertyName].DateTimeValue))
        else
          raise EMVCDeserializationException.CreateFmt('Cannot deserialize date or time value for "%s"',
            [APropertyName]);
      end;

    jdtBool:
      if (AValue.Kind <> tkRecord) then { nullables }
      begin
        AValue := TValue.From<Boolean>(AJSONObject[APropertyName].BoolValue);
      end
      else
      begin
        if AValue.TypeInfo = TypeInfo(NullableBoolean) then
          AValue := TValue.From<NullableBoolean>(NullableBoolean(AJSONObject[APropertyName].BoolValue))
        else
          raise EMVCDeserializationException.CreateFmt('Cannot deserialize boolean value for "%s"', [APropertyName]);
      end;

    jdtObject:
      begin
        if (AValue.TypeInfo = System.TypeInfo(TValue)) then
          AValue := TValue.FromVariant(AJSONObject[APropertyName].O['value'].VariantValue)
        else
        begin
          // dt: if a key is null, jsondataobjects assign it the type jdtObject
          if AJSONObject[APropertyName].ObjectValue <> nil then
          begin
            case AValue.Kind of
              tkInterface:
                begin
                  JsonObjectToObject(AJSONObject.O[APropertyName], lChildObject,
                    GetSerializationType(lChildObject, AType), AIgnored);
                end;
              tkClass:
                begin
                  JsonObjectToObject(AJSONObject.O[APropertyName], lChildObject,
                    GetSerializationType(lChildObject, AType), AIgnored);
                end;
              tkString, tkUString:
                begin
                  AValue := AJSONObject.O[APropertyName].ToJSON();
                end;
              tkRecord:
                begin
                  if AValue.TypeInfo = TypeInfo(NullableString) then
                  begin
                    AValue := TValue.From<NullableString>(NullableString(AJSONObject.O[APropertyName].ToJSON()));
                  end
                  else
                  begin
                    lRef := PByte(AValue.GetReferenceToRawData);
                    JSONObjectToNestedRecordFieldStatic(AJSONObject, ARTTIField, 0, lRef);
                  end;
                end;
            else
              begin
                raise Exception.Create('Type not suppported: ' + GetEnumName(TypeInfo(TJsonDataType),
                  Ord(AJSONObject[APropertyName].Typ)));
              end;
            end;
          end;
        end;
      end;

    jdtArray:
      begin
        lCtx := TRttiContext.Create;
        try
          if AValue.Kind = tkArray then
          begin
            if AValue.GetArrayLength <> AJSONObject.A[APropertyName].Count then
            begin
              raise EMVCDeserializationException.Create(Format('Wrong array size, expected %d, got %d',
                [AValue.GetArrayLength, AJSONObject.A[APropertyName].Count]));
            end;
            SetLength(lArr, AJSONObject.A[APropertyName].Count);
            lInnerType := lCtx.GetType(AValue.GetArrayElement(0).TypeInfo);
            BuildATValueArrayFromJSONArrayOfJSONObject;
            AValue := TValue.FromArray(ARTTIField.FieldType.Handle, lArr);
          end
          else if AValue.Kind = tkDynArray then
          begin
            SetLength(lArr, AJSONObject.A[APropertyName].Count);
            if Length(lArr) > 0 then
            begin
              // DT: This line is required to know the typeinfo of an element of the dynamic array
              // still not created (see BuildATValueArrayFromJSONArrayOfJSONObject).
              // This is required because the dynamic array is still
              // not dimensioned here, for a static array this is not necessary.
              AValue := TValue.FromArray(ARTTIField.FieldType.Handle, [TValue.Empty]);
              lInnerType := lCtx.GetType(AValue.GetArrayElement(0).TypeInfo);
              if lInnerType.IsRecord then
              begin
                BuildATValueArrayFromJSONArrayOfJSONObject;
              end
              else
              begin
                BuildATValueArrayFromJSONArrayOfSimpleType;
                // raise Exception.Create('Unsupported type: ' + ARTTIField.FieldType.Name);
              end;
            end;
            AValue := TValue.FromArray(ARTTIField.FieldType.Handle, lArr);
          end
          else
          begin
            raise Exception.Create('A JSON Array cannot be mapped to ' + ARTTIField.FieldType.Name);
          end;
        finally
          lCtx.Free;
        end;
      end;
  end;
end;

procedure TMVCJsonDataObjectsSerializer.JsonObjectToDataSet(
  const AJSONObject: TJDOJsonObject; const ADataSet: TDataSet;
  const SerializationMetaInfo: TSerializationMetaInfo);
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
      if SerializationMetaInfo.FieldsMetaInfo[Field.Index].Ignored then
      begin
        Continue;
      end;
  //    lName := GetNameAs(ADataSet.Owner, Field.Name, Field.FieldName);

//      if (IsIgnoredAttribute(AIgnoredFields, lName)) or (IsIgnoredComponent(ADataSet.Owner, Field.Name)) then
//        continue;

//      lName := TMVCSerializerHelper.ApplyNameCase(GetNameCase(ADataSet, ANameCase), lName);

      lName := SerializationMetaInfo.FieldsMetaInfo[Field.Index].NameAs;

      if not AJSONObject.Contains(lName) then
        continue;

      if (AJSONObject[lName].Typ = jdtObject) and (AJSONObject.Values[lName].ObjectValue = nil) then
      // Nullable Type
      begin
        Field.Clear;
        continue;
      end;

      case Field.DataType of
        TFieldType.ftBoolean:
          Field.AsBoolean := AJSONObject.B[lName];

        TFieldType.ftInteger, TFieldType.ftSmallint, TFieldType.ftShortint, TFieldType.ftByte, TFieldType.ftLongword,
          TFieldType.ftWord, TFieldType.ftAutoInc:
          Field.AsInteger := AJSONObject.I[lName];

        TFieldType.ftLargeint:
          Field.AsLargeInt := AJSONObject.L[lName];

        TFieldType.ftCurrency:
          Field.AsCurrency := AJSONObject.F[lName];

        TFieldType.ftSingle:
          Field.AsSingle := AJSONObject.F[lName];

        TFieldType.ftFloat, TFieldType.ftFMTBcd, TFieldType.ftBCD:
          Field.AsFloat := AJSONObject.F[lName];

        ftString, ftWideString, ftMemo, ftWideMemo:
          Field.AsWideString := AJSONObject.S[lName];

        TFieldType.ftDate:
          Field.AsDateTime := ISODateToDate(AJSONObject.S[lName]);

        TFieldType.ftDateTime, TFieldType.ftTimeStamp:
          Field.AsDateTime := ISOTimeStampToDateTime(AJSONObject.S[lName]);

        TFieldType.ftTime:
          Field.AsDateTime := ISOTimeToTime(AJSONObject.S[lName]);

{$IFDEF TOKYOORBETTER}
        TFieldType.ftGuid:
          Field.AsGuid := StringToGUID(AJSONObject.S[lName]);
{$ENDIF}
        TFieldType.ftGraphic, TFieldType.ftBlob, TFieldType.ftStream:
          begin
            SS := TStringStream.Create(AJSONObject.S[lName]);
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
                  JsonArrayToDataSet(
                    AJSONObject.A[lName],
                    NestedDataSet,
                    SerializationMetaInfo.IgnoredFields,
                    SerializationMetaInfo.NameCase);
                end;
              dtObject:
                begin
                  NestedDataSet.Edit;
                  JsonObjectToDataSet(
                    AJSONObject.O[lName],
                    NestedDataSet,
                    SerializationMetaInfo.IgnoredFields,
                    SerializationMetaInfo.NameCase);
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

procedure TMVCJsonDataObjectsSerializer.JsonObjectToDataSet(const AJSONObject: TJDOJsonObject; const ADataSet: TDataSet;
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

      lName := TMVCSerializerHelper.ApplyNameCase(GetNameCase(ADataSet, ANameCase), lName);

      if not AJSONObject.Contains(lName) then
        continue;

      if (AJSONObject[lName].Typ = jdtObject) and (AJSONObject.Values[lName].ObjectValue = nil) then
      // Nullable Type
      begin
        Field.Clear;
        continue;
      end;

      case Field.DataType of
        TFieldType.ftBoolean:
          Field.AsBoolean := AJSONObject.B[lName];

        TFieldType.ftInteger, TFieldType.ftSmallint, TFieldType.ftShortint, TFieldType.ftByte, TFieldType.ftLongword,
          TFieldType.ftWord, TFieldType.ftAutoInc:
          Field.AsInteger := AJSONObject.I[lName];

        TFieldType.ftLargeint:
          Field.AsLargeInt := AJSONObject.L[lName];

        TFieldType.ftCurrency:
          Field.AsCurrency := AJSONObject.F[lName];

        TFieldType.ftSingle:
          Field.AsSingle := AJSONObject.F[lName];

        TFieldType.ftFloat, TFieldType.ftFMTBcd, TFieldType.ftBCD:
          Field.AsFloat := AJSONObject.F[lName];

        ftString, ftWideString, ftMemo, ftWideMemo:
          Field.AsWideString := AJSONObject.S[lName];

        TFieldType.ftDate:
          Field.AsDateTime := ISODateToDate(AJSONObject.S[lName]);

        TFieldType.ftDateTime, TFieldType.ftTimeStamp:
          Field.AsDateTime := ISOTimeStampToDateTime(AJSONObject.S[lName]);

        TFieldType.ftTime:
          Field.AsDateTime := ISOTimeToTime(AJSONObject.S[lName]);

{$IFDEF TOKYOORBETTER}
        TFieldType.ftGuid:
          Field.AsGuid := StringToGUID(AJSONObject.S[lName]);
{$ENDIF}
        TFieldType.ftGraphic, TFieldType.ftBlob, TFieldType.ftStream:
          begin
            SS := TStringStream.Create(AJSONObject.S[lName]);
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
                  JsonArrayToDataSet(AJSONObject.A[lName], NestedDataSet, AIgnoredFields, ANameCase);
                end;
              dtObject:
                begin
                  NestedDataSet.Edit;
                  JsonObjectToDataSet(AJSONObject.O[lName], NestedDataSet, AIgnoredFields, ANameCase);
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

procedure TMVCJsonDataObjectsSerializer.JsonObjectToObject(const AJSONObject: TJDOJsonObject; const AObject: TObject;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);
var
  lObjType: TRttiType;
  lProp: TRttiProperty;
  lFld: TRttiField;
  lAttributeValue: TValue;
  lKeyName: string;
  lErrMsg: string;
begin
  if AObject = nil then
  begin
    Exit;
  end;

  if AObject is TJsonObject then
  begin
    if not Assigned(AObject) then
    begin
      raise EMVCDeserializationException.Create(AObject.ClassName + ' is not assigned');
    end;
    TJsonObject(AObject).Assign(AJSONObject);
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
              JsonDataValueToAttribute(AObject, lProp, AJSONObject, lKeyName, lAttributeValue, AType,
                AIgnoredAttributes, lProp.GetAttributes);
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
                [lKeyName, lProp.PropertyType.ToString(), JDO_TYPE_DESC[AJSONObject[lKeyName].Typ]]);
            end
            else
            begin
              lErrMsg := Format('Invalid class typecast for property "%s" [Actual: %s]',
                [lKeyName, JDO_TYPE_DESC[AJSONObject[lKeyName].Typ]]);
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
              JsonDataValueToAttribute(AObject, lFld, AJSONObject, lKeyName, lAttributeValue, AType, AIgnoredAttributes,
                lFld.GetAttributes);
              if (not lAttributeValue.IsEmpty) and (not lAttributeValue.IsObject) then
                lFld.SetValue(AObject, lAttributeValue);
            end;
        except
          on E: EInvalidCast do
          begin
            if lFld <> nil then
            begin
              lErrMsg := Format('Invalid class typecast for field "%s" [Expected: %s, Actual: %s]',
                [lKeyName, lFld.FieldType.ToString(), JDO_TYPE_DESC[AJSONObject[lKeyName].Typ]]);
            end
            else
            begin
              lErrMsg := Format('Invalid class typecast for field "%s" [Actual: %s]',
                [lKeyName, JDO_TYPE_DESC[AJSONObject[lKeyName].Typ]]);
            end;
            raise EMVCException.Create(HTTP_STATUS.BadRequest, lErrMsg);
          end;
        end;
      end;
  end;
end;

procedure TMVCJsonDataObjectsSerializer.JSONObjectToRecord(const JSONObject: TJsonObject; RTTIType: TRttiRecordType;
  out Buffer: PByte);
var
  lTypeSize: Integer;
  AIgnoredAttributes: TMVCIgnoredList;
  lKeyName: string;
  lAttributeValue: TValue;
  lErrMsg: string;
  lField: TRttiField;
begin
  if RTTIType = nil then
  begin
    raise EMVCDeserializationException.Create('Insufficient RTTI to deserialize record');
  end;
  lTypeSize := RTTIType.TypeSize;
  GetMem(Buffer, lTypeSize);
  FillChar(Buffer^, lTypeSize, 0);
{$IF Defined(SYDNEYORBETTER)}
  InvokeRecordInitializer(Buffer, RTTIType.Handle);
{$ENDIF}
  lField := nil;
  AIgnoredAttributes := [];
  try
    for lField in RTTIType.GetFields do
      if (not TMVCSerializerHelper.HasAttribute<MVCDoNotDeserializeAttribute>(lField)) and
        (not IsIgnoredAttribute(AIgnoredAttributes, lField.Name)) then
      begin
        lKeyName := TMVCSerializerHelper.GetKeyName(lField, RTTIType);
        if lField.FieldType.IsRecord then
        begin
          JSONObjectToNestedRecordField(JSONObject.O[lKeyName], lField, 0, Buffer);
        end
        else
        begin
          lAttributeValue := lField.GetValue(Buffer);
          JSONObjectPropertyToTValueForRecord(JSONObject, lKeyName, TMVCSerializationType.stProperties,
            AIgnoredAttributes, lAttributeValue, lField.GetAttributes, lField);
          lField.SetValue(Buffer, lAttributeValue);
        end;
      end;
  except
    on E: EInvalidCast do
    begin
      if lField <> nil then
      begin
        lErrMsg := Format('Invalid class typecast for field "%s" [Expected: %s, Actual: %s]',
          [lKeyName, lField.FieldType.ToString(), JDO_TYPE_DESC[JSONObject[lKeyName].Typ]]);
      end
      else
      begin
        lErrMsg := Format('Invalid class typecast for field "%s" [Actual: %s]',
          [lKeyName, JDO_TYPE_DESC[JSONObject[lKeyName].Typ]]);
      end;
      raise EMVCException.Create(HTTP_STATUS.BadRequest, lErrMsg);
    end;
  end;
end;

procedure TMVCJsonDataObjectsSerializer.JSONObjectToRecordStatic(const JSONObject: TJsonObject;
  RTTIType: TRttiRecordType; var Buffer: PByte);
var
  lTypeSize: Integer;
  AIgnoredAttributes: TMVCIgnoredList;
  lKeyName: string;
  lAttributeValue: TValue;
  lErrMsg: string;
  lField: TRttiField;
begin
  lTypeSize := RTTIType.TypeSize;
  GetMem(Buffer, lTypeSize);
  FillChar(Buffer^, lTypeSize, 0);
{$IF Defined(SYDNEYORBETTER)}
  InvokeRecordInitializer(Buffer, RTTIType.Handle);
{$ENDIF}
  lField := nil;
  AIgnoredAttributes := [];
  try
    for lField in RTTIType.GetFields do
      if (not TMVCSerializerHelper.HasAttribute<MVCDoNotDeserializeAttribute>(lField)) and
        (not IsIgnoredAttribute(AIgnoredAttributes, lField.Name)) then
      begin
        lKeyName := TMVCSerializerHelper.GetKeyName(lField, RTTIType);
        if lField.FieldType.IsRecord then
        begin
          JSONObjectToNestedRecordField(JSONObject.O[lKeyName], lField, 0, Buffer);
        end
        else
        begin
          lAttributeValue := lField.GetValue(Buffer);
          JSONObjectPropertyToTValueForRecord(JSONObject, lKeyName, TMVCSerializationType.stProperties,
            AIgnoredAttributes, lAttributeValue, lField.GetAttributes, lField);
          lField.SetValue(Buffer, lAttributeValue);
        end;
      end;
  except
    on E: EInvalidCast do
    begin
      if lField <> nil then
      begin
        lErrMsg := Format('Invalid class typecast for field "%s" [Expected: %s, Actual: %s]',
          [lKeyName, lField.FieldType.ToString(), JDO_TYPE_DESC[JSONObject[lKeyName].Typ]]);
      end
      else
      begin
        lErrMsg := Format('Invalid class typecast for field "%s" [Actual: %s]',
          [lKeyName, JDO_TYPE_DESC[JSONObject[lKeyName].Typ]]);
      end;
      raise EMVCException.Create(HTTP_STATUS.BadRequest, lErrMsg);
    end;
  end;
end;

procedure TMVCJsonDataObjectsSerializer.JSONObjectToNestedRecordField(const JSONObject: TJsonObject;
  RecordFieldRTTIType: TRttiField; const TypeOffset: Integer; var Buffer: PByte);
var
  lChildType: TRttiType;
  lChildFieldOffset: Integer;
  lKeyName: String;
  lValue: TValue;
  lField: TRttiField;
begin
  if RecordFieldRTTIType.FieldType.TypeKind <> tkRecord then
  begin
    raise EMVCDeserializationException.Create('Only record type allowed');
  end;

  lChildType := RecordFieldRTTIType.FieldType;
  lChildFieldOffset := RecordFieldRTTIType.Offset + TypeOffset;

  for lField in lChildType.GetFields do
  begin
    lKeyName := TMVCSerializerHelper.GetKeyName(lField, lChildType);
    lValue := lField.GetValue(Buffer + lChildFieldOffset);
    JSONObjectPropertyToTValueForRecord(JSONObject, lKeyName, stFields, nil, lValue, nil, lField);
    lField.SetValue(Buffer + lChildFieldOffset, lValue);
  end;
end;

procedure TMVCJsonDataObjectsSerializer.JSONObjectToNestedRecordFieldStatic(const JSONObject: TJsonObject;
  RecordFieldRTTIType: TRttiField; const TypeOffset: Integer; var Buffer: PByte);
var
  lChildType: TRttiType;
  lKeyName: String;
  lValue: TValue;
  lField: TRttiField;
begin
  if RecordFieldRTTIType.FieldType.TypeKind <> tkRecord then
  begin
    raise EMVCDeserializationException.Create('Only record type allowed');
  end;

  // Recupero il tipo e l'offset
  lChildType := RecordFieldRTTIType.FieldType;
  // lChildFieldOffset := RecordFieldRTTIType.Offset + TypeOffset;

  // recupero i campi
  for lField in lChildType.GetFields do
  begin
    lKeyName := TMVCSerializerHelper.GetKeyName(lField, lChildType);
    lValue := lField.GetValue(Buffer); // + lChildFieldOffset);
    JSONObjectPropertyToTValueForRecord(JSONObject, lKeyName, stFields, nil, lValue, nil, lField);
    lField.SetValue(Buffer { + lChildFieldOffset } , lValue);
  end;
end;

procedure TMVCJsonDataObjectsSerializer.ListToJsonArray(const AList: IMVCList; const AJsonArray: TJDOJsonArray;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList;
  const ASerializationAction: TMVCSerializationAction);
var
  I: Integer;
  lDict: IMVCLinks;
  lSer: IMVCTypeSerializer;
  lJsonDataType: TJsonDataType;
  lJSONValue: TJsonBaseObject;
begin
  if not Assigned(AList) then
  begin
    raise EMVCSerializationException.Create('List not assigned');
  end;
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
      lJSONValue := ConvertObjectToJsonValue(AList.GetItem(I), AType, AIgnoredAttributes, nil, ASerializationAction,
        lJsonDataType);
      case lJsonDataType of
        jdtArray:
          begin
            AJsonArray.Add(lJSONValue as TJsonArray);
          end;
        jdtObject:
          begin
            AJsonArray.Add(lJSONValue as TJsonObject);
          end;
      else
        begin
          lJSONValue.Free;
          RaiseSerializationError('Invalid JSON Data Type: ' + GetEnumName(TypeInfo(TJsonDataType),
            Ord(lJsonDataType)));
        end
      end;
    end;
  end;
end;

procedure TMVCJsonDataObjectsSerializer.ObjectToJsonObject(const AObject: TObject; const AJSONObject: TJDOJsonObject;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);
begin
  InternalObjectToJsonObject(AObject, AJSONObject, AType, AIgnoredAttributes, nil, nil, nil);
end;

procedure TMVCJsonDataObjectsSerializer.InternalObjectToJsonObject(
  const AObject: TObject;
  const AJSONObject: TJDOJsonObject;
  const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList;
  const ASerializationAction: TMVCSerializationAction;
  const Links: IMVCLinks;
  const Serializer: IMVCTypeSerializer);
var
  ObjType: TRttiType;
  Prop: TRttiProperty;
  Fld: TRttiField;
begin
  { TODO -oDanieleT -cGeneral : Find a way to automatically add HATEOS }
  if AObject = nil then
  begin
    Exit;
  end;
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
            TValueToJSONObjectProperty(AJSONObject, TMVCSerializerHelper.GetKeyName(Prop, ObjType),
              Prop.GetValue(AObject), AType, AIgnoredAttributes, Prop.GetAttributes);
        end;
      end;
    stFields:
      begin
        for Fld in ObjType.GetFields do
        begin
          if (not TMVCSerializerHelper.HasAttribute<MVCDoNotSerializeAttribute>(Fld)) and
            (not IsIgnoredAttribute(AIgnoredAttributes, Fld.Name)) then
            TValueToJSONObjectProperty(AJSONObject, TMVCSerializerHelper.GetKeyName(Fld, ObjType),
              Fld.GetValue(AObject), AType, AIgnoredAttributes, Fld.GetAttributes);
        end;
      end;
  end;

  if Assigned(ASerializationAction) then
  begin
    ASerializationAction(AObject, Links);
    TJDOLinks(Links).FillJSONArray(AJSONObject.A[TMVCConstants.HATEOAS_PROP_NAME]);
  end;
end;

procedure TMVCJsonDataObjectsSerializer.InternalRecordToJsonObject(const ARecord: Pointer;
  const ARecordTypeInfo: PTypeInfo; const AJSONObject: TJDOJsonObject; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList; const ASerializationAction: TMVCSerializationAction;
  const Links: IMVCLinks; const Serializer: IMVCTypeSerializer);
var
  ObjType: TRttiType;
  Prop: TRttiProperty;
  Fld: TRttiField;
  lKeyName: String;
begin
  { TODO -oDanieleT -cGeneral : Find a way to automatically add HATEOS }
  if ARecord = nil then
  begin
    Exit;
  end;
  ObjType := GetRttiContext.GetType(ARecordTypeInfo);
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
            TValueToJSONObjectProperty(AJSONObject, TMVCSerializerHelper.GetKeyName(Prop, ObjType),
              Prop.GetValue(ARecord), AType, AIgnoredAttributes, Prop.GetAttributes);
        end;
      end;
    stFields:
      begin
        try
          for Fld in ObjType.GetFields do
          begin
            if (not TMVCSerializerHelper.HasAttribute<MVCDoNotSerializeAttribute>(Fld)) and
              (not IsIgnoredAttribute(AIgnoredAttributes, Fld.Name)) then
            begin
              lKeyName := TMVCSerializerHelper.GetKeyName(Fld, ObjType);
              TValueToJSONObjectProperty(AJSONObject, lKeyName, Fld.GetValue(ARecord), AType, AIgnoredAttributes,
                Fld.GetAttributes);
            end;
          end;
        except
          on E: Exception do
          begin
            raise EMVCSerializationException.CreateFmt('Cannot serialize field [%s] - [CLS: %s][MSG: %s]',
              [lKeyName, E.ClassName, E.Message]);
          end;
        end;
      end;
  end;
end;

procedure TMVCJsonDataObjectsSerializer.InternalSerializeDataSet(const ADataSet: TDataSet; const AJsonArray: TJsonArray;
  const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase;
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

procedure TMVCJsonDataObjectsSerializer.InternalSerializeDataSetRecord(const DataSet: TDataSet;
  const JSONObject: TJsonObject; const IgnoredFields: TMVCIgnoredList; const NameCase: TMVCNameCase;
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
      TJDOLinks(lLinks).FillJSONArray(JSONObject.A[TMVCSerializerHelper.ApplyNameCase(lNameCase,
        TMVCConstants.HATEOAS_PROP_NAME)]);
    end;
  finally
    lDataSetFields.Free;
  end;
end;

procedure TMVCJsonDataObjectsSerializer.InternalTValueToJsonObject(const AValue: TValue;
  const AJSONObject: TJDOJsonObject; const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList;
  const ASerializationAction: TMVCSerializationAction; const Links: IMVCLinks; const Serializer: IMVCTypeSerializer);
var
  ObjType: TRttiType;
  Prop: TRttiProperty;
  Fld: TRttiField;
begin
  if AValue.IsEmpty then
  begin
    Exit;
  end;
  if AValue.TypeInfo.Kind <> tkRecord then
  begin
    raise EMVCSerializationException.Create('Expected Record');
  end;

  ObjType := GetRttiContext.GetType(AValue.TypeInfo);
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
            TValueToJSONObjectProperty(AJSONObject, TMVCSerializerHelper.GetKeyName(Prop, ObjType),
              Prop.GetValue(AValue.GetReferenceToRawData), AType, AIgnoredAttributes, Prop.GetAttributes);
        end;
      end;
    stFields:
      begin
        for Fld in ObjType.GetFields do
        begin
          if (not TMVCSerializerHelper.HasAttribute<MVCDoNotSerializeAttribute>(Fld)) and
            (not IsIgnoredAttribute(AIgnoredAttributes, Fld.Name)) then
            TValueToJSONObjectProperty(AJSONObject, TMVCSerializerHelper.GetKeyName(Fld, ObjType),
              Fld.GetValue(AValue.GetReferenceToRawData), AType, AIgnoredAttributes, Fld.GetAttributes);
        end;
      end;
  end;

  // if Assigned(ASerializationAction) then
  // begin
  // ASerializationAction(AObject, Links);
  // TJDOLinks(Links).FillJSONArray(AJsonObject.A[TMVCConstants.HATEOAS_PROP_NAME]);
  // end;
end;

class function TMVCJsonDataObjectsSerializer.Parse<T>(const AString: string): T;
begin
  Result := TJDOJsonObject.Parse(AString) as T;
  if not Assigned(Result) then
    raise EMVCDeserializationException.Create('Cannot parse string as ' + T.ClassName);
end;

class function TMVCJsonDataObjectsSerializer.ParseArray(const AString: string): TJDOJsonArray;
begin
  Result := Parse<TJDOJsonArray>(AString);
end;

class function TMVCJsonDataObjectsSerializer.ParseObject(const AString: string): TJDOJsonObject;
begin
  Result := Parse<TJDOJsonObject>(AString);
end;

procedure TMVCJsonDataObjectsSerializer.ParseStringAsTValueUsingMetadata(
  const AStringValue: String;
  const DestinationTypeInfo: PTypeInfo;
  const ExceptionHintString: String;
  const AAttributes: TArray<TCustomAttribute>;
  var AValue: TValue);
var
  lValueTypeInfo: PTypeInfo;
  lEnumSerType: TMVCEnumSerializationType;
  lEnumAsAttr: MVCEnumSerializationAttribute;
  lEnumMappedValues: TList<string>;
  lMappedValueIndex: Integer;
  lOutInteger: Integer;
  lOutInteger64: Int64;
  lInt: Integer;
begin
  lValueTypeInfo := DestinationTypeInfo;
  if (lValueTypeInfo = System.TypeInfo(TDate)) then
    AValue := TValue.From<TDate>(ISODateToDate(AStringValue))

  else if (lValueTypeInfo = System.TypeInfo(TDateTime)) then
    AValue := TValue.From<TDateTime>(ISOTimeStampToDateTime(AStringValue))

  else if (lValueTypeInfo = System.TypeInfo(TTime)) then
    AValue := TValue.From<TTime>(ISOTimeToTime(AStringValue))
  else if (lValueTypeInfo.Kind = tkRecord) and (lValueTypeInfo <> TypeInfo(TValue)) then { nullables }
  begin
    if lValueTypeInfo = TypeInfo(NullableString) then
    begin
      AValue := TValue.From<NullableString>(NullableString(AStringValue))
    end
    else if lValueTypeInfo = TypeInfo(NullableTDate) then
    begin
      AValue := TValue.From<NullableTDate>(NullableTDate(ISODateToDate(AStringValue)))
    end
    else if lValueTypeInfo = TypeInfo(NullableTDateTime) then
    begin
      AValue := TValue.From<NullableTDateTime>
        (NullableTDateTime(ISOTimeStampToDateTime(AStringValue)))
    end
    else if lValueTypeInfo = TypeInfo(NullableTTime) then
    begin
      AValue := TValue.From<NullableTTime>(NullableTTime(ISOTimeToTime(AStringValue)))
    end
    else if lValueTypeInfo = TypeInfo(NullableTGUID) then
    begin
      AValue := TValue.From<NullableTGUID>(TMVCGuidHelper.StringToGUIDEx(AStringValue));
    end
    else
    begin
      raise EMVCSerializationException.CreateFmt('Cannot deserialize "%s" from string', [ExceptionHintString]);
    end;
  end
  else if (lValueTypeInfo.Kind = tkEnumeration) then
  begin
    lEnumSerType := estEnumName;
    lEnumMappedValues := nil;
    if TMVCSerializerHelper.AttributeExists<MVCEnumSerializationAttribute>(AAttributes, lEnumAsAttr) then
    begin
      lEnumSerType := lEnumAsAttr.SerializationType;
      lEnumMappedValues := lEnumAsAttr.MappedValues;
    end;

    if lEnumSerType = estEnumName then
    begin
      lOutInteger := GetEnumValue(lValueTypeInfo, AStringValue);
      if lOutInteger = -1 then
      begin
        raise EMVCSerializationException.CreateFmt('Cannot deserialize "%s" from mapped values',
          [ExceptionHintString]);
      end;
      TValue.Make(lOutInteger, lValueTypeInfo, AValue)
    end
    else
    begin
      lMappedValueIndex := lEnumMappedValues.IndexOf(AStringValue);
      if lMappedValueIndex < 0 then
        raise EMVCSerializationException.CreateFmt('Cannot deserialize "%s" from mapped values',
          [ExceptionHintString]);

      TValue.Make(GetEnumValue(lValueTypeInfo, GetEnumName(lValueTypeInfo, lMappedValueIndex)),
        lValueTypeInfo, AValue)
    end;
  end
  else if (lValueTypeInfo.Kind = tkInteger) and (TryStrToInt(AStringValue, lOutInteger)) then
  begin
    AValue := lOutInteger;
  end
  else if (lValueTypeInfo.Kind = tkInt64) and (TryStrToInt64(AStringValue, lOutInteger64)) then
  begin
    AValue := lOutInteger64;
  end
  else if lValueTypeInfo.Kind = tkSet then
  begin
    lInt := StringToSet(lValueTypeInfo, StringReplace(AStringValue, ' ', '', [rfReplaceAll]));
    TValue.Make(lInt, lValueTypeInfo, AValue);
  end
  else
  begin
    AValue := TValue.From<string>(AStringValue);
  end;
end;

procedure TMVCJsonDataObjectsSerializer.RecordToJsonObject(const ARecord: Pointer; const ARecordTypeInfo: PTypeInfo;
  const AJSONObject: TJDOJsonObject; const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);
begin
  InternalRecordToJsonObject(ARecord, ARecordTypeInfo, AJSONObject, AType, AIgnoredAttributes, nil, nil, nil);
end;

function TMVCJsonDataObjectsSerializer.SerializeCollection(const AList: TObject; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList; const ASerializationAction: TMVCSerializationAction): string;
var
  JSONArray: TJDOJsonArray;
  ObjList: IMVCList;
  Obj: TObject;
  lLinks: IMVCLinks;
  lSer: IMVCTypeSerializer;
  lObjType: TRttiType;
begin
  Result := EmptyStr;

  if not Assigned(AList) then
    Exit;

  if AList is TJsonBaseObject then
    Exit(TJsonBaseObject(AList).ToJSON(True));

  lObjType := GetRttiContext.GetType(AList.ClassType);

  if GetTypeSerializers.ContainsKey(lObjType.Handle) then
  begin
    GetTypeSerializers.Items[lObjType.Handle].SerializeRoot(AList, TObject(JSONArray), []);
    try
      Result := JSONArray.ToJSON(True);
    finally
      JSONArray.Free;
    end;
    Exit;
  end;

  ObjList := TDuckTypedList.Wrap(AList);
  if Assigned(ObjList) then
  begin
    JSONArray := TJDOJsonArray.Create;
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
          InternalObjectToJsonObject(Obj, JSONArray.AddObject, GetSerializationType(Obj, AType), AIgnoredAttributes,
            ASerializationAction, lLinks, lSer);
        end;
      end
      else
      begin
        for Obj in ObjList do
        begin
          if Obj <> nil then
          begin
            if Obj is TDataSet then
            begin
              DataSetToJsonArray(TDataSet(Obj), JSONArray.AddArray, TMVCNameCase.ncLowerCase, nil,nil,);
            end
            else
            begin
              ObjectToJsonObject(Obj, JSONArray.AddObject, GetSerializationType(Obj, AType), AIgnoredAttributes)
            end;
          end
          else
          begin
            JSONArray.Add(TJsonObject(nil));
          end;
        end;
      end;
      Result := JSONArray.ToJSON(True);
    finally
      JSONArray.Free;
    end;
  end;
end;

function TMVCJsonDataObjectsSerializer.SerializeArrayOfRecord(
  var ATValueContainingAnArray: TValue; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList;
  const ASerializationAction: TMVCSerializationAction): string;
var
  I: Integer;
  lCurrentArrayItem: TValue;
  lJSONArr: TJsonArray;
  lJObj: TJsonObject;
begin
  if not ATValueContainingAnArray.IsArray then
  begin
    raise EMVCSerializationException.Create(String(ATValueContainingAnArray.TypeInfo^.Name) + ' is not an array');
  end;
  if ATValueContainingAnArray.GetArrayLength = 0 then
  begin
    Result := '[]';
  end;

  lJSONArr := TJsonArray.Create;
  try
    for I := 0 to ATValueContainingAnArray.GetArrayLength - 1 do
    begin
      lJObj := lJSONArr.AddObject;
      lCurrentArrayItem := ATValueContainingAnArray.GetArrayElement(I);
      if lCurrentArrayItem.IsObjectInstance then
      begin
        raise EMVCSerializationException.CreateFmt('Found a "%s" while serializing array. Instance types not allowed in arrays - [HINT] Use list of objects instead of array', [lCurrentArrayItem.AsObject.ClassName]);
      end
      else
      begin
        InternalRecordToJsonObject(
          lCurrentArrayItem.GetReferenceToRawData,
          lCurrentArrayItem.TypeInfo,
          lJObj,
          TMVCSerializationType.stFields,
          nil,
          nil,
          nil,
          nil
          );
      end;
    end;
    Result := lJSONArr.ToJSON();
  finally
    lJSONArr.free;
  end;
end;

function TMVCJsonDataObjectsSerializer.SerializeCollection(const AList: IInterface; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList; const ASerializationAction: TMVCSerializationAction): string;
begin
  Result := SerializeCollection(TObject(AList), AType, AIgnoredAttributes, ASerializationAction);
end;

function TMVCJsonDataObjectsSerializer.SerializeDataSet(const ADataSet: TDataSet; const AIgnoredFields: TMVCIgnoredList;
  const ANameCase: TMVCNameCase; const ASerializationAction: TMVCDatasetSerializationAction): string;
var
  JSONArray: TJDOJsonArray;
begin
  Result := EmptyStr;

  if (not Assigned(ADataSet)) then
    Exit('null');
  if ADataSet.IsEmpty then
    Exit('[]'); // https://github.com/danieleteti/delphimvcframework/issues/219

  JSONArray := TJsonArray.Create;
  try
    InternalSerializeDataSet(ADataSet, JSONArray, AIgnoredFields, ANameCase, ASerializationAction);
    Result := JSONArray.ToJSON(True);
  finally
    JSONArray.Free;
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

function TMVCJsonDataObjectsSerializer.SerializeObject(const AObject: TObject; const AType: TMVCSerializationType;
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
    Result := SerializeObject(TObject(AObject), AType, TMVCIgnoredList(LIgnoredAttrs.ToArray), ASerializationAction);
  finally
    LIgnoredAttrs.Free;
  end;
end;

function TMVCJsonDataObjectsSerializer.SerializeObjectToJSON(const AObject: TObject; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList; const ASerializationAction: TMVCSerializationAction): TJDOJsonObject;
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

function TMVCJsonDataObjectsSerializer.SerializeRecord(const ARecord: Pointer; const ARecordTypeInfo: PTypeInfo;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList;
  const ASerializationAction: TMVCSerializationAction): string;
var
  lJSON: TJDOJsonObject;
begin
  lJSON := TJDOJsonObject.Create;
  try
    RecordToJsonObject(ARecord, ARecordTypeInfo, lJSON, TMVCSerializationType.stFields, nil);
    Result := lJSON.ToJSON(True);
  finally
    lJSON.Free;
  end;

end;

function TMVCJsonDataObjectsSerializer.TryMapNullableFloat(var Value: TValue; const JSONDataObject: TJsonObject;
  const AttribName: string): Boolean;
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

function TMVCJsonDataObjectsSerializer.TryNullableToJSON(const AValue: TValue; const AJSONObject: TJDOJsonObject;
  const AName: string; const ACustomAttributes: TArray<TCustomAttribute>): Boolean;
var
  lFoundANullable: Boolean;
begin
  Result := False;
  lFoundANullable := False;
  if (AValue.TypeInfo = System.TypeInfo(NullableString)) then
  begin
    lFoundANullable := True;
    if AValue.AsType<NullableString>().HasValue then
    begin
      AJSONObject.S[AName] := AValue.AsType<NullableString>().Value;
      Result := True;
    end
  end else if (AValue.TypeInfo = System.TypeInfo(NullableInt32)) then
  begin
    lFoundANullable := True;
    if AValue.AsType<NullableInt32>().HasValue then
    begin
      AJSONObject.I[AName] := AValue.AsType<NullableInt32>().Value;
      Result := True;
    end
  end else if (AValue.TypeInfo = System.TypeInfo(NullableInt64)) then
  begin
    lFoundANullable := True;
    if AValue.AsType<NullableInt64>().HasValue then
    begin
      AJSONObject.L[AName] := AValue.AsType<NullableInt64>().Value;
      Result := True;
    end
  end else if (AValue.TypeInfo = System.TypeInfo(NullableInt16)) then
  begin
    lFoundANullable := True;
    if AValue.AsType<NullableInt16>().HasValue then
    begin
      AJSONObject.I[AName] := AValue.AsType<NullableInt16>().Value;
      Result := True;
    end;
  end else if (AValue.TypeInfo = System.TypeInfo(NullableTDate)) then
  begin
    lFoundANullable := True;
    if AValue.AsType<NullableTDate>().HasValue then
    begin
      AJSONObject.S[AName] := DateToISODate(AValue.AsType<NullableTDate>().Value);
      Result := True;
    end;
  end else if (AValue.TypeInfo = System.TypeInfo(NullableTDateTime)) then
  begin
    lFoundANullable := True;
    if AValue.AsType<NullableTDateTime>().HasValue then
    begin
      AJSONObject.S[AName] := DateTimeToISOTimeStamp(AValue.AsType<NullableTDateTime>().Value);
      Result := True;
    end;
  end else if (AValue.TypeInfo = System.TypeInfo(NullableTTime)) then
  begin
    lFoundANullable := True;
    if AValue.AsType<NullableTTime>().HasValue then
    begin
      AJSONObject.S[AName] := TimeToISOTime(AValue.AsType<NullableTTime>().Value);
      Result := True;
    end;
  end else if (AValue.TypeInfo = System.TypeInfo(NullableBoolean)) then
  begin
    lFoundANullable := True;
    if AValue.AsType<NullableBoolean>().HasValue then
    begin
      AJSONObject.B[AName] := AValue.AsType<NullableBoolean>().Value;
      Result := True;
    end;
  end else if (AValue.TypeInfo = System.TypeInfo(NullableCurrency)) then
  begin
    lFoundANullable := True;
    if AValue.AsType<NullableCurrency>().HasValue then
    begin
      AJSONObject.F[AName] := AValue.AsType<NullableCurrency>().Value;
      Result := True;
    end
  end else if (AValue.TypeInfo = System.TypeInfo(NullableSingle)) then
  begin
    lFoundANullable := True;
    if AValue.AsType<NullableSingle>().HasValue then
    begin
      AJSONObject.F[AName] := AValue.AsType<NullableSingle>().Value;
      Result := True;
    end;
  end else if (AValue.TypeInfo = System.TypeInfo(NullableDouble)) then
  begin
    lFoundANullable := True;
    if AValue.AsType<NullableDouble>().HasValue then
    begin
      AJSONObject.F[AName] := AValue.AsType<NullableDouble>().Value;
      Result := True;
    end;
  end else if (AValue.TypeInfo = System.TypeInfo(NullableExtended)) then
  begin
    lFoundANullable := True;
    if AValue.AsType<NullableExtended>().HasValue then
    begin
      AJSONObject.F[AName] := AValue.AsType<NullableExtended>().Value;
      Result := True;
    end;
  end else if (AValue.TypeInfo = System.TypeInfo(NullableUInt16)) then { from here all nullable integers }
  begin
    lFoundANullable := True;
    if AValue.AsType<NullableUInt16>().HasValue then
    begin
      AJSONObject.I[AName] := AValue.AsType<NullableUInt16>().Value;
      Result := True;
    end;
  end else if (AValue.TypeInfo = System.TypeInfo(NullableUInt32)) then
  begin
    lFoundANullable := True;
    if AValue.AsType<NullableUInt32>().HasValue then
    begin
      AJSONObject.I[AName] := AValue.AsType<NullableUInt32>().Value;
      Result := True;
    end;
  end else if (AValue.TypeInfo = System.TypeInfo(NullableUInt64)) then
  begin
    lFoundANullable := True;
    if AValue.AsType<NullableUInt64>().HasValue then
    begin
      AJSONObject.I[AName] := AValue.AsType<NullableUInt64>().Value;
      Result := True;
    end;
  end else if (AValue.TypeInfo = System.TypeInfo(NullableTGUID)) then
  begin
    lFoundANullable := True;
    if AValue.AsType<NullableTGUID>().HasValue then
    begin
      if TMVCSerializerHelper.AttributeExists<MVCSerializeGuidWithoutBracesAttribute>(ACustomAttributes) then
        AJSONObject.S[AName] := TMVCGuidHelper.GUIDToStringEx(AValue.AsType<NullableTGUID>().Value)
      else
        AJSONObject.S[AName] := GUIDToString(AValue.AsType<NullableTGUID>().Value);
      Result := True;
    end;
  end;


  { if the type is a nullable but doesn't contains a value... }
  if lFoundANullable and (not Result) and MVCSerializeNulls then
  begin
    AJSONObject.Values[AName] := nil;
  end;
  { if MVCSerializeEmptyNullableAsNull = False, an empty nullable doesn't have to contains "null"}

  Result := lFoundANullable; {caller needs to know if AJSONObject contains a valid data}
end;

procedure TMVCJsonDataObjectsSerializer.DeserializeObject(const ASerializedObject: string; const AObject: TObject;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList; const ARootNode: string);
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
      GetTypeSerializers.Items[AObject.ClassInfo].DeserializeRoot(SelectRootNodeOrWholeObject(ARootNode, JSONObject),
        AObject, [])
    end
    else
    begin
      JsonObjectToObject(SelectRootNodeOrWholeObject(ARootNode, JSONObject), AObject,
        GetSerializationType(AObject, AType), AIgnoredAttributes);
    end;
  finally
    JSONObject.Free;
  end;
end;

function TMVCJsonDataObjectsSerializer.GetDataSetFields(const ADataSet: TDataSet; const AIgnoredFields: TMVCIgnoredList;
  const ANameCase: TMVCNameCase = ncAsIs): TMVCDataSetFields;
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

procedure TMVCJsonDataObjectsSerializer.AddTValueToJsonArray(const Value: TValue; const JSON: TJDOJsonArray);
var
  lOrdinalValue: Int64;
  lValueAsObj: TObject;
  lTypeName: string;
  lJSONValue: TJsonBaseObject;
  lJsonDataType: TJsonDataType;
begin
  if Value.IsEmpty then
  begin
    JSON.Add(TJsonObject(nil));
    Exit;
  end;

  case Value.Kind of
    tkInteger:
      begin
        JSON.Add(Value.AsInteger);
      end;
    tkFloat:
      begin
{$IFDEF NEXTGEN}
        lTypeName := PChar(Pointer(Value.TypeInfo.Name));
{$ELSE}
        lTypeName := string(Value.TypeInfo.Name);
{$ENDIF}
        if (lTypeName = 'TDate') or (lTypeName = 'TDateTime') or (lTypeName = 'TTime') then
        begin
          JSON.Add(DateTimeToISOTimeStamp(Value.AsExtended));
        end
        else
        begin
          JSON.Add(Value.AsExtended);
        end;
      end;
    tkString, tkUString, tkWChar, tkLString, tkWString:
      begin
        JSON.Add(Value.AsString);
      end;
    tkInt64:
      begin
        JSON.Add(Value.AsInt64);
      end;
    tkEnumeration:
      begin
        if (Value.TypeInfo = System.TypeInfo(Boolean)) then
        begin
          JSON.Add(Value.AsBoolean);
        end
        else
        begin
          Value.TryAsOrdinal(lOrdinalValue);
          JSON.Add(lOrdinalValue);
        end;
      end;
    tkClass, tkInterface:
      begin
        if Value.Kind = tkInterface then
          lValueAsObj := TObject(Value.AsInterface)
        else
          lValueAsObj := Value.AsObject;

        lJSONValue := ConvertObjectToJsonValue(lValueAsObj, GetSerializationType(lValueAsObj), [], nil, nil,
          lJsonDataType);
        case lJsonDataType of
          jdtArray:
            begin
              JSON.Add(TJsonArray(lJSONValue));
            end;
          jdtObject:
            begin
              JSON.Add(TJsonObject(lJSONValue));
            end;
        else
          begin
            lJSONValue.Free;
            RaiseSerializationError('Invalid JSON Type')
          end;
        end;
      end;
  else
    raise EMVCException.Create('Invalid type');
  end;
end;

procedure TValueToJSONObjectPropertyEx(const Value: TValue; const JSON: TJDOJsonObject; const KeyName: string);
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
        lTypeName := PChar(Pointer(Value.TypeInfo.Name));
{$ELSE}
        lTypeName := string(Value.TypeInfo.Name);
{$ENDIF}
        if (lTypeName = 'TDate') or (lTypeName = 'TDateTime') or (lTypeName = 'TTime') then
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
        if (Value.TypeInfo = System.TypeInfo(Boolean)) then
        begin
          JSON.B[KeyName] := Value.AsBoolean;
        end
        else
        begin
          Value.TryAsOrdinal(lOrdinalValue);
          JSON.I[KeyName] := lOrdinalValue;
        end;
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

function StrToJSONObject(const AValue: string; ARaiseExceptionOnError: Boolean): TJDOJsonObject;
var
  lJSON: TJDOJsonObject;
begin
  lJSON := nil;
  try
    lJSON := TJDOJsonObject.Parse(AValue) as TJDOJsonObject;
    if ARaiseExceptionOnError and (lJSON = nil) then
    begin
      raise EMVCException.Create('Invalid JSON');
    end;
    Result := lJSON;
  except
    on E: Exception do
    begin
      lJSON.Free;
      Result := nil;
      if ARaiseExceptionOnError then
      begin
        raise EMVCDeserializationException.Create('Invalid JSON Object - ' + E.Message);
      end;
    end;
  end;
end;

function StrToJSONArray(const AValue: string; ARaiseExceptionOnError: Boolean): TJDOJsonArray;
var
  lJSON: TJDOJsonArray;
begin
  lJSON := nil;
  try
    lJSON := TJDOJsonObject.Parse(AValue) as TJDOJsonArray;
    if ARaiseExceptionOnError and (lJSON = nil) then
    begin
      raise EMVCException.Create('Invalid JSON');
    end;
    Result := lJSON;
  except
    on E: Exception do
    begin
      lJSON.Free;
      Result := nil;
      if ARaiseExceptionOnError then
      begin
        raise EMVCDeserializationException.Create('Invalid JSON Array - ' + E.Message);
      end;
    end;
  end;
end;

procedure JsonObjectToObject(const AJSONObject: TJDOJsonObject; const AObject: TObject);
begin
  JsonObjectToObject(AJSONObject, AObject, TMVCSerializationType.stDefault, nil)
end;

procedure JsonArrayToList(const AJsonArray: TJDOJsonArray; const AList: IMVCList; const AClazz: TClass;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);
var
  lSer: TMVCJsonDataObjectsSerializer;
  I: Integer;
  lObj: TObject;
begin
  lSer := TMVCJsonDataObjectsSerializer.Create;
  try
    for I := 0 to AJsonArray.Count - 1 do
    begin
      lObj := AClazz.Create;
      try
        lSer.JsonObjectToObject(AJsonArray[I].ObjectValue, lObj, TMVCSerializationType.stDefault, nil);
      except
        lObj.Free;
        raise;
      end;
    end;
  finally
    lSer.Free;
  end;
end;

procedure JsonObjectToObject(const AJSONObject: TJDOJsonObject; const AObject: TObject;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);
var
  lSer: TMVCJsonDataObjectsSerializer;
begin
  lSer := TMVCJsonDataObjectsSerializer.Create;
  try
    lSer.JsonObjectToObject(AJSONObject, AObject, AType, AIgnoredAttributes);
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

procedure TJSONObjectHelper.LoadFromString(const Value: string; Encoding: TEncoding; Utf8WithoutBOM: Boolean);
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

{ TMVCRecordHelper }

function TMVCJsonDataObjectsSerializer.JSONObjectToRecord<T>(const JSONObject: TJsonObject): T;
var
  lTypeSize: Integer;
  lTypeInfo: PTypeInfo;
  AIgnoredAttributes: TMVCIgnoredList;
  lKeyName: string;
  lAttributeValue: TValue;
  lErrMsg: string;
  lField: TRttiField;
  lBuffer: PByte;
  lCtx: TRttiContext;
  lRTTIType: TRttiType;
begin
  lCtx := GetRttiContext;
  lRTTIType := lCtx.GetType(TypeInfo(T));
  if not lRTTIType.IsRecord then
  begin
    raise EMVCDeserializationException.Create('Extected record, got ' + lRTTIType.QualifiedName);
  end;

  lTypeInfo := lRTTIType.Handle;
  lBuffer := @Result;
  lField := nil;
  AIgnoredAttributes := [];
  try
    for lField in lRTTIType.GetFields do
      if (not TMVCSerializerHelper.HasAttribute<MVCDoNotDeserializeAttribute>(lField)) and
        (not IsIgnoredAttribute(AIgnoredAttributes, lField.Name)) then
      begin
        lKeyName := TMVCSerializerHelper.GetKeyName(lField, lRTTIType);
        //issue 648
        if lField.FieldType.IsRecord and not lField.FieldType.Handle.NameFld.ToString.StartsWith('Nullable') then
        begin
          JSONObjectToNestedRecordField(JSONObject.O[lKeyName], lField, 0, lBuffer);
        end
        else
        begin
          lAttributeValue := lField.GetValue(lBuffer);
          JSONObjectPropertyToTValueForRecord(JSONObject, lKeyName, TMVCSerializationType.stFields, AIgnoredAttributes,
            lAttributeValue, lField.GetAttributes, lField);
          lField.SetValue(lBuffer, lAttributeValue);
        end;
      end;
  except
    on E: EInvalidCast do
    begin
      if lField <> nil then
      begin
        lErrMsg := Format('Invalid class typecast for field "%s" [Expected: %s, Actual: %s]',
          [lKeyName, lField.FieldType.ToString(), JDO_TYPE_DESC[JSONObject[lKeyName].Typ]]);
      end
      else
      begin
        lErrMsg := Format('Invalid class typecast for field "%s" [Actual: %s]',
          [lKeyName, JDO_TYPE_DESC[JSONObject[lKeyName].Typ]]);
      end;
      raise EMVCException.Create(HTTP_STATUS.BadRequest, lErrMsg);
    end;
  end;
end;

function TMVCJsonDataObjectsSerializer.StrToRecord<T>(const AJSONString: String): T;
var
  lSer: TMVCJsonDataObjectsSerializer;
  LJObj: TJsonObject;
  lBuff: PByte;
begin
  lSer := TMVCJsonDataObjectsSerializer.Create(nil);
  try
    LJObj := StrToJSONObject(AJSONString);
    try
      lBuff := @Result;
      lSer.JSONObjectToNestedRecordField(LJObj, nil, 0, lBuff);
    finally
      LJObj.Free;
    end;
  finally
    lSer.Free;
  end;
end;

{ TJSONUtils }

class function TJSONUtils.JSONArrayToArrayOfRecord<T>(const JSONArray: TJsonArray): TArray<T>;
var
  I: Integer;
  lSer: TMVCJsonDataObjectsSerializer;
begin
  lSer := TMVCJsonDataObjectsSerializer.Create(nil);
  try
    SetLength(Result, JSONArray.Count);
    for I := Low(Result) to High(Result) do
    begin
      Result[I] := JSONObjectToRecord<T>(JSONArray.Items[I].ObjectValue, lSer);
    end;
  finally
    lSer.Free;
  end;
end;

class function TJSONUtils.JSONArrayToListOf<T>(const JSONArray: TJsonArray): TObjectList<T>;
var
  I: Integer;
  lSer: TMVCJsonDataObjectsSerializer;
begin
  lSer := TMVCJsonDataObjectsSerializer.Create(nil);
  try
    Result := TObjectList<T>.Create(True);
    try
      for I := 0 to JSONArray.Count - 1 do
      begin
        Result.Add(JsonObjectToObject<T>(JSONArray.Items[I].ObjectValue));
      end;
    except
      Result.Free;
      raise;

    end;
  finally
    lSer.Free;
  end;
end;

class function TJSONUtils.JsonObjectToObject<T>(const JSONObject: TJsonObject): T;
var
  lSer: TMVCJsonDataObjectsSerializer;
begin
  lSer := TMVCJsonDataObjectsSerializer.Create(nil);
  try
    Result := T.Create;
    try
      lSer.JsonObjectToObject(JSONObject, Result, TMVCSerializationType.stDefault, nil);
    except
      Result.Free;
      raise;
    end;
  finally
    lSer.Free;
  end;
end;

class function TJSONUtils.JSONObjectToRecord<T>(const JSONObject: TJsonObject): T;
var
  lSer: TMVCJsonDataObjectsSerializer;
begin
  lSer := TMVCJsonDataObjectsSerializer.Create(nil);
  try
    Result := JSONObjectToRecord<T>(JSONObject, lSer);
  finally
    lSer.Free;
  end;
end;

class function TJSONUtils.JSONObjectToRecord<T>(const JSONObject: TJsonObject;
  const Serializer: TMVCJsonDataObjectsSerializer): T;
begin
  Result := Serializer.JSONObjectToRecord<T>(JSONObject);
end;

{ TMVCJsonDataObjectsSerializer.TSerializationMetaInfo }

class function TMVCJsonDataObjectsSerializer.TSerializationMetaInfo.CreateFieldsMetaInfo(
  const ADataSet: TDataSet; const ANameCase: TMVCNameCase;
  const AIgnoredFields: TMVCIgnoredList): TSerializationMetaInfo;
var
  lField: TField;
  I: Integer;
  lName: String;
begin
  Result.IgnoredFields := AIgnoredFields;
  Result.NameCase := ANameCase;
  SetLength(Result.FieldsMetaInfo, ADataSet.Fields.Count);
  for I := 0 to ADataSet.FieldCount - 1 do
  begin
    lField := ADataSet.Fields[I];
    lName := GetNameAs(ADataSet.Owner, lField.Name, lField.FieldName);
    Result.FieldsMetaInfo[I].Ignored := IsIgnoredAttribute(AIgnoredFields, lName)
      or (IsIgnoredComponent(ADataSet.Owner, lField.Name));
    Result.FieldsMetaInfo[I].NameAs :=
      TMVCSerializerHelper.ApplyNameCase(
        GetNameCase(ADataSet, ANameCase), lName);
  end;
end;


end.
