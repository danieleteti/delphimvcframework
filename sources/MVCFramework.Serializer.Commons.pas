// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// Collaborators on this file: Ezequiel Juliano Müller (ezequieljuliano@gmail.com)
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

unit MVCFramework.Serializer.Commons;

{$I dmvcframework.inc}
{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  System.Rtti,
  System.Classes,
  System.SysUtils,
  System.DateUtils,
  System.TypInfo,

{$IFDEF SYSTEMNETENCODING}
  System.NetEncoding,

{$ELSE}
  Soap.EncdDecd,

{$ENDIF}
  MVCFramework.Commons,
  Data.DB,
  System.Generics.Collections,
  JsonDataObjects, MVCFramework.DuckTyping;

type
  EMVCSerializationException = class(EMVCException)
  end;

  EMVCDeserializationException = class(EMVCException)
  end;

  TMVCSerializationType = (stUnknown, stDefault, stProperties, stFields);

  TMVCNameCase = (ncAsIs, ncUpperCase, ncLowerCase, ncCamelCase, ncPascalCase, ncSnakeCase);

  TMVCDataType = (dtObject, dtArray);

  TMVCDatasetSerializationType = (dstSingleRecord, dstAllRecords);

  TMVCEnumSerializationType = (estEnumName, estEnumOrd, estEnumMappedValues);

  TMVCIgnoredList = array of string;

  TMVCSerializationAction<T: class> = reference to procedure(const AObject: T;
    const Links: IMVCLinks);
  TMVCSerializationAction = reference to procedure(const AObject: TObject; const Links: IMVCLinks);
  TMVCDataSetSerializationAction = reference to procedure(const ADataSet: TDataset;
    const Links: IMVCLinks);
  TMVCDataSetFieldSerializationAction = reference to procedure(const AField: TField;
    const AJsonObject: TJsonObject; var Handled: Boolean);

  MVCValueAsTypeAttribute = class(TCustomAttribute)
  private
    FValueTypeInfo: PTypeInfo;
  protected
    { protected declarations }
  public
    constructor Create(AValueTypeInfo: PTypeInfo);
    function ValueTypeInfo: PTypeInfo;
  end;

  MVCDoNotSerializeAttribute = class(TCustomAttribute)
  end;

  MVCDoNotDeSerializeAttribute = class(TCustomAttribute)
  end;

  MVCSerializeAsStringAttribute = class(TCustomAttribute)
  private
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }
  end;

  MVCNameCaseAttribute = class(TCustomAttribute)
  private
    FKeyCase: TMVCNameCase;
    function GetKeyCase: TMVCNameCase;
  protected
    { protected declarations }
  public
    constructor Create(const AKeyCase: TMVCNameCase);
    property KeyCase: TMVCNameCase read GetKeyCase;
  end;

  MapperJSONNaming = MVCNameCaseAttribute deprecated 'Use MVCNameCaseAttribute';

  MVCNameAsAttribute = class(TCustomAttribute)
  private
    fName: string;
    fFixed: Boolean;
  protected
    { protected declarations }
  public
    constructor Create(const AName: string; const Fixed: Boolean = False);
    property name: string read fName;
    property Fixed: Boolean read fFixed;
  end;

  MapperJSONSer = MVCNameAsAttribute deprecated 'Use MVCNameAsAttribute';

  MVCListOfAttribute = class(TCustomAttribute)
  private
    FValue: TClass;
  protected
    { protected declarations }
  public
    constructor Create(const AValue: TClass);
    property Value: TClass read FValue;
  end;

  MapperListOfAttribute = MVCListOfAttribute deprecated 'Use MVCListOfAttribute';

  MVCDataSetFieldAttribute = class(TCustomAttribute)
  private
    FDataType: TMVCDataType;
  protected
    { protected declarations }
  public
    constructor Create(const ADataType: TMVCDataType);
    property DataType: TMVCDataType read FDataType;
  end;

  MVCSerializeAttribute = class(TCustomAttribute)
  private
    FSerializationType: TMVCSerializationType;
  protected
    { protected declarations }
  public
    constructor Create(const ASerializationType: TMVCSerializationType);
    property SerializationType: TMVCSerializationType read FSerializationType;
  end;

  MVCColumnAttribute = class(TCustomAttribute)
  private
    FFieldName: string;
    FIsPK: Boolean;
    procedure SetFieldName(const Value: string);
    procedure SetIsPK(const Value: Boolean);
  public
    constructor Create(AFieldName: string; AIsPK: Boolean = False);
    property FieldName: string read FFieldName write SetFieldName;
    property IsPK: Boolean read FIsPK write SetIsPK;
  end;

  MVCEnumSerializationAttribute = class(TCustomAttribute)
  private
    FSerializationType: TMVCEnumSerializationType;
    FMappedValues: TList<string>;
  public
    constructor Create(const ASerializationType: TMVCEnumSerializationType;
      const AMappedValues: string = '');
    destructor Destroy; override;
    property SerializationType: TMVCEnumSerializationType read FSerializationType;
    property MappedValues: TList<string> read FMappedValues;
  end;


  MVCOwnedAttribute = class(TCustomAttribute)
  private
    fClassRef: TClass;
  public
    constructor Create(const ClassRef: TClass = nil);
    property ClassRef: TClass read fClassRef;
  end;
  
  /// <summary>
  ///  Use this attribute in the model class to define a field of type TGuid if at the time of attribute serialization the value
  ///  of the guid field will be obtained without braces.
  ///  Sample: 61013848-8736-4d8b-ad25-91df4c255561
  /// </summary>
  MVCSerializeGuidWithoutBracesAttribute = class(TCustomAttribute);

  TMVCSerializerHelper = record
  private
    { private declarations }
  public
    class function ApplyNameCase(const NameCase: TMVCNameCase; const Value: string): string; static;
    class function GetKeyName(const AField: TRttiField; const AType: TRttiType): string;
      overload; static;
    class function GetKeyName(const AProperty: TRttiProperty; const AType: TRttiType): string;
      overload; static;
    class function HasAttribute<T: class>(const AMember: TRttiObject): Boolean; overload; static;
    class function HasAttribute<T: class>(const AMember: TRttiObject; out AAttribute: T): Boolean;
      overload; static;
    class function AttributeExists<T: TCustomAttribute>(const AAttributes: TArray<TCustomAttribute>;
      out AAttribute: T): Boolean; overload; static;
    class function AttributeExists<T: TCustomAttribute>(const AAttributes: TArray<TCustomAttribute>)
      : Boolean; overload; static;

    class procedure EncodeStream(AInput, AOutput: TStream); static;
    class procedure DecodeStream(AInput, AOutput: TStream); static;
    class function EncodeString(const AInput: string): string; static;
    class function DecodeString(const AInput: string): string; static;
    class procedure DeSerializeStringStream(AStream: TStream; const ASerializedString: string;
      const AEncoding: string); static;
    class procedure DeSerializeBase64StringStream(AStream: TStream;
      const ABase64SerializedString: string); static;
    class function GetTypeKindAsString(const ATypeKind: TTypeKind): string; static;
    class function StringToTypeKind(const AValue: string): TTypeKind; static;
    class function CreateObject(const AObjectType: TRttiType): TObject; overload; static;
    class function CreateObject(const AQualifiedClassName: string): TObject; overload; static;
    class function IsAPropertyToSkip(const aPropName: string): Boolean; static; inline;
  end;


  TMVCLinksCallback = reference to procedure(const Links: TMVCStringDictionary);

  IMVCResponseData = interface
    ['{DF69BE0E-3212-4535-8B78-38EEF0F5B656}']
    function GetMetadata: TMVCStringDictionary;
    property MetaData: TMVCStringDictionary read GetMetadata;
    function GetData: TObject;
    property Data: TObject read GetData;
  end;

  // Well Known Response Objects
  [MVCNameCase(ncLowerCase)]
  TMVCResponseBase = class abstract(TInterfacedObject, IMVCResponseData)
  protected
    function GetMetadata: TMVCStringDictionary; virtual; abstract;
    function GetData: TObject; virtual; abstract;
  end;

  [MVCNameCase(ncLowerCase)]
  TMVCTask = class
  private
    fID: string;
    fHREF: string;
  public
    property HREF: string read fHREF write fHREF;
    property ID: string read fID write fID;
    constructor Create(const HREF, ID: string);
  end;

  [MVCNameCase(ncLowerCase)]
  TMVCAcceptedResponse = class(TMVCResponseBase)
  private
    fTask: TMVCTask;
  public
    property Task: TMVCTask read fTask;
    // constructor Create(const aTask: TMVCTask); overload;
    constructor Create(const HREF, ID: string);
    destructor Destroy; override;
  end;

  [MVCNameCase(ncLowerCase)]
  TMVCResponseData = class(TMVCResponseBase, IMVCResponseData)
  private
    fData: TObject;
    fMetaData: TMVCStringDictionary;
    fOwns: Boolean;
    fDataSetSerializationType: TMVCDatasetSerializationType;
  protected
    function GetMetadata: TMVCStringDictionary; override;
    function GetData: TObject; override;
  public
    constructor Create(const AObject: TObject; const AOwns: Boolean = False;
      const ADataSetSerializationType: TMVCDatasetSerializationType = TMVCDatasetSerializationType.
      dstAllRecords); virtual;
    destructor Destroy; override;
    function SerializationType: TMVCDatasetSerializationType;
    [MVCNameAs('items')]
    property Items: TObject read GetData;
    [MVCNameAs('meta')]
    property MetaData: TMVCStringDictionary read GetMetadata;
  end deprecated 'Use "ObjectDict"';

  TDataObjectHolder = TMVCResponseData deprecated 'Use "ObjectDict"';

  THTTPStatusCode = 100 .. 599;

  TMVCObjectListResponse = class(TMVCResponseData)
  public
    constructor Create(const AObject: TObject; Owns: Boolean = True); reintroduce;
  end;

  TMVCObjectResponse = class(TMVCResponseData)
  public
    constructor Create(const AObject: TObject; Owns: Boolean = True); reintroduce;
  end;

  IMVCObjectDictionary = interface
    ['{B54F02EE-4B3B-4E55-9E6B-FB6CFE746028}']
    function Add(const Name: string; const Value: TObject;
      const SerializationAction: TMVCSerializationAction = nil;
      const AIgnoredFields: TMVCIgnoredList = nil): IMVCObjectDictionary; overload;
    function Add(const Name: string; const Value: TDataset;
      const SerializationAction: TMVCDataSetSerializationAction = nil;
      const DataSetSerializationType: TMVCDatasetSerializationType = dstAllRecords;
      const NameCase: TMVCNameCase = TMVCNameCase.ncLowerCase;
      const AIgnoredFields: TMVCIgnoredList = nil): IMVCObjectDictionary; overload;
    function TryGetValue(const Name: string; out Value: TObject): Boolean; overload;
    function Count: Integer;
    function ContainsKey(const Key: string): Boolean;
    function Keys: TArray<string>;
  end;

  TMVCObjectDictionary = class(TInterfacedObject, IMVCObjectDictionary)
  public
  {
    TMVCSerializationAction = reference to procedure(const AObject: TObject; const Links: IMVCLinks);
    TMVCDataSetSerializationAction = reference to procedure(const ADataSet: TDataset; const Links: IMVCLinks);
  }
    type

    TMVCObjectDictionaryValueItem = class
    private
      fOwns: Boolean;
      fData: TObject;
      fSerializationAction: TMVCSerializationAction;
      fDataSetSerializationAction: TMVCDataSetSerializationAction;
      fDataSetFieldNameCase: TMVCNameCase;
      fDataSetSerializationType: TMVCDatasetSerializationType;
      fIgnoredFields: TMVCIgnoredList;
    public
      constructor Create(const Owns: Boolean; const Data: TObject;
        const SerializationAction: TMVCSerializationAction;
        const AIgnoredFields: TMVCIgnoredList = nil); overload;
      constructor Create(const Owns: Boolean; const Data: TDataset;
        const SerializationAction: TMVCDataSetSerializationAction;
        const DataSetSerializationType: TMVCDatasetSerializationType;
        const NameCase: TMVCNameCase;
        const AIgnoredFields: TMVCIgnoredList = nil); overload;
      destructor Destroy; override;
      property Data: TObject read fData;
      property SerializationAction: TMVCSerializationAction read fSerializationAction;
      property DataSetSerializationAction: TMVCDataSetSerializationAction
        read fDataSetSerializationAction;
      property DataSetFieldNameCase: TMVCNameCase read fDataSetFieldNameCase;
      property IgnoredFields: TMVCIgnoredList read fIgnoredFields;
      property DataSetSerializationType: TMVCDatasetSerializationType
        read fDataSetSerializationType;
    end;
  strict private
    function GetItem(const Key: string): TMVCObjectDictionaryValueItem;
  private
    fOwnsValueItemData: Boolean;
  protected
    fDict: TObjectDictionary<string, TMVCObjectDictionaryValueItem>;
  public
    constructor Create(const OwnsValues: Boolean = True); overload; virtual;
    constructor Create(const aKey: string; const Value: TObject; const OwnsValues: Boolean = True);
      overload; virtual;
    destructor Destroy; override;
    procedure Clear;
    function Add(const Name: string; const Value: TObject;
      const SerializationAction: TMVCSerializationAction = nil;
      const AIgnoredFields: TMVCIgnoredList = nil): IMVCObjectDictionary; overload;
    function Add(const Name: string; const Value: TDataset;
      const SerializationAction: TMVCDataSetSerializationAction = nil;
      const DataSetSerializationType: TMVCDatasetSerializationType = dstAllRecords;
      const NameCase: TMVCNameCase = TMVCNameCase.ncLowerCase;
      const AIgnoredFields: TMVCIgnoredList = nil): IMVCObjectDictionary; overload;
    function TryGetValue(const Name: string; out Value: TObject): Boolean; overload;
    function Count: Integer;
    function ContainsKey(const Key: string): Boolean;
    function Keys: TArray<string>;
    property Items[const Key: string]: TMVCObjectDictionaryValueItem read GetItem; default;
  end;

  IMVCJSONSerializer = interface
    ['{1FB9E04A-D1D6-4C92-B945-257D81B39A25}']
    procedure ObjectToJsonObject(const AObject: TObject; const AJsonObject: TJDOJsonObject;
      const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);
    procedure RecordToJsonObject(const ARecord: Pointer; const ARecordTypeInfo: PTypeInfo;
      const AJsonObject: TJDOJsonObject;
      const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);
    procedure ListToJsonArray(const AList: IMVCList; const AJsonArray: TJDOJsonArray;
      const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList;
      const ASerializationAction: TMVCSerializationAction = nil);
    procedure JsonObjectToObject(const AJsonObject: TJDOJsonObject; const AObject: TObject;
      const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList);
  end;

var
  /// <summary>
  /// Use this variable when you want to convert your local time as UTC or when you receive an UTC ISOTimeStamp and
  /// do not want to apply the time zone when converting.
  /// The default value of gLocalTimeStampAsUTC = False.
  /// </summary>
  /// <example>
  /// * For gLocalTimeStampAsUTC = False and timezone: - 03:00
  /// ISOTimeStamp: 2021-01-11T14:22:17.763Z = DateTime: 2021-01-11 11:22:17.763
  /// DateTime: 2021-01-11 14:22:17.763 = ISOTimeStamp: 2021-01-11T14:22:17.763-03:00
  ///
  /// * For gLocalTimeStampAsUTC = True and timezone: - 03:00
  /// ISOTimeStamp: 2021-01-11T14:22:17.763Z = DateTime: 2021-01-11 14:22:17
  /// DateTime: 2021-01-11 14:22:17.763 = ISOTimeStamp: 2021-01-11T14:22:17.763Z
  /// </example>
  gLocalTimeStampAsUTC: Boolean;

function DateTimeToISOTimeStamp(const ADateTime: TDateTime): string;
function DateToISODate(const ADate: TDateTime): string;
function TimeToISOTime(const ATime: TTime): string;

procedure MapDataSetFieldToRTTIField(const AField: TField; const aRTTIField: TRttiField;
  const AObject: TObject);
function MapDataSetFieldToNullableRTTIField(const AValue: TValue; const AField: TField;
  const aRTTIField: TRttiField; const AObject: TObject): Boolean;
function MapDataSetFieldToNullableRTTIProperty(const AValue: TValue; const AField: TField;
  const aRTTIProp: TRttiProperty; const AObject: TObject): Boolean;

/// <summary>
/// Supports ISO8601 in the following formats:
/// yyyy-mm-ddThh:nn:ss
/// yyyy-mm-ddThh:nn:ss.000Z
/// </summary>
function ISOTimeStampToDateTime(const ADateTime: string): TDateTime;
function ISODateToDate(const ADate: string): TDate;
function ISOTimeToTime(const ATime: string): TTime;

const
  JSONNameLowerCase = ncLowerCase deprecated 'Use MVCNameCaseAttribute(ncLowerCase)';
  JSONNameUpperCase = ncUpperCase deprecated 'Use MVCNameCaseAttribute(ncUpperCase)';

function StrDict: TMVCStringDictionary; overload;
function StrDict(const aKeys: array of string; const aValues: array of string)
  : TMVCStringDictionary; overload;
function ObjectDict(const OwnsValues: Boolean = True): IMVCObjectDictionary;
function GetPaginationMeta(const CurrPageNumber: UInt32; const CurrPageSize: UInt32;
  const DefaultPageSize: UInt32; const URITemplate: string): TMVCStringDictionary;
procedure RaiseSerializationError(const Msg: string);
procedure RaiseDeSerializationError(const Msg: string);

implementation

uses
  Data.FmtBcd,
  MVCFramework.Nullables,
  System.Generics.Defaults;

procedure RaiseSerializationError(const Msg: string);
begin
  raise EMVCSerializationException.Create(Msg) at ReturnAddress;
end;

procedure RaiseDeSerializationError(const Msg: string);
begin
  raise EMVCDeSerializationException.Create(Msg) at ReturnAddress;
end;

function StrDict: TMVCStringDictionary; overload;
begin
  Result := TMVCStringDictionary.Create;
end;

function GetPaginationMeta(const CurrPageNumber: UInt32; const CurrPageSize: UInt32;
  const DefaultPageSize: UInt32; const URITemplate: string): TMVCStringDictionary;
var
  lMetaKeys: array of string;
  lMetaValues: array of string;
begin
  Insert('curr_page', lMetaKeys, 0);
  Insert(CurrPageNumber.ToString(), lMetaValues, 0);

  if CurrPageNumber > 1 then
  begin
    Insert('prev_page_uri', lMetaKeys, 0);
    Insert(Format(URITemplate, [(CurrPageNumber - 1)]), lMetaValues, 0);
  end;

  if CurrPageSize = DefaultPageSize then
  begin
    Insert('next_page_uri', lMetaKeys, 0);
    Insert(Format(URITemplate, [(CurrPageNumber + 1)]), lMetaValues, 0);
  end;
  Result := StrDict(lMetaKeys, lMetaValues);
end;

function ObjectDict(const OwnsValues: Boolean): IMVCObjectDictionary;
begin
  Result := TMVCObjectDictionary.Create(OwnsValues);
end;

function StrDict(const aKeys: array of string; const aValues: array of string)
  : TMVCStringDictionary; overload;
var
  I: Integer;
begin
  if Length(aKeys) <> Length(aValues) then
  begin
    raise EMVCException.CreateFmt('Dict error. Got %d keys but %d values',
      [Length(aKeys), Length(aValues)]);
  end;
  Result := StrDict();
  for I := low(aKeys) to high(aKeys) do
  begin
    Result.Add(aKeys[I], aValues[I]);
  end;
end;

function DateTimeToISOTimeStamp(const ADateTime: TDateTime): string;
begin
  Result := DateToISO8601(ADateTime, gLocalTimeStampAsUTC);
end;

function DateToISODate(const ADate: TDateTime): string;
begin
  Result := FormatDateTime('YYYY-MM-DD', ADate);
end;

function TimeToISOTime(const ATime: TTime): string;
var
  fs: TFormatSettings;
begin
  fs.TimeSeparator := ':';
  Result := FormatDateTime('hh:nn:ss', ATime, fs);
end;

function ISOTimeStampToDateTime(const ADateTime: string): TDateTime;
var
  lDateTime: string;
  lIsUTC: Boolean;
begin
  lDateTime := ADateTime;
  if lDateTime.Length < 19 then
    raise Exception.CreateFmt
      ('Invalid parameter "%s". Hint: DateTime parameters must be formatted in ISO8601 (e.g. 2010-10-12T10:12:23)',
      [ADateTime]);

  if lDateTime.Chars[10] = ' ' then
  begin
    lDateTime := lDateTime.Substring(0, 10) + 'T' + lDateTime.Substring(11);
  end;

  lIsUTC := lDateTime.Length > 19;
  Result := ISO8601ToDate(lDateTime, True);
  if lIsUTC and (not gLocalTimeStampAsUTC) then
  begin
    Result := TTimeZone.Local.ToLocalTime(Result);
  end;
end;

function ISODateToDate(const ADate: string): TDate;
begin
  Result := EncodeDate(StrToInt(Copy(ADate, 1, 4)), StrToInt(Copy(ADate, 6, 2)),
    StrToInt(Copy(ADate, 9, 2)));
end;

function ISOTimeToTime(const ATime: string): TTime;
begin
  Result := EncodeTime(StrToInt(Copy(ATime, 1, 2)), StrToInt(Copy(ATime, 4, 2)),
    StrToInt(Copy(ATime, 7, 2)), 0);
end;

{ TMVCSerializerHelper }

class procedure TMVCSerializerHelper.DeSerializeBase64StringStream(AStream: TStream;
  const ABase64SerializedString: string);
var
  SS: TStringStream;
begin
  AStream.Size := 0;
  SS := TStringStream.Create(ABase64SerializedString, TEncoding.ASCII);
  try
    SS.Position := 0;
    DecodeStream(SS, AStream);
  finally
    SS.Free;
  end;
end;

class procedure TMVCSerializerHelper.DeSerializeStringStream(AStream: TStream;
  const ASerializedString: string; const AEncoding: string);
var
  Encoding: TEncoding;
  SS: TStringStream;
begin
  AStream.Position := 0;
  Encoding := TEncoding.GetEncoding(AEncoding);
  SS := TStringStream.Create(ASerializedString, Encoding);
  try
    SS.Position := 0;
    AStream.CopyFrom(SS, SS.Size);
  finally
    SS.Free;
  end;
end;

class function TMVCSerializerHelper.GetKeyName(const AField: TRttiField;
  const AType: TRttiType): string;
var
  Attrs: TArray<TCustomAttribute>;
  Attr: TCustomAttribute;
begin
  {
    Dear future me...
    Yes, this method is called a lot of times, but after some tests
    seems that the performance loss is very low, so if you don't have any
    new evidence don't try to improve it...
  }
  Result := AField.Name;
  Attrs := AField.GetAttributes;
  for Attr in Attrs do
  begin
    if Attr is MVCNameAsAttribute then
    begin
      Exit(MVCNameAsAttribute(Attr).Name);
    end;
  end;

  Attrs := AType.GetAttributes;
  for Attr in Attrs do
  begin
    if Attr is MVCNameCaseAttribute then
    begin
      Exit(TMVCSerializerHelper.ApplyNameCase(MVCNameCaseAttribute(Attr).KeyCase, AField.Name));
    end;
  end;
end;

class function TMVCSerializerHelper.AttributeExists<T>(const AAttributes: TArray<TCustomAttribute>;
  out AAttribute: T): Boolean;
var
  Att: TCustomAttribute;
begin
  AAttribute := nil;
  for Att in AAttributes do
    if Att is T then
    begin
      AAttribute := T(Att);
      Break;
    end;
  Result := (AAttribute <> nil);
end;

class function TMVCSerializerHelper.ApplyNameCase(const NameCase: TMVCNameCase;
  const Value: string): string;
begin
  case NameCase of
    ncUpperCase:
      begin
        Result := UpperCase(Value);
      end;
    ncLowerCase:
      begin
        Result := LowerCase(Value);
      end;
    ncCamelCase:
      begin
        Result := CamelCase(Value);
      end;
    ncPascalCase:
      begin
        Result := CamelCase(Value, True);
      end;
    ncSnakeCase:
      begin
        Result := SnakeCase(Value);
      end;
    ncAsIs:
      begin
        Result := Value;
      end
  else
    raise Exception.Create('Invalid NameCase');
  end;
end;

class function TMVCSerializerHelper.AttributeExists<T>(const AAttributes
  : TArray<TCustomAttribute>): Boolean;
var
  Att: TCustomAttribute;
begin
  Result := False;
  for Att in AAttributes do
    if Att is T then
      Exit(True);
end;

class function TMVCSerializerHelper.CreateObject(const AObjectType: TRttiType): TObject;
var
  MetaClass: TClass;
  Method: TRttiMethod;
begin
  MetaClass := nil;
  Method := nil;

  for Method in AObjectType.GetMethods do
    if Method.HasExtendedInfo and Method.IsConstructor then
      if Length(Method.GetParameters) = 0 then
      begin
        MetaClass := AObjectType.AsInstance.MetaclassType;
        Break;
      end;

  if Assigned(MetaClass) then
    Result := Method.Invoke(MetaClass, []).AsObject
  else
    raise EMVCException.CreateFmt('Cannot find a propert constructor for %s',
      [AObjectType.ToString]);
end;

class function TMVCSerializerHelper.CreateObject(const AQualifiedClassName: string): TObject;
var
  Context: TRttiContext;
  ObjectType: TRttiType;
begin
{$IF not Defined(TokyoOrBetter)}
  Result := nil;
{$ENDIF}
  Context := TRttiContext.Create;
  try
    ObjectType := Context.FindType(AQualifiedClassName);
    if Assigned(ObjectType) then
      Result := CreateObject(ObjectType)
    else
      raise Exception.CreateFmt
        ('Cannot find RTTI for %s. Hint: Is the specified classtype linked in the module?',
        [AQualifiedClassName]);
  finally
    Context.Free;
  end;
end;

class procedure TMVCSerializerHelper.DecodeStream(AInput, AOutput: TStream);
begin

{$IFDEF SYSTEMNETENCODING}
  TNetEncoding.Base64.Decode(AInput, AOutput);

{$ELSE}
  Soap.EncdDecd.DecodeStream(AInput, AOutput);

{$ENDIF}
end;

class function TMVCSerializerHelper.DecodeString(const AInput: string): string;
begin

{$IFDEF SYSTEMNETENCODING}
  Result := TNetEncoding.Base64.Decode(AInput);

{$ELSE}
  Result := Soap.EncdDecd.DecodeString(AInput);

{$ENDIF}
end;

class procedure TMVCSerializerHelper.EncodeStream(AInput, AOutput: TStream);
begin

{$IFDEF SYSTEMNETENCODING}
  TNetEncoding.Base64.Encode(AInput, AOutput);

{$ELSE}
  Soap.EncdDecd.EncodeStream(AInput, AOutput);

{$ENDIF}
end;

class function TMVCSerializerHelper.EncodeString(const AInput: string): string;
begin

{$IFDEF SYSTEMNETENCODING}
  Result := TNetEncoding.Base64.Encode(AInput);

{$ELSE}
  Result := Soap.EncdDecd.EncodeString(AInput);

{$ENDIF}
end;

class function TMVCSerializerHelper.GetKeyName(const AProperty: TRttiProperty;
  const AType: TRttiType): string;
var
  Attrs: TArray<TCustomAttribute>;
  Attr: TCustomAttribute;
begin
  { TODO -oDanieleT -cGeneral : in un rendering di una lista, quante volte viene chiamata questa funzione? }
  { Tante volte, ma eliminando tutta la logica si guadagnerebbe al massiom il 6% nel caso tipico, forse non vale la pena di aggiungere una cache apposita }
  Result := AProperty.Name;

  Attrs := AProperty.GetAttributes;
  for Attr in Attrs do
  begin
    if Attr is MVCNameAsAttribute then
    begin
      Result := MVCNameAsAttribute(Attr).Name;
      if MVCNameAsAttribute(Attr).Fixed then { if FIXED the attribute NameAs remains untouched }
      begin
        Exit
      end
      else
      begin
        Break;
      end;
    end;
  end;

  Attrs := AType.GetAttributes;
  for Attr in Attrs do
  begin
    if Attr is MVCNameCaseAttribute then
    begin
      Exit(TMVCSerializerHelper.ApplyNameCase(MVCNameCaseAttribute(Attr).KeyCase, Result));
    end;
  end;
end;

class function TMVCSerializerHelper.GetTypeKindAsString(const ATypeKind: TTypeKind): string;
begin
  Result := GetEnumName(TypeInfo(TTypeKind), Ord(ATypeKind));
  Result := Result.Remove(0, 2).ToLower;
end;

class function TMVCSerializerHelper.HasAttribute<T>(const AMember: TRttiObject): Boolean;
var
  Attrs: TArray<TCustomAttribute>;
  Attr: TCustomAttribute;
begin
  Result := False;
  Attrs := AMember.GetAttributes;
  if Length(Attrs) = 0 then
    Exit(False);
  for Attr in Attrs do
    if Attr is T then
      Exit(True);
end;

class function TMVCSerializerHelper.HasAttribute<T>(const AMember: TRttiObject;
  out AAttribute: T): Boolean;
var
  Attrs: TArray<TCustomAttribute>;
  Attr: TCustomAttribute;
begin
  AAttribute := nil;
  Result := False;
  Attrs := AMember.GetAttributes;
  for Attr in Attrs do
    if Attr is T then
    begin
      AAttribute := T(Attr);
      Exit(True);
    end;
end;

class function TMVCSerializerHelper.IsAPropertyToSkip(const aPropName: string): Boolean;
begin
  Result := (aPropName = 'RefCount') or (aPropName = 'Disposed');
end;

class function TMVCSerializerHelper.StringToTypeKind(const AValue: string): TTypeKind;
begin
  Result := TTypeKind(GetEnumValue(TypeInfo(TTypeKind), 'tk' + AValue));
end;

{ MVCValueAsTypeAttribute }

constructor MVCValueAsTypeAttribute.Create(AValueTypeInfo: PTypeInfo);
begin
  inherited Create;
  FValueTypeInfo := AValueTypeInfo;
end;

function MVCValueAsTypeAttribute.ValueTypeInfo: PTypeInfo;
begin
  Result := FValueTypeInfo;
end;

{ MVCNameCaseAttribute }

constructor MVCNameCaseAttribute.Create(const AKeyCase: TMVCNameCase);
begin
  inherited Create;
  FKeyCase := AKeyCase;
end;

function MVCNameCaseAttribute.GetKeyCase: TMVCNameCase;
begin
  Result := FKeyCase;
end;

{ MVCNameAsAttribute }

constructor MVCNameAsAttribute.Create(const AName: string; const Fixed: Boolean = False);
begin
  inherited Create;
  fName := AName;
  fFixed := Fixed;
end;

{ MVCListOfAttribute }

constructor MVCListOfAttribute.Create(const AValue: TClass);
begin
  inherited Create;
  FValue := AValue;
end;

{ MVCDataSetFieldAttribute }

constructor MVCDataSetFieldAttribute.Create(const ADataType: TMVCDataType);
begin
  inherited Create;
  FDataType := ADataType;
end;

{ MVCSerializeAttribute }

constructor MVCSerializeAttribute.Create(const ASerializationType: TMVCSerializationType);
begin
  inherited Create;
  FSerializationType := ASerializationType;
end;

{ MVCColumnAttribute }

constructor MVCColumnAttribute.Create(AFieldName: string; AIsPK: Boolean);
begin
  inherited Create;
  FFieldName := AFieldName;
  FIsPK := AIsPK;
end;

procedure MVCColumnAttribute.SetFieldName(const Value: string);
begin
  FFieldName := Value;
end;

procedure MVCColumnAttribute.SetIsPK(const Value: Boolean);
begin
  FIsPK := Value;
end;

{ MVCEnumSerializationTypeAttribute }

constructor MVCEnumSerializationAttribute.Create(const ASerializationType
  : TMVCEnumSerializationType; const AMappedValues: string);
begin
  FMappedValues := TList<string>.Create(TDelegatedComparer<string>.Create(
    function(const Left, Right: string): Integer
    begin
      Result := CompareText(Left, Right);
    end));

  FSerializationType := ASerializationType;

  if (FSerializationType = estEnumMappedValues) then
  begin
    if AMappedValues.Trim.IsEmpty then
      raise EMVCException.Create('Mapped values are required for estEnumMappedValues type.');

    FMappedValues.AddRange(AMappedValues.Split([',', ';', ' ']));
  end;
end;

destructor MVCEnumSerializationAttribute.Destroy;
begin
  FMappedValues.Free;
  inherited;
end;

{ TMVCTask }

constructor TMVCTask.Create(const HREF, ID: string);
begin
  inherited Create;
  fHREF := HREF;
  fID := ID;
end;

{ TMVCAcceptedResponse }

// constructor TMVCAcceptedResponse.Create(const aTask: TMVCTask);
// begin
// inherited Create;
// fTask := aTask;
// end;

constructor TMVCAcceptedResponse.Create(const HREF, ID: string);
begin
  inherited Create;
  fTask := TMVCTask.Create(HREF, ID);
end;

destructor TMVCAcceptedResponse.Destroy;
begin
  fTask.Free;
  inherited;
end;

{ TObjectResponseBase }

constructor TMVCResponseData.Create(const AObject: TObject; const AOwns: Boolean;
const ADataSetSerializationType: TMVCDatasetSerializationType);
begin
  inherited Create;
  fData := AObject;
  fMetaData := TMVCStringDictionary.Create;
  fOwns := AOwns;
  fDataSetSerializationType := ADataSetSerializationType;
end;

destructor TMVCResponseData.Destroy;
begin
  fMetaData.Free;
  if fOwns then
  begin
    fData.Free;
  end;
  inherited;
end;

function TMVCResponseData.GetData: TObject;
begin
  Result := fData;
end;

function TMVCResponseData.GetMetadata: TMVCStringDictionary;
begin
  Result := fMetaData;
end;

function TMVCResponseData.SerializationType: TMVCDatasetSerializationType;
begin
  Result := fDataSetSerializationType;
end;

{ TMVCObjectListResponse }

constructor TMVCObjectListResponse.Create(const AObject: TObject; Owns: Boolean);
begin
  inherited Create(AObject, Owns, dstAllRecords);
end;

{ TMVCObjectResponse }

constructor TMVCObjectResponse.Create(const AObject: TObject; Owns: Boolean = True);
begin
  inherited Create(AObject, Owns, dstSingleRecord);
end;

procedure MapDataSetFieldToRTTIField(const AField: TField; const aRTTIField: TRttiField;
const AObject: TObject);
var
  lInternalStream: TStream;
  lSStream: TStringStream;
  lValue: TValue;
  lStrValue: string;
{$IF not Defined(TokyoOrBetter)}
  lFieldValue: string;
{$ENDIF}
begin
  lValue := aRTTIField.GetValue(AObject);
  if lValue.Kind = tkRecord then
  begin
    if MapDataSetFieldToNullableRTTIField(lValue, AField, aRTTIField, AObject) then
    begin
      Exit;
    end;
  end;

  // if we reached this point, the field is not a nullable type...
  case AField.DataType of
    ftString, ftWideString:
      begin
        // mysql tinytext is identified as string, but raises an Invalid Class Cast
        // so we need to do some more checks...
        case aRTTIField.FieldType.TypeKind of
          tkString, tkUString:
            begin
              aRTTIField.SetValue(AObject, AField.AsString);
            end;
          tkWideString:
            begin
              aRTTIField.SetValue(AObject, AField.AsWideString);
            end;
          tkRecord:
            begin
              if TypeInfo(TGUID) = aRTTIField.FieldType.Handle then
              begin
                aRTTIField.SetValue(AObject, TValue.From<TGUID>(StringToGUID(AField.AsString)));
              end
              else
              begin
                raise EMVCException.CreateFmt('Unsupported record type: %s.%s', [aRTTIField.Parent.Name, aRTTIField.Name]);
              end;
            end;
          tkClass: { mysql - maps a tiny field, identified as string, into a TStream }
            begin
              lInternalStream := aRTTIField.GetValue(AObject).AsObject as TStream;
              if lInternalStream = nil then
              begin
                raise EMVCException.CreateFmt
                  ('Property target for %s field is nil. [HINT] Initialize the stream before load data',
                  [AField.FieldName]);
              end;
              lInternalStream.Size := 0;
              lStrValue := AField.AsString;
              if not lStrValue.IsEmpty then
              begin
                lInternalStream.Write(lStrValue, Length(lStrValue));
                lInternalStream.Position := 0;
              end;
            end
        else
          begin
            raise EMVCException.CreateFmt('Unsupported FieldType (%d) for field %s',
              [Ord(AField.DataType), AField.FieldName]);
          end;
        end;
        // aRTTIField.SetValue(AObject, AField.AsString);
      end;
    ftLargeint, ftAutoInc:
      begin
        aRTTIField.SetValue(AObject, AField.AsLargeInt);
      end;
    ftInteger, ftSmallint, ftShortint, ftByte:
      begin
        // sqlite doesn't support boolean, so are identified as integers
        // so we need to do some more checks...
        if (aRTTIField.FieldType.TypeKind = tkEnumeration) and (aRTTIField.FieldType.Handle = TypeInfo(Boolean)) then
        begin
          aRTTIField.SetValue(AObject, AField.AsInteger = 1);
        end
		else if (aRTTIField.FieldType.TypeKind = tkEnumeration) then
        begin
          TValue(AField.AsInteger).ExtractRawData(PByte(Pointer(AObject)) + aRTTIField.Offset);
        end
        else
        begin
          aRTTIField.SetValue(AObject, AField.AsInteger);
        end;
      end;
    ftLongWord, ftWord:
      begin
        aRTTIField.SetValue(AObject, AField.AsLongWord);
      end;
    ftCurrency:
      begin
        aRTTIField.SetValue(AObject, AField.AsCurrency);
      end;
    ftFMTBcd:
      begin
        aRTTIField.SetValue(AObject, BCDtoCurrency(AField.AsBCD));
      end;
    ftDate:
      begin
        aRTTIField.SetValue(AObject, Trunc(AField.AsDateTime));
      end;
    ftDateTime:
      begin
        aRTTIField.SetValue(AObject, AField.AsDateTime);
      end;
    ftTime:
      begin
        aRTTIField.SetValue(AObject, Frac(AField.AsDateTime));
      end;
    ftTimeStamp:
      begin
        aRTTIField.SetValue(AObject, AField.AsDateTime);
      end;
    ftBoolean:
      begin
        aRTTIField.SetValue(AObject, AField.AsBoolean);
      end;
    ftMemo, ftWideMemo:
      begin
        case aRTTIField.FieldType.TypeKind of
          tkString, tkUString:
            begin
              {TODO -oDanieleT -cGeneral : Optimize this code... too complex}
              if AField.DataType = ftMemo then
                aRTTIField.SetValue(AObject, TMemoField(AField).AsWideString)
              else if AField.DataType = ftWideMemo then
                aRTTIField.SetValue(AObject, TWideMemoField(AField).AsWideString)
              else
              begin
                //https://github.com/danieleteti/delphimvcframework/issues/490 (24 nov 2022)
                //lSStream := TStringStream.Create('', TEncoding.Unicode);
                lSStream := TStringStream.Create('');
                try
                  TBlobField(AField).SaveToStream(lSStream);
                  aRTTIField.SetValue(AObject, lSStream.DataString);
                finally
                  lSStream.Free;
                end;
              end;
            end;
          tkFloat: { sqlite - date types stored as text }
            begin
              if TypeInfo(TDate) = aRTTIField.FieldType.Handle then
              begin
                aRTTIField.SetValue(AObject, ISODateToDate(AField.AsString));
              end
              else if TypeInfo(TDateTime) = aRTTIField.FieldType.Handle then
              begin
                aRTTIField.SetValue(AObject, ISOTimeStampToDateTime(AField.AsString));
              end
              else if TypeInfo(TTime) = aRTTIField.FieldType.Handle then
              begin
                aRTTIField.SetValue(AObject, ISOTimeToTime(AField.AsString));
              end
              else
              begin
                RaiseDeSerializationError('Cannot deserialize field ' +
                  AField.FieldName);
              end;
            end;
          tkRecord:
            begin
              if TypeInfo(TGUID) = aRTTIField.FieldType.Handle then
              begin
                aRTTIField.SetValue(AObject, TValue.From<TGUID>(StringToGUID(AField.AsString)));
              end
              else
              begin
                raise EMVCException.CreateFmt('Unsupported record type: %s.%s', [aRTTIField.Parent.Name, aRTTIField.Name]);
              end;
            end
        else
          begin
            // In case you want to map a binary blob into a Delphi Stream
            lInternalStream := aRTTIField.GetValue(AObject).AsObject as TStream;
            if lInternalStream = nil then
            begin
              raise EMVCException.CreateFmt('Property target for %s field is nil',
                [AField.FieldName]);
            end;
            lInternalStream.Position := 0;
            TBlobField(AField).SaveToStream(lInternalStream);
            lInternalStream.Position := 0;
          end;
        end;
      end;
    ftBCD:
      begin
        aRTTIField.SetValue(AObject, BCDtoCurrency(AField.AsBCD));
      end;
    ftFloat, ftSingle:
      begin
        aRTTIField.SetValue(AObject, AField.AsFloat);
      end;
    ftBlob:
      begin
        lInternalStream := aRTTIField.GetValue(AObject).AsObject as TStream;
        if AField.IsNull then
        begin
          lInternalStream.Free;
          aRTTIField.SetValue(AObject, nil);
          Exit;
        end;
        if lInternalStream = nil then
        begin
          lInternalStream := TMemoryStream.Create;
          aRTTIField.SetValue(AObject, lInternalStream);
        end;
        lInternalStream.Position := 0;
        TBlobField(AField).SaveToStream(lInternalStream);
        lInternalStream.Position := 0;
      end;
    ftGuid:
      begin
{$IF Defined(TokyoOrBetter)}
        if AField.IsNull then
        begin
          aRTTIField.SetValue(AObject, TValue.Empty)
        end
        else if TypeInfo(NullableTGUID) = aRTTIField.FieldType.Handle then
        begin
          aRTTIField.SetValue(AObject, TValue.From<NullableTGUID>(AField.AsGuid));
        end
        else
        begin
          aRTTIField.SetValue(AObject, TValue.From<TGUID>(AField.AsGuid));
        end;
{$ELSE}
        lFieldValue := AField.AsString;
        if lFieldValue.IsEmpty then
        begin
          lFieldValue := '{00000000-0000-0000-0000-000000000000}';
        end;
        aRTTIField.SetValue(AObject, TValue.From<TGUID>(StringToGUID(lFieldValue)));
{$ENDIF}
      end;
    ftDBaseOle: // xml
      begin
        lSStream := TStringStream.Create('', TEncoding.Unicode);
        try
          TBlobField(AField).SaveToStream(lSStream);
          aRTTIField.SetValue(AObject, lSStream.DataString);
        finally
          lSStream.Free;
        end;
      end
  else
    raise EMVCException.CreateFmt('Unsupported FieldType (%d) for field %s',
      [Ord(AField.DataType), AField.FieldName]);
  end;
end;

function MapDataSetFieldToNullableRTTIField(const AValue: TValue; const AField: TField;
const aRTTIField: TRttiField; const AObject: TObject): Boolean;
var
  lStr: string;
begin
  Assert(AValue.Kind = tkRecord);
  Result := False;
  if AValue.IsType(TypeInfo(NullableString)) then
  begin
    if AField.IsNull then
    begin
      aRTTIField.GetValue(AObject).AsType<NullableString>().Clear;
    end
    else
    begin
      aRTTIField.SetValue(AObject, TValue.From<NullableString>(AField.AsString));
    end;
    Result := True;
  end
  else if AValue.IsType(TypeInfo(NullableInt32)) then
  begin
    if AField.IsNull then
    begin
      aRTTIField.GetValue(AObject).AsType<NullableInt32>().Clear;
    end
    else
    begin
      aRTTIField.SetValue(AObject, TValue.From<NullableInt32>(AField.AsLargeInt));
    end;
    Result := True;
  end
  else if AValue.IsType(TypeInfo(NullableUInt32)) then
  begin
    if AField.IsNull then
    begin
      aRTTIField.GetValue(AObject).AsType<NullableUInt32>().Clear;
    end
    else
    begin
      aRTTIField.SetValue(AObject, TValue.From<NullableUInt32>(AField.AsLargeInt));
    end;
    Result := True;
  end
  else if AValue.IsType(TypeInfo(NullableInt64)) then
  begin
    if AField.IsNull then
    begin
      aRTTIField.GetValue(AObject).AsType<NullableInt64>().Clear;
    end
    else
    begin
      aRTTIField.SetValue(AObject, TValue.From<NullableInt64>(AField.AsLargeInt));
    end;
    Result := True;
  end
  else if AValue.IsType(TypeInfo(NullableUInt64)) then
  begin
    if AField.IsNull then
    begin
      aRTTIField.GetValue(AObject).AsType<NullableUInt64>().Clear;
    end
    else
    begin
      aRTTIField.SetValue(AObject, TValue.From<NullableUInt64>(AField.AsLargeInt));
    end;
    Result := True;
  end
  else if AValue.IsType(TypeInfo(NullableInt16)) then
  begin
    if AField.IsNull then
    begin
      aRTTIField.GetValue(AObject).AsType<NullableInt16>().Clear;
    end
    else
    begin
      aRTTIField.SetValue(AObject, TValue.From<NullableInt16>(AField.AsLargeInt));
    end;
    Result := True;
  end
  else if AValue.IsType(TypeInfo(NullableUInt16)) then
  begin
    if AField.IsNull then
    begin
      aRTTIField.GetValue(AObject).AsType<NullableUInt16>().Clear;
    end
    else
    begin
      aRTTIField.SetValue(AObject, TValue.From<NullableUInt16>(AField.AsInteger));
    end;
    Result := True;
  end
  else if AValue.IsType(TypeInfo(NullableTDate)) then
  begin
    if AField.IsNull then
    begin
      aRTTIField.GetValue(AObject).AsType<NullableTDate>().Clear;
    end
    else
    begin
      if not (AField.DataType in [ftWideMemo]) then
      begin
        aRTTIField.SetValue(AObject, TValue.From<NullableTDate>(AField.AsDateTime));
      end
      else
      begin
        {SQLite case...}
        lStr := AField.AsWideString;
        aRTTIField.SetValue(AObject, TValue.From<NullableTDate>(ISODateToDate(lStr)));
      end;
    end;
    Result := True;
  end
  else if AValue.IsType(TypeInfo(NullableTDateTime)) then
  begin
    if AField.IsNull then
    begin
      aRTTIField.GetValue(AObject).AsType<NullableTDateTime>().Clear;
    end
    else
    begin
      if not (AField.DataType in [ftWideMemo]) then
      begin
        aRTTIField.SetValue(AObject, TValue.From<NullableTDateTime>(AField.AsDateTime));
      end
      else
      begin
        {SQLite case...}
        lStr := AField.AsWideString;
        aRTTIField.SetValue(AObject, TValue.From<NullableTDateTime>(ISOTimeStampToDateTime(lStr)));
      end;
    end;
    Result := True;
  end
  else if AValue.IsType(TypeInfo(NullableTTime)) then
  begin
    if AField.IsNull then
    begin
      aRTTIField.GetValue(AObject).AsType<NullableTTime>().Clear;
    end
    else
    begin
      if not (AField.DataType in [ftWideMemo]) then
      begin
        aRTTIField.SetValue(AObject, TValue.From<NullableTTime>(AField.AsDateTime));
      end
      else
      begin
        {SQLite case...}
        lStr := AField.AsWideString;
        aRTTIField.SetValue(AObject, TValue.From<NullableTTime>(ISOTimeToTime(lStr)));
      end;
    end;
    Result := True;
  end
  else if AValue.IsType(TypeInfo(NullableBoolean)) then
  begin
    if AField.IsNull then
    begin
      aRTTIField.GetValue(AObject).AsType<NullableBoolean>().Clear;
    end
    else
    begin
      aRTTIField.SetValue(AObject, TValue.From<NullableBoolean>(AField.AsBoolean));
    end;
    Result := True;
  end
  else if AValue.IsType(TypeInfo(NullableDouble)) then
  begin
    if AField.IsNull then
    begin
      aRTTIField.GetValue(AObject).AsType<NullableDouble>().Clear;
    end
    else
    begin
      aRTTIField.SetValue(AObject, TValue.From<NullableDouble>(AField.AsFloat));
    end;
    Result := True;
  end
  else if AValue.IsType(TypeInfo(NullableSingle)) then
  begin
    if AField.IsNull then
    begin
      aRTTIField.GetValue(AObject).AsType<NullableSingle>().Clear;
    end
    else
    begin
      aRTTIField.SetValue(AObject, TValue.From<NullableSingle>(AField.AsSingle));
    end;
    Result := True;
  end
  else if AValue.IsType(TypeInfo(NullableExtended)) then
  begin
    if AField.IsNull then
    begin
      aRTTIField.GetValue(AObject).AsType<NullableExtended>().Clear;
    end
    else
    begin
      aRTTIField.SetValue(AObject, TValue.From<NullableExtended>(AField.AsExtended));
    end;
    Result := True;
  end
  else if AValue.IsType(TypeInfo(NullableCurrency)) then
  begin
    if AField.IsNull then
    begin
      aRTTIField.GetValue(AObject).AsType<NullableCurrency>().Clear;
    end
    else
    begin
      aRTTIField.SetValue(AObject, TValue.From<NullableCurrency>(AField.AsCurrency));
    end;
    Result := True;
  end
  else if AValue.IsType(TypeInfo(NullableTGUID)) then
  begin
    if AField.IsNull then
    begin
      aRTTIField.GetValue(AObject).AsType<NullableTGUID>().Clear;
    end
    else
    begin
{$IF defined(TOKYOORBETTER)}
      if AField.DataType = ftGuid then
        aRTTIField.SetValue(AObject, TValue.From<NullableTGUID>(AField.AsGuid))
      else
{$ENDIF}
        aRTTIField.SetValue(AObject, TValue.From<NullableTGUID>(StringToGUID(AField.AsString)))
    end;
    Result := True;
  end
end;

function MapDataSetFieldToNullableRTTIProperty(const AValue: TValue; const AField: TField;
const aRTTIProp: TRttiProperty; const AObject: TObject): Boolean;
begin
  Assert(AValue.Kind = tkRecord);
  Result := False;
  if AValue.IsType(TypeInfo(NullableString)) then
  begin
    if AField.IsNull then
    begin
      aRTTIProp.GetValue(AObject).AsType<NullableString>().Clear;
    end
    else
    begin
      aRTTIProp.SetValue(AObject, TValue.From<NullableString>(AField.AsString));
    end;
    Result := True;
  end
  else if AValue.IsType(TypeInfo(NullableInt32)) then
  begin
    if AField.IsNull then
    begin
      aRTTIProp.GetValue(AObject).AsType<NullableInt32>().Clear;
    end
    else
    begin
      aRTTIProp.SetValue(AObject, TValue.From<NullableInt32>(AField.AsLargeInt));
    end;
    Result := True;
  end
  else if AValue.IsType(TypeInfo(NullableUInt32)) then
  begin
    if AField.IsNull then
    begin
      aRTTIProp.GetValue(AObject).AsType<NullableUInt32>().Clear;
    end
    else
    begin
      aRTTIProp.SetValue(AObject, TValue.From<NullableUInt32>(AField.AsLargeInt));
    end;
    Result := True;
  end
  else if AValue.IsType(TypeInfo(NullableInt64)) then
  begin
    if AField.IsNull then
    begin
      aRTTIProp.GetValue(AObject).AsType<NullableInt64>().Clear;
    end
    else
    begin
      aRTTIProp.SetValue(AObject, TValue.From<NullableInt64>(AField.AsLargeInt));
    end;
    Result := True;
  end
  else if AValue.IsType(TypeInfo(NullableUInt64)) then
  begin
    if AField.IsNull then
    begin
      aRTTIProp.GetValue(AObject).AsType<NullableUInt64>().Clear;
    end
    else
    begin
      aRTTIProp.SetValue(AObject, TValue.From<NullableUInt64>(AField.AsLargeInt));
    end;
    Result := True;
  end
  else if AValue.IsType(TypeInfo(NullableInt16)) then
  begin
    if AField.IsNull then
    begin
      aRTTIProp.GetValue(AObject).AsType<NullableInt16>().Clear;
    end
    else
    begin
      aRTTIProp.SetValue(AObject, TValue.From<NullableInt16>(AField.AsLargeInt));
    end;
    Result := True;
  end
  else if AValue.IsType(TypeInfo(NullableUInt16)) then
  begin
    if AField.IsNull then
    begin
      aRTTIProp.GetValue(AObject).AsType<NullableUInt16>().Clear;
    end
    else
    begin
      aRTTIProp.SetValue(AObject, TValue.From<NullableUInt16>(AField.AsInteger));
    end;
    Result := True;
  end
  else if AValue.IsType(TypeInfo(NullableTDate)) then
  begin
    if AField.IsNull then
    begin
      aRTTIProp.GetValue(AObject).AsType<NullableTDate>().Clear;
    end
    else
    begin
      aRTTIProp.SetValue(AObject, TValue.From<NullableTDate>(AField.AsDateTime));
    end;
    Result := True;
  end
  else if AValue.IsType(TypeInfo(NullableTDateTime)) then
  begin
    if AField.IsNull then
    begin
      aRTTIProp.GetValue(AObject).AsType<NullableTDateTime>().Clear;
    end
    else
    begin
      aRTTIProp.SetValue(AObject, TValue.From<NullableTDateTime>(AField.AsDateTime));
    end;
    Result := True;
  end
  else if AValue.IsType(TypeInfo(NullableTTime)) then
  begin
    if AField.IsNull then
    begin
      aRTTIProp.GetValue(AObject).AsType<NullableTTime>().Clear;
    end
    else
    begin
      aRTTIProp.SetValue(AObject, TValue.From<NullableTTime>(AField.AsDateTime));
    end;
    Result := True;
  end
  else if AValue.IsType(TypeInfo(NullableBoolean)) then
  begin
    if AField.IsNull then
    begin
      aRTTIProp.GetValue(AObject).AsType<NullableBoolean>().Clear;
    end
    else
    begin
      aRTTIProp.SetValue(AObject, TValue.From<NullableBoolean>(AField.AsBoolean));
    end;
    Result := True;
  end
  else if AValue.IsType(TypeInfo(NullableDouble)) then
  begin
    if AField.IsNull then
    begin
      aRTTIProp.GetValue(AObject).AsType<NullableDouble>().Clear;
    end
    else
    begin
      aRTTIProp.SetValue(AObject, TValue.From<NullableDouble>(AField.AsFloat));
    end;
    Result := True;
  end
  else if AValue.IsType(TypeInfo(NullableSingle)) then
  begin
    if AField.IsNull then
    begin
      aRTTIProp.GetValue(AObject).AsType<NullableSingle>().Clear;
    end
    else
    begin
      aRTTIProp.SetValue(AObject, TValue.From<NullableSingle>(AField.AsSingle));
    end;
    Result := True;
  end
  else if AValue.IsType(TypeInfo(NullableExtended)) then
  begin
    if AField.IsNull then
    begin
      aRTTIProp.GetValue(AObject).AsType<NullableExtended>().Clear;
    end
    else
    begin
      aRTTIProp.SetValue(AObject, TValue.From<NullableExtended>(AField.AsExtended));
    end;
    Result := True;
  end
  else if AValue.IsType(TypeInfo(NullableCurrency)) then
  begin
    if AField.IsNull then
    begin
      aRTTIProp.GetValue(AObject).AsType<NullableCurrency>().Clear;
    end
    else
    begin
      aRTTIProp.SetValue(AObject, TValue.From<NullableCurrency>(AField.AsCurrency));
    end;
    Result := True;
  end
end;

{ TMVCObjectDictionary }

function TMVCObjectDictionary.Add(const Name: string; const Value: TObject;
  const SerializationAction: TMVCSerializationAction;
  const AIgnoredFields: TMVCIgnoredList): IMVCObjectDictionary;
begin
  fDict.Add(name, TMVCObjectDictionaryValueItem.Create(
    fOwnsValueItemData,
    Value,
    SerializationAction,
    AIgnoredFields
    ));
  Result := Self;
end;

function TMVCObjectDictionary.Add(
  const Name: string;
  const Value: TDataset;
  const SerializationAction: TMVCDataSetSerializationAction;
  const DataSetSerializationType: TMVCDatasetSerializationType;
  const NameCase: TMVCNameCase;
  const AIgnoredFields: TMVCIgnoredList): IMVCObjectDictionary;
begin
  fDict.Add(name, TMVCObjectDictionaryValueItem.Create(
    fOwnsValueItemData,
    Value,
    SerializationAction,
    DataSetSerializationType,
    NameCase,
    AIgnoredFields));
  Result := Self;
end;

procedure TMVCObjectDictionary.Clear;
begin
  fDict.Clear;
end;

function TMVCObjectDictionary.ContainsKey(const Key: string): Boolean;
begin
  Result := fDict.ContainsKey(Key);
end;

function TMVCObjectDictionary.Count: Integer;
begin
  Result := fDict.Count;
end;

constructor TMVCObjectDictionary.Create(const aKey: string; const Value: TObject;
const OwnsValues: Boolean);
begin
  Create(OwnsValues);
  Add(aKey, Value);
end;

constructor TMVCObjectDictionary.Create(const OwnsValues: Boolean);
begin
  inherited Create;
  fOwnsValueItemData := OwnsValues;
  fDict := TObjectDictionary<string, TMVCObjectDictionaryValueItem>.Create([doOwnsValues]);
end;

destructor TMVCObjectDictionary.Destroy;
begin
  fDict.Free;
  inherited;
end;

function TMVCObjectDictionary.GetItem(const Key: string): TMVCObjectDictionaryValueItem;
begin
  Result := fDict.Items[Key];
end;

function TMVCObjectDictionary.Keys: TArray<string>;
begin
  Result := fDict.Keys.ToArray;
end;

function TMVCObjectDictionary.TryGetValue(const Name: string; out Value: TObject): Boolean;
var
  lItem: TMVCObjectDictionaryValueItem;
begin
  Result := fDict.TryGetValue(name, lItem);
  if Result then
    Value := lItem.Data;
end;

{ TMVCObjectDictionary.TMVCObjectDictionaryValueItem }

constructor TMVCObjectDictionary.TMVCObjectDictionaryValueItem.Create(
  const Owns: Boolean;
  const Data: TObject;
  const SerializationAction: TMVCSerializationAction;
  const AIgnoredFields: TMVCIgnoredList = nil);
begin
  inherited Create;
  fOwns := Owns;
  fData := Data;
  fSerializationAction := SerializationAction;
  fDataSetFieldNameCase := ncAsIs; { not used }
  fIgnoredFields := AIgnoredFields;
end;

constructor TMVCObjectDictionary.TMVCObjectDictionaryValueItem.Create(
  const Owns: Boolean;
  const Data: TDataset;
  const SerializationAction: TMVCDataSetSerializationAction;
  const DataSetSerializationType: TMVCDatasetSerializationType;
  const NameCase: TMVCNameCase;
  const AIgnoredFields: TMVCIgnoredList = nil);
begin
  Create(Owns, Data, nil, AIgnoredFields);
  fDataSetFieldNameCase := NameCase;
  fDataSetSerializationType := DataSetSerializationType;
  fDataSetSerializationAction := SerializationAction;
end;

destructor TMVCObjectDictionary.TMVCObjectDictionaryValueItem.Destroy;
begin
  if fOwns then
    fData.Free;
  inherited;
end;

{ MVCOwnedAttribute }

constructor MVCOwnedAttribute.Create(const ClassRef: TClass);
begin
  inherited Create;
  fClassRef := ClassRef;
end;




initialization

gLocalTimeStampAsUTC := False;

end.
