// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2019 Daniele Teti and the DMVCFramework Team
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
  Data.DB;

type

  TMVCSerializationType = (stUnknown, stDefault, stProperties, stFields);

  TMVCNameCase = (ncAsIs, ncUpperCase, ncLowerCase, ncCamelCase);

  TMVCDataType = (dtObject, dtArray);

  TMVCDatasetSerializationType = (dstSingleRecord, dstAllRecords);

  TMVCEnumSerializationType = (estEnumName, estEnumOrd);

  TMVCIgnoredList = array of string;

  TMVCSerializationAction<T: class> = reference to procedure(const AObject: T; const Links: IMVCLinks);
  TMVCSerializationAction = reference to procedure(const AObject: TObject; const Links: IMVCLinks);
  TMVCDataSetSerializationAction = reference to procedure(const ADataSet: TDataset; const Links: IMVCLinks);

  EMVCSerializationException = class(EMVCException)
  end;

  EMVCDeserializationException = class(EMVCException)
  end;

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
  private
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }
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
    FName: string;
    function GetName: string;
  protected
    { protected declarations }
  public
    constructor Create(const AName: string);
    property name: string read GetName;
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
    FIsPK: boolean;
    procedure SetFieldName(const Value: string);
    procedure SetIsPK(const Value: boolean);
  public
    constructor Create(AFieldName: string; AIsPK: boolean = false);
    property FieldName: string read FFieldName write SetFieldName;
    property IsPK: boolean read FIsPK write SetIsPK;
  end;

  MVCEnumSerializationTypeAttribute = class(TCustomAttribute)
  private
    FEnumPrefix: string;
    FEnumSerializationType: TMVCEnumSerializationType;
  public
    constructor Create(const AEnumSerializationType: TMVCEnumSerializationType; const AEnumPrefix: string = '');
    property EnumSerializationType: TMVCEnumSerializationType read FEnumSerializationType;
    property EnumPrefix: string read FEnumPrefix;
  end;

  TMVCSerializerHelper = record
  private
    { private declarations }
  public
    class function GetKeyName(const AField: TRttiField; const AType: TRttiType): string; overload; static;
    class function GetKeyName(const AProperty: TRttiProperty; const AType: TRttiType): string; overload; static;

    class function HasAttribute<T: class>(const AMember: TRttiNamedObject): boolean; overload; static;
    class function HasAttribute<T: class>(const AMember: TRttiNamedObject; out AAttribute: T): boolean;
      overload; static;

    class function AttributeExists<T: TCustomAttribute>(const AAttributes: TArray<TCustomAttribute>; out AAttribute: T)
      : boolean; overload; static;
    class function AttributeExists<T: TCustomAttribute>(const AAttributes: TArray<TCustomAttribute>): boolean;
      overload; static;

    class procedure EncodeStream(AInput, AOutput: TStream); static;
    class procedure DecodeStream(AInput, AOutput: TStream); static;

    class function EncodeString(const AInput: string): string; static;
    class function DecodeString(const AInput: string): string; static;

    class procedure DeSerializeStringStream(AStream: TStream; const ASerializedString: string;
      const AEncoding: string); static;
    class procedure DeSerializeBase64StringStream(AStream: TStream; const ABase64SerializedString: string); static;

    class function GetTypeKindAsString(const ATypeKind: TTypeKind): string; static;
    class function StringToTypeKind(const AValue: string): TTypeKind; static;

    class function CreateObject(const AObjectType: TRttiType): TObject; overload; static;
    class function CreateObject(const AQualifiedClassName: string): TObject; overload; static;

    class function IsAPropertyToSkip(const aPropName: string): boolean; static;
  end;

  TMVCLinksCallback = reference to procedure(const Links: TMVCStringDictionary);

  // Well Known Response Objects
  [MVCNameCase(ncLowerCase)]
  TMVCResponseBase = class abstract

  end;

  [MVCNameCase(ncLowerCase)]
  TMVCTask = class
  private
    fID: String;
    fHREF: String;
  public
    property HREF: String read fHREF write fHREF;
    property ID: String read fID write fID;
    constructor Create(const HREF, ID: String);
  end;

  [MVCNameCase(ncLowerCase)]
  TMVCAcceptedResponse = class(TMVCResponseBase)
  private
    fTask: TMVCTask;
  public
    property Task: TMVCTask read fTask;
    // constructor Create(const aTask: TMVCTask); overload;
    constructor Create(const HREF, ID: String);
    destructor Destroy; override;
  end;

  [MVCNameCase(ncLowerCase)]
  TDataObjectHolder = class
  private
    FData: TObject;
    FMetadata: TMVCStringDictionary;
    FOwns: boolean;
    FDataSetSerializationType: TMVCDatasetSerializationType;
  public
    constructor Create(const AObject: TObject; const AOwns: boolean = false;
      const ADataSetSerializationType: TMVCDatasetSerializationType = TMVCDatasetSerializationType.
      dstAllRecords); virtual;
    destructor Destroy; override;
    function SerializationType: TMVCDatasetSerializationType;
    [MVCNameAs('data')]
    property Items: TObject read FData;
    [MVCNameAs('meta')]
    property Metadata: TMVCStringDictionary read FMetadata;
  end;

function DateTimeToISOTimeStamp(const ADateTime: TDateTime): string;
function DateToISODate(const ADate: TDateTime): string;
function TimeToISOTime(const ATime: TTime): string;

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

function NewDataObjectHolder(const AObject: TObject; const AMetaFiller: TProc<TMVCStringDictionary> = nil;
  const AOwns: boolean = false): TDataObjectHolder;
function NewCollectionHolder(const AList: TObject; const AMetaFiller: TProc<TMVCStringDictionary> = nil;
  const AOwns: boolean = false): TDataObjectHolder;

implementation

uses
  MVCFramework.Serializer.JsonDataObjects,
  MVCFramework.Serializer.Intf;

function NewDataObjectHolder(const AObject: TObject; const AMetaFiller: TProc<TMVCStringDictionary> = nil;
  const AOwns: boolean = false): TDataObjectHolder;
begin
  Result := TDataObjectHolder.Create(AObject, AOwns, dstSingleRecord);
  if Assigned(AMetaFiller) then
  begin
    AMetaFiller(Result.FMetadata);
  end;
end;

function NewCollectionHolder(const AList: TObject; const AMetaFiller: TProc<TMVCStringDictionary> = nil;
  const AOwns: boolean = false): TDataObjectHolder;
begin
  Result := TDataObjectHolder.Create(AList, AOwns, dstAllRecords);
  if Assigned(AMetaFiller) then
  begin
    AMetaFiller(Result.FMetadata);
  end;
end;

function DateTimeToISOTimeStamp(const ADateTime: TDateTime): string;
begin
  // fs.TimeSeparator := ':';
  Result := DateToISO8601(ADateTime, true)
  // Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', ADateTime, fs);
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
  Result := ISO8601ToDate(lDateTime, true);
end;

function ISODateToDate(const ADate: string): TDate;
begin
  Result := EncodeDate(StrToInt(Copy(ADate, 1, 4)), StrToInt(Copy(ADate, 6, 2)), StrToInt(Copy(ADate, 9, 2)));
end;

function ISOTimeToTime(const ATime: string): TTime;
begin
  Result := EncodeTime(StrToInt(Copy(ATime, 1, 2)), StrToInt(Copy(ATime, 4, 2)), StrToInt(Copy(ATime, 7, 2)), 0);
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

class procedure TMVCSerializerHelper.DeSerializeStringStream(AStream: TStream; const ASerializedString: string;
  const AEncoding: string);
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

class function TMVCSerializerHelper.GetKeyName(const AField: TRttiField; const AType: TRttiType): string;
var
  Attrs: TArray<TCustomAttribute>;
  Attr: TCustomAttribute;
begin
  Result := AField.Name;

  Attrs := AField.GetAttributes;
  for Attr in Attrs do
    if Attr is MVCNameAsAttribute then
      Exit(MVCNameAsAttribute(Attr).Name);

  Attrs := AType.GetAttributes;
  for Attr in Attrs do
    if Attr is MVCNameCaseAttribute then
    begin
      case MVCNameCaseAttribute(Attr).KeyCase of
        ncUpperCase:
          begin
            Exit(UpperCase(AField.Name));
          end;
        ncLowerCase:
          begin
            Exit(LowerCase(AField.Name));
          end;
        ncCamelCase:
          begin
            Exit(LowerCase(AField.Name.Chars[0]) + AField.Name.Substring(1));
          end;
      end;
    end;
end;

class function TMVCSerializerHelper.AttributeExists<T>(const AAttributes: TArray<TCustomAttribute>;
  out AAttribute: T): boolean;
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

class function TMVCSerializerHelper.AttributeExists<T>(const AAttributes: TArray<TCustomAttribute>): boolean;
var
  Att: TCustomAttribute;
begin
  Result := false;
  for Att in AAttributes do
    if Att is T then
      Exit(true);
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
    raise EMVCException.CreateFmt('Cannot find a propert constructor for %s', [AObjectType.ToString]);
end;

class function TMVCSerializerHelper.CreateObject(const AQualifiedClassName: string): TObject;
var
  Context: TRttiContext;
  ObjectType: TRttiType;
begin
{$IF not Defined(SeattleOrBetter)}
  Result := nil;
{$ENDIF}
  Context := TRttiContext.Create;
  try
    ObjectType := Context.FindType(AQualifiedClassName);
    if Assigned(ObjectType) then
      Result := CreateObject(ObjectType)
    else
      raise Exception.CreateFmt('Cannot find RTTI for %s. Hint: Is the specified classtype linked in the module?',
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

class function TMVCSerializerHelper.GetKeyName(const AProperty: TRttiProperty; const AType: TRttiType): string;
var
  Attrs: TArray<TCustomAttribute>;
  Attr: TCustomAttribute;
begin
  Result := AProperty.Name;

  Attrs := AProperty.GetAttributes;
  for Attr in Attrs do
    if Attr is MVCNameAsAttribute then
      Exit(MVCNameAsAttribute(Attr).Name);

  Attrs := AType.GetAttributes;
  for Attr in Attrs do
    if Attr is MVCNameCaseAttribute then
    begin
      case MVCNameCaseAttribute(Attr).KeyCase of
        ncUpperCase:
          begin
            Exit(UpperCase(AProperty.Name));
          end;
        ncLowerCase:
          begin
            Exit(LowerCase(AProperty.Name));
          end;
        ncCamelCase:
          begin
            Exit(LowerCase(AProperty.Name.Chars[0]) + AProperty.Name.Substring(1));
          end;
      end;
    end;
end;

class function TMVCSerializerHelper.GetTypeKindAsString(const ATypeKind: TTypeKind): string;
begin
  Result := GetEnumName(TypeInfo(TTypeKind), Ord(ATypeKind));
  Result := Result.Remove(0, 2).ToLower;
end;

class function TMVCSerializerHelper.HasAttribute<T>(const AMember: TRttiNamedObject): boolean;
var
  Attrs: TArray<TCustomAttribute>;
  Attr: TCustomAttribute;
begin
  Result := false;
  Attrs := AMember.GetAttributes;
  if Length(Attrs) = 0 then
    Exit(false);
  for Attr in Attrs do
    if Attr is T then
      Exit(true);
end;

class function TMVCSerializerHelper.HasAttribute<T>(const AMember: TRttiNamedObject; out AAttribute: T): boolean;
var
  Attrs: TArray<TCustomAttribute>;
  Attr: TCustomAttribute;
begin
  AAttribute := nil;
  Result := false;
  Attrs := AMember.GetAttributes;
  for Attr in Attrs do
    if Attr is T then
    begin
      AAttribute := T(Attr);
      Exit(true);
    end;
end;

class function TMVCSerializerHelper.IsAPropertyToSkip(const aPropName: string): boolean;
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

constructor MVCNameAsAttribute.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

function MVCNameAsAttribute.GetName: string;
begin
  Result := FName;
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

constructor MVCColumnAttribute.Create(AFieldName: string; AIsPK: boolean);
begin
  inherited Create;
  FFieldName := AFieldName;
  FIsPK := AIsPK;
end;

procedure MVCColumnAttribute.SetFieldName(const Value: string);
begin
  FFieldName := Value;
end;

procedure MVCColumnAttribute.SetIsPK(const Value: boolean);
begin
  FIsPK := Value;
end;

{ MVCEnumSerializationTypeAttribute }

constructor MVCEnumSerializationTypeAttribute.Create(const AEnumSerializationType: TMVCEnumSerializationType;
  const AEnumPrefix: string);
begin
  FEnumSerializationType := AEnumSerializationType;
  FEnumPrefix := AEnumPrefix;
end;

{ TMVCTask }

constructor TMVCTask.Create(const HREF, ID: String);
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

constructor TMVCAcceptedResponse.Create(const HREF, ID: String);
begin
  inherited Create;
  fTask := TMVCTask.Create(HREF, ID);
end;

destructor TMVCAcceptedResponse.Destroy;
begin
  fTask.Free;
  inherited;
end;

{ TDataObjectHolder }

constructor TDataObjectHolder.Create(const AObject: TObject; const AOwns: boolean;
  const ADataSetSerializationType: TMVCDatasetSerializationType);
begin
  inherited Create;
  FData := AObject;
  FMetadata := TMVCStringDictionary.Create;
  FOwns := AOwns;
  FDataSetSerializationType := ADataSetSerializationType;
end;

destructor TDataObjectHolder.Destroy;
begin
  FMetadata.Free;
  if FOwns then
  begin
    FData.Free;
  end;
  inherited;
end;

function TDataObjectHolder.SerializationType: TMVCDatasetSerializationType;
begin
  Result := FDataSetSerializationType;
end;

end.
