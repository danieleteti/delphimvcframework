// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2017 Daniele Teti and the DMVCFramework Team
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

  MVCFramework.Commons;

type

  TMVCSerializationType = (stDefault, stProperties, stFields);

  TMVCNameCase = (ncAsIs, ncUpperCase, ncLowerCase);

  TMVCDataType = (dtObject, dtArray);

  TMVCIgnoredList = array of string;

  EMVCSerializationException = class(Exception)
  private
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }
  end;

  EMVCDeserializationException = class(Exception)
  private
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }
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
    property Name: string read GetName;
  end;

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

  TMVCSerializerHelpful = record
  private
    { private declarations }
  public
    class function GetKeyName(const AField: TRttiField; const AType: TRttiType): string; overload; static;
    class function GetKeyName(const AProperty: TRttiProperty; const AType: TRttiType): string; overload; static;

    class function HasAttribute<T: class>(const AMember: TRttiNamedObject): Boolean; overload; static;
    class function HasAttribute<T: class>(const AMember: TRttiNamedObject; out AAttribute: T): Boolean; overload; static;

    class function AttributeExists<T: TCustomAttribute>(const AAttributes: TArray<TCustomAttribute>; out AAttribute: T): Boolean; overload; static;
    class function AttributeExists<T: TCustomAttribute>(const AAttributes: TArray<TCustomAttribute>): Boolean; overload; static;

    class procedure EncodeStream(AInput, AOutput: TStream); static;
    class procedure DecodeStream(AInput, AOutput: TStream); static;

    class function EncodeString(const AInput: string): string; static;
    class function DecodeString(const AInput: string): string; static;

    class procedure DeSerializeStringStream(AStream: TStream; const ASerializedString: string; const AEncoding: string); static;
    class procedure DeSerializeBase64StringStream(AStream: TStream; const ABase64SerializedString: string); static;

    class function GetTypeKindAsString(const ATypeKind: TTypeKind): String; static;
    class function StringToTypeKind(const AValue: String): TTypeKind; static;

    class function CreateObject(const AObjectType: TRttiType): TObject; overload; static;
    class function CreateObject(const AQualifiedClassName: string): TObject; overload; static;
  end;

function DateTimeToISOTimeStamp(const ADateTime: TDateTime): string;
function DateToISODate(const ADate: TDateTime): string;
function TimeToISOTime(const ATime: TTime): string;

function ISOTimeStampToDateTime(const ADateTime: string): TDateTime;
function ISODateToDate(const ADate: string): TDate;
function ISOTimeToTime(const ATime: string): TTime;

const
  JSONNameLowerCase = ncLowerCase deprecated 'Use MVCNameCaseAttribute(ncLowerCase)';
  JSONNameUpperCase = ncUpperCase deprecated 'Use MVCNameCaseAttribute(ncUpperCase)';

implementation

function DateTimeToISOTimeStamp(const ADateTime: TDateTime): string;
var
  fs: TFormatSettings;
begin
  fs.TimeSeparator := ':';
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', ADateTime, fs);
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
begin
  Result := EncodeDateTime(StrToInt(Copy(ADateTime, 1, 4)), StrToInt(Copy(ADateTime, 6, 2)), StrToInt(Copy(ADateTime, 9, 2)),
    StrToInt(Copy(ADateTime, 12, 2)), StrToInt(Copy(ADateTime, 15, 2)), StrToInt(Copy(ADateTime, 18, 2)), 0);
end;

function ISODateToDate(const ADate: string): TDate;
begin
  Result := EncodeDate(StrToInt(Copy(ADate, 1, 4)), StrToInt(Copy(ADate, 6, 2)), StrToInt(Copy(ADate, 9, 2)));
end;

function ISOTimeToTime(const ATime: string): TTime;
begin
  Result := EncodeTime(StrToInt(Copy(ATime, 1, 2)), StrToInt(Copy(ATime, 4, 2)), StrToInt(Copy(ATime, 7, 2)), 0);
end;

{ TMVCSerializerHelpful }

class procedure TMVCSerializerHelpful.DeSerializeBase64StringStream(
  AStream: TStream; const ABase64SerializedString: string);
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

class procedure TMVCSerializerHelpful.DeSerializeStringStream(AStream: TStream; const ASerializedString: string; const AEncoding: string);
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

class function TMVCSerializerHelpful.GetKeyName(const AField: TRttiField; const AType: TRttiType): string;
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
      end;
    end;
end;

class function TMVCSerializerHelpful.AttributeExists<T>(const AAttributes: TArray<TCustomAttribute>; out AAttribute: T): Boolean;
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

class function TMVCSerializerHelpful.AttributeExists<T>(
  const AAttributes: TArray<TCustomAttribute>): Boolean;
var
  Att: TCustomAttribute;
begin
  Result := False;
  for Att in AAttributes do
    if Att is T then
      Exit(True);
end;

class function TMVCSerializerHelpful.CreateObject(const AObjectType: TRttiType): TObject;
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

class function TMVCSerializerHelpful.CreateObject(const AQualifiedClassName: string): TObject;
var
  Context: TRttiContext;
  ObjectType: TRttiType;
begin
  Context := TRttiContext.Create;
  try
    ObjectType := Context.FindType(AQualifiedClassName);
    if Assigned(ObjectType) then
      Result := CreateObject(ObjectType)
    else
      raise Exception.CreateFmt('Cannot find Rtti for %s. Hint: Is the specified classtype linked in the module?', [AQualifiedClassName]);
  finally
    Context.Free;
  end;
end;

class procedure TMVCSerializerHelpful.DecodeStream(AInput, AOutput: TStream);
begin

  {$IFDEF SYSTEMNETENCODING}

  TNetEncoding.Base64.Decode(AInput, AOutput);

  {$ELSE}

  Soap.EncdDecd.DecodeStream(AInput, AOutput);

  {$ENDIF}

end;

class function TMVCSerializerHelpful.DecodeString(const AInput: string): string;
begin

  {$IFDEF SYSTEMNETENCODING}

  Result := TNetEncoding.Base64.Decode(AInput);

  {$ELSE}

  Result := Soap.EncdDecd.DecodeString(AInput);

  {$ENDIF}

end;

class procedure TMVCSerializerHelpful.EncodeStream(AInput, AOutput: TStream);
begin

  {$IFDEF SYSTEMNETENCODING}

  TNetEncoding.Base64.Encode(AInput, AOutput);

  {$ELSE}

  Soap.EncdDecd.EncodeStream(AInput, AOutput);

  {$ENDIF}

end;

class function TMVCSerializerHelpful.EncodeString(const AInput: string): string;
begin

  {$IFDEF SYSTEMNETENCODING}

  Result := TNetEncoding.Base64.Encode(AInput);

  {$ELSE}

  Result := Soap.EncdDecd.EncodeString(AInput);

  {$ENDIF}

end;

class function TMVCSerializerHelpful.GetKeyName(const AProperty: TRttiProperty; const AType: TRttiType): string;
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
      end;
    end;
end;

class function TMVCSerializerHelpful.GetTypeKindAsString(const ATypeKind: TTypeKind): String;
begin
  Result := GetEnumName(TypeInfo(TTypeKind), Ord(ATypeKind));
  Result := Result.Remove(0, 2).ToLower;
end;

class function TMVCSerializerHelpful.HasAttribute<T>(const AMember: TRttiNamedObject): Boolean;
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

class function TMVCSerializerHelpful.HasAttribute<T>(const AMember: TRttiNamedObject; out AAttribute: T): Boolean;
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

class function TMVCSerializerHelpful.StringToTypeKind(const AValue: String): TTypeKind;
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

end.
