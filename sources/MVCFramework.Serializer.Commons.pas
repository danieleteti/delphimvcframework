// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2017 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
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
// *************************************************************************** }

unit MVCFramework.Serializer.Commons;

interface

{$I dmvcframework.inc}


uses
  System.Rtti, System.Classes, System.SysUtils, System.Generics.Collections, MVCFramework.Serializer.Intf,
  System.TypInfo, MVCFramework.MultiMap, MVCFramework.Commons;

type
  TSerializerHelpers = class sealed
    class function GetKeyName(const ARttiField: TRttiField; AType: TRttiType)
      : string; overload;
    class function GetKeyName(const ARttiProp: TRttiProperty; AType: TRttiType)
      : string; overload;
    class function HasAttribute<T: class>(ARTTIMember: TRttiNamedObject)
      : boolean; overload;
    class function HasAttribute<T: class>(ARTTIMember: TRttiNamedObject;
      out AAttribute: T): boolean; overload;
    class function AttributeExists<T: TCustomAttribute>(Attributes: TArray<TCustomAttribute>; out Attribute: T)
      : boolean;
    class procedure EncodeStream(Input, Output: TStream);
    class procedure DecodeStream(Input, Output: TStream);
    class function EncodeString(const Input: string): string;
    class function DecodeString(const Input: string): string;
    class procedure DeSerializeStringStream(aStream: TStream;
      const aSerializedString: string; aEncoding: string);

    class procedure DeSerializeBase64StringStream(aStream: TStream;
      const aBase64SerializedString: string); static;
    class function GetTypeKindAsString(const ATypeKind: TTypeKind): String;
    class function StringToTypeKind(const AValue: String): TTypeKind;
  end;

  EMVCSerializationException = class(Exception)

  end;

  EMVCDeserializationException = class(Exception)

  end;

  TMVCSerializersRegistry = class sealed
  strict private
    class var SSerializers: TDictionary<string, IMVCSerializer>;
    class var SCustomTypeSerializers: TObjectList<TMVCTuple<String, PTypeInfo, IMVCTypeSerializer>>;
  public
    class function GetSerializer(aContentType: String): IMVCSerializer;
    class procedure RegisterSerializer(aContentType: string; aMVCSerUnSer: IMVCSerializer);
    class procedure UnRegisterSerializer(aContentType: string);
    class procedure RegisterTypeSerializer(
      aSerializerName: string;
      aTypeInfo: PTypeInfo;
      aMVCTypeSerializer: IMVCTypeSerializer);
    class procedure UnRegisterTypeSerializer(aContentType: string; aTypeInfo: PTypeInfo);
    class function GetTypeSerializer(aSerializerName: String;
      aTypeInfo: PTypeInfo): IMVCTypeSerializer;
    class constructor Create;
    class destructor Destroy;
  end;

  TValueAsType = class(TCustomAttribute)
  private
    FTValueTypeInfo: PTypeInfo;
  public
    constructor Create(ATValueTypeInfo: PTypeInfo);
    function TValueTypeInfo: PTypeInfo;
  end;

  MVCDoNotSerializeAttribute = class(TCustomAttribute)

  end;

  MVCSerializeAsString = class(TCustomAttribute)
  strict private
    FEncoding: string;
    procedure SetEncoding(const Value: string);

  const
    DefaultEncoding = 'utf8';
  private
    function GetEncoding: string;
  public
    constructor Create(aEncoding: string = DefaultEncoding);
    property Encoding: string read GetEncoding write SetEncoding;
  end;

implementation

uses
  ObjectsMappers
{$IFDEF SYSTEMNETENCODING}
    , System.NetEncoding
  // so that the old functions in Soap.EncdDecd can be inlined
{$ENDIF}
    , Soap.EncdDecd;

{ TSerializer }

class procedure TSerializerHelpers.DeSerializeBase64StringStream(
  aStream: TStream; const aBase64SerializedString: string);
var
  SS: TStringStream;
begin
  // deserialize the stream as Base64 encoded string...
  aStream.Size := 0;
  SS := TStringStream.Create(aBase64SerializedString, TEncoding.ASCII);
  try
    SS.Position := 0;
    DecodeStream(SS, aStream);
  finally
    SS.Free;
  end;
end;

class procedure TSerializerHelpers.DeSerializeStringStream(aStream: TStream;
  const aSerializedString: string; aEncoding: string);
var
  SerEnc: TEncoding;
  SS: TStringStream;
begin
  // deserialize the stream as a normal string...
  aStream.Position := 0;
  SerEnc := TEncoding.GetEncoding(aEncoding);
  SS := TStringStream.Create(aSerializedString, SerEnc);
  try
    SS.Position := 0;
    aStream.CopyFrom(SS, SS.Size);
  finally
    SS.Free;
  end;
end;

class function TSerializerHelpers.GetKeyName(const ARttiField: TRttiField;
  AType: TRttiType): string;
var
  attrs: TArray<TCustomAttribute>;
  attr: TCustomAttribute;
begin
  // JSONSer property attribute handling
  attrs := ARttiField.GetAttributes;
  for attr in attrs do
  begin
    if attr is MapperJSONSer then
      Exit(MapperJSONSer(attr).Name);
  end;

  // JSONNaming class attribute handling
  attrs := AType.GetAttributes;
  for attr in attrs do
  begin
    if attr is MapperJSONNaming then
    begin
      case MapperJSONNaming(attr).KeyCase of
        JSONNameUpperCase:
          begin
            Exit(UpperCase(ARttiField.Name));
          end;
        JSONNameLowerCase:
          begin
            Exit(LowerCase(ARttiField.Name));
          end;
      end;
    end;
  end;

  // Default
  Result := ARttiField.Name;
end;

class function TSerializerHelpers.AttributeExists<T>(
  Attributes: TArray<TCustomAttribute>; out Attribute: T): boolean;
var
  lAtt: TCustomAttribute;
begin
  Attribute := nil;
  for lAtt in Attributes do
  begin
    if lAtt is T then
    begin
      Attribute := T(lAtt);
      Break;
    end;
  end;
  Result := Attribute <> nil;
end;

class procedure TSerializerHelpers.DecodeStream(Input, Output: TStream);
begin
  Soap.EncdDecd.DecodeStream(Input, Output);
end;

class function TSerializerHelpers.DecodeString(const Input: string): string;
begin
  Result := Soap.EncdDecd.DecodeString(Input);
end;

class procedure TSerializerHelpers.EncodeStream(Input, Output: TStream);
begin
  Soap.EncdDecd.EncodeStream(Input, Output);
end;

class function TSerializerHelpers.EncodeString(const Input: string): string;
begin
  Result := Soap.EncdDecd.EncodeString(Input);
end;

class function TSerializerHelpers.GetKeyName(const ARttiProp: TRttiProperty;
  AType: TRttiType): string;
var
  attrs: TArray<TCustomAttribute>;
  attr: TCustomAttribute;
begin
  // JSONSer property attribute handling
  attrs := ARttiProp.GetAttributes;
  for attr in attrs do
  begin
    if attr is MapperJSONSer then
      Exit(MapperJSONSer(attr).Name);
  end;

  // JSONNaming class attribute handling
  attrs := AType.GetAttributes;
  for attr in attrs do
  begin
    if attr is MapperJSONNaming then
    begin
      case MapperJSONNaming(attr).KeyCase of
        JSONNameUpperCase:
          begin
            Exit(UpperCase(ARttiProp.Name));
          end;
        JSONNameLowerCase:
          begin
            Exit(LowerCase(ARttiProp.Name));
          end;
      end;
    end;
  end;

  // Default
  Result := ARttiProp.Name;
end;

class function TSerializerHelpers.GetTypeKindAsString(
  const ATypeKind: TTypeKind): String;
begin
  Result := GetEnumName(TypeInfo(TTypeKind), Ord(ATypeKind));
  Result := Result.Remove(0, 2).ToLower;
end;

class function TSerializerHelpers.HasAttribute<T>(
  ARTTIMember: TRttiNamedObject): boolean;
var
  attrs: TArray<TCustomAttribute>;
  attr: TCustomAttribute;
begin
  Result := false;
  attrs := ARTTIMember.GetAttributes;
  if Length(attrs) = 0 then
    Exit(false);
  for attr in attrs do
    if attr is T then
      Exit(True);
end;

class function TSerializerHelpers.HasAttribute<T>(ARTTIMember: TRttiNamedObject;
  out AAttribute: T): boolean;
var
  attrs: TArray<TCustomAttribute>;
  attr: TCustomAttribute;
begin
  AAttribute := nil;
  Result := false;
  attrs := ARTTIMember.GetAttributes;
  for attr in attrs do
    if attr is T then
    begin
      AAttribute := T(attr);
      Exit(True);
    end;
end;

class function TSerializerHelpers.StringToTypeKind(
  const AValue: String): TTypeKind;
begin
  Result := TTypeKind(GetEnumValue(TypeInfo(TTypeKind), 'tk' + AValue));
end;

{ TMVCSerUnSerRegistry }

class constructor TMVCSerializersRegistry.Create;
begin
  SSerializers := TDictionary<String, IMVCSerializer>.Create;
  SCustomTypeSerializers := TObjectList < TMVCTuple < String, PTypeInfo, IMVCTypeSerializer >>.Create(True);
end;

class destructor TMVCSerializersRegistry.Destroy;
begin
  SSerializers.Free;
  SCustomTypeSerializers.Free;
end;

class function TMVCSerializersRegistry.GetSerializer(
  aContentType: String): IMVCSerializer;
begin
  if not SSerializers.TryGetValue(aContentType, Result) then
    raise EMVCSerializationException.CreateFmt('Cannot find a suitable serializer for %s', [aContentType]);
end;

class function TMVCSerializersRegistry.GetTypeSerializer(
  aSerializerName: String;
  aTypeInfo: PTypeInfo): IMVCTypeSerializer;
var
  lList: TList<TMVCTuple<String, PTypeInfo, IMVCTypeSerializer>>;
  I: Integer;
  lItem: TMVCTuple<string, PTypeInfo, IMVCTypeSerializer>;
begin
  Result := nil;
  for I := 0 to SCustomTypeSerializers.Count - 1 do
  begin
    lItem := SCustomTypeSerializers[I];
    if (lItem.Val1 = aSerializerName) and (lItem.Val2 = aTypeInfo) then
    begin
      Result := lItem.Val3;
      Break;
    end;
  end;
end;

class procedure TMVCSerializersRegistry.RegisterSerializer(aContentType: string;
  aMVCSerUnSer: IMVCSerializer);
begin
  TMVCSerializersRegistry.SSerializers.Add(aContentType, aMVCSerUnSer);
end;

class procedure TMVCSerializersRegistry.RegisterTypeSerializer(
  aSerializerName: string; aTypeInfo: PTypeInfo;
  aMVCTypeSerializer: IMVCTypeSerializer);
begin
  SCustomTypeSerializers.Add(TMVCTuple<String, PTypeInfo, IMVCTypeSerializer>.Create(aSerializerName, aTypeInfo,
    aMVCTypeSerializer));
end;

class procedure TMVCSerializersRegistry.UnRegisterSerializer(aContentType: string);
begin
  TMVCSerializersRegistry.SSerializers.Remove(aContentType);
end;

class procedure TMVCSerializersRegistry.UnRegisterTypeSerializer(
  aContentType: string; aTypeInfo: PTypeInfo);
begin
  raise Exception.Create('Not implemented');
  // SCustomTypeSerializers.Add(aContentType,
  // TMVCPair<PTypeInfo, IMVCTypeSerializer>.Create(aTypeInfo, aMVCTypeSerializer));
end;

{ TValueAsType }

constructor TValueAsType.Create(ATValueTypeInfo: PTypeInfo);
begin
  inherited Create;
  FTValueTypeInfo := ATValueTypeInfo;
end;

function TValueAsType.TValueTypeInfo: PTypeInfo;
begin
  Result := FTValueTypeInfo;
end;

{ MVCSerializeAsString }

constructor MVCSerializeAsString.Create(aEncoding: string);
begin
  inherited Create;
  FEncoding := aEncoding;
end;

function MVCSerializeAsString.GetEncoding: string;
begin
  if FEncoding.IsEmpty then
    FEncoding := DefaultEncoding;
  Result := FEncoding;
end;

procedure MVCSerializeAsString.SetEncoding(const Value: string);
begin
  FEncoding := Value;
end;

end.
