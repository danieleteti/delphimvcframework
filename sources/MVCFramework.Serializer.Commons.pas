unit MVCFramework.Serializer.Commons;

interface

uses
  System.Rtti, System.Classes, System.SysUtils, System.Generics.Collections, MVCFramework.Serializer.Intf,
  System.TypInfo;

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

  TMVCSerUnSerRegistry = class sealed
  strict private
    class var SStorage: TDictionary<string, IMVCSerUnSer>;
  public
    class function GetSerUnSer(aContentType: String): IMVCSerUnSer;
    class procedure RegisterSerializer(aContentType: string; aMVCSerUnSer: IMVCSerUnSer);
    class procedure UnRegisterSerializer(aContentType: string);
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

implementation

uses
  ObjectsMappers
{$IFDEF SYSTEMNETENCODING}
    , System.NetEncoding,
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

class procedure TSerializerHelpers.EncodeStream(Input, Output: TStream);
begin
  Soap.EncdDecd.EncodeStream(Input, Output);
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

class constructor TMVCSerUnSerRegistry.Create;
begin
  SStorage := TDictionary<String, IMVCSerUnSer>.Create;
end;

class destructor TMVCSerUnSerRegistry.Destroy;
begin
  SStorage.Free;
end;

class function TMVCSerUnSerRegistry.GetSerUnSer(
  aContentType: String): IMVCSerUnSer;
begin
  if not SStorage.TryGetValue(aContentType, Result) then
    raise EMVCSerializationException.CreateFmt('Cannot find a suitable serializer for %s', [aContentType]);
end;

class procedure TMVCSerUnSerRegistry.RegisterSerializer(aContentType: string;
  aMVCSerUnSer: IMVCSerUnSer);
begin
  TMVCSerUnSerRegistry.SStorage.Add(aContentType, aMVCSerUnSer);
end;

class procedure TMVCSerUnSerRegistry.UnRegisterSerializer(aContentType: string);
begin
  TMVCSerUnSerRegistry.SStorage.Remove(aContentType);
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

end.
