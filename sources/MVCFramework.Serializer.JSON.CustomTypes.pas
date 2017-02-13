unit MVCFramework.Serializer.JSON.CustomTypes;

interface

uses MVCFramework.Serializer.Commons, MVCFramework.Serializer.Intf,
  System.Rtti;

type
  TStreamSerializerJSON = class(TInterfacedObject, IMVCTypeSerializer)
  public
    procedure SerializeInstance(
      const ElementType: TRTTIType;
      const ElementAttributes: TArray<TCustomAttribute>;
      const InstanceField: TValue;
      var SerializerObject: TObject);
    procedure DeserializeInstance(
      const ElementType: TRTTIType;
      const ElementAttributes: TArray<TCustomAttribute>;
      const SerializerObject: TObject;
      var InstanceField: TValue);
  end;

  { Example of custom serializer for Reference types }
  TStringListSerializerJSON = class(TInterfacedObject, IMVCTypeSerializer)
  public
    procedure SerializeInstance(
      const ElementType: TRTTIType;
      const ElementAttributes: TArray<TCustomAttribute>;
      const InstanceField: TValue;
      var SerializerObject: TObject);
    procedure DeserializeInstance(
      const ElementType: TRTTIType;
      const ElementAttributes: TArray<TCustomAttribute>;
      const SerializerObject: TObject;
      var InstanceField: TValue);
  end;

  { Example of custom serializer for Value types }
  TArrayOfStringsSerializerJSON = class(TInterfacedObject, IMVCTypeSerializer)
  public
    procedure SerializeInstance(
      const ElementType: TRTTIType;
      const ElementAttributes: TArray<TCustomAttribute>;
      const InstanceField: TValue;
      var SerializerObject: TObject);
    procedure DeserializeInstance(
      const ElementType: TRTTIType;
      const ElementAttributes: TArray<TCustomAttribute>;
      const SerializerObject: TObject;
      var InstanceField: TValue);
  end;

implementation

uses
  MVCFramework.TypesAliases, System.Classes, System.SysUtils,
  MVCFramework.Serializer.JSON;

{ TStreamSerializerJSON }

procedure TStreamSerializerJSON.DeserializeInstance(
  const ElementType: TRTTIType;
  const ElementAttributes: TArray<TCustomAttribute>;
  const SerializerObject: TObject;
  var InstanceField: TValue);
var
  lJValue: TJSONValue;
  lAtt: MVCSerializeAsString;
  lInstanceStream: TStream;
  lSerializedString: String;
  lEncoding: TEncoding;
  lSStream: TStringStream;
  lSWriter: TStreamWriter;
begin
  lJValue := TJSONValue(SerializerObject);
  lInstanceStream := TStream(InstanceField.AsObject);

  if not(lJValue is TJSONString) then
    raise EMVCDeserializationException.Create('Expected JSONString in ' + ElementType.ToString);

  lSerializedString := lJValue.Value;

  lInstanceStream.Position := 0;
  if TSerializerHelpers.AttributeExists<MVCSerializeAsString>(ElementAttributes, lAtt) then
  begin
    // serialize the stream as a normal string...
    lEncoding := TEncoding.GetEncoding(lAtt.Encoding);
    lSStream := TStringStream.Create(lSerializedString, lEncoding);
    try
      lSStream.Position := 0;
      lInstanceStream.CopyFrom(lSStream, lSStream.Size);
    finally
      lSStream.Free;
    end;
  end
  else
  begin
    // deserialize the stream as Base64 encoded string...
    lSWriter := TStreamWriter.Create(lInstanceStream);
    try
      lSWriter.Write(TSerializerHelpers.DecodeString(lSerializedString));
    finally
      lSWriter.Free;
    end;
  end;
end;

procedure TStreamSerializerJSON.SerializeInstance(
  const ElementType: TRTTIType;
  const ElementAttributes: TArray<TCustomAttribute>;
  const InstanceField: TValue;
  var SerializerObject: TObject);
var
  lAtt: MVCSerializeAsString;
  lStream: TStream;
  lEncodingName: string;
  SerEnc: TEncoding;
  buff: TBytes;
  lStreamAsString: string;
  SS: TStringStream;
begin
  lStream := InstanceField.AsObject as TStream;
  if TSerializerHelpers.AttributeExists<MVCSerializeAsString>(ElementAttributes, lAtt) then
  begin
    // serialize the stream as a normal string...
    lStream.Position := 0;
    lEncodingName := lAtt.Encoding;
    SerEnc := TEncoding.GetEncoding(lEncodingName);
    try
      SetLength(buff, lStream.Size);
      lStream.Read(buff, lStream.Size);
      lStreamAsString := SerEnc.GetString(buff);
      SetLength(buff, 0);
      SerializerObject := TJSONString.Create(UTF8Encode(lStreamAsString));
    finally
      SerEnc.Free;
    end;
  end
  else
  begin
    // serialize the stream as Base64 encoded string...
    lStream.Position := 0;
    SS := TStringStream.Create;
    try
      TSerializerHelpers.EncodeStream(lStream, SS);
      SerializerObject := TJSONString.Create(SS.DataString);
    finally
      SS.Free;
    end;
  end;
end;

{ TStringListSerializerJSON }

procedure TStringListSerializerJSON.DeserializeInstance(
  const ElementType: TRTTIType;
  const ElementAttributes: TArray<TCustomAttribute>;
  const SerializerObject: TObject;
  var InstanceField: TValue);
var
  lJArr: TJSONArray;
  lSList: TStringList;
  I: Integer;
  lObj: TObject;
begin
  lObj := InstanceField.AsObject;
  if (lObj <> nil) and (not(InstanceField.AsObject is TStrings)) then
    raise EMVCDeserializationException.CreateFmt('Expected %s, actual %s',
      ['TStrings', ElementType.QualifiedName]);
  lJArr := SerializerObject as TJSONArray;
  { Reference types MUST use the internal "AsObject" while value types can directly assign to InstanceField }
  if lObj = nil then
    InstanceField := TStringList.Create;
  lSList := TStringList(InstanceField.AsObject);
  for I := 0 to lJArr.Count - 1 do
    lSList.Add(lJArr.Items[I].Value);
end;

procedure TStringListSerializerJSON.SerializeInstance(
  const ElementType: TRTTIType;
  const ElementAttributes: TArray<TCustomAttribute>;
  const InstanceField: TValue;
  var SerializerObject: TObject);
var
  lSList: TStringList;
  lJArr: TJSONArray;
  lItem: String;
begin
  lSList := InstanceField.AsObject as TStringList;
  lJArr := TJSONArray.Create;
  try
    for lItem in lSList do
    begin
      lJArr.Add(lItem);
    end;
  except
    lJArr.Free;
    raise;
  end;
  SerializerObject := lJArr;
end;

{ TArrayOfStringsSerializerJSON }

procedure TArrayOfStringsSerializerJSON.DeserializeInstance(
  const ElementType: TRTTIType;
  const ElementAttributes: TArray<TCustomAttribute>;
  const SerializerObject: TObject; var InstanceField: TValue);
var
  lJArr: TJSONArray;
  lSList: TStringList;
  I: Integer;
  lArr: TArray<String>;
begin
  if not(InstanceField.IsArray) then
    raise EMVCDeserializationException.CreateFmt('Expected %s, actual %s',
      ['TArray<String>', TSerializerHelpers.GetTypeKindAsString(InstanceField.Kind)]);
  { The seriealizerobject represent something that the main serializer decided to pass to the custom types serializer. }
  lJArr := SerializerObject as TJSONArray;
  SetLength(lArr, lJArr.Count);
  for I := 0 to lJArr.Count - 1 do
    lArr[I] := lJArr.Items[I].Value;
  { Reference types MUST use the internal "AsObject" while value types can directly assign to InstanceField }
  InstanceField := TValue.From < TArray < String >> (lArr);
end;

procedure TArrayOfStringsSerializerJSON.SerializeInstance(
  const ElementType: TRTTIType;
  const ElementAttributes: TArray<TCustomAttribute>;
  const InstanceField: TValue; var SerializerObject: TObject);
var
  lArr: TArray<String>;
  lJArr: TJSONArray;
  lItem: String;
begin
  lArr := InstanceField.AsType<TArray<String>>;
  lJArr := TJSONArray.Create;
  try
    for lItem in lArr do
    begin
      lJArr.Add(lItem);
    end;
  except
    lJArr.Free;
    raise;
  end;
  SerializerObject := lJArr;
end;

{ TMyClassSerializerJSON }

initialization

TMVCSerializersRegistry.RegisterTypeSerializer(TMVCJSONSerializer.SERIALIZER_NAME,
  TypeInfo(TStream),
  TStreamSerializerJSON.Create);
TMVCSerializersRegistry.RegisterTypeSerializer(TMVCJSONSerializer.SERIALIZER_NAME,
  TypeInfo(TStringStream),
  TStreamSerializerJSON.Create);
TMVCSerializersRegistry.RegisterTypeSerializer(TMVCJSONSerializer.SERIALIZER_NAME,
  TypeInfo(TMemoryStream),
  TStreamSerializerJSON.Create);
TMVCSerializersRegistry.RegisterTypeSerializer(TMVCJSONSerializer.SERIALIZER_NAME,
  TypeInfo(TStringList),
  TStringListSerializerJSON.Create);
TMVCSerializersRegistry.RegisterTypeSerializer(TMVCJSONSerializer.SERIALIZER_NAME,
  TypeInfo(TArray<String>),
  TArrayOfStringsSerializerJSON.Create);

finalization

end.
