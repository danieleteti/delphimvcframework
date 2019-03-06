// ***************************************************************************
//
// TNullabletypes requires spring4d framework
//
// https://bitbucket.org/sglienke/spring4d.git
//
// Contributor: 2019 - João Antônio Duarte (joao.antonioduarte@hotmail.com)
//
//
// ***************************************************************************

unit MVCFramework.Serializer.JsonDataObjects.NullableTypes;

interface

uses
  MVCFramework.Serializer.Intf,
  System.Rtti;

type

  TNullableIntegerSerializer = class(TInterfacedObject, IMVCTypeSerializer)
  public
    procedure SerializeAttribute(const AElementValue: TValue;
      const APropertyName: string;
      const ASerializerObject: TObject;
      const AAttributes: TArray<TCustomAttribute>
      );

    procedure SerializeRoot(
      const AObject: TObject;
      out ASerializerObject: TObject;
      const AAttributes: TArray<TCustomAttribute>
      );

    procedure DeserializeAttribute(
      var AElementValue: TValue;
      const APropertyName: string;
      const ASerializerObject: TObject;
      const AAttributes: TArray<TCustomAttribute>
      );

    procedure DeserializeRoot(
      const ASerializerObject: TObject;
      const AObject: TObject;
      const AAttributes: TArray<TCustomAttribute>
      );

    procedure Deserialize(
      const ASerializedObject: TObject;
      var AElementValue: TValue;
      const AAttributes: TArray<TCustomAttribute>
      );
  end;

  TNullableInt64Serializer = class(TInterfacedObject, IMVCTypeSerializer)
  public
    procedure SerializeAttribute(const AElementValue: TValue;
      const APropertyName: string;
      const ASerializerObject: TObject;
      const AAttributes: TArray<TCustomAttribute>
      );

    procedure SerializeRoot(
      const AObject: TObject;
      out ASerializerObject: TObject;
      const AAttributes: TArray<TCustomAttribute>
      );

    procedure DeserializeAttribute(
      var AElementValue: TValue;
      const APropertyName: string;
      const ASerializerObject: TObject;
      const AAttributes: TArray<TCustomAttribute>
      );

    procedure DeserializeRoot(
      const ASerializerObject: TObject;
      const AObject: TObject;
      const AAttributes: TArray<TCustomAttribute>
      );

    procedure Deserialize(
      const ASerializedObject: TObject;
      var AElementValue: TValue;
      const AAttributes: TArray<TCustomAttribute>
      );
  end;

  TNullableCurrencySerializer = class(TInterfacedObject, IMVCTypeSerializer)
  public
    procedure SerializeAttribute(
      const AElementValue: TValue;
      const APropertyName: string;
      const ASerializerObject: TObject;
      const AAttributes: TArray<TCustomAttribute>
      );
    procedure SerializeRoot(
      const AObject: TObject;
      out ASerializerObject: TObject;
      const AAttributes: TArray<TCustomAttribute>
      );

    procedure DeserializeAttribute(
      var AElementValue: TValue;
      const APropertyName: string;
      const ASerializerObject: TObject;
      const AAttributes: TArray<TCustomAttribute>
      );

    procedure DeserializeRoot(
      const ASerializerObject: TObject;
      const AObject: TObject;
      const AAttributes: TArray<TCustomAttribute>
      );

    procedure Deserialize(
      const ASerializedObject: TObject;
      var AElementValue: TValue;
      const AAttributes: TArray<TCustomAttribute>
      );
  end;

  TNullableStringSerializer = class(TInterfacedObject, IMVCTypeSerializer)
  public
    procedure SerializeAttribute(
      const AElementValue: TValue;
      const APropertyName: string;
      const ASerializerObject: TObject;
      const AAttributes: TArray<TCustomAttribute>
      );
    procedure SerializeRoot(
      const AObject: TObject;
      out ASerializerObject: TObject;
      const AAttributes: TArray<TCustomAttribute>
      );

    procedure DeserializeAttribute(
      var AElementValue: TValue;
      const APropertyName: string;
      const ASerializerObject: TObject;
      const AAttributes: TArray<TCustomAttribute>
      );

    procedure DeserializeRoot(
      const ASerializerObject: TObject;
      const AObject: TObject;
      const AAttributes: TArray<TCustomAttribute>
      );

    procedure Deserialize(
      const ASerializedObject: TObject;
      var AElementValue: TValue;
      const AAttributes: TArray<TCustomAttribute>
      );
  end;

  TNullableDateTimeSerializer = class(TInterfacedObject, IMVCTypeSerializer)
  public
    procedure SerializeAttribute(
      const AElementValue: TValue;
      const APropertyName: string;
      const ASerializerObject: TObject;
      const AAttributes: TArray<TCustomAttribute>
      );
    procedure SerializeRoot(
      const AObject: TObject;
      out ASerializerObject: TObject;
      const AAttributes: TArray<TCustomAttribute>
      );

    procedure DeserializeAttribute(
      var AElementValue: TValue;
      const APropertyName: string;
      const ASerializerObject: TObject;
      const AAttributes: TArray<TCustomAttribute>
      );

    procedure DeserializeRoot(
      const ASerializerObject: TObject;
      const AObject: TObject;
      const AAttributes: TArray<TCustomAttribute>
      );

    procedure Deserialize(
      const ASerializedObject: TObject;
      var AElementValue: TValue;
      const AAttributes: TArray<TCustomAttribute>
      );
  end;

  TNullableDateSerializer = class(TInterfacedObject, IMVCTypeSerializer)
  public
    procedure SerializeAttribute(
      const AElementValue: TValue;
      const APropertyName: string;
      const ASerializerObject: TObject;
      const AAttributes: TArray<TCustomAttribute>
      );
    procedure SerializeRoot(
      const AObject: TObject;
      out ASerializerObject: TObject;
      const AAttributes: TArray<TCustomAttribute>
      );

    procedure DeserializeAttribute(
      var AElementValue: TValue;
      const APropertyName: string;
      const ASerializerObject: TObject;
      const AAttributes: TArray<TCustomAttribute>
      );

    procedure DeserializeRoot(
      const ASerializerObject: TObject;
      const AObject: TObject;
      const AAttributes: TArray<TCustomAttribute>
      );

    procedure Deserialize(
      const ASerializedObject: TObject;
      var AElementValue: TValue;
      const AAttributes: TArray<TCustomAttribute>
      );
  end;

  TNullableTimeSerializer = class(TInterfacedObject, IMVCTypeSerializer)
  public
    procedure SerializeAttribute(
      const AElementValue: TValue;
      const APropertyName: string;
      const ASerializerObject: TObject;
      const AAttributes: TArray<TCustomAttribute>
      );
    procedure SerializeRoot(
      const AObject: TObject;
      out ASerializerObject: TObject;
      const AAttributes: TArray<TCustomAttribute>
      );

    procedure DeserializeAttribute(
      var AElementValue: TValue;
      const APropertyName: string;
      const ASerializerObject: TObject;
      const AAttributes: TArray<TCustomAttribute>
      );

    procedure DeserializeRoot(
      const ASerializerObject: TObject;
      const AObject: TObject;
      const AAttributes: TArray<TCustomAttribute>
      );

    procedure Deserialize(
      const ASerializedObject: TObject;
      var AElementValue: TValue;
      const AAttributes: TArray<TCustomAttribute>
      );
  end;

/// <summary>Register all NullablesSerializers in your serializer</summary>
/// <param name="ASerializer">Your Serializer</param>
procedure RegisterNullableTypeSerializersInSerializer(ASerializer: IMVCSerializer);

implementation

uses
  JsonDataObjects,
  MVCFramework.Serializer.Commons,
  MVCFramework.NullableTypes;

procedure RegisterNullableTypeSerializersInSerializer(ASerializer: IMVCSerializer);
begin
  ASerializer.RegisterTypeSerializer(TypeInfo(TNullableInteger), TNullableIntegerSerializer.Create);
  ASerializer.RegisterTypeSerializer(TypeInfo(TNullableInt64), TNullableInt64Serializer.Create);
  ASerializer.RegisterTypeSerializer(TypeInfo(TNullableCurrency), TNullableCurrencySerializer.Create);
  ASerializer.RegisterTypeSerializer(TypeInfo(TNullableString), TNullableStringSerializer.Create);
  ASerializer.RegisterTypeSerializer(TypeInfo(TNullableDateTime), TNullableDateTimeSerializer.Create);
  ASerializer.RegisterTypeSerializer(TypeInfo(TNullableDate), TNullableDateSerializer.Create);
  ASerializer.RegisterTypeSerializer(TypeInfo(TNullableTime), TNullableTimeSerializer.Create);
end;

{ TNullableIntegerSerializer }

procedure TNullableIntegerSerializer.Deserialize(const ASerializedObject: TObject; var AElementValue: TValue;
  const AAttributes: TArray<TCustomAttribute>);
begin
  raise EMVCSerializationException.Create('Not implemented');
end;

procedure TNullableIntegerSerializer.DeserializeAttribute(var AElementValue: TValue; const APropertyName: string;
  const ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
var
  LJSON: TJDOJsonObject;
  LNullInt: TNullableInteger;
begin
  LJSON := ASerializerObject as TJDOJsonObject;
  LNullInt := AElementValue.AsType<TNullableInteger>;
  if LJSON.Values[APropertyName].Typ in [jdtNone, jdtObject] then { json nulls are recognized as jdtObject }
  begin
    LNullInt := nil;
  end
  else
  begin
    LNullInt := LJSON.I[APropertyName];
  end;
  AElementValue := TValue.From<TNullableInteger>(LNullInt);
end;

procedure TNullableIntegerSerializer.DeserializeRoot(const ASerializerObject, AObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
begin
  raise EMVCSerializationException.Create('Not implemented');
end;

procedure TNullableIntegerSerializer.SerializeAttribute(const AElementValue: TValue; const APropertyName: string;
  const ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
var
  LNullInteger: TNullableInteger;
begin
  LNullInteger := AElementValue.AsType<TNullableInteger>;
  if LNullInteger.HasValue then
    (ASerializerObject as TJDOJsonObject).I[APropertyName] := LNullInteger.Value
  else
    (ASerializerObject as TJDOJsonObject).Values[APropertyName] := nil;
end;

procedure TNullableIntegerSerializer.SerializeRoot(const AObject: TObject; out ASerializerObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
begin
  raise EMVCSerializationException.Create('Not implemented');
end;

{ TNullableInt64Serializer }

procedure TNullableInt64Serializer.Deserialize(const ASerializedObject: TObject; var AElementValue: TValue;
  const AAttributes: TArray<TCustomAttribute>);
begin
  raise EMVCSerializationException.Create('Not implemented');
end;

procedure TNullableInt64Serializer.DeserializeAttribute(var AElementValue: TValue; const APropertyName: string;
  const ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
var
  LJSON: TJDOJsonObject;
  LNullInt64: TNullableInt64;
begin
  LJSON := ASerializerObject as TJDOJsonObject;
  LNullInt64 := AElementValue.AsType<TNullableInt64>;
  if LJSON.Values[APropertyName].Typ in [jdtNone, jdtObject] then { json nulls are recognized as jdtObject }
  begin
    LNullInt64 := nil;
  end
  else
  begin
    LNullInt64 := LJSON.L[APropertyName];
  end;
  AElementValue := TValue.From<TNullableInt64>(LNullInt64);
end;

procedure TNullableInt64Serializer.DeserializeRoot(const ASerializerObject, AObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
begin
  raise EMVCSerializationException.Create('Not implemented');
end;

procedure TNullableInt64Serializer.SerializeAttribute(const AElementValue: TValue; const APropertyName: string;
  const ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
var
  LNullInt64: TNullableInt64;
begin
  LNullInt64 := AElementValue.AsType<TNullableInt64>;
  if LNullInt64.HasValue then
    (ASerializerObject as TJDOJsonObject).L[APropertyName] := LNullInt64.Value
  else
    (ASerializerObject as TJDOJsonObject).Values[APropertyName] := nil;
end;

procedure TNullableInt64Serializer.SerializeRoot(const AObject: TObject; out ASerializerObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
begin
  raise EMVCSerializationException.Create('Not implemented');
end;

{ TNullableCurrencySerializer }

procedure TNullableCurrencySerializer.Deserialize(const ASerializedObject: TObject; var AElementValue: TValue;
  const AAttributes: TArray<TCustomAttribute>);
begin
  raise EMVCSerializationException.Create('Not implemented');
end;

procedure TNullableCurrencySerializer.DeserializeAttribute(var AElementValue: TValue; const APropertyName: string;
  const ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
var
  LJSON: TJDOJsonObject;
  LNullCurrency: TNullableCurrency;
begin
  LJSON := ASerializerObject as TJDOJsonObject;
  LNullCurrency := AElementValue.AsType<TNullableCurrency>;
  if LJSON.Values[APropertyName].Typ in [jdtNone, jdtObject] then { json nulls are recognized as jdtObject }
  begin
    LNullCurrency := nil;
  end
  else
  begin
    LNullCurrency := Currency(LJSON.F[APropertyName]);
  end;
  AElementValue := TValue.From<TNullableCurrency>(lNullCurrency);
end;

procedure TNullableCurrencySerializer.DeserializeRoot(const ASerializerObject, AObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
begin
  raise EMVCSerializationException.Create('Not implemented');
end;

procedure TNullableCurrencySerializer.SerializeAttribute(const AElementValue: TValue; const APropertyName: string;
  const ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
var
  LNullCurrency: TNullableCurrency;
begin
  LNullCurrency := AElementValue.AsType<TNullableCurrency>;
  if LNullCurrency.HasValue then
    (ASerializerObject as TJDOJsonObject).F[APropertyName] := LNullCurrency.Value
  else
    (ASerializerObject as TJDOJsonObject).Values[APropertyName] := nil;
end;

procedure TNullableCurrencySerializer.SerializeRoot(const AObject: TObject; out ASerializerObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
begin
  raise EMVCSerializationException.Create('Not implemented');
end;

{ TNullableStringSerializer }

procedure TNullableStringSerializer.Deserialize(const ASerializedObject: TObject; var AElementValue: TValue;
  const AAttributes: TArray<TCustomAttribute>);
begin
  raise EMVCSerializationException.Create('Not implemented');
end;

procedure TNullableStringSerializer.DeserializeAttribute(var AElementValue: TValue; const APropertyName: string;
  const ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
var
  LJSON: TJDOJsonObject;
  LNullString: TNullableString;
begin
  LJSON := ASerializerObject as TJDOJsonObject;
  LNullString := AElementValue.AsType<TNullableString>;
  if LJSON.Values[APropertyName].Typ in [jdtNone, jdtObject] then { json nulls are recognized as jdtObject }
  begin
    LNullString := nil;
  end
  else
  begin
    LNullString := LJSON.S[APropertyName];
  end;
  AElementValue := TValue.From<TNullableString>(LNullString);
end;

procedure TNullableStringSerializer.DeserializeRoot(const ASerializerObject, AObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
begin
  raise EMVCSerializationException.Create('Not implemented');
end;

procedure TNullableStringSerializer.SerializeAttribute(const AElementValue: TValue; const APropertyName: string;
  const ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
var
  LNullString: TNullableString;
begin
  LNullString := AElementValue.AsType<TNullableString>;
  if LNullString.HasValue then
    (ASerializerObject as TJDOJsonObject).S[APropertyName] := LNullString.Value
  else
    (ASerializerObject as TJDOJsonObject).Values[APropertyName] := nil;
end;

procedure TNullableStringSerializer.SerializeRoot(const AObject: TObject; out ASerializerObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
begin
  raise EMVCSerializationException.Create('Not implemented');
end;

{ TNullableDateTimeSerializer }

procedure TNullableDateTimeSerializer.Deserialize(const ASerializedObject: TObject; var AElementValue: TValue;
  const AAttributes: TArray<TCustomAttribute>);
begin
  raise EMVCSerializationException.Create('Not implemented');
end;

procedure TNullableDateTimeSerializer.DeserializeAttribute(var AElementValue: TValue; const APropertyName: string;
  const ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
var
  LJSON: TJDOJsonObject;
  LNullDateTime: TNullableDateTime;
begin
  LJSON := ASerializerObject as TJDOJsonObject;
  LNullDateTime := AElementValue.AsType<TNullableDateTime>;
  if LJSON.Values[APropertyName].Typ in [jdtNone, jdtObject] then { json nulls are recognized as jdtObject }
  begin
    LNullDateTime := nil;
  end
  else
  begin
    LNullDateTime := ISOTimeStampToDateTime(LJSON.S[APropertyName]);
  end;
  AElementValue := TValue.From<TNullableDateTime>(LNullDateTime);
end;

procedure TNullableDateTimeSerializer.DeserializeRoot(const ASerializerObject, AObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
begin
  raise EMVCSerializationException.Create('Not implemented');
end;

procedure TNullableDateTimeSerializer.SerializeAttribute(const AElementValue: TValue; const APropertyName: string;
  const ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
var
  LNullDateTime: TNullableDateTime;
begin
  LNullDateTime := AElementValue.AsType<TNullableDateTime>;
  if LNullDateTime.HasValue then
    (ASerializerObject as TJDOJsonObject).S[APropertyName] := DateTimeToISOTimeStamp(LNullDateTime.Value)
  else
    (ASerializerObject as TJDOJsonObject).Values[APropertyName] := nil;
end;

procedure TNullableDateTimeSerializer.SerializeRoot(const AObject: TObject; out ASerializerObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
begin
  raise EMVCSerializationException.Create('Not implemented');
end;

{ TNullableDateSerializer }

procedure TNullableDateSerializer.Deserialize(const ASerializedObject: TObject; var AElementValue: TValue;
  const AAttributes: TArray<TCustomAttribute>);
begin
  raise EMVCSerializationException.Create('Not implemented');
end;

procedure TNullableDateSerializer.DeserializeAttribute(var AElementValue: TValue; const APropertyName: string;
  const ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
var
  LJSON: TJDOJsonObject;
  LNullDate: TNullableDate;
begin
  LJSON := ASerializerObject as TJDOJsonObject;
  LNullDate := AElementValue.AsType<TNullableDate>;
  if LJSON.Values[APropertyName].Typ in [jdtNone, jdtObject] then { json nulls are recognized as jdtObject }
  begin
    LNullDate := nil;
  end
  else
  begin
    LNullDate := ISODateToDate(LJSON.S[APropertyName]);
  end;
  AElementValue := TValue.From<TNullableDate>(LNullDate);
end;

procedure TNullableDateSerializer.DeserializeRoot(const ASerializerObject, AObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
begin
  raise EMVCSerializationException.Create('Not implemented');
end;

procedure TNullableDateSerializer.SerializeAttribute(const AElementValue: TValue; const APropertyName: string;
  const ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
var
  LNullDate: TNullableDate;
begin
  LNullDate := AElementValue.AsType<TNullableDate>;
  if LNullDate.HasValue then
    (ASerializerObject as TJDOJsonObject).S[APropertyName] := DateToISODate(LNullDate.Value)
  else
    (ASerializerObject as TJDOJsonObject).Values[APropertyName] := nil;
end;

procedure TNullableDateSerializer.SerializeRoot(const AObject: TObject; out ASerializerObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
begin
  raise EMVCSerializationException.Create('Not implemented');
end;

{ TNullableTimeSerializer }

procedure TNullableTimeSerializer.Deserialize(const ASerializedObject: TObject; var AElementValue: TValue;
  const AAttributes: TArray<TCustomAttribute>);
begin
  raise EMVCSerializationException.Create('Not implemented');
end;

procedure TNullableTimeSerializer.DeserializeAttribute(var AElementValue: TValue; const APropertyName: string;
  const ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
var
  LJSON: TJDOJsonObject;
  LNullTime: TNullableTime;
begin
  LJSON := ASerializerObject as TJDOJsonObject;
  LNullTime := AElementValue.AsType<TNullableTime>;
  if LJSON.Values[APropertyName].Typ in [jdtNone, jdtObject] then { json nulls are recognized as jdtObject }
  begin
    LNullTime := nil;
  end
  else
  begin
    LNullTime := ISOTimeToTime(LJSON.S[APropertyName]);
  end;
  AElementValue := TValue.From<TNullableTime>(LNullTime);
end;

procedure TNullableTimeSerializer.DeserializeRoot(const ASerializerObject, AObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
begin
  raise EMVCSerializationException.Create('Not implemented');
end;

procedure TNullableTimeSerializer.SerializeAttribute(const AElementValue: TValue; const APropertyName: string;
  const ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
var
  LNullTime: TNullableTime;
begin
  LNullTime := AElementValue.AsType<TNullableTime>;
  if LNullTime.HasValue then
    (ASerializerObject as TJDOJsonObject).S[APropertyName] := TimeToISOTime(LNullTime.Value)
  else
    (ASerializerObject as TJDOJsonObject).Values[APropertyName] := nil;
end;

procedure TNullableTimeSerializer.SerializeRoot(const AObject: TObject; out ASerializerObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
begin
  raise EMVCSerializationException.Create('Not implemented');
end;

end.
