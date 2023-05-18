// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
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
// ***************************************************************************

unit CustomTypesSerializersU;

interface

uses
  MVCFramework.Serializer.Intf,
  System.Rtti, MVCFramework.Serializer.Commons;

type
  // Custom serializer for TNullableAliasSerializer type
  TNullableIntegerSerializer = class(TInterfacedObject, IMVCTypeSerializer)
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
      const AAttributes: TArray<TCustomAttribute>;
      const ASerializationAction: TMVCSerializationAction = nil
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
      const AAttributes: TArray<TCustomAttribute>;
      const ASerializationAction: TMVCSerializationAction = nil
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
      const AAttributes: TArray<TCustomAttribute>;
      const ASerializationAction: TMVCSerializationAction = nil
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

  TPersonSerializer = class(TInterfacedObject, IMVCTypeSerializer)
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
      const AAttributes: TArray<TCustomAttribute>;
      const ASerializationAction: TMVCSerializationAction = nil
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

implementation

uses
  JsonDataObjects,
  Spring,
  MVCFramework.Serializer.JsonDataObjects,
  BusinessObjectsU,
  System.SysUtils;

procedure TNullableIntegerSerializer.Deserialize(const ASerializedObject: TObject;
  var AElementValue: TValue; const AAttributes: TArray<TCustomAttribute>);
begin

end;

procedure TNullableIntegerSerializer.DeserializeAttribute(
  var AElementValue: TValue;
  const APropertyName: string;
  const ASerializerObject: TObject;
  const AAttributes: TArray<TCustomAttribute>
  );
var
  lJSON: TJDOJsonObject;
  lNullInt: Nullable<Integer>;
begin
  lJSON := ASerializerObject as TJDOJsonObject;
  lNullInt := AElementValue.AsType<Nullable<Integer>>;
  if lJSON.Values[APropertyName].Typ in [jdtNone, jdtObject { json nulls are recognized as jdtObject } ] then
  begin
    lNullInt := nil;
  end
  else
  begin
    lNullInt := lJSON.I[APropertyName];
  end;
  AElementValue := TValue.From < Nullable < Integer >> (lNullInt);
end;

procedure TNullableIntegerSerializer.DeserializeRoot(const ASerializerObject,
  AObject: TObject; const AAttributes: TArray<TCustomAttribute>);
begin
  raise Exception.Create('Not implemented');
end;

procedure TNullableIntegerSerializer.SerializeAttribute(const AElementValue: TValue;
  const APropertyName: string; const ASerializerObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
var
  lNullInteger: Nullable<Integer>;
begin
  lNullInteger := AElementValue.AsType<TNullableInteger>;
  if lNullInteger.HasValue then
    (ASerializerObject as TJDOJsonObject).I[APropertyName] := lNullInteger.Value
  else
    (ASerializerObject as TJDOJsonObject).Values[APropertyName] := nil;
end;

procedure TNullableIntegerSerializer.SerializeRoot(
      const AObject: TObject;
      out ASerializerObject: TObject;
      const AAttributes: TArray<TCustomAttribute>;
      const ASerializationAction: TMVCSerializationAction = nil
      );
begin
  raise EMVCSerializationException.Create('Not supported');
end;

{ TNullableCurrencySerializer }

procedure TNullableCurrencySerializer.Deserialize(
  const ASerializedObject: TObject; var AElementValue: TValue;
  const AAttributes: TArray<TCustomAttribute>);
begin

end;

procedure TNullableCurrencySerializer.DeserializeAttribute(
  var AElementValue: TValue;
  const APropertyName: string;
  const ASerializerObject: TObject;
  const AAttributes: TArray<TCustomAttribute>
  );
var
  lJSON: TJDOJsonObject;
  lNullCurrency: Nullable<Currency>;
begin
  lJSON := ASerializerObject as TJDOJsonObject;
  lNullCurrency := AElementValue.AsType<Nullable<Currency>>;
  if lJSON.Values[APropertyName].Typ in [jdtNone, jdtObject { json nulls are recognized as jdtObject } ] then
  begin
    lNullCurrency := nil;
  end
  else
  begin
    lNullCurrency := Currency(lJSON.F[APropertyName]);
  end;
  AElementValue := TValue.From < Nullable < Currency >> (lNullCurrency);
end;

procedure TNullableCurrencySerializer.DeserializeRoot(const ASerializerObject,
  AObject: TObject; const AAttributes: TArray<TCustomAttribute>);
begin
  raise Exception.Create('Not implemented');
end;

procedure TNullableCurrencySerializer.SerializeAttribute(const AElementValue: TValue;
  const APropertyName: string; const ASerializerObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
var
  lNullCurrency: Nullable<Currency>;
begin
  lNullCurrency := AElementValue.AsType<TNullableCurrency>;
  if lNullCurrency.HasValue then
    (ASerializerObject as TJDOJsonObject).F[APropertyName] := lNullCurrency.Value
  else
    (ASerializerObject as TJDOJsonObject).Values[APropertyName] := nil;
end;

procedure TNullableCurrencySerializer.SerializeRoot(
      const AObject: TObject;
      out ASerializerObject: TObject;
      const AAttributes: TArray<TCustomAttribute>;
      const ASerializationAction: TMVCSerializationAction = nil
      );
begin
  raise EMVCSerializationException.Create('Not supported');
end;

{ TNullableStringSerializer }

procedure TNullableStringSerializer.Deserialize(
  const ASerializedObject: TObject; var AElementValue: TValue;
  const AAttributes: TArray<TCustomAttribute>);
begin

end;

procedure TNullableStringSerializer.DeserializeAttribute(
  var AElementValue: TValue;
  const APropertyName: string;
  const ASerializerObject: TObject;
  const AAttributes: TArray<TCustomAttribute>
  );
var
  lJSON: TJDOJsonObject;
  lNullString: Nullable<string>;
begin
  lJSON := ASerializerObject as TJDOJsonObject;
  lNullString := AElementValue.AsType<Nullable<string>>;
  if lJSON.Values[APropertyName].Typ in [jdtNone, jdtObject { json nulls are recognized as jdtObject } ] then
  begin
    lNullString := nil;
  end
  else
  begin
    lNullString := lJSON.S[APropertyName];
  end;
  AElementValue := TValue.From < Nullable < string >> (lNullString);
end;

procedure TNullableStringSerializer.DeserializeRoot(const ASerializerObject,
  AObject: TObject; const AAttributes: TArray<TCustomAttribute>);
begin
  raise Exception.Create('Not implemented');
end;

procedure TNullableStringSerializer.SerializeAttribute(const AElementValue: TValue;
  const APropertyName: string; const ASerializerObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
var
  lNullString: Nullable<string>;
begin
  lNullString := AElementValue.AsType<TNullableString>;
  if lNullString.HasValue then
    (ASerializerObject as TJDOJsonObject).S[APropertyName] := lNullString.Value
  else
    (ASerializerObject as TJDOJsonObject).Values[APropertyName] := nil;
end;

procedure TNullableStringSerializer.SerializeRoot(
      const AObject: TObject;
      out ASerializerObject: TObject;
      const AAttributes: TArray<TCustomAttribute>;
      const ASerializationAction: TMVCSerializationAction = nil
      );
begin
  raise EMVCSerializationException.Create('Not supported');
end;

{ TPersonSerializer }

procedure TPersonSerializer.Deserialize(const ASerializedObject: TObject;
  var AElementValue: TValue; const AAttributes: TArray<TCustomAttribute>);
begin

end;

procedure TPersonSerializer.DeserializeAttribute(
  var AElementValue: TValue;
  const APropertyName: string;
  const ASerializerObject: TObject;
  const AAttributes: TArray<TCustomAttribute>
  );
begin
  raise Exception.Create('Not implemented');
end;

procedure TPersonSerializer.DeserializeRoot(const ASerializerObject,
  AObject: TObject; const AAttributes: TArray<TCustomAttribute>);
begin
  raise Exception.Create('Not implemented');
end;

procedure TPersonSerializer.SerializeAttribute(const AElementValue: TValue;
  const APropertyName: string; const ASerializerObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
var
  lPerson: TPerson;
  lJSON: TJDOJsonObject;
begin
  lPerson := AElementValue.AsObject as TPerson;
  lJSON := TJDOJsonObject.Create;
  lJSON.S['firstname'] := lPerson.FirstName;
  lJSON.B['customserialization'] := True;
  TJDOJsonObject(ASerializerObject).O[APropertyName] := lJSON;
end;

procedure TPersonSerializer.SerializeRoot(
      const AObject: TObject;
      out ASerializerObject: TObject;
      const AAttributes: TArray<TCustomAttribute>;
      const ASerializationAction: TMVCSerializationAction = nil
      );
var
  lPerson: TPerson;
  lJSON: TJDOJsonObject;
begin
  lPerson := AObject as TPerson;
  lJSON := TJDOJsonObject.Create;
  lJSON.S['firstname'] := lPerson.FirstName;
  lJSON.B['customserialization'] := True;
  ASerializerObject := lJSON;
end;

end.
