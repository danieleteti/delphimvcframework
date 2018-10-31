// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2018 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.Serializer.JsonDataObjects.CustomTypes;

{$I dmvcframework.inc}

interface

uses
  System.Rtti,
  System.Classes,
  System.SysUtils,
  MVCFramework.Serializer.Intf,
  MVCFramework.Serializer.Commons;

type

  TMVCStreamSerializerJsonDataObject = class(TInterfacedObject, IMVCTypeSerializer)
  protected
    // procedure Serialize(const AElementValue: TValue; var ASerializerObject: TObject;
    // const AAttributes: TArray<TCustomAttribute>);
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
      const ASerializerObject: TObject; const AObject: TObject;
      const AAttributes: TArray<TCustomAttribute>);

  public
    { public declarations }
  end;

  TMVCStringDictionarySerializer = class(TInterfacedObject, IMVCTypeSerializer)
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
  end;

implementation

uses
  MVCFramework.Serializer.JsonDataObjects,
  Data.DB,
  MVCFramework.Commons,
  System.Generics.Collections,
  JsonDataObjects;

procedure TMVCStreamSerializerJsonDataObject.DeserializeAttribute(
  var AElementValue: TValue;
  const APropertyName: string;
  const ASerializerObject: TObject;
  const AAttributes: TArray<TCustomAttribute>
  );
var
  lStream: TStream;
  SS: TStringStream;
  lJSON: TJDOJsonObject;
begin
  lJSON := ASerializerObject as TJDOJsonObject;
  if Assigned(lJSON) then
  begin
    lStream := AElementValue.AsObject as TStream;
    lStream.Size := 0;
    if Assigned(lStream) then
    begin
      SS := TStringStream.Create(lJSON.S[APropertyName]);
      try
        SS.Position := 0;
        if TMVCSerializerHelper.AttributeExists<MVCSerializeAsStringAttribute>(AAttributes) then
        begin
          lStream.CopyFrom(SS, 0);
        end
        else
        begin
          TMVCSerializerHelper.DecodeStream(SS, lStream);
        end;
      finally
        SS.Free;
      end;
    end;
  end;
end;

procedure TMVCStreamSerializerJsonDataObject.DeserializeRoot(
  const ASerializerObject: TObject; const AObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
var
  lValue: TValue;
begin
  lValue := AObject;
  DeserializeAttribute(lValue, 'data', ASerializerObject, AAttributes);
end;

procedure TMVCStreamSerializerJsonDataObject.SerializeAttribute(
  const AElementValue: TValue; const APropertyName: string;
  const ASerializerObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
var
  Stream: TStream;
  SS: TStringStream;
begin
  Stream := AElementValue.AsObject as TStream;
  if Assigned(Stream) then
  begin
    if TMVCSerializerHelper.AttributeExists<MVCSerializeAsStringAttribute>(AAttributes) then
    begin
      SS := TStringStream.Create;
      try
        Stream.Position := 0;
        SS.CopyFrom(Stream, Stream.Size);
        TJsonObject(ASerializerObject).S[APropertyName] := SS.DataString;
      finally
        SS.Free;
      end;
    end
    else
    begin
      SS := TStringStream.Create;
      try
        Stream.Position := 0;
        TMVCSerializerHelper.EncodeStream(Stream, SS);
        TJsonObject(ASerializerObject).S[APropertyName] := SS.DataString;
      finally
        SS.Free;
      end;
    end;
  end
  else
  begin
    TJsonObject(ASerializerObject).Values[APropertyName] := nil;
  end;
end;

procedure TMVCStreamSerializerJsonDataObject.SerializeRoot(const AObject: TObject;
  out ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
var
  lSerializerObject: TJsonObject;
begin
  lSerializerObject := TJsonObject.Create;
  try
    SerializeAttribute(AObject, 'data', ASerializerObject, AAttributes);
  except
    lSerializerObject.Free;
    raise;
  end;
end;

{ TMVCStringDictionarySerializer }

procedure TMVCStringDictionarySerializer.DeserializeAttribute(
  var AElementValue: TValue;
  const APropertyName: string;
  const ASerializerObject: TObject;
  const AAttributes: TArray<TCustomAttribute>
  );
var
  lStringDict: TMVCStringDictionary;
  lJSON: TJDOJsonObject;
  i: Integer;
begin
  lStringDict := AElementValue.AsObject as TMVCStringDictionary;
  lJSON := ASerializerObject as TJDOJsonObject;
  for i := 0 to lJSON.O[APropertyName].Count - 1 do
  begin
    lStringDict.AddProperty(lJSON.Names[i], lJSON.S[lJSON.Names[i]])
  end;
end;

procedure TMVCStringDictionarySerializer.DeserializeRoot(
  const ASerializerObject: TObject;
  const AObject: TObject;
  const AAttributes: TArray<TCustomAttribute>
  );
var
  lStringDict: TMVCStringDictionary;
  lJSON: TJDOJsonObject;
  i: Integer;
begin
  lStringDict := AObject as TMVCStringDictionary;
  lJSON := ASerializerObject as TJDOJsonObject;
  for i := 0 to lJSON.Count - 1 do
  begin
    lStringDict.AddProperty(lJSON.Names[i], lJSON.S[lJSON.Names[i]])
  end;
end;

procedure TMVCStringDictionarySerializer.SerializeAttribute(
  const AElementValue: TValue; const APropertyName: string;
  const ASerializerObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
var
  lStringDict: TMVCStringDictionary;
  lPair: TPair<string, string>;
  lOutObject: TJsonObject;
  lJsonDict: TJsonObject;
begin
  lStringDict := AElementValue.AsObject as TMVCStringDictionary;
  lOutObject := ASerializerObject as TJsonObject;
  lOutObject.O[APropertyName] := TJsonObject.Create;
  lJsonDict := lOutObject.O[APropertyName];
  if Assigned(lStringDict) then
  begin
    for lPair in lStringDict do
    begin
      lJsonDict.S[lPair.Key] := lPair.Value;
    end;
  end;
end;

procedure TMVCStringDictionarySerializer.SerializeRoot(const AObject: TObject;
  out ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
var
  lStringDict: TMVCStringDictionary;
  lPair: TPair<string, string>;
  lOutObject: TJsonObject;
begin
  lStringDict := AObject as TMVCStringDictionary;
  lOutObject := TJsonObject.Create;
  if Assigned(lStringDict) then
  begin
    for lPair in lStringDict do
    begin
      lOutObject.S[lPair.Key] := lPair.Value;
    end;
  end;
  ASerializerObject := lOutObject;
end;

end.
