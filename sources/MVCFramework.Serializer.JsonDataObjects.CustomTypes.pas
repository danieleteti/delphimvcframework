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

  TStreamSerializerJsonDataObject = class(TInterfacedObject, IMVCTypeSerializer)
  private
    { private declarations }
  protected
    procedure Serialize(const AElementValue: TValue; var ASerializerObject: TObject;
      const AAttributes: TArray<TCustomAttribute>);
    procedure Deserialize(const ASerializedObject: TObject; var AElementValue: TValue;
      const AAttributes: TArray<TCustomAttribute>);
  public
    { public declarations }
  end;

  TMVCStringDictionarySerializer = class(TInterfacedObject, IMVCTypeSerializer)
  public
    procedure Serialize(const AElementValue: TValue; var ASerializerObject: TObject;
      const AAttributes: System.TArray<System.TCustomAttribute>);
    procedure Deserialize(const ASerializedObject: TObject; var AElementValue: TValue;
      const AAttributes: System.TArray<System.TCustomAttribute>);
  end;

implementation

uses
  MVCFramework.Serializer.JsonDataObjects,
  Data.DB,
  MVCFramework.Commons,
  System.Generics.Collections,
  JsonDataObjects;

{ TStreamSerializerJsonDataObject }

procedure TStreamSerializerJsonDataObject.Deserialize(const ASerializedObject: TObject; var AElementValue: TValue;
  const AAttributes: TArray<TCustomAttribute>);
var
  JsonValue: TJsonValue;
  Stream: TStream;
  SS: TStringStream;
begin
  JsonValue := ASerializedObject as TJsonValue;
  if Assigned(JsonValue) then
  begin
    Stream := AElementValue.AsObject as TStream;
    if Assigned(Stream) then
    begin
      if TMVCSerializerHelpful.AttributeExists<MVCSerializeAsStringAttribute>(AAttributes) then
      begin
        SS := TStringStream.Create(JsonValue.Value);
        try
          SS.Position := 0;
          Stream.CopyFrom(SS, SS.Size);
        finally
          SS.Free;
        end;
      end
      else
      begin
        SS := TStringStream.Create(JsonValue.Value);
        try
          SS.Position := 0;
          TMVCSerializerHelpful.DecodeStream(SS, Stream);
        finally
          SS.Free;
        end;
      end;
    end;
  end;
end;

procedure TStreamSerializerJsonDataObject.Serialize(const AElementValue: TValue; var ASerializerObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
var
  Stream: TStream;
  SS: TStringStream;
  DataString: string;
begin
  Stream := AElementValue.AsObject as TStream;
  if Assigned(Stream) then
  begin
    if TMVCSerializerHelpful.AttributeExists<MVCSerializeAsStringAttribute>(AAttributes) then
    begin
      Stream.Position := 0;
      SS := TStringStream.Create;
      try
        SS.CopyFrom(Stream, Stream.Size);
        DataString := SS.DataString;
        ASerializerObject := TJsonValue.Create(SS.DataString);
      finally
        SS.Free;
      end;
    end
    else
    begin
      SS := TStringStream.Create;
      try
        Stream.Position := 0;
        TMVCSerializerHelpful.EncodeStream(Stream, SS);
        ASerializerObject := TJsonValue.Create(SS.DataString);
      finally
        SS.Free;
      end;
    end;
  end;
end;

{ TMVCStringDictionarySerializer }

procedure TMVCStringDictionarySerializer.Deserialize(const ASerializedObject: TObject; var AElementValue: TValue;
  const AAttributes: System.TArray<System.TCustomAttribute>);
begin
  raise EMVCDeserializationException.Create('Not Implemented');
end;

procedure TMVCStringDictionarySerializer.Serialize(const AElementValue: TValue; var ASerializerObject: TObject;
  const AAttributes: System.TArray<System.TCustomAttribute>);
var
  lStringDict: TMVCStringDictionary;
  lPair: TPair<string, string>;
  lJSONObject: TJsonObject;
begin
  lStringDict := AElementValue.AsObject as TMVCStringDictionary;
  if Assigned(lStringDict) then
  begin
    lJSONObject := TJsonObject.Create;
    for lPair in lStringDict do
    begin
      lJSONObject.S[lPair.Key] := lPair.Value;
    end;
    ASerializerObject := lJSONObject;
  end;
end;

end.
