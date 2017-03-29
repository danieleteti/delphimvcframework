// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2017 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.Serializer.JSON.CustomTypes;

{$I dmvcframework.inc}

interface

uses
  System.Rtti,
  System.Classes,
  System.SysUtils,
  System.JSON,
  MVCFramework.Serializer.Intf,
  MVCFramework.Serializer.Commons;

type

  TStreamSerializerJSON = class(TInterfacedObject, IMVCTypeSerializer)
  private
    { private declarations }
  protected
    procedure Serialize(const AElementValue: TValue; var ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
    procedure Deserialize(const ASerializedObject: TObject; var AElementValue: TValue; const AAttributes: TArray<TCustomAttribute>);
  public
    { public declarations }
  end;

implementation

{ TStreamSerializerJSON }

procedure TStreamSerializerJSON.Deserialize(
  const ASerializedObject: TObject; var AElementValue: TValue;
  const AAttributes: TArray<TCustomAttribute>);
var
  JSONValue: TJSONValue;
  Stream: TStream;
  SS: TStringStream;
begin
  JSONValue := ASerializedObject as TJSONValue;
  if Assigned(JSONValue) then
  begin
    Stream := AElementValue.AsObject as TStream;
    if Assigned(Stream) then
    begin
      if TMVCSerializerHelpful.AttributeExists<MVCSerializeAsStringAttribute>(AAttributes) then
      begin
        SS := TStringStream.Create(JSONValue.Value);
        try
          SS.Position := 0;
          Stream.CopyFrom(SS, SS.Size);
        finally
          SS.Free;
        end;
      end
      else
      begin
        SS := TStringStream.Create(JSONValue.Value);
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

procedure TStreamSerializerJSON.Serialize(const AElementValue: TValue;
  var ASerializerObject: TObject;
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
        ASerializerObject := TJSONString.Create(SS.DataString);
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
        ASerializerObject := TJSONString.Create(SS.DataString);
      finally
        SS.Free;
      end;
    end;
  end;
end;

end.
