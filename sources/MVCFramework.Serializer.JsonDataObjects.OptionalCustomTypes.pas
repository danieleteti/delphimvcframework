// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.Serializer.JsonDataObjects.OptionalCustomTypes;

{$IFDEF LINUX}
{$ERROR 'This unit is not compatible with linux!'}
{$ENDIF}

interface

uses
  System.Rtti,
  System.Generics.Collections,
  MVCFramework.Serializer.Intf,
  MVCFramework.Serializer.Commons;

type
  TMVCBitmapSerializerJsonDataObject = class(TInterfacedObject, IMVCTypeSerializer)
  protected
    procedure SerializeAttribute(const AElementValue: TValue; const APropertyName: string;
      const ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
    procedure SerializeRoot(const AObject: TObject; out ASerializerObject: TObject;
      const AAttributes: TArray<TCustomAttribute>; const ASerializationAction: TMVCSerializationAction = nil);

    procedure DeserializeAttribute(var AElementValue: TValue; const APropertyName: string;
      const ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);

    procedure DeserializeRoot(const ASerializerObject: TObject; const AObject: TObject;
      const AAttributes: TArray<TCustomAttribute>);

  end;

procedure RegisterOptionalCustomTypesSerializers(const JDOSerializer: IMVCSerializer); overload;
procedure RegisterOptionalCustomTypesSerializersForJSON(const Serializers
  : TDictionary<string, IMVCSerializer>); overload;

implementation

uses
{$IFDEF MSWINDOWS}
  Vcl.Graphics, {do not specialize VCL or FMX, let use the UnitScope defined in the project}
  Vcl.Imaging.pngimage, {do not specialize VCL or FMX, le use the UnitScope defined in the project}
  Vcl.Imaging.jpeg, {do not specialize VCL or FMX, le use the UnitScope defined in the project}
{$ENDIF}
  System.Classes,
  System.SysUtils,
  JsonDataObjects,
  MVCFramework.Commons;

procedure RegisterOptionalCustomTypesSerializers(const JDOSerializer: IMVCSerializer);
begin
{$IFDEF MSWINDOWS}
  JDOSerializer
    .RegisterTypeSerializer(TypeInfo(TBitmap), TMVCBitmapSerializerJsonDataObject.Create);
  JDOSerializer
    .RegisterTypeSerializer(TypeInfo(TPngImage), TMVCBitmapSerializerJsonDataObject.Create);
  JDOSerializer
    .RegisterTypeSerializer(TypeInfo(TJPEGImage), TMVCBitmapSerializerJsonDataObject.Create);
{$ENDIF}
end;

procedure RegisterOptionalCustomTypesSerializersForJSON(const Serializers: TDictionary<string, IMVCSerializer>);
begin
  RegisterOptionalCustomTypesSerializers(Serializers.Items[TMVCMediaType.APPLICATION_JSON]);
end;

{ TMVCBitmapSerializerJsonDataObject }

procedure TMVCBitmapSerializerJsonDataObject.DeserializeAttribute(
  var AElementValue: TValue; const APropertyName: string;
  const ASerializerObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
var
  lGraphic: TGraphic;
  SS: TStringStream;
  lJSON: TJDOJsonObject;
  lTmpStream: TMemoryStream;
begin
  lJSON := ASerializerObject as TJDOJsonObject;
  if Assigned(lJSON) then
  begin
    lGraphic := AElementValue.AsObject as TGraphic;
    if Assigned(lGraphic) then
    begin
      SS := TStringStream.Create(lJSON.S[APropertyName]);
      try
        SS.Position := 0;
        lTmpStream := TMemoryStream.Create;
        try
          TMVCSerializerHelper.DecodeStream(SS, lTmpStream);
          lTmpStream.Position := 0;
          lGraphic.LoadFromStream(lTmpStream);
        finally
          lTmpStream.Free;
        end;
      finally
        SS.Free;
      end;
    end;
  end;
end;

procedure TMVCBitmapSerializerJsonDataObject.DeserializeRoot(
  const ASerializerObject, AObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
begin
  raise EMVCDeserializationException.Create('Direct image deserialization not supported');
end;

procedure TMVCBitmapSerializerJsonDataObject.SerializeAttribute(
  const AElementValue: TValue; const APropertyName: string;
  const ASerializerObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
var
  lGraphic: TGraphic;
  lTmpStream: TMemoryStream;
  lStringStream: TStringStream;
begin
  lGraphic := AElementValue.AsObject as TGraphic;
  if Assigned(lGraphic) then
  begin
    lStringStream := TStringStream.Create('', TEncoding.ASCII);
    try
      lTmpStream := TMemoryStream.Create;
      try
        lGraphic.SaveToStream(lTmpStream);
        lTmpStream.Position := 0;
        TMVCSerializerHelper.EncodeStream(lTmpStream, lStringStream);
      finally
        lTmpStream.Free;
      end;
      TJDOJsonObject(ASerializerObject).S[APropertyName] := lStringStream.DataString;
    finally
      lStringStream.Free;
    end;
  end
  else
  begin
    TJsonObject(ASerializerObject).Values[APropertyName] := nil;
  end;
end;

procedure TMVCBitmapSerializerJsonDataObject.SerializeRoot(
  const AObject: TObject; out ASerializerObject: TObject;
  const AAttributes: TArray<TCustomAttribute>;
  const ASerializationAction: TMVCSerializationAction);
begin
  ASerializerObject := TJsonObject.Create;
  try
    SerializeAttribute(AObject, 'data', ASerializerObject, AAttributes);
  except
    ASerializerObject.Free;
    raise;
  end;
end;

end.
