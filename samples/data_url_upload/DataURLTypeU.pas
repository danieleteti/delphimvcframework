// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
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

unit DataURLTypeU;

interface

uses
  System.SysUtils,
  System.NetEncoding,
  System.Rtti,
  JsonDataObjects,
  MVCFramework.Serializer.Intf,
  MVCFramework.Serializer.Commons;

type
  // Holds a payload decoded from a RFC 2397 data URL together with its MIME type.
  // Sample input received from the browser:
  //   data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAA...
  TDataURL = record
    MimeType: string;
    Data: TBytes;
    function IsEmpty: Boolean;
    function Size: Integer;
    function ToDataURL: string;
    class function Parse(const ADataURL: string): TDataURL; static;
  end;

  // Custom type serializer that plugs TDataURL into MVCFramework.Serializer.JsonDataObjects.
  // Register once per content type at startup, e.g.:
  //   FMVC.Serializers.Items[TMVCMediaType.APPLICATION_JSON]
  //     .RegisterTypeSerializer(TypeInfo(TDataURL), TDataURLSerializer.Create);
  TDataURLSerializer = class(TInterfacedObject, IMVCTypeSerializer)
  public
    procedure SerializeAttribute(const AElementValue: TValue; const APropertyName: string;
      const ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
    procedure SerializeRoot(const AObject: TObject; out ASerializerObject: TObject;
      const AAttributes: TArray<TCustomAttribute>;
      const ASerializationAction: TMVCSerializationAction = nil);
    procedure DeserializeAttribute(var AElementValue: TValue; const APropertyName: string;
      const ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
    procedure DeserializeRoot(const ASerializerObject: TObject; const AObject: TObject;
      const AAttributes: TArray<TCustomAttribute>);
  end;

implementation

{ TDataURL }

function TDataURL.IsEmpty: Boolean;
begin
  Result := Length(Data) = 0;
end;

function TDataURL.Size: Integer;
begin
  Result := Length(Data);
end;

class function TDataURL.Parse(const ADataURL: string): TDataURL;
var
  lHeader, lPayload: string;
  lCommaPos, lSemiPos: Integer;
begin
  if not ADataURL.StartsWith('data:', True) then
    raise EConvertError.Create('Not a data URL: missing "data:" prefix');

  lCommaPos := ADataURL.IndexOf(',');
  if lCommaPos < 0 then
    raise EConvertError.Create('Malformed data URL: missing comma separator');

  lHeader  := ADataURL.Substring(5, lCommaPos - 5); // skip 'data:'
  lPayload := ADataURL.Substring(lCommaPos + 1);

  lSemiPos := lHeader.IndexOf(';');
  if lSemiPos >= 0 then
    Result.MimeType := lHeader.Substring(0, lSemiPos)
  else
    Result.MimeType := lHeader;

  if Result.MimeType = '' then
    Result.MimeType := 'text/plain';

  if lHeader.Contains('base64') then
    Result.Data := TNetEncoding.Base64.DecodeStringToBytes(lPayload)
  else
    Result.Data := TEncoding.UTF8.GetBytes(TNetEncoding.URL.Decode(lPayload));
end;

function TDataURL.ToDataURL: string;
begin
  if MimeType = '' then
    Result := Format('data:application/octet-stream;base64,%s',
      [TNetEncoding.Base64.EncodeBytesToString(Data)])
  else
    Result := Format('data:%s;base64,%s',
      [MimeType, TNetEncoding.Base64.EncodeBytesToString(Data)]);
end;

{ TDataURLSerializer }

procedure TDataURLSerializer.SerializeAttribute(const AElementValue: TValue;
  const APropertyName: string; const ASerializerObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
begin
  (ASerializerObject as TJDOJsonObject).S[APropertyName] :=
    AElementValue.AsType<TDataURL>.ToDataURL;
end;

procedure TDataURLSerializer.DeserializeAttribute(var AElementValue: TValue;
  const APropertyName: string; const ASerializerObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
var
  lJSON: TJDOJsonObject;
  lRaw: string;
begin
  lJSON := ASerializerObject as TJDOJsonObject;
  if not lJSON.Contains(APropertyName) then
    Exit;

  lRaw := lJSON.S[APropertyName];
  if lRaw = '' then
    Exit;

  AElementValue := TValue.From<TDataURL>(TDataURL.Parse(lRaw));
end;

procedure TDataURLSerializer.SerializeRoot(const AObject: TObject;
  out ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>;
  const ASerializationAction: TMVCSerializationAction);
begin
  raise EMVCSerializationException.Create('TDataURL cannot be used as root');
end;

procedure TDataURLSerializer.DeserializeRoot(const ASerializerObject, AObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
begin
  raise EMVCSerializationException.Create('TDataURL cannot be used as root');
end;

end.
