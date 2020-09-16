// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2020 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.Serializer.HTML;

{$I dmvcframework.inc}
{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  System.Classes,
  System.Rtti,
  System.TypInfo,
  System.Variants,
  System.Generics.Collections,
  Data.SqlTimSt,
  Data.FmtBcd,
  Data.DB,
  MVCFramework.Commons,
  MVCFramework.Serializer.Intf,
  MVCFramework.Serializer.Abstract,
  MVCFramework.DuckTyping,
  MVCFramework.Serializer.Commons,
  System.SysUtils;

type
  TMVCHTMLSerializer = class(TMVCAbstractSerializer, IMVCSerializer)
  protected
    procedure RaiseNotImplemented;
    function HTMLEntitiesEncode(s: string): string;
  public
    procedure AfterConstruction; override;
    { IMVCSerializer }

    procedure RegisterTypeSerializer(const ATypeInfo: PTypeInfo; AInstance: IMVCTypeSerializer);

    function SerializeObject(
      const AObject: TObject;
      const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = nil;
      const ASerializationAction: TMVCSerializationAction = nil
      ): string; overload;

    function SerializeObject(
      const AObject: IInterface;
      const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = nil;
      const ASerializationAction: TMVCSerializationAction = nil
      ): string; overload;

    function SerializeCollection(
      const AList: TObject;
      const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = nil;
      const ASerializationAction: TMVCSerializationAction = nil
      ): string; overload;

    function SerializeCollection(
      const AList: IInterface;
      const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = nil;
      const ASerializationAction: TMVCSerializationAction = nil
      ): string; overload;

    function SerializeDataSet(
      const ADataSet: TDataSet;
      const AIgnoredFields: TMVCIgnoredList = [];
      const ANameCase: TMVCNameCase = ncAsIs;
      const ASerializationAction: TMVCDatasetSerializationAction = nil
      ): string;

    function SerializeDataSetRecord(
      const ADataSet: TDataSet;
      const AIgnoredFields: TMVCIgnoredList = [];
      const ANameCase: TMVCNameCase = ncAsIs;
      const ASerializationAction: TMVCDatasetSerializationAction = nil
      ): string;

    procedure DeserializeObject(
      const ASerializedObject: string;
      const AObject: TObject;
      const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = nil;
      const ARootNode: string = ''
      ); overload;

    procedure DeserializeObject(
      const ASerializedObject: string;
      const AObject: IInterface;
      const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = nil
      ); overload;

    procedure DeserializeCollection(
      const ASerializedList: string;
      const AList: TObject;
      const AClazz: TClass;
      const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = nil;
      const ARootNode: string = ''
      ); overload;

    procedure DeserializeCollection(
      const ASerializedList: string;
      const AList: IInterface;
      const AClazz: TClass;
      const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = nil
      ); overload;

    procedure DeserializeDataSet(
      const ASerializedDataSet: string;
      const ADataSet: TDataSet;
      const AIgnoredFields: TMVCIgnoredList = [];
      const ANameCase: TMVCNameCase = ncAsIs
      );

    procedure DeserializeDataSetRecord(
      const ASerializedDataSetRecord: string;
      const ADataSet: TDataSet;
      const AIgnoredFields: TMVCIgnoredList = [];
      const ANameCase: TMVCNameCase = ncAsIs
      );
  end;

implementation

uses
  MVCFramework.Logger,
  MVCFramework.DataSet.Utils,
  MVCFramework.Nullables,
  MVCFramework;

const
  HTML_HEADER = '<!DOCTYPE html><html><head><title>DMVCFramework Exception</title>' +
    '<style>' +
  // 'body {font-size: 120%; max-width: 800px; margin: auto; font-family: Calibri,Candara,Segoe,Segoe UI,Optima,Arial,sans-serif; }' +
    'body{' +
    '		font-family: Arial, Helvetica, sans-serif;' +
    '		font-size: 200%;' +
    '	}' +
    '	.info, .success, .warning, .error, .validation {' +
    '		border: 1px solid;' +
    '		margin: 10px 0px;' +
    '		padding: 15px 10px 15px 25px;' +
    '		background-repeat: no-repeat;' +
    '		background-position: 10px center;' +
    '	}' +
    '	.info {' +
    '		color: #00529B;' +
    '		background-color: #BDE5F8;' +
    '		font-size: 50%;' +
    '	}' +
    '	.success {' +
    '		color: #4F8A10;' +
    '		background-color: #DFF2BF;' +
    '	}' +
    '	.warning {' +
    '		color: #9F6000;' +
    '		background-color: #FEEFB3;' +
    '		font-size: 70%;' +
    '	}' +
    '	.error{' +
    '		color: #D8000C;' +
    '		background-color: #FFBABA;' +
    '	}' +
    '.container {max-width: 1000px; margin: auto; margin-top: 3rem;}' +
    '</style>' +
    '</head><body><div class="container">';
  HTML_FOOTER = '</div></body></html>';

  { TMVCHTMLSerializer }

procedure TMVCHTMLSerializer.AfterConstruction;
begin
  inherited AfterConstruction;
end;

procedure TMVCHTMLSerializer.DeserializeCollection(
  const ASerializedList: string; const AList: IInterface; const AClazz: TClass;
  const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList);
begin
  RaiseNotImplemented;
end;

procedure TMVCHTMLSerializer.DeserializeCollection(
  const ASerializedList: string; const AList: TObject; const AClazz: TClass;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList;
  const ARootNode: string);
begin
  RaiseNotImplemented;
end;

procedure TMVCHTMLSerializer.DeserializeDataSet(
  const ASerializedDataSet: string; const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase);
begin
  RaiseNotImplemented;
end;

procedure TMVCHTMLSerializer.DeserializeDataSetRecord(
  const ASerializedDataSetRecord: string; const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase);
begin
  RaiseNotImplemented;
end;

procedure TMVCHTMLSerializer.DeserializeObject(const ASerializedObject: string;
  const AObject: TObject; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList; const ARootNode: string);
begin
  RaiseNotImplemented;
end;

procedure TMVCHTMLSerializer.DeserializeObject(const ASerializedObject: string;
  const AObject: IInterface; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList);
begin
  RaiseNotImplemented;
end;

function TMVCHTMLSerializer.HTMLEntitiesEncode(s: string): string;
  procedure repl(var s: string; r: string; posi: Integer);
  begin
    delete(s, posi, 1);
    insert(r, s, posi);
  end;

var
  I: Integer;
  r: string;
begin
  I := 0;
  while I < Length(s) do
  begin
    r := '';
    case ord(s[I]) of
      160:
        r := 'nbsp';
      161:
        r := 'excl';
      162:
        r := 'cent';
      163:
        r := 'ound';
      164:
        r := 'curren';
      165:
        r := 'yen';
      166:
        r := 'brvbar';
      167:
        r := 'sect';
      168:
        r := 'uml';
      169:
        r := 'copy';
      170:
        r := 'ordf';
      171:
        r := 'laquo';
      172:
        r := 'not';
      173:
        r := 'shy';
      174:
        r := 'reg';
      175:
        r := 'macr';
      176:
        r := 'deg';
      177:
        r := 'plusmn';
      178:
        r := 'sup2';
      179:
        r := 'sup3';
      180:
        r := 'acute';
      181:
        r := 'micro';
      182:
        r := 'para';
      183:
        r := 'middot';
      184:
        r := 'cedil';
      185:
        r := 'sup1';
      186:
        r := 'ordm';
      187:
        r := 'raquo';
      188:
        r := 'frac14';
      189:
        r := 'frac12';
      190:
        r := 'frac34';
      191:
        r := 'iquest';
      192:
        r := 'Agrave';
      193:
        r := 'Aacute';
      194:
        r := 'Acirc';
      195:
        r := 'Atilde';
      196:
        r := 'Auml';
      197:
        r := 'Aring';
      198:
        r := 'AElig';
      199:
        r := 'Ccedil';
      200:
        r := 'Egrave';
      201:
        r := 'Eacute';
      202:
        r := 'Ecirc';
      203:
        r := 'Euml';
      204:
        r := 'Igrave';
      205:
        r := 'Iacute';
      206:
        r := 'Icirc';
      207:
        r := 'Iuml';
      208:
        r := 'ETH';
      209:
        r := 'Ntilde';
      210:
        r := 'Ograve';
      211:
        r := 'Oacute';
      212:
        r := 'Ocirc';
      213:
        r := 'Otilde';
      214:
        r := 'Ouml';
      215:
        r := 'times';
      216:
        r := 'Oslash';
      217:
        r := 'Ugrave';
      218:
        r := 'Uacute';
      219:
        r := 'Ucirc';
      220:
        r := 'Uuml';
      221:
        r := 'Yacute';
      222:
        r := 'THORN';
      223:
        r := 'szlig';
      224:
        r := 'agrave';
      225:
        r := 'aacute';
      226:
        r := 'acirc';
      227:
        r := 'atilde';
      228:
        r := 'auml';
      229:
        r := 'aring';
      230:
        r := 'aelig';
      231:
        r := 'ccedil';
      232:
        r := 'egrave';
      233:
        r := 'eacute';
      234:
        r := 'ecirc';
      235:
        r := 'euml';
      236:
        r := 'igrave';
      237:
        r := 'iacute';
      238:
        r := 'icirc';
      239:
        r := 'iuml';
      240:
        r := 'eth';
      241:
        r := 'ntilde';
      242:
        r := 'ograve';
      243:
        r := 'oacute';
      244:
        r := 'ocirc';
      245:
        r := 'otilde';
      246:
        r := 'ouml';
      247:
        r := 'divide';
      248:
        r := 'oslash';
      249:
        r := 'ugrave';
      250:
        r := 'uacute';
      251:
        r := 'ucirc';
      252:
        r := 'uuml';
      253:
        r := 'yacute';
      254:
        r := 'thorn';
      255:
        r := 'yuml';
    end;
    if r <> '' then
    begin
      repl(s, '&' + r + ';', I);
    end;
    Inc(I)
  end;
  Result := s;
end;

procedure TMVCHTMLSerializer.RaiseNotImplemented;
begin
  raise EMVCException.Create('Not Implemented');
end;

procedure TMVCHTMLSerializer.RegisterTypeSerializer(const ATypeInfo: PTypeInfo;
  AInstance: IMVCTypeSerializer);
begin
  RaiseNotImplemented;
end;

function TMVCHTMLSerializer.SerializeCollection(const AList: TObject;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList;
  const ASerializationAction: TMVCSerializationAction): string;
begin
  RaiseNotImplemented;
end;

function TMVCHTMLSerializer.SerializeCollection(const AList: IInterface;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList;
  const ASerializationAction: TMVCSerializationAction): string;
begin
  RaiseNotImplemented;
end;

function TMVCHTMLSerializer.SerializeDataSet(const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase;
  const ASerializationAction: TMVCDatasetSerializationAction): string;
begin
  RaiseNotImplemented;
end;

function TMVCHTMLSerializer.SerializeDataSetRecord(const ADataSet: TDataSet;
  const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase;
  const ASerializationAction: TMVCDatasetSerializationAction): string;
begin
  RaiseNotImplemented;
end;

function TMVCHTMLSerializer.SerializeObject(const AObject: TObject;
  const AType: TMVCSerializationType; const AIgnoredAttributes: TMVCIgnoredList;
  const ASerializationAction: TMVCSerializationAction): string;
var
  lBody: string;
  lMVCException: EMVCException;
  lException: Exception;
  lErrResponse: TMVCErrorResponse;
  lErrResponseItem: TMVCErrorResponseItem;
  lErr: string;
  function EmitExceptionClass(const Value: string): string;
  begin
    Result := Result + '<div class="warning">Exception Class Name: ' + HTMLEntitiesEncode(Value) + '</div>';
  end;
  function EmitTitle(const HTTPStatusCode: Word; const Value: string): string;
  begin
    Result := '';
    if Assigned(FConfig) then
    begin
      Result := '<h1>' + FConfig[TMVCConfigKey.ServerName] + '</h1>';
    end;
    Result := Result + '<div class="error"><p>HTTP ' + HTTPStatusCode.ToString + '</p>';
    Result := Result + '<p>' + HTMLEntitiesEncode(Value) + '</p></div>';
  end;

begin
  lBody := '';
  if AObject is Exception then
  begin
    if AObject is EMVCException then
    begin
      lMVCException := EMVCException(AObject);
      lBody :=
        EmitTitle(lMVCException.HttpErrorCode, lMVCException.Message) + sLineBreak +
        EmitExceptionClass(lMVCException.ClassName) + sLineBreak +
        '<p>' + HTMLEntitiesEncode(lMVCException.DetailedMessage) + '</p>' + sLineBreak +
        '<div class="info">' +
        '<p> Application Error Code: ' + lMVCException.ApplicationErrorCode.ToString + '</p>' + sLineBreak +
        '<p> Error Items: <ul>' + sLineBreak;
      for lErr in lMVCException.ErrorItems do
      begin
        lBody := lBody + '<li>' + HTMLEntitiesEncode(lErr) + '</li>';
      end;
      lBody := lBody + '<ul></p></div>';
    end
    else
    begin
      lException := Exception(AObject);
      lBody := EmitTitle(500, lException.Message) + sLineBreak +
        EmitExceptionClass(lException.ClassName) + sLineBreak;
    end;
  end;

  if AObject is TMVCErrorResponse then
  begin
    lErrResponse := TMVCErrorResponse(AObject);
    lBody :=
      EmitTitle(lErrResponse.StatusCode, lErrResponse.Message) + sLineBreak +
      EmitExceptionClass(lErrResponse.ClassName) + sLineBreak +
      '<div class="info">' +
      '<p>' + HTMLEntitiesEncode(lErrResponse.DetailedMessage) + '</p>' + sLineBreak +
      '<p>Application Error Code: ' + lErrResponse.AppErrorCode.ToString + '</p>' + sLineBreak +
      '<p>Error Items: <ul>' + sLineBreak;
    for lErrResponseItem in lErrResponse.Items do
    begin
      lBody := lBody + '<li>' + HTMLEntitiesEncode(lErrResponseItem.Message) + '</li>';
    end;
    lBody := lBody + '<ul></p></div>';
  end;

  if lBody.IsEmpty then
  begin
    RaiseNotImplemented
  end
  else
  begin
    Result := HTML_HEADER + lBody + HTML_FOOTER;
  end;
end;

function TMVCHTMLSerializer.SerializeObject(const AObject: IInterface;
  const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList;
  const ASerializationAction: TMVCSerializationAction): string;
begin
  RaiseNotImplemented;
end;

end.
