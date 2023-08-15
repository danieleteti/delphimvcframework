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

unit MVCFramework.Serializer.HTML;

{$I dmvcframework.inc}
{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
	System.SysUtils,
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
  MVCFramework.Serializer.Commons;

type
  TMVCHTMLSerializer = class(TMVCAbstractSerializer, IMVCSerializer)
  protected
    procedure RaiseNotImplemented;
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

    function SerializeRecord(
      const ARecord: Pointer;
      const ARecordTypeInfo: PTypeInfo;
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

    function SerializeArrayOfRecord(
      var ATValueContainingAnArray: TValue;
      const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = nil;
      const ASerializationAction: TMVCSerializationAction = nil): string;
  end;

implementation

uses
  System.NetEncoding,
  MVCFramework,
  MVCFramework.Logger,
  MVCFramework.DataSet.Utils,
  MVCFramework.Nullables;

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


function HTMLEntitiesEncode(const Text: string): String;
begin
  Result := TNetEncoding.HTML.Encode(Text);
end;
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

function TMVCHTMLSerializer.SerializeArrayOfRecord(
  var ATValueContainingAnArray: TValue; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList;
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
    Result := Result + '<div class="error"><p>' +
      HTTPStatusCode.ToString + ' ' +
      HTTP_STATUS.ReasonStringFor(HTTPStatusCode) + '</p>';
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
        '<p> Application Error Code: ' + lMVCException.ApplicationErrorCode.ToString + '</p>' + sLineBreak;
      if Length(lMVCException.ErrorItems) > 0 then
      begin
        lBody := lBody + '<p> Error Items: <ul>' + sLineBreak;
        for lErr in lMVCException.ErrorItems do
        begin
          lBody := lBody + '<li>' + HTMLEntitiesEncode(lErr) + '</li>';
        end;
        lBody := lBody + '<ul></p>';
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
      '<p>Application Error Code: ' + lErrResponse.AppErrorCode.ToString + '</p>' + sLineBreak;
    if lErrResponse.Items.Count > 0 then
    begin
      lBody := lBody + '<p>Error Items: <ul>' + sLineBreak;
      for lErrResponseItem in lErrResponse.Items do
      begin
        lBody := lBody + '<li>' + HTMLEntitiesEncode(lErrResponseItem.Message) + '</li>';
      end;
      lBody := lBody + '<ul></p>';
    end;
    lBody := lBody + '</div>';
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

function TMVCHTMLSerializer.SerializeRecord(const ARecord: Pointer;
  const ARecordTypeInfo: PTypeInfo; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList;
  const ASerializationAction: TMVCSerializationAction): string;
begin
  raise Exception.Create('Not implemented');
end;

end.
