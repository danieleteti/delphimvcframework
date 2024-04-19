// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2024 Daniele Teti and the DMVCFramework Team
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
  MVCFramework.Nullables, System.StrUtils;

const
  HTML_HEADER = '<!doctype html>' +
  '<html>' +
  '<head>' +
  '<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />' +
  '<meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=1">' +
  '<title>DMVCFramework</title>' +
  '<style>'+
  'body {' +
  '  background: #fff;'+
  '  padding: 2rem;' +
  '  margin: 0 auto;' +
  '  max-width: 50rem;' +
  '  font-family: "Segoe UI Light", Tahoma, Arial, ui-sans-serif, sans-serif;' +
  '}' +
  '.color1 { color: #C5C5C5; } /* https://color.adobe.com/it/search?q=Primary%20colors&t=term */' +
  '.color2 { color: #827C78; }' +
  '.color3 { color: #005195; }' +
  '.color4 { color: #003A69; }' +
  '.color5 { color: #000000; }' +
  '.container {' +
  '  background: #f4f4f4;'+
  '  border: 2px solid #827C78;' +
  '  margin-top: 2rem;' +
  '  margin: auto;' +
  '  width: fit-content;' +
  '  min-width: 30rem;' +
  '  padding: 1rem;' +
  '  padding-left: 2rem;' +
  '  padding-right: 2rem;' +
  '}' +
  '}' +
  '</style>'+
  '</head>' +
  '<body>' +
  '<div class="container">';

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
  function EmitExceptionClass(const ClazzName, Message: string): string;
  begin
      Result := Result + '<h2>' +
        IfThen(not ClazzName.IsEmpty, HTMLEntitiesEncode(ClazzName) + ': ') + Message + '</h2>';
  end;
  function EmitTitle(const HTTPStatusCode: Word): string;
  begin
    Result := '<h1><span class="color1">' + HTTPStatusCode.ToString + '</span><span class="color2"> ' + HTTP_STATUS.ReasonStringFor(HTTPStatusCode) + '</span></h1>';
    if Assigned(FConfig) then
    begin
      Result := Result + '<h3 class="color4">' + FConfig[TMVCConfigKey.ServerName] + '</h3>';
    end;
  end;

  function GetHTMLBody(
    const HTTPStatusCode: Integer;
    const Message: String;
    const DetailedMessage: String;
    const ClazzName: String;
    const AppErrorCode: Integer;
    const ErrorItems: TArray<String>): String;
  var
    lErr: String;
  begin
    Result :=
        EmitTitle(HTTPStatusCode) + sLineBreak +
        EmitExceptionClass(ClazzName, Message) + sLineBreak +
        '<h3 class="color5">' + HTMLEntitiesEncode(DetailedMessage) + '</h3>' + sLineBreak;
      Result := Result + '<div>';
      if AppErrorCode <> 0 then
      begin
        Result := Result + '<p>Application Error Code: ' + AppErrorCode.ToString + '</p>';
      end;
      if Assigned(ErrorItems) and (Length(ErrorItems) > 0) then
      begin
        Result := Result + '<p>Error Items: <ul>' + sLineBreak;
        for lErr in ErrorItems do
        begin
          Result := Result + '<li>' + HTMLEntitiesEncode(lErr) + '</li>';
        end;
        Result := Result + '</ul></p>';
      end;
      Result := Result + '</div>';
  end;
var
  lBody: string;
  lMVCException: EMVCException;
  lException: Exception;
  lErrResponse: TMVCErrorResponse;
begin
  lBody := '';
  if AObject is Exception then
  begin
    if AObject is EMVCException then
    begin
      lMVCException := EMVCException(AObject);
      lBody := GetHTMLBody(
        lMVCException.HTTPStatusCode,
        lMVCException.Message,
        lMVCException.DetailedMessage,
        lMVCException.ClassName,
        lMVCException.ApplicationErrorCode,
        lMVCException.ErrorItems);
    end
    else
    begin
      lException := Exception(AObject);
      lBody := EmitTitle(500) + sLineBreak +
        EmitExceptionClass(lException.ClassName, lException.Message) + sLineBreak;
    end;
  end;

  if AObject is TMVCErrorResponse then
  begin
    lErrResponse := TMVCErrorResponse(AObject);
    lBody := GetHTMLBody(
      lErrResponse.StatusCode,
      lErrResponse.Message,
      lErrResponse.DetailedMessage,
      lErrResponse.ClassName,
      lErrResponse.AppErrorCode,
      nil);
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
