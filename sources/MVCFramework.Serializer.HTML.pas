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
  '<meta charset="UTF-8">' +
  '<meta name="viewport" content="width=device-width, initial-scale=1">' +
  '<title>DMVCFramework Error</title>' +
  '<style>' +
  '* { margin: 0; padding: 0; box-sizing: border-box; }' +
  'body {' +
  '  font-family: ''Consolas'', ''Monaco'', ''Courier New'', monospace;' +
  '  background: #f5f5f5;' +
  '  color: #2c3e50;' +
  '  min-height: 100vh;' +
  '  display: flex;' +
  '  align-items: center;' +
  '  justify-content: center;' +
  '  padding: 1.5rem;' +
  '  font-size: 14px;' +
  '  line-height: 1.5;' +
  '}' +
  '@media (prefers-color-scheme: dark) {' +
  '  body { background: #1e1e1e; color: #d4d4d4; }' +
  '}' +
  '.container {' +
  '  background: #ffffff;' +
  '  border: 1px solid #d1d5db;' +
  '  border-radius: 8px;' +
  '  max-width: 50rem;' +
  '  width: 100%;' +
  '  box-shadow: 0 4px 6px rgba(0,0,0,0.1);' +
  '}' +
  '@media (prefers-color-scheme: dark) {' +
  '  .container {' +
  '    background: #252526;' +
  '    border: 1px solid #3e3e42;' +
  '    box-shadow: 0 8px 16px rgba(0,0,0,0.4);' +
  '  }' +
  '}' +
  '.header {' +
  '  background: linear-gradient(135deg, #005195 0%, #003A69 100%);' +
  '  color: white;' +
  '  padding: 1rem 1.5rem;' +
  '  display: flex;' +
  '  align-items: center;' +
  '  gap: 1rem;' +
  '  border-bottom: 2px solid #005195;' +
  '}' +
  '@media (prefers-color-scheme: dark) {' +
  '  .header {' +
  '    background: #2d2d30;' +
  '    border-bottom: 2px solid #007acc;' +
  '  }' +
  '}' +
  '.status-code {' +
  '  font-size: 2.5rem;' +
  '  font-weight: bold;' +
  '  color: #ffffff;' +
  '  font-family: ''Consolas'', monospace;' +
  '  text-shadow: 0 2px 4px rgba(0,0,0,0.2);' +
  '}' +
  '@media (prefers-color-scheme: dark) {' +
  '  .status-code { color: #f48771; }' +
  '}' +
  '.status-info { flex: 1; }' +
  '.status-text {' +
  '  font-size: 1.25rem;' +
  '  color: #e0f2fe;' +
  '  margin-bottom: 0.25rem;' +
  '}' +
  '@media (prefers-color-scheme: dark) {' +
  '  .status-text { color: #ce9178; }' +
  '}' +
  '.server-name {' +
  '  font-size: 0.875rem;' +
  '  color: #bae6fd;' +
  '}' +
  '@media (prefers-color-scheme: dark) {' +
  '  .server-name { color: #858585; }' +
  '}' +
  '.body { padding: 1.5rem; }' +
  '.error-line {' +
  '  display: flex;' +
  '  gap: 1rem;' +
  '  margin-bottom: 1rem;' +
  '  padding: 0.75rem;' +
  '  background: #eff6ff;' +
  '  border-left: 3px solid #005195;' +
  '  border-radius: 4px;' +
  '}' +
  '@media (prefers-color-scheme: dark) {' +
  '  .error-line {' +
  '    background: #1e1e1e;' +
  '    border-left: 3px solid #f48771;' +
  '  }' +
  '}' +
  '.error-label {' +
  '  color: #003A69;' +
  '  font-weight: bold;' +
  '  min-width: 80px;' +
  '}' +
  '@media (prefers-color-scheme: dark) {' +
  '  .error-label { color: #569cd6; }' +
  '}' +
  '.error-value {' +
  '  color: #0369a1;' +
  '  flex: 1;' +
  '}' +
  '@media (prefers-color-scheme: dark) {' +
  '  .error-value { color: #ce9178; }' +
  '}' +
  '.details-section {' +
  '  background: #f8fafc;' +
  '  border: 1px solid #e0e7ff;' +
  '  border-radius: 4px;' +
  '  padding: 1rem;' +
  '  margin-top: 1rem;' +
  '}' +
  '@media (prefers-color-scheme: dark) {' +
  '  .details-section {' +
  '    background: #1e1e1e;' +
  '    border: 1px solid #3e3e42;' +
  '  }' +
  '}' +
  '.details-title {' +
  '  color: #005195;' +
  '  font-weight: bold;' +
  '  margin-bottom: 0.5rem;' +
  '  font-size: 0.875rem;' +
  '}' +
  '@media (prefers-color-scheme: dark) {' +
  '  .details-title { color: #4ec9b0; }' +
  '}' +
  '.details-content {' +
  '  color: #475569;' +
  '  font-size: 0.875rem;' +
  '}' +
  '@media (prefers-color-scheme: dark) {' +
  '  .details-content { color: #9cdcfe; }' +
  '}' +
  '.error-items { margin-top: 0.75rem; }' +
  '.error-items li {' +
  '  padding: 0.25rem 0;' +
  '  padding-left: 1.25rem;' +
  '  position: relative;' +
  '  color: #0c4a6e;' +
  '}' +
  '@media (prefers-color-scheme: dark) {' +
  '  .error-items li { color: #d7ba7d; }' +
  '}' +
  '.error-items li:before {' +
  '  content: "\25B8";' +
  '  position: absolute;' +
  '  left: 0;' +
  '  color: #005195;' +
  '}' +
  '@media (prefers-color-scheme: dark) {' +
  '  .error-items li:before { color: #007acc; }' +
  '}' +
  '.footer {' +
  '  padding: 0.75rem 1.5rem;' +
  '  background: #f8fafc;' +
  '  border-top: 1px solid #e5e7eb;' +
  '  font-size: 0.75rem;' +
  '  color: #64748b;' +
  '  text-align: center;' +
  '}' +
  '@media (prefers-color-scheme: dark) {' +
  '  .footer {' +
  '    background: #2d2d30;' +
  '    border-top: 1px solid #3e3e42;' +
  '    color: #858585;' +
  '  }' +
  '}' +
  '.badge {' +
  '  display: inline-block;' +
  '  background: #dbeafe;' +
  '  color: #1e3a8a;' +
  '  padding: 0.125rem 0.5rem;' +
  '  border-radius: 3px;' +
  '  font-size: 0.75rem;' +
  '  margin-left: 0.5rem;' +
  '  border: 1px solid #93c5fd;' +
  '}' +
  '@media (prefers-color-scheme: dark) {' +
  '  .badge {' +
  '    background: #5a1d1d;' +
  '    color: #f48771;' +
  '    border: none;' +
  '  }' +
  '}' +
  '</style>' +
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
  raise EMVCException.Create('Not Implemented for ' + ClassName);
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
  function GetHTMLBody(
    const HTTPStatusCode: Integer;
    const Message: String;
    const DetailedMessage: String;
    const ClazzName: String;
    const AppErrorCode: Integer;
    const ErrorItems: TArray<String>): String;
  var
    lErr: String;
    lServerName: String;
  begin
    // Server name
    if Assigned(FConfig) then
      lServerName := FConfig[TMVCConfigKey.ServerName]
    else
      lServerName := 'DMVCFramework ' + DMVCFRAMEWORK_VERSION;

    // Header with status code
    Result := '<div class="header">' +
      '<div class="status-code">' + HTTPStatusCode.ToString + '</div>' +
      '<div class="status-info">' +
      '<div class="status-text">' + HTTP_STATUS.ReasonStringFor(HTTPStatusCode) + '</div>' +
      '<div class="server-name">' + HTMLEntitiesEncode(lServerName) + '</div>' +
      '</div>' +
      '</div>';

    // Body
    Result := Result + '<div class="body">';

    // Exception class
    if not ClazzName.IsEmpty then
    begin
      Result := Result + '<div class="error-line">' +
        '<div class="error-label">Exception:</div>' +
        '<div class="error-value">' + HTMLEntitiesEncode(ClazzName) + '</div>' +
        '</div>';
    end;

    // Message
    if not Message.IsEmpty then
    begin
      Result := Result + '<div class="error-line">' +
        '<div class="error-label">Message:</div>' +
        '<div class="error-value">' + HTMLEntitiesEncode(Message) + '</div>' +
        '</div>';
    end;

    // Detailed message
    if not DetailedMessage.IsEmpty then
    begin
      Result := Result + '<div class="details-section">' +
        '<div class="details-title">DETAILED MESSAGE</div>' +
        '<div class="details-content">' + HTMLEntitiesEncode(DetailedMessage) + '</div>' +
        '</div>';
    end;

    // Error code and items
    if (AppErrorCode <> 0) or (Assigned(ErrorItems) and (Length(ErrorItems) > 0)) then
    begin
      Result := Result + '<div class="details-section">' +
        '<div class="details-title">ADDITIONAL INFO';
      if AppErrorCode <> 0 then
        Result := Result + ' <span class="badge">Error Code: ' + AppErrorCode.ToString + '</span>';
      Result := Result + '</div>';

      if Assigned(ErrorItems) and (Length(ErrorItems) > 0) then
      begin
        Result := Result + '<ul class="error-items" style="list-style:none;padding:0;margin:0;">';
        for lErr in ErrorItems do
        begin
          Result := Result + '<li>' + HTMLEntitiesEncode(lErr) + '</li>';
        end;
        Result := Result + '</ul>';
      end;
      Result := Result + '</div>';
    end;

    Result := Result + '</div>'; // close body

    // Footer
    Result := Result + '<div class="footer">Powered by DelphiMVCFramework</div>';
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
      lBody := GetHTMLBody(
        500,
        lException.Message,
        '',
        lException.ClassName,
        0,
        nil);
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
