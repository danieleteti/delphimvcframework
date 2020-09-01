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
// *************************************************************************** }

unit MVCFramework.Middleware.Trace;

{$I dmvcframework.inc}

interface

uses
  MVCFramework,
  MVCFramework.Logger,
  MVCFramework.Commons;

type
  TMVCTraceMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    fMaxBodySize: Int64;
  protected
    procedure OnAfterControllerAction(Context: TWebContext; const AActionNAme: string;
      const Handled: Boolean);
    procedure OnBeforeRouting(Context: TWebContext; var Handled: Boolean);
    procedure OnBeforeControllerAction(Context: TWebContext;
      const AControllerQualifiedClassName: string; const AActionNAme: string; var Handled: Boolean);
    procedure OnAfterRouting(Context: TWebContext; const AHandled: Boolean);
  public
    constructor Create(const MaxBodySizeInTrace: UInt64 = 1024);
  end;

implementation

uses
  System.SysUtils,
  System.ZLib,
  System.Classes,
  MVCFramework.Rtti.Utils,
  Web.HTTPApp, System.Math;

constructor TMVCTraceMiddleware.Create(const MaxBodySizeInTrace: UInt64 = 1024);
begin
  inherited Create;
  fMaxBodySize := MaxBodySizeInTrace;
end;

procedure TMVCTraceMiddleware.OnAfterControllerAction(Context: TWebContext;
  const AActionNAme: string; const Handled: Boolean);
var
  lContentStream: TStringStream;
begin
  Log.Debug('[AFTER ACTION][RESPONSE][STATUS] ' +
    Format('%d: %s', [Context.Response.StatusCode, Context.Response.ReasonString]),
    'trace');
  Log.Debug('[AFTER ACTION][RESPONSE][CUSTOM HEADERS] ' + string.Join(' | ',
    Context.Response.CustomHeaders.ToStringArray), 'trace');
  Log.Debug('[AFTER ACTION][RESPONSE][CONTENT-TYPE] ' + Context.Response.ContentType, 'trace');

  lContentStream := TStringStream.Create;
  try
    if Assigned(Context.Response.RawWebResponse.ContentStream) then
    begin
      lContentStream.CopyFrom(Context.Response.RawWebResponse.ContentStream,
        Min(Context.Response.RawWebResponse.ContentStream.Size, fMaxBodySize));
      Context.Response.RawWebResponse.ContentStream.Position := 0;
    end
    else
    begin
      lContentStream.WriteString(Context.Response.RawWebResponse.Content.Substring(0, fMaxBodySize));
    end;
    Log.Debug('[AFTER ACTION][RESPONSE][BODY] ' + lContentStream.DataString, 'trace');
  finally
    lContentStream.Free;
  end;
end;

procedure TMVCTraceMiddleware.OnAfterRouting(Context: TWebContext; const AHandled: Boolean);
begin
  Log.Debug('[AFTER ROUTING][REQUESTED URL: %s][HANDLED: %s]',
    [Context.Request.PathInfo, AHandled.ToString(TUseBoolStrs.True)], 'trace');
end;

procedure TMVCTraceMiddleware.OnBeforeControllerAction(Context: TWebContext;
  const AControllerQualifiedClassName, AActionNAme: string; var Handled: Boolean);
begin
  Log.Debug('[BEFORE ACTION][CONTROLLER: %s][ACTION: %s]',
    [AControllerQualifiedClassName, AActionNAme], 'trace');
end;

procedure TMVCTraceMiddleware.OnBeforeRouting(Context: TWebContext; var Handled: Boolean);
var
  lContentStream: TStringStream;
  lContentType: string;
begin
  lContentStream := TStringStream.Create;
  try
    Context.Request.RawWebRequest.ReadTotalContent;
    Log.Debug('[BEFORE ROUTING][REQUEST][URL] ' + Context.Request.RawWebRequest.PathInfo, 'trace');
    Log.Debug('[BEFORE ROUTING][REQUEST][QUERYSTRING] ' + Context.Request.RawWebRequest.QueryFields.
      DelimitedText, 'trace');

    lContentType := Context.Request.Headers['content-type'].ToLower;
    if lContentType.StartsWith(TMVCMediaType.APPLICATION_JSON, true) or
      lContentType.StartsWith(TMVCMediaType.APPLICATION_XML, true) or
      lContentType.StartsWith(TMVCMediaType.APPLICATION_FORM_URLENCODED, true) or
      lContentType.StartsWith('text/') then
    begin
      lContentStream.WriteString(EncodingGetString(lContentType,
        Context.Request.RawWebRequest.RawContent).Substring(0, fMaxBodySize));
    end
    else
    begin
      lContentStream.WriteString('<hidden non text content>');
    end;
    Log.Debug('[BEFORE ROUTING][REQUEST][BODY] ' + lContentStream.DataString, 'trace');
  finally
    lContentStream.Free;
  end;
end;

end.
