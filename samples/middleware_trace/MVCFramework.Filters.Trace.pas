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
// *************************************************************************** }

unit MVCFramework.Filters.Trace;

interface

uses MVCFramework;

{$I dmvcframework.inc}

type
  TMVCTraceFilter = class(TProtocolFilter)
  private
    fMaxBodySize: Int64;
  protected
    procedure DoFilter(Context: TWebContext); override;
  public
    constructor Create(const MaxBodySizeInTrace: UInt64 = 1024);
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  MVCFramework.Commons,
  MVCFramework.Logger,
  Web.HTTPApp;

{ TMVCTraceFilter }

constructor TMVCTraceFilter.Create(const MaxBodySizeInTrace: UInt64 = 1024);
begin
  inherited Create;
  fMaxBodySize := MaxBodySizeInTrace;
end;

procedure TMVCTraceFilter.DoFilter(Context: TWebContext);
var
  lContentStream: TStringStream;
  lContentType: string;
  lReq: TMVCWebRequest;
begin
  lContentStream := TStringStream.Create;
  try
    Context.Request.RawWebRequest.ReadTotalContent;
    lReq := Context.Request;
    Log.Debug('[BEFORE ROUTING][%s][IP: %s][URL: %s][QUERYSTRING: %s][LENGTH: %d]', [
      lReq.HTTPMethodAsString,
      lReq.ClientIp,
      lReq.RawWebRequest.PathInfo,
      lReq.RawWebRequest.QueryFields.DelimitedText,
      lReq.RawWebRequest.ContentLength
      ],'trace');
    if Context.Request.HTTPMethod in [httpPOST, httpPUT] then
    begin
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
    end;
  finally
    lContentStream.Free;
  end;
  DoNext(Context);
end;

end.
