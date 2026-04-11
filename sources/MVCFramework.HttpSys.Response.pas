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
// *************************************************************************** }

unit MVCFramework.HttpSys.Response;

/// <summary>
/// TMVCWebResponse adapter for the Windows HTTP Server API (HTTP.sys).
/// Buffers response data, then builds and sends an HTTP_RESPONSE via HttpSendHttpResponse.
/// </summary>

{$I dmvcframework.inc}

interface

{$IFDEF MSWINDOWS}

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  Web.HTTPApp,
  Winapi.Windows,
  MVCFramework,
  MVCFramework.HttpSysApi;

type
  TMVCHttpSysResponse = class(TMVCWebResponse)
  private
    FReqQueueHandle: THandle;
    FRequestId: HTTP_REQUEST_ID;
    FStatusCode: Integer;
    FReasonString: string;
    FReasonAnsi: UTF8String;
    FContentType: string;
    FContentEncoding: string;
    FContent: string;
    FContentStream: TStream;
    FOwnsContentStream: Boolean;
    FLocation: string;
    FCustomHeaders: TStringList;
    FCookies: TCookieCollection;
    FHeadersSent: Boolean;
    FDate: TDateTime;
  protected
    function GetCustomHeaders: TStrings; override;
    function GetReasonString: string; override;
    function GetStatusCode: Integer; override;
    function GetCookies: TCookieCollection; override;
    function GetContentType: string; override;
    function GetContentEncoding: string; override;
    function GetLocation: string; override;
    function GetContent: string; override;
    procedure SetReasonString(const AValue: string); override;
    procedure SetStatusCode(const AValue: Integer); override;
    procedure SetContentType(const AValue: string); override;
    procedure SetLocation(const AValue: string); override;
    procedure SetContent(const AValue: string); override;
    procedure SetContentEncoding(const Value: string); override;
    function GetContentStream: TStream; override;
    function GetContentLength: Int64; override;
    procedure SetDate(const AValue: TDateTime); override;
    function GetRawWebResponse: TWebResponse; override;
  public
    constructor Create(AReqQueueHandle: THandle; ARequestId: HTTP_REQUEST_ID);
    destructor Destroy; override;
    procedure Flush; override;
    procedure SetCustomHeader(const AName, AValue: string); override;
    procedure SetContentStream(const AStream: TStream; const AContentType: string); override;
    procedure InternalSetContentStream(const AStream: TStream; const AOwns: Boolean); override;
    function GetCustomHeader(const AName: string): string; override;
    procedure SendRedirect(const AUrl: string); override;
    procedure SendResponse; override;
  end;

{$ENDIF}

implementation

{$IFDEF MSWINDOWS}

uses
  System.DateUtils;

const
  { Max number of unknown headers we support in a single response }
  MAX_UNKNOWN_HEADERS = 64;

{ TMVCHttpSysResponse }

constructor TMVCHttpSysResponse.Create(AReqQueueHandle: THandle; ARequestId: HTTP_REQUEST_ID);
begin
  inherited Create;
  FReqQueueHandle := AReqQueueHandle;
  FRequestId := ARequestId;
  FStatusCode := 200;
  FReasonString := 'OK';
  FContentType := '';
  FContentEncoding := '';
  FContent := '';
  FContentStream := nil;
  FOwnsContentStream := False;
  FLocation := '';
  FCustomHeaders := TStringList.Create;
  FCookies := TCookieCollection.Create(TWebResponse(nil), TCookie);
  FHeadersSent := False;
  FDate := Now;
end;

destructor TMVCHttpSysResponse.Destroy;
begin
  if FOwnsContentStream then
    FContentStream.Free;
  FCookies.Free;
  FCustomHeaders.Free;
  inherited;
end;

function TMVCHttpSysResponse.GetCustomHeaders: TStrings;
begin
  Result := FCustomHeaders;
end;

function TMVCHttpSysResponse.GetReasonString: string;
begin
  Result := FReasonString;
end;

function TMVCHttpSysResponse.GetStatusCode: Integer;
begin
  Result := FStatusCode;
end;

function TMVCHttpSysResponse.GetCookies: TCookieCollection;
begin
  Result := FCookies;
end;

function TMVCHttpSysResponse.GetContentType: string;
begin
  Result := FContentType;
end;

function TMVCHttpSysResponse.GetContentEncoding: string;
begin
  Result := FContentEncoding;
end;

function TMVCHttpSysResponse.GetLocation: string;
begin
  Result := FLocation;
end;

function TMVCHttpSysResponse.GetContent: string;
begin
  Result := FContent;
end;

procedure TMVCHttpSysResponse.SetReasonString(const AValue: string);
begin
  FReasonString := AValue;
end;

procedure TMVCHttpSysResponse.SetStatusCode(const AValue: Integer);
begin
  FStatusCode := AValue;
end;

procedure TMVCHttpSysResponse.SetContentType(const AValue: string);
begin
  FContentType := AValue;
end;

procedure TMVCHttpSysResponse.SetLocation(const AValue: string);
begin
  FLocation := AValue;
end;

procedure TMVCHttpSysResponse.SetContent(const AValue: string);
begin
  FContent := AValue;
end;

procedure TMVCHttpSysResponse.SetContentEncoding(const Value: string);
begin
  FContentEncoding := Value;
end;

function TMVCHttpSysResponse.GetContentStream: TStream;
begin
  Result := FContentStream;
end;

function TMVCHttpSysResponse.GetContentLength: Int64;
begin
  if Assigned(FContentStream) then
    Result := FContentStream.Size
  else if FContent <> '' then
    Result := Length(TEncoding.UTF8.GetBytes(FContent))
  else
    Result := 0;
end;

procedure TMVCHttpSysResponse.SetDate(const AValue: TDateTime);
begin
  FDate := AValue;
end;

function TMVCHttpSysResponse.GetRawWebResponse: TWebResponse;
begin
  Result := nil; { No TWebResponse for HTTP.sys }
end;

procedure TMVCHttpSysResponse.Flush;
var
  lResponse: HTTP_RESPONSE;
  lDataChunk: HTTP_DATA_CHUNK;
  lBytesSent: ULONG;
  lResult: ULONG;
  lBodyBytes: TBytes;
  lBodyStream: TMemoryStream;
  lContentTypeAnsi: UTF8String;
  lContentEncodingAnsi: UTF8String;
  lLocationAnsi: UTF8String;
  lUnknownHeaders: array[0..MAX_UNKNOWN_HEADERS - 1] of HTTP_UNKNOWN_HEADER;
  lUnknownHeaderNames: array[0..MAX_UNKNOWN_HEADERS - 1] of UTF8String;
  lUnknownHeaderValues: array[0..MAX_UNKNOWN_HEADERS - 1] of UTF8String;
  lUnknownCount: Integer;
  I: Integer;
  lCookie: TCookie;
  lCookieStr: string;
  lHeaderName, lHeaderValue: string;
begin
  if FHeadersSent then Exit;
  FHeadersSent := True;

  FillChar(lResponse, SizeOf(lResponse), 0);
  lResponse.Version.MajorVersion := 1;
  lResponse.Version.MinorVersion := 1;
  lResponse.StatusCode := FStatusCode;

  { Reason string (must be AnsiString/PAnsiChar) }
  FReasonAnsi := UTF8String(FReasonString);
  lResponse.ReasonLength := Length(FReasonAnsi);
  lResponse.pReason := PAnsiChar(FReasonAnsi);

  { Known response headers }

  { Content-Type }
  if FContentType <> '' then
  begin
    lContentTypeAnsi := UTF8String(FContentType);
    lResponse.Headers.KnownHeaders[Ord(HttpHeaderResponseContentType)].RawValueLength :=
      Length(lContentTypeAnsi);
    lResponse.Headers.KnownHeaders[Ord(HttpHeaderResponseContentType)].pRawValue :=
      PAnsiChar(lContentTypeAnsi);
  end;

  { Content-Encoding }
  if FContentEncoding <> '' then
  begin
    lContentEncodingAnsi := UTF8String(FContentEncoding);
    lResponse.Headers.KnownHeaders[Ord(HttpHeaderResponseContentEncoding)].RawValueLength :=
      Length(lContentEncodingAnsi);
    lResponse.Headers.KnownHeaders[Ord(HttpHeaderResponseContentEncoding)].pRawValue :=
      PAnsiChar(lContentEncodingAnsi);
  end;

  { Location (for redirects) }
  if FLocation <> '' then
  begin
    lLocationAnsi := UTF8String(FLocation);
    lResponse.Headers.KnownHeaders[Ord(HttpHeaderResponseLocation)].RawValueLength :=
      Length(lLocationAnsi);
    lResponse.Headers.KnownHeaders[Ord(HttpHeaderResponseLocation)].pRawValue :=
      PAnsiChar(lLocationAnsi);
  end;

  { Unknown/custom headers }
  lUnknownCount := 0;

  { Add custom headers }
  for I := 0 to FCustomHeaders.Count - 1 do
  begin
    if lUnknownCount >= MAX_UNKNOWN_HEADERS then
      Break;
    lHeaderName := FCustomHeaders.Names[I];
    lHeaderValue := Trim(FCustomHeaders.ValueFromIndex[I]);
    if lHeaderName = '' then
      Continue;

    lUnknownHeaderNames[lUnknownCount] := UTF8String(lHeaderName);
    lUnknownHeaderValues[lUnknownCount] := UTF8String(lHeaderValue);
    lUnknownHeaders[lUnknownCount].NameLength := Length(lUnknownHeaderNames[lUnknownCount]);
    lUnknownHeaders[lUnknownCount].pName := PAnsiChar(lUnknownHeaderNames[lUnknownCount]);
    lUnknownHeaders[lUnknownCount].RawValueLength := Length(lUnknownHeaderValues[lUnknownCount]);
    lUnknownHeaders[lUnknownCount].pRawValue := PAnsiChar(lUnknownHeaderValues[lUnknownCount]);
    Inc(lUnknownCount);
  end;

  { Add cookies as Set-Cookie headers }
  for I := 0 to FCookies.Count - 1 do
  begin
    if lUnknownCount >= MAX_UNKNOWN_HEADERS then
      Break;
    lCookie := FCookies[I];
    lCookieStr := lCookie.Name + '=' + lCookie.Value;
    if lCookie.Path <> '' then
      lCookieStr := lCookieStr + '; Path=' + lCookie.Path;
    if lCookie.Domain <> '' then
      lCookieStr := lCookieStr + '; Domain=' + lCookie.Domain;
    if lCookie.Expires > 0 then
      lCookieStr := lCookieStr + '; Expires=' + FormatDateTime('ddd, dd mmm yyyy hh:nn:ss "GMT"', TTimeZone.Local.ToUniversalTime(lCookie.Expires));
    if lCookie.Secure then
      lCookieStr := lCookieStr + '; Secure';
    if lCookie.HttpOnly then
      lCookieStr := lCookieStr + '; HttpOnly';

    lUnknownHeaderNames[lUnknownCount] := 'Set-Cookie';
    lUnknownHeaderValues[lUnknownCount] := UTF8String(lCookieStr);
    lUnknownHeaders[lUnknownCount].NameLength := Length(lUnknownHeaderNames[lUnknownCount]);
    lUnknownHeaders[lUnknownCount].pName := PAnsiChar(lUnknownHeaderNames[lUnknownCount]);
    lUnknownHeaders[lUnknownCount].RawValueLength := Length(lUnknownHeaderValues[lUnknownCount]);
    lUnknownHeaders[lUnknownCount].pRawValue := PAnsiChar(lUnknownHeaderValues[lUnknownCount]);
    Inc(lUnknownCount);
  end;

  if lUnknownCount > 0 then
  begin
    lResponse.Headers.UnknownHeaderCount := lUnknownCount;
    lResponse.Headers.pUnknownHeaders := @lUnknownHeaders[0];
  end;

  { Entity body }
  if Assigned(FContentStream) and (FContentStream.Size > 0) then
  begin
    { Stream body }
    lBodyStream := TMemoryStream.Create;
    try
      FContentStream.Position := 0;
      lBodyStream.CopyFrom(FContentStream, FContentStream.Size);

      FillChar(lDataChunk, SizeOf(lDataChunk), 0);
      lDataChunk.DataChunkType := HttpDataChunkFromMemory;
      lDataChunk.FromMemory.pBuffer := lBodyStream.Memory;
      lDataChunk.FromMemory.BufferLength := lBodyStream.Size;

      lResponse.EntityChunkCount := 1;
      lResponse.pEntityChunks := @lDataChunk;

      lResult := HttpSendHttpResponse(FReqQueueHandle, FRequestId, 0,
        @lResponse, nil, lBytesSent, nil, 0, nil, nil);
      if lResult <> NO_ERROR then
        raise Exception.CreateFmt('HttpSendHttpResponse failed with error %d', [lResult]);
    finally
      lBodyStream.Free;
    end;
  end
  else if FContent <> '' then
  begin
    { String body }
    lBodyBytes := TEncoding.UTF8.GetBytes(FContent);

    FillChar(lDataChunk, SizeOf(lDataChunk), 0);
    lDataChunk.DataChunkType := HttpDataChunkFromMemory;
    lDataChunk.FromMemory.pBuffer := @lBodyBytes[0];
    lDataChunk.FromMemory.BufferLength := Length(lBodyBytes);

    lResponse.EntityChunkCount := 1;
    lResponse.pEntityChunks := @lDataChunk;

    lResult := HttpSendHttpResponse(FReqQueueHandle, FRequestId, 0,
      @lResponse, nil, lBytesSent, nil, 0, nil, nil);
    if lResult <> NO_ERROR then
      raise Exception.CreateFmt('HttpSendHttpResponse failed with error %d', [lResult]);
  end
  else
  begin
    { No body }
    lResponse.EntityChunkCount := 0;
    lResponse.pEntityChunks := nil;

    lResult := HttpSendHttpResponse(FReqQueueHandle, FRequestId, 0,
      @lResponse, nil, lBytesSent, nil, 0, nil, nil);
    if lResult <> NO_ERROR then
      raise Exception.CreateFmt('HttpSendHttpResponse failed with error %d', [lResult]);
  end;
end;

procedure TMVCHttpSysResponse.SetCustomHeader(const AName, AValue: string);
begin
  FCustomHeaders.Values[AName] := AValue;
end;

procedure TMVCHttpSysResponse.SetContentStream(const AStream: TStream;
  const AContentType: string);
begin
  if FOwnsContentStream and Assigned(FContentStream) then
    FContentStream.Free;
  FContentStream := AStream;
  FOwnsContentStream := True;
  FContentType := AContentType;
end;

procedure TMVCHttpSysResponse.InternalSetContentStream(const AStream: TStream;
  const AOwns: Boolean);
begin
  if FOwnsContentStream and Assigned(FContentStream) and (FContentStream <> AStream) then
    FContentStream.Free;
  FContentStream := AStream;
  FOwnsContentStream := AOwns;
end;

function TMVCHttpSysResponse.GetCustomHeader(const AName: string): string;
begin
  Result := FCustomHeaders.Values[AName];
end;

procedure TMVCHttpSysResponse.SendRedirect(const AUrl: string);
begin
  FStatusCode := 302;
  FReasonString := 'Found';
  FLocation := AUrl;
  Flush;
end;

procedure TMVCHttpSysResponse.SendResponse;
begin
  Flush;
end;

{$ENDIF}

end.
