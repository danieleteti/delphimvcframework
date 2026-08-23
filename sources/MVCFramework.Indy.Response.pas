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

unit MVCFramework.Indy.Response;

{$I dmvcframework.inc}

interface

uses
  System.Classes, System.SysUtils,
  Web.HTTPApp,
  IdCustomHTTPServer, IdContext, IdCookie, IdIOHandler, IdGlobal,
  MVCFramework;

type
  TMVCIndyDirectResponse = class(TMVCWebResponse)
  private
    FResponseInfo: TIdHTTPResponseInfo;
    FContext: TIdContext;
    FCookies: TCookieCollection;
    FCustomHeaders: TStringList;
    FHeadersSent: Boolean;
    FSingleFlushResponse: Boolean;
    FChunkedStreaming: Boolean;
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
    constructor Create(const AContext: TIdContext;
      const AResponseInfo: TIdHTTPResponseInfo;
      const ASingleFlushResponse: Boolean = False);
    destructor Destroy; override;
    procedure Flush; override;
    procedure SetCustomHeader(const AName, AValue: string); override;
    procedure SetContentStream(const AStream: TStream; const AContentType: string); override;
    procedure InternalSetContentStream(const AStream: TStream; const AOwns: Boolean); override;
    function GetCustomHeader(const AName: string): string; override;
    procedure SendRedirect(const AUrl: string); override;
    procedure SendResponse; override;
    function CreateChunkedWriter: IMVCChunkedResponseWriter; override;
  end;

implementation

uses
  System.DateUtils, MVCFramework.Commons;

{ TMVCIndyDirectResponse }

constructor TMVCIndyDirectResponse.Create(const AContext: TIdContext;
  const AResponseInfo: TIdHTTPResponseInfo;
  const ASingleFlushResponse: Boolean);
begin
  inherited Create;
  FResponseInfo := AResponseInfo;
  FContext := AContext;
  FCookies := TCookieCollection.Create(TWebResponse(nil), TCookie);
  FCustomHeaders := TStringList.Create;
  FHeadersSent := False;
  FChunkedStreaming := False;
  FSingleFlushResponse := ASingleFlushResponse;
end;

destructor TMVCIndyDirectResponse.Destroy;
begin
  FCookies.Free;
  FCustomHeaders.Free;
  inherited;
end;

function TMVCIndyDirectResponse.GetCustomHeaders: TStrings;
begin
  Result := FCustomHeaders;
end;

function TMVCIndyDirectResponse.GetReasonString: string;
begin
  Result := FResponseInfo.ResponseText;
end;

function TMVCIndyDirectResponse.GetStatusCode: Integer;
begin
  Result := FResponseInfo.ResponseNo;
end;

function TMVCIndyDirectResponse.GetCookies: TCookieCollection;
begin
  Result := FCookies;
end;

function TMVCIndyDirectResponse.GetContentType: string;
begin
  Result := FResponseInfo.ContentType;
end;

function TMVCIndyDirectResponse.GetContentEncoding: string;
begin
  Result := FResponseInfo.ContentEncoding;
end;

function TMVCIndyDirectResponse.GetLocation: string;
begin
  Result := FCustomHeaders.Values['location'];
end;

function TMVCIndyDirectResponse.GetContent: string;
begin
  Result := FResponseInfo.ContentText;
end;

procedure TMVCIndyDirectResponse.SetReasonString(const AValue: string);
begin
  FResponseInfo.ResponseText := AValue;
end;

procedure TMVCIndyDirectResponse.SetStatusCode(const AValue: Integer);
begin
  FResponseInfo.ResponseNo := AValue;
end;

procedure TMVCIndyDirectResponse.SetContentType(const AValue: string);
begin
  FResponseInfo.ContentType := AValue;
end;

procedure TMVCIndyDirectResponse.SetLocation(const AValue: string);
begin
  FCustomHeaders.Values['location'] := AValue;
end;

procedure TMVCIndyDirectResponse.SetContent(const AValue: string);
begin
  FResponseInfo.ContentText := AValue;
end;

procedure TMVCIndyDirectResponse.SetContentEncoding(const Value: string);
begin
  FResponseInfo.ContentEncoding := Value;
end;

function TMVCIndyDirectResponse.GetContentStream: TStream;
begin
  Result := FResponseInfo.ContentStream;
end;

function TMVCIndyDirectResponse.GetContentLength: Int64;
begin
  Result := FResponseInfo.ContentLength;
end;

procedure TMVCIndyDirectResponse.SetDate(const AValue: TDateTime);
begin
  FResponseInfo.Date := AValue;
end;

function TMVCIndyDirectResponse.GetRawWebResponse: TWebResponse;
begin
  Result := nil; // No TWebResponse for direct Indy
end;

procedure TMVCIndyDirectResponse.Flush;
var
  I: Integer;
  lCookie: TCookie;
  lIO: TIdIOHandler;
begin
  if FHeadersSent then Exit;
  // A streaming writer (SSE, JSONL, JSONArray) has already emitted the
  // full HTTP response (status + headers + body) directly on the socket.
  // Nothing left for the framework to send; writing anything here would
  // corrupt the already-committed response. Also ask Indy to close the
  // TCP connection at the end of the request so the client sees EOF
  // and stops reading (streaming writers can't announce a Content-Length
  // upfront and don't use chunked encoding).
  if StreamingHandled then
  begin
    FHeadersSent := True;
    // close-based writers (SSE/JSONL/JSONArray) need EOF via close; the
    // chunked writer terminates with 0-chunk and keeps the connection alive.
    FResponseInfo.CloseConnection := not FChunkedStreaming;
    Exit;
  end;
  FHeadersSent := True;

  // Sync custom headers to Indy response
  for I := 0 to FCustomHeaders.Count - 1 do
  begin
    FResponseInfo.CustomHeaders.Values[FCustomHeaders.Names[I]] :=
      Trim(FCustomHeaders.ValueFromIndex[I]);
  end;

  // Sync cookies to Indy response via Set-Cookie headers
  for I := 0 to FCookies.Count - 1 do
  begin
    lCookie := FCookies[I];
    with FResponseInfo.Cookies.Add do
    begin
      CookieName := lCookie.Name;
      Value := lCookie.Value;
      Path := lCookie.Path;
      Domain := lCookie.Domain;
      Expires := lCookie.Expires;
      Secure := lCookie.Secure;
      HttpOnly := lCookie.HttpOnly;
      {$IF CompilerVersion >= 35.0}  // TCookie.SameSite exists since 10.4.2, but CompilerVersion is 34.0 for the whole 10.4 line: gate at 11
      SameSite := lCookie.SameSite;
      {$ENDIF}
    end;
  end;

  if FSingleFlushResponse then
  begin
    // Opt-in: buffer headers+body and send in one IOHandler write.
    // Required for embedded/non-conforming HTTP clients that do not
    // reassemble a response split across separate TCP segments.
    // Body is materialized in memory: not suitable for large streams.
    lIO := FContext.Connection.IOHandler;
    lIO.WriteBufferOpen;
    try
      FResponseInfo.WriteHeader;
      FResponseInfo.WriteContent;
    finally
      lIO.WriteBufferFlush;
      lIO.WriteBufferClose;
    end;
  end;
  // Default path: Indy emits the response automatically after the
  // handler returns (WriteHeader + WriteContent on its own schedule).
end;

procedure TMVCIndyDirectResponse.SetCustomHeader(const AName, AValue: string);
begin
  FCustomHeaders.Values[MVCStripCRLF(AName)] := MVCStripCRLF(AValue);
end;

procedure TMVCIndyDirectResponse.SetContentStream(const AStream: TStream;
  const AContentType: string);
begin
  FResponseInfo.ContentStream := AStream;
  FResponseInfo.FreeContentStream := True;
  ContentType := AContentType;
end;

procedure TMVCIndyDirectResponse.InternalSetContentStream(const AStream: TStream;
  const AOwns: Boolean);
begin
  FResponseInfo.ContentStream := AStream;
  FResponseInfo.FreeContentStream := AOwns;
end;

function TMVCIndyDirectResponse.GetCustomHeader(const AName: string): string;
begin
  Result := FCustomHeaders.Values[AName];
end;

type
  TMVCIndyChunkedWriter = class(TInterfacedObject, IMVCChunkedResponseWriter)
  private
    FResponseInfo: TIdHTTPResponseInfo;
    FIO: TIdIOHandler;
    FEnc: IIdTextEncoding;
    FConnected: Boolean;
    procedure RawWrite(const AText: string);
  public
    constructor Create(const AResponseInfo: TIdHTTPResponseInfo; const AIO: TIdIOHandler);
    procedure SendHeaders(const AContentType, ACharset: string);
    procedure WriteChunk(const ABytes: TBytes);
    procedure Finish;
    function Connected: Boolean;
  end;

constructor TMVCIndyChunkedWriter.Create(const AResponseInfo: TIdHTTPResponseInfo;
  const AIO: TIdIOHandler);
begin
  inherited Create;
  FResponseInfo := AResponseInfo;
  FIO := AIO;
  FEnc := IndyTextEncoding_UTF8;
  FConnected := True;
end;

procedure TMVCIndyChunkedWriter.RawWrite(const AText: string);
begin
  if not FConnected then
    Exit;
  try
    FIO.Write(AText, FEnc);
  except
    FConnected := False;
  end;
end;

procedure TMVCIndyChunkedWriter.SendHeaders(const AContentType, ACharset: string);
begin
  // Let Indy emit a correct, keep-alive header; we add chunked ourselves and
  // suppress Content-Length. After WriteHeader, Indy marks the header written
  // and will NOT append its own body/header for this request.
  FResponseInfo.ResponseNo := 200;
  FResponseInfo.ContentType := AContentType + '; charset=' + ACharset;
  FResponseInfo.CloseConnection := False;
  // Set the dedicated TransferEncoding property (NOT a CustomHeaders entry):
  // TIdHTTPResponseInfo.WriteHeader only honors ContentLength = -1 (i.e. omits
  // Content-Length) when this property is 'chunked'/non-identity. With it unset,
  // Indy sees an empty body, synthesizes a default HTML error page and emits its
  // byte count as Content-Length. Setting the property both suppresses that and
  // makes Indy emit the Transfer-Encoding: chunked header itself (no duplicate).
  FResponseInfo.TransferEncoding := 'chunked';
  FResponseInfo.CustomHeaders.Values['Cache-Control'] := 'no-cache';
  FResponseInfo.ContentLength := -1; // suppress automatic Content-Length
  if not FConnected then Exit;
  try
    FResponseInfo.WriteHeader;
  except
    FConnected := False;
  end;
end;

procedure TMVCIndyChunkedWriter.WriteChunk(const ABytes: TBytes);
var
  lIdBytes: TIdBytes;
begin
  if (not FConnected) or (Length(ABytes) = 0) then
    Exit;
  try
    // <hexlen>CRLF
    FIO.Write(IntToHex(Length(ABytes), 1) + #13#10, FEnc);
    SetLength(lIdBytes, Length(ABytes));
    Move(ABytes[0], lIdBytes[0], Length(ABytes));
    FIO.Write(lIdBytes);
    FIO.Write(#13#10, FEnc); // trailing CRLF
  except
    FConnected := False;
  end;
end;

procedure TMVCIndyChunkedWriter.Finish;
begin
  RawWrite('0'#13#10#13#10); // terminating chunk
end;

function TMVCIndyChunkedWriter.Connected: Boolean;
begin
  Result := FConnected;
end;

function TMVCIndyDirectResponse.CreateChunkedWriter: IMVCChunkedResponseWriter;
begin
  StreamingHandled := True; // engine skips function-return render; Flush no-ops
  FChunkedStreaming := True; // keep-alive: do NOT force CloseConnection in Flush
  Result := TMVCIndyChunkedWriter.Create(FResponseInfo, FContext.Connection.IOHandler);
end;

procedure TMVCIndyDirectResponse.SendRedirect(const AUrl: string);
begin
  FResponseInfo.Redirect(AUrl);
end;

procedure TMVCIndyDirectResponse.SendResponse;
begin
  Flush;
end;

end.
