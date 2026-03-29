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

unit MVCFramework.SSE.Writer;

/// <summary>
/// Lightweight streaming writers for SSE and JSON Lines (NDJSON) from any controller action.
///
/// NOTE: These writers require an Indy-based application server (TIdHTTPWebBrokerBridge).
/// They access the Indy IOHandler directly for incremental streaming.
/// For ISAPI or Apache module deployments, use TMVCSSEController + TMVCSSEBroker instead.
///
/// Use these for simple "one client, one stream" scenarios:
/// - AI chat streaming (token by token)
/// - Progress reporting (long-running task)
/// - Large result set streaming (database query)
/// - Log tailing
///
/// For multi-client broadcasting (chat rooms, live dashboards, notifications),
/// use TMVCSSEController + TMVCSSEBroker instead.
/// </summary>

{$I dmvcframework.inc}

interface

uses
  System.SysUtils,
  MVCFramework,
  MVCFramework.Commons;

type
  /// <summary>
  /// Base class for HTTP streaming writers.
  /// Manages the raw Indy IOHandler connection and provides low-level write operations.
  /// Do not use directly - use TMVCSSEWriter or TMVCJSONLWriter.
  /// </summary>
  TMVCStreamWriter = class abstract
  private
    FIOHandler: TObject;   // TIdIOHandler
    FEncodingIntf: IInterface; // IIdTextEncoding
    FConnected: Boolean;
  protected
    procedure WriteRaw(const AText: string);
    procedure BeginBuffer;
    procedure EndBuffer;
  public
    constructor Create(const AContext: TWebContext;
      const AContentType: string;
      const ACharset: string = 'utf-8');
    function Connected: Boolean;
  end;

  /// <summary>
  /// SSE (Server-Sent Events) streaming writer.
  ///
  /// Writes events in text/event-stream format. The browser's EventSource API
  /// handles reconnection, last-event-id, and event dispatching automatically.
  ///
  /// Usage:
  ///   procedure TMyController.ChatStream;
  ///   var
  ///     lSSE: TMVCSSEWriter;
  ///   begin
  ///     lSSE := TMVCSSEWriter.Create(Context);
  ///     try
  ///       lSSE.Send('token', 'Hello');
  ///       Sleep(100);
  ///       lSSE.Send('token', 'World');
  ///       lSSE.Send('done', '');
  ///     finally
  ///       lSSE.Free;
  ///     end;
  ///   end;
  /// </summary>
  TMVCSSEWriter = class(TMVCStreamWriter)
  public
    constructor Create(const AContext: TWebContext;
      const ACharset: string = 'utf-8';
      const ARetryMS: Integer = 10000);

    /// <summary>
    /// Sends an SSE event with event type, data, and optional id.
    /// Multi-line data is split automatically.
    /// </summary>
    procedure Send(const AEvent, AData: string; const AId: string = ''); overload;

    /// <summary>
    /// Sends an SSE data-only event (received as 'message' by EventSource).
    /// </summary>
    procedure Send(const AData: string); overload;

    /// <summary>
    /// Sends an SSE comment (ignored by EventSource, useful as heartbeat).
    /// </summary>
    procedure SendComment(const AText: string);
  end;

  /// <summary>
  /// JSON Lines (NDJSON) streaming writer.
  ///
  /// Writes one JSON object per line in application/x-ndjson format.
  /// Each line is a complete, self-contained JSON document.
  ///
  /// Usage:
  ///   procedure TMyController.StreamPeople;
  ///   var
  ///     lJSONL: TMVCJSONLWriter;
  ///   begin
  ///     lJSONL := TMVCJSONLWriter.Create(Context);
  ///     try
  ///       lJSONL.Send('{"name":"Peter","age":30}');
  ///       lJSONL.Send('{"name":"Bruce","age":40}');
  ///     finally
  ///       lJSONL.Free;
  ///     end;
  ///   end;
  /// </summary>
  TMVCJSONLWriter = class(TMVCStreamWriter)
  public
    constructor Create(const AContext: TWebContext;
      const ACharset: string = 'utf-8');

    /// <summary>
    /// Sends a single JSON line. A newline is appended automatically.
    /// </summary>
    procedure Send(const AJSONLine: string);
  end;

implementation

uses
  IdIOHandler,
  IdGlobal,
  IdContext,
  IdHTTPWebBrokerBridge;

const
  LF = #10;
  CRLF = #13#10;

type
  TIdHTTPAppResponseAccess = class(TIdHTTPAppResponse);

{ TMVCStreamWriter }

constructor TMVCStreamWriter.Create(const AContext: TWebContext;
  const AContentType: string; const ACharset: string);
var
  lRawContext: TIdContext;
  lIOHandler: TIdIOHandler;
  lEncoding: IIdTextEncoding;
begin
  inherited Create;
  if not (AContext.Response.RawWebResponse is TIdHTTPAppResponse) then
    raise EMVCException.Create(HTTP_STATUS.InternalServerError,
      'Streaming writers require an Indy-based application server');
  lRawContext := TIdHTTPAppResponseAccess(AContext.Response.RawWebResponse).FThread;
  lIOHandler := lRawContext.Connection.IOHandler;
  lEncoding := IndyTextEncoding(ACharset);
  FIOHandler := lIOHandler;
  FEncodingIntf := lEncoding;
  FConnected := True;

  // Write HTTP headers directly to the socket
  lIOHandler.WriteBufferOpen;
  try
    lIOHandler.Write('HTTP/1.1 200 OK' + CRLF, lEncoding);
    lIOHandler.Write('Content-Type: ' + AContentType + '; charset=' + ACharset + CRLF, lEncoding);
    lIOHandler.Write('Cache-Control: no-cache' + CRLF, lEncoding);
    lIOHandler.Write('Connection: keep-alive' + CRLF, lEncoding);
    lIOHandler.Write('X-Accel-Buffering: no' + CRLF, lEncoding);
    lIOHandler.Write(CRLF, lEncoding);
  finally
    lIOHandler.WriteBufferClose;
  end;
end;

procedure TMVCStreamWriter.WriteRaw(const AText: string);
begin
  if not FConnected then
    Exit;
  try
    TIdIOHandler(FIOHandler).Write(AText, IIdTextEncoding(FEncodingIntf));
  except
    FConnected := False;
  end;
end;

procedure TMVCStreamWriter.BeginBuffer;
begin
  if FConnected then
    TIdIOHandler(FIOHandler).WriteBufferOpen;
end;

procedure TMVCStreamWriter.EndBuffer;
begin
  if FConnected then
  try
    TIdIOHandler(FIOHandler).WriteBufferClose;
  except
    FConnected := False;
  end;
end;

function TMVCStreamWriter.Connected: Boolean;
begin
  Result := FConnected;
end;

{ TMVCSSEWriter }

constructor TMVCSSEWriter.Create(const AContext: TWebContext;
  const ACharset: string; const ARetryMS: Integer);
begin
  inherited Create(AContext, 'text/event-stream', ACharset);
  WriteRaw('retry: ' + IntToStr(ARetryMS) + LF + LF);
  WriteRaw(': stream opened' + LF + LF);
end;

procedure TMVCSSEWriter.Send(const AEvent, AData: string; const AId: string);
var
  lLines: TArray<string>;
  lLine: string;
begin
  if not Connected then
    Exit;
  try
    BeginBuffer;
    try
      if not AId.IsEmpty then
        WriteRaw('id: ' + AId + LF);
      if not AEvent.IsEmpty then
        WriteRaw('event: ' + AEvent + LF);
      lLines := AData.Split([#10]);
      for lLine in lLines do
        WriteRaw('data: ' + lLine + LF);
      WriteRaw(LF);
    finally
      EndBuffer;
    end;
  except
    FConnected := False;
  end;
end;

procedure TMVCSSEWriter.Send(const AData: string);
begin
  Send('', AData);
end;

procedure TMVCSSEWriter.SendComment(const AText: string);
begin
  WriteRaw(': ' + AText + LF + LF);
end;

{ TMVCJSONLWriter }

constructor TMVCJSONLWriter.Create(const AContext: TWebContext;
  const ACharset: string);
begin
  inherited Create(AContext, 'application/x-ndjson', ACharset);
end;

procedure TMVCJSONLWriter.Send(const AJSONLine: string);
begin
  WriteRaw(AJSONLine + LF);
end;

end.
