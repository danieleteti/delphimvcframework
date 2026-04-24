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
    FConnection: TObject;  // TIdContext.Connection - kept to close the socket
  protected
    procedure WriteRaw(const AText: string);
    procedure BeginBuffer;
    procedure EndBuffer;
  public
    constructor Create(const AContext: TWebContext;
      const AContentType: string;
      const ACharset: string = 'utf-8');
    destructor Destroy; override;
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

  /// <summary>
  /// JSON Array streaming writer. Emits a well-formed JSON array
  /// (application/json) one element at a time, directly to the socket.
  ///
  /// The "["  is written by the constructor, the "]" is written by the
  /// destructor (or by an explicit call to Close). Between calls only
  /// the running element and a separator comma live in memory: server
  /// RAM stays flat regardless of how many elements are emitted. The
  /// client, by contrast, receives a single valid JSON array and can
  /// parse it like any other JSON response.
  ///
  /// Typical use: streaming a TDataSet with 100k+ rows to the client
  /// without ever materialising the full result in the server.
  ///
  /// Usage:
  ///   procedure TMyController.StreamPeople;
  ///   var
  ///     lW: TMVCJSONArrayWriter;
  ///   begin
  ///     lW := TMVCJSONArrayWriter.Create(Context);
  ///     try
  ///       while not ds.Eof do
  ///       begin
  ///         if not lW.Connected then Break;
  ///         lW.Send(Serializer.SerializeDataSetRecord(ds));
  ///         ds.Next;
  ///       end;
  ///     finally
  ///       lW.Free;  // emits the closing "]"
  ///     end;
  ///   end;
  ///
  /// Like the other writers in this unit, this class requires an Indy-
  /// based backend (Indy Direct or WebBroker on TIdHTTPWebBrokerBridge).
  /// </summary>
  TMVCJSONArrayWriter = class(TMVCStreamWriter)
  private
    fFirstItem: Boolean;
    fClosed: Boolean;
  public
    constructor Create(const AContext: TWebContext;
      const ACharset: string = 'utf-8');
    destructor Destroy; override;

    /// <summary>
    /// Emits one element into the array. The argument must be a complete
    /// JSON value (object, array, primitive). A separator comma is
    /// prepended automatically starting from the second call. Writes
    /// directly to the socket; no intermediate buffer keeps past items.
    /// </summary>
    procedure Send(const AJSONValue: string);

    /// <summary>
    /// Writes the closing "]" and terminates the array. Called
    /// automatically by the destructor; invoke manually only if you
    /// need the client to see the complete array before this object
    /// goes out of scope.
    /// </summary>
    procedure Close;
  end;

implementation

uses
  IdIOHandler,
  IdGlobal,
  IdContext,
  IdTCPConnection;

const
  LF = #10;
  CRLF = #13#10;

{ TMVCStreamWriter }

constructor TMVCStreamWriter.Create(const AContext: TWebContext;
  const AContentType: string; const ACharset: string);
var
  lClientConn: TObject;
  lRawContext: TIdContext;
  lIOHandler: TIdIOHandler;
  lEncoding: IIdTextEncoding;
begin
  inherited Create;
  lClientConn := AContext.Request.GetClientConnection;
  if not Assigned(lClientConn) or not (lClientConn is TIdContext) then
    raise EMVCException.Create(HTTP_STATUS.InternalServerError,
      'Streaming writer requires an Indy-based server backend ' +
      '(TMVCIndyServer or WebBroker hosted by TIdHTTPWebBrokerBridge)');
  lRawContext := TIdContext(lClientConn);
  lIOHandler := lRawContext.Connection.IOHandler;
  lEncoding := IndyTextEncoding(ACharset);
  FIOHandler := lIOHandler;
  FConnection := lRawContext.Connection;
  FEncodingIntf := lEncoding;
  FConnected := True;

  // Let the framework know the response is already being produced on
  // the socket. The engine skips the function-return rendering path
  // after the action returns and Response.Flush becomes a no-op, so
  // no second HTTP response is spliced on top of ours.
  AContext.Response.StreamingHandled := True;

  // Without a Content-Length header (we can't know the final size in
  // advance) nor chunked encoding, the client must rely on connection
  // close as the end-of-body signal. Ask the server to close the
  // connection after this response so the client stops reading at the
  // right moment and plain HTTP clients (curl, browser fetch) work
  // without hanging.
  lRawContext.Connection.IOHandler.WriteBufferOpen;
  try
    lIOHandler.Write('HTTP/1.1 200 OK' + CRLF, lEncoding);
    lIOHandler.Write('Content-Type: ' + AContentType + '; charset=' + ACharset + CRLF, lEncoding);
    lIOHandler.Write('Cache-Control: no-cache' + CRLF, lEncoding);
    lIOHandler.Write('Connection: close' + CRLF, lEncoding);
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

destructor TMVCStreamWriter.Destroy;
begin
  // Close the TCP connection now that the full streamed response has
  // been written. Required because:
  //   - the response has no Content-Length (the writer doesn't know
  //     the total size up-front and uses neither chunked encoding)
  //   - without EOF, an HTTP/1.1 client would wait forever on the
  //     keep-alive socket
  //   - TIdHTTPServer would otherwise append an empty default response
  //     on top of ours after the action returns
  // Closing the socket from here gives the client a clean EOF and
  // prevents Indy from re-using the connection for a second (unwanted)
  // response inside the same handler cycle.
  if Assigned(FConnection) and (FConnection is TIdTCPConnection) then
  begin
    try
      TIdTCPConnection(FConnection).Disconnect;
    except
      // best effort
    end;
  end;
  FConnected := False;
  inherited;
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

{ TMVCJSONArrayWriter }

constructor TMVCJSONArrayWriter.Create(const AContext: TWebContext;
  const ACharset: string);
begin
  inherited Create(AContext, 'application/json', ACharset);
  fFirstItem := True;
  fClosed := False;
  WriteRaw('[');
end;

destructor TMVCJSONArrayWriter.Destroy;
begin
  Close;
  inherited;
end;

procedure TMVCJSONArrayWriter.Send(const AJSONValue: string);
begin
  if fClosed then
    Exit;
  if fFirstItem then
    fFirstItem := False
  else
    WriteRaw(',');
  WriteRaw(AJSONValue);
end;

procedure TMVCJSONArrayWriter.Close;
begin
  if fClosed then
    Exit;
  fClosed := True;
  if Connected then
    WriteRaw(']');
end;

end.
