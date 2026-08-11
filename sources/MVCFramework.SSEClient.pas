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
//
// SSE Client - Server-Sent Events client implementation
//
// Follows the W3C EventSource specification:
// - Robust parsing of SSE fields (id, event, data, retry)
// - Multiline data support (multiple "data:" fields concatenated with LF)
// - String-based Last-Event-ID (spec-compliant)
// - Automatic reconnection with configurable retry timeout
// - Thread-safe termination via TInterlocked
// - Incremental streaming via OnReceiveData
// - Clean shutdown: Stop() signals termination via TInterlocked flag,
//   which is checked by OnReceiveData (sets AAbort := True).
//   The next server heartbeat triggers the callback and aborts the request.
//   Stop() is non-blocking — callers should invoke it from a background
//   thread or call it fire-and-forget if UI responsiveness is needed.
//
// Uses THTTPClient directly (no TNetHTTPClient/TNetHTTPRequest components)
// for a simpler single-object lifecycle.
//
// IMPORTANT: OnEvent/OnError callbacks are raised from the background thread.
// Use TThread.Queue or TThread.Synchronize for UI updates.
//
// ***************************************************************************

unit MVCFramework.SSEClient;

interface

uses
  System.SysUtils,
  System.Classes,
  System.NetConsts,
  System.Net.HttpClient,
  System.Net.URLClient,
  System.SyncObjs;

type
  TSSEClientEvent = reference to procedure(const AId, AEvent, AData: string);
  TSSEClientErrorEvent = reference to procedure(const AError: string);
  TSSEClientOpenEvent = reference to procedure;
  TSSEClientHeadersEvent = reference to procedure(var AHeaders: TNetHeaders);

  /// <summary>
  /// Standalone SSE parser for unit-testing without network.
  /// Feed raw SSE data via Feed() and collect events via OnEvent.
  /// Also used internally by TMVCSSEClient for parsing.
  /// </summary>
  TSSEClientParser = class
  private
    fBuffer: string;
    fCurrentId: string;
    fCurrentEvent: string;
    fCurrentData: TStringList;
    fLastEventId: string;
    fReconnectTimeout: Integer;
    fOnEvent: TSSEClientEvent;
    procedure ProcessBuffer;
    procedure DispatchEvent;
    procedure ResetEventState;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Feed(const AData: string);
    procedure Reset;
    property OnEvent: TSSEClientEvent read fOnEvent write fOnEvent;
    property LastEventId: string read fLastEventId;
    property ReconnectTimeout: Integer read fReconnectTimeout write fReconnectTimeout;
  end;

  TMVCSSEClient = class
  private
    fURL: string;
    fTerminated: Int64;
    fIgnoreCertificateErrors: Boolean;
    fWorkerThread: TThread;
    fParser: TSSEClientParser;
    fOnEvent: TSSEClientEvent;
    fOnError: TSSEClientErrorEvent;
    fOnOpen: TSSEClientOpenEvent;
    fOnQueryHeaders: TSSEClientHeadersEvent;
    fOpenFired: Boolean;
    fResponseStream: TMemoryStream;
    fLastProcessedPos: Int64;
    fPendingBytes: TBytes;   // trailing incomplete UTF-8 sequence, if any
    fStopEvent: TEvent;
    function GetLastEventId: string;
    function GetReconnectTimeout: Integer;
    procedure SetReconnectTimeout(const Value: Integer);
    procedure DoReceiveLoop;
    procedure HandleReceiveData(const Sender: TObject;
      AContentLength, AReadCount: Int64; var AAbort: Boolean);
    procedure ValidateServerCertificate(const Sender: TObject;
      const ARequest: TURLRequest; const Certificate: TCertificate;
      var Accepted: Boolean);
  public
    constructor Create(const AURL: string;
      const AIgnoreCertificateErrors: Boolean = True);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    function IsRunning: Boolean;
    property OnEvent: TSSEClientEvent read fOnEvent write fOnEvent;
    property OnError: TSSEClientErrorEvent read fOnError write fOnError;
    property OnOpen: TSSEClientOpenEvent read fOnOpen write fOnOpen;
    property OnQueryHeaders: TSSEClientHeadersEvent read fOnQueryHeaders write fOnQueryHeaders;
    property LastEventId: string read GetLastEventId;
    property ReconnectTimeout: Integer read GetReconnectTimeout write SetReconnectTimeout;
    property URL: string read fURL write fURL;
  end;

/// <summary>
/// Decodes the longest complete UTF-8 prefix of APending + ANewData, keeping
/// any trailing incomplete sequence in APending for the next call.
/// </summary>
/// <remarks>
/// The stream is UTF-8 by specification, but nothing aligns a socket read to
/// a character boundary: an accented letter, an emoji or a CJK ideogram can
/// have its bytes split across two reads. TEncoding.UTF8.GetString is
/// stateless and raises EEncodingError on the truncated half, so an event
/// stream has to be decoded incrementally — which is what the WHATWG spec
/// prescribes. Exposed at unit level so it can be tested without a network.
/// </remarks>
function DecodeUTF8Incremental(var APending: TBytes; const ANewData: TBytes): string;

implementation

const
  DEFAULT_RECONNECT_TIMEOUT = 10000;
  LF = #10;
  CR = #13;

{ ========================================================================= }
{ Incremental UTF-8 decoding                                                 }
{ ========================================================================= }

// Number of bytes in the sequence started by ALeadByte. Returns 1 for a byte
// that cannot start one, so invalid input is passed through to the decoder
// rather than buffered forever.
function UTF8SequenceLength(ALeadByte: Byte): Integer;
begin
  if ALeadByte < $80 then
    Result := 1
  else if (ALeadByte and $E0) = $C0 then
    Result := 2
  else if (ALeadByte and $F0) = $E0 then
    Result := 3
  else if (ALeadByte and $F8) = $F0 then
    Result := 4
  else
    Result := 1;
end;

function DecodeUTF8Incremental(var APending: TBytes; const ANewData: TBytes): string;
var
  LAll: TBytes;
  LCut, I, LSteps, LNeeded: Integer;
begin
  if Length(APending) = 0 then
    LAll := ANewData
  else
    LAll := APending + ANewData;

  if Length(LAll) = 0 then
  begin
    SetLength(APending, 0);
    Exit('');
  end;

  // Walk back over at most three continuation bytes looking for the lead
  // byte of the last sequence. A sequence is at most four bytes long, so
  // anything further back is already complete.
  LCut := Length(LAll);
  I := Length(LAll) - 1;
  LSteps := 0;
  while (I >= 0) and (LSteps < 4) do
  begin
    if (LAll[I] and $C0) = $80 then
    begin
      // Continuation byte: the lead byte is further back.
      Dec(I);
      Inc(LSteps);
      Continue;
    end;

    LNeeded := UTF8SequenceLength(LAll[I]);
    if Length(LAll) - I < LNeeded then
      LCut := I;   // truncated: hold these bytes back for the next chunk
    Break;
  end;

  if LCut = Length(LAll) then
  begin
    SetLength(APending, 0);
    Result := TEncoding.UTF8.GetString(LAll);
  end
  else
  begin
    APending := Copy(LAll, LCut, Length(LAll) - LCut);
    Result := TEncoding.UTF8.GetString(Copy(LAll, 0, LCut));
  end;
end;

{ ========================================================================= }
{ SSE Parsing Logic                                                         }
{ ========================================================================= }

procedure ParseSSEField(const ALine: string; out AFieldName, AFieldValue: string);
var
  LColonPos: Integer;
begin
  LColonPos := Pos(':', ALine);
  if LColonPos > 0 then
  begin
    AFieldName := Copy(ALine, 1, LColonPos - 1);
    AFieldValue := Copy(ALine, LColonPos + 1, MaxInt);
    // Remove single leading space after colon (per spec)
    if (Length(AFieldValue) > 0) and (AFieldValue[1] = ' ') then
      Delete(AFieldValue, 1, 1);
  end
  else
  begin
    // Line with no colon: field name is the whole line, value is empty
    AFieldName := ALine;
    AFieldValue := '';
  end;
end;

{ ========================================================================= }
{ TSSEClientParser                                                          }
{ ========================================================================= }

constructor TSSEClientParser.Create;
begin
  inherited Create;
  fCurrentData := TStringList.Create;
  fReconnectTimeout := DEFAULT_RECONNECT_TIMEOUT;
  fBuffer := '';
  fLastEventId := '';
  ResetEventState;
end;

destructor TSSEClientParser.Destroy;
begin
  fCurrentData.Free;
  inherited;
end;

procedure TSSEClientParser.ResetEventState;
begin
  fCurrentId := '';
  fCurrentEvent := '';
  fCurrentData.Clear;
end;

procedure TSSEClientParser.Reset;
begin
  fBuffer := '';
  ResetEventState;
end;

procedure TSSEClientParser.DispatchEvent;
var
  LData: string;
begin
  if fCurrentData.Count = 0 then
  begin
    ResetEventState;
    Exit;
  end;
  // Concatenate data parts with LF (per spec: multiple data fields joined with U+000A)
  LData := string.Join(LF, fCurrentData.ToStringArray);
  if Assigned(fOnEvent) then
    fOnEvent(fCurrentId, fCurrentEvent, LData);
  ResetEventState;
end;

procedure TSSEClientParser.ProcessBuffer;
var
  LLineEnd: Integer;
  LLine: string;
  LFieldName, LFieldValue: string;
  LRetryVal: Integer;
  I: Integer;
begin
  while True do
  begin
    // Find next line ending (LF, CRLF, or CR)
    LLineEnd := 0;
    for I := 1 to Length(fBuffer) do
    begin
      if (fBuffer[I] = LF) or (fBuffer[I] = CR) then
      begin
        LLineEnd := I;
        Break;
      end;
    end;

    if LLineEnd = 0 then
      Break; // No complete line yet

    LLine := Copy(fBuffer, 1, LLineEnd - 1);

    // Consume the line ending (handle CRLF as single terminator)
    if (LLineEnd < Length(fBuffer)) and (fBuffer[LLineEnd] = CR) and (fBuffer[LLineEnd + 1] = LF) then
      Delete(fBuffer, 1, LLineEnd + 1)
    else
      Delete(fBuffer, 1, LLineEnd);

    // Empty line = dispatch event
    if LLine.IsEmpty then
    begin
      DispatchEvent;
      Continue;
    end;

    // Comment line (starts with ':')
    if LLine[1] = ':' then
      Continue;

    // Parse field
    ParseSSEField(LLine, LFieldName, LFieldValue);

    if SameText(LFieldName, 'data') then
      fCurrentData.Add(LFieldValue)
    else if SameText(LFieldName, 'id') then
    begin
      fCurrentId := LFieldValue;
      // Per spec: do not update lastEventId if value contains null char
      if Pos(#0, LFieldValue) = 0 then
        fLastEventId := LFieldValue;
    end
    else if SameText(LFieldName, 'event') then
      fCurrentEvent := LFieldValue
    else if SameText(LFieldName, 'retry') then
    begin
      if TryStrToInt(LFieldValue.Trim, LRetryVal) and (LRetryVal >= 0) then
        fReconnectTimeout := LRetryVal;
    end;
    // Unknown fields are ignored per spec
  end;
end;

procedure TSSEClientParser.Feed(const AData: string);
begin
  fBuffer := fBuffer + AData;
  ProcessBuffer;
end;

{ ========================================================================= }
{ TMVCSSEClient                                                             }
{ ========================================================================= }

constructor TMVCSSEClient.Create(const AURL: string;
  const AIgnoreCertificateErrors: Boolean);
begin
  inherited Create;
  fURL := AURL;
  fIgnoreCertificateErrors := AIgnoreCertificateErrors;
  TInterlocked.Exchange(fTerminated, 1);
  fStopEvent := TEvent.Create(nil, True, False, '');
  fParser := TSSEClientParser.Create;
  fParser.OnEvent :=
    procedure(const AId, AEvent, AData: string)
    begin
      if Assigned(fOnEvent) then
        fOnEvent(AId, AEvent, AData);
    end;
end;

destructor TMVCSSEClient.Destroy;
begin
  Stop;
  fParser.Free;
  fStopEvent.Free;
  inherited;
end;

function TMVCSSEClient.GetLastEventId: string;
begin
  Result := fParser.LastEventId;
end;

function TMVCSSEClient.GetReconnectTimeout: Integer;
begin
  Result := fParser.ReconnectTimeout;
end;

procedure TMVCSSEClient.SetReconnectTimeout(const Value: Integer);
begin
  fParser.ReconnectTimeout := Value;
end;

function TMVCSSEClient.IsRunning: Boolean;
begin
  Result := TInterlocked.Read(fTerminated) = 0;
end;

procedure TMVCSSEClient.Start;
begin
  if not Assigned(fOnEvent) then
    raise Exception.Create('OnEvent handler must be assigned before calling Start');

  TInterlocked.Exchange(fTerminated, 0);
  fStopEvent.ResetEvent;

  fWorkerThread := TThread.CreateAnonymousThread(
    procedure
    begin
      DoReceiveLoop;
    end);
  fWorkerThread.FreeOnTerminate := False;
  fWorkerThread.Start;
end;

procedure TMVCSSEClient.Stop;
begin
  TInterlocked.Exchange(fTerminated, 1);
  fStopEvent.SetEvent;
  // The worker thread will exit when the next OnReceiveData fires
  // (triggered by server heartbeat) and AAbort is set to True.
  if Assigned(fWorkerThread) then
  begin
    fWorkerThread.WaitFor;
    FreeAndNil(fWorkerThread);
  end;
end;

procedure TMVCSSEClient.ValidateServerCertificate(const Sender: TObject;
  const ARequest: TURLRequest; const Certificate: TCertificate;
  var Accepted: Boolean);
begin
  Accepted := True;
end;

procedure TMVCSSEClient.HandleReceiveData(const Sender: TObject;
  AContentLength, AReadCount: Int64; var AAbort: Boolean);
var
  LNewData: string;
  LBytes: TBytes;
  LBytesToRead: Int64;
begin
  AAbort := TInterlocked.Read(fTerminated) = 1;
  if AAbort then
    Exit;

  if fResponseStream = nil then
    Exit;

  // Fire OnOpen once per connection
  if not fOpenFired then
  begin
    fOpenFired := True;
    if Assigned(fOnOpen) then
      fOnOpen();
  end;

  // Read only the newly arrived data from the response stream
  LBytesToRead := fResponseStream.Size - fLastProcessedPos;
  if LBytesToRead <= 0 then
    Exit;

  SetLength(LBytes, LBytesToRead);
  fResponseStream.Position := fLastProcessedPos;
  fResponseStream.ReadBuffer(LBytes[0], LBytesToRead);
  fLastProcessedPos := fResponseStream.Size;

  // Decode incrementally: a read boundary can fall inside a multi-byte
  // character, and decoding the raw chunk would raise EEncodingError on the
  // truncated half, dropping the connection.
  LNewData := DecodeUTF8Incremental(fPendingBytes, LBytes);
  if LNewData <> '' then
    fParser.Feed(LNewData);
end;

procedure TMVCSSEClient.DoReceiveLoop;
var
  LClient: THTTPClient;
  LResponseStream: TMemoryStream;
  LHeaders: TNetHeaders;
begin
  while TInterlocked.Read(fTerminated) = 0 do
  begin
    fParser.Reset;
    fOpenFired := False;
    fLastProcessedPos := 0;
    SetLength(fPendingBytes, 0);   // a reconnect starts a fresh byte stream

    LClient := THTTPClient.Create;
    try
      LClient.Accept := 'text/event-stream';
      LClient.HandleRedirects := True;
      LClient.ConnectionTimeout := 10000;
      LClient.OnReceiveData := HandleReceiveData;

      if fIgnoreCertificateErrors then
        LClient.OnValidateServerCertificate := ValidateServerCertificate;

      LHeaders := [TNetHeader.Create('Cache-Control', 'no-cache')];
      if not fParser.LastEventId.IsEmpty then
        LHeaders := LHeaders + [TNetHeader.Create('Last-Event-ID', fParser.LastEventId)];

      if Assigned(fOnQueryHeaders) then
        fOnQueryHeaders(LHeaders);

      LResponseStream := TMemoryStream.Create;
      try
        fResponseStream := LResponseStream;
        try
          // Blocks until server closes, error, or AAbort in OnReceiveData
          LClient.Get(fURL, LResponseStream, LHeaders);
        except
          on E: Exception do
          begin
            if TInterlocked.Read(fTerminated) = 1 then
              Break;
            if Assigned(fOnError) then
              fOnError(E.Message);
          end;
        end;
      finally
        fResponseStream := nil;
        LResponseStream.Free;
      end;
    finally
      LClient.Free;
    end;

    // Reconnect wait (instantly cancellable by Stop via fStopEvent)
    if TInterlocked.Read(fTerminated) = 0 then
    begin
      if Assigned(fOnError) then
        fOnError('Connection lost, reconnecting in ' + IntToStr(fParser.ReconnectTimeout) + 'ms');
      fStopEvent.WaitFor(fParser.ReconnectTimeout);
    end;
  end;
end;

end.
