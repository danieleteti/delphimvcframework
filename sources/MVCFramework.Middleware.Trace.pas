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

unit MVCFramework.Middleware.Trace;

{$I dmvcframework.inc}

{$WARN SYMBOL_DEPRECATED OFF} // the class is deprecated for its users, not inside its own unit

interface

uses
  MVCFramework,
  MVCFramework.Logger,
  MVCFramework.Commons;

type
  // Middleware that logs the full lifecycle of every request (incoming request
  // line + body, controller/action dispatch, response status/headers/body) and
  // measures the end-to-end processing time.
  //
  // Each request is tagged with a correlation id so that, even under heavy
  // concurrency, all log lines belonging to the same request can be grepped
  // together. An inbound 'X-Request-ID' / 'X-Correlation-ID' header is honored
  // (distributed-tracing friendly); otherwise a fresh id is generated. The id
  // is echoed back to the client via a response header.
  TMVCTraceMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    fMaxBodySize: Integer;
    fLogTag: string;
    fCorrelationHeaderName: string;
    function ResolveCorrelationId(AContext: TWebContext): string;
    function RequestId(AContext: TWebContext): string;
    function ClampBody(const AValue: string): string;
  protected
    procedure OnAfterControllerAction(AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      const AHandled: Boolean);
    procedure OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
    procedure OnBeforeControllerAction(AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionNAme: string; var AHandled: Boolean);
    procedure OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
  public
    // MaxBodySizeInTrace: max number of characters of the body logged (request and response).
    // LogTag: LoggerPro tag used for every line emitted by this middleware.
    // CorrelationHeaderName: request/response header carrying the correlation id.
    constructor Create(const MaxBodySizeInTrace: UInt64 = 1024;
      const LogTag: string = 'trace';
      const CorrelationHeaderName: string = 'X-Request-ID');
  end deprecated 'Use the Trace HTTP filter (MVCFramework.Filters): AEngine.Use(TraceHTTPFilter(...))';

implementation

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Diagnostics,
  System.SyncObjs,
  MVCFramework.Filters.Internal; // MVCTraceBodyText

const
  // keys used to carry per-request state across the middleware hooks
  TRACE_DATA_REQUESTID = '__trace_request_id';
  TRACE_DATA_TIMESTAMP = '__trace_started_ticks';

var
  // process-wide monotonic counter used as a fallback correlation id
  gRequestCounter: Int64 = 0;

constructor TMVCTraceMiddleware.Create(const MaxBodySizeInTrace: UInt64 = 1024;
  const LogTag: string = 'trace'; const CorrelationHeaderName: string = 'X-Request-ID');
begin
  inherited Create;
  fMaxBodySize := Integer(Min(MaxBodySizeInTrace, MaxInt));
  fLogTag := LogTag;
  fCorrelationHeaderName := CorrelationHeaderName;
end;

function TMVCTraceMiddleware.ClampBody(const AValue: string): string;
begin
  Result := AValue.Substring(0, Min(AValue.Length, fMaxBodySize));
end;

function TMVCTraceMiddleware.ResolveCorrelationId(AContext: TWebContext): string;
begin
  // honor an upstream correlation id (proxy/gateway/distributed tracing) ...
  Result := AContext.Request.Headers[fCorrelationHeaderName];
  if Result.IsEmpty then
    Result := AContext.Request.Headers['X-Correlation-ID'];
  // ... otherwise mint a short, process-unique one
  if Result.IsEmpty then
    Result := IntToHex(TInterlocked.Increment(gRequestCounter), 6);
end;

function TMVCTraceMiddleware.RequestId(AContext: TWebContext): string;
begin
  // populated in OnBeforeRouting; used as a prefix on every other line
  if not AContext.Data.TryGetValue(TRACE_DATA_REQUESTID, Result) then
    Result := '------';
  Result := '[rid:' + Result + ']';
end;

procedure TMVCTraceMiddleware.OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
var
  lContentType: string;
  lReq: TMVCWebRequest;
  lCorrelationId: string;
  lBytes: TBytes;
begin
  lCorrelationId := ResolveCorrelationId(AContext);
  AContext.Data[TRACE_DATA_REQUESTID] := lCorrelationId;
  AContext.Data[TRACE_DATA_TIMESTAMP] := TStopwatch.GetTimeStamp.ToString;
  // echo the correlation id so the caller can match its request in the logs
  AContext.Response.SetCustomHeader(fCorrelationHeaderName, lCorrelationId);

  AContext.Request.ReadTotalContent;
  lReq := AContext.Request;
  Log.Debug('%s [BEFORE ROUTING][%s][IP: %s][URL: %s][QUERYSTRING: %s][LENGTH: %d][ACCEPT: %s][USER-AGENT: %s][AUTHORIZATION: %s]',
    [
    RequestId(AContext),
    lReq.HTTPMethodAsString,
    lReq.ClientIp,
    lReq.PathInfo,
    lReq.QueryFieldsDelimitedText,
    lReq.ContentLength,
    lReq.Accept,
    lReq.UserAgent,
    MVCRedactSecret(lReq.Authorization)
    ], fLogTag);

  if AContext.Request.HTTPMethod in [httpPOST, httpPUT, httpPATCH] then
  begin
    lContentType := AContext.Request.Headers['content-type'].ToLower;
    if lContentType.StartsWith(TMVCMediaType.APPLICATION_JSON, True) or
      lContentType.StartsWith(TMVCMediaType.APPLICATION_XML, True) or
      lContentType.StartsWith(TMVCMediaType.APPLICATION_FORM_URLENCODED, True) or
      lContentType.StartsWith('text/') then
    begin
      lBytes := AContext.Request.RawContent;
      Log.Debug('%s [BEFORE ROUTING][REQUEST][BODY] %s',
        [RequestId(AContext), ClampBody(MVCTraceBodyText(MVCTraceBodyBytes(lBytes, fMaxBodySize), Length(lBytes),
        lContentType))], fLogTag);
    end
    else
    begin
      Log.Debug('%s [BEFORE ROUTING][REQUEST][BODY] <hidden %d bytes of %s content>',
        [RequestId(AContext), AContext.Request.ContentLength, lContentType], fLogTag);
    end;
  end;
end;

procedure TMVCTraceMiddleware.OnBeforeControllerAction(AContext: TWebContext;
  const AControllerQualifiedClassName, AActionNAme: string; var AHandled: Boolean);
begin
  Log.Debug('%s [BEFORE ACTION][CONTROLLER: %s][ACTION: %s]',
    [RequestId(AContext), AControllerQualifiedClassName, AActionNAme], fLogTag);
end;

procedure TMVCTraceMiddleware.OnAfterControllerAction(AContext: TWebContext;
  const AControllerQualifiedClassName: string; const AActionName: string;
  const AHandled: Boolean);
var
  lBytes: TBytes;
  lBody: string;
begin
  Log.Debug('%s [AFTER ACTION][RESPONSE][STATUS] %d: %s',
    [RequestId(AContext), AContext.Response.StatusCode, AContext.Response.ReasonString], fLogTag);
  Log.Debug('%s [AFTER ACTION][RESPONSE][CUSTOM HEADERS] %s',
    [RequestId(AContext), MVCRedactedHeaders(AContext.Response.CustomHeaders)], fLogTag);
  Log.Debug('%s [AFTER ACTION][RESPONSE][CONTENT-TYPE] %s',
    [RequestId(AContext), AContext.Response.ContentType], fLogTag);

  if Assigned(AContext.Response.ContentStream) then
  begin
    SetLength(lBytes, Min(AContext.Response.ContentStream.Size, fMaxBodySize));
    AContext.Response.ContentStream.Position := 0;
    AContext.Response.ContentStream.ReadBuffer(lBytes, Length(lBytes));
    AContext.Response.ContentStream.Position := 0;
    lBody := MVCTraceBodyText(lBytes, AContext.Response.ContentStream.Size, AContext.Response.ContentType);
  end
  else
  begin
    lBody := ClampBody(AContext.Response.Content);
  end;
  Log.Debug('%s [AFTER ACTION][RESPONSE][BODY] %s', [RequestId(AContext), lBody], fLogTag);
end;

procedure TMVCTraceMiddleware.OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
var
  lStartTicks: Int64;
  lElapsedMS: Double;
  lStartStr: string;
begin
  // single, grep-friendly summary line that always fires (even on 404/no-match),
  // including the wall-clock time spent serving the request
  lElapsedMS := -1;
  if AContext.Data.TryGetValue(TRACE_DATA_TIMESTAMP, lStartStr)
    and Int64.TryParse(lStartStr, lStartTicks) then
  begin
    lElapsedMS := (TStopwatch.GetTimeStamp - lStartTicks) * 1000 / TStopwatch.Frequency;
  end;

  Log.Debug('%s [AFTER ROUTING][%s %s] -> %d %s [HANDLED: %s][%.2f ms]',
    [
    RequestId(AContext),
    AContext.Request.HTTPMethodAsString,
    AContext.Request.PathInfo,
    AContext.Response.StatusCode,
    AContext.Response.ReasonString,
    BoolToStr(AHandled, True),
    lElapsedMS
    ], fLogTag);
end;

end.
