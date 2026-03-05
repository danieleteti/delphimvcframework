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

unit MVCFramework.SSEController;

interface

uses
  System.SysUtils,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.SSE;

type
  TMVCSSEController = class abstract(TMVCController)
  protected
    /// <summary>
    /// Called when a new client connects to the SSE stream.
    /// Override to set up groups, custom data, or send an initial message.
    /// </summary>
    procedure OnClientConnected(const AConnection: TSSEConnection); virtual;
    /// <summary>
    /// Called when a client disconnects from the SSE stream.
    /// Override for cleanup or logging.
    /// </summary>
    procedure OnClientDisconnected(const AConnection: TSSEConnection); virtual;
    /// <summary>
    /// Called periodically while the client is connected.
    /// Override to check an external data source (database, file, queue, etc.)
    /// and call AConnection.Send() when something has changed.
    /// This is the main extension point for "notify on change" scenarios
    /// where changes originate outside this process.
    /// ANextIntervalMS: set to change the delay before the next OnInterval call.
    /// Initialized to the value of Interval before each call, so the default
    /// behavior is unchanged. Set a shorter value to poll faster when changes
    /// are expected, or a longer value to back off when idle.
    /// </summary>
    procedure OnInterval(const AConnection: TSSEConnection;
      var ANextIntervalMS: Integer); virtual;
    /// <summary>
    /// Default interval in milliseconds between OnInterval calls.
    /// Default: 1000 (1 second). Override to customize.
    /// OnInterval can further adjust this per-call via ANextIntervalMS.
    /// </summary>
    function Interval: Integer; virtual;
    /// <summary>
    /// Heartbeat interval in milliseconds. Override to customize.
    /// Default: 15000 (15 seconds).
    /// A comment line ": heartbeat" is sent at this interval to keep
    /// the connection alive and detect dead clients.
    /// </summary>
    function HeartbeatInterval: Integer; virtual;
    /// <summary>
    /// Retry timeout in milliseconds sent to the client.
    /// The browser will wait this long before reconnecting after a disconnect.
    /// Default: 10000 (10 seconds). Sent once at the beginning of the stream.
    /// </summary>
    function RetryTimeout: Integer; virtual;
    /// <summary>
    /// Character encoding for the SSE stream.
    /// Default: 'utf-8'.
    /// </summary>
    function Charset: string; virtual;
    /// <summary>
    /// Channel name for this SSE endpoint.
    /// Default: the request path (e.g. '/stocks').
    /// Override to use a custom channel name.
    /// </summary>
    function ChannelName: string; virtual;
  public
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces('text/event-stream')]
    procedure EventStream;
  end;

implementation

uses
  System.SyncObjs,
  System.Classes,
  IdContext,
  IdHTTPWebBrokerBridge,
  IdIOHandler,
  IdGlobal,
  MVCFramework.Logger;

const
  LF = #10;
  CRLF = #13#10;

type
  TIdHTTPAppResponseAccess = class(TIdHTTPAppResponse);

{ TMVCSSEController }

procedure TMVCSSEController.OnClientConnected(const AConnection: TSSEConnection);
begin
  // Override in descendants
end;

procedure TMVCSSEController.OnClientDisconnected(const AConnection: TSSEConnection);
begin
  // Override in descendants
end;

procedure TMVCSSEController.OnInterval(const AConnection: TSSEConnection;
  var ANextIntervalMS: Integer);
begin
  // Override in descendants to check external data sources.
  // Modify ANextIntervalMS to change the delay before the next call.
end;

function TMVCSSEController.Interval: Integer;
begin
  Result := TMVCConstants.SSE_INTERVAL_DEFAULT;
end;

function TMVCSSEController.HeartbeatInterval: Integer;
begin
  Result := TMVCConstants.SSE_HEARTBEAT_DEFAULT;
end;

function TMVCSSEController.RetryTimeout: Integer;
begin
  Result := TMVCConstants.SSE_RETRY_DEFAULT;
end;

function TMVCSSEController.Charset: string;
begin
  Result := TMVCConstants.DEFAULT_CONTENT_CHARSET;
end;

function TMVCSSEController.ChannelName: string;
begin
  Result := Context.Request.PathInfo;
end;

procedure TMVCSSEController.EventStream;
var
  LRawContext: TIdContext;
  LIOHandler: TIdIOHandler;
  LConnection: TSSEConnection;
  LChannel: string;
  LEncoding: IIdTextEncoding;
  LLastHeartbeat: Cardinal;
  LNow: Cardinal;
  LNextInterval: Integer;
  LWaitResult: TWaitResult;

  procedure FlushQueue;
  var
    LItems: TArray<TSSEQueueItem>;
    LItem: TSSEQueueItem;
    LDataLines: TArray<string>;
    LLine: string;
  begin
    LItems := LConnection.DequeueAll;
    if Length(LItems) = 0 then
      Exit;
    LIOHandler.WriteBufferOpen;
    try
      for LItem in LItems do
      begin
        case LItem.Kind of
          ssMessage:
          begin
            if not LItem.Message.Id.IsEmpty then
            begin
              LIOHandler.Write('id: ' + LItem.Message.Id + LF, LEncoding);
              LConnection.LastEventId := LItem.Message.Id;
            end;
            if not LItem.Message.Event.IsEmpty then
              LIOHandler.Write('event: ' + LItem.Message.Event + LF, LEncoding);
            LDataLines := LItem.Message.Data.Split([#10]);
            for LLine in LDataLines do
              LIOHandler.Write('data: ' + LLine + LF, LEncoding);
            LIOHandler.Write(LF, LEncoding);
          end;
          ssComment:
            LIOHandler.Write(': ' + LItem.Comment + LF + LF, LEncoding);
          ssDisconnect:
            LConnection.MarkDisconnected;
        end;
      end;
    finally
      LIOHandler.WriteBufferClose;
    end;
    LLastHeartbeat := TThread.GetTickCount;
  end;

begin
  inherited;
  if not (Context.Response.RawWebResponse is TIdHTTPAppResponse) then
    raise EMVCException.Create(HTTP_STATUS.InternalServerError,
      ClassName + ' can only be used with INDY based application server');

  LRawContext := TIdHTTPAppResponseAccess(Context.Response.RawWebResponse).FThread;
  LIOHandler := LRawContext.Connection.IOHandler;
  LEncoding := IndyTextEncoding(Charset);
  LChannel := ChannelName;

  // Write HTTP headers
  LIOHandler.WriteBufferOpen;
  try
    LIOHandler.Write('HTTP/1.1 200 OK' + CRLF, LEncoding);
    LIOHandler.Write('Content-Type: text/event-stream; charset=' + Charset + CRLF, LEncoding);
    LIOHandler.Write('Cache-Control: no-cache' + CRLF, LEncoding);
    LIOHandler.Write('Connection: keep-alive' + CRLF, LEncoding);
    LIOHandler.Write('X-Accel-Buffering: no' + CRLF, LEncoding);
    LIOHandler.Write(CRLF, LEncoding);
  finally
    LIOHandler.WriteBufferClose;
  end;

  LIOHandler.Write('retry: ' + IntToStr(RetryTimeout) + LF + LF, LEncoding);
  LIOHandler.Write(': ok' + LF + LF, LEncoding);

  LConnection := TSSEConnection.Create(TGUID.NewGuid.ToString);
  try
    LConnection.LastEventId := Context.Request.Headers[TMVCConstants.SSE_LAST_EVENT_ID].Trim;
    SSEBroker.RegisterConnection(LChannel, LConnection);
    try
      OnClientConnected(LConnection);
      try
        LLastHeartbeat := TThread.GetTickCount;
        LNextInterval := Interval;

        while LRawContext.Connection.Connected
          and LConnection.IsConnected
          and (not IsShuttingDown) do
        begin
          // 1. Wait for broker messages or timeout
          LWaitResult := LConnection.WaitForData(LNextInterval);

          // 2. Flush any broker-pushed messages
          FlushQueue;

          // 3. On timeout, call OnInterval and flush its messages
          if LWaitResult = wrTimeout then
          begin
            LNextInterval := Interval;
            OnInterval(LConnection, LNextInterval);
            FlushQueue;
          end;

          // 4. Heartbeat if needed
          LNow := TThread.GetTickCount;
          if (LNow - LLastHeartbeat) >= Cardinal(HeartbeatInterval) then
          begin
            try
              LIOHandler.Write(': heartbeat' + LF + LF, LEncoding);
              LLastHeartbeat := LNow;
            except
              Break;
            end;
          end;
        end;
      except
        on E: Exception do
          LogW('SSE connection error for client %s: %s', [LConnection.ClientId, E.Message], 'sse');
      end;

      try
        OnClientDisconnected(LConnection);
      except
        on E: Exception do
          LogW('SSE OnClientDisconnected error for client %s: %s', [LConnection.ClientId, E.Message], 'sse');
      end;
    finally
      SSEBroker.UnregisterConnection(LChannel, LConnection);
    end;
  finally
    LConnection.Free;
  end;
end;

end.
