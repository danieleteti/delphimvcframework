// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2025 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.WebSocket.Controller;

{$I dmvcframework.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Logger,
  MVCFramework.WebSocket,
  MVCFramework.WebSocket.ConnectionManager,
  IdContext,
  IdHTTPWebBrokerBridge,
  IdIOHandler;

type
  /// <summary>
  /// Base controller for WebSocket endpoints
  /// Inherit from this class to handle WebSocket connections
  /// </summary>
  TMVCWebSocketController = class abstract(TMVCController)
  private
    FConnectionManager: TMVCWebSocketConnectionManager;
    FPingInterval: Integer;
    FMaxMessageSize: Int64;
    function GetMaxMessagesPerSecond: Integer;
    procedure SetMaxMessagesPerSecond(const Value: Integer);
    function GetMaxBytesPerSecond: Integer;
    procedure SetMaxBytesPerSecond(const Value: Integer);
    procedure HandleConnection(AContext: TIdContext);
    function PerformHandshake(AContext: TIdContext): Boolean;
    procedure ProcessFrames(AConnection: TMVCWebSocketConnection);
  protected
    /// <summary>
    /// Called when a new WebSocket connection is established
    /// Override this to handle connection setup
    /// </summary>
    procedure OnConnect(AConnection: TMVCWebSocketConnection); virtual;

    /// <summary>
    /// Called when a WebSocket connection is closed
    /// Override this to handle cleanup
    /// </summary>
    procedure OnDisconnect(AConnection: TMVCWebSocketConnection;
      ACode: TMVCWebSocketCloseCode; const AReason: string); virtual;

    /// <summary>
    /// Called when a text message is received
    /// Override this to handle text messages
    /// </summary>
    procedure OnTextMessage(AConnection: TMVCWebSocketConnection;
      const AMessage: string); virtual;

    /// <summary>
    /// Called when a binary message is received
    /// Override this to handle binary messages
    /// </summary>
    procedure OnBinaryMessage(AConnection: TMVCWebSocketConnection;
      const AData: TBytes); virtual;

    /// <summary>
    /// Called when a ping is received
    /// By default, automatically sends a pong response
    /// Override to customize behavior
    /// </summary>
    procedure OnPing(AConnection: TMVCWebSocketConnection;
      const AData: TBytes); virtual;

    /// <summary>
    /// Called when a pong is received
    /// Override to handle pong frames
    /// </summary>
    procedure OnPong(AConnection: TMVCWebSocketConnection;
      const AData: TBytes); virtual;

    /// <summary>
    /// Called when an error occurs
    /// Override to handle errors
    /// </summary>
    procedure OnError(AConnection: TMVCWebSocketConnection;
      const AError: Exception); virtual;

    /// <summary>
    /// Broadcast a text message to all connected clients
    /// </summary>
    procedure BroadcastText(const AMessage: string);

    /// <summary>
    /// Broadcast a binary message to all connected clients
    /// </summary>
    procedure BroadcastBinary(const AData: TBytes);

    /// <summary>
    /// Get all active connections
    /// </summary>
    function GetConnections: TArray<TMVCWebSocketConnection>;

    /// <summary>
    /// Get connection count
    /// </summary>
    function GetConnectionCount: Integer;

    /// <summary>
    /// Access to the connection manager
    /// </summary>
    property ConnectionManager: TMVCWebSocketConnectionManager read FConnectionManager;
  public
    constructor Create; override;
    destructor Destroy; override;

    /// <summary>
    /// Default endpoint for WebSocket connections
    /// Map this to your desired path using [MVCPath('/ws')]
    /// </summary>
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    procedure Index;

    /// <summary>
    /// Ping interval in seconds (0 = disabled)
    /// Default: 30 seconds
    /// </summary>
    property PingInterval: Integer read FPingInterval write FPingInterval;

    /// <summary>
    /// Maximum message size in bytes
    /// Default: 1MB
    /// </summary>
    property MaxMessageSize: Int64 read FMaxMessageSize write FMaxMessageSize;

    /// <summary>
    /// Maximum messages per second per connection (0 = disabled)
    /// Default: 30 messages/sec
    /// Set this property to customize rate limiting for your application
    /// Example: MaxMessagesPerSecond := 50; // Allow 50 messages per second
    /// </summary>
    property MaxMessagesPerSecond: Integer read GetMaxMessagesPerSecond write SetMaxMessagesPerSecond;

    /// <summary>
    /// Maximum bytes per second per connection (0 = disabled)
    /// Default: 102400 (100KB/sec)
    /// Set this property to customize bandwidth limiting for your application
    /// Example: MaxBytesPerSecond := 1048576; // Allow 1MB per second
    /// </summary>
    property MaxBytesPerSecond: Integer read GetMaxBytesPerSecond write SetMaxBytesPerSecond;
  end;

  /// <summary>
  /// Exception raised for WebSocket controller errors
  /// </summary>
  EMVCWebSocketControllerException = class(EMVCException);

implementation

uses
  IdGlobal,
  System.DateUtils;

type
  TIdHTTPAppResponseAccess = class(TIdHTTPAppResponse);

{ TMVCWebSocketController }

constructor TMVCWebSocketController.Create;
begin
  inherited Create;
  FConnectionManager := TMVCWebSocketConnectionManager.Create;
  FPingInterval := 30; // 30 seconds default
  FMaxMessageSize := 1024 * 1024; // 1MB default
  // Rate limiting is configured with defaults in TMVCWebSocketConnectionManager
  // (30 messages/sec, 100KB/sec)
end;

destructor TMVCWebSocketController.Destroy;
begin
  FConnectionManager.Free;
  inherited;
end;

procedure TMVCWebSocketController.Index;
var
  lRawContext: TIdContext;
begin
  inherited;

  // Verify we're running on Indy
  if not (Context.Response.RawWebResponse is TIdHTTPAppResponse) then
  begin
    raise EMVCWebSocketControllerException.Create(HTTP_STATUS.InternalServerError,
      ClassName + ' can only be used with INDY based application server');
  end;

  lRawContext := TIdHTTPAppResponseAccess(Context.Response.RawWebResponse).FThread;

  // Handle the WebSocket connection
  HandleConnection(lRawContext);
end;

function TMVCWebSocketController.PerformHandshake(AContext: TIdContext): Boolean;
var
  lUpgrade, lConnection, lWebSocketKey, lWebSocketVersion: string;
  lAcceptKey: string;
  lIOHandler: TIdIOHandler;
begin
  Result := False;

  // Read handshake headers from Context.Request
  lUpgrade := Context.Request.Headers['Upgrade'];
  lConnection := Context.Request.Headers['Connection'];
  lWebSocketKey := Context.Request.Headers['Sec-WebSocket-Key'];
  lWebSocketVersion := Context.Request.Headers['Sec-WebSocket-Version'];

  // Validate handshake
  if not TMVCWebSocketHandshake.IsValidHandshakeRequest(lUpgrade, lConnection, lWebSocketVersion) then
  begin
    Log.Error('Invalid WebSocket handshake request', 'WebSocket');
    Exit;
  end;

  if lWebSocketKey.IsEmpty then
  begin
    Log.Error('Missing Sec-WebSocket-Key header', 'WebSocket');
    Exit;
  end;

  // Calculate accept key
  lAcceptKey := TMVCWebSocketHandshake.CalculateAcceptKey(lWebSocketKey);

  // Send handshake response
  lIOHandler := AContext.Connection.IOHandler;
  lIOHandler.WriteBufferOpen;
  try
    lIOHandler.WriteLn('HTTP/1.1 101 Switching Protocols');
    lIOHandler.WriteLn('Upgrade: websocket');
    lIOHandler.WriteLn('Connection: Upgrade');
    lIOHandler.WriteLn('Sec-WebSocket-Accept: ' + lAcceptKey);

    // Optional: Support for subprotocols
    if Context.Request.Headers['Sec-WebSocket-Protocol'] <> '' then
    begin
      // For now, echo back the first protocol
      lIOHandler.WriteLn('Sec-WebSocket-Protocol: ' +
        Context.Request.Headers['Sec-WebSocket-Protocol']);
    end;

    lIOHandler.WriteLn; // Empty line to end headers
  finally
    lIOHandler.WriteBufferClose;
  end;

  // Clear any remaining data in the input buffer after handshake
  lIOHandler.InputBuffer.Clear;

  Log.Info('WebSocket handshake successful', 'WebSocket');
  Result := True;
end;

procedure TMVCWebSocketController.HandleConnection(AContext: TIdContext);
var
  lConnection: TMVCWebSocketConnection;
begin
  // Perform WebSocket handshake
  if not PerformHandshake(AContext) then
    Exit;

  // Create connection object
  lConnection := TMVCWebSocketConnection.Create(AContext, FConnectionManager);
  try
    FConnectionManager.AddConnection(lConnection);
    try
      Log.Info('WebSocket connection established: ' + lConnection.ConnectionId, 'WebSocket');

      // Notify connection
      try
        OnConnect(lConnection);
      except
        on E: Exception do
          Log.Error('Error in OnConnect: ' + E.Message, 'WebSocket');
      end;

      // Process frames
      ProcessFrames(lConnection);

    finally
      FConnectionManager.RemoveConnection(lConnection);
      Log.Info('WebSocket connection closed: ' + lConnection.ConnectionId, 'WebSocket');
    end;
  finally
    lConnection.Free;
  end;
end;

procedure TMVCWebSocketController.ProcessFrames(AConnection: TMVCWebSocketConnection);
var
  Frame: TMVCWebSocketFrame;
  MessageBuffer: TBytes;
  TextMessage: string;
  CloseCode: TMVCWebSocketCloseCode;
  CloseReason: string;
  LastPingTime: TDateTime;
begin
  MessageBuffer := nil;
  LastPingTime := Now;

  while AConnection.IsConnected do
  begin
    try
      // Send ping if interval elapsed
      if (FPingInterval > 0) and (SecondsBetween(Now, LastPingTime) >= FPingInterval) then
      begin
        AConnection.SendPing;
        LastPingTime := Now;
      end;

      // Check if data is available (with timeout)
      if not AConnection.IOHandler.InputBufferIsEmpty or
         AConnection.IOHandler.Readable(100) then
      begin
        // Read frame
        Frame := TMVCWebSocketFrameParser.ParseFrame(AConnection.IOHandler);
        AConnection.LastActivity := Now;

        // Validate message size
        if Int64(Length(MessageBuffer)) + Int64(Frame.PayloadLength) > FMaxMessageSize then
        begin
          AConnection.Close(TMVCWebSocketCloseCode.MessageTooBig, 'Message too large');
          Break;
        end;

        // Process frame based on opcode
        case Frame.Opcode of
          TMVCWebSocketOpcode.Continuation:
          begin
            // Append to message buffer
            MessageBuffer := MessageBuffer + Frame.Payload;

            // If this is the final fragment
            if Frame.Fin then
            begin
              // Determine if text or binary based on first frame
              // (This is simplified; proper implementation tracks first frame opcode)
              TextMessage := TEncoding.UTF8.GetString(MessageBuffer);
              try
                OnTextMessage(AConnection, TextMessage);
              except
                on E: Exception do
                  OnError(AConnection, E);
              end;
              SetLength(MessageBuffer, 0);
            end;
          end;

          TMVCWebSocketOpcode.Text:
          begin
            if Frame.Fin then
            begin
              // Complete text message - validate UTF-8
              try
                TextMessage := TEncoding.UTF8.GetString(Frame.Payload);
                // RFC 6455 Section 8.1: Text frames MUST contain valid UTF-8
                // TEncoding.UTF8.GetString does basic validation
              except
                on E: Exception do
                begin
                  // Invalid UTF-8 encoding
                  AConnection.Close(TMVCWebSocketCloseCode.InvalidPayload, 'Invalid UTF-8 in text frame');
                  Break;
                end;
              end;

              try
                OnTextMessage(AConnection, TextMessage);
              except
                on E: Exception do
                  OnError(AConnection, E);
              end;
            end
            else
            begin
              // Start of fragmented message
              MessageBuffer := Frame.Payload;
            end;
          end;

          TMVCWebSocketOpcode.Binary:
          begin
            if Frame.Fin then
            begin
              // Complete binary message
              try
                OnBinaryMessage(AConnection, Frame.Payload);
              except
                on E: Exception do
                  OnError(AConnection, E);
              end;
            end
            else
            begin
              // Start of fragmented message
              MessageBuffer := Frame.Payload;
            end;
          end;

          TMVCWebSocketOpcode.Close:
          begin
            // Parse close frame
            if Length(Frame.Payload) >= 2 then
            begin
              CloseCode := TMVCWebSocketCloseCode((Frame.Payload[0] shl 8) or Frame.Payload[1]);
              if Length(Frame.Payload) > 2 then
                CloseReason := TEncoding.UTF8.GetString(Frame.Payload, 2, Length(Frame.Payload) - 2)
              else
                CloseReason := '';
            end
            else
            begin
              CloseCode := TMVCWebSocketCloseCode.NoStatusReceived;
              CloseReason := '';
            end;

            // Notify application
            try
              OnDisconnect(AConnection, CloseCode, CloseReason);
            except
              on E: Exception do
                Log.Error('Error in OnDisconnect: ' + E.Message, 'WebSocket');
            end;

            // Echo close frame back
            AConnection.Close(CloseCode, CloseReason);
            Break;
          end;

          TMVCWebSocketOpcode.Ping:
          begin
            try
              OnPing(AConnection, Frame.Payload);
            except
              on E: Exception do
                OnError(AConnection, E);
            end;
          end;

          TMVCWebSocketOpcode.Pong:
          begin
            try
              OnPong(AConnection, Frame.Payload);
            except
              on E: Exception do
                OnError(AConnection, E);
            end;
          end;

        else
          // Unknown opcode
          AConnection.Close(TMVCWebSocketCloseCode.ProtocolError, 'Unknown opcode');
          Break;
        end;
      end;

    except
      on E: Exception do
      begin
        Log.Error('Error processing frame: ' + E.Message, 'WebSocket');
        OnError(AConnection, E);
        Break;
      end;
    end;
  end;
end;

procedure TMVCWebSocketController.OnConnect(AConnection: TMVCWebSocketConnection);
begin
  // Override in descendants
end;

procedure TMVCWebSocketController.OnDisconnect(AConnection: TMVCWebSocketConnection;
  ACode: TMVCWebSocketCloseCode; const AReason: string);
begin
  // Override in descendants
end;

procedure TMVCWebSocketController.OnTextMessage(AConnection: TMVCWebSocketConnection;
  const AMessage: string);
begin
  // Override in descendants
end;

procedure TMVCWebSocketController.OnBinaryMessage(AConnection: TMVCWebSocketConnection;
  const AData: TBytes);
begin
  // Override in descendants
end;

procedure TMVCWebSocketController.OnPing(AConnection: TMVCWebSocketConnection;
  const AData: TBytes);
begin
  // Default: Send pong response
  AConnection.SendPong(AData);
end;

procedure TMVCWebSocketController.OnPong(AConnection: TMVCWebSocketConnection;
  const AData: TBytes);
begin
  // Override in descendants if needed
end;

procedure TMVCWebSocketController.OnError(AConnection: TMVCWebSocketConnection;
  const AError: Exception);
begin
  Log.Error('WebSocket error: ' + AError.Message, 'WebSocket');
end;

procedure TMVCWebSocketController.BroadcastText(const AMessage: string);
begin
  FConnectionManager.BroadcastText(AMessage);
end;

procedure TMVCWebSocketController.BroadcastBinary(const AData: TBytes);
begin
  FConnectionManager.BroadcastBinary(AData);
end;

function TMVCWebSocketController.GetConnections: TArray<TMVCWebSocketConnection>;
begin
  Result := FConnectionManager.GetAllConnections;
end;

function TMVCWebSocketController.GetConnectionCount: Integer;
begin
  Result := FConnectionManager.GetConnectionCount;
end;

function TMVCWebSocketController.GetMaxMessagesPerSecond: Integer;
begin
  Result := FConnectionManager.MaxMessagesPerSecond;
end;

procedure TMVCWebSocketController.SetMaxMessagesPerSecond(const Value: Integer);
begin
  FConnectionManager.MaxMessagesPerSecond := Value;
end;

function TMVCWebSocketController.GetMaxBytesPerSecond: Integer;
begin
  Result := FConnectionManager.MaxBytesPerSecond;
end;

procedure TMVCWebSocketController.SetMaxBytesPerSecond(const Value: Integer);
begin
  FConnectionManager.MaxBytesPerSecond := Value;
end;

end.
