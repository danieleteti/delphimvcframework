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

unit MVCFramework.WebSocket.ConnectionManager;

{$I dmvcframework.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  System.DateUtils,
  IdContext,
  IdIOHandler,
  MVCFramework.WebSocket,
  MVCFramework.WebSocket.RateLimiter;

type
  TMVCWebSocketConnection = class;
  TMVCWebSocketConnectionManager = class;

  /// <summary>
  /// Event triggered when a WebSocket connection receives a text message
  /// </summary>
  TMVCWebSocketTextMessageEvent = procedure(AConnection: TMVCWebSocketConnection;
    const AMessage: string) of object;

  /// <summary>
  /// Event triggered when a WebSocket connection receives a binary message
  /// </summary>
  TMVCWebSocketBinaryMessageEvent = procedure(AConnection: TMVCWebSocketConnection;
    const AData: TBytes) of object;

  /// <summary>
  /// Event triggered when a WebSocket connection is closed
  /// </summary>
  TMVCWebSocketCloseEvent = procedure(AConnection: TMVCWebSocketConnection;
    ACode: TMVCWebSocketCloseCode; const AReason: string) of object;

  /// <summary>
  /// Event triggered when a WebSocket connection has an error
  /// </summary>
  TMVCWebSocketErrorEvent = procedure(AConnection: TMVCWebSocketConnection;
    const AError: Exception) of object;

  /// <summary>
  /// Represents a single WebSocket connection
  /// </summary>
  TMVCWebSocketConnection = class
  private
    FConnectionId: string;
    FContext: TIdContext;
    FIOHandler: TIdIOHandler;
    FConnectedAt: TDateTime;
    FLastActivity: TDateTime;
    FUserData: TObject;
    FOwnsUserData: Boolean;
    FLock: TCriticalSection;
    FManager: TMVCWebSocketConnectionManager;
    FCustomData: TDictionary<string, string>;
    FRateLimiter: TMVCConnectionRateLimiter;  // RATE LIMITING SUPPORT

    procedure SetUserData(const Value: TObject);
  public
    constructor Create(AContext: TIdContext; AManager: TMVCWebSocketConnectionManager);
    destructor Destroy; override;

    /// <summary>
    /// Send a text message to the client
    /// </summary>
    procedure SendText(const AMessage: string);

    /// <summary>
    /// Send a binary message to the client
    /// </summary>
    procedure SendBinary(const AData: TBytes);

    /// <summary>
    /// Send a ping frame
    /// </summary>
    procedure SendPing(const AData: TBytes = nil);

    /// <summary>
    /// Send a pong frame
    /// </summary>
    procedure SendPong(const AData: TBytes = nil);

    /// <summary>
    /// Close the connection
    /// </summary>
    procedure Close(ACode: TMVCWebSocketCloseCode = TMVCWebSocketCloseCode.NormalClosure;
      const AReason: string = '');

    /// <summary>
    /// Check if the connection is still connected
    /// </summary>
    function IsConnected: Boolean;

    /// <summary>
    /// Unique identifier for this connection
    /// </summary>
    property ConnectionId: string read FConnectionId;

    /// <summary>
    /// When the connection was established
    /// </summary>
    property ConnectedAt: TDateTime read FConnectedAt;

    /// <summary>
    /// Last activity timestamp
    /// </summary>
    property LastActivity: TDateTime read FLastActivity write FLastActivity;

    /// <summary>
    /// Custom user data associated with this connection
    /// </summary>
    property UserData: TObject read FUserData write SetUserData;

    /// <summary>
    /// Whether this connection owns and should free the UserData object
    /// </summary>
    property OwnsUserData: Boolean read FOwnsUserData write FOwnsUserData;

    /// <summary>
    /// Custom string-based data storage
    /// </summary>
    property CustomData: TDictionary<string, string> read FCustomData;

    /// <summary>
    /// Underlying Indy context
    /// </summary>
    property Context: TIdContext read FContext;

    /// <summary>
    /// Underlying IO handler
    /// </summary>
    property IOHandler: TIdIOHandler read FIOHandler;

    /// <summary>
    /// Rate limiter for this connection
    /// </summary>
    property RateLimiter: TMVCConnectionRateLimiter read FRateLimiter;
  end;

  /// <summary>
  /// Manages multiple WebSocket connections
  /// </summary>
  TMVCWebSocketConnectionManager = class
  private
    FConnections: TThreadList<TMVCWebSocketConnection>;
    FConnectionsById: TDictionary<string, TMVCWebSocketConnection>;
    FLock: TCriticalSection;
    FOnTextMessage: TMVCWebSocketTextMessageEvent;
    FOnBinaryMessage: TMVCWebSocketBinaryMessageEvent;
    FOnClose: TMVCWebSocketCloseEvent;
    FOnError: TMVCWebSocketErrorEvent;
    FMaxMessagesPerSecond: Integer;  // RATE LIMITING CONFIG
    FMaxBytesPerSecond: Integer;     // RATE LIMITING CONFIG
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// Add a connection to the manager
    /// </summary>
    procedure AddConnection(AConnection: TMVCWebSocketConnection);

    /// <summary>
    /// Remove a connection from the manager
    /// </summary>
    procedure RemoveConnection(AConnection: TMVCWebSocketConnection); overload;
    procedure RemoveConnection(const AConnectionId: string); overload;

    /// <summary>
    /// Get a connection by ID
    /// </summary>
    function GetConnection(const AConnectionId: string): TMVCWebSocketConnection;

    /// <summary>
    /// Get all active connections
    /// </summary>
    function GetAllConnections: TArray<TMVCWebSocketConnection>;

    /// <summary>
    /// Get count of active connections
    /// </summary>
    function GetConnectionCount: Integer;

    /// <summary>
    /// Broadcast a text message to all connections
    /// </summary>
    procedure BroadcastText(const AMessage: string);

    /// <summary>
    /// Broadcast a binary message to all connections
    /// </summary>
    procedure BroadcastBinary(const AData: TBytes);

    /// <summary>
    /// Close all connections
    /// </summary>
    procedure CloseAll(ACode: TMVCWebSocketCloseCode = TMVCWebSocketCloseCode.NormalClosure;
      const AReason: string = '');

    /// <summary>
    /// Remove stale connections (no activity for specified duration)
    /// </summary>
    procedure RemoveStaleConnections(ATimeoutMinutes: Integer = 5);

    /// <summary>
    /// Event handlers
    /// </summary>
    property OnTextMessage: TMVCWebSocketTextMessageEvent read FOnTextMessage write FOnTextMessage;
    property OnBinaryMessage: TMVCWebSocketBinaryMessageEvent read FOnBinaryMessage write FOnBinaryMessage;
    property OnClose: TMVCWebSocketCloseEvent read FOnClose write FOnClose;
    property OnError: TMVCWebSocketErrorEvent read FOnError write FOnError;

    /// <summary>
    /// Maximum messages per second per connection (default: 30)
    /// Set to 0 to disable rate limiting
    /// </summary>
    property MaxMessagesPerSecond: Integer read FMaxMessagesPerSecond write FMaxMessagesPerSecond;

    /// <summary>
    /// Maximum bytes per second per connection (default: 102400 = 100KB)
    /// Set to 0 to disable rate limiting
    /// </summary>
    property MaxBytesPerSecond: Integer read FMaxBytesPerSecond write FMaxBytesPerSecond;
  end;

implementation

uses
  IdGlobal,
  MVCFramework.Logger,
  LoggerPro;

{ TMVCWebSocketConnection }

constructor TMVCWebSocketConnection.Create(AContext: TIdContext;
  AManager: TMVCWebSocketConnectionManager);
begin
  inherited Create;
  FContext := AContext;
  FIOHandler := AContext.Connection.IOHandler;
  FConnectedAt := Now;
  FLastActivity := Now;
  // Generate a unique connection ID (UUID-like format)
  FConnectionId := Format('%.8x-%.4x-%.4x-%.4x-%.12x', [
    Cardinal(Random(High(Integer))),
    Word(Random(High(Word))),
    Word(Random(High(Word))),
    Word(Random(High(Word))),
    Cardinal(Random(High(Integer))) * Cardinal(Random(High(Integer)))
  ]);
  FLock := TCriticalSection.Create;
  FManager := AManager;
  FOwnsUserData := False;
  FCustomData := TDictionary<string, string>.Create;

  // Create rate limiter with manager's settings
  if (AManager.MaxMessagesPerSecond > 0) and (AManager.MaxBytesPerSecond > 0) then
  begin
    FRateLimiter := TMVCConnectionRateLimiter.Create(
      FConnectionId,
      AManager.MaxMessagesPerSecond,
      AManager.MaxBytesPerSecond
    );
  end
  else
    FRateLimiter := nil;  // Rate limiting disabled
end;

destructor TMVCWebSocketConnection.Destroy;
begin
  if Assigned(FRateLimiter) then
    FRateLimiter.Free;
  if FOwnsUserData and Assigned(FUserData) then
    FUserData.Free;
  FCustomData.Free;
  FLock.Free;
  inherited;
end;

procedure TMVCWebSocketConnection.SetUserData(const Value: TObject);
begin
  if FOwnsUserData and Assigned(FUserData) then
    FUserData.Free;
  FUserData := Value;
end;

procedure TMVCWebSocketConnection.SendText(const AMessage: string);
var
  lFrame: TMVCWebSocketFrame;
  lMessageBytes: TBytes;
  lMessageSize: Integer;
begin
  // Check rate limit if enabled
  if Assigned(FRateLimiter) then
  begin
    lMessageBytes := TEncoding.UTF8.GetBytes(AMessage);
    lMessageSize := Length(lMessageBytes);

    if not FRateLimiter.RecordMessage(lMessageSize) then
    begin
      // Rate limit exceeded
      Log.Warn(Format('Rate limit exceeded for connection %s (tried to send %d bytes)',
        [FConnectionId, lMessageSize]), 'RateLimiter');
      raise EMVCWebSocketException.Create('Rate limit exceeded');
    end;
  end;

  FLock.Enter;
  try
    lFrame := TMVCWebSocketFrameParser.CreateTextFrame(AMessage, False);
    TMVCWebSocketFrameParser.WriteFrame(FIOHandler, lFrame);
    // Flush the output buffer to ensure immediate delivery
    FIOHandler.WriteBufferFlush;
    FLastActivity := Now;
  finally
    FLock.Leave;
  end;
end;

procedure TMVCWebSocketConnection.SendBinary(const AData: TBytes);
var
  lFrame: TMVCWebSocketFrame;
begin
  // Check rate limit if enabled
  if Assigned(FRateLimiter) then
  begin
    if not FRateLimiter.RecordMessage(Length(AData)) then
    begin
      Log.Warn(Format('Rate limit exceeded for connection %s (tried to send %d bytes)',
        [FConnectionId, Length(AData)]), 'RateLimiter');
      raise EMVCWebSocketException.Create('Rate limit exceeded');
    end;
  end;

  FLock.Enter;
  try
    lFrame := TMVCWebSocketFrameParser.CreateBinaryFrame(AData, False);
    TMVCWebSocketFrameParser.WriteFrame(FIOHandler, lFrame);
    FIOHandler.WriteBufferFlush;
    FLastActivity := Now;
  finally
    FLock.Leave;
  end;
end;

procedure TMVCWebSocketConnection.SendPing(const AData: TBytes);
var
  lFrame: TMVCWebSocketFrame;
begin
  FLock.Enter;
  try
    lFrame := TMVCWebSocketFrameParser.CreatePingFrame(AData, False);
    TMVCWebSocketFrameParser.WriteFrame(FIOHandler, lFrame);
    FIOHandler.WriteBufferFlush;
    FLastActivity := Now;
  finally
    FLock.Leave;
  end;
end;

procedure TMVCWebSocketConnection.SendPong(const AData: TBytes);
var
  lFrame: TMVCWebSocketFrame;
begin
  FLock.Enter;
  try
    lFrame := TMVCWebSocketFrameParser.CreatePongFrame(AData, False);
    TMVCWebSocketFrameParser.WriteFrame(FIOHandler, lFrame);
    FIOHandler.WriteBufferFlush;
    FLastActivity := Now;
  finally
    FLock.Leave;
  end;
end;

procedure TMVCWebSocketConnection.Close(ACode: TMVCWebSocketCloseCode; const AReason: string);
var
  lFrame: TMVCWebSocketFrame;
begin
  FLock.Enter;
  try
    if IsConnected then
    begin
      lFrame := TMVCWebSocketFrameParser.CreateCloseFrame(ACode, AReason, False);
      TMVCWebSocketFrameParser.WriteFrame(FIOHandler, lFrame);
      FContext.Connection.Disconnect;
    end;
  finally
    FLock.Leave;
  end;
end;

function TMVCWebSocketConnection.IsConnected: Boolean;
begin
  Result := Assigned(FContext) and FContext.Connection.Connected;
end;

{ TMVCWebSocketConnectionManager }

constructor TMVCWebSocketConnectionManager.Create;
begin
  inherited Create;
  FConnections := TThreadList<TMVCWebSocketConnection>.Create;
  FConnectionsById := TDictionary<string, TMVCWebSocketConnection>.Create;
  FLock := TCriticalSection.Create;

  // Default rate limiting settings
  FMaxMessagesPerSecond := 30;      // 30 messages per second
  FMaxBytesPerSecond := 102400;     // 100KB per second
end;

destructor TMVCWebSocketConnectionManager.Destroy;
begin
  CloseAll;
  FConnections.Free;
  FLock.Enter;
  try
    FConnectionsById.Free;
  finally
    FLock.Leave;
  end;
  FLock.Free;
  inherited;
end;

procedure TMVCWebSocketConnectionManager.AddConnection(AConnection: TMVCWebSocketConnection);
begin
  FLock.Enter;
  try
    FConnections.Add(AConnection);
    FConnectionsById.Add(AConnection.ConnectionId, AConnection);
  finally
    FLock.Leave;
  end;
end;

procedure TMVCWebSocketConnectionManager.RemoveConnection(AConnection: TMVCWebSocketConnection);
begin
  if not Assigned(AConnection) then
    Exit;

  FLock.Enter;
  try
    FConnections.Remove(AConnection);
    FConnectionsById.Remove(AConnection.ConnectionId);
  finally
    FLock.Leave;
  end;
end;

procedure TMVCWebSocketConnectionManager.RemoveConnection(const AConnectionId: string);
var
  lConn: TMVCWebSocketConnection;
begin
  lConn := GetConnection(AConnectionId);
  if Assigned(lConn) then
    RemoveConnection(lConn);
end;

function TMVCWebSocketConnectionManager.GetConnection(const AConnectionId: string): TMVCWebSocketConnection;
begin
  FLock.Enter;
  try
    if not FConnectionsById.TryGetValue(AConnectionId, Result) then
      Result := nil;
  finally
    FLock.Leave;
  end;
end;

function TMVCWebSocketConnectionManager.GetAllConnections: TArray<TMVCWebSocketConnection>;
var
  lList: TList<TMVCWebSocketConnection>;
begin
  lList := FConnections.LockList;
  try
    Result := lList.ToArray;
  finally
    FConnections.UnlockList;
  end;
end;

function TMVCWebSocketConnectionManager.GetConnectionCount: Integer;
var
  lList: TList<TMVCWebSocketConnection>;
begin
  lList := FConnections.LockList;
  try
    Result := lList.Count;
  finally
    FConnections.UnlockList;
  end;
end;

procedure TMVCWebSocketConnectionManager.BroadcastText(const AMessage: string);
var
  lConnections: TArray<TMVCWebSocketConnection>;
  lConn: TMVCWebSocketConnection;
  lSuccessCount: Integer;
begin
  lSuccessCount := 0;
  lConnections := GetAllConnections;

  // Debug: Log broadcast attempt
  Log.Debug(Format('Broadcasting to %d connections', [Length(lConnections)]), 'WebSocket');

  for lConn in lConnections do
  begin
    try
      if lConn.IsConnected then
      begin
        Log.Debug('Sending to: ' + lConn.ConnectionId, 'WebSocket');
        lConn.SendText(AMessage);
        Inc(lSuccessCount);
      end
      else
      begin
        Log.Debug('Skipping disconnected: ' + lConn.ConnectionId, 'WebSocket');
      end;
    except
      on E: EMVCWebSocketException do
      begin
        if E.Message.Contains('Rate limit') then
        begin
          // Rate limit exceeded - log but continue
          Log.Warn('Rate limit exceeded for ' + lConn.ConnectionId, 'WebSocket');
        end
        else
        begin
          // Other WebSocket error
          Log.Error('Broadcast error to ' + lConn.ConnectionId + ': ' + E.Message, 'WebSocket');
        end;
      end;
      on E: Exception do
      begin
        // Log error but continue broadcasting to others
        Log.Error('Broadcast error to ' + lConn.ConnectionId + ': ' + E.Message, 'WebSocket');
      end;
    end;
  end;
  Log.Debug(Format('Broadcast completed: %d/%d successful', [lSuccessCount, Length(lConnections)]), 'WebSocket');
end;

procedure TMVCWebSocketConnectionManager.BroadcastBinary(const AData: TBytes);
var
  lConnections: TArray<TMVCWebSocketConnection>;
  lConn: TMVCWebSocketConnection;
begin
  lConnections := GetAllConnections;
  for lConn in lConnections do
  begin
    try
      if lConn.IsConnected then
        lConn.SendBinary(AData);
    except
      on E: EMVCWebSocketException do
      begin
        if E.Message.Contains('Rate limit') then
          Log.Warn('Rate limit exceeded for ' + lConn.ConnectionId, 'WebSocket');
      end;
      // Ignore other errors for individual connections
    end;
  end;
end;

procedure TMVCWebSocketConnectionManager.CloseAll(ACode: TMVCWebSocketCloseCode;
  const AReason: string);
var
  lConnections: TArray<TMVCWebSocketConnection>;
  lConn: TMVCWebSocketConnection;
begin
  lConnections := GetAllConnections;
  for lConn in lConnections do
  begin
    try
      lConn.Close(ACode, AReason);
    except
      // Ignore errors
    end;
  end;
end;

procedure TMVCWebSocketConnectionManager.RemoveStaleConnections(ATimeoutMinutes: Integer);
var
  lConnections: TArray<TMVCWebSocketConnection>;
  lConn: TMVCWebSocketConnection;
  lCutoff: TDateTime;
begin
  lCutoff := IncMinute(Now, -ATimeoutMinutes);
  lConnections := GetAllConnections;

  for lConn in lConnections do
  begin
    if (lConn.LastActivity < lCutoff) or (not lConn.IsConnected) then
    begin
      try
        if lConn.IsConnected then
          lConn.Close(TMVCWebSocketCloseCode.GoingAway, 'Timeout');
      except
        // Ignore errors
      end;
      RemoveConnection(lConn);
    end;
  end;
end;

end.
