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
  MVCFramework.WebSocket;

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
end;

destructor TMVCWebSocketConnection.Destroy;
begin
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
  Frame: TMVCWebSocketFrame;
begin
  FLock.Enter;
  try
    Frame := TMVCWebSocketFrameParser.CreateTextFrame(AMessage, False);
    TMVCWebSocketFrameParser.WriteFrame(FIOHandler, Frame);
    // Flush the output buffer to ensure immediate delivery
    FIOHandler.WriteBufferFlush;
    FLastActivity := Now;
  finally
    FLock.Leave;
  end;
end;

procedure TMVCWebSocketConnection.SendBinary(const AData: TBytes);
var
  Frame: TMVCWebSocketFrame;
begin
  FLock.Enter;
  try
    Frame := TMVCWebSocketFrameParser.CreateBinaryFrame(AData, False);
    TMVCWebSocketFrameParser.WriteFrame(FIOHandler, Frame);
    FIOHandler.WriteBufferFlush;
    FLastActivity := Now;
  finally
    FLock.Leave;
  end;
end;

procedure TMVCWebSocketConnection.SendPing(const AData: TBytes);
var
  Frame: TMVCWebSocketFrame;
begin
  FLock.Enter;
  try
    Frame := TMVCWebSocketFrameParser.CreatePingFrame(AData, False);
    TMVCWebSocketFrameParser.WriteFrame(FIOHandler, Frame);
    FIOHandler.WriteBufferFlush;
    FLastActivity := Now;
  finally
    FLock.Leave;
  end;
end;

procedure TMVCWebSocketConnection.SendPong(const AData: TBytes);
var
  Frame: TMVCWebSocketFrame;
begin
  FLock.Enter;
  try
    Frame := TMVCWebSocketFrameParser.CreatePongFrame(AData, False);
    TMVCWebSocketFrameParser.WriteFrame(FIOHandler, Frame);
    FIOHandler.WriteBufferFlush;
    FLastActivity := Now;
  finally
    FLock.Leave;
  end;
end;

procedure TMVCWebSocketConnection.Close(ACode: TMVCWebSocketCloseCode; const AReason: string);
var
  Frame: TMVCWebSocketFrame;
begin
  FLock.Enter;
  try
    if IsConnected then
    begin
      Frame := TMVCWebSocketFrameParser.CreateCloseFrame(ACode, AReason, False);
      TMVCWebSocketFrameParser.WriteFrame(FIOHandler, Frame);
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
  Conn: TMVCWebSocketConnection;
begin
  Conn := GetConnection(AConnectionId);
  if Assigned(Conn) then
    RemoveConnection(Conn);
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
  List: TList<TMVCWebSocketConnection>;
begin
  List := FConnections.LockList;
  try
    Result := List.ToArray;
  finally
    FConnections.UnlockList;
  end;
end;

function TMVCWebSocketConnectionManager.GetConnectionCount: Integer;
var
  List: TList<TMVCWebSocketConnection>;
begin
  List := FConnections.LockList;
  try
    Result := List.Count;
  finally
    FConnections.UnlockList;
  end;
end;

procedure TMVCWebSocketConnectionManager.BroadcastText(const AMessage: string);
var
  Connections: TArray<TMVCWebSocketConnection>;
  Conn: TMVCWebSocketConnection;
  SuccessCount: Integer;
begin
  SuccessCount := 0;
  Connections := GetAllConnections;

  // Debug: Log broadcast attempt
  Log.Debug(Format('Broadcasting to %d connections', [Length(Connections)]), 'WebSocket');

  for Conn in Connections do
  begin
    try
      if Conn.IsConnected then
      begin
        Log.Debug('Sending to: ' + Conn.ConnectionId, 'WebSocket');
        Conn.SendText(AMessage);
        Inc(SuccessCount);
      end
      else
      begin
        Log.Debug('Skipping disconnected: ' + Conn.ConnectionId, 'WebSocket');
      end;
    except
      on E: Exception do
      begin
        // Log error but continue broadcasting to others
        Log.Error('Broadcast error to ' + Conn.ConnectionId + ': ' + E.Message, 'WebSocket');
      end;
    end;
  end;
  Log.Debug(Format('Broadcast completed: %d/%d successful', [SuccessCount, Length(Connections)]), 'WebSocket');
end;

procedure TMVCWebSocketConnectionManager.BroadcastBinary(const AData: TBytes);
var
  Connections: TArray<TMVCWebSocketConnection>;
  Conn: TMVCWebSocketConnection;
begin
  Connections := GetAllConnections;
  for Conn in Connections do
  begin
    try
      if Conn.IsConnected then
        Conn.SendBinary(AData);
    except
      // Ignore errors for individual connections
    end;
  end;
end;

procedure TMVCWebSocketConnectionManager.CloseAll(ACode: TMVCWebSocketCloseCode;
  const AReason: string);
var
  Connections: TArray<TMVCWebSocketConnection>;
  Conn: TMVCWebSocketConnection;
begin
  Connections := GetAllConnections;
  for Conn in Connections do
  begin
    try
      Conn.Close(ACode, AReason);
    except
      // Ignore errors
    end;
  end;
end;

procedure TMVCWebSocketConnectionManager.RemoveStaleConnections(ATimeoutMinutes: Integer);
var
  Connections: TArray<TMVCWebSocketConnection>;
  Conn: TMVCWebSocketConnection;
  Cutoff: TDateTime;
begin
  Cutoff := IncMinute(Now, -ATimeoutMinutes);
  Connections := GetAllConnections;

  for Conn in Connections do
  begin
    if (Conn.LastActivity < Cutoff) or (not Conn.IsConnected) then
    begin
      try
        if Conn.IsConnected then
          Conn.Close(TMVCWebSocketCloseCode.GoingAway, 'Timeout');
      except
        // Ignore errors
      end;
      RemoveConnection(Conn);
    end;
  end;
end;

end.
