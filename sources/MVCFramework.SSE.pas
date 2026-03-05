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

unit MVCFramework.SSE;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections;

type
  TSSEMessage = record
    Event: string;
    Data: string;
    Id: string;
    class function Create(const AEvent, AData: string; const AId: string = ''): TSSEMessage; static;
  end;

  TSSEQueueItemKind = (ssMessage, ssComment, ssDisconnect);

  TSSEQueueItem = record
    Kind: TSSEQueueItemKind;
    Message: TSSEMessage;
    Comment: string;
  end;

  TSSEConnection = class
  private
    FQueue: TList<TSSEQueueItem>;
    FLock: TCriticalSection;
    FEvent: TEvent;
    FConnected: Int64;
    FClientId: string;
    FGroups: TList<string>;
    FGroupsLock: TCriticalSection;
    FCustomData: TObject;
    FOwnsCustomData: Boolean;
    FLastEventId: string;
  public
    constructor Create(const AClientId: string);
    destructor Destroy; override;
    /// <summary>
    /// Enqueue an SSE message for delivery to this client. Thread-safe.
    /// Can be called from any thread.
    /// </summary>
    procedure Send(const AMessage: TSSEMessage);
    /// <summary>
    /// Enqueue a comment line (used for heartbeats). Thread-safe.
    /// </summary>
    procedure SendComment(const AText: string);
    /// <summary>
    /// Request disconnection of this client. Thread-safe.
    /// </summary>
    procedure Disconnect;
    /// <summary>
    /// Dequeue all pending items. Called only from the Indy I/O thread.
    /// </summary>
    function DequeueAll: TArray<TSSEQueueItem>;
    /// <summary>
    /// Wait for data to be available or timeout.
    /// Returns wrSignaled if data is ready, wrTimeout otherwise.
    /// </summary>
    function WaitForData(ATimeoutMS: Cardinal): TWaitResult;
    /// <summary>
    /// Returns True if this connection is still marked as connected.
    /// </summary>
    function IsConnected: Boolean;
    /// <summary>
    /// Mark this connection as disconnected.
    /// </summary>
    procedure MarkDisconnected;

    // Group management
    procedure JoinGroup(const AGroupName: string);
    procedure LeaveGroup(const AGroupName: string);
    function IsInGroup(const AGroupName: string): Boolean;
    function GetGroups: TArray<string>;

    property ClientId: string read FClientId;
    property LastEventId: string read FLastEventId write FLastEventId;
    property CustomData: TObject read FCustomData write FCustomData;
    property OwnsCustomData: Boolean read FOwnsCustomData write FOwnsCustomData;
  end;

  TMVCSSEBroker = class
  private
    FChannels: TObjectDictionary<string, TObjectList<TSSEConnection>>;
    FLock: TCriticalSection;
    class var FInstance: TMVCSSEBroker;
    function GetOrCreateChannel(const AChannel: string): TObjectList<TSSEConnection>;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// Register a connection to a channel. Called by the SSE controller
    /// when a client connects.
    /// </summary>
    procedure RegisterConnection(const AChannel: string; AConnection: TSSEConnection);
    /// <summary>
    /// Unregister a connection from a channel. Called by the SSE controller
    /// when a client disconnects. Does NOT free the connection.
    /// </summary>
    procedure UnregisterConnection(const AChannel: string; AConnection: TSSEConnection);

    /// <summary>
    /// Broadcast a message to all connections on a channel.
    /// Thread-safe, can be called from any thread.
    /// </summary>
    procedure Broadcast(const AChannel: string; const AMessage: TSSEMessage);
    /// <summary>
    /// Broadcast a message to connections in a specific group on a channel.
    /// </summary>
    procedure BroadcastToGroup(const AChannel, AGroup: string; const AMessage: TSSEMessage);
    /// <summary>
    /// Send a message to a specific client by ID on a channel.
    /// </summary>
    procedure SendTo(const AChannel, AClientId: string; const AMessage: TSSEMessage);

    /// <summary>
    /// Returns the number of active connections on a channel.
    /// </summary>
    function ConnectionCount(const AChannel: string): Integer;
    /// <summary>
    /// Returns all client IDs on a channel.
    /// </summary>
    function GetClientIds(const AChannel: string): TArray<string>;
    /// <summary>
    /// Returns all active channel names.
    /// </summary>
    function GetChannelNames: TArray<string>;

    class function Instance: TMVCSSEBroker;
  end;

/// <summary>
/// Shortcut to TMVCSSEBroker.Instance
/// </summary>
function SSEBroker: TMVCSSEBroker;

implementation

function SSEBroker: TMVCSSEBroker;
begin
  Result := TMVCSSEBroker.Instance;
end;

{ TSSEMessage }

class function TSSEMessage.Create(const AEvent, AData: string; const AId: string): TSSEMessage;
begin
  Result.Event := AEvent;
  Result.Data := AData;
  Result.Id := AId;
end;

{ TSSEConnection }

constructor TSSEConnection.Create(const AClientId: string);
begin
  inherited Create;
  FClientId := AClientId;
  FQueue := TList<TSSEQueueItem>.Create;
  FLock := TCriticalSection.Create;
  FEvent := TEvent.Create(nil, False, False, '');
  FConnected := 1;
  FGroups := TList<string>.Create;
  FGroupsLock := TCriticalSection.Create;
  FCustomData := nil;
  FOwnsCustomData := False;
  FLastEventId := '';
end;

destructor TSSEConnection.Destroy;
begin
  MarkDisconnected;
  if FOwnsCustomData then
    FCustomData.Free;
  FGroupsLock.Free;
  FGroups.Free;
  FEvent.Free;
  FLock.Free;
  FQueue.Free;
  inherited;
end;

procedure TSSEConnection.Send(const AMessage: TSSEMessage);
var
  LItem: TSSEQueueItem;
begin
  if not IsConnected then
    Exit;
  LItem.Kind := ssMessage;
  LItem.Message := AMessage;
  LItem.Comment := '';
  FLock.Enter;
  try
    FQueue.Add(LItem);
  finally
    FLock.Leave;
  end;
  FEvent.SetEvent;
end;

procedure TSSEConnection.SendComment(const AText: string);
var
  LItem: TSSEQueueItem;
begin
  if not IsConnected then
    Exit;
  LItem.Kind := ssComment;
  LItem.Comment := AText;
  FLock.Enter;
  try
    FQueue.Add(LItem);
  finally
    FLock.Leave;
  end;
  FEvent.SetEvent;
end;

procedure TSSEConnection.Disconnect;
var
  LItem: TSSEQueueItem;
begin
  LItem.Kind := ssDisconnect;
  LItem.Comment := '';
  FLock.Enter;
  try
    FQueue.Add(LItem);
  finally
    FLock.Leave;
  end;
  FEvent.SetEvent;
end;

function TSSEConnection.DequeueAll: TArray<TSSEQueueItem>;
begin
  FLock.Enter;
  try
    Result := FQueue.ToArray;
    FQueue.Clear;
  finally
    FLock.Leave;
  end;
end;

function TSSEConnection.WaitForData(ATimeoutMS: Cardinal): TWaitResult;
begin
  Result := FEvent.WaitFor(ATimeoutMS);
end;

function TSSEConnection.IsConnected: Boolean;
begin
  Result := TInterlocked.Read(FConnected) = 1;
end;

procedure TSSEConnection.MarkDisconnected;
begin
  TInterlocked.Exchange(FConnected, 0);
  FEvent.SetEvent; // Wake up any waiting thread
end;

procedure TSSEConnection.JoinGroup(const AGroupName: string);
begin
  FGroupsLock.Enter;
  try
    if not FGroups.Contains(AGroupName) then
      FGroups.Add(AGroupName);
  finally
    FGroupsLock.Leave;
  end;
end;

procedure TSSEConnection.LeaveGroup(const AGroupName: string);
begin
  FGroupsLock.Enter;
  try
    FGroups.Remove(AGroupName);
  finally
    FGroupsLock.Leave;
  end;
end;

function TSSEConnection.IsInGroup(const AGroupName: string): Boolean;
begin
  FGroupsLock.Enter;
  try
    Result := FGroups.Contains(AGroupName);
  finally
    FGroupsLock.Leave;
  end;
end;

function TSSEConnection.GetGroups: TArray<string>;
begin
  FGroupsLock.Enter;
  try
    Result := FGroups.ToArray;
  finally
    FGroupsLock.Leave;
  end;
end;

{ TMVCSSEBroker }

constructor TMVCSSEBroker.Create;
begin
  inherited Create;
  FChannels := TObjectDictionary<string, TObjectList<TSSEConnection>>.Create([doOwnsValues]);
  FLock := TCriticalSection.Create;
end;

destructor TMVCSSEBroker.Destroy;
begin
  FLock.Free;
  FChannels.Free;
  inherited;
end;

function TMVCSSEBroker.GetOrCreateChannel(const AChannel: string): TObjectList<TSSEConnection>;
begin
  // Caller must hold FLock
  if not FChannels.TryGetValue(AChannel, Result) then
  begin
    Result := TObjectList<TSSEConnection>.Create(False); // Does NOT own connections
    FChannels.Add(AChannel, Result);
  end;
end;

procedure TMVCSSEBroker.RegisterConnection(const AChannel: string; AConnection: TSSEConnection);
var
  LList: TObjectList<TSSEConnection>;
begin
  FLock.Enter;
  try
    LList := GetOrCreateChannel(AChannel);
    if not LList.Contains(AConnection) then
      LList.Add(AConnection);
  finally
    FLock.Leave;
  end;
end;

procedure TMVCSSEBroker.UnregisterConnection(const AChannel: string; AConnection: TSSEConnection);
var
  LList: TObjectList<TSSEConnection>;
begin
  FLock.Enter;
  try
    if FChannels.TryGetValue(AChannel, LList) then
    begin
      LList.Remove(AConnection);
      if LList.Count = 0 then
        FChannels.Remove(AChannel);
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TMVCSSEBroker.Broadcast(const AChannel: string; const AMessage: TSSEMessage);
var
  LList: TObjectList<TSSEConnection>;
  LConnections: TArray<TSSEConnection>;
  LConn: TSSEConnection;
begin
  FLock.Enter;
  try
    if not FChannels.TryGetValue(AChannel, LList) then
      Exit;
    LConnections := LList.ToArray;
  finally
    FLock.Leave;
  end;
  // Send outside the lock to avoid deadlocks
  for LConn in LConnections do
  begin
    if LConn.IsConnected then
      LConn.Send(AMessage);
  end;
end;

procedure TMVCSSEBroker.BroadcastToGroup(const AChannel, AGroup: string; const AMessage: TSSEMessage);
var
  LList: TObjectList<TSSEConnection>;
  LConnections: TArray<TSSEConnection>;
  LConn: TSSEConnection;
begin
  FLock.Enter;
  try
    if not FChannels.TryGetValue(AChannel, LList) then
      Exit;
    LConnections := LList.ToArray;
  finally
    FLock.Leave;
  end;
  for LConn in LConnections do
  begin
    if LConn.IsConnected and LConn.IsInGroup(AGroup) then
      LConn.Send(AMessage);
  end;
end;

procedure TMVCSSEBroker.SendTo(const AChannel, AClientId: string; const AMessage: TSSEMessage);
var
  LList: TObjectList<TSSEConnection>;
  LConnections: TArray<TSSEConnection>;
  LConn: TSSEConnection;
begin
  FLock.Enter;
  try
    if not FChannels.TryGetValue(AChannel, LList) then
      Exit;
    LConnections := LList.ToArray;
  finally
    FLock.Leave;
  end;
  for LConn in LConnections do
  begin
    if LConn.IsConnected and SameText(LConn.ClientId, AClientId) then
    begin
      LConn.Send(AMessage);
      Break;
    end;
  end;
end;

function TMVCSSEBroker.ConnectionCount(const AChannel: string): Integer;
var
  LList: TObjectList<TSSEConnection>;
begin
  FLock.Enter;
  try
    if FChannels.TryGetValue(AChannel, LList) then
      Result := LList.Count
    else
      Result := 0;
  finally
    FLock.Leave;
  end;
end;

function TMVCSSEBroker.GetClientIds(const AChannel: string): TArray<string>;
var
  LList: TObjectList<TSSEConnection>;
  I: Integer;
begin
  FLock.Enter;
  try
    if FChannels.TryGetValue(AChannel, LList) then
    begin
      SetLength(Result, LList.Count);
      for I := 0 to LList.Count - 1 do
        Result[I] := LList[I].ClientId;
    end
    else
      Result := [];
  finally
    FLock.Leave;
  end;
end;

function TMVCSSEBroker.GetChannelNames: TArray<string>;
var
  LKey: string;
  I: Integer;
begin
  FLock.Enter;
  try
    SetLength(Result, FChannels.Count);
    I := 0;
    for LKey in FChannels.Keys do
    begin
      Result[I] := LKey;
      Inc(I);
    end;
  finally
    FLock.Leave;
  end;
end;

class function TMVCSSEBroker.Instance: TMVCSSEBroker;
var
  LNewInstance: TMVCSSEBroker;
begin
  if FInstance = nil then
  begin
    LNewInstance := TMVCSSEBroker.Create;
    if TInterlocked.CompareExchange<TMVCSSEBroker>(FInstance, LNewInstance, nil) <> nil then
      LNewInstance.Free; // Another thread won the race
  end;
  Result := FInstance;
end;

initialization

finalization
  FreeAndNil(TMVCSSEBroker.FInstance);

end.
