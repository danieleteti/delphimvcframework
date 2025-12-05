unit MVCFramework.WebSocket.Server;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.SyncObjs,
  IdContext,
  IdCustomTCPServer,
  IdTCPServer,
  IdIOHandler,
  IdGlobal,
  MVCFramework.WebSocket;

type
  /// <summary>
  /// Exception raised to terminate a WebSocket connection
  /// Can be raised from any event handler (OnMessage, OnClientConnect, etc.)
  /// </summary>
  EMVCWebSocketDisconnect = class(Exception);

  // Forward declarations
  TWebSocketClient = class;
  TMVCWebSocketServer = class;

  /// <summary>
  /// Event types for WebSocket server callbacks
  /// </summary>
  TWebSocketLogEvent = reference to procedure(const AMessage: string);
  TWebSocketMessageEvent = reference to procedure(AClient: TWebSocketClient; const AMessage: string);
  TWebSocketBinaryEvent = reference to procedure(AClient: TWebSocketClient; const AData: TBytes);
  TWebSocketErrorEvent = reference to procedure(AClient: TWebSocketClient; const AError: string);
  /// <summary>
  /// Called when client connects after successful handshake
  /// AClient.Data: Assign your custom object here, will be freed automatically on disconnect
  /// </summary>
  TWebSocketClientConnectEvent = reference to procedure(AClient: TWebSocketClient);
  /// <summary>
  /// Called when client disconnects
  /// AClient.Data is still available here, will be freed after this event
  /// </summary>
  TWebSocketClientDisconnectEvent = reference to procedure(AClient: TWebSocketClient);
  /// <summary>
  /// Called periodically (based on PeriodicMessageInterval) to generate custom messages to send to client
  /// AClient.Data: Your custom object (set in OnClientConnect)
  /// Return empty string to skip sending
  /// </summary>
  TWebSocketPeriodicMessageEvent = reference to procedure(AClient: TWebSocketClient; out AMessage: string);

  /// <summary>
  /// Represents a connected WebSocket client
  /// </summary>
  TWebSocketClient = class
  private
    FContext: TIdContext;
    FClientId: string;
    FUsername: string;
    FGroups: TArray<string>;
    FData: TObject;
    FOwnsData: Boolean;
    FPeriodicInterval: Integer;
    FServer: TMVCWebSocketServer;
    function GetConnectedUsersCount: Integer;
  public
    constructor Create(AContext: TIdContext; const AClientId: string; AServer: TMVCWebSocketServer; AOwnsData: Boolean = True);
    destructor Destroy; override;

    /// <summary>
    /// Send text message to this client
    /// </summary>
    procedure SendText(const AMessage: string);

    /// <summary>
    /// Send binary data to this client
    /// </summary>
    procedure SendBinary(const AData: TBytes);

    /// <summary>
    /// Broadcast text message to all connected clients (including self)
    /// </summary>
    procedure Broadcast(const AMessage: string);

    /// <summary>
    /// Broadcast text message to all connected clients except self
    /// </summary>
    procedure BroadcastToPeers(const AMessage: string);

    /// <summary>
    /// Send text message to a specific user by username
    /// Returns True if user was found and message sent
    /// </summary>
    function SendTo(const AUsername: string; const AMessage: string): Boolean;

    /// <summary>
    /// Send text message to all users in a specific group
    /// Returns number of users who received the message
    /// </summary>
    function SendToGroup(const AGroupName: string; const AMessage: string): Integer;

    /// <summary>
    /// Add this client to a group
    /// </summary>
    procedure JoinGroup(const AGroupName: string);

    /// <summary>
    /// Remove this client from a group
    /// </summary>
    procedure LeaveGroup(const AGroupName: string);

    /// <summary>
    /// Check if this client is in a specific group
    /// </summary>
    function IsInGroup(const AGroupName: string): Boolean;

    /// <summary>
    /// Disconnect this client
    /// Raises EMVCWebSocketDisconnect exception to terminate the connection cleanly
    /// </summary>
    procedure Disconnect(const AReason: string = '');

    property Context: TIdContext read FContext;
    property ClientId: string read FClientId;

    /// <summary>
    /// Username for this client (can be set in OnClientConnect)
    /// If not set, defaults to ClientId (IP address)
    /// </summary>
    property Username: string read FUsername write FUsername;

    /// <summary>
    /// Groups this client belongs to
    /// </summary>
    property Groups: TArray<string> read FGroups;

    /// <summary>
    /// Total number of connected users
    /// </summary>
    property ConnectedUsersCount: Integer read GetConnectedUsersCount;

    /// <summary>
    /// Custom session data for this client
    /// Set this in OnClientConnect event
    /// If OwnsData is True (default), it will be freed automatically when client disconnects
    /// </summary>
    property Data: TObject read FData write FData;
    property OwnsData: Boolean read FOwnsData write FOwnsData;

    /// <summary>
    /// Periodic message interval in milliseconds for this specific client
    /// 0 = disabled, uses server's default PeriodicMessageInterval if not set
    /// Can be modified in OnClientConnect or OnPeriodicMessage events
    /// </summary>
    property PeriodicInterval: Integer read FPeriodicInterval write FPeriodicInterval;
  end;

  /// <summary>
  /// Unified WebSocket server
  /// Supports both simple Echo and advanced Broadcast patterns
  /// Use BroadcastText/BroadcastBinary methods for chat/notification scenarios
  /// Or simply echo back in OnMessage handler for request/response scenarios
  /// </summary>
  TMVCWebSocketServer = class(TIdTCPServer)
  private
    FClients: TObjectList<TWebSocketClient>;
    FClientsLock: TCriticalSection;
    FOnLog: TWebSocketLogEvent;
    FOnMessage: TWebSocketMessageEvent;
    FOnBinaryData: TWebSocketBinaryEvent;
    FOnError: TWebSocketErrorEvent;
    FOnClientConnect: TWebSocketClientConnectEvent;
    FOnClientDisconnect: TWebSocketClientDisconnectEvent;
    FOnPeriodicMessage: TWebSocketPeriodicMessageEvent;
    FPeriodicMessageInterval: Integer;
    procedure OnExecuteEvent(AContext: TIdContext);
    function PerformHandshake(AContext: TIdContext; out AClientId: string): Boolean;
    procedure ProcessFrames(AClient: TWebSocketClient);
    function GetClientCount: Integer;
  protected
    procedure DoLog(const AMessage: string); virtual;
    procedure DoMessage(AClient: TWebSocketClient; const AMessage: string); virtual;
    procedure DoBinaryData(AClient: TWebSocketClient; const AData: TBytes); virtual;
    procedure DoError(AClient: TWebSocketClient; const AError: string); virtual;
    procedure DoClientConnect(AClient: TWebSocketClient); virtual;
    procedure DoClientDisconnect(AClient: TWebSocketClient); virtual;
    /// <summary>
    /// Override this to handle incoming text messages
    /// Default: does nothing (use OnMessage event instead)
    /// </summary>
    procedure HandleTextMessage(AClient: TWebSocketClient; const AMessage: string); virtual;
    /// <summary>
    /// Override this to handle incoming binary data
    /// Default: does nothing (use OnBinaryData event instead)
    /// </summary>
    procedure HandleBinaryData(AClient: TWebSocketClient; const AData: TBytes); virtual;
  public
    constructor Create(APort: Integer = 9091); reintroduce;
    destructor Destroy; override;

    /// <summary>
    /// Default interval in milliseconds for periodic messages (0 = disabled, default = 0)
    /// Set to > 0 to enable periodic message sending (e.g., 5000 for every 5 seconds)
    /// This is the default for all clients, but can be customized per-client in OnClientConnect
    /// </summary>
    property PeriodicMessageInterval: Integer read FPeriodicMessageInterval write FPeriodicMessageInterval;

    /// <summary>
    /// Broadcast text message to all connected clients (optionally exclude one)
    /// </summary>
    procedure BroadcastText(const AMessage: string; AExcludeClient: TWebSocketClient = nil);

    /// <summary>
    /// Broadcast binary data to all connected clients (optionally exclude one)
    /// </summary>
    procedure BroadcastBinary(const AData: TBytes; AExcludeClient: TWebSocketClient = nil);

    /// <summary>
    /// Send text message to specific client
    /// </summary>
    procedure SendTextToClient(AClient: TWebSocketClient; const AMessage: string);

    /// <summary>
    /// Send binary data to specific client
    /// </summary>
    procedure SendBinaryToClient(AClient: TWebSocketClient; const AData: TBytes);

    /// <summary>
    /// Find a client by username
    /// Returns nil if not found
    /// </summary>
    function FindClientByUsername(const AUsername: string): TWebSocketClient;

    /// <summary>
    /// Get all clients in a specific group
    /// </summary>
    function GetClientsByGroup(const AGroupName: string): TArray<TWebSocketClient>;

    /// <summary>
    /// Get all connected usernames
    /// </summary>
    function GetConnectedUsernames: TArray<string>;

    property ClientCount: Integer read GetClientCount;
    property OnLog: TWebSocketLogEvent read FOnLog write FOnLog;
    property OnMessage: TWebSocketMessageEvent read FOnMessage write FOnMessage;
    property OnBinaryData: TWebSocketBinaryEvent read FOnBinaryData write FOnBinaryData;
    property OnError: TWebSocketErrorEvent read FOnError write FOnError;
    property OnClientConnect: TWebSocketClientConnectEvent read FOnClientConnect write FOnClientConnect;
    property OnClientDisconnect: TWebSocketClientDisconnectEvent read FOnClientDisconnect write FOnClientDisconnect;
    property OnPeriodicMessage: TWebSocketPeriodicMessageEvent read FOnPeriodicMessage write FOnPeriodicMessage;
  end;

implementation

uses
  System.DateUtils,
  System.Math;

{ TWebSocketClient }

constructor TWebSocketClient.Create(AContext: TIdContext; const AClientId: string; AServer: TMVCWebSocketServer; AOwnsData: Boolean);
begin
  inherited Create;
  FContext := AContext;
  FClientId := AClientId;
  FUsername := AClientId; // Default username is ClientId (IP address)
  FServer := AServer;
  FData := nil;
  FOwnsData := AOwnsData;
  FPeriodicInterval := 0; // Will use server default
  SetLength(FGroups, 0); // Empty groups array
end;

destructor TWebSocketClient.Destroy;
begin
  if FOwnsData and Assigned(FData) then
    FData.Free;
  inherited;
end;

procedure TWebSocketClient.SendText(const AMessage: string);
begin
  if Assigned(FServer) then
    FServer.SendTextToClient(Self, AMessage);
end;

procedure TWebSocketClient.SendBinary(const AData: TBytes);
begin
  if Assigned(FServer) then
    FServer.SendBinaryToClient(Self, AData);
end;

procedure TWebSocketClient.Broadcast(const AMessage: string);
begin
  if Assigned(FServer) then
    FServer.BroadcastText(AMessage);
end;

procedure TWebSocketClient.BroadcastToPeers(const AMessage: string);
begin
  if Assigned(FServer) then
    FServer.BroadcastText(AMessage, Self);
end;

function TWebSocketClient.SendTo(const AUsername: string; const AMessage: string): Boolean;
var
  LTargetClient: TWebSocketClient;
begin
  Result := False;
  if Assigned(FServer) then
  begin
    LTargetClient := FServer.FindClientByUsername(AUsername);
    if Assigned(LTargetClient) then
    begin
      FServer.SendTextToClient(LTargetClient, AMessage);
      Result := True;
    end;
  end;
end;

function TWebSocketClient.SendToGroup(const AGroupName: string; const AMessage: string): Integer;
var
  LClients: TArray<TWebSocketClient>;
  LClient: TWebSocketClient;
begin
  Result := 0;
  if Assigned(FServer) then
  begin
    LClients := FServer.GetClientsByGroup(AGroupName);
    for LClient in LClients do
    begin
      FServer.SendTextToClient(LClient, AMessage);
      Inc(Result);
    end;
  end;
end;

procedure TWebSocketClient.JoinGroup(const AGroupName: string);
var
  I: Integer;
begin
  // Check if already in group
  for I := 0 to Length(FGroups) - 1 do
  begin
    if SameText(FGroups[I], AGroupName) then
      Exit; // Already in group
  end;

  // Add to group
  SetLength(FGroups, Length(FGroups) + 1);
  FGroups[Length(FGroups) - 1] := AGroupName;
end;

procedure TWebSocketClient.LeaveGroup(const AGroupName: string);
var
  I, J: Integer;
begin
  for I := 0 to Length(FGroups) - 1 do
  begin
    if SameText(FGroups[I], AGroupName) then
    begin
      // Remove this group by shifting array
      for J := I to Length(FGroups) - 2 do
        FGroups[J] := FGroups[J + 1];
      SetLength(FGroups, Length(FGroups) - 1);
      Exit;
    end;
  end;
end;

function TWebSocketClient.IsInGroup(const AGroupName: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Length(FGroups) - 1 do
  begin
    if SameText(FGroups[I], AGroupName) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TWebSocketClient.GetConnectedUsersCount: Integer;
begin
  if Assigned(FServer) then
    Result := FServer.ClientCount
  else
    Result := 0;
end;

procedure TWebSocketClient.Disconnect(const AReason: string);
begin
  if AReason.IsEmpty then
    raise EMVCWebSocketDisconnect.Create('Client disconnected')
  else
    raise EMVCWebSocketDisconnect.Create(AReason);
end;

{ TMVCWebSocketServer }

constructor TMVCWebSocketServer.Create(APort: Integer);
begin
  inherited Create(nil);
  DefaultPort := APort;
  OnExecute := OnExecuteEvent;
  FClients := TObjectList<TWebSocketClient>.Create(True); // Owns objects
  FClientsLock := TCriticalSection.Create;
  FPeriodicMessageInterval := 0; // Disabled by default
end;

destructor TMVCWebSocketServer.Destroy;
begin
  FClientsLock.Free;
  FClients.Free;
  inherited;
end;

procedure TMVCWebSocketServer.DoLog(const AMessage: string);
begin
  if Assigned(FOnLog) then
    FOnLog(AMessage);
end;

procedure TMVCWebSocketServer.DoMessage(AClient: TWebSocketClient; const AMessage: string);
begin
  if Assigned(FOnMessage) then
    FOnMessage(AClient, AMessage);
end;

procedure TMVCWebSocketServer.DoBinaryData(AClient: TWebSocketClient; const AData: TBytes);
begin
  if Assigned(FOnBinaryData) then
    FOnBinaryData(AClient, AData);
end;

procedure TMVCWebSocketServer.DoError(AClient: TWebSocketClient; const AError: string);
begin
  if Assigned(FOnError) then
    FOnError(AClient, AError);
end;

procedure TMVCWebSocketServer.DoClientConnect(AClient: TWebSocketClient);
begin
  if Assigned(FOnClientConnect) then
    FOnClientConnect(AClient);
end;

procedure TMVCWebSocketServer.DoClientDisconnect(AClient: TWebSocketClient);
begin
  if Assigned(FOnClientDisconnect) then
    FOnClientDisconnect(AClient);
end;

procedure TMVCWebSocketServer.HandleTextMessage(AClient: TWebSocketClient; const AMessage: string);
begin
  // Default: do nothing (user should handle in OnMessage event or override this)
end;

procedure TMVCWebSocketServer.HandleBinaryData(AClient: TWebSocketClient; const AData: TBytes);
begin
  // Default: do nothing (user should handle in OnBinaryData event or override this)
end;

function TMVCWebSocketServer.GetClientCount: Integer;
begin
  FClientsLock.Enter;
  try
    Result := FClients.Count;
  finally
    FClientsLock.Leave;
  end;
end;

function TMVCWebSocketServer.PerformHandshake(AContext: TIdContext; out AClientId: string): Boolean;
var
  LRequestLine, LLine: string;
  LHeaders: TStringList;
  LWebSocketKey, LAcceptKey, LProtocol: string;
  LIOHandler: TIdIOHandler;
begin
  Result := False;
  LIOHandler := AContext.Connection.IOHandler;
  LHeaders := TStringList.Create;
  try
    // Read HTTP request
    LRequestLine := LIOHandler.ReadLn(IndyTextEncoding_UTF8);
    if not LRequestLine.StartsWith('GET ') then
      Exit;

    // Read all headers
    while True do
    begin
      LLine := LIOHandler.ReadLn(IndyTextEncoding_UTF8);
      if LLine = '' then
        Break;
      LHeaders.Add(LLine);
    end;

    // Find required headers
    LWebSocketKey := '';
    LProtocol := '';
    for LLine in LHeaders do
    begin
      if LLine.StartsWith('Sec-WebSocket-Key:', True) then
      begin
        LWebSocketKey := LLine.Substring(18).Trim;
      end
      else if LLine.StartsWith('Sec-WebSocket-Protocol:', True) then
      begin
        LProtocol := LLine.Substring(23).Trim;
      end;
    end;

    if LWebSocketKey.IsEmpty then
      Exit;

    // Calculate accept key
    LAcceptKey := TMVCWebSocketHandshake.CalculateAcceptKey(LWebSocketKey);

    // Extract client ID from protocol header (format: "chat-username")
    if not LProtocol.IsEmpty then
    begin
      if LProtocol.StartsWith('chat-') then
        AClientId := LProtocol.Substring(5) // Extract username after "chat-"
      else
        AClientId := LProtocol; // Use protocol as-is if not chat format
    end
    else
    begin
      // Fallback to IP if no protocol specified
      AClientId := AContext.Connection.Socket.Binding.PeerIP;
    end;

    // Send 101 Switching Protocols response
    LIOHandler.WriteLn('HTTP/1.1 101 Switching Protocols');
    LIOHandler.WriteLn('Upgrade: websocket');
    LIOHandler.WriteLn('Connection: Upgrade');
    LIOHandler.WriteLn('Sec-WebSocket-Accept: ' + LAcceptKey);

    // Echo back the protocol if client sent one
    if not LProtocol.IsEmpty then
      LIOHandler.WriteLn('Sec-WebSocket-Protocol: ' + LProtocol);

    LIOHandler.WriteLn(''); // Empty line to end headers

    // CRITICAL: Clear buffer after handshake
    LIOHandler.InputBuffer.Clear;

    Result := True;
  finally
    LHeaders.Free;
  end;
end;

procedure TMVCWebSocketServer.ProcessFrames(AClient: TWebSocketClient);
var
  LFrame: TMVCWebSocketFrame;
  LResponseFrame: TMVCWebSocketFrame;
  LIOHandler: TIdIOHandler;
  LMessage: string;
  LLastPeriodicMessageTime: TDateTime;
  LPeriodicMessage: string;
  LReadTimeout: Integer;
  LClientInterval: Integer;
begin
  LIOHandler := AClient.Context.Connection.IOHandler;
  LLastPeriodicMessageTime := Now;

  while AClient.Context.Connection.Connected do
  begin
    try
      // Get current interval for this client (can change during connection)
      // If client has specific interval set, use it, otherwise use server default
      if AClient.PeriodicInterval > 0 then
        LClientInterval := AClient.PeriodicInterval
      else
        LClientInterval := FPeriodicMessageInterval;

      // Update read timeout dynamically
      if LClientInterval > 0 then
        LReadTimeout := Min(100, LClientInterval div 10)
      else
        LReadTimeout := 100;

      // Check if we need to send periodic message
      if (LClientInterval > 0) and Assigned(FOnPeriodicMessage) and
         (MilliSecondsBetween(Now, LLastPeriodicMessageTime) >= LClientInterval) then
      begin
        // Call event with message passed by reference
        LPeriodicMessage := '';
        FOnPeriodicMessage(AClient, LPeriodicMessage);

        // If interval was changed inside the callback, update it
        AClient.PeriodicInterval := LClientInterval;

        if not LPeriodicMessage.IsEmpty then
        begin
          LResponseFrame := TMVCWebSocketFrameParser.CreateTextFrame(LPeriodicMessage, False);
          TMVCWebSocketFrameParser.WriteFrame(LIOHandler, LResponseFrame);
        end;
        LLastPeriodicMessageTime := Now;
      end;

      // Wait for data with dynamic timeout
      if not LIOHandler.InputBufferIsEmpty or LIOHandler.Readable(LReadTimeout) then
      begin
        // Read frame (server side, expecting masked frames from client)
        LFrame := TMVCWebSocketFrameParser.ParseFrame(LIOHandler, True);

        case LFrame.Opcode of
          TMVCWebSocketOpcode.Text:
          begin
            // Get text message
            LMessage := TEncoding.UTF8.GetString(LFrame.Payload);
            DoMessage(AClient, LMessage);

            // Handle message
            HandleTextMessage(AClient, LMessage);
          end;

          TMVCWebSocketOpcode.Binary:
          begin
            DoBinaryData(AClient, LFrame.Payload);

            // Handle binary data
            HandleBinaryData(AClient, LFrame.Payload);
          end;

          TMVCWebSocketOpcode.Close:
          begin
            DoLog(Format('Client %s closing connection', [AClient.ClientId]));

            // Echo close frame
            LResponseFrame := TMVCWebSocketFrameParser.CreateCloseFrame(
              TMVCWebSocketCloseCode.NormalClosure, 'Goodbye');
            TMVCWebSocketFrameParser.WriteFrame(LIOHandler, LResponseFrame);
            Break;
          end;

          TMVCWebSocketOpcode.Ping:
          begin
            DoLog(Format('Ping from %s', [AClient.ClientId]));

            // Respond with pong
            LResponseFrame := TMVCWebSocketFrameParser.CreatePongFrame(LFrame.Payload, False);
            TMVCWebSocketFrameParser.WriteFrame(LIOHandler, LResponseFrame);
          end;

          TMVCWebSocketOpcode.Pong:
          begin
            DoLog(Format('Pong from %s', [AClient.ClientId]));
          end;
        end;
      end;

    except
      on E: EMVCWebSocketDisconnect do
      begin
        // Client requested disconnection - terminate cleanly
        DoLog(Format('Client %s disconnected: %s', [AClient.ClientId, E.Message]));
        Break;
      end;
      on E: Exception do
      begin
        DoError(AClient, E.Message);
        Break;
      end;
    end;
  end;
end;

procedure TMVCWebSocketServer.OnExecuteEvent(AContext: TIdContext);
var
  LClientId: string;
  LClient: TWebSocketClient;
begin
  LClient := nil;
  try
    // Perform WebSocket handshake
    if PerformHandshake(AContext, LClientId) then
    begin
      DoLog(Format('WebSocket handshake successful for %s', [LClientId]));

      // Create client object
      LClient := TWebSocketClient.Create(AContext, LClientId, Self);

      // Add to clients list
      FClientsLock.Enter;
      try
        FClients.Add(LClient);
      finally
        FClientsLock.Leave;
      end;


      // Notify client connected
      DoClientConnect(LClient);
      // Process WebSocket frames
      ProcessFrames(LClient);
    end
    else
    begin
      DoLog(Format('Handshake failed for %s', [LClientId]));
    end;

  finally
    if LClient <> nil then
    begin
      DoLog(Format('Client disconnected: %s', [LClient.ClientId]));

      // Notify client disconnected
      DoClientDisconnect(LClient);

      // Remove from clients list (will be freed automatically)
      FClientsLock.Enter;
      try
        FClients.Remove(LClient);
      finally
        FClientsLock.Leave;
      end;
    end;
  end;
end;

procedure TMVCWebSocketServer.BroadcastText(const AMessage: string; AExcludeClient: TWebSocketClient);
var
  LClient: TWebSocketClient;
  LFrame: TMVCWebSocketFrame;
begin
  LFrame := TMVCWebSocketFrameParser.CreateTextFrame(AMessage, False);

  FClientsLock.Enter;
  try
    for LClient in FClients do
    begin
      if LClient <> AExcludeClient then
      begin
        try
          TMVCWebSocketFrameParser.WriteFrame(LClient.Context.Connection.IOHandler, LFrame);
        except
          on E: Exception do
            DoLog(Format('Error broadcasting to %s: %s', [LClient.ClientId, E.Message]));
        end;
      end;
    end;
  finally
    FClientsLock.Leave;
  end;
end;

procedure TMVCWebSocketServer.BroadcastBinary(const AData: TBytes; AExcludeClient: TWebSocketClient);
var
  LClient: TWebSocketClient;
  LFrame: TMVCWebSocketFrame;
begin
  LFrame := TMVCWebSocketFrameParser.CreateBinaryFrame(AData, False);

  FClientsLock.Enter;
  try
    for LClient in FClients do
    begin
      if LClient <> AExcludeClient then
      begin
        try
          TMVCWebSocketFrameParser.WriteFrame(LClient.Context.Connection.IOHandler, LFrame);
        except
          on E: Exception do
            DoLog(Format('Error broadcasting to %s: %s', [LClient.ClientId, E.Message]));
        end;
      end;
    end;
  finally
    FClientsLock.Leave;
  end;
end;

procedure TMVCWebSocketServer.SendTextToClient(AClient: TWebSocketClient; const AMessage: string);
var
  LFrame: TMVCWebSocketFrame;
begin
  LFrame := TMVCWebSocketFrameParser.CreateTextFrame(AMessage, False);
  TMVCWebSocketFrameParser.WriteFrame(AClient.Context.Connection.IOHandler, LFrame);
end;

procedure TMVCWebSocketServer.SendBinaryToClient(AClient: TWebSocketClient; const AData: TBytes);
var
  LFrame: TMVCWebSocketFrame;
begin
  LFrame := TMVCWebSocketFrameParser.CreateBinaryFrame(AData, False);
  TMVCWebSocketFrameParser.WriteFrame(AClient.Context.Connection.IOHandler, LFrame);
end;

function TMVCWebSocketServer.FindClientByUsername(const AUsername: string): TWebSocketClient;
var
  LClient: TWebSocketClient;
begin
  Result := nil;
  FClientsLock.Enter;
  try
    for LClient in FClients do
    begin
      if SameText(LClient.Username, AUsername) then
      begin
        Result := LClient;
        Exit;
      end;
    end;
  finally
    FClientsLock.Leave;
  end;
end;

function TMVCWebSocketServer.GetClientsByGroup(const AGroupName: string): TArray<TWebSocketClient>;
var
  LClient: TWebSocketClient;
  LList: TList<TWebSocketClient>;
begin
  LList := TList<TWebSocketClient>.Create;
  try
    FClientsLock.Enter;
    try
      for LClient in FClients do
      begin
        if LClient.IsInGroup(AGroupName) then
          LList.Add(LClient);
      end;
    finally
      FClientsLock.Leave;
    end;
    Result := LList.ToArray;
  finally
    LList.Free;
  end;
end;

function TMVCWebSocketServer.GetConnectedUsernames: TArray<string>;
var
  LClient: TWebSocketClient;
  LList: TList<string>;
begin
  LList := TList<string>.Create;
  try
    FClientsLock.Enter;
    try
      for LClient in FClients do
        LList.Add(LClient.Username);
    finally
      FClientsLock.Leave;
    end;
    Result := LList.ToArray;
  finally
    LList.Free;
  end;
end;

end.
