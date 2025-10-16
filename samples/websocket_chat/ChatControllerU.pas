unit ChatControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.WebSocket.Controller,
  MVCFramework.WebSocket.ConnectionManager,
  MVCFramework.WebSocket,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.DateUtils,
  JsonDataObjects;

var
  // Global shared chat history - must be shared across all controller instances
  GChatHistory: TThreadList<string>;
  // Global shared connection manager - CRITICAL: must be shared across all threads!
  GChatConnectionManager: TMVCWebSocketConnectionManager;

type
  /// <summary>
  /// Simple chat room implementation using WebSocket
  /// </summary>
  [MVCPath('/chat')]
  TChatController = class(TMVCWebSocketController)
  private
    procedure AddToHistory(const AMessage: string);
    function GetHistory: TArray<string>;
    // Use global connection manager instead of per-instance one
    procedure BroadcastTextGlobal(const AMessage: string);
    function GetConnectionCountGlobal: Integer;
  protected
    procedure OnConnect(AConnection: TMVCWebSocketConnection); override;
    procedure OnDisconnect(AConnection: TMVCWebSocketConnection;
      ACode: TMVCWebSocketCloseCode; const AReason: string); override;
    procedure OnTextMessage(AConnection: TMVCWebSocketConnection;
      const AMessage: string); override;
    procedure OnError(AConnection: TMVCWebSocketConnection;
      const AError: Exception); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    /// <summary>
    /// HTTP endpoint to get chat statistics
    /// </summary>
    [MVCPath('/stats')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(TMVCMediaType.APPLICATION_JSON)]
    procedure GetStats;

    /// <summary>
    /// HTTP endpoint to serve the chat HTML page
    /// </summary>
    [MVCPath('/page')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    procedure GetChatPage;
  end;

implementation

uses
  MVCFramework.Logger;

{ TChatController }

constructor TChatController.Create;
begin
  inherited Create;

  // Configure WebSocket settings
  PingInterval := 30; // Send ping every 30 seconds
  MaxMessageSize := 10 * 1024; // 10KB max message size
end;

procedure TChatController.BroadcastTextGlobal(const AMessage: string);
begin
  GChatConnectionManager.BroadcastText(AMessage);
end;

function TChatController.GetConnectionCountGlobal: Integer;
begin
  Result := GChatConnectionManager.GetConnectionCount;
end;

destructor TChatController.Destroy;
begin
  inherited;
end;

procedure TChatController.AddToHistory(const AMessage: string);
var
  List: TList<string>;
begin
  List := GChatHistory.LockList;
  try
    List.Add(AMessage);
    // Keep only last 100 messages
    while List.Count > 100 do
      List.Delete(0);
  finally
    GChatHistory.UnlockList;
  end;
end;

function TChatController.GetHistory: TArray<string>;
var
  List: TList<string>;
begin
  List := GChatHistory.LockList;
  try
    Result := List.ToArray;
  finally
    GChatHistory.UnlockList;
  end;
end;

procedure TChatController.OnConnect(AConnection: TMVCWebSocketConnection);
var
  WelcomeMsg: TJsonObject;
  History: TArray<string>;
  HistoryJson: TJsonArray;
  Msg: string;
  NotificationMsg: TJsonObject;
begin
  // NOTE: Do NOT call inherited - we manage connections in global manager
  // inherited would add to per-instance manager which is wrong

  // CRITICAL: Add connection to GLOBAL manager (not per-instance)
  GChatConnectionManager.AddConnection(AConnection);

  Log.Info('New chat connection: ' + AConnection.ConnectionId, 'Chat');

  // Send welcome message with history
  WelcomeMsg := TJsonObject.Create;
  try
    WelcomeMsg.S['type'] := 'welcome';
    WelcomeMsg.S['connectionId'] := AConnection.ConnectionId;
    WelcomeMsg.I['userCount'] := GetConnectionCountGlobal;
    WelcomeMsg.S['timestamp'] := DateTimeToStr(Now);

    // Add chat history
    History := GetHistory;
    HistoryJson := WelcomeMsg.A['history'];
    for Msg in History do
      HistoryJson.Add(Msg);

    AConnection.SendText(WelcomeMsg.ToJSON);
  finally
    WelcomeMsg.Free;
  end;

  // Notify other users
  NotificationMsg := TJsonObject.Create;
  try
    NotificationMsg.S['type'] := 'user_joined';
    NotificationMsg.S['connectionId'] := AConnection.ConnectionId;
    NotificationMsg.I['userCount'] := GetConnectionCountGlobal;
    NotificationMsg.S['timestamp'] := DateTimeToStr(Now);

    // Broadcast to all except the new connection
    BroadcastTextGlobal(NotificationMsg.ToJSON);
  finally
    NotificationMsg.Free;
  end;
end;

procedure TChatController.OnDisconnect(AConnection: TMVCWebSocketConnection;
  ACode: TMVCWebSocketCloseCode; const AReason: string);
var
  NotificationMsg: TJsonObject;
begin
  // NOTE: Do NOT call inherited - we manage connections in global manager

  // CRITICAL: Remove connection from GLOBAL manager
  GChatConnectionManager.RemoveConnection(AConnection);

  Log.Info(Format('Chat connection closed: %s (Code: %d, Reason: %s)',
    [AConnection.ConnectionId, Ord(ACode), AReason]), 'Chat');

  // Notify other users
  NotificationMsg := TJsonObject.Create;
  try
    NotificationMsg.S['type'] := 'user_left';
    NotificationMsg.S['connectionId'] := AConnection.ConnectionId;
    NotificationMsg.I['userCount'] := GetConnectionCountGlobal;
    NotificationMsg.S['timestamp'] := DateTimeToStr(Now);

    BroadcastTextGlobal(NotificationMsg.ToJSON);
  finally
    NotificationMsg.Free;
  end;
end;

procedure TChatController.OnTextMessage(AConnection: TMVCWebSocketConnection;
  const AMessage: string);
var
  IncomingMsg: TJsonObject;
  OutgoingMsg: TJsonObject;
  MessageType: string;
  MessageText: string;
  Username: string;
begin
  // NOTE: inherited does nothing, safe to skip

  // Parse incoming JSON message
  IncomingMsg := TJsonObject.Parse(AMessage) as TJsonObject;
  try
    MessageType := IncomingMsg.S['type'];

    if SameText(MessageType, 'chat') then
    begin
      // chat message
      MessageText := IncomingMsg.S['text'];
      Username := AConnection.CustomData.Items['username'];
      if Username.IsEmpty then
        Username := Copy(AConnection.ConnectionId, 1, 8);

      Log.Info(Format('Chat message from %s: %s', [Username, MessageText]), 'Chat');

      // Create broadcast message
      OutgoingMsg := TJsonObject.Create;
      try
        OutgoingMsg.S['type'] := 'message';
        OutgoingMsg.S['connectionId'] := AConnection.ConnectionId;
        OutgoingMsg.S['username'] := Username;
        OutgoingMsg.S['text'] := MessageText;
        OutgoingMsg.S['timestamp'] := DateTimeToStr(Now);

        // Add to history
        AddToHistory(OutgoingMsg.ToJSON);

        // Broadcast to all users
        BroadcastTextGlobal(OutgoingMsg.ToJSON);
      finally
        OutgoingMsg.Free;
      end;
    end
    else if SameText(MessageType, 'typing') then
    begin
      // typing notification
      Username := AConnection.CustomData.Items['username'];
      if Username.IsEmpty then
        Username := Copy(AConnection.ConnectionId, 1, 8);

      OutgoingMsg := TJsonObject.Create;
      try
        OutgoingMsg.S['type'] := 'typing';
        OutgoingMsg.S['connectionId'] := AConnection.ConnectionId;
        OutgoingMsg.S['username'] := Username;

        BroadcastTextGlobal(OutgoingMsg.ToJSON);
      finally
        OutgoingMsg.Free;
      end;
    end
    else if SameText(MessageType, 'username') then
    begin
      // set username
      Username := IncomingMsg.S['username'];
      AConnection.CustomData.AddOrSetValue('username', Username);

      Log.Info(Format('User %s set username to: %s',
        [AConnection.ConnectionId, Username]), 'Chat');

      // Send confirmation
      OutgoingMsg := TJsonObject.Create;
      try
        OutgoingMsg.S['type'] := 'username_set';
        OutgoingMsg.S['username'] := Username;
        AConnection.SendText(OutgoingMsg.ToJSON);
      finally
        OutgoingMsg.Free;
      end;
    end;

  finally
    IncomingMsg.Free;
  end;
end;

procedure TChatController.OnError(AConnection: TMVCWebSocketConnection;
  const AError: Exception);
begin
  inherited;
  Log.Error(Format('WebSocket error for %s: %s',
    [AConnection.ConnectionId, AError.Message]), 'Chat');
end;

procedure TChatController.GetStats;
var
  Stats: TJsonObject;
begin
  Stats := TJsonObject.Create;
  try
    Stats.I['activeConnections'] := GetConnectionCount;
    Stats.I['messagesInHistory'] := Length(GetHistory);
    Stats.S['timestamp'] := DateTimeToStr(Now);

    Render(Stats);
  finally
    Stats.Free;
  end;
end;

procedure TChatController.GetChatPage;
const
  HTML_PAGE =
    '<!DOCTYPE html>' +
    '<html lang="en">' +
    '<head>' +
    '    <meta charset="UTF-8">' +
    '    <meta name="viewport" content="width=device-width, initial-scale=1.0">' +
    '    <title>DMVC WebSocket Chat</title>' +
    '    <style>' +
    '        body { font-family: Arial, sans-serif; max-width: 800px; margin: 50px auto; padding: 20px; }' +
    '        h1 { color: #333; }' +
    '        #status { padding: 10px; margin: 10px 0; border-radius: 5px; }' +
    '        #status.connected { background: #d4edda; color: #155724; }' +
    '        #status.disconnected { background: #f8d7da; color: #721c24; }' +
    '        #messages { border: 1px solid #ddd; height: 400px; overflow-y: auto; padding: 10px; margin: 10px 0; background: #f9f9f9; }' +
    '        .message { margin: 5px 0; padding: 5px; }' +
    '        .message.system { color: #666; font-style: italic; }' +
    '        .message.user { color: #000; }' +
    '        .message .username { font-weight: bold; color: #0066cc; }' +
    '        .message .timestamp { font-size: 0.8em; color: #999; margin-left: 10px; }' +
    '        #inputArea { display: flex; gap: 10px; }' +
    '        #messageInput { flex: 1; padding: 10px; font-size: 16px; }' +
    '        #sendButton { padding: 10px 20px; font-size: 16px; background: #0066cc; color: white; border: none; cursor: pointer; }' +
    '        #sendButton:hover { background: #0052a3; }' +
    '        #sendButton:disabled { background: #ccc; cursor: not-allowed; }' +
    '        #usernameArea { margin: 10px 0; }' +
    '        #usernameInput { padding: 5px; }' +
    '        #setUsernameButton { padding: 5px 15px; background: #28a745; color: white; border: none; cursor: pointer; }' +
    '    </style>' +
    '</head>' +
    '<body>' +
    '    <h1>🚀 DMVC WebSocket Chat</h1>' +
    '    <div id="status" class="disconnected">Disconnected</div>' +
    '    <div id="usernameArea">' +
    '        <input type="text" id="usernameInput" placeholder="Enter username">' +
    '        <button id="setUsernameButton" onclick="setUsername()">Set Username</button>' +
    '    </div>' +
    '    <div id="messages"></div>' +
    '    <div id="inputArea">' +
    '        <input type="text" id="messageInput" placeholder="Type a message..." disabled>' +
    '        <button id="sendButton" onclick="sendMessage()" disabled>Send</button>' +
    '    </div>' +
    '    <script>' +
    '        let ws;' +
    '        let connectionId;' +
    '        const messagesDiv = document.getElementById("messages");' +
    '        const statusDiv = document.getElementById("status");' +
    '        const messageInput = document.getElementById("messageInput");' +
    '        const sendButton = document.getElementById("sendButton");' +
    '' +
    '        function connect() {' +
    '            const protocol = window.location.protocol === "https:" ? "wss:" : "ws:";' +
    '            const wsUrl = protocol + "//" + window.location.host + "/chat";' +
    '            ws = new WebSocket(wsUrl);' +
    '' +
    '            ws.onopen = () => {' +
    '                statusDiv.textContent = "Connected";' +
    '                statusDiv.className = "connected";' +
    '                messageInput.disabled = false;' +
    '                sendButton.disabled = false;' +
    '            };' +
    '' +
    '            ws.onmessage = (event) => {' +
    '                const msg = JSON.parse(event.data);' +
    '                handleMessage(msg);' +
    '            };' +
    '' +
    '            ws.onclose = () => {' +
    '                statusDiv.textContent = "Disconnected - Reconnecting...";' +
    '                statusDiv.className = "disconnected";' +
    '                messageInput.disabled = true;' +
    '                sendButton.disabled = true;' +
    '                setTimeout(connect, 3000);' +
    '            };' +
    '' +
    '            ws.onerror = (error) => {' +
    '                console.error("WebSocket error:", error);' +
    '            };' +
    '        }' +
    '' +
    '        function handleMessage(msg) {' +
    '            switch(msg.type) {' +
    '                case "welcome":' +
    '                    connectionId = msg.connectionId;' +
    '                    addSystemMessage(`Welcome! Your ID: ${connectionId}. Users online: ${msg.userCount}`);' +
    '                    if (msg.history) {' +
    '                        msg.history.forEach(historyMsg => {' +
    '                            const parsed = JSON.parse(historyMsg);' +
    '                            addUserMessage(parsed.username, parsed.text, parsed.timestamp);' +
    '                        });' +
    '                    }' +
    '                    break;' +
    '                case "message":' +
    '                    addUserMessage(msg.username, msg.text, msg.timestamp);' +
    '                    break;' +
    '                case "user_joined":' +
    '                    addSystemMessage(`User joined. Users online: ${msg.userCount}`);' +
    '                    break;' +
    '                case "user_left":' +
    '                    addSystemMessage(`User left. Users online: ${msg.userCount}`);' +
    '                    break;' +
    '                case "username_set":' +
    '                    addSystemMessage(`Username set to: ${msg.username}`);' +
    '                    break;' +
    '            }' +
    '        }' +
    '' +
    '        function addSystemMessage(text) {' +
    '            const msgDiv = document.createElement("div");' +
    '            msgDiv.className = "message system";' +
    '            msgDiv.textContent = text;' +
    '            messagesDiv.appendChild(msgDiv);' +
    '            messagesDiv.scrollTop = messagesDiv.scrollHeight;' +
    '        }' +
    '' +
    '        function addUserMessage(username, text, timestamp) {' +
    '            const msgDiv = document.createElement("div");' +
    '            msgDiv.className = "message user";' +
    '            msgDiv.innerHTML = `<span class="username">${username}:</span> ${text}<span class="timestamp">${timestamp}</span>`;' +
    '            messagesDiv.appendChild(msgDiv);' +
    '            messagesDiv.scrollTop = messagesDiv.scrollHeight;' +
    '        }' +
    '' +
    '        function sendMessage() {' +
    '            const text = messageInput.value.trim();' +
    '            if (text && ws.readyState === WebSocket.OPEN) {' +
    '                ws.send(JSON.stringify({ type: "chat", text: text }));' +
    '                messageInput.value = "";' +
    '            }' +
    '        }' +
    '' +
    '        function setUsername() {' +
    '            const username = document.getElementById("usernameInput").value.trim();' +
    '            if (username && ws.readyState === WebSocket.OPEN) {' +
    '                ws.send(JSON.stringify({ type: "username", username: username }));' +
    '            }' +
    '        }' +
    '' +
    '        messageInput.addEventListener("keypress", (e) => {' +
    '            if (e.key === "Enter") sendMessage();' +
    '        });' +
    '' +
    '        connect();' +
    '    </script>' +
    '</body>' +
    '</html>';
begin
  ContentType := TMVCMediaType.TEXT_HTML;
  Render(HTML_PAGE);
end;

initialization
  GChatHistory := TThreadList<string>.Create;
  GChatConnectionManager := TMVCWebSocketConnectionManager.Create;

finalization
  GChatHistory.Free;
  GChatConnectionManager.Free;

end.
