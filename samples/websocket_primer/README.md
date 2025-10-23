# WebSocket Echo Server with Periodic Messages

This demo showcases DelphiMVCFramework WebSocket support featuring:
- **Echo server** with request/response pattern
- **Periodic heartbeat messages** sent automatically by the server
- **Per-client session data** management
- **Dynamic interval adjustment** based on client behavior

## 🎯 What You'll Learn

- How to handle WebSocket connections
- Sending periodic server-initiated messages
- Managing per-client session state
- Dynamically adjusting message intervals
- Using all WebSocket event handlers

## 📦 What's Included

### Server
- **WebSocketServerEcho.dpr** - Console echo server using `TMVCWebSocketServer`
  - Listens on port **9091**
  - Echoes back all text messages
  - **Sends periodic heartbeat messages** to each client
  - Demonstrates per-client interval configuration (localhost: 2s, local network: 3s, others: 10s)
  - **Dynamic interval adjustment**: After 5 heartbeats, interval increases by 1 second each time
  - Per-client session data tracking message count
  - Fully documented event handlers (see source code for detailed comments)

### Clients
1. **WebSocketClientTest.dpr** - VCL desktop client application
2. **www/index.html** - Web browser client with real-time statistics

## 🚀 How to Run

### 1. Start the Echo Server

```bash
cd samples\websocket_primer
WebSocketServerEcho.exe
```

You'll see:
```
=== WebSocket Echo Server with Periodic Messages ===

Starting echo server on port 9091...
Echo server running!

Connect with:
  ws://localhost:9091/
```

### 2. Test with Web Client (Recommended)

1. Open `www/index.html` in your browser
2. Click **Connect** (URL is pre-filled: `ws://localhost:9091/`)
3. Watch the statistics panel for:
   - **Messages Sent** - Your outgoing messages
   - **Messages Received** - Echo responses from server
   - **Periodic Messages** - Automatic heartbeat messages (green)
   - **Connected Time** - Live connection timer
4. Type messages and press Enter to test echo functionality
5. Observe heartbeat messages arriving automatically every 2 seconds
6. After 5 heartbeats, watch the interval increase dynamically

### 3. Test with VCL Client

1. Run `WebSocketClientTest.exe`
2. URL is already set to `ws://localhost:9091/`
3. Click **Connect**
4. Type message and click **Send**
5. Observe both echo responses and periodic heartbeat messages

## 💡 Key Features Demonstrated

### 1. Periodic Messages
```delphi
// Set default interval
Server.PeriodicMessageInterval := 5000; // 5 seconds

// Or configure per-client in OnClientConnect
Server.OnClientConnect := procedure(AClient: TWebSocketClient)
begin
  if AClient.ClientId = '127.0.0.1' then
    AClient.PeriodicInterval := 2000  // Localhost: every 2 seconds
  else
    AClient.PeriodicInterval := 10000; // Others: every 10 seconds
end;
```

### 2. Session Data Management
```delphi
// Create session data for each client
AClient.Data := TClientSessionData.Create;

// Access it in any event
LSessionData := TClientSessionData(AClient.Data);
Inc(LSessionData.MessageCount);

// Automatically freed on disconnect
```

### 3. Dynamic Interval Adjustment
```delphi
Server.OnPeriodicMessage := function(AClient: TWebSocketClient; var ACurrentInterval: Integer): string
begin
  // Change interval dynamically
  if SomeCondition then
    ACurrentInterval := ACurrentInterval + 1000; // Slow down

  Result := 'Heartbeat message';
end;
```

### 4. Sending Messages to Clients
```delphi
// Send echo response using client method
Server.OnMessage := procedure(AClient: TWebSocketClient; const AMessage: string)
begin
  // Simple and direct - send to this client
  AClient.SendText(Format('Echo: %s', [AMessage]));

  // Alternative methods available:
  // AClient.SendBinary(ByteData);           // Send binary data
  // AClient.Broadcast(msg);                 // Send to all clients including self
  // AClient.BroadcastToPeers(msg);          // Send to all except self
  // AClient.SendTo(username, msg);          // Send to specific user
  // AClient.SendToGroup(groupName, msg);    // Send to a group
end;
```

### 5. All Event Handlers
The demo shows usage of:
- ✅ `OnLog` - Server internal events
- ✅ `OnClientConnect` - Client connection with session initialization
- ✅ `OnClientDisconnect` - Client disconnection with statistics
- ✅ `OnMessage` - Incoming text messages with echo response
- ✅ `OnError` - Error handling
- ✅ `OnPeriodicMessage` - Server-initiated messages with dynamic timing

See the source code for detailed comments explaining when and how to use each event.

## 🏗️ Architecture

### Unified Server Class
Uses `TMVCWebSocketServer` - a unified WebSocket server that supports:
- Simple echo/request-response patterns (this demo)
- Broadcast/chat patterns (see `websocket_chat` demo)
- Hybrid patterns combining both

### Per-Client Methods
Each connected client (`TWebSocketClient`) has methods to send messages:
- `AClient.SendText(msg)` - Send text message to this client
- `AClient.SendBinary(data)` - Send binary data to this client
- `AClient.Broadcast(msg)` - Send to all clients including self
- `AClient.BroadcastToPeers(msg)` - Send to all other clients (excluding self)
- `AClient.SendTo(username, msg)` - Send to a specific user by username
- `AClient.SendToGroup(groupName, msg)` - Send to all users in a group

### Per-Client Properties
- `AClient.ClientId` - Unique identifier (IP address)
- `AClient.Username` - Username (can be set, defaults to ClientId)
- `AClient.Data` - Custom session object (auto-freed)
- `AClient.PeriodicInterval` - Individual periodic message interval
- `AClient.Context` - Indy TCP connection context
- `AClient.Groups` - Array of groups this client belongs to

## 📝 When to Use This Pattern

**Use Echo/Request-Response pattern when:**
- Clients don't need to communicate with each other
- You need request/response semantics (like REST over WebSocket)
- Server needs to push periodic updates to clients
- Each client has independent state/session

**For chat/broadcasting, see:** `samples/websocket_chat`

## 🎨 Web Client Features

The HTML client (`www/index.html`) includes:
- 📊 Real-time statistics dashboard
- 🟢 Visual distinction between echo and heartbeat messages
- ⏱️ Connection duration timer
- 📋 Message log with timestamps
- 🧹 Clear log functionality
- 🎯 Fully responsive design

## 🔧 Customization Ideas

1. **Change heartbeat frequency** - Modify `AInitialInterval` values
2. **Add authentication** - Check credentials in `OnClientConnect`
3. **Implement rate limiting** - Track message count in session data
4. **Custom message format** - Parse JSON in `OnMessage`
5. **Health monitoring** - Use `OnPeriodicMessage` to check client responsiveness

## 📚 Related Demos

- **websocket_chat** - Broadcasting server with multi-user chat
- See source code comments for complete event handler documentation

## 🐛 Troubleshooting

**Port already in use?**
```
Error: EIdCouldNotBindSocket
```
→ Change port in `TMVCWebSocketServer.Create(9091)`

**No periodic messages?**
→ Make sure `AInitialInterval > 0` is set in `OnClientConnect`

**Client can't connect?**
→ Check firewall settings for port 9091

## 📄 License

Apache License 2.0
