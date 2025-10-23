# 💬 WebSocket Chat Demo

A beautiful, real-time chat application built with **DelphiMVCFramework** demonstrating WebSocket functionality with HTTP server integration.

## 🌟 Overview

This demo showcases how to create a complete chat application using:
- **HTTP Server** (port 8080) - Serves the web client interface
- **WebSocket Server** (port 9091) - Handles real-time chat communication

The application demonstrates the common pattern of running HTTP and WebSocket servers on different ports, which is typical in production environments.

## ✨ Features

### Server Features
- ✅ Dual server architecture (HTTP + WebSocket)
- ✅ Real-time message broadcasting to all connected users
- ✅ User join/leave notifications
- ✅ Online user counter
- ✅ Custom username support
- ✅ Server-side logging with timestamps
- ✅ Clean separation of concerns

### Web Client Features
- 🎨 Beautiful gradient UI design
- 📱 Responsive layout
- 👤 Random username generation
- 💬 Real-time message display
- 📊 Live user count
- 🔔 Visual notifications for join/leave events
- ⌨️ Enter key support for sending messages
- 🎯 Message type distinction (own/other/server)
- ⏰ Timestamp for each message
- 🖱️ Smooth animations and transitions

## 🚀 How to Run

### Step 1: Compile the Server
```bash
cd c:\DEV\dmvcframework\samples\websocket_chat
dcc32 WebSocketChatServer.dpr
```

### Step 2: Run the Server
```bash
WebSocketChatServer.exe
```

You should see:
```
=======================================================
    DMVCFramework WebSocket Chat Demo
=======================================================

Starting HTTP server on port 8080...
  -> HTTP server running at http://localhost:8080/

Starting WebSocket server on port 9091...
  -> WebSocket server running at ws://localhost:9091/

=======================================================
  READY! Open your browser to:
  http://localhost:8080/
=======================================================
```

### Step 3: Open Multiple Browser Windows
1. Open your browser to `http://localhost:8080/`
2. Enter a username (or use the randomly generated one)
3. Click "Join Chat"
4. Open another browser window/tab to simulate multiple users
5. Start chatting!

## 📁 Project Structure

```
websocket_chat/
├── WebSocketChatServer.dpr      # Main program (HTTP + WebSocket servers)
├── WebModuleU.pas/dfm           # Web module for serving static files
├── www/
│   └── index.html               # Beautiful web chat client
└── README.md                    # This file
```

## 💻 Code Highlights

### Server Architecture

The main program creates two servers:

```delphi
// HTTP Server for web client
LWebServer := TIdHTTPWebBrokerBridge.Create(nil);
LWebServer.DefaultPort := 8080;
LWebServer.Active := True;

// WebSocket Server for chat
LWSServer := TMVCWebSocketServer.Create(9091);
LWSServer.Active := True;
```

### Event-Based Chat Implementation

The chat server uses anonymous methods for clean, inline event handling:

```delphi
// Handle incoming messages - broadcast to all
LWSServer.OnMessage := procedure(AClient: TWebSocketClient; const AMessage: string)
var
  LChatMsg: string;
begin
  LChatMsg := Format('[%s]: %s', [AClient.ClientId, AMessage]);
  AClient.Broadcast(LChatMsg);  // Send to ALL clients including sender
end;

// Handle client connection - notify all users
LWSServer.OnClientConnect := procedure(AClient: TWebSocketClient)
var
  LWelcomeMsg: string;
begin
  LWelcomeMsg := Format('[SERVER]: %s joined the chat', [AClient.ClientId]);
  AClient.Broadcast(LWelcomeMsg);
end;

// Handle disconnection - notify remaining users
LWSServer.OnClientDisconnect := procedure(AClient: TWebSocketClient)
var
  LGoodbyeMsg: string;
begin
  LGoodbyeMsg := Format('[SERVER]: %s left the chat', [AClient.ClientId]);
  AClient.BroadcastToPeers(LGoodbyeMsg);  // Send to all EXCEPT disconnecting client
end;
```

**Key Features:**
- `AClient.Broadcast(msg)` - Send to all connected clients (including sender)
- `AClient.BroadcastToPeers(msg)` - Send to all clients except sender
- No need for custom server class - just use anonymous methods!

### Web Client Connection

The JavaScript client connects to the WebSocket server:

```javascript
// Automatically uses current hostname with WebSocket port
const wsUrl = `ws://${window.location.hostname}:9091/`;

// Connect with username in protocol header
ws = new WebSocket(wsUrl, ['chat-' + username]);
```

### Message Broadcasting Methods

Two convenient methods available on every `TWebSocketClient`:

```delphi
// Broadcast to everyone (including sender)
AClient.Broadcast('Message to all');

// Broadcast only to other clients (excluding sender)
AClient.BroadcastToPeers('Message to others only');
```

These methods are internally implemented as wrappers around the server's `BroadcastText` method, providing a more intuitive OOP API.

## 🎯 Message Format

The server uses a simple text-based message format:

- **Chat messages**: `[username]: message content`
- **Join notification**: `[SERVER]: username joined the chat (N users online)`
- **Leave notification**: `[SERVER]: username left the chat (N users online)`

The web client parses these messages and applies appropriate styling.

## 🏗️ How It Works

### 1. Server Startup
1. HTTP server starts on port 8080
2. WebSocket server starts on port 9091
3. Both servers run in parallel

### 2. Client Connection
1. Browser loads HTML from HTTP server (port 8080)
2. JavaScript connects to WebSocket server (port 9091)
3. Username sent via `Sec-WebSocket-Protocol` header

### 3. Message Flow
```
User A sends message
    ↓
WebSocket Server receives
    ↓
Server broadcasts to ALL clients
    ↓
All users see message in real-time
```

### 4. Join/Leave Events
```
New user connects
    ↓
DoClientConnect called
    ↓
Server broadcasts: "X joined the chat (N users online)"
    ↓
All users see notification
```

## 🔧 Customization Ideas

### Server-Side
- Add private messaging between users
- Implement chat rooms/channels
- Add message persistence (database)
- Implement user authentication
- Add rate limiting per user
- Implement admin commands
- Add message filtering/moderation

### Client-Side
- Add emoji picker
- Implement typing indicators
- Add message reactions
- Show user list panel
- Add dark/light theme toggle
- Implement message search
- Add file/image sharing
- Show notification sounds
- Add message timestamps
- Implement user avatars

### Advanced Features
- Add SSL/TLS support (wss://)
- Implement reconnection logic
- Add message delivery confirmation
- Implement read receipts
- Add push notifications
- Implement user status (online/away/busy)

## 🐛 Troubleshooting

### Port Already in Use
If you see errors about ports being in use:
```bash
# Check what's using the ports
netstat -ano | findstr :8080
netstat -ano | findstr :9091

# Kill the process or change ports in code
```

### WebSocket Connection Failed
1. Make sure the server is running
2. Check browser console for errors
3. Verify firewall isn't blocking ports
4. Try accessing from `localhost` instead of `127.0.0.1`

### Can't See Other Users' Messages
1. Make sure you're connected (status shows "Connected as [username]")
2. Open browser developer console to check for WebSocket errors
3. Verify server console shows both users connected

### Messages Not Broadcasting
Check server console for errors. Each message should show:
```
[HH:MM:SS] <username> message content
```

## 📚 What You'll Learn

- ✅ Running HTTP and WebSocket servers together
- ✅ Serving static files via DMVCFramework
- ✅ WebSocket connection management
- ✅ Broadcasting messages to multiple clients
- ✅ Handling client connect/disconnect events
- ✅ Per-client state management
- ✅ Building responsive web chat interfaces
- ✅ JavaScript WebSocket API usage
- ✅ Real-time message parsing and formatting
- ✅ Modern CSS styling and animations

## 🎓 Educational Value

This demo is perfect for learning:

1. **WebSocket Basics** - See how real-time bidirectional communication works
2. **Server Architecture** - Understand HTTP + WebSocket dual-server pattern
3. **Event Handling** - Learn connection/disconnection event management
4. **Message Broadcasting** - Understand how to send messages to multiple clients
5. **Web UI Development** - Study modern responsive chat interface design
6. **Protocol Design** - See simple message format and parsing

## 🌐 Production Considerations

When moving to production, consider:

- Use **SSL/TLS** (HTTPS + WSS)
- Implement **authentication/authorization**
- Add **rate limiting** to prevent spam
- Implement **message validation** and sanitization
- Add **database persistence** for message history
- Implement **load balancing** for multiple server instances
- Add **monitoring and logging**
- Implement **graceful shutdown** handling
- Add **health check endpoints**
- Use **environment variables** for configuration

## 📖 Related Demos

- **websocket_primer** - Basic WebSocket server with periodic messages
- **websocket_echo** - Simple echo server for testing

## 🤝 Contributing

Found a bug or have a feature request? Please open an issue on the DMVCFramework GitHub repository.

## 📄 License

This demo is part of the DelphiMVCFramework project.

---

**Enjoy real-time chatting with DMVCFramework! 💬**
