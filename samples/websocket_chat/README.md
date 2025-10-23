# WebSocket Chat Server Example

This example demonstrates how to use DelphiMVCFramework's WebSocket support to create a real-time chat application.

## Important Notes

⚠️ **WebSocket Only Works with Standalone Server**

WebSocket requires a persistent TCP connection and **only works with the Indy standalone server**.
It does **NOT** work with:
- Apache modules
- ISAPI modules
- IIS

This is why this example is a **standalone console application** using `TIdHTTPWebBrokerBridge`.

## Features

- ✅ Real-time WebSocket chat
- ✅ Multiple concurrent users
- ✅ Chat history (last 100 messages)
- ✅ User join/leave notifications
- ✅ Custom usernames
- ✅ Automatic reconnection
- ✅ Ping/Pong keep-alive
- ✅ HTML5 web client included
- ✅ REST API for statistics

## Building and Running

### Prerequisites

- DelphiMVCFramework installed

### Compile

1. Open `WebSocketChatServer.dproj` in Delphi IDE
2. Build the project (Shift+F9)
3. Run (F9)

The server will start on port **8080** by default.

## Usage

### Web Client

Open your browser and navigate to:

```
http://localhost:8080/chat/page
```

You can open multiple browser tabs to simulate multiple users.

### WebSocket Endpoint

Connect to the WebSocket endpoint at:

```
ws://localhost:8080/chat
```

### Statistics API

Get current server statistics:

```bash
curl http://localhost:8080/chat/stats
```

Response:
```json
{
  "activeConnections": 3,
  "messagesInHistory": 42,
  "timestamp": "16/10/2025 14:30:00"
}
```

## WebSocket Protocol

### Client → Server Messages

#### Send Chat Message
```json
{
  "type": "chat",
  "text": "Hello, world!"
}
```

#### Set Username
```json
{
  "type": "username",
  "username": "JohnDoe"
}
```

#### Typing Notification
```json
{
  "type": "typing"
}
```

### Server → Client Messages

#### Welcome Message (on connect)
```json
{
  "type": "welcome",
  "connectionId": "a1b2c3d4",
  "userCount": 5,
  "timestamp": "16/10/2025 14:30:00",
  "history": [
    "{\"type\":\"message\",\"username\":\"Alice\",\"text\":\"Hi\",\"timestamp\":\"...\"}",
    ...
  ]
}
```

#### Chat Message (broadcast)
```json
{
  "type": "message",
  "connectionId": "a1b2c3d4",
  "username": "Alice",
  "text": "Hello everyone!",
  "timestamp": "16/10/2025 14:30:00"
}
```

#### User Joined
```json
{
  "type": "user_joined",
  "connectionId": "e5f6g7h8",
  "userCount": 6,
  "timestamp": "16/10/2025 14:30:05"
}
```

#### User Left
```json
{
  "type": "user_left",
  "connectionId": "a1b2c3d4",
  "userCount": 5,
  "timestamp": "16/10/2025 14:30:10"
}
```

## Architecture

### Files

- **WebSocketChatServer.dpr** - Main program entry point
- **WebModuleU.pas** - Web module configuration
- **ChatControllerU.pas** - Chat controller implementation (inherits from `TMVCWebSocketController`)

### Key Classes

#### `TMVCWebSocketController`

Base class for WebSocket endpoints. Provides:

```pascal
procedure OnConnect(AConnection: TMVCWebSocketConnection); virtual;
procedure OnDisconnect(AConnection: TMVCWebSocketConnection;
  ACode: TMVCWebSocketCloseCode; const AReason: string); virtual;
procedure OnTextMessage(AConnection: TMVCWebSocketConnection;
  const AMessage: string); virtual;
procedure OnBinaryMessage(AConnection: TMVCWebSocketConnection;
  const AData: TBytes); virtual;
procedure OnPing(AConnection: TMVCWebSocketConnection;
  const AData: TBytes); virtual;
procedure OnPong(AConnection: TMVCWebSocketConnection;
  const AData: TBytes); virtual;
procedure OnError(AConnection: TMVCWebSocketConnection;
  const AError: Exception); virtual;

procedure BroadcastText(const AMessage: string);
procedure BroadcastBinary(const AData: TBytes);
```

#### `TMVCWebSocketConnection`

Represents a single WebSocket connection:

```pascal
procedure SendText(const AMessage: string);
procedure SendBinary(const AData: TBytes);
procedure SendPing(const AData: TBytes = nil);
procedure SendPong(const AData: TBytes = nil);
procedure Close(ACode: TMVCWebSocketCloseCode; const AReason: string);

property ConnectionId: string;
property ConnectedAt: TDateTime;
property LastActivity: TDateTime;
property UserData: TObject;
property CustomData: TDictionary<string, string>;
```

## Configuration

You can configure WebSocket behavior in your controller:

```pascal
constructor TChatController.Create;
begin
  inherited;
  PingInterval := 30;           // Send ping every 30 seconds
  MaxMessageSize := 10 * 1024;  // 10KB max message size
end;
```

## Testing

### Test with Multiple Clients

Open multiple browser tabs to `http://localhost:8080/chat/page` and start chatting!

### Test with WebSocket Client Library

Use any WebSocket client (JavaScript, Python, etc.):

```javascript
const ws = new WebSocket('ws://localhost:8080/chat');

ws.onopen = () => {
  console.log('Connected!');
  ws.send(JSON.stringify({ type: 'chat', text: 'Hello!' }));
};

ws.onmessage = (event) => {
  console.log('Message:', JSON.parse(event.data));
};
```

## Troubleshooting

### Port Already in Use

If port 8080 is busy, edit `WebSocketChatServer.dpr`:

```delphi
RunServer(9090); // Use different port
```

### Connection Refused

Make sure:
1. Server is running
2. No firewall blocking the port
3. Using correct URL (`ws://` not `wss://` for non-SSL)

### SSL/TLS (WSS)

For secure WebSocket (wss://), you need to configure SSL on the Indy server.
See DMVC documentation for SSL configuration with `TIdHTTPWebBrokerBridge`.

## License

Apache License 2.0 - Same as DelphiMVCFramework

## Credits

Built with [DelphiMVCFramework](https://github.com/danieleteti/delphimvcframework) 🚀
