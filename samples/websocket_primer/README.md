# DelphiMVCFramework WebSocket Sample

This sample demonstrates the WebSocket implementation in DelphiMVCFramework.

## What's Included

### Server Components

- **WebSocketServerU.dpr** - Console server application
- **WebSocketWebModuleU.pas** - Web module configuration
- **WebSocketControllerU.pas** - WebSocket controllers
  - `TWebSocketEchoController` - Simple echo server
  - `TWebSocketChatController` - Multi-user chat room

### Client Components

1. **VCL Client** (WebSocketClientTest.dpr)
   - Full-featured Delphi VCL test client
   - Connect/disconnect functionality
   - Send text messages
   - Auto-reconnect option
   - Ping/pong support

2. **Web Client** (www/index.html)
   - HTML/JavaScript browser client
   - Modern UI design
   - Works with any browser

## WebSocket Endpoints

### `/ws/echo` - Echo Server
- Echoes back any text message you send
- Demonstrates basic WebSocket communication
- Adds timestamp to echoed messages

### `/ws/chat` - Chat Room
- Broadcasts messages to all connected clients
- Shows user join/leave notifications
- Displays total user count
- Demonstrates multi-client scenarios

## How to Run

### 1. Start the Server

```bash
# Compile and run the server
dcc32 WebSocketServerU.dpr
WebSocketServerU.exe
```

The server will start on port 8080 and display:
```
** DelphiMVCFramework WebSocket Sample **
Starting HTTP Server on port 8080

WebSocket Endpoints:
  ws://localhost:8080/ws/echo  - Echo server
  ws://localhost:8080/ws/chat  - Chat room

Press Ctrl+C to stop the server
```

### 2. Test with VCL Client

1. Compile `WebSocketClientTest.dpr`
2. Run the executable
3. Enter WebSocket URL (default: `ws://localhost:8080/ws/echo`)
4. Click "Connect"
5. Type messages and click "Send"

### 3. Test with Browser

1. Open your browser
2. Navigate to: `http://localhost:8080/static/index.html`
3. Select endpoint from dropdown
4. Click "Connect"
5. Start chatting!

## Features Demonstrated

### Core Features
- ✅ WebSocket handshake (RFC 6455)
- ✅ Text message frames
- ✅ Binary message frames
- ✅ Ping/Pong frames (keep-alive)
- ✅ Close frames with reason codes
- ✅ Frame masking (client to server)
- ✅ Extended payload lengths (16-bit and 64-bit)

### Advanced Features
- ✅ Connection management
- ✅ Broadcasting to multiple clients
- ✅ Per-connection state management
- ✅ Auto-reconnect (client-side)
- ✅ Thread-safe operations
- ✅ Graceful disconnection

## Architecture

### Server-Side

```delphi
// Define a WebSocket controller
[MVCPath('/ws/myendpoint')]
TMyWebSocketController = class(TMVCWebSocketController)
protected
  procedure OnConnect(AConnection: TMVCWebSocketConnection); override;
  procedure OnTextMessage(AConnection: TMVCWebSocketConnection;
    const AMessage: string); override;
  procedure OnDisconnect(AConnection: TMVCWebSocketConnection;
    ACode: TMVCWebSocketCloseCode; const AReason: string); override;
end;

// Register in WebModule
FMVC.AddController(TMyWebSocketController);
```

### Client-Side

```delphi
// Create client
FClient := TMVCWebSocketClient.Create('ws://localhost:8080/ws/myendpoint');
FClient.OnTextMessage := OnTextMessage;
FClient.OnConnect := OnConnect;
FClient.AutoReconnect := True;

// Connect
FClient.Connect;

// Send message
FClient.SendText('Hello WebSocket!');

// Cleanup
FClient.Disconnect;
FClient.Free;
```

## Protocol Details

### WebSocket Frame Structure

```
 0                   1                   2                   3
 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
+-+-+-+-+-------+-+-------------+-------------------------------+
|F|R|R|R| opcode|M| Payload len |    Extended payload length    |
|I|S|S|S|  (4)  |A|     (7)     |             (16/64)           |
|N|V|V|V|       |S|             |   (if payload len==126/127)   |
| |1|2|3|       |K|             |                               |
+-+-+-+-+-------+-+-------------+ - - - - - - - - - - - - - - - +
|     Extended payload length continued, if payload len == 127  |
+ - - - - - - - - - - - - - - - +-------------------------------+
|                               |Masking-key, if MASK set to 1  |
+-------------------------------+-------------------------------+
| Masking-key (continued)       |          Payload Data         |
+-------------------------------- - - - - - - - - - - - - - - - +
:                     Payload Data continued ...                :
+ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +
|                     Payload Data continued ...                |
+---------------------------------------------------------------+
```

### Opcodes

- `0x0` - Continuation frame
- `0x1` - Text frame (UTF-8)
- `0x2` - Binary frame
- `0x8` - Connection close
- `0x9` - Ping
- `0xA` - Pong

### Close Codes

- `1000` - Normal closure
- `1001` - Going away
- `1002` - Protocol error
- `1003` - Unsupported data
- `1007` - Invalid payload
- `1008` - Policy violation
- `1009` - Message too big
- `1011` - Internal server error

## Testing Scenarios

### 1. Basic Echo Test
1. Connect to `/ws/echo`
2. Send: "Hello"
3. Receive: "[timestamp] Echo: Hello"

### 2. Multi-User Chat
1. Open two browser tabs (or one browser + VCL client)
2. Connect both to `/ws/chat`
3. Send message from one client
4. See message appear in both clients
5. Disconnect one client
6. See notification in remaining client

### 3. Connection Resilience
1. Enable auto-reconnect in client
2. Connect to server
3. Stop the server
4. Restart the server
5. Client should auto-reconnect

### 4. Ping/Pong Keep-Alive
1. Connect to any endpoint
2. Wait (server sends ping every 30 seconds)
3. Client automatically responds with pong
4. Connection stays alive

## Configuration

### Server Configuration

```delphi
// In your WebSocket controller
constructor TMyWebSocketController.Create;
begin
  inherited;
  PingInterval := 30; // Ping interval in seconds (0 = disabled)
  MaxMessageSize := 1024 * 1024; // 1MB max message size
end;
```

### Client Configuration

```delphi
FClient.AutoReconnect := True;
FClient.ReconnectInterval := 5; // Reconnect after 5 seconds
```

## Thread Safety

All WebSocket operations are thread-safe:
- Connection manager uses `TCriticalSection`
- Per-connection locks for send operations
- Safe broadcasting to multiple clients
- Event callbacks synchronized with main thread (in VCL client)

## Logging

The server uses LoggerPro for logging:
- Connection events (connect/disconnect)
- Message traffic
- Errors and exceptions

Check console output for real-time logs.

## Requirements

- Delphi 11 Alexandria or later
- Indy components (included with Delphi)
- DelphiMVCFramework
- LoggerPro (included with DMVC)

## Troubleshooting

### "Can only be used with INDY based application server"
**Solution**: This implementation requires Indy. Make sure you're using `TIdHTTPWebBrokerBridge`.

### Connection refused
**Solution**: Ensure server is running on port 8080. Check firewall settings.

### Messages not appearing
**Solution**: Check browser console for JavaScript errors. Verify WebSocket URL is correct.

### Auto-reconnect not working
**Solution**: Enable auto-reconnect option and ensure ReconnectInterval is set.

## Performance Tips

1. **Broadcast Optimization**: For high-frequency broadcasts, consider batching messages
2. **Message Size**: Keep messages reasonably sized (< 1MB recommended)
3. **Connection Limits**: Default is 1024 connections, adjust based on your needs
4. **Ping Interval**: Balance between keeping connections alive and reducing traffic

## Next Steps

- Implement authentication (use JWT in handshake)
- Add rooms/channels for chat
- Implement compression (permessage-deflate)
- Add binary protocol (MessagePack)
- Scale with connection pooling
- Add metrics/monitoring

## References

- [RFC 6455 - The WebSocket Protocol](https://tools.ietf.org/html/rfc6455)
- [MDN WebSocket API](https://developer.mozilla.org/en-US/docs/Web/API/WebSocket)
- [DelphiMVCFramework Documentation](https://github.com/danieleteti/delphimvcframework)

## License

Same as DelphiMVCFramework - Apache License 2.0
