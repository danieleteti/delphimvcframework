# DelphiMVCFramework WebSocket Client Sample

This sample demonstrates the VCL WebSocket client implementation in DelphiMVCFramework.

## What's Included

### Client Application

- **WebSocketClientTest.dpr** - VCL application entry point
- **WebSocketClientTestU.pas/.dfm** - Main form with WebSocket client UI

## Features

- ✅ Connect to WebSocket servers
- ✅ Send text messages
- ✅ Send Ping frames and receive Pong responses
- ✅ Automatic reconnection support
- ✅ Real-time message logging
- ✅ Thread-safe UI updates

## How to Run

### 1. Start the WebSocket Server

Use the minimal server sample:

```bash
cd ..\websocket_minimal
dcc32 -B -NSSystem -U"..\..\sources" MinimalWebSocketServer.dpr
MinimalWebSocketServer.exe
```

The server will start on port 9091.

### 2. Run the VCL Client

```bash
cd samples\websocket_primer
dcc32 -B -NSSystem -U"..\..\sources" WebSocketClientTest.dpr
WebSocketClientTest.exe
```

### 3. Use the Client

1. The default URL is `ws://localhost:9091/` (connects to minimal server)
2. Click **Connect** to establish WebSocket connection
3. Use **Send** to send text messages
4. Use **Ping** to test server responsiveness
5. Monitor the log area for incoming messages and events
6. Enable **Auto Reconnect** for automatic reconnection on disconnect

## Client Features

### Connection Management
- Connect/disconnect buttons
- Visual connection state
- Auto-reconnect option with 5-second interval

### Messaging
- Send text messages to server
- Receive and display server messages
- Real-time message logging

### Ping/Pong
- Send Ping frames
- Receive and display Pong responses
- Verify server responsiveness

### Thread Safety
- All UI updates synchronized via `TThread.Queue`
- Safe event callbacks from background thread

## Testing Scenarios

### 1. Basic Connection Test
1. Start minimal server
2. Click "Connect"
3. Verify "Connected!" message in log
4. Click "Disconnect"
5. Verify "Disconnected" message

### 2. Message Exchange
1. Connect to server
2. Type message in text field
3. Click "Send"
4. See echo response in log

### 3. Ping/Pong Test
1. Connect to server
2. Click "Ping"
3. See "Pong received!" in log
4. Verifies connection is alive

### 4. Auto-Reconnect
1. Enable "Auto Reconnect" checkbox
2. Connect to server
3. Stop the server
4. See "Disconnected" and "Attempting to reconnect..." messages
5. Restart server within 5 seconds
6. See automatic reconnection

## Client Architecture

```delphi
// Create WebSocket client
FWebSocketClient := TMVCWebSocketClient.Create(EditURL.Text);

// Set up event callbacks
FWebSocketClient.OnConnect := OnConnect;
FWebSocketClient.OnDisconnect := OnDisconnect;
FWebSocketClient.OnTextMessage := OnTextMessage;
FWebSocketClient.OnError := OnError;
FWebSocketClient.OnPong := OnPong;

// Configure auto-reconnect
FWebSocketClient.AutoReconnect := CheckAutoReconnect.Checked;

// Connect
FWebSocketClient.Connect;

// Send messages
FWebSocketClient.SendText('Hello WebSocket!');

// Send ping
FWebSocketClient.SendPing;

// Disconnect
FWebSocketClient.Disconnect;
```

## Event Handlers

All event handlers use `TThread.Queue` for thread-safe UI updates:

```delphi
procedure TFormWebSocketClient.OnTextMessage(Sender: TMVCWebSocketClient; const AMessage: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      Log('Received: ' + AMessage);
    end);
end;

procedure TFormWebSocketClient.OnPong(Sender: TMVCWebSocketClient);
begin
  TThread.Queue(nil,
    procedure
    begin
      Log('Pong received!');
    end);
end;
```

## Protocol Details

### WebSocket Opcodes

- `0x1` - Text frame (UTF-8)
- `0x2` - Binary frame
- `0x8` - Connection close
- `0x9` - Ping
- `0xA` - Pong

### Close Codes

- `1000` - Normal closure
- `1001` - Going away
- `1002` - Protocol error
- `1011` - Internal server error

### Masking

Per RFC 6455:
- Client → Server: frames MUST be masked
- Server → Client: frames MUST NOT be masked

The client automatically handles masking for all outgoing frames.

## Related Samples

- **`samples/websocket_minimal/`** - Standalone WebSocket server
  - Minimal implementation (~200 lines)
  - Event-based architecture
  - No MVC dependencies
  - Perfect for testing this client

- **`samples/websocket_minimal/test_minimal.html`** - Browser test client
  - HTML/JavaScript WebSocket client
  - Works alongside VCL client

## Implementation Files

### Core WebSocket Units
- `sources/MVCFramework.WebSocket.pas` - RFC 6455 protocol implementation
- `sources/MVCFramework.WebSocket.Client.pas` - Client implementation
- `sources/MVCFramework.WebSocket.ConnectionManager.pas` - Connection tracking
- `sources/MVCFramework.WebSocket.RateLimiter.pas` - Rate limiting utilities

## Requirements

- Delphi 11 Alexandria or later (tested with Delphi 12)
- Indy components (included with Delphi)
- DelphiMVCFramework

## Troubleshooting

### Connection refused
**Solution**: Ensure minimal server is running on port 9091. Check firewall settings.

### "Reserved bits must be 0" error
**Solution**: This has been fixed. Client correctly handles unmasked frames from server.

### Pong not appearing
**Solution**: OnPong event is now implemented. Verify Ping button is clicked while connected.

### Messages not appearing in log
**Solution**: Verify EditMessage field is enabled (only enabled when connected).

## Technical Notes

- Uses Indy `TIdTCPClient` for TCP connection
- Implements RFC 6455 WebSocket protocol
- Background thread for receiving messages
- Proper cleanup on disconnect
- Graceful shutdown with Close frame

## Next Steps

- Add support for binary messages
- Implement compression (permessage-deflate)
- Add authentication (JWT in handshake headers)
- Add SSL/TLS support (wss://)
- Add message history/replay

## References

- [RFC 6455 - The WebSocket Protocol](https://tools.ietf.org/html/rfc6455)
- [MDN WebSocket API](https://developer.mozilla.org/en-US/docs/Web/API/WebSocket)
- [DelphiMVCFramework Documentation](https://github.com/danieleteti/delphimvcframework)

## License

Apache License 2.0 (same as DelphiMVCFramework)
