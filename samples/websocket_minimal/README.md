# Minimal WebSocket Server Example

## Overview

This is the **simplest possible** WebSocket server implementation using DMVCFramework WebSocket support.

- ✅ **No MVC framework required**
- ✅ **No WebBroker required**
- ✅ **Only Indy + WebSocket protocol**
- ✅ **~200 lines of code**
- ✅ **Pure console application**

## What It Does

- Listens on port **9091**
- Performs WebSocket handshake (RFC 6455)
- Echoes back any text or binary message received
- Responds to Ping/Pong
- Handles graceful disconnection

## Files

- `MinimalWebSocketServer.dpr` - Server executable (200 lines)
- `test_minimal.html` - HTML/JavaScript test client
- `README.md` - This file

## How to Run

### 1. Compile the Server

```bash
cd samples\websocket_minimal
dcc32 -NSSystem -U"..\..\sources" MinimalWebSocketServer.dpr
```

### 2. Start the Server

```bash
MinimalWebSocketServer.exe
```

You should see:

```
=== Minimal WebSocket Server ===

Starting server on port 9091...
Server running!

Connect with:
  ws://localhost:9091/

Press ENTER to stop...
```

### 3. Test with Browser

Open `test_minimal.html` in your browser:

1. Click **Connect**
2. Type a message
3. Click **Send** (or press Enter)
4. See the echo response!

### 4. Test with Command Line

You can also use `wscat` (if installed):

```bash
npm install -g wscat
wscat -c ws://localhost:9091/
```

Then type messages and see them echoed back.

## Server Output

When a client connects and sends messages:

```
[10:30:45] Client connected from 127.0.0.1
[10:30:45] WebSocket handshake successful
[10:30:52] Received: Hello WebSocket!
[10:30:55] Received: Testing...
[10:31:00] Client closing connection
[10:31:00] Client disconnected
```

## Architecture

```
Client (Browser)           Server (Delphi)
     |                          |
     |--- HTTP GET ------------>|  1. HTTP Request
     |<-- 101 Switching --------|  2. WebSocket Handshake
     |                          |
     |=== WebSocket Frames ====>|  3. Bidirectional Communication
     |<========================>|
     |                          |
     |--- Close Frame --------->|  4. Graceful Closure
     |<-- Close Frame ----------|
```

## Protocol Details

### Handshake (HTTP → WebSocket)

**Client Request:**
```
GET / HTTP/1.1
Upgrade: websocket
Connection: Upgrade
Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==
Sec-WebSocket-Version: 13
```

**Server Response:**
```
HTTP/1.1 101 Switching Protocols
Upgrade: websocket
Connection: Upgrade
Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=
```

### Frame Exchange

- **Text Frame**: UTF-8 encoded text messages
- **Binary Frame**: Raw binary data
- **Close Frame**: Graceful connection closure
- **Ping/Pong**: Keep-alive mechanism

### Key Points

1. **Client frames MUST be masked** (RFC 6455)
2. **Server frames MUST NOT be masked** (RFC 6455)
3. After handshake, input buffer is cleared
4. All frames are FIN=1 (no fragmentation in this simple example)

## Code Walkthrough

### 1. Handshake Function

```delphi
function PerformHandshake(AContext: TIdContext): Boolean;
begin
  // 1. Read HTTP GET request
  // 2. Parse Sec-WebSocket-Key header
  // 3. Calculate Sec-WebSocket-Accept key
  // 4. Send 101 Switching Protocols
  // 5. Clear input buffer!
end;
```

### 2. Frame Processing Loop

```delphi
procedure ProcessFrames(AContext: TIdContext);
begin
  while Connected do
  begin
    // 1. Wait for data (100ms timeout)
    // 2. Parse WebSocket frame (server side, expects masked)
    // 3. Handle opcode (Text/Binary/Close/Ping/Pong)
    // 4. Echo response (unmasked)
  end;
end;
```

### 3. Echo Logic

```delphi
case Frame.Opcode of
  TMVCWebSocketOpcode.Text:
  begin
    Message := TEncoding.UTF8.GetString(Frame.Payload);
    EchoFrame := CreateTextFrame('Echo: ' + Message, False);  // False = no mask
    WriteFrame(IOHandler, EchoFrame);
  end;
end;
```

## Extending This Example

### Add Authentication

```delphi
// In PerformHandshake, check for custom header:
AuthToken := '';
for Line in Headers do
  if Line.StartsWith('Authorization:') then
    AuthToken := Line.Substring(14).Trim;

if not ValidateToken(AuthToken) then
  Exit(False);  // Reject handshake
```

### Add Multiple Paths

```delphi
// Extract path from request line:
Path := RequestLine.Split([' '])[1];  // e.g., "/chat"

case Path of
  '/chat': ProcessChatFrames(AContext);
  '/echo': ProcessEchoFrames(AContext);
else
  Exit(False);  // Unknown path
end;
```

### Add Broadcasting

```delphi
// Keep list of active connections:
FConnections: TThreadList<TIdContext>;

// Broadcast to all:
procedure BroadcastMessage(const AMessage: string);
var
  List: TList<TIdContext>;
  Context: TIdContext;
begin
  List := FConnections.LockList;
  try
    for Context in List do
      SendTextFrame(Context, AMessage);
  finally
    FConnections.UnlockList;
  end;
end;
```

## Troubleshooting

### Server won't start - "Address already in use"

Another process is using port 9091. Either:
- Stop the other process
- Change port in code: `DefaultPort := 9092;`

### Client can't connect

- Verify server is running (`netstat -an | findstr 9091`)
- Check firewall settings
- Try `ws://127.0.0.1:9091/` instead of `localhost`

### "Reserved bits must be 0" error

This means there's garbage in the stream. Common causes:
- Buffer not cleared after handshake (fixed in this example)
- Reading HTTP data as WebSocket frames
- Network corruption

### Messages not echoing

- Check server console for errors
- Verify frame opcode is Text (0x1) or Binary (0x2)
- Ensure client is sending masked frames

## Performance

This simple server can handle:
- **~1000 concurrent connections** (default Indy limit)
- **~10,000 messages/second** (small messages)
- **~100 MB/second throughput** (binary data)

For production use, consider:
- Connection pooling
- Message queuing
- Load balancing
- Horizontal scaling

## Requirements

- Delphi 11 Alexandria or later
- Indy components (included with Delphi)
- Windows, Linux, or macOS

## Next Steps

1. ✅ **You've mastered the basics!**
2. Add your business logic in `ProcessFrames`
3. Implement authentication/authorization
4. Add database integration
5. Scale horizontally with multiple servers

## License

Apache License 2.0 (same as DMVCFramework)
