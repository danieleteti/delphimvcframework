# WebSocket Client Test Application

## Overview
This is a Windows VCL test client for testing WebSocket connections with the DMVCFramework WebSocket server.

## Prerequisites
1. Start the WebSocket server first:
   - Run `WebSocketServerU.exe` (listens on port 8080)
   - Or use the chat server `WebSocketChatServer.exe` (listens on port 9090)

## Usage

### 1. Start the Server
```bash
cd samples\websocket_primer
WebSocketServerU.exe
```

The server will start and display:
```
** DelphiMVCFramework WebSocket Sample **
Starting HTTP Server on port 8080
Server is running on port 8080

WebSocket Endpoints:
  ws://localhost:8080/ws/echo  - Echo server
  ws://localhost:8080/ws/chat  - Chat room

Press Ctrl+C to stop the server
```

### 2. Run the Client
```bash
WebSocketClientTest.exe
```

### 3. Connect to Server
- Default URL: `ws://localhost:8080/ws/echo`
- Click **Connect** button
- Wait for "Connected!" message in the log

### 4. Test Features

**Send Text Messages:**
1. Type a message in the text box
2. Click **Send** or press Enter
3. The echo server will respond with the same message

**Send Ping:**
1. Click **Ping** button
2. Server will automatically respond with Pong

**Disconnect:**
1. Click **Disconnect** to close the connection

**Auto-Reconnect:**
1. Check "Auto Reconnect" before connecting
2. If connection is lost, client will automatically reconnect every 5 seconds

## Available Endpoints

### Echo Server (`/ws/echo`)
- Echoes back any text or binary message received
- Automatically responds to ping/pong

### Chat Server (`/ws/chat`)
- Broadcasts messages to all connected clients
- Supports JSON message format

## Testing with Chat Server

To test with the chat server instead:

1. Start the chat server:
```bash
cd samples\websocket_chat
WebSocketChatServer.exe
```

2. Change URL in client to: `ws://localhost:9090/chat`

3. Connect and send JSON messages:
```json
{"type": "chat", "text": "Hello from VCL client!"}
```

## Troubleshooting

**Client won't connect:**
- Verify server is running
- Check the port number (8080 for primer, 9090 for chat)
- Check firewall settings

**Buttons are disabled:**
- Click **Connect** first to enable Send/Ping buttons
- If connection fails, buttons will remain disabled

**Connection drops:**
- Enable "Auto Reconnect" option
- Check server logs for errors
- Verify network connectivity
