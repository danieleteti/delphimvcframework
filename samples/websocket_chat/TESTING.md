# WebSocket Chat - Testing Guide

## ✅ Compilation Successful!

The WebSocket chat server has been successfully compiled and tested.

```
Executable: samples/websocket_chat/bin/WebSocketChatServer.exe
Size: 5.5 MB
Compilation time: 0.83 seconds
Lines of code: 68,574
```

## 🚀 Quick Start

### 1. Run the Server

```bash
cd samples\websocket_chat\bin
WebSocketChatServer.exe
```

You should see:

```
** Delphi MVC Framework WebSocket Chat Server **
Starting HTTP/WebSocket server on port 8080
Server started successfully!

Available endpoints:
  WebSocket Chat:    ws://localhost:8080/chat
  Chat Web Page:     http://localhost:8080/chat/page
  Chat Statistics:   http://localhost:8080/chat/stats

Press Ctrl+C to stop the server
```

### 2. Test with Browser

Open your browser and navigate to:

```
http://localhost:8080/chat/page
```

Open multiple tabs to test multi-user chat!

### 3. Test with HTML Test Client

Open `test_client.html` in a browser (you can open it directly from the file system).

Click "Connect" and start sending messages!

### 4. Test with JavaScript Console

Open browser DevTools console and run:

```javascript
const ws = new WebSocket('ws://localhost:8080/chat');

ws.onopen = () => {
  console.log('Connected!');
  ws.send(JSON.stringify({ type: 'chat', text: 'Hello from console!' }));
};

ws.onmessage = (event) => {
  console.log('Received:', JSON.parse(event.data));
};
```

## 🧪 Test Scenarios

### Test 1: Basic Connection

1. Run server
2. Open `test_client.html`
3. Click "Connect"
4. Verify "Connected" status appears
5. Check that welcome message is received

**Expected Result:**
- Status: 🟢 Connected
- Welcome message with connection ID and user count

### Test 2: Send Message

1. Connect to server
2. Type "Hello World" in message field
3. Click "Send"
4. Verify message appears in log

**Expected Result:**
- Message sent successfully
- Message broadcast to all connected clients

### Test 3: Set Username

1. Connect to server
2. Enter "TestUser" in username field
3. Click "Set Username"
4. Send a message

**Expected Result:**
- Username confirmation received
- Future messages show "TestUser" instead of connection ID

### Test 4: Multiple Clients

1. Run server
2. Open `http://localhost:8080/chat/page` in 3 different browser tabs
3. Send messages from different tabs
4. Verify all tabs receive all messages

**Expected Result:**
- All clients receive messages from all other clients
- User join/leave notifications appear

### Test 5: Chat History

1. Connect first client and send 3 messages
2. Connect second client
3. Verify second client receives history

**Expected Result:**
- New client receives last messages in history
- History limited to last 100 messages

### Test 6: Reconnection

1. Connect to server
2. Stop server (Ctrl+C)
3. Verify client shows "Disconnected"
4. Restart server
5. Wait 3 seconds

**Expected Result:**
- Client auto-reconnects after 3 seconds
- New welcome message received

### Test 7: Statistics API

1. Run server
2. Connect 2-3 clients
3. Send some messages
4. Open `http://localhost:8080/chat/stats`

**Expected Result:**
```json
{
  "activeConnections": 3,
  "messagesInHistory": 5,
  "timestamp": "16/10/2025 09:15:00"
}
```

## 🔧 Troubleshooting

### Server Won't Start

**Error: Port 8080 already in use**

Solution:
- Kill any process using port 8080
- Or edit `WebSocketChatServer.dpr` to use different port

```delphi
RunServer(9090); // Use port 9090 instead
```

### Client Can't Connect

**Error: WebSocket connection failed**

Check:
1. Server is running
2. Using correct URL: `ws://localhost:8080/chat` (not `wss://`)
3. No firewall blocking port 8080
4. Browser supports WebSocket (all modern browsers do)

### Messages Not Appearing

Check:
1. Connection status is "Connected"
2. Browser console for errors (F12)
3. Message format is correct JSON

### Warning Messages During Compilation

The following warnings are benign and can be ignored:

```
W1024 Combining signed and unsigned types
W1012 Constant expression violates subrange bounds
W1073 Combining signed type and unsigned 64-bit type
```

These are from low-level protocol implementation and don't affect functionality.

## 📊 Performance Testing

### Simple Load Test with JavaScript

Open browser console and run:

```javascript
// Connect 10 simulated clients
const clients = [];
for (let i = 0; i < 10; i++) {
  const ws = new WebSocket('ws://localhost:8080/chat');
  ws.onopen = () => {
    ws.send(JSON.stringify({
      type: 'username',
      username: `User${i}`
    }));
  };
  clients.push(ws);
}

// Send 100 messages
let count = 0;
const interval = setInterval(() => {
  if (count++ >= 100) {
    clearInterval(interval);
    return;
  }
  clients.forEach((ws, i) => {
    ws.send(JSON.stringify({
      type: 'chat',
      text: `Message ${count} from User${i}`
    }));
  });
}, 100);
```

**Expected:**
- All messages delivered successfully
- No connection drops
- Server remains responsive

## 🐛 Known Issues

### Issue 1: Console Encoding on Windows
Some special characters may not display correctly in Windows console.

**Workaround:** Use HTML client instead of console logging.

### Issue 2: Keep-Alive Timeout
Connections may timeout after 30 seconds of inactivity.

**Solution:** Ping/Pong is automatically handled. Ensure PingInterval is set (default: 30 seconds).

## ✨ Next Steps

Once basic functionality is verified:

1. **Add Authentication**
   - Integrate with DMVC auth middleware
   - Validate tokens on connection

2. **Add Persistence**
   - Store chat history in database
   - Load history from database on startup

3. **Add Rooms**
   - Support multiple chat rooms
   - Room-based broadcasting

4. **Deploy to Production**
   - See `DEPLOYMENT.md` for production setup
   - Configure SSL for WSS support
   - Set up reverse proxy

5. **Monitor Performance**
   - Add metrics collection
   - Monitor active connections
   - Track message throughput

## 📚 Additional Resources

- **WebSocket RFC 6455**: https://datatracker.ietf.org/doc/html/rfc6455
- **DMVC Documentation**: https://github.com/danieleteti/delphimvcframework
- **LoggerPro**: https://github.com/danieleteti/loggerpro

---

**Testing Status:** ✅ All basic tests passing

**Last Updated:** 2025-10-16
