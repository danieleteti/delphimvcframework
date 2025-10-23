# WebSocket Groups/Rooms Demo

This demo showcases DelphiMVCFramework WebSocket **group (room) management** features:
- 👥 **Multiple chat rooms** - Create and join different rooms dynamically
- 📨 **Group messaging** - Send messages to specific rooms only
- 🚪 **Join/Leave groups** - Dynamically add/remove clients from groups
- 👤 **Custom usernames** - Set unique usernames for clients
- 📋 **List users** - See who's in each room

## 🎯 What You'll Learn

- How to use `JoinGroup()` and `LeaveGroup()`
- How to send messages to groups with `SendToGroup()`
- How to list users in a group with `GetClientsByGroup()`
- How to check if a client is in a group with `IsInGroup()`
- Managing per-client usernames
- Building a multi-room chat system

## 📦 What's Included

### Server
- **WebSocketGroupServer.dpr** - Console server implementing room-based chat
  - Listens on port **9092**
  - Command-based interface (`/join`, `/leave`, `/msg`, etc.)
  - Multiple simultaneous chat rooms
  - User notifications when others join/leave rooms
  - Username management

### Client
- **www/index.html** - Web browser client with:
  - Quick command buttons
  - Real-time message display
  - Color-coded messages (sent/received/system)
  - Built-in help documentation

## 🚀 How to Run

### 1. Start the Server

```bash
cd samples\websocket_groups
WebSocketGroupServer.exe
```

You'll see:
```
=== WebSocket Group/Room Server ===

Starting group server on port 9092...
Group server running!

Connect with:
  ws://localhost:9092/
```

### 2. Open Multiple Browser Windows

1. Open `www/index.html` in 2-3 browser windows
2. Click **Connect** in each window
3. Each client gets a default username (their IP address)

### 3. Try These Examples

**Window 1:**
```
/setname Alice
/join lobby
/msg lobby Hello everyone!
```

**Window 2:**
```
/setname Bob
/join lobby
/msg lobby Hi Alice!
```

**Window 3:**
```
/setname Charlie
/join developers
/join lobby
/msg developers Anyone here?
/msg lobby I'm in both rooms!
```

## 💡 Group API Examples

### 1. Joining and Leaving Groups

```delphi
// Client joins a group
AClient.JoinGroup('lobby');
AClient.JoinGroup('developers');

// Check if in group
if AClient.IsInGroup('lobby') then
  Writeln('Client is in lobby');

// Get all groups this client is in
for LGroup in AClient.Groups do
  Writeln(LGroup);

// Leave a group
AClient.LeaveGroup('lobby');
```

### 2. Sending Messages to Groups

```delphi
Server.OnMessage := procedure(AClient: TWebSocketClient; const AMessage: string)
begin
  // Send message to everyone in the "lobby" group
  AClient.SendToGroup('lobby', Format('[%s] %s', [AClient.Username, AMessage]));

  // Returns the number of clients who received the message
  var LCount := AClient.SendToGroup('developers', 'Important announcement');
  Writeln(Format('Message sent to %d users', [LCount]));
end;
```

### 3. Getting Users in a Group

```delphi
// Get all clients in a specific group
var LClients := Server.GetClientsByGroup('lobby');

Writeln(Format('Users in lobby: %d', [Length(LClients)]));
for var LClient in LClients do
  Writeln(Format('  - %s', [LClient.Username]));
```

### 4. Notifying Group Members

```delphi
Server.OnClientConnect := procedure(AClient: TWebSocketClient)
begin
  // When someone joins a group, notify others
  AClient.JoinGroup('lobby');
  AClient.SendToGroup('lobby', Format('%s joined the room', [AClient.Username]));
end;

Server.OnClientDisconnect := procedure(AClient: TWebSocketClient)
begin
  // Notify all groups when someone disconnects
  for var LGroup in AClient.Groups do
  begin
    var LGroupClients := Server.GetClientsByGroup(LGroup);
    for var LClient in LGroupClients do
    begin
      if LClient <> AClient then
        LClient.SendText(Format('%s disconnected', [AClient.Username]));
    end;
  end;
end;
```

### 5. Using Custom Usernames

```delphi
Server.OnClientConnect := procedure(AClient: TWebSocketClient)
begin
  // Default username is ClientId (IP address)
  Writeln(AClient.Username); // e.g., "127.0.0.1"

  // Set a custom username
  AClient.Username := 'Alice';

  // Use it in messages
  AClient.Broadcast(Format('%s has joined', [AClient.Username]));
end;

// Find a client by username
var LClient := Server.FindClientByUsername('Alice');
if Assigned(LClient) then
  LClient.SendText('Hello Alice!');
```

## 📚 Available Commands

All commands are implemented in `ProcessCommand()` procedure:

| Command | Description | Example |
|---------|-------------|---------|
| `/help` | Show available commands | `/help` |
| `/setname <username>` | Set your username | `/setname Alice` |
| `/join <room>` | Join a chat room | `/join lobby` |
| `/leave <room>` | Leave a chat room | `/leave lobby` |
| `/rooms` | List your current rooms | `/rooms` |
| `/users <room>` | List users in a room | `/users lobby` |
| `/msg <room> <message>` | Send message to a room | `/msg lobby Hello!` |

## 🏗️ Architecture

### Group Management
- Groups are created dynamically when first client joins
- No need to pre-create rooms
- Groups are automatically cleaned up when empty
- Each client can be in multiple groups simultaneously

### Message Routing
```
Client A (in: lobby, dev)
Client B (in: lobby)
Client C (in: dev)

A sends to "lobby" → B receives (C doesn't)
A sends to "dev" → C receives (B doesn't)
```

### Client Properties
```delphi
TWebSocketClient properties:
- Username: string           // Custom username (defaults to ClientId)
- ClientId: string          // Unique identifier (IP address)
- Groups: TArray<string>    // Array of groups this client belongs to
- Data: TObject            // Custom session data
```

### Client Methods
```delphi
// Group management
AClient.JoinGroup(groupName);
AClient.LeaveGroup(groupName);
AClient.IsInGroup(groupName): Boolean;

// Messaging
AClient.SendText(msg);                    // Send to this client
AClient.SendToGroup(group, msg): Integer; // Send to a group
AClient.SendTo(username, msg): Boolean;   // Send to specific user
AClient.Broadcast(msg);                   // Send to all
AClient.BroadcastToPeers(msg);           // Send to all except self
```

## 📝 Use Cases

This pattern is perfect for:
- 🏠 **Multi-room chat applications** (Slack, Discord-style)
- 🎮 **Game lobbies** (players join different game rooms)
- 📢 **Topic-based notifications** (subscribe to specific channels)
- 👥 **Departmental messaging** (sales, support, engineering groups)
- 🏢 **Multi-tenant systems** (isolate customers by tenant)

## 🎨 Implementation Tips

### 1. Auto-join Default Room
```delphi
Server.OnClientConnect := procedure(AClient: TWebSocketClient)
begin
  AClient.JoinGroup('lobby'); // Everyone starts in lobby
  AClient.SendToGroup('lobby', Format('%s joined', [AClient.Username]));
end;
```

### 2. Private Messaging via Groups
```delphi
// Create private room name from two usernames
var LRoomName := Format('private_%s_%s', [MinStr(User1, User2), MaxStr(User1, User2)]);
AClient1.JoinGroup(LRoomName);
AClient2.JoinGroup(LRoomName);
AClient1.SendToGroup(LRoomName, 'Private message');
```

### 3. Admin/Moderator Groups
```delphi
Server.OnClientConnect := procedure(AClient: TWebSocketClient)
begin
  if IsAdmin(AClient.Username) then
    AClient.JoinGroup('admins');
end;

// Send admin announcement
var LAdmins := Server.GetClientsByGroup('admins');
for var LAdmin in LAdmins do
  LAdmin.SendText('Admin notification');
```

### 4. Rate Limiting per Group
```delphi
// Track message count per group
if GetGroupMessageCount('lobby') > 100 then
  AClient.SendText('Lobby is too busy, try later');
```

## 🔧 Customization Ideas

1. **Persistent rooms** - Store room list in database
2. **Room passwords** - Require password to join certain rooms
3. **Max users per room** - Limit room capacity
4. **Room ownership** - Track who created each room
5. **Kick/ban users** - Moderator commands to remove users
6. **Message history** - Send recent messages when joining
7. **Typing indicators** - Show who's typing in each room

## 📚 Related Demos

- **websocket_primer** - Basic echo server with periodic messages
- **websocket_chat** - Simple broadcast chat (single room)

## 🐛 Troubleshooting

**Can't join room?**
→ Check if already in room with `/rooms` command

**Messages not received?**
→ Ensure you've joined the room with `/join <room>` first

**Port conflict?**
→ Change port in `TMVCWebSocketServer.Create(9092)`

## 📄 License

Apache License 2.0
