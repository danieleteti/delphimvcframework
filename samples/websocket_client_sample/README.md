# DMVCFramework WebSocket Client Demo

This sample demonstrates how to use the DMVCFramework WebSocket **client** to connect to a WebSocket server written in Python (or any other language).

## Overview

This demo shows that DMVCFramework's WebSocket client can communicate with any standard WebSocket server, not just Delphi servers. The example uses a simple Python WebSocket server that echoes back a greeting message.

## Components

### 1. Python WebSocket Server (`websocket_server.py`)

A simple Python WebSocket server that:
- Listens on `ws://localhost:8765`
- Receives a name from the client
- Responds with "Hello {name}!"

### 2. Delphi VCL Client (`vclclient\WebSocketClientVCL.dpr`)

A Delphi VCL application that:
- Connects to the Python WebSocket server
- Sends the user's name
- Displays the server's greeting response
- Shows connection status and message log

## How to Run

### 1. Start the Python Server

First, install the required Python package:

```bash
pip install websockets
```

Then run the server:

```bash
python websocket_server.py
```

The server will start listening on `ws://localhost:8765`

### 2. Run the Delphi Client

1. Open `vclclient\WebSocketClientVCL.dproj` in Delphi IDE
2. Build and run the application
3. Click "Connect" button
4. Enter your name in the text field
5. Click "Send Name" button
6. The server will respond with "Hello {your name}!"

## Features Demonstrated

- **Cross-platform WebSocket communication**: Delphi client connecting to Python server
- **TMVCWebSocketClient usage**: How to use DMVCFramework's WebSocket client class
- **Event-driven architecture**: OnConnect, OnDisconnect, OnMessage, OnError events
- **Connection management**: Connect, disconnect, send messages
- **UI updates**: Enable/disable controls based on connection state

## API Usage

```delphi
// Create the client
FWebSocketClient := TMVCWebSocketClient.Create;

// Set up event handlers
FWebSocketClient.OnConnect := OnConnect;
FWebSocketClient.OnDisconnect := OnDisconnect;
FWebSocketClient.OnMessage := OnMessage;
FWebSocketClient.OnError := OnError;

// Connect to server
FWebSocketClient.Connect('ws://localhost:8765');

// Send a message
FWebSocketClient.SendText('Daniele');

// Disconnect
FWebSocketClient.Disconnect;
```

## Requirements

### Python Server
- Python 3.7 or later
- websockets package (`pip install websockets`)

### Delphi Client
- Delphi 10.2 Tokyo or later
- DMVCFramework

## License

Copyright (c) 2010-2025 Daniele Teti and the DMVCFramework Team

Licensed under the Apache License, Version 2.0
