#!/usr/bin/env python
# ***************************************************************************
#
# Delphi MVC Framework
#
# Copyright (c) 2010-2025 Daniele Teti and the DMVCFramework Team
#
# https://github.com/danieleteti/delphimvcframework
#
# ***************************************************************************
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# ***************************************************************************

"""
WebSocket Echo Server (Python)

This is a simple WebSocket server that demonstrates interoperability with
DMVCFramework's WebSocket client. It shows that WebSocket is a standard
protocol that works across different programming languages and platforms.

The server:
- Listens on ws://localhost:8765
- Accepts WebSocket connections from any client
- Receives text messages from connected clients
- Echoes each message back with a server timestamp
- Maintains persistent connections for multiple message exchanges

This example proves that DMVCFramework's WebSocket client can communicate
with any standards-compliant WebSocket server, not just Delphi servers.

Usage:
    pip install websockets
    python websocket_server.py

Then connect with the Delphi VCL client application.
"""

import asyncio
from datetime import datetime

from websockets.asyncio.server import serve


async def hello(websocket):
    """
    Handle WebSocket client connection.

    This coroutine is called for each new WebSocket client connection.
    It maintains the connection and processes messages in a loop until
    the client disconnects.

    Args:
        websocket: The WebSocket connection object
    """
    # Log when a client connects (shows IP and port)
    print(f"[INFO] Client connected from {websocket.remote_address}")

    try:
        # Keep connection open and handle multiple messages
        # The 'async for' loop receives messages as they arrive
        # and keeps the connection alive until the client disconnects
        async for message in websocket:
            # Log received message to console
            print(f"<<< Received: {message}")

            # Create response with server timestamp to prove it's server-generated
            # This demonstrates bidirectional WebSocket communication
            server_time = datetime.now().strftime("%H:%M:%S")
            response = f"[{server_time}] Echo: {message}"

            # Send response back to client
            await websocket.send(response)

            # Log sent message to console
            print(f">>> Sent: {response}")

    except Exception as e:
        # Handle any errors during message processing
        # (e.g., connection drops, protocol errors)
        print(f"[ERROR] {e}")

    finally:
        # Always execute when connection closes, whether normally or due to error
        print(f"[INFO] Client disconnected")


async def main():
    """
    Main server entry point.

    Creates and starts the WebSocket server on localhost:8765.
    The server runs indefinitely until interrupted (Ctrl+C).
    """
    # Create WebSocket server
    # - hello: handler function called for each new connection
    # - "localhost": bind to local machine only (use "0.0.0.0" for all interfaces)
    # - 8765: port number to listen on
    async with serve(hello, "localhost", 8765) as server:
        print("[INFO] WebSocket server started on ws://localhost:8765")
        print("[INFO] Press Ctrl+C to stop")

        # Run server forever (until Ctrl+C or process killed)
        await server.serve_forever()


if __name__ == "__main__":
    # Entry point when script is run directly
    # asyncio.run() creates event loop and runs the main coroutine
    asyncio.run(main())