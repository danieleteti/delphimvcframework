// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2025 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ***************************************************************************

/// <summary>
/// DMVCFramework WebSocket Client Demo
///
/// This application demonstrates how to use TMVCWebSocketClient to connect
/// to any standards-compliant WebSocket server. The demo connects to a
/// Python WebSocket echo server to prove cross-platform interoperability.
///
/// Key Features:
/// - Simple VCL interface for WebSocket operations
/// - Connect/Disconnect from WebSocket servers
/// - Send and receive text messages
/// - Event-driven architecture with background thread handling
/// - Thread-safe UI updates using TThread.Synchronize
///
/// Before running this demo:
/// 1. Install Python websockets package: pip install websockets
/// 2. Start the Python server: python websocket_server.py
/// 3. Run this application and click "Connect"
/// 4. Type a name and click "Send Name" to see the echo response
///
/// The server will respond with: [HH:MM:SS] Echo: {your message}
///
/// This demo proves that DMVCFramework's WebSocket client works with
/// any WebSocket server, not just Delphi/DMVCFramework servers.
/// </summary>
program WebSocketClientVCL;

uses
  Vcl.Forms,
  MainFormU in 'MainFormU.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
