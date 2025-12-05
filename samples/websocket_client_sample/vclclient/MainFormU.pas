// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2025 Daniele Teti and the DMVCFramework T, Vcl.StdCtrlseam
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

unit MainFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  MVCFramework.WebSocket.Client, MVCFramework.WebSocket;

type
  /// <summary>
  /// Main form for the WebSocket Client Demo.
  ///
  /// This VCL application demonstrates how to use DMVCFramework's WebSocket client
  /// to connect to any standards-compliant WebSocket server. In this example,
  /// we connect to a Python WebSocket server to prove cross-language compatibility.
  ///
  /// Features:
  /// - Connect/Disconnect from WebSocket server
  /// - Send text messages to server
  /// - Receive and display server responses
  /// - Visual connection status indicator
  /// - Message log with timestamps
  ///
  /// The WebSocket client runs on a background thread and uses TThread.Synchronize
  /// to update the UI safely from event handlers.
  /// </summary>
  TMainForm = class(TForm)
    edtServerURL: TEdit;
    btnConnect: TButton;
    edtName: TEdit;
    btnSend: TButton;
    Memo1: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblStatus: TLabel;
    btnDisconnect: TButton;
    btnClear: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
  private
    /// <summary>
    /// The WebSocket client instance.
    /// Created with the server URL in FormCreate and destroyed in FormDestroy.
    /// </summary>
    FWebSocketClient: TMVCWebSocketClient;

    /// <summary>
    /// Adds a timestamped message to the log memo.
    /// </summary>
    procedure Log(const AMessage: string);

    /// <summary>
    /// Called when WebSocket connection is successfully established.
    /// This event is fired from a background thread, so we use TThread.Synchronize
    /// to safely update the UI (log and enable/disable buttons).
    /// </summary>
    procedure OnConnect(Sender: TMVCWebSocketClient);

    /// <summary>
    /// Called when WebSocket connection is closed.
    /// Receives the close code (normal closure, error, etc.) and reason string.
    /// Uses TThread.Synchronize for thread-safe UI updates.
    /// </summary>
    procedure OnDisconnect(Sender: TMVCWebSocketClient; ACode: TMVCWebSocketCloseCode; const AReason: string);

    /// <summary>
    /// Called when a text message is received from the server.
    /// The message is displayed in the log with a timestamp.
    /// Uses TThread.Synchronize for thread-safe UI updates.
    /// </summary>
    procedure OnTextMessage(Sender: TMVCWebSocketClient; const AMessage: string);

    /// <summary>
    /// Called when a WebSocket error occurs (connection failure, protocol error, etc.).
    /// Uses TThread.Synchronize for thread-safe UI updates.
    /// </summary>
    procedure OnError(Sender: TMVCWebSocketClient; const AError: Exception);

    /// <summary>
    /// Updates UI controls based on connection state.
    /// Enables/disables buttons and changes status label color.
    /// Must be called from the main thread.
    /// </summary>
    procedure UpdateUI;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Create WebSocket client with default URL
  // The URL can be changed by the user before connecting
  FWebSocketClient := TMVCWebSocketClient.Create('ws://localhost:8765');

  // Assign event handlers for WebSocket events
  // These handlers will be called from a background thread,
  // so they must use TThread.Synchronize to update the UI
  FWebSocketClient.OnConnect := OnConnect;
  FWebSocketClient.OnDisconnect := OnDisconnect;
  FWebSocketClient.OnTextMessage := OnTextMessage;
  FWebSocketClient.OnError := OnError;

  // Initialize UI state (all controls disabled except Connect button)
  UpdateUI;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  // Clean up: disconnect and free the WebSocket client
  if FWebSocketClient.IsConnected then
    FWebSocketClient.Disconnect;
  FWebSocketClient.Free;
end;

procedure TMainForm.Log(const AMessage: string);
begin
  // Add timestamped message to the log memo
  // Format: [HH:MM:SS] Message
  Memo1.Lines.Add(Format('[%s] %s', [FormatDateTime('hh:nn:ss', Now), AMessage]));
end;

procedure TMainForm.btnConnectClick(Sender: TObject);
begin
  try
    // Check if URL has changed - if so, recreate the client
    // TMVCWebSocketClient is created with a URL that cannot be changed later
    if FWebSocketClient.URL <> edtServerURL.Text then
    begin
      Log('Recreating client for new URL: ' + edtServerURL.Text);

      // Free old client and create new one with new URL
      FWebSocketClient.Free;
      FWebSocketClient := TMVCWebSocketClient.Create(edtServerURL.Text);

      // Re-assign all event handlers
      FWebSocketClient.OnConnect := OnConnect;
      FWebSocketClient.OnDisconnect := OnDisconnect;
      FWebSocketClient.OnTextMessage := OnTextMessage;
      FWebSocketClient.OnError := OnError;
    end;

    Log('Connecting to ' + edtServerURL.Text + '...');

    // Initiate connection to WebSocket server
    // This method returns immediately - the actual connection happens asynchronously
    // The OnConnect event will be triggered when connection succeeds
    FWebSocketClient.Connect;

    Log('Connect method returned');
  except
    on E: Exception do
    begin
      Log('EXCEPTION during connect: ' + E.ClassName + ': ' + E.Message);
      UpdateUI;
    end;
  end;
end;

procedure TMainForm.btnDisconnectClick(Sender: TObject);
begin
  // Close the WebSocket connection
  if FWebSocketClient.IsConnected then
  begin
    Log('Disconnecting...');
    FWebSocketClient.Disconnect;
    // OnDisconnect event will be triggered after disconnection
  end;
end;

procedure TMainForm.btnSendClick(Sender: TObject);
begin
  // Send a text message to the server
  // The server will echo it back with a timestamp
  if FWebSocketClient.IsConnected then
  begin
    Log('Sending name: ' + edtName.Text);
    FWebSocketClient.SendText(edtName.Text);
    // The response will arrive via OnTextMessage event
  end
  else
    ShowMessage('Not connected to server!');
end;

procedure TMainForm.btnClearClick(Sender: TObject);
begin
  // Clear the message log
  Memo1.Clear;
end;

procedure TMainForm.OnConnect(Sender: TMVCWebSocketClient);
begin
  // IMPORTANT: This event is called from a background thread!
  // We use TThread.Queue (non-blocking) instead of TThread.Synchronize (blocking)
  // because we don't need the WebSocket thread to wait for UI updates
  TThread.Queue(nil,
    procedure
    begin
      Log('*** CONNECTED ***');
      UpdateUI; // Enable/disable buttons based on new connection state
    end);
end;

procedure TMainForm.OnDisconnect(Sender: TMVCWebSocketClient; ACode: TMVCWebSocketCloseCode; const AReason: string);
begin
  // IMPORTANT: This event is called from a background thread!
  // ACode indicates the reason for disconnection:
  //   - Normal closure (1000)
  //   - Going away (1001)
  //   - Protocol error (1002)
  //   - etc.
  // Using TThread.Queue for non-blocking execution
  TThread.Queue(nil,
    procedure
    begin
      Log(Format('*** DISCONNECTED *** (Code: %d, Reason: %s)', [Ord(ACode), AReason]));
      UpdateUI; // Enable/disable buttons based on new connection state
    end);
end;

procedure TMainForm.OnTextMessage(Sender: TMVCWebSocketClient; const AMessage: string);
begin
  // IMPORTANT: This event is called from a background thread!
  // This is triggered when the server sends us a text message
  // In our case, the Python server echoes our message with a timestamp
  //
  // Using TThread.Queue instead of TThread.Synchronize because:
  // - We don't need to block the WebSocket receive thread
  // - Messages may arrive in rapid succession
  // - Better performance for high-frequency events
  // - Message order is still preserved by the queue
  TThread.Queue(nil,
    procedure
    begin
      Log('Received: ' + AMessage);
    end);
end;

procedure TMainForm.OnError(Sender: TMVCWebSocketClient; const AError: Exception);
begin
  // IMPORTANT: This event is called from a background thread!
  // This handles WebSocket errors like connection failures, protocol errors, etc.
  // Using TThread.Queue for non-blocking error reporting
  TThread.Queue(nil,
    procedure
    begin
      Log('ERROR: ' + AError.Message);
    end);
end;

procedure TMainForm.UpdateUI;
var
  IsConnected: Boolean;
begin
  // Update UI controls based on connection state
  // This method should only be called from the main thread!
  IsConnected := FWebSocketClient.IsConnected;

  // Enable/disable buttons based on connection state
  btnConnect.Enabled := not IsConnected;      // Can only connect when disconnected
  btnDisconnect.Enabled := IsConnected;        // Can only disconnect when connected
  btnSend.Enabled := IsConnected;          // Can only send messages when connected
  edtServerURL.Enabled := not IsConnected;     // Can only change URL when disconnected

  // Update visual status indicator
  if IsConnected then
  begin
    lblStatus.Caption := 'Connected';
    lblStatus.Font.Color := clGreen;
  end
  else
  begin
    lblStatus.Caption := 'Disconnected';
    lblStatus.Font.Color := clRed;
  end;
end;

end.
