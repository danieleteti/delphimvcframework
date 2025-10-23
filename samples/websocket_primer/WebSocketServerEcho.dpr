program WebSocketServerEcho;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Generics.Collections,
  System.SyncObjs,
  MVCFramework.WebSocket.Server in '..\..\sources\MVCFramework.WebSocket.Server.pas',
  MVCFramework.WebSocket in '..\..\sources\MVCFramework.WebSocket.pas';

type
  /// <summary>
  /// Per-client session data
  /// This object is created in OnClientConnect and automatically freed on disconnect
  /// </summary>
  TClientSessionData = class
  private
    FMessageCount: Integer;
  public
    constructor Create;
    property MessageCount: Integer read FMessageCount write FMessageCount;
  end;

{ TClientSessionData }

constructor TClientSessionData.Create;
begin
  inherited;
  FMessageCount := 0;
end;

var
  Server: TMVCWebSocketServer;

begin
  ReportMemoryLeaksOnShutdown := True;

  try
    Writeln('=== WebSocket Echo Server with Periodic Messages ===');
    Writeln('');
    Writeln('Starting echo server on port 9091...');

    Server := TMVCWebSocketServer.Create(9091);
    try
      // ============================================================================
      // OnLog - OPTIONAL
      // Called for: Internal server events (handshake success/failure, errors, etc.)
      // Use for: Debugging, monitoring server activity, logging
      // ============================================================================
      Server.OnLog := procedure(const AMessage: string)
      begin
        Writeln(Format('[%s] %s', [TimeToStr(Now), AMessage]));
      end;

      // ============================================================================
      // OnClientConnect - OPTIONAL but RECOMMENDED
      // Called when: A client successfully connects after WebSocket handshake completes
      // Use for:
      //   - Initialize per-client session data (AClient.Data := TMyClass.Create)
      //   - Set per-client periodic message interval (AInitialInterval := 5000)
      //   - Log connection events
      //   - Authenticate/authorize clients
      //   - Send welcome messages
      // Parameters:
      //   - AClient: The connected client object
      //   - AInitialInterval: var parameter - set to override server default periodic interval
      // ============================================================================
      Server.OnClientConnect := procedure(AClient: TWebSocketClient; var AInitialInterval: Integer)
      begin
        Writeln(Format('[%s] CLIENT CONNECTED: %s', [TimeToStr(Now), AClient.ClientId]));

        // Create session data for this client (will be automatically freed on disconnect)
        AClient.Data := TClientSessionData.Create;

        // Set different intervals based on client IP
        if AClient.ClientId.StartsWith('192.168.') then
        begin
          AInitialInterval := 3000; // Local network: every 3 seconds
          Writeln(Format('  -> Local client, setting interval to 3 seconds', []));
        end
        else if AClient.ClientId = '127.0.0.1' then
        begin
          AInitialInterval := 2000; // Localhost: every 2 seconds (for testing)
          Writeln(Format('  -> Localhost client, setting interval to 2 seconds', []));
        end
        else
        begin
          AInitialInterval := 10000; // Others: every 10 seconds
          Writeln(Format('  -> Remote client, setting interval to 10 seconds', []));
        end;
      end;

      // ============================================================================
      // OnClientDisconnect - OPTIONAL
      // Called when: A client disconnects (gracefully or due to error)
      // Use for:
      //   - Log disconnection events
      //   - Read final statistics from AClient.Data (available before it's freed)
      //   - Notify other clients
      //   - Clean up external resources
      // Note: AClient.Data is freed automatically AFTER this event
      // ============================================================================
      Server.OnClientDisconnect := procedure(AClient: TWebSocketClient)
      var
        LSessionData: TClientSessionData;
      begin
        Writeln(Format('[%s] CLIENT DISCONNECTED: %s', [TimeToStr(Now), AClient.ClientId]));
        if AClient.Data <> nil then
        begin
          LSessionData := TClientSessionData(AClient.Data);
          Writeln(Format('  -> Total periodic messages sent to this client: %d', [LSessionData.MessageCount]));
        end;
      end;

      // ============================================================================
      // OnMessage - REQUIRED (or override HandleTextMessage)
      // Called when: A text message is received from a client
      // Use for:
      //   - Process incoming text messages
      //   - Send responses via Server.SendTextToClient(AClient, 'response')
      //   - Broadcast to all clients via Server.BroadcastText('message')
      //   - Access client session data via AClient.Data
      // Parameters:
      //   - AClient: The client who sent the message
      //   - AMessage: The text message content (UTF-8)
      // ============================================================================
      Server.OnMessage := procedure(AClient: TWebSocketClient; const AMessage: string)
      begin
        Writeln(Format('[%s] Message from %s: %s', [TimeToStr(Now), AClient.ClientId, AMessage]));

        // Echo back the message
        Server.SendTextToClient(AClient, Format('Echo: %s', [AMessage]));
      end;

      // ============================================================================
      // OnBinaryData - OPTIONAL
      // Called when: Binary data is received from a client
      // Use for:
      //   - Process binary messages (files, images, protocol buffers, etc.)
      //   - Send binary responses via Server.SendBinaryToClient(AClient, ByteArray)
      //   - Broadcast binary data via Server.BroadcastBinary(ByteArray)
      // Parameters:
      //   - AClient: The client who sent the data
      //   - AData: TBytes containing the binary payload
      // Not used in this example - similar pattern to OnMessage
      // ============================================================================

      // ============================================================================
      // OnError - OPTIONAL but RECOMMENDED
      // Called when: An error occurs during frame processing (invalid frames, etc.)
      // Use for:
      //   - Log errors for debugging
      //   - Handle protocol violations
      //   - Implement custom error recovery
      //   - Notify monitoring systems
      // Parameters:
      //   - AClient: The client that caused the error
      //   - AError: Error message description
      // ============================================================================
      Server.OnError := procedure(AClient: TWebSocketClient; const AError: string)
      begin
        Writeln(Format('[%s] ERROR from %s: %s', [TimeToStr(Now), AClient.ClientId, AError]));
      end;

      // ============================================================================
      // OnPeriodicMessage - OPTIONAL
      // Called when: Periodically at specified interval (if PeriodicMessageInterval > 0)
      // Use for:
      //   - Send heartbeat/keepalive messages
      //   - Push server-initiated updates (stock prices, notifications, etc.)
      //   - Implement per-client timing logic
      //   - Monitor client connection health
      // Parameters:
      //   - AClient: The client to send message to
      //   - ACurrentInterval: var parameter - modify to change timing dynamically
      // Returns:
      //   - Message string to send (return empty string to skip sending)
      // Note:
      //   - Set Server.PeriodicMessageInterval > 0 OR AInitialInterval in OnClientConnect to enable
      //   - Interval can be changed per-client via ACurrentInterval parameter
      //   - Also accessible via AClient.PeriodicInterval property
      // ============================================================================
      Server.OnPeriodicMessage := function(AClient: TWebSocketClient; var ACurrentInterval: Integer): string
      var
        LSessionData: TClientSessionData;
      begin
        // Get client session data
        LSessionData := TClientSessionData(AClient.Data);
        Inc(LSessionData.FMessageCount);

        // After 5 messages, slow down progressively (demonstrates dynamic interval adjustment)
        if LSessionData.MessageCount > 5 then
        begin
          ACurrentInterval := ACurrentInterval + 1000; // Add 1 second each time
          Result := Format('[SERVER HEARTBEAT #%d] Time: %s | Interval: %d ms (slowing down)',
            [LSessionData.MessageCount, FormatDateTime('hh:nn:ss', Now), ACurrentInterval]);
        end
        else
        begin
          Result := Format('[SERVER HEARTBEAT #%d] Time: %s | Interval: %d ms',
            [LSessionData.MessageCount, FormatDateTime('hh:nn:ss', Now), ACurrentInterval]);
        end;

        Writeln(Format('[%s] Sent periodic message to %s: %s', [TimeToStr(Now), AClient.ClientId, Result]));
      end;

      Server.Active := True;

      Writeln('Echo server running!');
      Writeln('');
      Writeln('Connect with:');
      Writeln('  ws://localhost:9091/');
      Writeln('');
      Writeln('Features:');
      Writeln('  - Echoes back all text messages');
      Writeln('  - Responds to Ping with Pong');
      Writeln('  - Sends periodic heartbeat messages (interval varies by client)');
      Writeln('  - Interval increases after 5 messages (demonstrates dynamic adjustment)');
      Writeln('  - Per-client session data (message counter)');
      Writeln('');
      Writeln('Press ENTER to stop...');
      Writeln('');

      Readln;

      Writeln('Stopping server...');

    finally
      Server.Free;
    end;

    Writeln('Done.');

  except
    on E: Exception do
    begin
      Writeln('ERROR: ' + E.ClassName + ': ' + E.Message);
      Readln;
      ExitCode := 1;
    end;
  end;
end.
