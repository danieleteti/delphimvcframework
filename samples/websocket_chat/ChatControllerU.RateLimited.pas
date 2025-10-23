// EXAMPLE: ChatController with Rate Limiting Configuration
// Copy these changes to your ChatControllerU.pas

constructor TChatController.Create;
begin
  inherited Create;

  // Configure WebSocket settings
  PingInterval := 30; // Send ping every 30 seconds
  MaxMessageSize := 10 * 1024; // 10KB max message size

  // ============================================================
  // RATE LIMITING CONFIGURATION (NEW!)
  // ============================================================

  // Configure rate limiting for chat application
  // Default values are reasonable (30 msg/sec, 100KB/sec)
  // Customize based on your needs:

  MaxMessagesPerSecond := 10;      // Allow 10 messages per second (prevents spam)
  MaxBytesPerSecond := 50 * 1024;  // Allow 50KB per second (adequate for text chat)

  // To disable rate limiting completely, uncomment these lines:
  // MaxMessagesPerSecond := 0;
  // MaxBytesPerSecond := 0;

  // ============================================================
  // RATE LIMITING FOR DIFFERENT USER TYPES
  // ============================================================
  // If you want different limits based on user type, you can do it in OnConnect:
end;

// Example: Different limits for different user types
procedure TChatController.OnConnect(AConnection: TMVCWebSocketConnection);
var
  lUserType: string;
begin
  // Add to global manager (IMPORTANT!)
  GChatConnectionManager.AddConnection(AConnection);

  // Example: Get user type from custom data or authentication
  // lUserType := GetUserType(AConnection);

  // Uncomment to enable per-user-type rate limiting:
  {
  if lUserType = 'premium' then
  begin
    MaxMessagesPerSecond := 50;      // Premium: 50 msg/sec
    MaxBytesPerSecond := 200 * 1024; // Premium: 200KB/sec
  end
  else if lUserType = 'basic' then
  begin
    MaxMessagesPerSecond := 20;      // Basic: 20 msg/sec
    MaxBytesPerSecond := 100 * 1024; // Basic: 100KB/sec
  end
  else // free
  begin
    MaxMessagesPerSecond := 10;      // Free: 10 msg/sec
    MaxBytesPerSecond := 50 * 1024;  // Free: 50KB/sec
  end;
  }

  // Send welcome message and history
  // ... rest of OnConnect code ...
end;

// Example: Handle rate limiting errors
procedure TChatController.OnError(AConnection: TMVCWebSocketConnection;
  const AError: Exception);
begin
  if AError is EMVCWebSocketException then
  begin
    if AError.Message.Contains('Rate limit') then
    begin
      Log.Warn(Format(
        'Rate limit exceeded for connection %s',
        [AConnection.ConnectionId]
      ), 'Chat');

      // Optionally: Send warning to the user
      try
        AConnection.SendText(
          '{"type":"error","message":"You are sending messages too quickly. Please slow down."}'
        );
      except
        // If sending fails (rate limited), close the connection
        AConnection.Close(
          TMVCWebSocketCloseCode.PolicyViolation,
          'Rate limit exceeded'
        );
      end;
    end;
  end;

  inherited;
end;
