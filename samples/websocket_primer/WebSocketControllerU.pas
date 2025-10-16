unit WebSocketControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.WebSocket.Controller,
  MVCFramework.WebSocket,
  MVCFramework.WebSocket.ConnectionManager,
  System.SysUtils,
  System.DateUtils,
  LoggerPro;

type
  /// <summary>
  /// Simple WebSocket echo controller
  /// Echoes back any text message received
  /// </summary>
  [MVCPath('/ws/echo')]
  TWebSocketEchoController = class(TMVCWebSocketController)
  protected
    procedure OnConnect(AConnection: TMVCWebSocketConnection); override;
    procedure OnDisconnect(AConnection: TMVCWebSocketConnection;
      ACode: TMVCWebSocketCloseCode; const AReason: string); override;
    procedure OnTextMessage(AConnection: TMVCWebSocketConnection;
      const AMessage: string); override;
    procedure OnBinaryMessage(AConnection: TMVCWebSocketConnection;
      const AData: TBytes); override;
    procedure OnError(AConnection: TMVCWebSocketConnection;
      const AError: Exception); override;
  end;

  /// <summary>
  /// WebSocket chat controller
  /// Broadcasts messages to all connected clients
  /// </summary>
  [MVCPath('/ws/chat')]
  TWebSocketChatController = class(TMVCWebSocketController)
  protected
    procedure OnConnect(AConnection: TMVCWebSocketConnection); override;
    procedure OnDisconnect(AConnection: TMVCWebSocketConnection;
      ACode: TMVCWebSocketCloseCode; const AReason: string); override;
    procedure OnTextMessage(AConnection: TMVCWebSocketConnection;
      const AMessage: string); override;
  end;

implementation

uses
  System.JSON,
  MVCFramework.Serializer.Defaults,
  MVCFramework.Serializer.Intf;

{ TWebSocketEchoController }

procedure TWebSocketEchoController.OnConnect(AConnection: TMVCWebSocketConnection);
begin
  inherited;
  Log.Info('Echo: Client connected - ' + AConnection.ConnectionId, 'WebSocket');

  // Send welcome message
  AConnection.SendText('Welcome to WebSocket Echo Server! Connection ID: ' + AConnection.ConnectionId);
  AConnection.SendText('Server Time: ' + DateTimeToStr(Now));
  AConnection.SendText('Send any text message and it will be echoed back to you.');
end;

procedure TWebSocketEchoController.OnDisconnect(AConnection: TMVCWebSocketConnection;
  ACode: TMVCWebSocketCloseCode; const AReason: string);
begin
  inherited;
  Log.Info(Format('Echo: Client disconnected - %s (Code: %d, Reason: %s)',
    [AConnection.ConnectionId, Ord(ACode), AReason]), 'WebSocket');
end;

procedure TWebSocketEchoController.OnTextMessage(AConnection: TMVCWebSocketConnection;
  const AMessage: string);
var
  Response: string;
begin
  inherited;
  Log.Info('Echo: Received message: ' + AMessage, 'WebSocket');

  // Echo the message back with timestamp
  Response := Format('[%s] Echo: %s', [FormatDateTime('hh:nn:ss', Now), AMessage]);
  AConnection.SendText(Response);
end;

procedure TWebSocketEchoController.OnBinaryMessage(AConnection: TMVCWebSocketConnection;
  const AData: TBytes);
begin
  inherited;
  Log.Info(Format('Echo: Received %d bytes of binary data', [Length(AData)]), 'WebSocket');

  // Echo binary data back
  AConnection.SendBinary(AData);
end;

procedure TWebSocketEchoController.OnError(AConnection: TMVCWebSocketConnection;
  const AError: Exception);
begin
  inherited;
  Log.Error('Echo: Error - ' + AError.Message, 'WebSocket');
end;

{ TWebSocketChatController }

procedure TWebSocketChatController.OnConnect(AConnection: TMVCWebSocketConnection);
var
  WelcomeMsg: string;
begin
  inherited;
  Log.Info('Chat: Client connected - ' + AConnection.ConnectionId, 'WebSocket');

  // Send welcome message to the new client
  WelcomeMsg := Format('Welcome to WebSocket Chat! You are user %s', [AConnection.ConnectionId]);
  AConnection.SendText(WelcomeMsg);

  // Broadcast to all clients
  BroadcastText(Format('User %s joined the chat. Total users: %d',
    [AConnection.ConnectionId, GetConnectionCount]));
end;

procedure TWebSocketChatController.OnDisconnect(AConnection: TMVCWebSocketConnection;
  ACode: TMVCWebSocketCloseCode; const AReason: string);
begin
  inherited;
  Log.Info(Format('Chat: Client disconnected - %s', [AConnection.ConnectionId]), 'WebSocket');

  // Notify all remaining clients
  BroadcastText(Format('User %s left the chat. Remaining users: %d',
    [AConnection.ConnectionId, GetConnectionCount - 1]));
end;

procedure TWebSocketChatController.OnTextMessage(AConnection: TMVCWebSocketConnection;
  const AMessage: string);
var
  BroadcastMsg: string;
  JsonMsg: TJSONObject;
begin
  inherited;
  Log.Info(Format('Chat: Message from %s: %s', [AConnection.ConnectionId, AMessage]), 'WebSocket');

  // Parse JSON if it's JSON format
  if AMessage.TrimLeft.StartsWith('{') then
  begin
    try
      JsonMsg := TJSONObject.ParseJSONValue(AMessage) as TJSONObject;
      try
        if Assigned(JsonMsg) then
        begin
          // Handle structured messages
          BroadcastMsg := Format('[%s] %s: %s',
            [FormatDateTime('hh:nn:ss', Now),
             AConnection.ConnectionId,
             JsonMsg.GetValue<string>('message')]);
        end
        else
          BroadcastMsg := Format('[%s] %s: %s',
            [FormatDateTime('hh:nn:ss', Now), AConnection.ConnectionId, AMessage]);
      finally
        JsonMsg.Free;
      end;
    except
      BroadcastMsg := Format('[%s] %s: %s',
        [FormatDateTime('hh:nn:ss', Now), AConnection.ConnectionId, AMessage]);
    end;
  end
  else
  begin
    // Plain text message
    BroadcastMsg := Format('[%s] %s: %s',
      [FormatDateTime('hh:nn:ss', Now), AConnection.ConnectionId, AMessage]);
  end;

  // Broadcast to all connected clients
  BroadcastText(BroadcastMsg);
end;

end.
