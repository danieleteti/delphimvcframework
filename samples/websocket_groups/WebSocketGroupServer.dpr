program WebSocketGroupServer;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MVCFramework.WebSocket.Server,
  MVCFramework.WebSocket;

type
  TClientSessionData = class
  private
    FUsername: string;
  public
    property Username: string read FUsername write FUsername;
  end;

var
  Server: TMVCWebSocketServer;

procedure ProcessCommand(AClient: TWebSocketClient; const AMessage: string);
var
  LCommand, LArg: string;
  LGroupClients: TArray<TWebSocketClient>;
  LSessionData: TClientSessionData;
  I: Integer;
  LSpacePos: Integer;
  LRoomName: string;
  LMessage: string;
begin
  if not AMessage.StartsWith('/') then
  begin
    AClient.SendText('Unknown command. Type /help for available commands.');
    Exit;
  end;

  // Split command from arguments (only split on first space)
  LSpacePos := Pos(' ', AMessage);
  if LSpacePos > 0 then
  begin
    LCommand := Copy(AMessage, 1, LSpacePos - 1).ToLower;
    LArg := Copy(AMessage, LSpacePos + 1, Length(AMessage));
  end
  else
  begin
    LCommand := AMessage.ToLower;
    LArg := '';
  end;

  LSessionData := TClientSessionData(AClient.Data);

  if LCommand = '/help' then
  begin
    AClient.SendText('=== Available Commands ===');
    AClient.SendText('/setname <username> - Set your username');
    AClient.SendText('/join <room> - Join a chat room');
    AClient.SendText('/leave <room> - Leave a chat room');
    AClient.SendText('/rooms - List your current rooms');
    AClient.SendText('/users <room> - List users in a room');
    AClient.SendText('/msg <room> <message> - Send message to a room');
    AClient.SendText('/help - Show this help');
  end
  else if LCommand = '/setname' then
  begin
    if LArg.IsEmpty then
    begin
      AClient.SendText('Usage: /setname <username>');
      Exit;
    end;

    LSessionData.Username := LArg;
    AClient.Username := LArg;
    AClient.SendText(Format('Username set to: %s', [LArg]));
  end
  else if LCommand = '/join' then
  begin
    if LArg.IsEmpty then
    begin
      AClient.SendText('Usage: /join <room>');
      Exit;
    end;

    if AClient.IsInGroup(LArg) then
    begin
      AClient.SendText(Format('You are already in room: %s', [LArg]));
      Exit;
    end;

    AClient.JoinGroup(LArg);
    AClient.SendText(Format('You joined room: %s', [LArg]));
    AClient.SendToGroup(LArg, Format('[SYSTEM] %s joined the room', [AClient.Username]));
  end
  else if LCommand = '/leave' then
  begin
    if LArg.IsEmpty then
    begin
      AClient.SendText('Usage: /leave <room>');
      Exit;
    end;

    if not AClient.IsInGroup(LArg) then
    begin
      AClient.SendText(Format('You are not in room: %s', [LArg]));
      Exit;
    end;

    LGroupClients := Server.GetClientsByGroup(LArg);
    for I := 0 to Length(LGroupClients) - 1 do
    begin
      if LGroupClients[I] <> AClient then
        LGroupClients[I].SendText(Format('[SYSTEM] %s left the room', [AClient.Username]));
    end;

    AClient.LeaveGroup(LArg);
    AClient.SendText(Format('You left room: %s', [LArg]));
  end
  else if LCommand = '/rooms' then
  begin
    if Length(AClient.Groups) = 0 then
    begin
      AClient.SendText('You are not in any rooms. Use /join <room> to join a room.');
    end
    else
    begin
      AClient.SendText('=== Your Rooms ===');
      for I := 0 to Length(AClient.Groups) - 1 do
        AClient.SendText(Format('  - %s', [AClient.Groups[I]]));
    end;
  end
  else if LCommand = '/users' then
  begin
    if LArg.IsEmpty then
    begin
      AClient.SendText('Usage: /users <room>');
      Exit;
    end;

    LGroupClients := Server.GetClientsByGroup(LArg);
    if Length(LGroupClients) = 0 then
    begin
      AClient.SendText(Format('Room "%s" is empty or does not exist.', [LArg]));
    end
    else
    begin
      AClient.SendText(Format('=== Users in %s ===', [LArg]));
      for I := 0 to Length(LGroupClients) - 1 do
        AClient.SendText(Format('  - %s', [LGroupClients[I].Username]));
    end;
  end
  else if LCommand = '/msg' then
  begin
    // Parse: /msg <room> <message>
    // LArg = "room message text here"
    Writeln(Format('[DEBUG] /msg command - LArg="%s"', [LArg]));

    // Find first space to split room from message
    LSpacePos := Pos(' ', LArg);
    Writeln(Format('[DEBUG] Space position: %d', [LSpacePos]));

    if LSpacePos = 0 then
    begin
      Writeln('[DEBUG] No space found in LArg');
      AClient.SendText('Usage: /msg <room> <message>');
      Exit;
    end;

    LRoomName := Copy(LArg, 1, LSpacePos - 1);
    LMessage := Copy(LArg, LSpacePos + 1, Length(LArg));

    Writeln(Format('[DEBUG] RoomName="%s", Message="%s"', [LRoomName, LMessage]));

    if LMessage.IsEmpty then
    begin
      Writeln('[DEBUG] Message is empty');
      AClient.SendText('Usage: /msg <room> <message>');
      Exit;
    end;

    if not AClient.IsInGroup(LRoomName) then
    begin
      Writeln(Format('[DEBUG] Client not in room "%s"', [LRoomName]));
      AClient.SendText(Format('You are not in room: %s', [LRoomName]));
      Exit;
    end;

    Writeln('[DEBUG] Sending to group');
    AClient.SendToGroup(LRoomName, Format('[%s] %s: %s', [LRoomName, AClient.Username, LMessage]));
  end
  else
  begin
    AClient.SendText(Format('Unknown command: %s. Type /help for available commands.', [LCommand]));
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;

  try
    Writeln('=== WebSocket Group/Room Server ===');
    Writeln('');
    Writeln('Starting group server on port 9092...');

    Server := TMVCWebSocketServer.Create(9092);
    try
      Server.OnLog := procedure(const AMessage: string)
      begin
        Writeln(Format('[%s] %s', [TimeToStr(Now), AMessage]));
      end;

      Server.OnClientConnect := procedure(AClient: TWebSocketClient)
      var
        LSessionData: TClientSessionData;
      begin
        Writeln(Format('[%s] CLIENT CONNECTED: %s', [TimeToStr(Now), AClient.ClientId]));

        LSessionData := TClientSessionData.Create;
        LSessionData.Username := AClient.ClientId;
        AClient.Data := LSessionData;

        AClient.SendText('=== Welcome to WebSocket Group Chat ===');
        AClient.SendText('Type /help for available commands');
        AClient.SendText(Format('Your default username is: %s', [AClient.ClientId]));
        AClient.SendText('Use /setname <username> to change it');
      end;

      Server.OnClientDisconnect := procedure(AClient: TWebSocketClient)
      var
        LGroup: string;
        LGroupClients: TArray<TWebSocketClient>;
        I, J: Integer;
      begin
        Writeln(Format('[%s] CLIENT DISCONNECTED: %s (%s)',
          [TimeToStr(Now), AClient.ClientId, AClient.Username]));

        for I := 0 to Length(AClient.Groups) - 1 do
        begin
          LGroup := AClient.Groups[I];
          LGroupClients := Server.GetClientsByGroup(LGroup);
          for J := 0 to Length(LGroupClients) - 1 do
          begin
            if LGroupClients[J] <> AClient then
              LGroupClients[J].SendText(Format('[SYSTEM] %s disconnected from %s', [AClient.Username, LGroup]));
          end;
        end;
      end;

      Server.OnMessage := procedure(AClient: TWebSocketClient; const AMessage: string)
      begin
        Writeln(Format('[%s] Message from %s (%s): %s',
          [TimeToStr(Now), AClient.Username, AClient.ClientId, AMessage]));
        ProcessCommand(AClient, AMessage);
      end;

      Server.OnError := procedure(AClient: TWebSocketClient; const AError: string)
      begin
        Writeln(Format('[%s] ERROR from %s: %s', [TimeToStr(Now), AClient.ClientId, AError]));
      end;

      Server.Active := True;

      Writeln('Group server running!');
      Writeln('');
      Writeln('Connect with:');
      Writeln('  ws://localhost:9092/');
      Writeln('');
      Writeln('Features:');
      Writeln('  - Multiple chat rooms (groups)');
      Writeln('  - Join/leave rooms dynamically');
      Writeln('  - Send messages to specific rooms');
      Writeln('  - List users in rooms');
      Writeln('  - Custom usernames');
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
