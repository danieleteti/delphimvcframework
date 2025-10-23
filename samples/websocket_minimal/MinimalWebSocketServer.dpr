program MinimalWebSocketServer;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  IdContext,
  IdCustomTCPServer,
  IdTCPServer,
  IdIOHandler,
  IdGlobal,
  MVCFramework.WebSocket in '..\..\sources\MVCFramework.WebSocket.pas';

type
  /// <summary>
  /// Event types for server callbacks
  /// </summary>
  TWebSocketLogEvent = reference to procedure(const AMessage: string);
  TWebSocketMessageEvent = reference to procedure(const AConnectionId, AMessage: string);
  TWebSocketErrorEvent = reference to procedure(const AConnectionId, AError: string);

  TSimpleWebSocketServer = class(TIdTCPServer)
  private
    FOnLog: TWebSocketLogEvent;
    FOnMessage: TWebSocketMessageEvent;
    FOnError: TWebSocketErrorEvent;
    procedure OnExecuteEvent(AContext: TIdContext);
    function PerformHandshake(AContext: TIdContext): Boolean;
    procedure ProcessFrames(AContext: TIdContext);
    procedure DoLog(const AMessage: string);
    procedure DoMessage(const AConnectionId, AMessage: string);
    procedure DoError(const AConnectionId, AError: string);
  public
    constructor Create; reintroduce;

    property OnLog: TWebSocketLogEvent read FOnLog write FOnLog;
    property OnMessage: TWebSocketMessageEvent read FOnMessage write FOnMessage;
    property OnError: TWebSocketErrorEvent read FOnError write FOnError;
  end;

{ TSimpleWebSocketServer }

constructor TSimpleWebSocketServer.Create;
begin
  inherited Create(nil);
  DefaultPort := 9091;
  OnExecute := OnExecuteEvent;
end;

procedure TSimpleWebSocketServer.DoLog(const AMessage: string);
begin
  if Assigned(FOnLog) then
    FOnLog(AMessage);
end;

procedure TSimpleWebSocketServer.DoMessage(const AConnectionId, AMessage: string);
begin
  if Assigned(FOnMessage) then
    FOnMessage(AConnectionId, AMessage);
end;

procedure TSimpleWebSocketServer.DoError(const AConnectionId, AError: string);
begin
  if Assigned(FOnError) then
    FOnError(AConnectionId, AError);
end;

function TSimpleWebSocketServer.PerformHandshake(AContext: TIdContext): Boolean;
var
  RequestLine, Line: string;
  Headers: TStringList;
  WebSocketKey, AcceptKey: string;
  IOHandler: TIdIOHandler;
begin
  Result := False;
  IOHandler := AContext.Connection.IOHandler;
  Headers := TStringList.Create;
  try
    // Read HTTP request
    RequestLine := IOHandler.ReadLn(IndyTextEncoding_UTF8);
    if not RequestLine.StartsWith('GET ') then
      Exit;

    // Read all headers
    while True do
    begin
      Line := IOHandler.ReadLn(IndyTextEncoding_UTF8);
      if Line = '' then
        Break;
      Headers.Add(Line);
    end;

    // Find Sec-WebSocket-Key
    WebSocketKey := '';
    for Line in Headers do
    begin
      if Line.StartsWith('Sec-WebSocket-Key:', True) then
      begin
        WebSocketKey := Line.Substring(18).Trim;
        Break;
      end;
    end;

    if WebSocketKey.IsEmpty then
      Exit;

    // Calculate accept key
    AcceptKey := TMVCWebSocketHandshake.CalculateAcceptKey(WebSocketKey);

    // Send 101 Switching Protocols response
    IOHandler.WriteLn('HTTP/1.1 101 Switching Protocols');
    IOHandler.WriteLn('Upgrade: websocket');
    IOHandler.WriteLn('Connection: Upgrade');
    IOHandler.WriteLn('Sec-WebSocket-Accept: ' + AcceptKey);
    IOHandler.WriteLn(''); // Empty line to end headers

    // CRITICAL: Clear buffer after handshake
    IOHandler.InputBuffer.Clear;

    Result := True;
  finally
    Headers.Free;
  end;
end;

procedure TSimpleWebSocketServer.ProcessFrames(AContext: TIdContext);
var
  Frame: TMVCWebSocketFrame;
  EchoFrame: TMVCWebSocketFrame;
  IOHandler: TIdIOHandler;
  Message: string;
begin
  IOHandler := AContext.Connection.IOHandler;

  while AContext.Connection.Connected do
  begin
    try
      // Wait for data with 100ms timeout
      if not IOHandler.InputBufferIsEmpty or IOHandler.Readable(100) then
      begin
        // Read frame (server side, expecting masked frames from client)
        Frame := TMVCWebSocketFrameParser.ParseFrame(IOHandler, True);

        case Frame.Opcode of
          TMVCWebSocketOpcode.Text:
          begin
            // Get text message
            Message := TEncoding.UTF8.GetString(Frame.Payload);
            DoMessage(AContext.Connection.Socket.Binding.PeerIP, Message);

            // Echo back (server does NOT mask frames to client)
            EchoFrame := TMVCWebSocketFrameParser.CreateTextFrame(
              Format('Echo: %s', [Message]), False);
            TMVCWebSocketFrameParser.WriteFrame(IOHandler, EchoFrame);
          end;

          TMVCWebSocketOpcode.Binary:
          begin
            DoLog(Format('Received %d bytes from %s',
              [Length(Frame.Payload), AContext.Connection.Socket.Binding.PeerIP]));

            // Echo back binary
            EchoFrame := TMVCWebSocketFrameParser.CreateBinaryFrame(Frame.Payload, False);
            TMVCWebSocketFrameParser.WriteFrame(IOHandler, EchoFrame);
          end;

          TMVCWebSocketOpcode.Close:
          begin
            DoLog(Format('Client %s closing connection',
              [AContext.Connection.Socket.Binding.PeerIP]));

            // Echo close frame
            EchoFrame := TMVCWebSocketFrameParser.CreateCloseFrame(
              TMVCWebSocketCloseCode.NormalClosure, 'Goodbye');
            TMVCWebSocketFrameParser.WriteFrame(IOHandler, EchoFrame);
            Break;
          end;

          TMVCWebSocketOpcode.Ping:
          begin
            DoLog(Format('Ping from %s', [AContext.Connection.Socket.Binding.PeerIP]));

            // Respond with pong
            EchoFrame := TMVCWebSocketFrameParser.CreatePongFrame(Frame.Payload, False);
            TMVCWebSocketFrameParser.WriteFrame(IOHandler, EchoFrame);
          end;

          TMVCWebSocketOpcode.Pong:
          begin
            DoLog(Format('Pong from %s', [AContext.Connection.Socket.Binding.PeerIP]));
          end;
        end;
      end;

    except
      on E: Exception do
      begin
        DoError(AContext.Connection.Socket.Binding.PeerIP, E.Message);
        Break;
      end;
    end;
  end;
end;

procedure TSimpleWebSocketServer.OnExecuteEvent(AContext: TIdContext);
var
  ConnectionId: string;
begin
  ConnectionId := AContext.Connection.Socket.Binding.PeerIP;
  DoLog(Format('Client connected from %s', [ConnectionId]));

  try
    // Perform WebSocket handshake
    if PerformHandshake(AContext) then
    begin
      DoLog(Format('WebSocket handshake successful for %s', [ConnectionId]));

      // Process WebSocket frames
      ProcessFrames(AContext);
    end
    else
    begin
      DoLog(Format('Handshake failed for %s', [ConnectionId]));
    end;

  finally
    DoLog(Format('Client disconnected: %s', [ConnectionId]));
  end;
end;

var
  Server: TSimpleWebSocketServer;

begin
  ReportMemoryLeaksOnShutdown := True;

  try
    Writeln('=== Minimal WebSocket Server ===');
    Writeln('');
    Writeln('Starting server on port 9091...');

    Server := TSimpleWebSocketServer.Create;
    try
      // Setup event handlers
      Server.OnLog := procedure(const AMessage: string)
      begin
        Writeln(Format('[%s] %s', [TimeToStr(Now), AMessage]));
      end;

      Server.OnMessage := procedure(const AConnectionId, AMessage: string)
      begin
        Writeln(Format('[%s] Message from %s: %s', [TimeToStr(Now), AConnectionId, AMessage]));
      end;

      Server.OnError := procedure(const AConnectionId, AError: string)
      begin
        Writeln(Format('[%s] ERROR from %s: %s', [TimeToStr(Now), AConnectionId, AError]));
      end;

      Server.Active := True;

      Writeln('Server running!');
      Writeln('');
      Writeln('Connect with:');
      Writeln('  ws://localhost:9091/');
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
