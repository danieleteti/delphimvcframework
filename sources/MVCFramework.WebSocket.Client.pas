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

unit MVCFramework.WebSocket.Client;

{$I dmvcframework.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  IdTCPClient,
  IdIOHandler,
  IdSSL,
  IdSSLOpenSSL,
  IdGlobal,
  MVCFramework.WebSocket;

type
  TMVCWebSocketClient = class;

  /// <summary>
  /// Event triggered when connected to the server
  /// </summary>
  TMVCWebSocketClientConnectEvent = procedure(Sender: TMVCWebSocketClient) of object;

  /// <summary>
  /// Event triggered when disconnected from the server
  /// </summary>
  TMVCWebSocketClientDisconnectEvent = procedure(Sender: TMVCWebSocketClient;
    ACode: TMVCWebSocketCloseCode; const AReason: string) of object;

  /// <summary>
  /// Event triggered when a text message is received
  /// </summary>
  TMVCWebSocketClientTextMessageEvent = procedure(Sender: TMVCWebSocketClient;
    const AMessage: string) of object;

  /// <summary>
  /// Event triggered when a binary message is received
  /// </summary>
  TMVCWebSocketClientBinaryMessageEvent = procedure(Sender: TMVCWebSocketClient;
    const AData: TBytes) of object;

  /// <summary>
  /// Event triggered when an error occurs
  /// </summary>
  TMVCWebSocketClientErrorEvent = procedure(Sender: TMVCWebSocketClient;
    const AError: Exception) of object;

  /// <summary>
  /// WebSocket client implementation for Delphi
  /// </summary>
  TMVCWebSocketClient = class
  private
    FTCPClient: TIdTCPClient;
    FSSLHandler: TIdSSLIOHandlerSocketOpenSSL;
    FURL: string;
    FHost: string;
    FPort: Integer;
    FPath: string;
    FUseSSL: Boolean;
    FConnected: Boolean;
    FLock: TCriticalSection;
    FReceiveThread: TThread;
    FAutoReconnect: Boolean;
    FReconnectInterval: Integer;

    // Events
    FOnConnect: TMVCWebSocketClientConnectEvent;
    FOnDisconnect: TMVCWebSocketClientDisconnectEvent;
    FOnTextMessage: TMVCWebSocketClientTextMessageEvent;
    FOnBinaryMessage: TMVCWebSocketClientBinaryMessageEvent;
    FOnError: TMVCWebSocketClientErrorEvent;

    procedure ParseURL(const AURL: string);
    function PerformHandshake: Boolean;
    procedure StartReceiveThread;
    procedure StopReceiveThread;
    procedure DoReceive;
    procedure SendPong(const AData: TBytes);
  public
    constructor Create(const AURL: string);
    destructor Destroy; override;

    /// <summary>
    /// Connect to the WebSocket server
    /// </summary>
    procedure Connect;

    /// <summary>
    /// Disconnect from the WebSocket server
    /// </summary>
    procedure Disconnect;

    /// <summary>
    /// Send a text message
    /// </summary>
    procedure SendText(const AMessage: string);

    /// <summary>
    /// Send a binary message
    /// </summary>
    procedure SendBinary(const AData: TBytes);

    /// <summary>
    /// Send a ping frame
    /// </summary>
    procedure SendPing(const AData: TBytes = nil);

    /// <summary>
    /// Check if connected
    /// </summary>
    function IsConnected: Boolean;

    /// <summary>
    /// WebSocket URL (ws:// or wss://)
    /// </summary>
    property URL: string read FURL;

    /// <summary>
    /// Connected state
    /// </summary>
    property Connected: Boolean read FConnected;

    /// <summary>
    /// Auto-reconnect on disconnect
    /// </summary>
    property AutoReconnect: Boolean read FAutoReconnect write FAutoReconnect;

    /// <summary>
    /// Reconnect interval in seconds
    /// </summary>
    property ReconnectInterval: Integer read FReconnectInterval write FReconnectInterval;

    /// <summary>
    /// Event handlers
    /// </summary>
    property OnConnect: TMVCWebSocketClientConnectEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TMVCWebSocketClientDisconnectEvent read FOnDisconnect write FOnDisconnect;
    property OnTextMessage: TMVCWebSocketClientTextMessageEvent read FOnTextMessage write FOnTextMessage;
    property OnBinaryMessage: TMVCWebSocketClientBinaryMessageEvent read FOnBinaryMessage write FOnBinaryMessage;
    property OnError: TMVCWebSocketClientErrorEvent read FOnError write FOnError;
  end;

  /// <summary>
  /// Exception raised for WebSocket client errors
  /// </summary>
  EMVCWebSocketClientException = class(Exception);

implementation

uses
  System.DateUtils,
  IdCoderMIME,
  IdHashSHA;

type
  TWebSocketReceiveThread = class(TThread)
  private
    FClient: TMVCWebSocketClient;
  protected
    procedure Execute; override;
  public
    constructor Create(AClient: TMVCWebSocketClient);
  end;

{ TMVCWebSocketClient }

constructor TMVCWebSocketClient.Create(const AURL: string);
begin
  inherited Create;
  FURL := AURL;
  ParseURL(AURL);

  FTCPClient := TIdTCPClient.Create(nil);
  FTCPClient.ConnectTimeout := 5000;
  FTCPClient.ReadTimeout := 1000;

  if FUseSSL then
  begin
    FSSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    FSSLHandler.SSLOptions.Method := sslvTLSv1_2;
    FSSLHandler.SSLOptions.Mode := sslmClient;
    FTCPClient.IOHandler := FSSLHandler;
  end;

  FTCPClient.Host := FHost;
  FTCPClient.Port := FPort;

  FLock := TCriticalSection.Create;
  FConnected := False;
  FAutoReconnect := False;
  FReconnectInterval := 5;
end;

destructor TMVCWebSocketClient.Destroy;
begin
  Disconnect;
  StopReceiveThread;
  FTCPClient.Free;
  if Assigned(FSSLHandler) then
    FSSLHandler.Free;
  FLock.Free;
  inherited;
end;

procedure TMVCWebSocketClient.ParseURL(const AURL: string);
var
  URI: string;
  SlashPos: Integer;
begin
  // Parse ws://host:port/path or wss://host:port/path
  if AURL.StartsWith('wss://', True) then
  begin
    FUseSSL := True;
    URI := AURL.Substring(6);
    FPort := 443; // Default WSS port
  end
  else if AURL.StartsWith('ws://', True) then
  begin
    FUseSSL := False;
    URI := AURL.Substring(5);
    FPort := 80; // Default WS port
  end
  else
    raise EMVCWebSocketClientException.Create('Invalid WebSocket URL. Must start with ws:// or wss://');

  // Parse host:port/path
  SlashPos := Pos('/', URI);
  if SlashPos > 0 then
  begin
    FPath := URI.Substring(SlashPos - 1);
    URI := URI.Substring(0, SlashPos - 1);
  end
  else
  begin
    FPath := '/';
  end;

  // Parse host:port
  if Pos(':', URI) > 0 then
  begin
    FHost := URI.Substring(0, Pos(':', URI) - 1);
    FPort := StrToIntDef(URI.Substring(Pos(':', URI)), FPort);
  end
  else
  begin
    FHost := URI;
  end;
end;

function TMVCWebSocketClient.PerformHandshake: Boolean;
var
  SecWebSocketKey: string;
  Request: string;
  Response: string;
  ExpectedAccept: string;
  Lines: TArray<string>;
  Line: string;
  AcceptKey: string;
  I: Integer;
begin
  Result := False;

  // Generate random key
  SecWebSocketKey := TIdEncoderMIME.EncodeBytes(
    ToBytes(IntToStr(Random(MaxInt)) + IntToStr(DateTimeToUnix(Now))));

  // Calculate expected accept key
  ExpectedAccept := TMVCWebSocketHandshake.CalculateAcceptKey(SecWebSocketKey);

  // Build handshake request
  Request := Format('GET %s HTTP/1.1'#13#10, [FPath]) +
             Format('Host: %s:%d'#13#10, [FHost, FPort]) +
             'Upgrade: websocket'#13#10 +
             'Connection: Upgrade'#13#10 +
             Format('Sec-WebSocket-Key: %s'#13#10, [SecWebSocketKey]) +
             'Sec-WebSocket-Version: 13'#13#10 +
             #13#10;

  // Send handshake
  FTCPClient.IOHandler.Write(Request, IndyTextEncoding_UTF8);

  // Read response
  Response := '';
  for I := 0 to 20 do // Read up to 20 lines
  begin
    Line := FTCPClient.IOHandler.ReadLn(IndyTextEncoding_UTF8);
    Response := Response + Line + #13#10;
    if Line = '' then
      Break; // End of headers
  end;

  // Validate response
  Lines := Response.Split([#13#10]);
  if Length(Lines) < 1 then
    Exit;

  // Check status line
  if not Lines[0].Contains('101') then
    Exit;

  // Find Sec-WebSocket-Accept header
  AcceptKey := '';
  for Line in Lines do
  begin
    if Line.StartsWith('Sec-WebSocket-Accept:', True) then
    begin
      AcceptKey := Line.Substring(21).Trim;
      Break;
    end;
  end;

  // Validate accept key
  Result := SameText(AcceptKey, ExpectedAccept);
end;

procedure TMVCWebSocketClient.Connect;
begin
  FLock.Enter;
  try
    if FConnected then
      Exit;

    try
      FTCPClient.Connect;

      if not PerformHandshake then
      begin
        FTCPClient.Disconnect;
        raise EMVCWebSocketClientException.Create('WebSocket handshake failed');
      end;

      FConnected := True;

      // Start receive thread
      StartReceiveThread;

      // Trigger event
      if Assigned(FOnConnect) then
        FOnConnect(Self);

    except
      on E: Exception do
      begin
        FConnected := False;
        if Assigned(FOnError) then
          FOnError(Self, E);
        raise;
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TMVCWebSocketClient.Disconnect;
begin
  FLock.Enter;
  try
    if not FConnected then
      Exit;

    StopReceiveThread;

    try
      // Send close frame
      if FTCPClient.Connected then
      begin
        try
          TMVCWebSocketFrameParser.WriteFrame(FTCPClient.IOHandler,
            TMVCWebSocketFrameParser.CreateCloseFrame(TMVCWebSocketCloseCode.NormalClosure, '', True));
        except
          // Ignore errors during close
        end;
      end;

      FTCPClient.Disconnect;
    except
      // Ignore errors
    end;

    FConnected := False;

    // Trigger event
    if Assigned(FOnDisconnect) then
      FOnDisconnect(Self, TMVCWebSocketCloseCode.NormalClosure, '');

  finally
    FLock.Leave;
  end;
end;

procedure TMVCWebSocketClient.SendText(const AMessage: string);
var
  Frame: TMVCWebSocketFrame;
begin
  if not FConnected then
    raise EMVCWebSocketClientException.Create('Not connected');

  FLock.Enter;
  try
    Frame := TMVCWebSocketFrameParser.CreateTextFrame(AMessage, True); // Client must mask
    TMVCWebSocketFrameParser.WriteFrame(FTCPClient.IOHandler, Frame);
  finally
    FLock.Leave;
  end;
end;

procedure TMVCWebSocketClient.SendBinary(const AData: TBytes);
var
  Frame: TMVCWebSocketFrame;
begin
  if not FConnected then
    raise EMVCWebSocketClientException.Create('Not connected');

  FLock.Enter;
  try
    Frame := TMVCWebSocketFrameParser.CreateBinaryFrame(AData, True); // Client must mask
    TMVCWebSocketFrameParser.WriteFrame(FTCPClient.IOHandler, Frame);
  finally
    FLock.Leave;
  end;
end;

procedure TMVCWebSocketClient.SendPing(const AData: TBytes);
var
  Frame: TMVCWebSocketFrame;
begin
  if not FConnected then
    raise EMVCWebSocketClientException.Create('Not connected');

  FLock.Enter;
  try
    Frame := TMVCWebSocketFrameParser.CreatePingFrame(AData, True); // Client must mask
    TMVCWebSocketFrameParser.WriteFrame(FTCPClient.IOHandler, Frame);
  finally
    FLock.Leave;
  end;
end;

procedure TMVCWebSocketClient.SendPong(const AData: TBytes);
var
  Frame: TMVCWebSocketFrame;
begin
  if not FConnected then
    raise EMVCWebSocketClientException.Create('Not connected');

  FLock.Enter;
  try
    Frame := TMVCWebSocketFrameParser.CreatePongFrame(AData, True); // Client must mask
    TMVCWebSocketFrameParser.WriteFrame(FTCPClient.IOHandler, Frame);
  finally
    FLock.Leave;
  end;
end;

function TMVCWebSocketClient.IsConnected: Boolean;
begin
  FLock.Enter;
  try
    Result := FConnected and FTCPClient.Connected;
  finally
    FLock.Leave;
  end;
end;

procedure TMVCWebSocketClient.StartReceiveThread;
begin
  if Assigned(FReceiveThread) then
    Exit;

  FReceiveThread := TWebSocketReceiveThread.Create(Self);
  FReceiveThread.Start;
end;

procedure TMVCWebSocketClient.StopReceiveThread;
begin
  if not Assigned(FReceiveThread) then
    Exit;

  FReceiveThread.Terminate;
  FReceiveThread.WaitFor;
  FreeAndNil(FReceiveThread);
end;

procedure TMVCWebSocketClient.DoReceive;
var
  Frame: TMVCWebSocketFrame;
  TextMessage: string;
  CloseCode: TMVCWebSocketCloseCode;
  CloseReason: string;
begin
  while FConnected and not TThread.CurrentThread.CheckTerminated do
  begin
    try
      // Check if data available
      if FTCPClient.IOHandler.InputBufferIsEmpty then
      begin
        FTCPClient.IOHandler.CheckForDataOnSource(100);
        if FTCPClient.IOHandler.InputBufferIsEmpty then
          Continue;
      end;

      // Read frame
      Frame := TMVCWebSocketFrameParser.ParseFrame(FTCPClient.IOHandler);

      // Process frame
      case Frame.Opcode of
        TMVCWebSocketOpcode.Text:
        begin
          TextMessage := TEncoding.UTF8.GetString(Frame.Payload);
          if Assigned(FOnTextMessage) then
            FOnTextMessage(Self, TextMessage);
        end;

        TMVCWebSocketOpcode.Binary:
        begin
          if Assigned(FOnBinaryMessage) then
            FOnBinaryMessage(Self, Frame.Payload);
        end;

        TMVCWebSocketOpcode.Close:
        begin
          // Parse close code
          if Length(Frame.Payload) >= 2 then
          begin
            CloseCode := TMVCWebSocketCloseCode((Frame.Payload[0] shl 8) or Frame.Payload[1]);
            if Length(Frame.Payload) > 2 then
              CloseReason := TEncoding.UTF8.GetString(Frame.Payload, 2, Length(Frame.Payload) - 2)
            else
              CloseReason := '';
          end
          else
          begin
            CloseCode := TMVCWebSocketCloseCode.NoStatusReceived;
            CloseReason := '';
          end;

          // Trigger event before disconnecting
          if Assigned(FOnDisconnect) then
            FOnDisconnect(Self, CloseCode, CloseReason);

          Disconnect;
          Break;
        end;

        TMVCWebSocketOpcode.Ping:
        begin
          // Send pong response
          SendPong(Frame.Payload);
        end;

        // Ignore pong frames
        TMVCWebSocketOpcode.Pong:
        begin
          // Nothing to do
        end;
      end;

    except
      on E: Exception do
      begin
        if Assigned(FOnError) then
          FOnError(Self, E);

        if FAutoReconnect then
        begin
          Disconnect;
          Sleep(FReconnectInterval * 1000);
          try
            Connect;
          except
            // Will retry on next iteration
          end;
        end
        else
          Break;
      end;
    end;
  end;
end;

{ TWebSocketReceiveThread }

constructor TWebSocketReceiveThread.Create(AClient: TMVCWebSocketClient);
begin
  inherited Create(True);
  FClient := AClient;
  FreeOnTerminate := False;
end;

procedure TWebSocketReceiveThread.Execute;
begin
  FClient.DoReceive;
end;

end.
