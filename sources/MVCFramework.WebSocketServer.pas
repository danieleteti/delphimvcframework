// *************************************************************************** }
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2022 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.WebSocketServer;

interface

uses
  System.SysUtils, System.Generics.Collections, IdGlobal, IdCoderMIME,
  IdCustomTCPServer, IdTCPConnection, IdContext, IdIOHandler,
  IdHashSHA, IdSSL, IdSSLOpenSSL, MVCFramework.Logger;

type
  TMVCCustomWebSocketServer = class(TIdCustomTCPServer)
  private
    fHashSHA1: TIdHashSHA1;
    fIOHandlerSSLOpenSSL: TIdServerIOHandlerSSLOpenSSL;
  protected
    procedure DoConnect(AContext: TIdContext); override;
    function DoExecute(AContext: TIdContext): Boolean; override;
    procedure BadRequest(AHandler: TIdIOHandler);
    function ParseHeaders(const Headers: string): TDictionary<String, String>;
  public
    procedure InitSSL(AIdServerIOHandlerSSLOpenSSL: TIdServerIOHandlerSSLOpenSSL);
    constructor Create;
    destructor Destroy; override;
  end;

  TMVCWebSocketIOHandler = class(TIdIOHandler)
  public
    /// <summary>
    /// Receive bytes to the client
    /// </summary>
    function ReadBytes: TArray<Byte>; reintroduce;
    /// <summary>
    /// Receive string from the client
    /// </summary>
    function ReadString: string;
    /// <summary>
    /// Send bytes to the client
    /// </summary>
    procedure WriteBytes(const RawData: TArray<Byte>);
    /// <summary>
    /// Send string to the client
    /// </summary>
    procedure WriteString(const Value: string);
    /// <summary>
    /// Alias for WriteString
    /// </summary>
    procedure Send(const Value: string); inline;
    /// <summary>
    /// Alias for ReadString
    /// </summary>
    function Receive: String;
  end;

  TWebSocketHandlerEvent = procedure(WSContext: TMVCWebSocketIOHandler) of object;

  TMVCWebSocketServer = class
  private
    fWSServer: TMVCCustomWebSocketServer;
    fOnMessageArrived: TWebSocketHandlerEvent;
    fOnConnect: TWebSocketHandlerEvent;
    fOnDisconnect: TWebSocketHandlerEvent;
    procedure Connect(AContext: TIdContext);
    procedure Disconnect(AContext: TIdContext);
    procedure Execute(AContext: TIdContext);
    procedure ContextCreated(AContext: TIdContext);
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);

  public
    constructor Create(const AWSPort: Word);
    destructor Destroy; override;
    property Active: Boolean read GetActive write SetActive;
    property OnConnect: TWebSocketHandlerEvent read fOnConnect write fOnConnect;
    property OnDisconnect: TWebSocketHandlerEvent read fOnDisconnect write fOnDisconnect;
    property OnMessageArrived: TWebSocketHandlerEvent read fOnMessageArrived write fOnMessageArrived;
  end;

implementation

const
  CRLF = #13#10;
  WS_LOG_TAG = 'websocket';
  WS_KEY_MAGIC_STRING = '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';

function TMVCCustomWebSocketServer.ParseHeaders(const Headers: string): TDictionary<String, String>;
var
  lPieces: TArray<string>;
  lLine: string;
  lLineParts: TArray<string>;
begin
  Result := TDictionary<string, string>.Create;
  lPieces := Headers.Split([CRLF]);
  for lLine in lPieces do
  begin
    lLineParts := lLine.Split([':']);
    if Length(lLineParts) > 1 then
    begin
      Result.AddOrSetValue(Trim(lLineParts[0]), Trim(lLineParts[1]));
    end;
  end;
end;

{ TWebSocketServer }

procedure TMVCCustomWebSocketServer.BadRequest(AHandler: TIdIOHandler);
begin
  AHandler.Write('HTTP/1.1 400 Bad Request' + CRLF + CRLF);
end;

constructor TMVCCustomWebSocketServer.Create;
begin
  inherited Create;
  fHashSHA1 := TIdHashSHA1.Create;
  fIOHandlerSSLOpenSSL := nil;
end;

destructor TMVCCustomWebSocketServer.Destroy;
begin
  fHashSHA1.Free;
  inherited;
end;

procedure TMVCCustomWebSocketServer.InitSSL(AIdServerIOHandlerSSLOpenSSL: TIdServerIOHandlerSSLOpenSSL);
var
  lNeedsToBeReactivated: Boolean;
begin
  lNeedsToBeReactivated := Active;
  Active := False;

  fIOHandlerSSLOpenSSL := AIdServerIOHandlerSSLOpenSSL;
  IOHandler := AIdServerIOHandlerSSLOpenSSL;

  if lNeedsToBeReactivated then
  begin
    Active := True;
  end;
end;

procedure TMVCCustomWebSocketServer.DoConnect(AContext: TIdContext);
begin
  if AContext.Connection.IOHandler is TIdSSLIOHandlerSocketBase then
  begin
    TIdSSLIOHandlerSocketBase(AContext.Connection.IOHandler).PassThrough := false;
  end;

  // Mark connection as "not handshaked"
  AContext.Connection.IOHandler.Tag := 0;
  inherited;
end;

function TMVCCustomWebSocketServer.DoExecute(AContext: TIdContext): Boolean;
var
  lHandler: TIdIOHandler;
  Bytes: TArray<Byte>;
  lMessage, lSecWebSocketKey, Hash: string;
  lHeaders: TDictionary<string, string>;
  lWSVersion: string;
begin
  lHandler := AContext.Connection.IOHandler;

  if lHandler.Tag = 0 then {let's begin the handshake}
  begin
    lHandler.CheckForDataOnSource(10);

    if not lHandler.InputBufferIsEmpty then
    begin
      // Parse HTTP headers
      try
        lHandler.InputBuffer.ExtractToBytes(TIdBytes(Bytes));
        lMessage := IndyTextEncoding_UTF8.GetString(TIdBytes(Bytes));
      except
        BadRequest(lHandler);
        try
          AContext.Connection.Disconnect(true);
        except
        end;
        Exit(false);
      end;

      lHeaders := ParseHeaders(lMessage);
      try
        if lHeaders.ContainsKey('Upgrade') and (lHeaders['Upgrade'] = 'websocket') and
          lHeaders.ContainsKey('Sec-WebSocket-Key') then
        begin
          if lHeaders.TryGetValue('Sec-WebSocket-Version', lWSVersion) then
          begin
            Log.Debug('WS Version: %s', [lWSVersion], WS_LOG_TAG);
          end
          else
          begin
            Log.Debug('Unknown WS Version', WS_LOG_TAG);
          end;

          // More info anbuyt this process, here
          // https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API/Writing_WebSocket_servers

          lSecWebSocketKey := lHeaders['Sec-WebSocket-Key'];
          Hash := TIdEncoderMIME.EncodeBytes(fHashSHA1.HashString(lSecWebSocketKey + WS_KEY_MAGIC_STRING));

          try
            lHandler.Write(
              'HTTP/1.1 101 Switching Protocols' + CRLF +
              'Upgrade: websocket' + CRLF +
              'Connection: Upgrade' + CRLF +
              'Sec-WebSocket-Version: 13' + CRLF +
              'Sec-WebSocket-Accept: ' + Hash + CRLF + CRLF,
              IndyTextEncoding_UTF8);
          except
          end;
          lHandler.Tag := 1;
        end;
      finally
        lHeaders.Free;
      end;
    end;
  end;

  Result := inherited;
end;

{ TWebSocketIOHandlerHelper }

function TMVCWebSocketIOHandler.ReadBytes: TArray<Byte>;
var
  lByte: Byte;
  lBuffer: array [0 .. 7] of Byte;
  i, lDecodedSize: Int64;
  lMask: array [0 .. 3] of Byte;
  lOpCode: Byte;
const
  OPCODE_CLOSE_FRAME = $80;
  OPCODE_PING_FRAME = $90;
  OPCODE_PONG_FRAME = $A0;
begin
  // https://stackoverflow.com/questions/8125507/how-can-i-send-and-receive-websocket-messages-on-the-server-side

  try
    lOpCode := ReadByte;

    {TODO -oDanieleT -cGeneral : Implement OpCode for CLOSE frame}
    //https://datatracker.ietf.org/doc/html/rfc6455#section-5.5.1

    {TODO -oDanieleT -cGeneral : Implement OpCode for PING frame}
    //https://datatracker.ietf.org/doc/html/rfc6455#section-5.5.2

    if lOpCode = OPCODE_CLOSE_FRAME then
    begin

    end else if lOpCode = $81 then
    begin
      lByte := ReadByte;
      case lByte of
        $FE:
          begin
            lBuffer[1] := ReadByte;
            lBuffer[0] := ReadByte;
            lBuffer[2] := 0;
            lBuffer[3] := 0;
            lBuffer[4] := 0;
            lBuffer[5] := 0;
            lBuffer[6] := 0;
            lBuffer[7] := 0;
            lDecodedSize := Int64(lBuffer);
          end;
        $FF:
          begin
            lBuffer[7] := ReadByte;
            lBuffer[6] := ReadByte;
            lBuffer[5] := ReadByte;
            lBuffer[4] := ReadByte;
            lBuffer[3] := ReadByte;
            lBuffer[2] := ReadByte;
            lBuffer[1] := ReadByte;
            lBuffer[0] := ReadByte;
            lDecodedSize := Int64(lBuffer);
          end;
      else
        begin
          lDecodedSize := lByte - 128;
        end;
      end;
      lMask[0] := ReadByte;
      lMask[1] := ReadByte;
      lMask[2] := ReadByte;
      lMask[3] := ReadByte;

      if lDecodedSize < 1 then
      begin
        Result := [];
        Exit;
      end;

      SetLength(Result, lDecodedSize);
      inherited ReadBytes(TIdBytes(Result), lDecodedSize, false);
      for i := 0 to lDecodedSize - 1 do
      begin
        Result[i] := Result[i] xor lMask[i mod 4];
      end;
    end;
  except
    SetLength(Result, 0);
  end;
end;

procedure TMVCWebSocketIOHandler.WriteBytes(const RawData: TArray<Byte>);
var
  lMsg: TArray<Byte>;
begin
  // https://stackoverflow.com/questions/8125507/how-can-i-send-and-receive-websocket-messages-on-the-server-side

  lMsg := [$81];
  if Length(RawData) <= 125 then
    lMsg := lMsg + [Length(RawData)]
  else if (Length(RawData) >= 126) and (Length(RawData) <= 65535) then
    lMsg := lMsg + [126, (Length(RawData) shr 8) and 255, Length(RawData) and 255]
  else
    lMsg := lMsg + [127, (Int64(Length(RawData)) shr 56) and 255, (Int64(Length(RawData)) shr 48) and 255,
      (Int64(Length(RawData)) shr 40) and 255, (Int64(Length(RawData)) shr 32) and 255, (Length(RawData) shr 24) and
      255, (Length(RawData) shr 16) and 255, (Length(RawData) shr 8) and 255, Length(RawData) and 255];

  lMsg := lMsg + RawData;

  try
    Write(TIdBytes(lMsg), Length(lMsg));
  except
    // do nothing
  end;
end;

function TMVCWebSocketIOHandler.ReadString: string;
begin
  Result := IndyTextEncoding_UTF8.GetString(TIdBytes(ReadBytes));
end;

function TMVCWebSocketIOHandler.Receive: String;
begin
  Result := ReadString;
end;

procedure TMVCWebSocketIOHandler.Send(const Value: string);
begin
  WriteString(Value);
end;

procedure TMVCWebSocketIOHandler.WriteString(const Value: string);
begin
  WriteBytes(TArray<Byte>(IndyTextEncoding_UTF8.GetBytes(Value)));
end;

constructor TMVCWebSocketServer.Create(const AWSPort: Word);
begin
  fWSServer := TMVCCustomWebSocketServer.Create;
  fWSServer.DefaultPort := AWSPort;
  fWSServer.OnConnect := Connect;
  fWSServer.OnExecute := Execute;
  fWSServer.OnDisconnect := Disconnect;
  fWSServer.OnContextCreated := ContextCreated;
end;

destructor TMVCWebSocketServer.Destroy;
begin
  fWSServer.Active := false;
  fWSServer.Free;
  inherited;
end;

procedure TMVCWebSocketServer.Connect(AContext: TIdContext);
begin
  LogD('Client connected: ' + AContext.Connection.Socket.Binding.PeerPort.ToString);
  if Assigned(fOnConnect) then
  begin
    try
      fOnConnect(TMVCWebSocketIOHandler(AContext));
    except
      on E: Exception do
      begin
        Log.Error('[Connect] ' + E.ClassName + ': ' + E.Message, WS_LOG_TAG);
      end;
    end;
  end;
end;

procedure TMVCWebSocketServer.ContextCreated(AContext: TIdContext);
begin

end;

procedure TMVCWebSocketServer.Disconnect(AContext: TIdContext);
begin
  LogD('Client disconnected: ' + AContext.Connection.Socket.Binding.PeerPort.ToString);
  if Assigned(fOnConnect) then
  begin
    try
      fOnDisconnect(TMVCWebSocketIOHandler(AContext));
    except
      on E: Exception do
      begin
        Log.Error('[Disconnect] ' + E.ClassName + ': ' + E.Message, WS_LOG_TAG);
      end;
    end;
  end;
end;

procedure TMVCWebSocketServer.Execute(AContext: TIdContext);
begin
  if Assigned(fOnMessageArrived) then
  begin
    try
      fOnMessageArrived(TMVCWebSocketIOHandler(AContext.Connection.IOHandler));
    except
      on E: Exception do
      begin
        Log.Error('[MessageArrived] ' + E.ClassName + ': ' + E.Message, WS_LOG_TAG);
      end;
    end;
  end;
end;

function TMVCWebSocketServer.GetActive: Boolean;
begin
  Result := fWSServer.Active;
end;

procedure TMVCWebSocketServer.SetActive(const Value: Boolean);
begin
  fWSServer.Active := Value;
end;

end.
