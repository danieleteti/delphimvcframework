// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.WebSocket;

{$I dmvcframework.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  IdGlobal,
  IdHashSHA,
  IdCoderMIME,
  IdIOHandler;

type
  /// <summary>
  /// WebSocket close status codes as defined in RFC 6455 Section 7.4
  /// </summary>
  TMVCWebSocketCloseCode = (
    NormalClosure = 1000,       // Normal closure
    GoingAway = 1001,           // Endpoint going away
    ProtocolError = 1002,       // Protocol error
    UnsupportedData = 1003,     // Unsupported data
    Reserved = 1004,            // Reserved, not used
    NoStatusReceived = 1005,    // Reserved, should not be sent
    AbnormalClosure = 1006,     // Reserved, should not be sent
    InvalidPayload = 1007,      // Invalid frame payload data
    PolicyViolation = 1008,     // Generic status code
    MessageTooBig = 1009,       // Message too big
    MandatoryExtension = 1010,  // Client needs extension
    InternalError = 1011,       // Server encountered error
    TLSHandshake = 1015         // Reserved, should not be sent
  );

  /// <summary>
  /// WebSocket frame opcodes as defined in RFC 6455 Section 5.2
  /// </summary>
  TMVCWebSocketOpcode = (
    Continuation = $0,    // Continuation frame
    Text = $1,           // Text frame
    Binary = $2,         // Binary frame
    Reserved3 = $3,      // Reserved
    Reserved4 = $4,      // Reserved
    Reserved5 = $5,      // Reserved
    Reserved6 = $6,      // Reserved
    Reserved7 = $7,      // Reserved
    Close = $8,          // Connection close
    Ping = $9,           // Ping
    Pong = $A            // Pong
  );

  /// <summary>
  /// WebSocket frame structure according to RFC 6455
  /// </summary>
  TMVCWebSocketFrame = record
    /// <summary>
    /// Indicates if this is the final fragment in a message
    /// </summary>
    Fin: Boolean;

    /// <summary>
    /// Reserved bits for extensions (RSV1, RSV2, RSV3)
    /// </summary>
    Reserved: Byte;

    /// <summary>
    /// Frame opcode (text, binary, close, ping, pong, etc.)
    /// </summary>
    Opcode: TMVCWebSocketOpcode;

    /// <summary>
    /// Indicates if the payload is masked (client to server must be masked)
    /// </summary>
    Masked: Boolean;

    /// <summary>
    /// Length of the payload data
    /// </summary>
    PayloadLength: UInt64;

    /// <summary>
    /// Masking key used if Masked = True
    /// </summary>
    MaskingKey: array[0..3] of Byte;

    /// <summary>
    /// Actual payload data
    /// </summary>
    Payload: TBytes;

    class function Create(AOpcode: TMVCWebSocketOpcode; const APayload: TBytes;
      AFin: Boolean = True; AMasked: Boolean = False): TMVCWebSocketFrame; static;
  end;

  /// <summary>
  /// Exception raised for WebSocket protocol errors
  /// </summary>
  EMVCWebSocketException = class(Exception);

  /// <summary>
  /// WebSocket frame parser and writer implementing RFC 6455
  /// </summary>
  TMVCWebSocketFrameParser = class sealed
  private
    class procedure ApplyMask(var Data: TBytes; const MaskKey: array of Byte); static;
  public
    /// <summary>
    /// Parse a WebSocket frame from a stream
    /// </summary>
    /// <param name="AIOHandler">Indy IO handler to read from</param>
    /// <param name="AServerSide">True if parsing as server (expects masked frames), False if parsing as client</param>
    /// <returns>Parsed WebSocket frame</returns>
    class function ParseFrame(AIOHandler: TIdIOHandler; AServerSide: Boolean = True): TMVCWebSocketFrame; static;

    /// <summary>
    /// Write a WebSocket frame to a stream
    /// </summary>
    /// <param name="AIOHandler">Indy IO handler to write to</param>
    /// <param name="AFrame">Frame to write</param>
    class procedure WriteFrame(AIOHandler: TIdIOHandler; const AFrame: TMVCWebSocketFrame); static;

    /// <summary>
    /// Create a text message frame
    /// </summary>
    class function CreateTextFrame(const AText: string; AMasked: Boolean = False): TMVCWebSocketFrame; static;

    /// <summary>
    /// Create a binary message frame
    /// </summary>
    class function CreateBinaryFrame(const AData: TBytes; AMasked: Boolean = False): TMVCWebSocketFrame; static;

    /// <summary>
    /// Create a close frame
    /// </summary>
    class function CreateCloseFrame(ACode: TMVCWebSocketCloseCode = TMVCWebSocketCloseCode.NormalClosure;
      const AReason: string = ''; AMasked: Boolean = False): TMVCWebSocketFrame; static;

    /// <summary>
    /// Create a ping frame
    /// </summary>
    class function CreatePingFrame(const AData: TBytes = nil; AMasked: Boolean = False): TMVCWebSocketFrame; static;

    /// <summary>
    /// Create a pong frame
    /// </summary>
    class function CreatePongFrame(const AData: TBytes = nil; AMasked: Boolean = False): TMVCWebSocketFrame; static;
  end;

  /// <summary>
  /// WebSocket handshake utilities
  /// </summary>
  TMVCWebSocketHandshake = class sealed
  private
    const WEBSOCKET_MAGIC_STRING = '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';
  public
    /// <summary>
    /// Calculate WebSocket accept key from client key
    /// </summary>
    /// <param name="AClientKey">Client's Sec-WebSocket-Key value</param>
    /// <returns>Calculated Sec-WebSocket-Accept value</returns>
    class function CalculateAcceptKey(const AClientKey: string): string; static;

    /// <summary>
    /// Validate if the handshake request is valid
    /// </summary>
    class function IsValidHandshakeRequest(const AUpgradeHeader, AConnectionHeader,
      AWebSocketVersion: string): Boolean; static;
  end;

implementation

uses
  System.NetEncoding;

{ TMVCWebSocketFrame }

class function TMVCWebSocketFrame.Create(AOpcode: TMVCWebSocketOpcode;
  const APayload: TBytes; AFin, AMasked: Boolean): TMVCWebSocketFrame;
begin
  Result.Fin := AFin;
  Result.Reserved := 0;
  Result.Opcode := AOpcode;
  Result.Masked := AMasked;
  Result.PayloadLength := Length(APayload);
  Result.Payload := APayload;

  if AMasked then
  begin
    // Generate cryptographically random masking key (RFC 6455 Section 10.3)
    // Note: For client-side masking only. Server->Client frames MUST NOT be masked.
    Result.MaskingKey[0] := Byte(Random(256));
    Result.MaskingKey[1] := Byte(Random(256));
    Result.MaskingKey[2] := Byte(Random(256));
    Result.MaskingKey[3] := Byte(Random(256));
    // TODO: Use TRandomNumberGenerator for cryptographically secure random
  end
  else
  begin
    FillChar(Result.MaskingKey, SizeOf(Result.MaskingKey), 0);
  end;
end;

{ TMVCWebSocketFrameParser }

class procedure TMVCWebSocketFrameParser.ApplyMask(var Data: TBytes; const MaskKey: array of Byte);
var
  I: Integer;
begin
  for I := 0 to Length(Data) - 1 do
  begin
    Data[I] := Data[I] xor MaskKey[I mod 4];
  end;
end;

class function TMVCWebSocketFrameParser.ParseFrame(AIOHandler: TIdIOHandler; AServerSide: Boolean): TMVCWebSocketFrame;
var
  Byte1, Byte2: Byte;
  I: Integer;
begin
  // Read first two bytes
  Byte1 := AIOHandler.ReadByte;
  Byte2 := AIOHandler.ReadByte;

  // Parse first byte
  Result.Fin := (Byte1 and $80) <> 0;
  Result.Reserved := (Byte1 and $70) shr 4;
  Result.Opcode := TMVCWebSocketOpcode(Byte1 and $0F);

  // RFC 6455 Section 5.2: Reserved bits MUST be 0 unless extension negotiated
  if Result.Reserved <> 0 then
    raise EMVCWebSocketException.Create('Reserved bits must be 0 (no extensions negotiated)');

  // Parse second byte
  Result.Masked := (Byte2 and $80) <> 0;
  Result.PayloadLength := Byte2 and $7F;

  // RFC 6455 Section 5.1: Client->Server frames MUST be masked, Server->Client frames MUST NOT be masked
  if AServerSide then
  begin
    // Server side: expecting masked frames from client
    if not Result.Masked then
      raise EMVCWebSocketException.Create('Client frames must be masked (RFC 6455 Section 5.1)');
  end
  else
  begin
    // Client side: expecting unmasked frames from server
    if Result.Masked then
      raise EMVCWebSocketException.Create('Server frames must not be masked (RFC 6455 Section 5.1)');
  end;

  // RFC 6455 Section 5.5: Control frames validation
  // Control frames are identified by opcodes where the most significant bit is 1 (0x8-0xF)
  if Ord(Result.Opcode) >= 8 then
  begin
    // Control frames MUST NOT be fragmented
    if not Result.Fin then
      raise EMVCWebSocketException.Create('Control frames must not be fragmented (RFC 6455 Section 5.5)');

    // Control frames MUST have payload length <= 125
    if Result.PayloadLength > 125 then
      raise EMVCWebSocketException.Create('Control frame payload too large (max 125 bytes)');
  end;

  // Extended payload length
  // Note: Indy's ReadUInt16/ReadUInt64 read big-endian (network byte order) and
  // return values already converted to native endianness, so no manual swap needed
  if Result.PayloadLength = 126 then
  begin
    // 16-bit extended payload length (RFC 6455: network byte order)
    Result.PayloadLength := AIOHandler.ReadUInt16;
  end
  else if Result.PayloadLength = 127 then
  begin
    // 64-bit extended payload length (RFC 6455: network byte order)
    Result.PayloadLength := AIOHandler.ReadUInt64;
  end;

  // Read masking key if present
  if Result.Masked then
  begin
    for I := 0 to 3 do
      Result.MaskingKey[I] := AIOHandler.ReadByte;
  end;

  // Read payload data
  if Result.PayloadLength > 0 then
  begin
    SetLength(Result.Payload, Result.PayloadLength);
    AIOHandler.ReadBytes(TIdBytes(Result.Payload), Integer(Result.PayloadLength), False);

    // Unmask if needed
    if Result.Masked then
      ApplyMask(Result.Payload, Result.MaskingKey);
  end
  else
  begin
    SetLength(Result.Payload, 0);
  end;
end;

class procedure TMVCWebSocketFrameParser.WriteFrame(AIOHandler: TIdIOHandler;
  const AFrame: TMVCWebSocketFrame);
var
  Byte1, Byte2: Byte;
  ExtPayloadLen16: UInt16;
  ExtPayloadLen64: UInt64;
  PayloadToWrite: TBytes;
  I: Integer;
begin
  // Prepare first byte
  Byte1 := 0;
  if AFrame.Fin then
    Byte1 := Byte1 or $80;
  Byte1 := Byte1 or ((AFrame.Reserved and $07) shl 4);
  Byte1 := Byte1 or (Byte(Ord(AFrame.Opcode)) and $0F);

  // Prepare second byte with payload length
  Byte2 := 0;
  if AFrame.Masked then
    Byte2 := Byte2 or $80;

  if AFrame.PayloadLength < 126 then
  begin
    Byte2 := Byte2 or Byte(AFrame.PayloadLength);
  end
  else if AFrame.PayloadLength < 65536 then
  begin
    Byte2 := Byte2 or 126;
  end
  else
  begin
    Byte2 := Byte2 or 127;
  end;

  // Write header
  AIOHandler.Write(Byte1);
  AIOHandler.Write(Byte2);

  // Write extended payload length if needed
  if AFrame.PayloadLength >= 126 then
  begin
    if AFrame.PayloadLength < 65536 then
    begin
      // 16-bit length (big-endian) - write as individual bytes
      ExtPayloadLen16 := AFrame.PayloadLength;
      AIOHandler.Write(Byte((ExtPayloadLen16 shr 8) and $FF));  // High byte
      AIOHandler.Write(Byte(ExtPayloadLen16 and $FF));           // Low byte
    end
    else
    begin
      // 64-bit length (big-endian) - write as individual bytes
      ExtPayloadLen64 := AFrame.PayloadLength;
      AIOHandler.Write(Byte((ExtPayloadLen64 shr 56) and $FF));
      AIOHandler.Write(Byte((ExtPayloadLen64 shr 48) and $FF));
      AIOHandler.Write(Byte((ExtPayloadLen64 shr 40) and $FF));
      AIOHandler.Write(Byte((ExtPayloadLen64 shr 32) and $FF));
      AIOHandler.Write(Byte((ExtPayloadLen64 shr 24) and $FF));
      AIOHandler.Write(Byte((ExtPayloadLen64 shr 16) and $FF));
      AIOHandler.Write(Byte((ExtPayloadLen64 shr 8) and $FF));
      AIOHandler.Write(Byte(ExtPayloadLen64 and $FF));
    end;
  end;

  // Write masking key if needed
  if AFrame.Masked then
  begin
    for I := 0 to 3 do
      AIOHandler.Write(AFrame.MaskingKey[I]);
  end;

  // Write payload
  if AFrame.PayloadLength > 0 then
  begin
    PayloadToWrite := Copy(AFrame.Payload);

    // Apply mask if needed
    if AFrame.Masked then
      ApplyMask(PayloadToWrite, AFrame.MaskingKey);

    AIOHandler.Write(TIdBytes(PayloadToWrite), Integer(AFrame.PayloadLength));
  end;
end;

class function TMVCWebSocketFrameParser.CreateTextFrame(const AText: string;
  AMasked: Boolean): TMVCWebSocketFrame;
var
  Bytes: TBytes;
begin
  Bytes := TEncoding.UTF8.GetBytes(AText);
  Result := TMVCWebSocketFrame.Create(TMVCWebSocketOpcode.Text, Bytes, True, AMasked);
end;

class function TMVCWebSocketFrameParser.CreateBinaryFrame(const AData: TBytes;
  AMasked: Boolean): TMVCWebSocketFrame;
begin
  Result := TMVCWebSocketFrame.Create(TMVCWebSocketOpcode.Binary, AData, True, AMasked);
end;

class function TMVCWebSocketFrameParser.CreateCloseFrame(ACode: TMVCWebSocketCloseCode;
  const AReason: string; AMasked: Boolean): TMVCWebSocketFrame;
var
  Payload: TBytes;
  ReasonBytes: TBytes;
  Code: UInt16;
begin
  // Close frame payload: 2-byte status code + optional reason
  Code := Ord(ACode);
  // Convert to big-endian
  Code := ((Code and $FF) shl 8) or ((Code and $FF00) shr 8);

  if AReason <> '' then
  begin
    ReasonBytes := TEncoding.UTF8.GetBytes(AReason);
    SetLength(Payload, 2 + Length(ReasonBytes));
    Move(Code, Payload[0], 2);
    Move(ReasonBytes[0], Payload[2], Length(ReasonBytes));
  end
  else
  begin
    SetLength(Payload, 2);
    Move(Code, Payload[0], 2);
  end;

  Result := TMVCWebSocketFrame.Create(TMVCWebSocketOpcode.Close, Payload, True, AMasked);
end;

class function TMVCWebSocketFrameParser.CreatePingFrame(const AData: TBytes;
  AMasked: Boolean): TMVCWebSocketFrame;
begin
  Result := TMVCWebSocketFrame.Create(TMVCWebSocketOpcode.Ping, AData, True, AMasked);
end;

class function TMVCWebSocketFrameParser.CreatePongFrame(const AData: TBytes;
  AMasked: Boolean): TMVCWebSocketFrame;
begin
  Result := TMVCWebSocketFrame.Create(TMVCWebSocketOpcode.Pong, AData, True, AMasked);
end;

{ TMVCWebSocketHandshake }

class function TMVCWebSocketHandshake.CalculateAcceptKey(const AClientKey: string): string;
var
  Hash: TIdHashSHA1;
  HashBytes: TIdBytes;
begin
  // Concatenate client key with magic string and hash with SHA-1
  Hash := TIdHashSHA1.Create;
  try
    HashBytes := Hash.HashString(AClientKey + WEBSOCKET_MAGIC_STRING);
    Result := TIdEncoderMIME.EncodeBytes(HashBytes);
  finally
    Hash.Free;
  end;
end;

class function TMVCWebSocketHandshake.IsValidHandshakeRequest(const AUpgradeHeader,
  AConnectionHeader, AWebSocketVersion: string): Boolean;
begin
  Result := SameText(AUpgradeHeader, 'websocket') and
            (Pos('upgrade', LowerCase(AConnectionHeader)) > 0) and
            (AWebSocketVersion = '13');
end;

end.
