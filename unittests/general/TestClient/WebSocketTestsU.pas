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

unit WebSocketTestsU;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  MVCFramework.WebSocket,
  MVCFramework.WebSocket.ConnectionManager,
  IdIOHandler,
  IdIOHandlerStack;

type
  [TestFixture]
  TTestWebSocketFrame = class
  public
    [Test]
    procedure TestCreateTextFrame;
    [Test]
    procedure TestCreateBinaryFrame;
    [Test]
    procedure TestCreateCloseFrame;
    [Test]
    procedure TestCreatePingFrame;
    [Test]
    procedure TestCreatePongFrame;
    [Test]
    procedure TestFrameWithMasking;
  end;

  [TestFixture]
  TTestWebSocketHandshake = class
  public
    [Test]
    procedure TestCalculateAcceptKey;
    [Test]
    procedure TestValidHandshakeRequest;
    [Test]
    procedure TestInvalidHandshakeRequest;
  end;

  [TestFixture]
  TTestWebSocketFrameParser = class
  private
    FMemoryStream: TMemoryStream;
    FIOHandler: TIdIOHandlerStack;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestParseTextFrame;
    [Test]
    procedure TestParseBinaryFrame;
    [Test]
    procedure TestParseCloseFrame;
    [Test]
    procedure TestParsePingPongFrame;
    [Test]
    procedure TestParseFrameWithExtendedLength16;
    [Test]
    procedure TestParseFrameWithExtendedLength64;
    [Test]
    procedure TestWriteAndParseFrame;
  end;

implementation

uses
  IdGlobal,
  System.NetEncoding;

{ TTestWebSocketFrame }

procedure TTestWebSocketFrame.TestCreateTextFrame;
var
  Frame: TMVCWebSocketFrame;
  Text: string;
begin
  Text := 'Hello WebSocket!';
  Frame := TMVCWebSocketFrameParser.CreateTextFrame(Text, False);

  Assert.IsTrue(Frame.Fin, 'Fin should be True');
  Assert.AreEqual(Ord(TMVCWebSocketOpcode.Text), Ord(Frame.Opcode), 'Opcode should be Text');
  Assert.IsFalse(Frame.Masked, 'Should not be masked');
  Assert.AreEqual(Length(TEncoding.UTF8.GetBytes(Text)), Integer(Frame.PayloadLength),
    'Payload length should match text byte length');
end;

procedure TTestWebSocketFrame.TestCreateBinaryFrame;
var
  Frame: TMVCWebSocketFrame;
  Data: TBytes;
begin
  SetLength(Data, 10);
  FillChar(Data[0], 10, $AB);

  Frame := TMVCWebSocketFrameParser.CreateBinaryFrame(Data, False);

  Assert.IsTrue(Frame.Fin, 'Fin should be True');
  Assert.AreEqual(Ord(TMVCWebSocketOpcode.Binary), Ord(Frame.Opcode), 'Opcode should be Binary');
  Assert.AreEqual(10, Integer(Frame.PayloadLength), 'Payload length should be 10');
end;

procedure TTestWebSocketFrame.TestCreateCloseFrame;
var
  Frame: TMVCWebSocketFrame;
  Code: UInt16;
begin
  Frame := TMVCWebSocketFrameParser.CreateCloseFrame(
    TMVCWebSocketCloseCode.NormalClosure, 'Goodbye', False);

  Assert.IsTrue(Frame.Fin, 'Fin should be True');
  Assert.AreEqual(Ord(TMVCWebSocketOpcode.Close), Ord(Frame.Opcode), 'Opcode should be Close');

  // Check close code (big-endian)
  Code := (Frame.Payload[0] shl 8) or Frame.Payload[1];
  Assert.AreEqual(1000, Integer(Code), 'Close code should be 1000');
end;

procedure TTestWebSocketFrame.TestCreatePingFrame;
var
  Frame: TMVCWebSocketFrame;
begin
  Frame := TMVCWebSocketFrameParser.CreatePingFrame(nil, False);

  Assert.IsTrue(Frame.Fin, 'Fin should be True');
  Assert.AreEqual(Ord(TMVCWebSocketOpcode.Ping), Ord(Frame.Opcode), 'Opcode should be Ping');
end;

procedure TTestWebSocketFrame.TestCreatePongFrame;
var
  Frame: TMVCWebSocketFrame;
begin
  Frame := TMVCWebSocketFrameParser.CreatePongFrame(nil, False);

  Assert.IsTrue(Frame.Fin, 'Fin should be True');
  Assert.AreEqual(Ord(TMVCWebSocketOpcode.Pong), Ord(Frame.Opcode), 'Opcode should be Pong');
end;

procedure TTestWebSocketFrame.TestFrameWithMasking;
var
  Frame: TMVCWebSocketFrame;
begin
  Frame := TMVCWebSocketFrameParser.CreateTextFrame('Test', True);

  Assert.IsTrue(Frame.Masked, 'Frame should be masked');
  Assert.AreNotEqual(0, Frame.MaskingKey[0], 'Masking key should be generated');
end;

{ TTestWebSocketHandshake }

procedure TTestWebSocketHandshake.TestCalculateAcceptKey;
var
  ClientKey, AcceptKey: string;
begin
  // Test with known value from RFC 6455
  ClientKey := 'dGhlIHNhbXBsZSBub25jZQ==';
  AcceptKey := TMVCWebSocketHandshake.CalculateAcceptKey(ClientKey);

  Assert.AreEqual('s3pPLMBiTxaQ9kYGzzhZRbK+xOo=', AcceptKey,
    'Accept key calculation failed');
end;

procedure TTestWebSocketHandshake.TestValidHandshakeRequest;
begin
  Assert.IsTrue(
    TMVCWebSocketHandshake.IsValidHandshakeRequest('websocket', 'upgrade', '13'),
    'Should accept valid handshake');

  Assert.IsTrue(
    TMVCWebSocketHandshake.IsValidHandshakeRequest('WebSocket', 'Upgrade', '13'),
    'Should be case-insensitive');
end;

procedure TTestWebSocketHandshake.TestInvalidHandshakeRequest;
begin
  Assert.IsFalse(
    TMVCWebSocketHandshake.IsValidHandshakeRequest('http', 'upgrade', '13'),
    'Should reject invalid upgrade header');

  Assert.IsFalse(
    TMVCWebSocketHandshake.IsValidHandshakeRequest('websocket', 'keep-alive', '13'),
    'Should reject invalid connection header');

  Assert.IsFalse(
    TMVCWebSocketHandshake.IsValidHandshakeRequest('websocket', 'upgrade', '12'),
    'Should reject invalid WebSocket version');
end;

{ TTestWebSocketFrameParser }

procedure TTestWebSocketFrameParser.Setup;
begin
  FMemoryStream := TMemoryStream.Create;
  FIOHandler := TIdIOHandlerStack.Create(nil);
end;

procedure TTestWebSocketFrameParser.TearDown;
begin
  FIOHandler.Free;
  FMemoryStream.Free;
end;

procedure TTestWebSocketFrameParser.TestParseTextFrame;
var
  OriginalFrame, ParsedFrame: TMVCWebSocketFrame;
  Stream: TMemoryStream;
  IOHandler: TIdIOHandlerStack;
  Text: string;
begin
  Stream := TMemoryStream.Create;
  IOHandler := TIdIOHandlerStack.Create(nil);
  try
    Text := 'Hello!';
    OriginalFrame := TMVCWebSocketFrameParser.CreateTextFrame(Text, False);

    // Write to stream
    TMVCWebSocketFrameParser.WriteFrame(IOHandler, OriginalFrame);
    Stream.Position := 0;

    // Parse from stream
    ParsedFrame := TMVCWebSocketFrameParser.ParseFrame(IOHandler);

    Assert.AreEqual(Ord(OriginalFrame.Opcode), Ord(ParsedFrame.Opcode), 'Opcode mismatch');
    Assert.AreEqual(OriginalFrame.Fin, ParsedFrame.Fin, 'Fin mismatch');
    Assert.AreEqual(Integer(OriginalFrame.PayloadLength), Integer(ParsedFrame.PayloadLength),
      'Payload length mismatch');

    Assert.AreEqual(Text, TEncoding.UTF8.GetString(ParsedFrame.Payload),
      'Payload content mismatch');
  finally
    IOHandler.Free;
    Stream.Free;
  end;
end;

procedure TTestWebSocketFrameParser.TestParseBinaryFrame;
var
  OriginalFrame, ParsedFrame: TMVCWebSocketFrame;
  Stream: TMemoryStream;
  IOHandler: TIdIOHandlerStack;
  Data: TBytes;
  I: Integer;
begin
  Stream := TMemoryStream.Create;
  IOHandler := TIdIOHandlerStack.Create(nil);
  try
    SetLength(Data, 100);
    for I := 0 to 99 do
      Data[I] := I mod 256;

    OriginalFrame := TMVCWebSocketFrameParser.CreateBinaryFrame(Data, False);

    // Write to stream
    TMVCWebSocketFrameParser.WriteFrame(IOHandler, OriginalFrame);
    Stream.Position := 0;

    // Parse from stream
    ParsedFrame := TMVCWebSocketFrameParser.ParseFrame(IOHandler);

    Assert.AreEqual(Ord(TMVCWebSocketOpcode.Binary), Ord(ParsedFrame.Opcode),
      'Opcode should be Binary');
    Assert.AreEqual(100, Integer(ParsedFrame.PayloadLength), 'Payload length mismatch');
  finally
    IOHandler.Free;
    Stream.Free;
  end;
end;

procedure TTestWebSocketFrameParser.TestParseCloseFrame;
var
  OriginalFrame, ParsedFrame: TMVCWebSocketFrame;
  Stream: TMemoryStream;
  IOHandler: TIdIOHandlerStack;
begin
  Stream := TMemoryStream.Create;
  IOHandler := TIdIOHandlerStack.Create(nil);
  try
    OriginalFrame := TMVCWebSocketFrameParser.CreateCloseFrame(
      TMVCWebSocketCloseCode.NormalClosure, 'Done', False);

    // Write to stream
    TMVCWebSocketFrameParser.WriteFrame(IOHandler, OriginalFrame);
    Stream.Position := 0;

    // Parse from stream
    ParsedFrame := TMVCWebSocketFrameParser.ParseFrame(IOHandler);

    Assert.AreEqual(Ord(TMVCWebSocketOpcode.Close), Ord(ParsedFrame.Opcode),
      'Opcode should be Close');
    Assert.IsTrue(Length(ParsedFrame.Payload) >= 2, 'Close frame should have payload');
  finally
    IOHandler.Free;
    Stream.Free;
  end;
end;

procedure TTestWebSocketFrameParser.TestParsePingPongFrame;
var
  PingFrame, PongFrame, ParsedFrame: TMVCWebSocketFrame;
  Stream: TMemoryStream;
  IOHandler: TIdIOHandlerStack;
begin
  Stream := TMemoryStream.Create;
  IOHandler := TIdIOHandlerStack.Create(nil);
  try
    // Test Ping
    PingFrame := TMVCWebSocketFrameParser.CreatePingFrame(nil, False);
    TMVCWebSocketFrameParser.WriteFrame(IOHandler, PingFrame);
    Stream.Position := 0;
    ParsedFrame := TMVCWebSocketFrameParser.ParseFrame(IOHandler);
    Assert.AreEqual(Ord(TMVCWebSocketOpcode.Ping), Ord(ParsedFrame.Opcode),
      'Should parse Ping frame');

    // Test Pong
    Stream.Clear;
    Stream.Position := 0;
    PongFrame := TMVCWebSocketFrameParser.CreatePongFrame(nil, False);
    TMVCWebSocketFrameParser.WriteFrame(IOHandler, PongFrame);
    Stream.Position := 0;
    ParsedFrame := TMVCWebSocketFrameParser.ParseFrame(IOHandler);
    Assert.AreEqual(Ord(TMVCWebSocketOpcode.Pong), Ord(ParsedFrame.Opcode),
      'Should parse Pong frame');
  finally
    IOHandler.Free;
    Stream.Free;
  end;
end;

procedure TTestWebSocketFrameParser.TestParseFrameWithExtendedLength16;
var
  OriginalFrame, ParsedFrame: TMVCWebSocketFrame;
  Stream: TMemoryStream;
  IOHandler: TIdIOHandlerStack;
  Data: TBytes;
begin
  Stream := TMemoryStream.Create;
  IOHandler := TIdIOHandlerStack.Create(nil);
  try
    // Create frame with payload > 125 bytes (requires 16-bit length)
    SetLength(Data, 1000);
    FillChar(Data[0], 1000, $AA);

    OriginalFrame := TMVCWebSocketFrameParser.CreateBinaryFrame(Data, False);

    // Write and parse
    TMVCWebSocketFrameParser.WriteFrame(IOHandler, OriginalFrame);
    Stream.Position := 0;
    ParsedFrame := TMVCWebSocketFrameParser.ParseFrame(IOHandler);

    Assert.AreEqual(1000, Integer(ParsedFrame.PayloadLength),
      'Should handle 16-bit extended length');
  finally
    IOHandler.Free;
    Stream.Free;
  end;
end;

procedure TTestWebSocketFrameParser.TestParseFrameWithExtendedLength64;
var
  OriginalFrame, ParsedFrame: TMVCWebSocketFrame;
  Stream: TMemoryStream;
  IOHandler: TIdIOHandlerStack;
  Data: TBytes;
begin
  Stream := TMemoryStream.Create;
  IOHandler := TIdIOHandlerStack.Create(nil);
  try
    // Create frame with payload > 65535 bytes (requires 64-bit length)
    SetLength(Data, 70000);
    FillChar(Data[0], 70000, $BB);

    OriginalFrame := TMVCWebSocketFrameParser.CreateBinaryFrame(Data, False);

    // Write and parse
    TMVCWebSocketFrameParser.WriteFrame(IOHandler, OriginalFrame);
    Stream.Position := 0;
    ParsedFrame := TMVCWebSocketFrameParser.ParseFrame(IOHandler);

    Assert.AreEqual(70000, Integer(ParsedFrame.PayloadLength),
      'Should handle 64-bit extended length');
  finally
    IOHandler.Free;
    Stream.Free;
  end;
end;

procedure TTestWebSocketFrameParser.TestWriteAndParseFrame;
var
  OriginalFrame, ParsedFrame: TMVCWebSocketFrame;
  Stream: TMemoryStream;
  IOHandler: TIdIOHandlerStack;
  Text: string;
begin
  Stream := TMemoryStream.Create;
  IOHandler := TIdIOHandlerStack.Create(nil);
  try
    Text := 'Test message with UTF-8: äöü';
    OriginalFrame := TMVCWebSocketFrameParser.CreateTextFrame(Text, True); // With masking

    // Write to stream
    TMVCWebSocketFrameParser.WriteFrame(IOHandler, OriginalFrame);
    Stream.Position := 0;

    // Parse from stream
    ParsedFrame := TMVCWebSocketFrameParser.ParseFrame(IOHandler);

    Assert.AreEqual(Text, TEncoding.UTF8.GetString(ParsedFrame.Payload),
      'Should correctly encode/decode UTF-8 with masking');
  finally
    IOHandler.Free;
    Stream.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestWebSocketFrame);
  TDUnitX.RegisterTestFixture(TTestWebSocketHandshake);
  TDUnitX.RegisterTestFixture(TTestWebSocketFrameParser);

end.
