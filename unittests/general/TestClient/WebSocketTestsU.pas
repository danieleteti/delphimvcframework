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
  MVCFramework.WebSocket.RateLimiter;

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
    procedure TestMaskPayload;
  end;

  [TestFixture]
  TTestWebSocketHandshake = class
  public
    [Test]
    procedure TestCalculateAcceptKey;
    [Test]
    procedure TestIsValidHandshakeRequest;
  end;

  [TestFixture]
  TTestWebSocketRateLimiter = class
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestTokenBucketBasic;
    [Test]
    procedure TestTokenBucketRefill;
    [Test]
    procedure TestTokenBucketBurst;
    [Test]
    procedure TestConnectionRateLimiter;
  end;

implementation

uses
  System.NetEncoding,
  System.DateUtils;

{ TTestWebSocketFrame }

procedure TTestWebSocketFrame.TestCreateTextFrame;
var
  lFrame: TMVCWebSocketFrame;
  lText: string;
begin
  lText := 'Hello WebSocket!';
  lFrame := TMVCWebSocketFrameParser.CreateTextFrame(lText, False);

  Assert.AreEqual(Ord(TMVCWebSocketOpcode.Text), Ord(lFrame.Opcode), 'Opcode should be Text');
  Assert.IsTrue(lFrame.Fin, 'FIN bit should be set');
  Assert.IsFalse(lFrame.Masked, 'Frame should not be masked');
  Assert.AreEqual(Length(lText), Length(lFrame.PayloadData), 'Payload length should match');
end;

procedure TTestWebSocketFrame.TestCreateBinaryFrame;
var
  lFrame: TMVCWebSocketFrame;
  lData: TBytes;
begin
  SetLength(lData, 100);
  lData[0] := $FF;
  lData[99] := $AA;

  lFrame := TMVCWebSocketFrameParser.CreateBinaryFrame(lData, False);

  Assert.AreEqual(Ord(TMVCWebSocketOpcode.Binary), Ord(lFrame.Opcode), 'Opcode should be Binary');
  Assert.IsTrue(lFrame.Fin, 'FIN bit should be set');
  Assert.IsFalse(lFrame.Masked, 'Frame should not be masked');
  Assert.AreEqual(100, Length(lFrame.PayloadData), 'Payload length should be 100');
  Assert.AreEqual(lData[0], lFrame.PayloadData[0], 'First byte should match');
  Assert.AreEqual(lData[99], lFrame.PayloadData[99], 'Last byte should match');
end;

procedure TTestWebSocketFrame.TestCreateCloseFrame;
var
  lFrame: TMVCWebSocketFrame;
begin
  lFrame := TMVCWebSocketFrameParser.CreateCloseFrame(TMVCWebSocketCloseCode.NormalClosure, 'Goodbye');

  Assert.AreEqual(Ord(TMVCWebSocketOpcode.Close), Ord(lFrame.Opcode), 'Opcode should be Close');
  Assert.IsTrue(lFrame.Fin, 'FIN bit should be set');
end;

procedure TTestWebSocketFrame.TestCreatePingFrame;
var
  lFrame: TMVCWebSocketFrame;
  lData: TBytes;
begin
  SetLength(lData, 10);
  lData[0] := $12;
  lData[9] := $34;

  lFrame := TMVCWebSocketFrameParser.CreatePingFrame(lData, False);

  Assert.AreEqual(Ord(TMVCWebSocketOpcode.Ping), Ord(lFrame.Opcode), 'Opcode should be Ping');
  Assert.IsTrue(lFrame.Fin, 'FIN bit should be set');
  Assert.AreEqual(10, Length(lFrame.PayloadData), 'Payload length should be 10');
end;

procedure TTestWebSocketFrame.TestCreatePongFrame;
var
  lFrame: TMVCWebSocketFrame;
  lData: TBytes;
begin
  SetLength(lData, 10);
  lData[0] := $12;
  lData[9] := $34;

  lFrame := TMVCWebSocketFrameParser.CreatePongFrame(lData, False);

  Assert.AreEqual(Ord(TMVCWebSocketOpcode.Pong), Ord(lFrame.Opcode), 'Opcode should be Pong');
  Assert.IsTrue(lFrame.Fin, 'FIN bit should be set');
  Assert.AreEqual(10, Length(lFrame.PayloadData), 'Payload length should be 10');
end;

procedure TTestWebSocketFrame.TestMaskPayload;
var
  lOriginal, lMasked, lUnmasked: TBytes;
  lMaskKey: array[0..3] of Byte;
begin
  // Create test data
  SetLength(lOriginal, 10);
  lOriginal[0] := $00;
  lOriginal[5] := $55;
  lOriginal[9] := $FF;

  lMaskKey[0] := $12;
  lMaskKey[1] := $34;
  lMaskKey[2] := $56;
  lMaskKey[3] := $78;

  // Mask the data
  lMasked := Copy(lOriginal);
  TMVCWebSocketFrameParser.ApplyMask(lMasked, lMaskKey);

  // Verify it's different
  Assert.AreNotEqual(lOriginal[0], lMasked[0], 'Masked data should be different');

  // Unmask it
  lUnmasked := Copy(lMasked);
  TMVCWebSocketFrameParser.ApplyMask(lUnmasked, lMaskKey);

  // Verify it matches original (XOR property)
  Assert.AreEqual(lOriginal[0], lUnmasked[0], 'Unmasked byte 0 should match original');
  Assert.AreEqual(lOriginal[5], lUnmasked[5], 'Unmasked byte 5 should match original');
  Assert.AreEqual(lOriginal[9], lUnmasked[9], 'Unmasked byte 9 should match original');
end;

{ TTestWebSocketHandshake }

procedure TTestWebSocketHandshake.TestCalculateAcceptKey;
var
  lClientKey, lExpectedAccept, lActualAccept: string;
begin
  // Test with RFC 6455 example key
  lClientKey := 'dGhlIHNhbXBsZSBub25jZQ==';
  lExpectedAccept := 's3pPLMBiTxaQ9kYGzzhZRbK+xOo=';

  lActualAccept := TMVCWebSocketHandshake.CalculateAcceptKey(lClientKey);

  Assert.AreEqual(lExpectedAccept, lActualAccept, 'Accept key should match RFC 6455 example');
end;

procedure TTestWebSocketHandshake.TestIsValidHandshakeRequest;
var
  lUpgrade, lConnection, lSecKey, lVersion: string;
  lIsValid: Boolean;
begin
  // Valid handshake
  lUpgrade := 'websocket';
  lConnection := 'Upgrade';
  lSecKey := 'dGhlIHNhbXBsZSBub25jZQ==';
  lVersion := '13';

  lIsValid := TMVCWebSocketHandshake.IsValidHandshakeRequest(
    lUpgrade, lConnection, lSecKey, lVersion);

  Assert.IsTrue(lIsValid, 'Valid handshake should be accepted');

  // Invalid handshake - wrong version
  lIsValid := TMVCWebSocketHandshake.IsValidHandshakeRequest(
    lUpgrade, lConnection, lSecKey, '12');

  Assert.IsFalse(lIsValid, 'Invalid version should be rejected');

  // Invalid handshake - missing upgrade
  lIsValid := TMVCWebSocketHandshake.IsValidHandshakeRequest(
    '', lConnection, lSecKey, lVersion);

  Assert.IsFalse(lIsValid, 'Missing upgrade should be rejected');
end;

{ TTestWebSocketRateLimiter }

procedure TTestWebSocketRateLimiter.Setup;
begin
  // Setup code if needed
end;

procedure TTestWebSocketRateLimiter.TearDown;
begin
  // Cleanup code if needed
end;

procedure TTestWebSocketRateLimiter.TestTokenBucketBasic;
var
  lLimiter: TMVCWebSocketRateLimiter;
begin
  lLimiter := TMVCWebSocketRateLimiter.Create(10, 5.0);
  try
    // Should be able to consume up to max tokens
    Assert.IsTrue(lLimiter.TryConsume(1), 'Should consume 1 token');
    Assert.IsTrue(lLimiter.TryConsume(5), 'Should consume 5 tokens');
    Assert.IsTrue(lLimiter.TryConsume(4), 'Should consume 4 tokens');

    // Now we've consumed all 10 tokens, next should fail
    Assert.IsFalse(lLimiter.TryConsume(1), 'Should fail when tokens exhausted');
  finally
    lLimiter.Free;
  end;
end;

procedure TTestWebSocketRateLimiter.TestTokenBucketRefill;
var
  lLimiter: TMVCWebSocketRateLimiter;
  lAvailable: Integer;
begin
  lLimiter := TMVCWebSocketRateLimiter.Create(10, 5.0); // 5 tokens per second
  try
    // Consume all tokens
    lLimiter.TryConsume(10);
    Assert.AreEqual(0, lLimiter.GetAvailableTokens, 'Should have 0 tokens');

    // Wait 1 second for refill (5 tokens should be added)
    Sleep(1000);
    lAvailable := lLimiter.GetAvailableTokens;

    // Should have approximately 5 tokens (allowing some timing variance)
    Assert.IsTrue(lAvailable >= 4, Format('Should have ~5 tokens after 1 second, got %d', [lAvailable]));
  finally
    lLimiter.Free;
  end;
end;

procedure TTestWebSocketRateLimiter.TestTokenBucketBurst;
var
  lLimiter: TMVCWebSocketRateLimiter;
begin
  lLimiter := TMVCWebSocketRateLimiter.Create(20, 5.0); // 20 burst, 5 sustained
  try
    // Should be able to burst up to 20
    Assert.IsTrue(lLimiter.TryConsume(20), 'Should allow burst of 20');
    Assert.IsFalse(lLimiter.TryConsume(1), 'Should fail after burst');

    // Reset and test
    lLimiter.Reset;
    Assert.AreEqual(20, lLimiter.GetAvailableTokens, 'Should have full tokens after reset');
  finally
    lLimiter.Free;
  end;
end;

procedure TTestWebSocketRateLimiter.TestConnectionRateLimiter;
var
  lConnLimiter: TMVCConnectionRateLimiter;
begin
  lConnLimiter := TMVCConnectionRateLimiter.Create('test-conn', 10, 1000); // 10 msg/sec, 1000 bytes/sec
  try
    // Should be able to send small messages
    Assert.IsTrue(lConnLimiter.RecordMessage(100), 'Message 1 should succeed');
    Assert.IsTrue(lConnLimiter.RecordMessage(100), 'Message 2 should succeed');
    Assert.IsTrue(lConnLimiter.RecordMessage(100), 'Message 3 should succeed');

    // Try to send more - should eventually hit byte limit or message limit
    Assert.IsTrue(lConnLimiter.RecordMessage(100), 'Message 4 should succeed');
    Assert.IsTrue(lConnLimiter.RecordMessage(100), 'Message 5 should succeed');

    // After consuming many tokens, we should hit a limit
    // Burst capacity is 2x, so we have 20 messages and 2000 bytes
    // But eventually we'll hit a limit
    lConnLimiter.RecordMessage(100); // Message 6
    lConnLimiter.RecordMessage(100); // Message 7
    lConnLimiter.RecordMessage(100); // Message 8
    lConnLimiter.RecordMessage(100); // Message 9
    lConnLimiter.RecordMessage(100); // Message 10

    // At this point we've sent 10 messages = 1000 bytes
    // We have burst capacity so more should work
    Assert.IsTrue(lConnLimiter.RecordMessage(100), 'Burst message should succeed');

    // But eventually the burst will be exhausted
    // Let's try to consume all remaining burst
    while lConnLimiter.RecordMessage(100) do
      ; // Consume all

    // Now it should fail
    Assert.IsFalse(lConnLimiter.RecordMessage(100), 'Should fail when limits exhausted');
  finally
    lConnLimiter.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestWebSocketFrame);
  TDUnitX.RegisterTestFixture(TTestWebSocketHandshake);
  TDUnitX.RegisterTestFixture(TTestWebSocketRateLimiter);

end.
