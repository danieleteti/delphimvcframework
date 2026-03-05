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

unit SSETestsU;

interface

uses
  DUnitX.TestFramework;

type
  { Tests for TSSEMessage record }
  [TestFixture]
  TTestSSEMessage = class
  public
    [Test]
    procedure TestCreate;
    [Test]
    procedure TestCreateWithoutId;
    [Test]
    procedure TestCreateEmptyFields;
  end;

  { Tests for TSSEConnection }
  [TestFixture]
  TTestSSEConnection = class
  public
    [Test]
    procedure TestCreateAndDestroy;
    [Test]
    procedure TestSendAndDequeue;
    [Test]
    procedure TestSendMultipleMessages;
    [Test]
    procedure TestSendComment;
    [Test]
    procedure TestDisconnectSendsDisconnectItem;
    [Test]
    procedure TestIsConnectedAfterCreate;
    [Test]
    procedure TestMarkDisconnected;
    [Test]
    procedure TestSendAfterDisconnectIsIgnored;
    [Test]
    procedure TestWaitForDataTimeout;
    [Test]
    procedure TestWaitForDataSignaled;
    [Test]
    procedure TestDequeueAllClearsQueue;
    [Test]
    procedure TestGroupManagement;
    [Test]
    procedure TestCustomData;
    [Test]
    procedure TestCustomDataOwnership;
    [Test]
    procedure TestLastEventId;
  end;

  { Tests for TMVCSSEBroker }
  [TestFixture]
  TTestSSEBroker = class
  public
    [Test]
    procedure TestSingletonInstance;
    [Test]
    procedure TestRegisterAndUnregister;
    [Test]
    procedure TestBroadcast;
    [Test]
    procedure TestBroadcastToEmptyChannel;
    [Test]
    procedure TestBroadcastToGroup;
    [Test]
    procedure TestSendTo;
    [Test]
    procedure TestSendToNonExistentClient;
    [Test]
    procedure TestConnectionCount;
    [Test]
    procedure TestGetClientIds;
    [Test]
    procedure TestGetChannelNames;
    [Test]
    procedure TestChannelRemovedWhenEmpty;
    [Test]
    procedure TestMultipleChannels;
    [Test]
    procedure TestBroadcastMultipleConnections;
  end;

  { Tests for SSE client-side parser (TSSEClientParser) }
  [TestFixture]
  TTestSSEClientParser = class
  public
    [Test]
    procedure TestSimpleMessage;
    [Test]
    procedure TestMessageWithAllFields;
    [Test]
    procedure TestMultipleMessages;
    [Test]
    procedure TestMultilineData;
    [Test]
    procedure TestCommentLinesIgnored;
    [Test]
    procedure TestRetryField;
    [Test]
    procedure TestLastEventIdUpdated;
    [Test]
    procedure TestIdWithNullCharNotSaved;
    [Test]
    procedure TestDataWithColons;
    [Test]
    procedure TestEmptyDataField;
    [Test]
    procedure TestFieldWithNoColon;
    [Test]
    procedure TestUnknownFieldIgnored;
    [Test]
    procedure TestIncrementalFeeding;
    [Test]
    procedure TestCRLFLineEndings;
    [Test]
    procedure TestCRLineEndings;
    [Test]
    procedure TestLFLineEndings;
    [Test]
    procedure TestEmptyEventDispatchesAsEmpty;
    [Test]
    procedure TestDefaultEventIsEmpty;
    [Test]
    procedure TestEventResetBetweenMessages;
    [Test]
    procedure TestNoSpaceAfterColon;
    [Test]
    procedure TestMultipleSpacesAfterColon;
  end;

  { Integration test: Broker + Connection push flow }
  [TestFixture]
  TTestSSEIntegration = class
  public
    [Test]
    procedure TestBrokerPushToConnection;
    [Test]
    procedure TestMultipleClientsReceiveBroadcast;
    [Test]
    procedure TestGroupBroadcastFiltersCorrectly;
    [Test]
    procedure TestDisconnectedClientSkipped;
    [Test]
    procedure TestConcurrentBroadcast;
  end;

implementation

uses
  System.SysUtils, System.Classes, System.SyncObjs, System.Generics.Collections,
  MVCFramework.SSE, MVCFramework.SSEClient;

{ ========================================================================= }
{ TTestSSEMessage                                                           }
{ ========================================================================= }

procedure TTestSSEMessage.TestCreate;
var
  LMsg: TSSEMessage;
begin
  LMsg := TSSEMessage.Create('stockupdate', '{"price":100}', '42');
  Assert.AreEqual('stockupdate', LMsg.Event);
  Assert.AreEqual('{"price":100}', LMsg.Data);
  Assert.AreEqual('42', LMsg.Id);
end;

procedure TTestSSEMessage.TestCreateWithoutId;
var
  LMsg: TSSEMessage;
begin
  LMsg := TSSEMessage.Create('ping', 'hello');
  Assert.AreEqual('ping', LMsg.Event);
  Assert.AreEqual('hello', LMsg.Data);
  Assert.AreEqual('', LMsg.Id);
end;

procedure TTestSSEMessage.TestCreateEmptyFields;
var
  LMsg: TSSEMessage;
begin
  LMsg := TSSEMessage.Create('', '');
  Assert.AreEqual('', LMsg.Event);
  Assert.AreEqual('', LMsg.Data);
  Assert.AreEqual('', LMsg.Id);
end;

{ ========================================================================= }
{ TTestSSEConnection                                                        }
{ ========================================================================= }

procedure TTestSSEConnection.TestCreateAndDestroy;
var
  LConn: TSSEConnection;
begin
  LConn := TSSEConnection.Create('client-1');
  try
    Assert.AreEqual('client-1', LConn.ClientId);
    Assert.IsTrue(LConn.IsConnected);
  finally
    LConn.Free;
  end;
end;

procedure TTestSSEConnection.TestSendAndDequeue;
var
  LConn: TSSEConnection;
  LItems: TArray<TSSEQueueItem>;
begin
  LConn := TSSEConnection.Create('c1');
  try
    LConn.Send(TSSEMessage.Create('test', 'data1', '1'));
    LItems := LConn.DequeueAll;
    Assert.AreEqual<Integer>(1, Length(LItems));
    Assert.IsTrue(LItems[0].Kind = ssMessage);
    Assert.AreEqual('test', LItems[0].Message.Event);
    Assert.AreEqual('data1', LItems[0].Message.Data);
    Assert.AreEqual('1', LItems[0].Message.Id);
  finally
    LConn.Free;
  end;
end;

procedure TTestSSEConnection.TestSendMultipleMessages;
var
  LConn: TSSEConnection;
  LItems: TArray<TSSEQueueItem>;
begin
  LConn := TSSEConnection.Create('c1');
  try
    LConn.Send(TSSEMessage.Create('e1', 'd1', '1'));
    LConn.Send(TSSEMessage.Create('e2', 'd2', '2'));
    LConn.Send(TSSEMessage.Create('e3', 'd3', '3'));
    LItems := LConn.DequeueAll;
    Assert.AreEqual<Integer>(3, Length(LItems));
    Assert.AreEqual('d1', LItems[0].Message.Data);
    Assert.AreEqual('d2', LItems[1].Message.Data);
    Assert.AreEqual('d3', LItems[2].Message.Data);
  finally
    LConn.Free;
  end;
end;

procedure TTestSSEConnection.TestSendComment;
var
  LConn: TSSEConnection;
  LItems: TArray<TSSEQueueItem>;
begin
  LConn := TSSEConnection.Create('c1');
  try
    LConn.SendComment('heartbeat');
    LItems := LConn.DequeueAll;
    Assert.AreEqual<Integer>(1, Length(LItems));
    Assert.IsTrue(LItems[0].Kind = ssComment);
    Assert.AreEqual('heartbeat', LItems[0].Comment);
  finally
    LConn.Free;
  end;
end;

procedure TTestSSEConnection.TestDisconnectSendsDisconnectItem;
var
  LConn: TSSEConnection;
  LItems: TArray<TSSEQueueItem>;
begin
  LConn := TSSEConnection.Create('c1');
  try
    LConn.Disconnect;
    LItems := LConn.DequeueAll;
    Assert.AreEqual<Integer>(1, Length(LItems));
    Assert.IsTrue(LItems[0].Kind = ssDisconnect);
  finally
    LConn.Free;
  end;
end;

procedure TTestSSEConnection.TestIsConnectedAfterCreate;
var
  LConn: TSSEConnection;
begin
  LConn := TSSEConnection.Create('c1');
  try
    Assert.IsTrue(LConn.IsConnected);
  finally
    LConn.Free;
  end;
end;

procedure TTestSSEConnection.TestMarkDisconnected;
var
  LConn: TSSEConnection;
begin
  LConn := TSSEConnection.Create('c1');
  try
    Assert.IsTrue(LConn.IsConnected);
    LConn.MarkDisconnected;
    Assert.IsFalse(LConn.IsConnected);
  finally
    LConn.Free;
  end;
end;

procedure TTestSSEConnection.TestSendAfterDisconnectIsIgnored;
var
  LConn: TSSEConnection;
  LItems: TArray<TSSEQueueItem>;
begin
  LConn := TSSEConnection.Create('c1');
  try
    LConn.MarkDisconnected;
    LConn.Send(TSSEMessage.Create('test', 'data'));
    LConn.SendComment('nope');
    LItems := LConn.DequeueAll;
    Assert.AreEqual<Integer>(0, Length(LItems));
  finally
    LConn.Free;
  end;
end;

procedure TTestSSEConnection.TestWaitForDataTimeout;
var
  LConn: TSSEConnection;
  LResult: TWaitResult;
begin
  LConn := TSSEConnection.Create('c1');
  try
    LResult := LConn.WaitForData(50); // 50ms timeout
    Assert.IsTrue(LResult = wrTimeout);
  finally
    LConn.Free;
  end;
end;

procedure TTestSSEConnection.TestWaitForDataSignaled;
var
  LConn: TSSEConnection;
  LResult: TWaitResult;
begin
  LConn := TSSEConnection.Create('c1');
  try
    // Send a message first, then wait - should be signaled immediately
    LConn.Send(TSSEMessage.Create('test', 'data'));
    LResult := LConn.WaitForData(1000);
    Assert.IsTrue(LResult = wrSignaled);
  finally
    LConn.Free;
  end;
end;

procedure TTestSSEConnection.TestDequeueAllClearsQueue;
var
  LConn: TSSEConnection;
  LItems: TArray<TSSEQueueItem>;
begin
  LConn := TSSEConnection.Create('c1');
  try
    LConn.Send(TSSEMessage.Create('test', 'data'));
    LItems := LConn.DequeueAll;
    Assert.AreEqual<Integer>(1, Length(LItems));
    // Second dequeue should be empty
    LItems := LConn.DequeueAll;
    Assert.AreEqual<Integer>(0, Length(LItems));
  finally
    LConn.Free;
  end;
end;

procedure TTestSSEConnection.TestGroupManagement;
var
  LConn: TSSEConnection;
  LGroups: TArray<string>;
begin
  LConn := TSSEConnection.Create('c1');
  try
    Assert.IsFalse(LConn.IsInGroup('vip'));

    LConn.JoinGroup('vip');
    Assert.IsTrue(LConn.IsInGroup('vip'));
    Assert.IsFalse(LConn.IsInGroup('admin'));

    LConn.JoinGroup('admin');
    LGroups := LConn.GetGroups;
    Assert.AreEqual<Integer>(2, Length(LGroups));

    // Join same group again - should not duplicate
    LConn.JoinGroup('vip');
    LGroups := LConn.GetGroups;
    Assert.AreEqual<Integer>(2, Length(LGroups));

    LConn.LeaveGroup('vip');
    Assert.IsFalse(LConn.IsInGroup('vip'));
    Assert.IsTrue(LConn.IsInGroup('admin'));
  finally
    LConn.Free;
  end;
end;

procedure TTestSSEConnection.TestCustomData;
var
  LConn: TSSEConnection;
  LObj: TStringList;
begin
  LConn := TSSEConnection.Create('c1');
  try
    Assert.IsNull(LConn.CustomData);
    LObj := TStringList.Create;
    LObj.Add('test');
    LConn.CustomData := LObj;
    LConn.OwnsCustomData := False;
    Assert.IsNotNull(LConn.CustomData);
    Assert.AreEqual('test', TStringList(LConn.CustomData)[0]);
  finally
    LConn.Free;
    LObj.Free;
  end;
end;

procedure TTestSSEConnection.TestCustomDataOwnership;
var
  LConn: TSSEConnection;
  LObj: TStringList;
begin
  LConn := TSSEConnection.Create('c1');
  LObj := TStringList.Create;
  LConn.CustomData := LObj;
  LConn.OwnsCustomData := True;
  // When LConn is freed, LObj should also be freed (no leak)
  LConn.Free;
  // If we get here without AV, ownership works
  Assert.Pass;
end;

procedure TTestSSEConnection.TestLastEventId;
var
  LConn: TSSEConnection;
begin
  LConn := TSSEConnection.Create('c1');
  try
    Assert.AreEqual('', LConn.LastEventId);
    LConn.LastEventId := '42';
    Assert.AreEqual('42', LConn.LastEventId);
    LConn.LastEventId := 'abc-def';
    Assert.AreEqual('abc-def', LConn.LastEventId);
  finally
    LConn.Free;
  end;
end;

{ ========================================================================= }
{ TTestSSEBroker                                                            }
{ ========================================================================= }

procedure TTestSSEBroker.TestSingletonInstance;
var
  LBroker1, LBroker2: TMVCSSEBroker;
begin
  LBroker1 := TMVCSSEBroker.Instance;
  LBroker2 := TMVCSSEBroker.Instance;
  Assert.AreSame(LBroker1, LBroker2);
  Assert.AreSame(LBroker1, SSEBroker);
end;

procedure TTestSSEBroker.TestRegisterAndUnregister;
var
  LBroker: TMVCSSEBroker;
  LConn: TSSEConnection;
begin
  LBroker := TMVCSSEBroker.Create;
  try
    LConn := TSSEConnection.Create('c1');
    try
      LBroker.RegisterConnection('/test', LConn);
      Assert.AreEqual(1, LBroker.ConnectionCount('/test'));
      LBroker.UnregisterConnection('/test', LConn);
      Assert.AreEqual(0, LBroker.ConnectionCount('/test'));
    finally
      LConn.Free;
    end;
  finally
    LBroker.Free;
  end;
end;

procedure TTestSSEBroker.TestBroadcast;
var
  LBroker: TMVCSSEBroker;
  LConn: TSSEConnection;
  LItems: TArray<TSSEQueueItem>;
begin
  LBroker := TMVCSSEBroker.Create;
  try
    LConn := TSSEConnection.Create('c1');
    try
      LBroker.RegisterConnection('/ch', LConn);
      LBroker.Broadcast('/ch', TSSEMessage.Create('evt', 'hello', '1'));

      LItems := LConn.DequeueAll;
      Assert.AreEqual<Integer>(1, Length(LItems));
      Assert.AreEqual('hello', LItems[0].Message.Data);
    finally
      LBroker.UnregisterConnection('/ch', LConn);
      LConn.Free;
    end;
  finally
    LBroker.Free;
  end;
end;

procedure TTestSSEBroker.TestBroadcastToEmptyChannel;
var
  LBroker: TMVCSSEBroker;
begin
  LBroker := TMVCSSEBroker.Create;
  try
    // Should not raise
    LBroker.Broadcast('/nonexistent', TSSEMessage.Create('evt', 'data'));
    Assert.Pass;
  finally
    LBroker.Free;
  end;
end;

procedure TTestSSEBroker.TestBroadcastToGroup;
var
  LBroker: TMVCSSEBroker;
  LConn1, LConn2: TSSEConnection;
  LItems: TArray<TSSEQueueItem>;
begin
  LBroker := TMVCSSEBroker.Create;
  try
    LConn1 := TSSEConnection.Create('c1');
    LConn2 := TSSEConnection.Create('c2');
    try
      LConn1.JoinGroup('vip');
      // LConn2 is NOT in 'vip' group
      LBroker.RegisterConnection('/ch', LConn1);
      LBroker.RegisterConnection('/ch', LConn2);

      LBroker.BroadcastToGroup('/ch', 'vip', TSSEMessage.Create('evt', 'vip-only'));

      LItems := LConn1.DequeueAll;
      Assert.AreEqual<Integer>(1, Length(LItems));
      Assert.AreEqual('vip-only', LItems[0].Message.Data);

      LItems := LConn2.DequeueAll;
      Assert.AreEqual<Integer>(0, Length(LItems));
    finally
      LBroker.UnregisterConnection('/ch', LConn1);
      LBroker.UnregisterConnection('/ch', LConn2);
      LConn1.Free;
      LConn2.Free;
    end;
  finally
    LBroker.Free;
  end;
end;

procedure TTestSSEBroker.TestSendTo;
var
  LBroker: TMVCSSEBroker;
  LConn1, LConn2: TSSEConnection;
  LItems: TArray<TSSEQueueItem>;
begin
  LBroker := TMVCSSEBroker.Create;
  try
    LConn1 := TSSEConnection.Create('alice');
    LConn2 := TSSEConnection.Create('bob');
    try
      LBroker.RegisterConnection('/ch', LConn1);
      LBroker.RegisterConnection('/ch', LConn2);

      LBroker.SendTo('/ch', 'bob', TSSEMessage.Create('dm', 'hello bob'));

      LItems := LConn1.DequeueAll;
      Assert.AreEqual<Integer>(0, Length(LItems));

      LItems := LConn2.DequeueAll;
      Assert.AreEqual<Integer>(1, Length(LItems));
      Assert.AreEqual('hello bob', LItems[0].Message.Data);
    finally
      LBroker.UnregisterConnection('/ch', LConn1);
      LBroker.UnregisterConnection('/ch', LConn2);
      LConn1.Free;
      LConn2.Free;
    end;
  finally
    LBroker.Free;
  end;
end;

procedure TTestSSEBroker.TestSendToNonExistentClient;
var
  LBroker: TMVCSSEBroker;
  LConn: TSSEConnection;
  LItems: TArray<TSSEQueueItem>;
begin
  LBroker := TMVCSSEBroker.Create;
  try
    LConn := TSSEConnection.Create('alice');
    try
      LBroker.RegisterConnection('/ch', LConn);
      LBroker.SendTo('/ch', 'nonexistent', TSSEMessage.Create('dm', 'hello'));
      // Should not crash, alice should not receive anything
      LItems := LConn.DequeueAll;
      Assert.AreEqual<Integer>(0, Length(LItems));
    finally
      LBroker.UnregisterConnection('/ch', LConn);
      LConn.Free;
    end;
  finally
    LBroker.Free;
  end;
end;

procedure TTestSSEBroker.TestConnectionCount;
var
  LBroker: TMVCSSEBroker;
  LConn1, LConn2: TSSEConnection;
begin
  LBroker := TMVCSSEBroker.Create;
  try
    Assert.AreEqual(0, LBroker.ConnectionCount('/ch'));
    LConn1 := TSSEConnection.Create('c1');
    LConn2 := TSSEConnection.Create('c2');
    try
      LBroker.RegisterConnection('/ch', LConn1);
      Assert.AreEqual(1, LBroker.ConnectionCount('/ch'));
      LBroker.RegisterConnection('/ch', LConn2);
      Assert.AreEqual(2, LBroker.ConnectionCount('/ch'));
      LBroker.UnregisterConnection('/ch', LConn1);
      Assert.AreEqual(1, LBroker.ConnectionCount('/ch'));
    finally
      LBroker.UnregisterConnection('/ch', LConn2);
      LConn1.Free;
      LConn2.Free;
    end;
  finally
    LBroker.Free;
  end;
end;

procedure TTestSSEBroker.TestGetClientIds;
var
  LBroker: TMVCSSEBroker;
  LConn1, LConn2: TSSEConnection;
  LIds: TArray<string>;
begin
  LBroker := TMVCSSEBroker.Create;
  try
    LConn1 := TSSEConnection.Create('alice');
    LConn2 := TSSEConnection.Create('bob');
    try
      LBroker.RegisterConnection('/ch', LConn1);
      LBroker.RegisterConnection('/ch', LConn2);
      LIds := LBroker.GetClientIds('/ch');
      Assert.AreEqual<Integer>(2, Length(LIds));
    finally
      LBroker.UnregisterConnection('/ch', LConn1);
      LBroker.UnregisterConnection('/ch', LConn2);
      LConn1.Free;
      LConn2.Free;
    end;
  finally
    LBroker.Free;
  end;
end;

procedure TTestSSEBroker.TestGetChannelNames;
var
  LBroker: TMVCSSEBroker;
  LConn1, LConn2: TSSEConnection;
  LNames: TArray<string>;
begin
  LBroker := TMVCSSEBroker.Create;
  try
    LConn1 := TSSEConnection.Create('c1');
    LConn2 := TSSEConnection.Create('c2');
    try
      LBroker.RegisterConnection('/stocks', LConn1);
      LBroker.RegisterConnection('/news', LConn2);
      LNames := LBroker.GetChannelNames;
      Assert.AreEqual<Integer>(2, Length(LNames));
    finally
      LBroker.UnregisterConnection('/stocks', LConn1);
      LBroker.UnregisterConnection('/news', LConn2);
      LConn1.Free;
      LConn2.Free;
    end;
  finally
    LBroker.Free;
  end;
end;

procedure TTestSSEBroker.TestChannelRemovedWhenEmpty;
var
  LBroker: TMVCSSEBroker;
  LConn: TSSEConnection;
begin
  LBroker := TMVCSSEBroker.Create;
  try
    LConn := TSSEConnection.Create('c1');
    try
      LBroker.RegisterConnection('/ch', LConn);
      Assert.AreEqual<Integer>(1, Length(LBroker.GetChannelNames));
      LBroker.UnregisterConnection('/ch', LConn);
      Assert.AreEqual<Integer>(0, Length(LBroker.GetChannelNames));
    finally
      LConn.Free;
    end;
  finally
    LBroker.Free;
  end;
end;

procedure TTestSSEBroker.TestMultipleChannels;
var
  LBroker: TMVCSSEBroker;
  LConn1, LConn2: TSSEConnection;
  LItems: TArray<TSSEQueueItem>;
begin
  LBroker := TMVCSSEBroker.Create;
  try
    LConn1 := TSSEConnection.Create('c1');
    LConn2 := TSSEConnection.Create('c2');
    try
      LBroker.RegisterConnection('/stocks', LConn1);
      LBroker.RegisterConnection('/news', LConn2);

      LBroker.Broadcast('/stocks', TSSEMessage.Create('stock', 'IBM'));
      LBroker.Broadcast('/news', TSSEMessage.Create('news', 'Breaking'));

      LItems := LConn1.DequeueAll;
      Assert.AreEqual<Integer>(1, Length(LItems));
      Assert.AreEqual('IBM', LItems[0].Message.Data);

      LItems := LConn2.DequeueAll;
      Assert.AreEqual<Integer>(1, Length(LItems));
      Assert.AreEqual('Breaking', LItems[0].Message.Data);
    finally
      LBroker.UnregisterConnection('/stocks', LConn1);
      LBroker.UnregisterConnection('/news', LConn2);
      LConn1.Free;
      LConn2.Free;
    end;
  finally
    LBroker.Free;
  end;
end;

procedure TTestSSEBroker.TestBroadcastMultipleConnections;
var
  LBroker: TMVCSSEBroker;
  LConns: array[0..4] of TSSEConnection;
  LItems: TArray<TSSEQueueItem>;
  I: Integer;
begin
  LBroker := TMVCSSEBroker.Create;
  try
    for I := 0 to High(LConns) do
    begin
      LConns[I] := TSSEConnection.Create('c' + I.ToString);
      LBroker.RegisterConnection('/ch', LConns[I]);
    end;
    try
      LBroker.Broadcast('/ch', TSSEMessage.Create('evt', 'all'));

      for I := 0 to High(LConns) do
      begin
        LItems := LConns[I].DequeueAll;
        Assert.AreEqual<Integer>(1, Length(LItems), 'Connection ' + I.ToString + ' should receive 1 message');
        Assert.AreEqual('all', LItems[0].Message.Data);
      end;
    finally
      for I := 0 to High(LConns) do
      begin
        LBroker.UnregisterConnection('/ch', LConns[I]);
        LConns[I].Free;
      end;
    end;
  finally
    LBroker.Free;
  end;
end;

{ ========================================================================= }
{ TTestSSEClientParser                                                      }
{ ========================================================================= }

procedure TTestSSEClientParser.TestSimpleMessage;
var
  LParser: TSSEClientParser;
  LReceived: Boolean;
  LId, LEvent, LData: string;
begin
  LParser := TSSEClientParser.Create;
  try
    LReceived := False;
    LParser.OnEvent :=
      procedure(const AId, AEvent, AData: string)
      begin
        LReceived := True;
        LId := AId;
        LEvent := AEvent;
        LData := AData;
      end;

    LParser.Feed('data: hello world' + #10 + #10);

    Assert.IsTrue(LReceived);
    Assert.AreEqual('', LId);
    Assert.AreEqual('', LEvent);
    Assert.AreEqual('hello world', LData);
  finally
    LParser.Free;
  end;
end;

procedure TTestSSEClientParser.TestMessageWithAllFields;
var
  LParser: TSSEClientParser;
  LId, LEvent, LData: string;
begin
  LParser := TSSEClientParser.Create;
  try
    LParser.OnEvent :=
      procedure(const AId, AEvent, AData: string)
      begin
        LId := AId;
        LEvent := AEvent;
        LData := AData;
      end;

    LParser.Feed(
      'id: 42' + #10 +
      'event: stockupdate' + #10 +
      'data: {"stock":"IBM","price":550}' + #10 +
      #10);

    Assert.AreEqual('42', LId);
    Assert.AreEqual('stockupdate', LEvent);
    Assert.AreEqual('{"stock":"IBM","price":550}', LData);
    Assert.AreEqual('42', LParser.LastEventId);
  finally
    LParser.Free;
  end;
end;

procedure TTestSSEClientParser.TestMultipleMessages;
var
  LParser: TSSEClientParser;
  LCount: Integer;
begin
  LParser := TSSEClientParser.Create;
  try
    LCount := 0;
    LParser.OnEvent :=
      procedure(const AId, AEvent, AData: string)
      begin
        Inc(LCount);
      end;

    LParser.Feed(
      'data: msg1' + #10 + #10 +
      'data: msg2' + #10 + #10 +
      'data: msg3' + #10 + #10);

    Assert.AreEqual(3, LCount);
  finally
    LParser.Free;
  end;
end;

procedure TTestSSEClientParser.TestMultilineData;
var
  LParser: TSSEClientParser;
  LData: string;
begin
  LParser := TSSEClientParser.Create;
  try
    LParser.OnEvent :=
      procedure(const AId, AEvent, AData: string)
      begin
        LData := AData;
      end;

    LParser.Feed(
      'data: line1' + #10 +
      'data: line2' + #10 +
      'data: line3' + #10 +
      #10);

    Assert.AreEqual('line1' + #10 + 'line2' + #10 + 'line3', LData);
  finally
    LParser.Free;
  end;
end;

procedure TTestSSEClientParser.TestCommentLinesIgnored;
var
  LParser: TSSEClientParser;
  LReceived: Boolean;
  LData: string;
begin
  LParser := TSSEClientParser.Create;
  try
    LReceived := False;
    LParser.OnEvent :=
      procedure(const AId, AEvent, AData: string)
      begin
        LReceived := True;
        LData := AData;
      end;

    LParser.Feed(
      ': this is a comment' + #10 +
      'data: hello' + #10 +
      ': another comment' + #10 +
      #10);

    Assert.IsTrue(LReceived);
    Assert.AreEqual('hello', LData);
  finally
    LParser.Free;
  end;
end;

procedure TTestSSEClientParser.TestRetryField;
var
  LParser: TSSEClientParser;
begin
  LParser := TSSEClientParser.Create;
  try
    LParser.OnEvent :=
      procedure(const AId, AEvent, AData: string)
      begin
      end;

    LParser.Feed(
      'retry: 5000' + #10 +
      'data: test' + #10 +
      #10);

    Assert.AreEqual(5000, LParser.ReconnectTimeout);
  finally
    LParser.Free;
  end;
end;

procedure TTestSSEClientParser.TestLastEventIdUpdated;
var
  LParser: TSSEClientParser;
begin
  LParser := TSSEClientParser.Create;
  try
    LParser.OnEvent :=
      procedure(const AId, AEvent, AData: string)
      begin
      end;

    LParser.Feed(
      'id: first' + #10 +
      'data: a' + #10 + #10 +
      'id: second' + #10 +
      'data: b' + #10 + #10);

    Assert.AreEqual('second', LParser.LastEventId);
  finally
    LParser.Free;
  end;
end;

procedure TTestSSEClientParser.TestIdWithNullCharNotSaved;
var
  LParser: TSSEClientParser;
begin
  LParser := TSSEClientParser.Create;
  try
    LParser.OnEvent :=
      procedure(const AId, AEvent, AData: string)
      begin
      end;

    // Set a valid ID first
    LParser.Feed(
      'id: good-id' + #10 +
      'data: a' + #10 + #10);
    Assert.AreEqual('good-id', LParser.LastEventId);

    // Now send an ID with null char - should NOT update LastEventId
    LParser.Feed(
      'id: bad' + #0 + 'id' + #10 +
      'data: b' + #10 + #10);
    Assert.AreEqual('good-id', LParser.LastEventId);
  finally
    LParser.Free;
  end;
end;

procedure TTestSSEClientParser.TestDataWithColons;
var
  LParser: TSSEClientParser;
  LData: string;
begin
  LParser := TSSEClientParser.Create;
  try
    LParser.OnEvent :=
      procedure(const AId, AEvent, AData: string)
      begin
        LData := AData;
      end;

    // Data containing colons should not be truncated
    LParser.Feed(
      'data: http://example.com:8080/path' + #10 +
      #10);

    Assert.AreEqual('http://example.com:8080/path', LData);
  finally
    LParser.Free;
  end;
end;

procedure TTestSSEClientParser.TestEmptyDataField;
var
  LParser: TSSEClientParser;
  LReceived: Boolean;
  LData: string;
begin
  LParser := TSSEClientParser.Create;
  try
    LReceived := False;
    LParser.OnEvent :=
      procedure(const AId, AEvent, AData: string)
      begin
        LReceived := True;
        LData := AData;
      end;

    LParser.Feed(
      'data:' + #10 +
      #10);

    Assert.IsTrue(LReceived);
    Assert.AreEqual('', LData);
  finally
    LParser.Free;
  end;
end;

procedure TTestSSEClientParser.TestFieldWithNoColon;
var
  LParser: TSSEClientParser;
  LReceived: Boolean;
  LData: string;
begin
  LParser := TSSEClientParser.Create;
  try
    LReceived := False;
    LParser.OnEvent :=
      procedure(const AId, AEvent, AData: string)
      begin
        LReceived := True;
        LData := AData;
      end;

    // Per spec: a line with no colon uses the whole line as field name, empty value
    // "data" alone with no colon is treated as "data" field with empty value
    LParser.Feed(
      'data' + #10 +
      #10);

    Assert.IsTrue(LReceived);
    Assert.AreEqual('', LData);
  finally
    LParser.Free;
  end;
end;

procedure TTestSSEClientParser.TestUnknownFieldIgnored;
var
  LParser: TSSEClientParser;
  LData: string;
begin
  LParser := TSSEClientParser.Create;
  try
    LParser.OnEvent :=
      procedure(const AId, AEvent, AData: string)
      begin
        LData := AData;
      end;

    LParser.Feed(
      'unknown: value' + #10 +
      'data: test' + #10 +
      'custom: ignored' + #10 +
      #10);

    Assert.AreEqual('test', LData);
  finally
    LParser.Free;
  end;
end;

procedure TTestSSEClientParser.TestIncrementalFeeding;
var
  LParser: TSSEClientParser;
  LCount: Integer;
  LData: string;
begin
  LParser := TSSEClientParser.Create;
  try
    LCount := 0;
    LParser.OnEvent :=
      procedure(const AId, AEvent, AData: string)
      begin
        Inc(LCount);
        LData := AData;
      end;

    // Feed data in small chunks (simulating network arrival)
    LParser.Feed('da');
    Assert.AreEqual(0, LCount);
    LParser.Feed('ta: hel');
    Assert.AreEqual(0, LCount);
    LParser.Feed('lo' + #10);
    Assert.AreEqual(0, LCount);  // Not dispatched yet (no blank line)
    LParser.Feed(#10);           // Blank line triggers dispatch
    Assert.AreEqual(1, LCount);
    Assert.AreEqual('hello', LData);
  finally
    LParser.Free;
  end;
end;

procedure TTestSSEClientParser.TestCRLFLineEndings;
var
  LParser: TSSEClientParser;
  LData: string;
begin
  LParser := TSSEClientParser.Create;
  try
    LParser.OnEvent :=
      procedure(const AId, AEvent, AData: string)
      begin
        LData := AData;
      end;

    LParser.Feed(
      'data: test' + #13#10 +
      #13#10);

    Assert.AreEqual('test', LData);
  finally
    LParser.Free;
  end;
end;

procedure TTestSSEClientParser.TestCRLineEndings;
var
  LParser: TSSEClientParser;
  LData: string;
begin
  LParser := TSSEClientParser.Create;
  try
    LParser.OnEvent :=
      procedure(const AId, AEvent, AData: string)
      begin
        LData := AData;
      end;

    LParser.Feed(
      'data: test' + #13 +
      #13);

    Assert.AreEqual('test', LData);
  finally
    LParser.Free;
  end;
end;

procedure TTestSSEClientParser.TestLFLineEndings;
var
  LParser: TSSEClientParser;
  LData: string;
begin
  LParser := TSSEClientParser.Create;
  try
    LParser.OnEvent :=
      procedure(const AId, AEvent, AData: string)
      begin
        LData := AData;
      end;

    LParser.Feed(
      'data: test' + #10 +
      #10);

    Assert.AreEqual('test', LData);
  finally
    LParser.Free;
  end;
end;

procedure TTestSSEClientParser.TestEmptyEventDispatchesAsEmpty;
var
  LParser: TSSEClientParser;
  LReceived: Boolean;
begin
  LParser := TSSEClientParser.Create;
  try
    LReceived := False;
    LParser.OnEvent :=
      procedure(const AId, AEvent, AData: string)
      begin
        LReceived := True;
      end;

    // Empty line with no data fields should NOT dispatch
    LParser.Feed(#10);
    Assert.IsFalse(LReceived);

    // Empty line after only comments should NOT dispatch
    LParser.Feed(': comment' + #10 + #10);
    Assert.IsFalse(LReceived);
  finally
    LParser.Free;
  end;
end;

procedure TTestSSEClientParser.TestDefaultEventIsEmpty;
var
  LParser: TSSEClientParser;
  LEvent: string;
begin
  LParser := TSSEClientParser.Create;
  try
    LParser.OnEvent :=
      procedure(const AId, AEvent, AData: string)
      begin
        LEvent := AEvent;
      end;

    LParser.Feed('data: test' + #10 + #10);
    Assert.AreEqual('', LEvent);
  finally
    LParser.Free;
  end;
end;

procedure TTestSSEClientParser.TestEventResetBetweenMessages;
var
  LParser: TSSEClientParser;
  LEvents: TList<string>;
begin
  LParser := TSSEClientParser.Create;
  LEvents := TList<string>.Create;
  try
    LParser.OnEvent :=
      procedure(const AId, AEvent, AData: string)
      begin
        LEvents.Add(AEvent);
      end;

    // First message has event, second doesn't
    LParser.Feed(
      'event: custom' + #10 +
      'data: msg1' + #10 + #10 +
      'data: msg2' + #10 + #10);

    Assert.AreEqual<Integer>(2, LEvents.Count);
    Assert.AreEqual('custom', LEvents[0]);
    Assert.AreEqual('', LEvents[1]); // Event should reset between messages
  finally
    LEvents.Free;
    LParser.Free;
  end;
end;

procedure TTestSSEClientParser.TestNoSpaceAfterColon;
var
  LParser: TSSEClientParser;
  LData: string;
begin
  LParser := TSSEClientParser.Create;
  try
    LParser.OnEvent :=
      procedure(const AId, AEvent, AData: string)
      begin
        LData := AData;
      end;

    // No space after colon - value should be the full text after ':'
    LParser.Feed('data:nospace' + #10 + #10);
    Assert.AreEqual('nospace', LData);
  finally
    LParser.Free;
  end;
end;

procedure TTestSSEClientParser.TestMultipleSpacesAfterColon;
var
  LParser: TSSEClientParser;
  LData: string;
begin
  LParser := TSSEClientParser.Create;
  try
    LParser.OnEvent :=
      procedure(const AId, AEvent, AData: string)
      begin
        LData := AData;
      end;

    // Per spec, only ONE leading space is removed
    LParser.Feed('data:   three spaces' + #10 + #10);
    Assert.AreEqual('  three spaces', LData);
  finally
    LParser.Free;
  end;
end;

{ ========================================================================= }
{ TTestSSEIntegration                                                       }
{ ========================================================================= }

procedure TTestSSEIntegration.TestBrokerPushToConnection;
var
  LBroker: TMVCSSEBroker;
  LConn: TSSEConnection;
  LWait: TWaitResult;
  LItems: TArray<TSSEQueueItem>;
begin
  LBroker := TMVCSSEBroker.Create;
  try
    LConn := TSSEConnection.Create('c1');
    try
      LBroker.RegisterConnection('/ch', LConn);

      // Simulate what the controller loop does
      LWait := LConn.WaitForData(50);
      Assert.IsTrue(LWait = wrTimeout); // No data yet

      // Now push from "another thread" (simulated)
      LBroker.Broadcast('/ch', TSSEMessage.Create('evt', 'pushed'));

      LWait := LConn.WaitForData(1000);
      Assert.IsTrue(LWait = wrSignaled);

      LItems := LConn.DequeueAll;
      Assert.AreEqual<Integer>(1, Length(LItems));
      Assert.AreEqual('pushed', LItems[0].Message.Data);
    finally
      LBroker.UnregisterConnection('/ch', LConn);
      LConn.Free;
    end;
  finally
    LBroker.Free;
  end;
end;

procedure TTestSSEIntegration.TestMultipleClientsReceiveBroadcast;
var
  LBroker: TMVCSSEBroker;
  LConns: array[0..2] of TSSEConnection;
  I: Integer;
begin
  LBroker := TMVCSSEBroker.Create;
  try
    for I := 0 to High(LConns) do
    begin
      LConns[I] := TSSEConnection.Create('c' + I.ToString);
      LBroker.RegisterConnection('/ch', LConns[I]);
    end;
    try
      LBroker.Broadcast('/ch', TSSEMessage.Create('evt', 'all'));

      for I := 0 to High(LConns) do
      begin
        Assert.IsTrue(LConns[I].WaitForData(1000) = wrSignaled);
        var LItems := LConns[I].DequeueAll;
        Assert.AreEqual<Integer>(1, Length(LItems));
      end;
    finally
      for I := 0 to High(LConns) do
      begin
        LBroker.UnregisterConnection('/ch', LConns[I]);
        LConns[I].Free;
      end;
    end;
  finally
    LBroker.Free;
  end;
end;

procedure TTestSSEIntegration.TestGroupBroadcastFiltersCorrectly;
var
  LBroker: TMVCSSEBroker;
  LVip, LRegular: TSSEConnection;
begin
  LBroker := TMVCSSEBroker.Create;
  try
    LVip := TSSEConnection.Create('vip-user');
    LRegular := TSSEConnection.Create('regular-user');
    try
      LVip.JoinGroup('premium');
      LBroker.RegisterConnection('/ch', LVip);
      LBroker.RegisterConnection('/ch', LRegular);

      LBroker.BroadcastToGroup('/ch', 'premium', TSSEMessage.Create('deal', 'exclusive'));

      var LVipItems := LVip.DequeueAll;
      var LRegItems := LRegular.DequeueAll;

      Assert.AreEqual<Integer>(1, Length(LVipItems));
      Assert.AreEqual('exclusive', LVipItems[0].Message.Data);
      Assert.AreEqual<Integer>(0, Length(LRegItems));
    finally
      LBroker.UnregisterConnection('/ch', LVip);
      LBroker.UnregisterConnection('/ch', LRegular);
      LVip.Free;
      LRegular.Free;
    end;
  finally
    LBroker.Free;
  end;
end;

procedure TTestSSEIntegration.TestDisconnectedClientSkipped;
var
  LBroker: TMVCSSEBroker;
  LConn1, LConn2: TSSEConnection;
begin
  LBroker := TMVCSSEBroker.Create;
  try
    LConn1 := TSSEConnection.Create('alive');
    LConn2 := TSSEConnection.Create('dead');
    try
      LBroker.RegisterConnection('/ch', LConn1);
      LBroker.RegisterConnection('/ch', LConn2);

      LConn2.MarkDisconnected;

      LBroker.Broadcast('/ch', TSSEMessage.Create('evt', 'test'));

      var LItems1 := LConn1.DequeueAll;
      var LItems2 := LConn2.DequeueAll;

      Assert.AreEqual<Integer>(1, Length(LItems1));
      Assert.AreEqual<Integer>(0, Length(LItems2));
    finally
      LBroker.UnregisterConnection('/ch', LConn1);
      LBroker.UnregisterConnection('/ch', LConn2);
      LConn1.Free;
      LConn2.Free;
    end;
  finally
    LBroker.Free;
  end;
end;

procedure TTestSSEIntegration.TestConcurrentBroadcast;
var
  LBroker: TMVCSSEBroker;
  LConn: TSSEConnection;
  LThreads: array[0..4] of TThread;
  I: Integer;
  LDone: TCountdownEvent;
begin
  LBroker := TMVCSSEBroker.Create;
  try
    LConn := TSSEConnection.Create('c1');
    try
      LBroker.RegisterConnection('/ch', LConn);
      LDone := TCountdownEvent.Create(5);
      try
        // Launch 5 threads that each broadcast 100 messages
        for I := 0 to 4 do
        begin
          LThreads[I] := TThread.CreateAnonymousThread(
            procedure
            var
              J: Integer;
            begin
              for J := 1 to 100 do
                LBroker.Broadcast('/ch', TSSEMessage.Create('evt', 'data'));
              LDone.Signal;
            end);
          LThreads[I].FreeOnTerminate := True;
          LThreads[I].Start;
        end;

        LDone.WaitFor(10000);
        // Allow messages to settle
        Sleep(100);

        var LItems := LConn.DequeueAll;
        Assert.AreEqual<Integer>(500, Length(LItems), 'Should receive all 500 messages (5 threads x 100)');
      finally
        LDone.Free;
      end;
    finally
      LBroker.UnregisterConnection('/ch', LConn);
      LConn.Free;
    end;
  finally
    LBroker.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestSSEMessage);
  TDUnitX.RegisterTestFixture(TTestSSEConnection);
  TDUnitX.RegisterTestFixture(TTestSSEBroker);
  TDUnitX.RegisterTestFixture(TTestSSEClientParser);
  TDUnitX.RegisterTestFixture(TTestSSEIntegration);

end.
