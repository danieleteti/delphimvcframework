unit TestStompClientU;

interface

uses
  DUnitX.TestFramework, StompClient, StompTypes, System.Diagnostics, System.SysUtils;

type

  [TestFixture]
  TTestSTOMP = class(TObject)
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    // Sample Methods
    // Simple single Test
    [Test]
    procedure TestPubSub10;
    // Test with TestCase Attribute to supply parameters.
    // [Test]
    // [TestCase('TestA', '1,2')]
    // [TestCase('TestB', '3,4')]
    // procedure Test2(const AValue1: Integer; const AValue2: Integer);
  end;

implementation

procedure TTestSTOMP.Setup;
begin
//  FSTOMP := TStompClient.CreateAndConnect('127.0.0.1', 61613, '',
//    TStompAcceptProtocol.Ver_1_0);
end;

procedure TTestSTOMP.TearDown;
begin
//  FSTOMP := nil;
end;

procedure TTestSTOMP.TestPubSub10;
var
  lFrame: IStompFrame;
  lSW: TStopWatch;
  lSTOMP: IStompClient;
  I: Integer;
begin
  lSTOMP := TStompClient.Create;
  lSTOMP.SetHeartBeat(1000, 0);
  lSTOMP.Connect('127.0.0.1', 61613, '', TStompAcceptProtocol.Ver_1_1);
  lSTOMP.Subscribe('/topic/mytopic');
  Sleep(2000);
  lSTOMP.Send('/topic/mytopic', 'Hello World1');
  lSTOMP.Send('/topic/mytopic', 'Hello World2');
  lSTOMP.Send('/topic/mytopic', 'Hello World3');
  lSTOMP.Send('/topic/mytopic', 'Hello World4');
  lSTOMP.Send('/topic/mytopic', 'Hello World5');

  lSW := TStopWatch.Create;
  for I := 1 to 5 do
  begin
    lFrame := lSTOMP.Receive(5000);
    Assert.IsTrue(lSW.ElapsedMilliseconds <= 50);
    Assert.IsNotNull(lFrame, 'Message not received by the sender');
    Assert.AreEqual('Hello World' + I.ToString, lFrame.Body);
  end;
  lFrame := lSTOMP.Receive(50);
end;

initialization

TDUnitX.RegisterTestFixture(TTestSTOMP);

end.
