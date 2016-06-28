unit MainU;

interface

uses
  StompTypes;

procedure Main(serveraddress: string = 'localhost';
  STOMP_VERSION: TStompAcceptProtocol = STOMP_Version_1_0);
procedure MainWithTransaction(serveraddress: string = 'localhost');
procedure Test_Unicode_Chars(serveraddress: string = 'localhost');

implementation

uses
  SysUtils,
  dateutils,
  StompClient,
  Diagnostics;

procedure Test_Unicode_Chars(serveraddress: string);
var
  stomp: IStompClient;
  s: IStompFrame;
  utf8s: UTF8String;
  s1: string;
const
  SERBO = 'Što je Unicode';
  SVEDESE = 'Vad är Unicode';
  ITALIANO = 'Cos''è Unicode';
begin
  stomp := StompUtils.NewStomp(serveraddress);
  stomp.Subscribe('/topic/unicode');

  stomp.Send('/topic/unicode', ITALIANO);
  stomp.Send('/topic/unicode', SERBO);
  stomp.Send('/topic/unicode', SVEDESE);

  s := stomp.Receive;
  assert(s <> nil);
  s1 := s.GetBody;
  assert(s.GetBody = ITALIANO);

  s := stomp.Receive;
  assert(s <> nil);
  s1 := s.GetBody;
  assert(s.GetBody = SERBO);

  s := stomp.Receive;
  assert(s <> nil);
  s1 := s.GetBody;
  assert(s.GetBody = SVEDESE);
end;

procedure MainWithTransaction(serveraddress: string);
var
  stomp, recv: IStompClient;
  frame: IStompFrame;
  s1: string;
const
  TR = 'TRDANIELE';
  TOPIC = '/topic/mytopic'; // TOPIC = PUB/SUB, QUEUE = LOAD BALANCER
  BODY1 = 'Hello World 1';
  BODY2 = 'Hello World 2';
  BODY3 = 'Hello World 3';
  BODY4 = 'Hello World 4';
begin
  stomp := StompUtils.NewStomp;
  recv := StompUtils.NewStomp;

  stomp.Subscribe(TOPIC);
  recv.Subscribe(TOPIC);

  stomp.BeginTransaction(TR);
  stomp.Send(TOPIC, BODY1, TR);
  stomp.Send(TOPIC, BODY2, TR);
  stomp.Send(TOPIC, BODY3, TR);
  stomp.Send(TOPIC, BODY4, TR);

  // NON DEVE TROVARE NULLA
  frame := recv.Receive;
  assert(frame = nil);
  stomp.CommitTransaction(TR);

  frame := recv.Receive;
  assert(frame <> nil);
  s1 := frame.GetBody;
  // assert(frame.GetBody = BODY1, frame.GetBody);

  frame := recv.Receive;
  assert(frame <> nil);
  s1 := frame.GetBody;
  // assert(frame.GetBody = BODY2, frame.GetBody);

  frame := recv.Receive;
  assert(frame <> nil);
  s1 := frame.GetBody;
  // assert(frame.GetBody = BODY3);

  frame := recv.Receive;
  assert(frame <> nil);
  s1 := frame.GetBody;
  // assert(frame.GetBody = BODY4);

  frame := recv.Receive;
  assert(frame = nil);
end;

procedure Main(serveraddress: string = 'localhost';
  STOMP_VERSION: TStompAcceptProtocol = STOMP_Version_1_0);
var
  stomp: IStompClient;
  frame: IStompFrame;
  i, c: Integer;
  msgcount: Cardinal;
  sw, sw1: TStopWatch;
  message_data: string;
  s1: string;
const
  MSG = 1000;
  MSG_SIZE = 1024; // div 10;
begin
  sw1 := TStopWatch.StartNew;
  message_data := StringOfChar('X', MSG_SIZE);
  WriteLn('TEST MESSAGE IS (', length(message_data), ' bytes - WILL BE UTF8 Encoded):', #13#10, '"',
    message_data, '"'#13#10#13#10);
  stomp := StompUtils.NewStomp(serveraddress, DEFAULT_STOMP_PORT, '', 'guest', 'guest',
    STOMP_VERSION);
  WriteLn('SERVER: ', stomp.GetServer, ' PROTOCOL VERSION: ' + stomp.GetProtocolVersion);

  if STOMP_VERSION = STOMP_Version_1_1 then
    // Include the required ID header for STOMP 1.1
    stomp.Subscribe('/topic/foo.bar', amAuto, StompUtils.NewHeaders.Add('id', '1234'))
  else
    // Do not include ID header because is not required for STOMP 1.0
    stomp.Subscribe('/topic/foo.bar', amAuto);

  for c := 1 to 6 do
  begin
    WriteLn;
    WriteLn('= STATS LOOP ', c, '=======================================');
    sw := TStopWatch.StartNew;
    for i := 1 to MSG do
    begin
      stomp.Send('/topic/foo.bar', message_data,
        StompUtils.NewHeaders.Add(TStompHeaders.NewPersistentHeader(true)));
      // if i mod 100 = 0 then
      WriteLn('Queued ', i, ' messages in ', sw.ElapsedMilliseconds, ' ms');
    end;
    WriteLn('Finished Queueing... Currently Queued ', MSG, ' messages in ',
      sw.ElapsedMilliseconds, ' ms');
    WriteLn('Now dequeuing...');

    msgcount := 0;
    sw := TStopWatch.StartNew;
    while msgcount < MSG do
    begin
      frame := stomp.Receive;
      if assigned(frame) then
      begin
        inc(msgcount);
        s1 := frame.GetBody;
        assert(frame.GetBody = message_data);
        frame := nil;
        if msgcount mod 100 = 0 then
          WriteLn('DeQueued ', msgcount, ' messages in ', sw.ElapsedMilliseconds, ' ms');
      end
    end;
    sw.Stop;
    WriteLn('Dequeued ', msgcount, ' stomp messages in ', sw.ElapsedMilliseconds, ' ms');
    WriteLn('Throughput: ',
      FormatFloat('###,##0.000', sw.ElapsedMilliseconds / msgcount), ' ms/msg (',
      FormatFloat('###,##0.000', msgcount / sw.ElapsedMilliseconds), ' msg/ms)');
    // WriteLn('= END LOOP ', c, '========================================='#13#10);
  end;
  stomp.Unsubscribe('/topic/foo.bar');
  stomp.Disconnect;
  sw.Stop;
  WriteLn('SPEED TEST FINISHED IN ', FormatFloat('###,##0.000', sw1.ElapsedMilliseconds / 1000),
    ' seconds');
end;

end.
