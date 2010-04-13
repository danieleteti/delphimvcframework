unit MainU;

interface

procedure Main(serveraddress: string = 'localhost');
procedure MainWithTransaction(serveraddress: string = 'localhost');
procedure Test_Unicode_Chars(serveraddress: string = 'localhost');

implementation

uses
  SysUtils,
  dateutils,
  StompClient,
  StompTypes,
  Diagnostics;

procedure Test_Unicode_Chars(serveraddress: string);
var
  stomp: IStompClient;
  s: IStompFrame;
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
  assert(s.GetBody = ITALIANO);

  s := stomp.Receive;
  assert(s <> nil);
  assert(s.GetBody = SERBO);

  s := stomp.Receive;
  assert(s <> nil);
  assert(s.GetBody = SVEDESE);
end;

procedure MainWithTransaction(serveraddress: string);
var
  stomp, recv: IStompClient;
  frame: IStompFrame;
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

  // NON DEVCE TROVARE NULLA
  frame := recv.Receive;
  assert(frame = nil);
  stomp.CommitTransaction(TR);

  frame := recv.Receive;
  assert(frame <> nil);
  assert(frame.GetBody = BODY1);

  frame := recv.Receive;
  assert(frame <> nil);
  assert(frame.GetBody = BODY2);

  frame := recv.Receive;
  assert(frame <> nil);
  assert(frame.GetBody = BODY3);

  frame := recv.Receive;
  assert(frame <> nil);
  assert(frame.GetBody = BODY4);

  frame := recv.Receive;
  assert(frame = nil);
end;

procedure Main(serveraddress: string = 'localhost');
var
  stomp: IStompClient;
  frame: IStompFrame;
  i, c: Integer;
  msgcount: Cardinal;
  sw, sw1: TStopWatch;
  message_data: string;
const
  MSG = 5000;
  MSG_SIZE = 300;
begin
  sw1 := TStopWatch.StartNew;
  message_data := StringOfChar('X', MSG_SIZE);
  WriteLn('TEST MESSAGE IS (', length(message_data) * sizeof(char), ' bytes):', #13#10, '"',
    message_data, '"'#13#10#13#10);
  stomp := StompUtils.NewStomp(serveraddress, DEFAULT_STOMP_PORT, '', 'Daniele', 'Teti');
  stomp.Subscribe('/topic/foo.bar');

  for c := 1 to 6 do
  begin
    WriteLn;
    WriteLn('= STATS LOOP ', c, '=======================================');
    sw := TStopWatch.StartNew;
    for i := 1 to MSG do
      stomp.Send('/topic/foo.bar', message_data,
        StompUtils.NewHeaders.Add(TStompHeaders.NewPersistentHeader(true)));
    WriteLn('Queued ', MSG, ' messages in ', sw.ElapsedMilliseconds, ' ms');
    WriteLn('Now dequeuing...');

    msgcount := 0;
    sw := TStopWatch.StartNew;
    while msgcount < MSG do
    begin
      frame := stomp.Receive;
      if assigned(frame) then
      begin
        inc(msgcount);
        assert(frame.GetBody = message_data);
        frame := nil;
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
