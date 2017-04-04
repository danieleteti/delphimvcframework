unit MainU;

{$MODE Delphi}

interface

procedure Main(serveraddress: string = 'localhost');
procedure MainWithTransaction(serveraddress: string = 'localhost');
procedure Test_Unicode_Chars(serveraddress: string = 'localhost');

implementation

uses
  SysUtils,
  dateutils,
  StompClient,
  StompTypes;

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
  headers: IStompHeaders;
  i, c: Integer;
  msgcount: Cardinal;
  totaltime, partialtime: TDateTime;
  partial: Double;
  message_data: string;
const
  MSG = 5000;
  MSG_SIZE = 300;
begin
  totaltime:=now;
  message_data := StringOfChar('X', MSG_SIZE);
  WriteLn('TEST MESSAGE IS (', length(message_data) * sizeof(char), ' bytes):', #13#10, '"',
    message_data, '"'#13#10#13#10);
  stomp := StompUtils.NewStomp(serveraddress, DEFAULT_STOMP_PORT, '', 'Daniele', 'Teti');
  stomp.Subscribe('/topic/foo.bar');

  for c := 1 to 6 do
  begin
    WriteLn;
    WriteLn('= STATS LOOP ', c, '=======================================');
    partialtime:=now;
    for i := 1 to MSG do
    begin
         headers:=StompUtils.NewHeaders;
         headers.Add(TStompHeaders.NewPersistentHeader(true));
         stomp.Send('/topic/foo.bar', message_data, headers);
    end;
    WriteLn('Queued ', MSG, ' messages in ', FormatFloat('##0,000.00',MilliSecondSpan(partialtime, now)), ' ms');
    WriteLn('Now dequeuing...');

    msgcount := 0;
    partialtime:=now;
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
    partial:=MilliSecondSpan(partialtime, now);
    WriteLn('Dequeued ', msgcount, ' stomp messages in ', FormatFloat('##0,000.00',partial), ' ms');
    WriteLn('Throughput: ',
      FormatFloat('###,##0.000', partial / msgcount), ' ms/msg (',
      FormatFloat('###,##0.000', msgcount / partial), ' msg/ms)');
    // WriteLn('= END LOOP ', c, '========================================='#13#10);
  end;
  stomp.Unsubscribe('/topic/foo.bar');
  stomp.Disconnect;
  WriteLn('SPEED TEST FINISHED IN ', FormatFloat('###,##0.000', MilliSecondSpan(totaltime, now) / 1000),
    ' seconds');
end;

end.
