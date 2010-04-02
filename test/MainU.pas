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

function NewStomp(serveraddress: string = 'localhost'): TStompClient;
begin
  Result := TStompClient.Create;
  Result.SetUserName('guest');
  Result.SetPassword('guest');
  Result.Connect(serveraddress);
end;

procedure Test_Unicode_Chars(serveraddress: string);
var
  stomp: TStompClient;
  s: IStompFrame;
const
  SERBO = 'Što je Unicode';
  SVEDESE = 'Vad är Unicode';
  ITALIANO = 'Cos''è Unicode';
begin
  stomp := NewStomp(serveraddress);
  try
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
  finally
    stomp.Free;
  end;
end;

procedure MainWithTransaction(serveraddress: string);
var
  stomp, recv: TStompClient;
  frame: IStompFrame;
  m: Integer;
const
  TR = 'TRDANIELE';
  TOPIC = '/topic/mytopic'; // TOPIC = PUB/SUB, QUEUE = LOAD BALANCER
  BODY1 = 'Hello World 1';
  BODY2 = 'Hello World 2';
  BODY3 = 'Hello World 3';
  BODY4 = 'Hello World 4';
begin
  stomp := NewStomp;
  try
    recv := NewStomp;
    try
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
    finally
      recv.Free;
    end;
  finally
    stomp.Free;
  end;
end;

procedure Main(serveraddress: string = 'localhost');
var
  stomp: TStompClient;
  frame: IStompFrame;
  i, c: Integer;
  msgcount: Cardinal;
  sw: TStopWatch;
  message_data: string;
const
  MSG = 1000;
  MSG_SIZE = 1000;
begin
  message_data := StringOfChar('X', MSG_SIZE);
  WriteLn('TEST MESSAGE (', length(message_data) * sizeof(char), ' bytes):', #13#10, '"', message_data,
    '"'#13#10#13#10);
  stomp := TStompClient.Create;
  try
    stomp.SetUserName('Daniele');
    stomp.SetPassword('Paperino');
    stomp.Connect(serveraddress);
    stomp.Subscribe('/topic/foo.bar');

    for c := 1 to 10 do
    begin
      WriteLn;
      WriteLn('= STATS LOOP ', c, '=======================================');
      for i := 1 to MSG do
      begin
        stomp.Send('/topic/foo.bar', message_data, StompUtils.NewHeaders.Add(TStompHeaders.NewPersistentHeader(true)));
        // '01234567890123456789012345678901234567890123456789'
        if i mod 1000 = 0 then
          WriteLn('Queued ', i, ' messages');
      end;

      msgcount := 0;
      sw.start;
      while msgcount < MSG do
      begin
        frame := stomp.Receive;
        if assigned(frame) then
        begin
          inc(msgcount);
          frame := Nil;
        end
      end;
      sw.Stop;
      WriteLn(msgcount, ' in ', sw.ElapsedMilliseconds, ' milliseconds and ', sw.ElapsedTicks, ' ticks');
      WriteLn('Throughput: ');
      WriteLn(FormatFloat('###,##0.000', sw.ElapsedMilliseconds / msgcount), ' ms/msg');
      WriteLn(FormatFloat('###,##0.000', msgcount / sw.ElapsedMilliseconds), ' msg/ms');
      WriteLn('= END LOOP ', c, '========================================='#13#10);
    end;

    stomp.Unsubscribe('/topic/foo.bar');
    stomp.Disconnect;
    write('test finished...');
  finally
    stomp.Free;
  end;
end;

end.
