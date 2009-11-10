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
  StopWatch;

function NewStomp(serveraddress: string = 'localhost'): TStompClient;
begin
  Result := TStompClient.Create;
  Result.UserName := 'guest';
  Result.Password := 'guest';
  Result.Connect(serveraddress);
end;


procedure Test_Unicode_Chars(serveraddress: string);
var
  stomp: TStompClient;
  s: TStompFrame;
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
    assert(s<>nil);
    assert(s.Body = ITALIANO);

    s := stomp.Receive;
    assert(s<>nil);
    assert(s.Body = SERBO);

    s := stomp.Receive;
    assert(s<>nil);
    assert(s.Body = SVEDESE);
  finally
    stomp.Free;
  end;
end;

procedure MainWithTransaction(serveraddress: string);
var
  stomp, recv: TStompClient;
  frame: TStompFrame;
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
      stomp.send(TOPIC, BODY1, TR);
      stomp.send(TOPIC, BODY2, TR);
      stomp.send(TOPIC, BODY3, TR);
      stomp.send(TOPIC, BODY4, TR);

      // NON DEVCE TROVARE NULLA
      frame := recv.Receive;
      assert(frame = nil);
      stomp.CommitTransaction(TR);

      // ORA DEVE LEGGERE I MESSAGGI
      // m := 0;
      // while true do
      // begin
      // frame := recv.Receive;
      // if frame <> nil then
      // begin
      // inc(m);
      // writeln(frame.output);
      // if m = 4 then
      // break;
      // end;
      // end;

      frame := recv.Receive;
      assert(frame <> nil);
      assert(frame.Body = BODY1);

      frame := recv.Receive;
      assert(frame <> nil);
      assert(frame.Body = BODY2);

      frame := recv.Receive;
      assert(frame <> nil);
      assert(frame.Body = BODY3);

      frame := recv.Receive;
      assert(frame <> nil);
      assert(frame.Body = BODY4);

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
  frame: TStompFrame;
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
  sw := TStopWatch.Create;
  try
    stomp := TStompClient.Create;
    try
      stomp.EnableReceipts := false;
      stomp.UserName := 'Daniele';
      stomp.Password := 'Paperino';
      stomp.Connect(serveraddress);
      stomp.Subscribe('/topic/foo.bar');

      for c := 1 to 10 do
      begin
        WriteLn;
        WriteLn('= STATS LOOP ', c, '=======================================');
        for i := 1 to MSG do
        begin
          stomp.send('/topic/foo.bar', message_data, StompUtils.StompHeaders.Add(shPersistent)
            // '01234567890123456789012345678901234567890123456789'
            );
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
            frame.Free;
          end
        end;
        sw.Stop;
        WriteLn(msgcount, ' in ', sw.ElapsedMiliseconds, ' milliseconds and ', sw.ElapsedTicks, ' ticks');
        WriteLn('Throughput: ');
        WriteLn(FormatFloat('###,##0.000', sw.ElapsedMiliseconds / msgcount), ' ms/msg');
        WriteLn(FormatFloat('###,##0.000', msgcount / sw.ElapsedMiliseconds), ' msg/ms');
        WriteLn('= END LOOP ', c, '========================================='#13#10);
      end;

      stomp.Unsubscribe('/topic/foo.bar');
      stomp.Disconnect;
      write('test finished...');
    finally
      stomp.Free;
    end;
  finally
    sw.Free;
  end;
end;

end.
