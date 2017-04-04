program SimpleMessaging;

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ELSE}
{$APPTYPE CONSOLE}
{$ENDIF}


uses
{$IFDEF FPC}
{$IFDEF UNIX}
  cthreads,

{$ENDIF}
{$ENDIF}
  SysUtils,
  StompClient,
  StompTypes;

procedure Example_Durable_Subscription;
var
  StompPub, StompSubscriber: IStompClient;
  StompFrame: IStompFrame;
  StompHeaders: IStompHeaders;
begin
  StompHeaders := TStompHeaders.Create;
  StompHeaders.Add(TStompHeaders.Subscription('my-unique-id'));

  WriteLn('==> Example_Durable_Subscription');

  write('> Register a subscriber to "/queue/durable01" using a client-id...');
  StompSubscriber := TStompClient.CreateAndConnect('127.0.0.1', 61613, 'client-id');
  StompSubscriber.Subscribe('/queue/durable01', amAuto, StompHeaders);
  StompSubscriber := nil;
  WriteLn('Now, disconnect from the broker.' + sLineBreak);

  write('> Sending a message to "/queue/durable01"');
  StompPub := TStompClient.CreateAndConnect;
  StompPub.Send('/queue/durable01',
    'this message has been sent when the subscriber client was disconnected');
  StompPub := nil;
  WriteLn('... and disconnect' + sLineBreak);

  WriteLn('> The previoous subscriber reconnects using the same client-id');
  StompSubscriber := TStompClient.CreateAndConnect('127.0.0.1', 61613, 'client-id');
  StompSubscriber.Subscribe('/queue/durable01', amAuto, StompHeaders);
  // default port
  repeat
    StompFrame := StompSubscriber.Receive(1000);
    if not Assigned(StompFrame) then
      WriteLn('No Message');
  until Assigned(StompFrame);
  WriteLn('> Found the message (even if it was disconnected when the message has been actually published.'
    + sLineBreak);
  WriteLn(StompFrame.Body); // Print "Some test message"
  WriteLn;
end;

procedure Example_Pub_Subscriber;
var
  StompPub, StompSubscriber: IStompClient;
  StompFrame: IStompFrame;
begin
  WriteLn('==> Example_Pub_Subscriber');
  StompSubscriber := TStompClient.CreateAndConnect;
  // StompSubscriber.Subscribe('/topic/dummy', amAuto, StompUtils.Headers.Add('auto-delete', 'true'));
  StompSubscriber.Subscribe('/topic/dummy');
  StompPub := TStompClient.CreateAndConnect;
  StompPub.Send('/topic/dummy', 'Some test message');
  repeat
    StompFrame := StompSubscriber.Receive(500);
  until Assigned(StompFrame);
  WriteLn(StompFrame.Body); // Print "Some test message"
  WriteLn;
  StompSubscriber.Unsubscribe('/topic/dummy');
end;

procedure Example_OnePub_TwoSubscriber;
var
  StompPub, StompSub1, StompSub2: IStompClient;
  StompFrame: IStompFrame;
begin
  WriteLn('==> Example_OnePub_TwoSubscriber');
  // first subscriber
  StompSub1 := TStompClient.CreateAndConnect;
  StompSub1.Subscribe('/topic/dummy');
  while Assigned(StompSub1.Receive(100)) do; // empty the queue

  // second subscriber
  StompSub2 := TStompClient.CreateAndConnect;
  StompSub2.Subscribe('/topic/dummy');
  while Assigned(StompSub2.Receive(100)) do; // empty the queue

  // publish the messages
  StompPub := TStompClient.CreateAndConnect;
  write('> Publishing 2 message on "/topic/dummy"...');
  StompPub.Send('/topic/dummy', 'First test message on a topic');
  StompPub.Send('/topic/dummy', 'Second test message on a topic');
  WriteLn('DONE!');

  // read messages from the subscriber1
  WriteLn('> Reading from SUB1');
  StompFrame := StompSub1.Receive(2000);
  if not Assigned(StompFrame) then
    raise Exception.Create('Cannot read message');
  WriteLn(StompFrame.Body);
  StompFrame := StompSub1.Receive(2000);
  if not Assigned(StompFrame) then
    raise Exception.Create('Cannot read message');
  WriteLn(StompFrame.Body);

  // read messages from the subscriber2
  WriteLn('> Reading from SUB2');
  StompFrame := StompSub2.Receive(2000);
  if not Assigned(StompFrame) then
    raise Exception.Create('Cannot read message');
  WriteLn(StompFrame.Body);
  StompFrame := StompSub2.Receive(2000);
  if not Assigned(StompFrame) then
    raise Exception.Create('Cannot read message');
  WriteLn(StompFrame.Body);
  WriteLn;
end;

procedure Example_PointToPoint;
var
  StompPub, StompSub1, StompSub2: IStompClient;
  StompFrame: IStompFrame;
begin
  WriteLn('==> Example_PointToPoint');
  StompSub1 := TStompClient.CreateAndConnect; // default port
  StompSub2 := TStompClient.CreateAndConnect; // default port
  StompSub1.Subscribe('/queue/PointToPoint');
  StompSub2.Subscribe('/queue/PointToPoint');

  //
  StompPub := TStompClient.CreateAndConnect; // default port
  StompPub.Send('/queue/PointToPoint', 'First test message on a queue');
  StompPub.Send('/queue/PointToPoint', 'Second test message on a queue');

  StompFrame := StompSub1.Receive(200);
  if Assigned(StompFrame) then
    WriteLn(StompFrame.Output);
  StompFrame := StompSub1.Receive(200);
  if Assigned(StompFrame) then
    WriteLn(StompFrame.Output);

  StompFrame := StompSub2.Receive(200);
  if Assigned(StompFrame) then
    WriteLn(StompFrame.Output);
  StompFrame := StompSub2.Receive(200);
  if Assigned(StompFrame) then
    WriteLn(StompFrame.Output);

  WriteLn;
end;

procedure Example_Simple_Queue;
var
  lProducer, lConsumer: IStompClient;
  StompFrame: IStompFrame;
begin
  WriteLn('==> Example_Simple_Queue');
  lConsumer := TStompClient.CreateAndConnect;

  { TODO -oDaniele -cGeneral : Checkthis }
  // create an auto-delete queue
  lConsumer.Subscribe('/queue/dummy', amClient,
    StompUtils.Headers.Add(TStompHeaders.AUTO_DELETE, 'true'));

  // creates a durable queue
  // lConsumer.Subscribe('/queue/dummy');

  lProducer := TStompClient.CreateAndConnect;
  lProducer.Send('/queue/dummy', 'Some test message',
    StompUtils.Headers.Add(TStompHeaders.AUTO_DELETE, 'true'));
  repeat
    StompFrame := lConsumer.Receive(500);
  until Assigned(StompFrame);
  WriteLn(StompFrame.Body); // Print "Some test message"
  lConsumer.Ack(StompFrame.MessageID);
  WriteLn;
  lConsumer.Unsubscribe('/queue/dummy123');
end;

begin
  try
    Example_Pub_Subscriber;
    Example_Simple_Queue;
    Example_OnePub_TwoSubscriber;
    Example_PointToPoint;
    Example_Durable_Subscription;
    WriteLn('>> TEST FINISHED <<');
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.message);
  end;
  readln

end.
