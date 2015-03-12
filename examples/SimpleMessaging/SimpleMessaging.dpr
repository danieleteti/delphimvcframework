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
  StompHeaders.Add(TStompHeaders.NewDurableSubscriptionHeader('my-unique-id'));

  WriteLn('==> Example_Durable_Subscription');
  StompSubscriber := StompUtils.NewStomp('127.0.0.1', 61613, 'client-id', 'guest', 'guest');
  // default port
  StompSubscriber.Subscribe('/queue/durable01', amAuto, StompHeaders);
  // StompSubscriber.Disconnect;
  StompSubscriber := nil;

  StompPub := StompUtils.NewStomp('127.0.0.1', 61613, '', 'guest', 'guest'); // default port
  StompPub.Send('/queue/durable01',
    'this message has been sent when the subscriber client was disconnected');
  // StompPub.Disconnect;
  StompPub := nil;

  StompSubscriber := StompUtils.NewStomp('127.0.0.1', 61613, 'client-id', 'guest', 'guest');
  StompSubscriber.Subscribe('/queue/durable01', amAuto, StompHeaders);
  // default port
  repeat
    StompFrame := StompSubscriber.Receive(1000);
    if not Assigned(StompFrame) then
      WriteLn('No Message');
  until Assigned(StompFrame);
  WriteLn(StompFrame.GetBody); // Print "Some test message"
  WriteLn;
end;

procedure Example_Pub_Subscriber;
var
  StompPub, StompSubscriber: IStompClient;
  StompFrame: IStompFrame;
begin
  WriteLn('==> Example_Pub_Subscriber');
  StompSubscriber := StompUtils.NewStomp('127.0.0.1', 61613, '', 'guest', 'guest');
  // default port
  StompSubscriber.Subscribe('/topic/dummy');
  StompPub := StompUtils.NewStomp('127.0.0.1', 61613, '', 'guest', 'guest'); // default port
  StompPub.Send('/topic/dummy', 'Some test message');
  repeat
    StompFrame := StompSubscriber.Receive;
  until Assigned(StompFrame);
  WriteLn(StompFrame.GetBody); // Print "Some test message"
  WriteLn;
end;

procedure Example_OnePub_TwoSubscriber;
var
  StompPub, StompSub1, StompSub2: IStompClient;
  StompFrame: IStompFrame;
begin
  WriteLn('==> Example_OnePub_TwoSubscriber');
  StompSub1 := StompUtils.NewStomp('127.0.0.1', 61613, '', 'guest', 'guest'); // default port
  StompSub2 := StompUtils.NewStomp('127.0.0.1', 61613, '', 'guest', 'guest'); // default port
  StompSub1.Subscribe('/topic/dummy');
  StompSub2.Subscribe('/topic/dummy');

  //
  StompPub := StompUtils.NewStomp('127.0.0.1', 61613, '', 'guest', 'guest'); // default port
  StompPub.Send('/topic/dummy', 'First test message on a topic');
  StompPub.Send('/topic/dummy', 'Second test message on a topic');

  StompFrame := StompSub1.Receive(2000);
  if Assigned(StompFrame) then
    WriteLn(StompFrame.GetBody);
  StompFrame := StompSub1.Receive(2000);
  if Assigned(StompFrame) then
    WriteLn(StompFrame.GetBody);

  StompFrame := StompSub2.Receive(2000);
  if Assigned(StompFrame) then
    WriteLn(StompFrame.GetBody);
  StompFrame := StompSub2.Receive(2000);
  if Assigned(StompFrame) then
    WriteLn(StompFrame.GetBody);
  WriteLn;
end;

procedure Example_PointToPoint;
var
  StompPub, StompSub1, StompSub2: IStompClient;
  StompFrame: IStompFrame;
begin
  WriteLn('==> Example_PointToPoint');
  StompSub1 := StompUtils.NewStomp('127.0.0.1', 61613, '', 'guest', 'guest'); // default port
  StompSub2 := StompUtils.NewStomp('127.0.0.1', 61613, '', 'guest', 'guest'); // default port
  StompSub1.Subscribe('/queue/dummy');
  StompSub2.Subscribe('/queue/dummy');

  //
  StompPub := StompUtils.NewStomp('127.0.0.1', 61613, '', 'guest', 'guest'); // default port
  StompPub.Send('/queue/dummy', 'First test message on a queue');
  StompPub.Send('/queue/dummy', 'Second test message on a queue');

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

begin
  try
    Example_Pub_Subscriber;
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
