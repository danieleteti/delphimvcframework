program consumer;

{$APPTYPE CONSOLE}

{$R *.res}

{
  https://www.rabbitmq.com/tutorials/tutorial-two-python.html
}

uses
  System.SysUtils, StompClient, StompTypes;

procedure Main;
var
  lClient: TStompClient;
  lStompFrame: IStompFrame;
  lMessage: string;
begin
  lClient := TStompClient.Create;
  lClient.Connect();
  WriteLn('Subscribing to queue "myjobqueue"');
  lClient.Subscribe('/queue/myjobqueue',
    TAckMode.amClient,
    StompUtils.Headers
    .Add('auto-delete', 'true')
    );

  while true do
  begin
    WriteLn(sLineBreak + 'Waiting for messages... (KILL program to exit)' + sLineBreak +
      StringOfChar('*', 40));

    if lClient.Receive(lStompFrame, 5000) then
    begin
      lMessage := lStompFrame.GetBody;
      WriteLn(Format('Got message [%s]. Please wait, I''m working on it...', [lMessage]));
      Sleep(1000 * lMessage.CountChar('.'));
      WriteLn(lMessage);
      WriteLn('Informing the broker that the message ' + lStompFrame.MessageID +
        ' has been properly processed');
      lClient.Ack(lStompFrame.MessageID);
    end
    else
      WriteLn('Cannot read message after timeout...');
  end;
  lClient.Disconnect;
end;

begin
  try
    Main;
    write('Press return to quit');
    ReadLn;
  except
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message);
      ReadLn;
    end;
  end;

end.
