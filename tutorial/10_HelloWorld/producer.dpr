program producer;

{$APPTYPE CONSOLE}

{$R *.res}

{
  https://www.rabbitmq.com/tutorials/tutorial-one-python.html
}

uses
  System.SysUtils,
  StompClient, StompTypes;

procedure Main;
var
  lClient: TStompClient;
  lFrame: IStompFrame;
begin
  lClient := TStompClient.Create;
  lClient.Connect();
  WriteLn('Sending messages to queue "myqueue"');
  lClient.Send('/queue/myqueue', 'Message 1');
//  lFrame := lClient.Receive(100);
//  if Assigned(lFrame) then
//    WriteLn(lFrame.Output);
  lClient.Send('/queue/myqueue', 'Message 2');
  WriteLn('Messages sent');
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
