program producer;

{$APPTYPE CONSOLE}

{$R *.res}

{
  https://www.rabbitmq.com/tutorials/tutorial-one-python.html
}

uses
  System.SysUtils,
  StompClient;

procedure Main;
var
  lClient: TStompClient;
begin
  lClient := TStompClient.Create;
  lClient.Connect();
  WriteLn('Sending messages to queue "myqueue"');
  lClient.Send('myqueue', 'Message 1');
  lClient.Send('myqueue', 'Message 2');
  WriteLn('Messages sent');
  lClient.Disconnect;
end;

begin
  try
    Main;
    Write('Press return to quit');
    ReadLn;
  except
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message);
      ReadLn;
    end;
  end;

end.
