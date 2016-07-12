program producer;

{$APPTYPE CONSOLE}

{$R *.res}

{
  https://www.rabbitmq.com/tutorials/tutorial-two-python.html
}

uses
  System.SysUtils,
  StompClient;

procedure Main;
var
  lClient: TStompClient;
  lMessage: String;
begin
  lClient := TStompClient.Create;
  lClient.Connect();
  WriteLn('Sending messages to queue "myjobqueue"');
  WriteLn('NOTE: Consumers will wait a second for each "." present in the message.');
  WriteLn('      empty message will terminate the program.');
  lMessage := '';
  repeat
    Write('Message to send: ');
    Readln(lMessage);
    if not lMessage.IsEmpty then
    begin
      lClient.Send('myjobqueue', lMessage);
    end;
  until lMessage.IsEmpty;
  WriteLn('bye bye');
  lClient.Disconnect;
end;

begin
  try
    Main;
    Readln;
  except
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message);
      Readln;
    end;
  end;

end.
