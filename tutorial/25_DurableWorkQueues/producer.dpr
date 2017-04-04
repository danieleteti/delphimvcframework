program producer;

{$APPTYPE CONSOLE}

{$R *.res}

{
  https://www.rabbitmq.com/tutorials/tutorial-two-python.html
  with durability
}

uses
  System.SysUtils,
  StompClient, StompTypes;

procedure Main;
var
  lClient: TStompClient;
  lMessage: String;
  lIsEmpty: Boolean;
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
    lIsEmpty := lMessage.Trim.IsEmpty;
    if not lIsEmpty then
    begin
      lClient.Send('/queue/myjobqueue', lMessage.Trim,
        StompUtils
          .Headers
          .Add('persistent', 'true')
        );
    end;
  until lIsEmpty;
  WriteLn('bye bye...');
  lClient.Disconnect;
end;

begin
  try
    Main;
    Write('Press return to exit');
    Readln;
  except
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message);
      Readln;
    end;
  end;

end.
