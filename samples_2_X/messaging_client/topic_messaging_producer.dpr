program topic_messaging_producer;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils, MVCFramework.RESTClient, System.JSON, MVCFramework.Commons,
  Winapi.Windows;

procedure Main;
var
  LCli: TRESTClient;
  LRes: IRESTResponse;
  LMsg: TJSONObject;
  LStrMsg, LMyClientID: string;
begin
  {
    USAGE PATTERN
    POST /messages/clients/<myuniqueid>
    POST /messages/queues/<queuename1> (message in the request body as json object)
  }

  LMyClientID := 'my-unique-client-id';
  LCli := TRESTClient.Create('localhost', 9999);
  try
    LCli.ReadTimeout(-1);
    LRes := LCli.doPOST('/messages', ['clients', LMyClientID]);
    LStrMsg := 'FIRST MESSAGE';
    while True do
    begin
      LMsg := TJSONObject.Create(TJSONPair.Create('msg', LStrMsg)); // ;;GetTickCount.ToString));
      WriteLn('Writing on the topic: ' + LMsg.ToJSON);
      LRes := LCli.doPOST('/messages', ['topic', 'topic1'], LMsg);
      WriteLn(LRes.ResponseCode, LRes.ResponseText);
      write('Send Message: ');
      ReadLn(LStrMsg);
    end;
  finally
    LCli.Free;
  end;
end;

begin
  try
    Main;
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;
  ReadLn;

end.
