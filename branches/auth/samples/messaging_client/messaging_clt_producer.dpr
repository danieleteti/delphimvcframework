program messaging_clt_producer;

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
  LMyClientID: string;
begin
  {
    USAGE PATTERN
    POST /messages/clients/<myuniqueid>
    POST /messages/queues/<queuename1> (message in the request body as json object)
  }

  LMyClientID := 'my-unique-client-id';
  LCli := TRESTClient.Create('localhost', 9999);
  try
    LCli.ReadTimeout := - 1;
    LRes := LCli.doPOST('/messages', ['clients', LMyClientID]);
    while True do
    begin
      LMsg := TJSONObject.Create(TJSONPair.Create('msg', GetTickCount.ToString));
      WriteLn('Writing on the queue: ' + LMsg.ToJSON);
      LCli.doPOST('/messages', ['queues', 'queue1'], LMsg);
      Sleep(10);
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
  readln;

end.
