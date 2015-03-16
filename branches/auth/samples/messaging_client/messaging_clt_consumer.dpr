program messaging_clt_consumer;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils, MVCFramework.RESTClient, System.JSON, MVCFramework.Commons;

procedure Main;
var
  LCli: TRESTClient;
  LRes: IRESTResponse;
  LMsg: string;
  LMyClientID: string;
begin
  {
    USAGE PATTERN
    POST /messages/clients/<myuniqueid>
    POST /messages/subscriptions/<queuename1>
    POST /messages/subscriptions/<queuename2>
    ...
    POST /messages/subscriptions/<queuenameN>
    GET /messages (this line can return 200 OK or 408 REQUEST TIMEOUT, however there is always a message body)

    to unsubscribe
    DELETE /messages/subscriptions/<queuename1>
    DELETE /messages/subscriptions/<queuename2>
    ...
    DELETE /messages/subscriptions/<queuenameN>
  }

  LMyClientID := 'my-unique-client-id';
  LCli := TRESTClient.Create('localhost', 9999);
  try
    LCli.ReadTimeout := - 1;
    LRes := LCli.doPOST('/messages', ['clients', LMyClientID]);
    LRes := LCli.doPOST('/messages', ['subscriptions', 'queue1']);
    if LRes.ResponseCode <> http_status.OK then
      WriteLn('Cannot subscribe');
    while True do
    begin
      WriteLn('Waiting on GET /messages');
      LRes := LCli.doGET('/messages', []);
      if LRes.ResponseCode = http_status.RequestTimeout then
        WriteLn('TIMEOUT: ' + LRes.BodyAsString)
      else
        WriteLn(LRes.BodyAsString);
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
