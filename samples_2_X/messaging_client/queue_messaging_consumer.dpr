program queue_messaging_consumer;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils, MVCFramework.RESTClient, System.JSON,
  Winapi.Windows, MVCFramework.Commons;

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
    POST /messages/subscriptions/queue/<queuename1>
    POST /messages/subscriptions/queue/<queuename2>
    ...
    POST /messages/subscriptions/queue/<queuenameN>
    GET /messages (this line can return 200 OK or 408 REQUEST TIMEOUT, however there is always a message body)

    to unsubscribe
    DELETE /messages/subscriptions/queue/<queuename1>
    DELETE /messages/subscriptions/queue/<queuename2>
    ...
    DELETE /messages/subscriptions/queue/<queuenameN>
  }

  LMyClientID := 'my-unique-client-id' + GetTickCount.ToString;
  LCli := TRESTClient.Create('localhost', 9999);
  try
    LCli.ReadTimeout(-1);
    LRes := LCli.doPOST('/messages', ['clients', LMyClientID]);
    if LRes.ResponseCode <> HTTP_STATUS.OK then
      WriteLn('Cannot set client id. ' + LRes.ResponseText);
    LRes := LCli.doPOST('/messages/subscriptions/queue', ['queue1']);
    if LRes.ResponseCode <> HTTP_STATUS.OK then
      WriteLn('Cannot subscribe. ' + LRes.ResponseText);
    while True do
    begin
      WriteLn('Waiting on GET /messages');
      LRes := LCli.doGET('/messages', []);
      if LRes.ResponseCode = HTTP_STATUS.RequestTimeout then
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
