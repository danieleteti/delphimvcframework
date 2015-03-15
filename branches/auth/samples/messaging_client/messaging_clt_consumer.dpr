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
