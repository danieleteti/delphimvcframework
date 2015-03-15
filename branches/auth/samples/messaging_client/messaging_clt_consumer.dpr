program messaging_clt_consumer;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils, MVCFramework.RESTClient, System.JSON;

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
    LRes := LCli.doGET('/messages', ['subscribe', 'queue1']);
    if LRes.ResponseCode <> 200 then
      raise Exception.Create(LRes.BodyAsString);
    LCli.doPOST('/messages', ['enqueue', 'queue1'],
      TJSONObject.Create(TJSONPair.Create('msg', 'PING')));
    while True do
    begin
      WriteLn('in attesa di /messages/receive');
      LRes := LCli.doGET('/messages', ['receive']);
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
