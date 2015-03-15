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
  LMsg: string;
  LMyClientID: string;
begin
  LMyClientID := 'my-unique-client-id';
  LCli := TRESTClient.Create('localhost', 9999);
  try
    LCli.ReadTimeout := - 1;
    LRes := LCli.doPOST('/messages', ['clients', LMyClientID]);
    while True do
    begin
      LCli.doPOST('/messages', ['queues', 'queue1'],
        TJSONObject.Create(TJSONPair.Create('msg', GetTickCount.ToString)));
      WriteLn('in attesa di /messages/queues');
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
