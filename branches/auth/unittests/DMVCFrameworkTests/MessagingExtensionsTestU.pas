unit MessagingExtensionsTestU;

interface

uses
  TestFramework,
  MVCFramework.RESTClient,
  LiveServerTestU;

type
  TMessagingExtensionsTestCase = class(TBaseServerTest)
  published
    procedure TestSubscribeOnATopic;
    procedure TestMultipleSubscribeOnSameTopic;
    procedure TestMultipleSubscribeAndUnsubscribe;
    procedure TestMultipleSubscribeAndUnsubscribeHARD;
    procedure TestSubscribeAndReceive;
  end;

implementation

uses
  System.SysUtils,
{$IF CompilerVersion < 27}
  Data.DBXJSON,
{$ELSE}
  System.JSON,
{$ENDIF}
  System.Classes,
  MVCFramework.Logger, MVCFramework.Commons;

{ TMessagingExtensionsTestCase }

procedure TMessagingExtensionsTestCase.TestMultipleSubscribeAndUnsubscribe;
var
  res: IRESTResponse;
  x: string;
begin
  RESTClient.ReadTimeout := - 1;
  DoLoginWith('guest');
  RESTClient.doPOST('/messages/clients', ['my-unique-id']);
  res := RESTClient.doGET('/messages/subscribe', ['test01']);
  CheckEquals(HTTP_STATUS.OK, res.ResponseCode, res.ResponseText);
  res := RESTClient.doGET('/messages/topics', []);
  x := Trim(res.BodyAsString);
  CheckEquals('/queue/test01', x);

  res := RESTClient.doGET('/messages/subscribe', ['test010']);
  CheckEquals(HTTP_STATUS.OK, res.ResponseCode, res.ResponseText);
  // server shod not return an error
  res := RESTClient.doGET('/messages/topics', []);
  x := Trim(res.BodyAsString);
  CheckEquals('/queue/test01;/queue/test010', x);

  res := RESTClient.doGET('/messages/unsubscribe', ['test01']);
  CheckEquals(HTTP_STATUS.OK, res.ResponseCode, res.ResponseText);
  // server shod not return an error
  res := RESTClient.doGET('/messages/topics', []);
  x := Trim(res.BodyAsString);
  CheckEquals('/queue/test010', x);

  CheckEquals(HTTP_STATUS.OK, res.ResponseCode, res.ResponseText);
  // server shod not return an error
  DoLogout;
end;

procedure TMessagingExtensionsTestCase.TestMultipleSubscribeAndUnsubscribeHARD;
var
  res: IRESTResponse;
  x: string;
begin
  RESTClient.ReadTimeout := - 1;
  DoLoginWith('guest');
  RESTClient.doPOST('/messages/clients', ['my-unique-id']);
  res := RESTClient.doGET('/messages/subscribe', ['test01']);
  res := RESTClient.doGET('/messages/subscribe', ['test010']);
  res := RESTClient.doGET('/messages/subscribe', ['test0101']);
  res := RESTClient.doGET('/messages/subscribe', ['test01010']);
  res := RESTClient.doGET('/messages/subscribe', ['test010101']);
  res := RESTClient.doGET('/messages/subscribe', ['test0101010']);

  CheckEquals(HTTP_STATUS.OK, res.ResponseCode, res.ResponseText);
  // server shod not return an error
  res := RESTClient.doGET('/messages/topics', []);
  x := Trim(res.BodyAsString);
  CheckEquals
    ('/queue/test01;/queue/test010;/queue/test0101;/queue/test01010;/queue/test010101;/queue/test0101010',
    x);

  res := RESTClient.doGET('/messages/unsubscribe', ['test010']);
  CheckEquals(HTTP_STATUS.OK, res.ResponseCode, res.ResponseText);
  // server shod not return an error
  res := RESTClient.doGET('/messages/topics', []);
  x := Trim(res.BodyAsString);
  CheckEquals
    ('/queue/test01;/queue/test0101;/queue/test01010;/queue/test010101;/queue/test0101010', x);
  DoLogout;
end;

procedure TMessagingExtensionsTestCase.TestMultipleSubscribeOnSameTopic;
var
  res: IRESTResponse;
begin
  DoLoginWith('guest');
  RESTClient.doPOST('/messages/clients', ['my-unique-id']);
  res := RESTClient.doGET('/messages/subscribe', ['test01']);
  CheckEquals(http_status.OK, res.ResponseCode, res.ResponseText);
  res := RESTClient.doGET('/messages/subscribe', ['test01']);
  CheckEquals(http_status.OK, res.ResponseCode, res.ResponseText);
  // server shod not return an error
  DoLogout;
end;

procedure TMessagingExtensionsTestCase.TestSubscribeAndReceive;
var
  res: IRESTResponse;
  messages: TJSONArray;
  sid: string;
  RMessageCount: Integer;
  I: Integer;
  o: TJSONObject;
const
  MSG_COUNT = 10;
begin
  DoLoginWith('guest');
  RESTClient.doPOST('/messages/clients', ['my-unique-id']);
  RESTClient.doGET('/messages/subscribe/test01', []);
  RESTClient.doGET('/messages/subscribe/test02', []);
  RESTClient.ReadTimeout := - 1;

  sid := RESTClient.SessionID;
  TThread.CreateAnonymousThread(
    procedure
    var
      RESTC: TRESTClient;
      I: Integer;
    begin
      TThread.Sleep(1000);
      RESTC := TRESTClient.Create('localhost', 9999);
      try
        RESTC.doPOST('/messages/clients', ['my-other-unique-id']);
        RESTC.ReadTimeout := 60 * 1000 * 30;
        RESTC.doGET('/login', ['guest']);
        for I := 1 to MSG_COUNT do
        begin
          RESTC.doPOST('/messages/enqueue/test02', [], TJSONObject.Create(TJSONPair.Create('hello',
            TJSONNumber.Create(I))));
          RESTC.doPOST('/messages/enqueue/test01', [], TJSONObject.Create(TJSONPair.Create('hello',
            TJSONNumber.Create(I))));
        end;
      finally
        RESTC.Free;
      end;
    end).Start;

  RMessageCount := 0;
  while RMessageCount < MSG_COUNT * 2 do
  begin
    res := RESTClient.doGET('/messages/receive', []);
    if res.ResponseCode = http_status.OK then
    begin
      CheckNotNull(res.BodyAsJsonObject.Get('_timestamp'), '_timestamp is not set');
      CheckNotNull(res.BodyAsJsonObject.Get('messages'), 'messages is not set');
      CheckIs(res.BodyAsJsonObject.Get('messages').JsonValue, TJSONArray,
        'Messages is not a TJSONArray');
      messages := res.BodyAsJsonObject.Get('messages').JsonValue as TJSONArray;
      if messages.Size > 0 then
        for I := 0 to messages.Size - 1 do
        begin
          o := messages.Get(I) as TJSONObject;
          logw(o.Get('message').toString);
        end;
      RMessageCount := RMessageCount + messages.Size;
    end;
    if res.ResponseCode = http_status.NoContent then // receive timeout
      break;
  end;
  CheckEquals(MSG_COUNT * 2, RMessageCount, 'message count');
  DoLogout;
end;

procedure TMessagingExtensionsTestCase.TestSubscribeOnATopic;
var
  res: IRESTResponse;
begin
  DoLoginWith('guest');
  RESTClient.doPOST('/messages/clients', ['my-unique-id']);
  res := RESTClient.doGET('/messages/subscribe', ['test01']);
  CheckEquals(http_status.OK, res.ResponseCode, res.ResponseText);
  res := RESTClient.doGET('/messages/unsubscribe', ['test01']);
  CheckEquals(http_status.OK, res.ResponseCode, res.ResponseText);
  DoLogout;
end;

initialization

{$IFDEF USE_MESSAGING}
  RegisterTest(TMessagingExtensionsTestCase.Suite);
{$ENDIF}

finalization

end.
