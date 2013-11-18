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
  Data.DBXJSON,
  System.Classes,
  MVCFramework.Logger;

{ TMessagingExtensionsTestCase }

procedure TMessagingExtensionsTestCase.TestMultipleSubscribeAndUnsubscribe;
var
  res: IRESTResponse;
  x: string;
begin
  RESTClient.ReadTimeout := -1;
  DoLoginWith('d.teti');
  res := RESTClient.doGET('/messages/subscribe', ['test01']);
  CheckEquals(200, res.ResponseCode, res.ResponseText);
  res := RESTClient.doGET('/messages/topics', []);
  x := Trim(res.BodyAsString);
  CheckEquals('/topic/test01', x);

  res := RESTClient.doGET('/messages/subscribe', ['test010']);
  CheckEquals(200, res.ResponseCode, res.ResponseText);
  // server shod not return an error
  res := RESTClient.doGET('/messages/topics', []);
  x := Trim(res.BodyAsString);
  CheckEquals('/topic/test01;/topic/test010', x);

  res := RESTClient.doGET('/messages/unsubscribe', ['test01']);
  CheckEquals(200, res.ResponseCode, res.ResponseText);
  // server shod not return an error
  res := RESTClient.doGET('/messages/topics', []);
  x := Trim(res.BodyAsString);
  CheckEquals('/topic/test010', x);

  CheckEquals(200, res.ResponseCode, res.ResponseText);
  // server shod not return an error
  DoLogout;
end;

procedure TMessagingExtensionsTestCase.TestMultipleSubscribeAndUnsubscribeHARD;
var
  res: IRESTResponse;
  x: string;
begin
  RESTClient.ReadTimeout := -1;
  DoLoginWith('d.teti');
  res := RESTClient.doGET('/messages/subscribe', ['test01']);
  res := RESTClient.doGET('/messages/subscribe', ['test010']);
  res := RESTClient.doGET('/messages/subscribe', ['test0101']);
  res := RESTClient.doGET('/messages/subscribe', ['test01010']);
  res := RESTClient.doGET('/messages/subscribe', ['test010101']);
  res := RESTClient.doGET('/messages/subscribe', ['test0101010']);

  CheckEquals(200, res.ResponseCode, res.ResponseText);
  // server shod not return an error
  res := RESTClient.doGET('/messages/topics', []);
  x := Trim(res.BodyAsString);
  CheckEquals
    ('/topic/test01;/topic/test010;/topic/test0101;/topic/test01010;/topic/test010101;/topic/test0101010',
    x);

  res := RESTClient.doGET('/messages/unsubscribe', ['test010']);
  CheckEquals(200, res.ResponseCode, res.ResponseText);
  // server shod not return an error
  res := RESTClient.doGET('/messages/topics', []);
  x := Trim(res.BodyAsString);
  CheckEquals
    ('/topic/test01;/topic/test0101;/topic/test01010;/topic/test010101;/topic/test0101010',
    x);
  DoLogout;
end;

procedure TMessagingExtensionsTestCase.TestMultipleSubscribeOnSameTopic;
var
  res: IRESTResponse;
begin
  DoLoginWith('d.teti');
  res := RESTClient.doGET('/messages/subscribe', ['test01']);
  CheckEquals(200, res.ResponseCode, res.ResponseText);
  res := RESTClient.doGET('/messages/subscribe', ['test01']);
  CheckEquals(200, res.ResponseCode, res.ResponseText);
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
  MSG_COUNT = 100;
begin
  DoLoginWith('d.teti');
  RESTClient.doGET('/messages/subscribe/test01', []);
  RESTClient.doGET('/messages/subscribe/test02', []);
  RESTClient.ReadTimeout := -1;

  RESTClient.doPOST('/messages/enqueue/test02', [],
    TJSONObject.Create(TJSONPair.Create('hello', 'world')));

  sid := RESTClient.SessionID;
  TThread.CreateAnonymousThread(
    procedure
    var
      RESTC: TRESTClient;
      I: Integer;
    begin
      TThread.Sleep(1000);
      RESTC := TRESTClient.Create('localhost', 8888);
      try
        RESTC.ReadTimeout := 60 * 1000 * 30;
        RESTC.doGET('/login', ['j.doe']);
        for I := 1 to MSG_COUNT do
        begin
          RESTC.doPOST('/messages/enqueue/test02', [],
            TJSONObject.Create(TJSONPair.Create('hello',
            TJSONNumber.Create(I))));
        end;
      finally
        RESTC.Free;
      end;
    end).Start;

  RMessageCount := 0;
  while RMessageCount < MSG_COUNT do
  begin
    res := RESTClient.doGET('/messages/receive', []);
    if res.ResponseCode = 200 then
    begin
      CheckNotNull(res.BodyAsJsonObject.Get('_timestamp'),
        '_timestamp is not set');
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
    if res.ResponseCode = 204 then // receive timeout
      break;
  end;
  CheckEquals(MSG_COUNT, RMessageCount, 'message count');
  DoLogout;
end;

procedure TMessagingExtensionsTestCase.TestSubscribeOnATopic;
var
  res: IRESTResponse;
begin
  DoLoginWith('d.teti');
  res := RESTClient.doGET('/messages/subscribe', ['test01']);
  CheckEquals(200, res.ResponseCode, res.ResponseText);
  res := RESTClient.doGET('/messages/unsubscribe', ['test01']);
  CheckEquals(200, res.ResponseCode, res.ResponseText);
  DoLogout;
end;

initialization

// RegisterTest(TMessagingExtensionsTestCase.Suite);
finalization

end.
