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
  MVCFramework.Logger, MVCFramework.Commons, Winapi.Windows;

{ TMessagingExtensionsTestCase }

procedure TMessagingExtensionsTestCase.TestMultipleSubscribeAndUnsubscribe;
var
  res: IRESTResponse;
  x: string;
begin
  RESTClient.ReadTimeout := - 1;
  DoLoginWith('guest');
  RESTClient.doPOST('/messages/clients', ['my-unique-id']);
  res := RESTClient.doPOST('/messages/subscriptions/test01', []);
  CheckEquals(HTTP_STATUS.OK, res.ResponseCode, res.ResponseText);
  res := RESTClient.doGET('/messages/subscriptions', []);
  x := Trim(res.BodyAsString);
  CheckEquals('/queue/test01', x);

  res := RESTClient.doPOST('/messages/subscriptions', ['test010']);
  CheckEquals(HTTP_STATUS.OK, res.ResponseCode, res.ResponseText);
  // server shod not return an error
  res := RESTClient.doGET('/messages/subscriptions', []);
  x := Trim(res.BodyAsString);
  CheckEquals('/queue/test01;/queue/test010', x);

  res := RESTClient.doDELETE('/messages/subscriptions/test01', []);
  CheckEquals(HTTP_STATUS.OK, res.ResponseCode, res.ResponseText);
  // server shod not return an error
  res := RESTClient.doGET('/messages/subscriptions', []);
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
  res := RESTClient.doPOST('/messages/subscriptions', ['test01']);
  res := RESTClient.doPOST('/messages/subscriptions', ['test010']);
  res := RESTClient.doPOST('/messages/subscriptions', ['test0101']);
  res := RESTClient.doPOST('/messages/subscriptions', ['test01010']);
  res := RESTClient.doPOST('/messages/subscriptions', ['test010101']);
  res := RESTClient.doPOST('/messages/subscriptions', ['test0101010']);

  CheckEquals(HTTP_STATUS.OK, res.ResponseCode, res.ResponseText);
  // server shod not return an error
  res := RESTClient.doGET('/messages/subscriptions', []);
  x := Trim(res.BodyAsString);
  CheckEquals
    ('/queue/test01;/queue/test010;/queue/test0101;/queue/test01010;/queue/test010101;/queue/test0101010',
    x);

  res := RESTClient.doDELETE('/messages/subscriptions', ['test010']);
  CheckEquals(HTTP_STATUS.OK, res.ResponseCode, res.ResponseText);
  // server shod not return an error
  res := RESTClient.doGET('/messages/subscriptions', []);
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
  res := RESTClient.doPOST('/messages/subscriptions/test01', []);
  CheckEquals(HTTP_STATUS.OK, res.ResponseCode, res.ResponseText);
  res := RESTClient.doPOST('/messages/subscriptions/test01', []);
  CheckEquals(HTTP_STATUS.OK, res.ResponseCode, res.ResponseText);
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
  J: Integer;
  LUnique: string;
const
  MSG_COUNT = 10;
begin
  LUnique := GetTickCount.ToString;

  DoLoginWith('guest');
  RESTClient.doPOST('/messages/clients', ['my-unique-' + LUnique]);
  RESTClient.doPOST('/messages', ['subscriptions', 'test01']);
  RESTClient.doPOST('/messages', ['subscriptions', 'test02']);
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
        RESTC.doPOST('/messages/clients', ['my-other-unique-' + LUnique]);
        RESTC.ReadTimeout := 60 * 1000 * 30;
        RESTC.doGET('/login', ['guest']);
        for I := 1 to MSG_COUNT do
        begin
          RESTC.doPOST('/messages/queues/test02', [], TJSONObject.Create(TJSONPair.Create('hello',
            TJSONNumber.Create(I))));
          RESTC.doPOST('/messages/queues/test01', [], TJSONObject.Create(TJSONPair.Create('hello',
            TJSONNumber.Create(I))));
        end;
      finally
        RESTC.Free;
      end;
    end).Start;

  RMessageCount := 0;
  for J := 1 to MSG_COUNT * 2 do
  begin
    res := RESTClient.doGET('/messages', []);
    if res.ResponseCode = HTTP_STATUS.OK then
    begin
      CheckIs(res.BodyAsJsonObject.Get('_timeout').JsonValue, TJSONFalse);
      CheckNotNull(res.BodyAsJsonObject.Get('_timestamp'), '_timestamp is not set');
      CheckNotNull(res.BodyAsJsonObject.Get('messages'), 'messages is not set');
      CheckIs(res.BodyAsJsonObject.Get('messages').JsonValue, TJSONArray,
        'Messages is not a TJSONArray');
      messages := res.BodyAsJsonObject.Get('messages').JsonValue as TJSONArray;
      if messages.Size > 0 then
        for I := 0 to messages.Size - 1 do
        begin
          o := messages.Get(I) as TJSONObject;
          logw(o.Get('message').ToString);
        end;
      RMessageCount := RMessageCount + messages.Size;
    end;
    if res.ResponseCode = HTTP_STATUS.RequestTimeout then // receive timeout
      break;
  end;
  CheckEquals(MSG_COUNT * 2, RMessageCount, 'message count');
  res := RESTClient.doGET('/messages', []);
  CheckIs(res.BodyAsJsonObject.Get('_timeout').JsonValue, TJSONTrue);
  DoLogout;
end;

procedure TMessagingExtensionsTestCase.TestSubscribeOnATopic;
var
  res: IRESTResponse;
begin
  DoLoginWith('guest');
  RESTClient.doPOST('/messages/clients', ['my-unique-id']);
  res := RESTClient.doPOST('/messages/subscriptions/test01', []);
  CheckEquals(HTTP_STATUS.OK, res.ResponseCode, res.ResponseText);
  res := RESTClient.doDELETE('/messages/subscriptions/test01', []);
  CheckEquals(HTTP_STATUS.OK, res.ResponseCode, res.ResponseText);
  DoLogout;
end;

initialization

{$IFDEF USE_MESSAGING}
  RegisterTest(TMessagingExtensionsTestCase.Suite);
{$ENDIF}

finalization

end.
