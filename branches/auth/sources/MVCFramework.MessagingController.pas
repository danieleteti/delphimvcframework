unit MVCFramework.MessagingController;

interface

uses
  MVCFramework,
  StompClient,
  StompTypes;

type

  [MVCPath('/messages')]
  TMVCBUSController = class(TMVCController)
  strict protected
    function GetUniqueDurableHeader(clientid, topicname: string): string;
    procedure InternalSubscribeUserToTopics(clientid: string; Stomp: IStompClient);
    procedure InternalSubscribeUserToTopic(clientid: string; topicname: string;
      StompClient: IStompClient);

    procedure AddTopicToUserSubscriptions(const ATopic: string);
    procedure RemoveTopicFromUserSubscriptions(const ATopic: string);
    procedure OnBeforeAction(Context: TWebContext; const AActionNAme: string;
      var Handled: Boolean); override;

  public
    [MVCHTTPMethod([httpPOST])]
    [MVCPath('/clients/($clientid)')]
    procedure SetClientID(CTX: TWebContext);

    [MVCPath('/subscriptions/($name)')]
    [MVCHTTPMethod([httpPOST])]
    procedure SubscribeToTopic(CTX: TWebContext);

    [MVCPath('/subscriptions/($name)')]
    [MVCHTTPMethod([httpDELETE])]
    procedure UnSubscribeFromTopic(CTX: TWebContext);

    [MVCHTTPMethod([httpGET])]
    [MVCPath]
    procedure ReceiveMessages(CTX: TWebContext);

    [MVCHTTPMethod([httpPOST])]
    [MVCPath('/queues/($topic)')]
    procedure EnqueueMessage(CTX: TWebContext);

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/subscriptions')]
    procedure CurrentlySubscribedTopics(CTX: TWebContext);
  end;

implementation

{ TMVCBUSController }

uses
  System.SysUtils,
  MVCFramework.Commons,
  System.DateUtils,
{$IF CompilerVersion < 27}
  Data.DBXJSON,
{$ELSE}
  System.JSON,
{$IFEND}
  MVCFramework.Logger,
  System.SyncObjs;

procedure TMVCBUSController.AddTopicToUserSubscriptions(const ATopic: string);
var
  x: string;
  topics: TArray<string>;
  t: string;
  ToAdd: Boolean;
begin
  x := Session['__subscriptions'];
  topics := x.Split([';']);
  ToAdd := true;
  for t in topics do
    if t.Equals(ATopic) then
    begin
      ToAdd := False;
    end;
  if ToAdd then
  begin
    SetLength(topics, length(topics) + 1);
    topics[length(topics) - 1] := ATopic;
    Session['__subscriptions'] := string.Join(';', topics);
  end;
end;

procedure TMVCBUSController.CurrentlySubscribedTopics(CTX: TWebContext);
begin
  ContentType := TMVCMimeType.TEXT_PLAIN;
  Render(Session['__subscriptions']);
end;

procedure TMVCBUSController.EnqueueMessage(CTX: TWebContext);
var
  topicname: string;
begin
  topicname := CTX.Request.Params['topic'].Trim;
  if topicname.IsEmpty then
    raise EMVCException.Create('Invalid or empty topic');

  EnqueueMessageOnTopic('/queue/' + topicname,
    CTX.Request.BodyAsJSONObject.Clone as TJSONObject, true);
  Render(200, 'Message sent to topic ' + topicname);
end;

function TMVCBUSController.GetUniqueDurableHeader(clientid, topicname: string): string;
begin
  Result := clientid + '___' + topicname.Replace('/', '_', [rfReplaceAll]);
end;

procedure TMVCBUSController.ReceiveMessages(CTX: TWebContext);
var
  Stomp: IStompClient;
  LClientID: string;
  frame: IStompFrame;
  obj, res: TJSONObject;
  LFrames: TArray<IStompFrame>;
  arr: TJSONArray;
  LLastReceivedMessageTS: TDateTime;
  LTimeOut: Boolean;
const

{$IFDEF TEST}
  RECEIVE_TIMEOUT = 5; // seconds

{$ELSE}
  RECEIVE_TIMEOUT = 60 * 5; // 5 minutes

{$ENDIF}
begin
  LTimeOut := False;
  LClientID := GetClientID;
  Stomp := GetNewStompClient(LClientID);
  try
    InternalSubscribeUserToTopics(LClientID, Stomp);
    // StartReceiving := now;

    LLastReceivedMessageTS := now;
    SetLength(LFrames, 0);
    while not IsShuttingDown do
    begin
      LTimeOut := False;
      frame := nil;
      Log('/messages receive');
      Stomp.Receive(frame, 100);
      if Assigned(frame) then
      // get 10 messages at max, and then send them to client
      begin
        LLastReceivedMessageTS := now;
        SetLength(LFrames, length(LFrames) + 1);
        LFrames[length(LFrames) - 1] := frame;
        Stomp.Ack(frame.MessageID);
        if length(LFrames) >= 10 then
          break;
      end
      else
      begin
        if (length(LFrames) > 0) then
          break;
        if SecondsBetween(now, LLastReceivedMessageTS) >= RECEIVE_TIMEOUT then
        begin
          LTimeOut := true;
          break;
        end;
      end;
    end;

    arr := TJSONArray.Create;
    res := TJSONObject.Create(TJSONPair.Create('messages', arr));
    for frame in LFrames do
    begin
      if Assigned(frame) then
      begin
        obj := TJSONObject.ParseJSONValue(frame.GetBody) as TJSONObject;
        if Assigned(obj) then
        begin
          arr.AddElement(obj);
        end
        else
        begin
          LogE(Format
            ('Not valid JSON object in topic requested by user %s. The raw message is "%s"',
            [LClientID, frame.GetBody]));
        end;
      end;
    end; // for in
    res.AddPair('_timestamp', FormatDateTime('yyyy-mm-dd hh:nn:ss', now));
    if LTimeOut then
    begin
      res.AddPair('_timeout', TJSONTrue.Create);
      Render(http_status.RequestTimeout, res);
    end
    else
    begin
      res.AddPair('_timeout', TJSONFalse.Create);
      Render(http_status.OK, res);
    end;

  finally
    // Stomp.Disconnect;
  end;
end;

procedure TMVCBUSController.RemoveTopicFromUserSubscriptions(const ATopic: string);
var
  x: string;
  topics, afterremovaltopics: TArray<string>;
  IndexToRemove: Integer;
  i: Integer;
begin
  x := Session['__subscriptions'];
  topics := x.Split([';']);
  IndexToRemove := 0;
  SetLength(afterremovaltopics, length(topics));
  for i := 0 to length(topics) - 1 do
  begin
    if not topics[i].Equals(ATopic) then
    begin
      afterremovaltopics[IndexToRemove] := topics[i];
      Inc(IndexToRemove);
    end;
  end;
  if IndexToRemove <> length(ATopic) - 1 then
    SetLength(afterremovaltopics, length(topics) - 1);

  if length(afterremovaltopics) = 0 then
    Session['__subscriptions'] := ''
  else
    Session['__subscriptions'] := string.Join(';', afterremovaltopics);
end;

procedure TMVCBUSController.SetClientID(CTX: TWebContext);
begin
  Session[CLIENTID_KEY] := CTX.Request.Params['clientid'];
end;

procedure TMVCBUSController.SubscribeToTopic(CTX: TWebContext);
var
  LStomp: IStompClient;
  LClientID: string;
  LTopicName: string;
begin
  LClientID := GetClientID;
  LTopicName := CTX.Request.Params['name'].ToLower;
  LStomp := GetNewStompClient(LClientID);
  try
    LTopicName := '/queue/' + LTopicName;
    InternalSubscribeUserToTopic(LClientID, LTopicName, LStomp);
    Render(200, 'Subscription OK for ' + LTopicName);
  finally
    // Stomp.Disconnect;
  end;
end;

procedure TMVCBUSController.InternalSubscribeUserToTopics(clientid: string; Stomp: IStompClient);
var
  x, t: string;
  topics: TArray<string>;
begin
  x := Session['__subscriptions'];
  topics := x.Split([';']);
  for t in topics do
    InternalSubscribeUserToTopic(clientid, t, Stomp);
end;

procedure TMVCBUSController.OnBeforeAction(Context: TWebContext; const AActionNAme: string;
  var Handled: Boolean);
begin
  inherited;
  if not StrToBool(Config['messaging']) then
  begin
    Handled := true;
    raise EMVCException.Create('Messaging extensions are not enabled');
  end;
  Handled := False;
end;

procedure TMVCBUSController.InternalSubscribeUserToTopic(clientid, topicname: string;
  StompClient: IStompClient);
var
  LDurSubHeader: string;
  LHeaders: IStompHeaders;
begin
  LHeaders := TStompHeaders.Create;
  LDurSubHeader := GetUniqueDurableHeader(clientid, topicname);
  LHeaders.Add(TStompHeaders.NewDurableSubscriptionHeader(LDurSubHeader));
  StompClient.Subscribe(topicname, amClient, LHeaders);
  LogE('SUBSCRIBE TO ' + clientid + '@' + topicname + ' dursubheader:' + LDurSubHeader);
  AddTopicToUserSubscriptions(topicname);
end;

procedure TMVCBUSController.UnSubscribeFromTopic(CTX: TWebContext);
var
  Stomp: IStompClient;
  clientid: string;
  thename: string;
  s: string;
begin
  clientid := GetClientID;
  thename := CTX.Request.Params['name'].ToLower;
  Stomp := GetNewStompClient(clientid);
  s := '/queue/' + thename;
  Stomp.Unsubscribe(s);
  RemoveTopicFromUserSubscriptions(s);
  Render(200, 'UnSubscription OK for ' + s);
end;

end.
