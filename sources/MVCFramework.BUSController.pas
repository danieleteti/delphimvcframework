unit MVCFramework.BUSController;

interface

uses
  MVCFramework,
  StompClient,
  StompTypes;

type

  [MVCPath('/messages')]
  TMVCBUSController = class(TMVCController)
  protected
    function GetUniqueDurableHeader(clientid, topicname: string): string;
    procedure InternalSubscribeUserToTopics(clientid: string;
      Stomp: IStompClient);
    procedure InternalSubscribeUserToTopic(clientid: string; topicname: string;
      StompClient: IStompClient);

    procedure AddTopicToUserSubscriptions(const ATopic: string);
    procedure RemoveTopicFromUserSubscriptions(const ATopic: string);
    procedure OnBeforeAction(Context: TWebContext; const AActionNAme: string;
      var Handled: Boolean); override;

  public
    [MVCPath('/subscribe/($name)')]
    procedure SubscribeToTopic(CTX: TWebContext);
    [MVCPath('/unsubscribe/($name)')]
    procedure UnSubscribeFromTopic(CTX: TWebContext);
    [MVCPath('/receive')]
    procedure ReceiveMessages(CTX: TWebContext);

    [MVCHTTPMethod([httpPOST])]
    [MVCPath('/enqueue/($topic)')]
    procedure EnqueueMessage(CTX: TWebContext);
    [MVCPath('/topics')]
    procedure CurrentlySubscribedTopics(CTX: TWebContext);
  end;

implementation

{ TMVCBUSController }

uses
  System.SysUtils,
  MVCFramework.Commons,
  System.DateUtils,
{$IF not Defined(VER270)}
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

  EnqueueMessageOnTopic('/topic/' + topicname,
    CTX.Request.BodyAsJSONObject.Clone as TJSONObject, true);
  Render(200, 'Message sent to topic ' + topicname);
end;

function TMVCBUSController.GetUniqueDurableHeader(clientid,
  topicname: string): string;
begin
  Result := clientid + '___' + topicname.Replace('/', '_', [rfReplaceAll]);
end;

procedure TMVCBUSController.ReceiveMessages(CTX: TWebContext);
var
  Stomp: IStompClient;
  clientid: string;
  frame: IStompFrame;
  // StartReceiving     : TDateTime;
  obj, res: TJSONObject;
  Frames: TArray<IStompFrame>;
  arr: TJSONArray;
  LastReceivedMessage: TDateTime;
  TOUT: Boolean;
const

{$IFDEF TEST}
  RECEIVE_TIMEOUT = 10; // seconds

{$ELSE}
  RECEIVE_TIMEOUT = 60 * 5; // 5 minutes

{$ENDIF}
begin
  TOUT := False;
  clientid := GetClientID;
  Stomp := GetNewStompClient(clientid);
  try
    InternalSubscribeUserToTopics(clientid, Stomp);
    // StartReceiving := now;

    LastReceivedMessage := now;
    SetLength(Frames, 0);
    while not IsShuttingDown do
    begin
      TOUT := False;
      frame := nil;
      LogE('Stomp.Receive');
      Stomp.Receive(frame, 500);
      if Assigned(frame) then
      // get 10 messages at max, and then send them to client
      begin
        LastReceivedMessage := now;
        SetLength(Frames, length(Frames) + 1);
        Frames[length(Frames) - 1] := frame;
        Stomp.Ack(frame.MessageID);
        if length(Frames) >= 10 then
          break;
      end
      else
      begin
        if (length(Frames) > 0) then
          break;
        if SecondsBetween(now, LastReceivedMessage) >= RECEIVE_TIMEOUT then
        begin
          TOUT := true;
          break;
        end;
      end;
    end;

    arr := TJSONArray.Create;
    res := TJSONObject.Create(TJSONPair.Create('messages', arr));
    for frame in Frames do
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
            [clientid, frame.GetBody]));
        end;
      end;
    end; // for in
    res.AddPair('_timestamp', FormatDateTime('yyyy-mm-dd hh:nn:ss', now));
    if TOUT then
      res.AddPair('_timedout', TJSONTrue.Create)
    else
      res.AddPair('_timedout', TJSONFalse.Create);

    Render(res);
  finally
    // Stomp.Disconnect;
  end;
end;

procedure TMVCBUSController.RemoveTopicFromUserSubscriptions
  (const ATopic: string);
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

procedure TMVCBUSController.SubscribeToTopic(CTX: TWebContext);
var
  Stomp: IStompClient;
  clientid: string;
  thename: string;
  s: string;
begin
  clientid := GetClientID;
  thename := CTX.Request.Params['name'].ToLower;
  Stomp := GetNewStompClient(clientid);
  try
    s := '/topic/' + thename;
    InternalSubscribeUserToTopic(clientid, s, Stomp);
    Render(200, 'Subscription OK for ' + s);
  finally
    // Stomp.Disconnect;
  end;
end;

procedure TMVCBUSController.InternalSubscribeUserToTopics(clientid: string;
  Stomp: IStompClient);
var
  x, t: string;
  topics: TArray<string>;
begin
  x := Session['__subscriptions'];
  topics := x.Split([';']);
  for t in topics do
    InternalSubscribeUserToTopic(clientid, t, Stomp);
end;

procedure TMVCBUSController.OnBeforeAction(Context: TWebContext;
  const AActionNAme: string; var Handled: Boolean);
begin
  inherited;
  if not StrToBool(Config['messaging']) then
  begin
    Handled := true;
    raise EMVCException.Create('Messaging extensions are not enabled');
  end;
  Handled := False;
end;

procedure TMVCBUSController.InternalSubscribeUserToTopic(clientid,
  topicname: string; StompClient: IStompClient);
var
  DurSubHeader: string;
begin
  DurSubHeader := GetUniqueDurableHeader(clientid, topicname);
  StompClient.Subscribe(topicname, amClient,
    StompUtils.NewHeaders.Add(TStompHeaders.NewDurableSubscriptionHeader
    (DurSubHeader)));
  LogE('SUBSCRIBE TO ' + clientid + '@' + topicname + ' dursubheader:' +
    DurSubHeader);
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
  s := '/topic/' + thename;
  Stomp.Unsubscribe(s);
  RemoveTopicFromUserSubscriptions(s);
  Render(200, 'UnSubscription OK for ' + s);
end;

end.
