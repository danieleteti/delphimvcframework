unit MainControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons;

type

  [MVCPath('/')]
  TMainController = class(TMVCController)
  public
    [MVCPath('/facts/datetime')]
    [MVCHTTPMethod([httpGET])]
    procedure GetDateTime;

    [MVCPath('/topics/($TopicName)/($ClientID)')]
    [MVCHTTPMethod([httpGET])]
    procedure TopicDequeue(const TopicName: string; const ClientID: string);

    [MVCPath('/topics/($TopicName)/($ClientID)')]
    [MVCHTTPMethod([httpPOST])]
    procedure TopicEnqueue(const TopicName: string; const ClientID: string);

    procedure OnBeforeAction(Context: TWebContext; const AActionName: string;
      var Handled: Boolean); override;
    procedure OnAfterAction(Context: TWebContext; const AActionName: string); override;
  end;

implementation

uses
  System.JSON, ObjectsMappers, System.SysUtils, StompClient, StompTypes;

const
  DEFAULT_TIMEOUT = 30000; // 30 secs
  WAIT_TIMEOUT = 1000; // 500 msec

procedure TMainController.GetDateTime;
var
  lJObj: TJSONObject;
begin
  lJObj := TJSONObject.Create;
  try
    lJObj.AddPair('datetime', ISODateTimeToString(Now));
    Render(lJObj);
  except
    lJObj.Free;
    raise;
  end;
end;

procedure TMainController.OnAfterAction(Context: TWebContext; const AActionName: string);
begin
  { Executed after each action }
  inherited;
end;

procedure TMainController.OnBeforeAction(Context: TWebContext; const AActionName: string;
  var Handled: Boolean);
begin
  { Executed before each action
    if handled is true (or an exception is raised) the actual
    action will not be called }
  inherited;
end;

procedure TMainController.TopicDequeue(const TopicName: string; const ClientID: string);
var
  lStomp: IStompClient;
  lStompFrame: IStompFrame;
  lJObj: TJSONObject;
  lJObjMsg: TJSONObject;
  lJArrMessages: TJSONArray;
  lTimeout: Integer;
  lCount: UInt64;
begin
  lStomp := TStompClient.Create;
  lStomp.Connect('127.0.0.1', 61613, ClientID);
  lStomp.Subscribe('/topic/' + TopicName, amAuto,
    StompUtils.Headers
    .Add(TStompHeaders.AUTO_DELETE, 'false')
    .Add(TStompHeaders.Subscription(ClientID))
    .Add(TStompHeaders.Persistent(true))
    );
  lJObj := TJSONObject.Create;
  try
    lJArrMessages := TJSONArray.Create;
    lJObj.AddPair('data', lJArrMessages);
    lCount := 0;
    lTimeout := DEFAULT_TIMEOUT;
    while lStomp.Receive(lStompFrame, lTimeout) do
    begin
      Inc(lCount);
      lJObjMsg := TJSONObject.ParseJSONValue(lStompFrame.Body) as TJSONObject;
      lJArrMessages.AddElement(lJObjMsg);
      lJObjMsg.AddPair('_messageid', lStompFrame.MessageID);
      lTimeout := WAIT_TIMEOUT; // wait to see in there are already other messages
    end;

    if lCount > 0 then
    begin
      lJObj.AddPair('status', 'ok');
      lJObj.AddPair('count', TJSONNumber.Create(lCount));
    end
    else
    begin
      lJObj.AddPair('status', 'timeout');
    end;
    Render(lJObj);
  except
    on E: Exception do
    begin
      lJObj.AddPair('status', 'error');
      lJObj.AddPair('error', E.Message);
      lJObj.AddPair('classname', E.ClassName);
      Render(lJObj);
    end;
  end;
end;

procedure TMainController.TopicEnqueue(const TopicName, ClientID: string);
var
  lJObj: TJSONObject;
  lStomp: TStompClient;
  lJMessage: TJSONObject;
begin
  lJObj := TJSONObject.ParseJSONValue(Context.Request.Body) as TJSONObject;
  try
    if Assigned(lJObj.GetValue('message')) and (Assigned(lJObj.GetValue('username'))) then
    begin
      lJMessage := TJSONObject.Create;
      try
        lJMessage.AddPair('username', lJObj.GetValue('username').Value);
        lJMessage.AddPair('message', lJObj.GetValue('message').Value);
        lStomp := TStompClient.Create;
        lStomp.Connect('127.0.0.1', 61613, ClientID);
        lStomp.Send('/topic/' + TopicName, lJMessage.ToJSON);
      finally
        lJMessage.Free;
      end;
    end;
  finally
    lJObj.Free;
  end;
end;

end.
