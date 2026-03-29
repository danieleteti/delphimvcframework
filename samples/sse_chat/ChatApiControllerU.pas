unit ChatApiControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons;

type
  [MVCPath('/api')]
  TChatApiController = class(TMVCController)
  public
    [MVCPath('/messages')]
    [MVCHTTPMethod([httpPOST])]
    [MVCConsumes(TMVCMediaType.APPLICATION_JSON)]
    procedure PostMessage;

    [MVCPath('/users')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(TMVCMediaType.APPLICATION_JSON)]
    procedure GetUsers;
  end;

implementation

uses
  System.SysUtils, System.JSON, MVCFramework.SSE, MVCFramework.Logger, ChatRoomU;

{ TChatApiController }

procedure TChatApiController.PostMessage;
var
  LJSON: TJSONObject;
  LUsername, LText, LTo: string;
  LTargetId, LSenderId: string;
  LMsgJSON: TJSONObject;
  LMsg: TSSEMessage;
begin
  LJSON := TJSONObject.ParseJSONValue(Context.Request.Body) as TJSONObject;
  if LJSON = nil then
  begin
    Render(HTTP_STATUS.BadRequest, 'Invalid JSON body');
    Exit;
  end;
  try
    LUsername := LJSON.GetValue<string>('username', '');
    LText := LJSON.GetValue<string>('text', '');
    LTo := LJSON.GetValue<string>('to', '');

    if LUsername.IsEmpty or LText.IsEmpty then
    begin
      Render(HTTP_STATUS.BadRequest, 'username and text are required');
      Exit;
    end;

    LMsgJSON := TJSONObject.Create;
    try
      LMsgJSON.AddPair('from', LUsername);
      LMsgJSON.AddPair('text', LText);

      if not LTo.IsEmpty then
      begin
        // Private message
        LMsgJSON.AddPair('private', TJSONBool.Create(True));
        LMsgJSON.AddPair('to', LTo);

        LTargetId := TChatRoom.Instance.GetClientIdByUsername(LTo);
        LSenderId := TChatRoom.Instance.GetClientIdByUsername(LUsername);
        LMsg := TSSEMessage.Create('chat', LMsgJSON.ToJSON);

        if not LTargetId.IsEmpty then
          SSEBroker.SendTo('/chat', LTargetId, LMsg);
        if (not LSenderId.IsEmpty) and (not SameText(LSenderId, LTargetId)) then
          SSEBroker.SendTo('/chat', LSenderId, LMsg);

        LogI('Private message from %s to %s: %s', [LUsername, LTo, LText], 'chat');
      end
      else
      begin
        // Broadcast message
        LMsgJSON.AddPair('private', TJSONBool.Create(False));
        LMsg := TSSEMessage.Create('chat', LMsgJSON.ToJSON);
        SSEBroker.Broadcast('/chat', LMsg);
        LogI('Broadcast message from %s: %s', [LUsername, LText], 'chat');
      end;
    finally
      LMsgJSON.Free;
    end;
  finally
    LJSON.Free;
  end;
  Render(HTTP_STATUS.Created, 'Message sent');
end;

procedure TChatApiController.GetUsers;
var
  LUsers: TArray<string>;
  LArr: TJSONArray;
  LUser: string;
begin
  LUsers := TChatRoom.Instance.GetUsernames;
  LArr := TJSONArray.Create;
  try
    for LUser in LUsers do
      LArr.Add(LUser);
    Render(LArr);
  except
    LArr.Free;
    raise;
  end;
end;

end.
