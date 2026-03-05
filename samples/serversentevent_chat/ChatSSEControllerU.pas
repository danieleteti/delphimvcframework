unit ChatSSEControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.SSEController, MVCFramework.SSE;

type
  [MVCPath('/chat')]
  TChatSSEController = class(TMVCSSEController)
  protected
    procedure OnClientConnected(const AConnection: TSSEConnection); override;
    procedure OnClientDisconnected(const AConnection: TSSEConnection); override;
  end;

implementation

uses
  MVCFramework.Logger, System.SysUtils, System.JSON, ChatRoomU;

{ TChatSSEController }

procedure TChatSSEController.OnClientConnected(const AConnection: TSSEConnection);
var
  LUsername: string;
begin
  LUsername := Context.Request.Params['username'];
  if LUsername.IsEmpty then
    LUsername := 'Anonymous';
  ChatRoomU.TChatRoom.Instance.AddUser(LUsername, AConnection.ClientId);
  LogI('Chat user connected: %s (clientId: %s)', [LUsername, AConnection.ClientId], 'chat');
  SSEBroker.Broadcast('/chat',
    TSSEMessage.Create('system', '"' + LUsername + ' has joined the chat"'));
  ChatRoomU.TChatRoom.Instance.BroadcastUserList;
end;

procedure TChatSSEController.OnClientDisconnected(const AConnection: TSSEConnection);
var
  LUsername: string;
begin
  LUsername := ChatRoomU.TChatRoom.Instance.RemoveUserByClientId(AConnection.ClientId);
  if not LUsername.IsEmpty then
  begin
    LogI('Chat user disconnected: %s (clientId: %s)', [LUsername, AConnection.ClientId], 'chat');
    SSEBroker.Broadcast('/chat',
      TSSEMessage.Create('system', '"' + LUsername + ' has left the chat"'));
    ChatRoomU.TChatRoom.Instance.BroadcastUserList;
  end;
end;

end.
