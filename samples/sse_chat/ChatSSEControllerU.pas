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
    /// <summary>
    /// Periodically sends a "stats" event with the current user count.
    /// This demonstrates OnInterval in a push-driven scenario:
    /// even though chat messages arrive via POST API and are pushed
    /// through the broker, OnInterval is used for periodic status updates
    /// that don't originate from client actions.
    /// </summary>
    procedure OnInterval(const AConnection: TSSEConnection;
      var ANextIntervalMS: Integer); override;
    function Interval: Integer; override;
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

  // Store last known user count for change detection in OnInterval
  AConnection.CustomData := TObject(NativeInt(
    Length(ChatRoomU.TChatRoom.Instance.GetUsernames)));
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

function TChatSSEController.Interval: Integer;
begin
  // Send stats every 10 seconds by default
  Result := 10000;
end;

procedure TChatSSEController.OnInterval(const AConnection: TSSEConnection;
  var ANextIntervalMS: Integer);
var
  LCurrentCount, LLastCount: NativeInt;
  LJSON: TJSONObject;
begin
  LCurrentCount := Length(ChatRoomU.TChatRoom.Instance.GetUsernames);
  LLastCount := NativeInt(AConnection.CustomData);

  // Send a stats event with the current user count
  LJSON := TJSONObject.Create;
  try
    LJSON.AddPair('online', TJSONNumber.Create(LCurrentCount));
    AConnection.Send(TSSEMessage.Create('stats', LJSON.ToJSON));
  finally
    LJSON.Free;
  end;

  // Adaptive: if user count changed, check more frequently
  // (activity is likely happening)
  if LCurrentCount <> LLastCount then
  begin
    AConnection.CustomData := TObject(LCurrentCount);
    ANextIntervalMS := 5000; // More frequent during activity
  end
  else
  begin
    ANextIntervalMS := 15000; // Back off when stable
  end;
end;

end.
