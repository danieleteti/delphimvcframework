unit ChatRoomU;

interface

uses
  System.SysUtils, System.SyncObjs, System.Generics.Collections,
  System.Generics.Defaults, System.JSON, MVCFramework.SSE;

type
  TChatRoom = class
  private
    fUsers: TDictionary<string, string>; // username -> clientId
    fLock: TCriticalSection;
    class var FInstance: TChatRoom;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddUser(const AUsername, AClientId: string);
    function RemoveUserByClientId(const AClientId: string): string;
    function GetClientIdByUsername(const AUsername: string): string;
    function GetUsernameByClientId(const AClientId: string): string;
    function GetUsernames: TArray<string>;
    function IsUsernameAvailable(const AUsername: string): Boolean;
    procedure BroadcastUserList;
    class function Instance: TChatRoom;
  end;

implementation

{ TChatRoom }

constructor TChatRoom.Create;
begin
  inherited Create;
  fUsers := TDictionary<string, string>.Create;
  fLock := TCriticalSection.Create;
end;

destructor TChatRoom.Destroy;
begin
  fLock.Free;
  fUsers.Free;
  inherited;
end;

procedure TChatRoom.AddUser(const AUsername, AClientId: string);
begin
  fLock.Enter;
  try
    fUsers.AddOrSetValue(AUsername, AClientId);
  finally
    fLock.Leave;
  end;
end;

function TChatRoom.RemoveUserByClientId(const AClientId: string): string;
var
  LPair: TPair<string, string>;
begin
  Result := '';
  fLock.Enter;
  try
    for LPair in fUsers do
    begin
      if SameText(LPair.Value, AClientId) then
      begin
        Result := LPair.Key;
        fUsers.Remove(LPair.Key);
        Break;
      end;
    end;
  finally
    fLock.Leave;
  end;
end;

function TChatRoom.GetClientIdByUsername(const AUsername: string): string;
begin
  Result := '';
  fLock.Enter;
  try
    fUsers.TryGetValue(AUsername, Result);
  finally
    fLock.Leave;
  end;
end;

function TChatRoom.GetUsernameByClientId(const AClientId: string): string;
var
  LPair: TPair<string, string>;
begin
  Result := '';
  fLock.Enter;
  try
    for LPair in fUsers do
    begin
      if SameText(LPair.Value, AClientId) then
      begin
        Result := LPair.Key;
        Break;
      end;
    end;
  finally
    fLock.Leave;
  end;
end;

function TChatRoom.GetUsernames: TArray<string>;
var
  LList: TList<string>;
begin
  fLock.Enter;
  try
    LList := TList<string>.Create;
    try
      LList.AddRange(fUsers.Keys);
      LList.Sort;
      Result := LList.ToArray;
    finally
      LList.Free;
    end;
  finally
    fLock.Leave;
  end;
end;

function TChatRoom.IsUsernameAvailable(const AUsername: string): Boolean;
begin
  fLock.Enter;
  try
    Result := not fUsers.ContainsKey(AUsername);
  finally
    fLock.Leave;
  end;
end;

procedure TChatRoom.BroadcastUserList;
var
  LUsers: TArray<string>;
  LJSON: TJSONArray;
  LUsername: string;
begin
  LUsers := GetUsernames;
  LJSON := TJSONArray.Create;
  try
    for LUsername in LUsers do
      LJSON.Add(LUsername);
    SSEBroker.Broadcast('/chat', TSSEMessage.Create('userlist', LJSON.ToJSON));
  finally
    LJSON.Free;
  end;
end;

class function TChatRoom.Instance: TChatRoom;
var
  LNewInstance: TChatRoom;
begin
  if FInstance = nil then
  begin
    LNewInstance := TChatRoom.Create;
    if TInterlocked.CompareExchange<TChatRoom>(FInstance, LNewInstance, nil) <> nil then
      LNewInstance.Free;
  end;
  Result := FInstance;
end;

initialization

finalization
  FreeAndNil(TChatRoom.FInstance);

end.
