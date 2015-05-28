unit Iocp.VariantSocket;

interface

uses
  Windows, Classes, SysUtils, Math, Iocp.TcpSocket, Iocp.PacketSocket, Iocp.VariantPacket,
  Iocp.Utils;

type
  TIocpVariantServerConnection = class(TIocpPacketConnection);

  TIocpVariantRequestEvent = procedure(Sender: TObject; Client: TIocpVariantServerConnection; Request, Response: TIocpVariantPacket) of object;

  TIocpVariantServer = class(TIocpPacketServer)
  private
    FOnRequest: TIocpVariantRequestEvent;
  protected
    procedure TriggerPacketRecv(Client: TIocpPacketConnection; const Packet: TIocpPacket); override;
  protected
    procedure DoOnRequest(Client: TIocpVariantServerConnection; Request, Response: TIocpVariantPacket); virtual;
  public
    constructor Create(AOwner: TComponent); overload; override;
  published
    property OnRequest: TIocpVariantRequestEvent read FOnRequest write FOnRequest;
  end;

  TIocpVariantClientConnection = class(TIocpPacketConnection)
  private
    FBusyEvent: THandle;
    FSendStream: TMemoryStream;
    FRequest, FSyncResponse: TIocpVariantPacket;
  protected
    procedure Initialize; override;
    function GetIsIdle: Boolean; override;
  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;

    procedure SetBusy(Busy: Boolean);
  end;

  TIocpVariantResponseEvent = procedure(Sender: TObject; Client: TIocpVariantClientConnection; Request, Response: TIocpVariantPacket) of object;

  TIocpVariantClient = class(TIocpPacketSocket)
  private
    FOnResponse: TIocpVariantResponseEvent;
    FServerPort: Word;
    FServerAddr: string;
    FRetryDelay: Integer;
    FTCPRetry: Integer;

    function GetIdleConnectionAndLock: TIocpVariantClientConnection;
    function SendRequest(Client: TIocpVariantClientConnection; Request, Response: TIocpVariantPacket): Boolean;
    function ProcessRetry(var ErrCount: Integer): Boolean;
    procedure SetRetryDelay(const Value: Integer);
  protected
    procedure TriggerPacketRecv(Client: TIocpPacketConnection; const Packet: TIocpPacket); override;
  protected
    procedure DoOnResponse(Client: TIocpVariantClientConnection; Request, Response: TIocpVariantPacket); virtual;
  public
    constructor Create(AOwner: TComponent); overload; override;

    function SyncRequest(Request, Response: TIocpVariantPacket): Boolean; overload;  // Response由调用者事先创建
    function SyncRequest(Request: TIocpVariantPacket): TIocpVariantPacket; overload; // 调用者负责释放返回的数据结构
    function AsyncRequest(Request: TIocpVariantPacket): Boolean;
  published
    property ServerAddr: string read FServerAddr write FServerAddr;
    property ServerPort: Word read FServerPort write FServerPort;

    // <0 无限重试、=0 不重试、>0 重试次数
    property TCPRetry: Integer read FTCPRetry write FTCPRetry default 10;
    property RetryDelay: Integer read FRetryDelay write SetRetryDelay default 2000;

    property OnResponse: TIocpVariantResponseEvent read FOnResponse write FOnResponse;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Iocp', [TIocpVariantClient, TIocpVariantServer]);
end;

{ TIocpVariantServer }

constructor TIocpVariantServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ConnectionClass := TIocpVariantServerConnection;
end;

procedure TIocpVariantServer.DoOnRequest(Client: TIocpVariantServerConnection;
  Request, Response: TIocpVariantPacket);
begin
  if Assigned(FOnRequest) then
    FOnRequest(Self, Client, Request, Response);
end;

procedure TIocpVariantServer.TriggerPacketRecv(Client: TIocpPacketConnection;
  const Packet: TIocpPacket);
var
  Request, Response: TIocpVariantPacket;
  Stream: TMemoryStream;
begin
  Request := TIocpVariantPacket.Create;
  Response := TIocpVariantPacket.Create;
  Stream := TMemoryStream.Create;
  try
    Request.LoadFromBuf(Packet.Data, Packet.Header.DataSize);
    Response.Cmd := Request.Cmd;
    Response.Params['success'] := True;
    DoOnRequest(TIocpVariantServerConnection(Client), Request, Response);
    Response.SaveToStream(Stream);
    Client.Send(Stream.Memory, Stream.Size);
  finally
    Request.Free;
    Response.Free;
    Stream.Free;
  end;
end;

{ TIocpVariantClientConnection }

constructor TIocpVariantClientConnection.Create(AOwner: TObject);
begin
  inherited Create(AOwner);

  FBusyEvent := CreateEvent(nil, True, True, nil);
  FSendStream := TMemoryStream.Create;
  FRequest := TIocpVariantPacket.Create;
end;

destructor TIocpVariantClientConnection.Destroy;
begin
  CloseHandle(FBusyEvent);
  FSendStream.Free;
  FRequest.Free;

  inherited Destroy;
end;

function TIocpVariantClientConnection.GetIsIdle: Boolean;
begin
  Result := inherited GetIsIdle and (WaitForSingleObject(FBusyEvent, 0) = WAIT_OBJECT_0);
end;

procedure TIocpVariantClientConnection.Initialize;
begin
  inherited Initialize;

  SetBusy(False);
end;

procedure TIocpVariantClientConnection.SetBusy(Busy: Boolean);
begin
  if Busy then
    ResetEvent(FBusyEvent)
  else
    SetEvent(FBusyEvent);
end;

{ TIocpVariantClient }

constructor TIocpVariantClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ConnectionClass := TIocpVariantClientConnection;
  FTCPRetry := 10;
  FRetryDelay := 2000;
end;

procedure TIocpVariantClient.DoOnResponse(Client: TIocpVariantClientConnection;
  Request, Response: TIocpVariantPacket);
begin
  if Assigned(FOnResponse) then
    FOnResponse(Self, Client, Request, Response);
end;

function TIocpVariantClient.GetIdleConnectionAndLock: TIocpVariantClientConnection;
var
  Connection: TIocpSocketConnection;
begin
  try
    for Connection in LockConnectionList.Values do
    begin
      if Connection.IsIdle then
      begin
        Result := TIocpVariantClientConnection(Connection);
        Result.SetBusy(True);
        Exit;
      end;
    end;
  finally
    UnlockConnectionList;
  end;

  Result := TIocpVariantClientConnection(Connect(FServerAddr, FServerPort));
  if (Result <> nil) then
    Result.SetBusy(True);
end;

function TIocpVariantClient.ProcessRetry(var ErrCount: Integer): Boolean;
begin
  Inc(ErrCount);
  if (FTCPRetry > 0) then
    Result := (ErrCount > FTCPRetry)
  else if (FTCPRetry < 0) then
    Result := False
  else
    Result := True;
end;

function TIocpVariantClient.SendRequest(Client: TIocpVariantClientConnection; Request,
  Response: TIocpVariantPacket): Boolean;
begin
  with Client do
  begin
    SetBusy(True);
    FRequest.Assign(Request);
    Request.SaveToStream(FSendStream);
    InterlockedExchangePointer(Pointer(FSyncResponse), Response);
    Result := (Send(FSendStream.Memory, FSendStream.Size) > 0);
  end;
end;

procedure TIocpVariantClient.SetRetryDelay(const Value: Integer);
begin
  FRetryDelay := Min(Value, 500);
end;

function TIocpVariantClient.AsyncRequest(Request: TIocpVariantPacket): Boolean;
var
  ErrCount: Integer;
  Client: TIocpVariantClientConnection;
begin
  ErrCount := 0;
  while True do
  begin
    Client := GetIdleConnectionAndLock;
    if (Client = nil) then Break;

    Result := SendRequest(Client, Request, nil);

    // 请求发送成功
    if Result then Exit;

    // 出错，重试
    if ProcessRetry(ErrCount) then Exit
    else
    begin
      Client.SetBusy(False);
      Client.Disconnect;
      Sleep(FRetryDelay);
    end;
  end;

  // 请求发送失败
  Result := False;
end;

function TIocpVariantClient.SyncRequest(Request, Response: TIocpVariantPacket): Boolean;
var
  ErrCount: Integer;
  Client: TIocpVariantClientConnection;
begin
  ErrCount := 0;
  while True do
  begin
    Client := GetIdleConnectionAndLock;
    if (Client = nil) then Break;

    Result := SendRequest(Client, Request, Response);

    // 请求发送成功，并且接收返回数据成功
    if Result and (WaitForObject(Client.FBusyEvent, Timeout) = WAIT_OBJECT_0) then Exit;

    // 出错，重试
    if ProcessRetry(ErrCount) then Exit
    else
    begin
      Client.SetBusy(False);
      Client.Disconnect;
      Sleep(FRetryDelay);
    end;
  end;

  // 请求超时
  Result := False;
end;

function TIocpVariantClient.SyncRequest(
  Request: TIocpVariantPacket): TIocpVariantPacket;
begin
  Result := TIocpVariantPacket.Create;
  if not SyncRequest(Request, Result) then
    FreeAndNil(Result);
end;

procedure TIocpVariantClient.TriggerPacketRecv(Client: TIocpPacketConnection;
  const Packet: TIocpPacket);
var
  NeedNewResponse: Boolean;
  Response: TIocpVariantPacket;
begin
  with TIocpVariantClientConnection(Client) do
  begin
    NeedNewResponse := (InterlockedExchangePointer(Pointer(FSyncResponse), FSyncResponse) = nil);
    if NeedNewResponse then
      Response := TIocpVariantPacket.Create
    else
      Response := FSyncResponse;
    try
      Response.LoadFromBuf(Packet.Data, Packet.Header.DataSize);
      DoOnResponse(TIocpVariantClientConnection(Client), TIocpVariantClientConnection(Client).FRequest, Response);
    finally
      SetBusy(False);
      if NeedNewResponse then
        Response.Free;
    end;
  end;
end;

end.
