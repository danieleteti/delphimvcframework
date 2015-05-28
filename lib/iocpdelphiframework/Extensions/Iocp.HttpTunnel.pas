unit Iocp.HttpTunnel;

interface

uses
  Windows, Messages, Classes, SysUtils, SyncObjs, StrUtils, System.Generics.Collections, Iocp.Winsock2,
  IoUtils, Iocp.TcpSocket, Iocp.HttpServer, Iocp.Buffer, Iocp.HttpUtils, Iocp.Logger, uGlobalVars;

type
  TIocpHttpTunnelConnection = class;

  TIocpHttpAgentConnection = class(TIocpSocketConnection)
  protected
    // 客户端请求的连接
    HttpTunnelConnection: TIocpHttpTunnelConnection;
    TunnelLocker: TCriticalSection;
  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;
  end;

  TIocpHttpAgent = class(TIocpTcpSocket)
  private
    function NewConnect(HttpTunnelConnection: TIocpHttpTunnelConnection): Boolean;
    function DoForward(HttpTunnelConnection: TIocpHttpTunnelConnection): Boolean;
  protected
    procedure TriggerClientConnected(Client: TIocpSocketConnection); override;
    procedure TriggerClientRecvData(Client: TIocpSocketConnection; buf: Pointer; len: Integer); override;
    procedure TriggerClientDisconnected(Client: TIocpSocketConnection); override;
  public
    constructor Create(AOwner: TComponent); override;

    // Method: GET, POST
//    function Request(HttpTunnelConnection: TIocpHttpTunnelConnection; const ServerAddr: string; ServerPort: Word): Boolean;
  end;

  TIocpHttpTunnelConnection = class(TIocpHttpConnection)
  protected
    // 连接到目标服务器的连接
    AgentConnection: TIocpHttpAgentConnection;
    AgentLocker: TCriticalSection;
    DstHost: string;
    DstPort: Word;
    ForwardHeader: string;
  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;
  end;

  TConfirmForwardEvent = function(Sender: TObject; Client: TIocpHttpTunnelConnection; out ServerAddr: string; out ServerPort: Word): Boolean of object;
  TIocpHttpTunnel = class(TIocpHttpServer)
  private
    FHttpAgent: TIocpHttpAgent;
    FConfirmForward: TConfirmForwardEvent;
  protected
    procedure TriggerClientDisconnected(Client: TIocpSocketConnection); override;
    procedure DoOnRequest(Client: TIocpHttpConnection); override;
  protected
    // 重载这个方法决定是否转发当前请求
    // Result = True, 转发
    // Result = False, 不转发, 可以设置False, 然后自己返回自定义的页面数据
    function TriggerConfirmForward(Client: TIocpHttpTunnelConnection;
      out ServerAddr: string; out ServerPort: Word): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ConfirmForward: TConfirmForwardEvent read FConfirmForward write FConfirmForward;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Iocp', [TIocpHttpTunnel]);
end;

function PortStr(const Port: Word): string;
begin
  if (Port <> 80) then
    Result := ':' + IntToStr(Port)
  else
    Result := '';
end;

{ TIocpHttpAgent }

constructor TIocpHttpAgent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ConnectionClass := TIocpHttpAgentConnection;
end;

function TIocpHttpAgent.DoForward(
  HttpTunnelConnection: TIocpHttpTunnelConnection): Boolean;
var
  AppendHeader: string;
  HeaderLine: string;

  FilePath: string;
  Stream: TStreamWriter;
begin
  with HttpTunnelConnection do
  try
    AgentLocker.Enter;
    if (AgentConnection = nil) then Exit(False);

    // 生成请求头，并添加 X-Forwarded-For 信息，
    // 以便服务端能获知客户端的真实IP

    // 摘自维基百科
    // http://zh.wikipedia.org/wiki/X-Forwarded-For
    // 这一HTTP头一般格式如下:
    // X-Forwarded-For: client1, proxy1, proxy2
    // 其中的值通过一个 逗号+空格 把多个IP地址区分开, 最左边(client1)是最原始客户端的IP地址, 代理服务器每成功收到一个请求，就把请求来源IP地址添加到右边。 在上面这个例子中，这个请求成功通过了三台代理服务器：proxy1, proxy2 及 proxy3。请求由client1发出，到达了proxy3(proxy3可能是请求的终点)。请求刚从client1中发出时，XFF是空的，请求被发往proxy1；通过proxy1的时候，client1被添加到XFF中，之后请求被发往proxy2;通过proxy2的时候，proxy1被添加到XFF中，之后请求被发往proxy3；通过proxy3时，proxy2被添加到XFF中，之后请求的的去向不明，如果proxy3不是请求终点，请求会被继续转发。
    // 鉴于伪造这一字段非常容易，应该谨慎使用X-Forwarded-For字段。正常情况下XFF中最后一个IP地址是最后一个代理服务器的IP地址, 这通常是一个比较可靠的信息来源。
    ForwardHeader := RequestCmdLine + #13#10;
    for HeaderLine in RequestHeader do
    begin
      if (HeaderLine = '') or (StrLIComp(@HeaderLine[1], 'X-Forwarded-For:', 16) = 0) then Continue;
      {if (StrLIComp(@HeaderLine[1], 'Host:', 5) = 0) then
        ForwardHeader := ForwardHeader + 'Host: ' + HttpTunnelConnection.DstHost + PortStr(HttpTunnelConnection.DstPort) + #13#10
      else}
        ForwardHeader := ForwardHeader + HeaderLine + #13#10;
    end;
    if (XForwardedFor = '') then
      AppendHeader := 'X-Forwarded-For: ' +  PeerIP;
    ForwardHeader := ForwardHeader + AppendHeader + #13#10;
    ForwardHeader := ForwardHeader + #13#10;

    if (AgentConnection.Send(RawByteString(ForwardHeader)) < 0) then
    begin
      Answer506;
      Exit(False);
    end;

    if (RequestPostData.Size > 0) and (AgentConnection.Send(RequestPostData.DataString) < 0) then
    begin
      Answer506;
      Exit(False);
    end;

    //**
      FilePath := Format('%sForward\H=%s C=%s.txt', [gAppPath, RequestHostName, PeerIP]);
      ForceDirectories(ExtractFilePath(FilePath));
      Stream := TFile.AppendText(FilePath);
      Stream.WriteLine('Request: ->');
      Stream.BaseStream.WriteBuffer(RawRequestText.DataString[1], RawRequestText.Size);
      Stream.WriteLine('Request(ForwardHeader): ->');
      Stream.Write(ForwardHeader);
      Stream.Free;
    //**

    Result := True;
  finally
    AgentLocker.Leave;
  end;
end;

function TIocpHttpAgent.NewConnect(HttpTunnelConnection: TIocpHttpTunnelConnection): Boolean;
begin
  Result := (AsyncConnect(HttpTunnelConnection.DstHost, HttpTunnelConnection.DstPort, HttpTunnelConnection) <> INVALID_SOCKET);
end;

{function TIocpHttpAgent.Request(HttpTunnelConnection: TIocpHttpTunnelConnection; const ServerAddr: string; ServerPort: Word): Boolean;
begin
  if (HttpTunnelConnection = nil) or (HttpTunnelConnection.RequestHeader.Text = '') then Exit(False);

  try
    HttpTunnelConnection.AgentLocker.Enter;
    if (HttpTunnelConnection.AgentConnection = nil) or (HttpTunnelConnection.AgentConnection.IsClosed) then
    begin
      Result := NewConnect(HttpTunnelConnection);
    end else
    begin
      Result := DoForward(HttpTunnelConnection);
    end;
  finally
    HttpTunnelConnection.AgentLocker.Leave;
  end;
end;}

procedure TIocpHttpAgent.TriggerClientConnected(
  Client: TIocpSocketConnection);
var
  ReqConn: TIocpHttpTunnelConnection;
  AgConn: TIocpHttpAgentConnection;
begin
  ReqConn := Client.Tag;
  if (ReqConn = nil) then Exit;

  AgConn := TIocpHttpAgentConnection(Client);

  try
    ReqConn.AgentLocker.Enter;
    ReqConn.AgentConnection := AgConn;
    AgConn.HttpTunnelConnection := ReqConn;
  finally
    ReqConn.AgentLocker.Leave;
  end;
end;

procedure TIocpHttpAgent.TriggerClientDisconnected(
  Client: TIocpSocketConnection);
begin
  with TIocpHttpAgentConnection(Client) do
  try
    TunnelLocker.Enter;
    if (HttpTunnelConnection = nil) then Exit;
    HttpTunnelConnection.AgentLocker.Enter;
    HttpTunnelConnection.AgentConnection := nil;
    HttpTunnelConnection.AgentLocker.Leave;
    HttpTunnelConnection.Disconnect;
    HttpTunnelConnection := nil;
  finally
    TunnelLocker.Leave;
  end;
end;

procedure TIocpHttpAgent.TriggerClientRecvData(Client: TIocpSocketConnection;
  buf: Pointer; len: Integer);
var
  FilePath: string;
  Stream: TStreamWriter;
begin
  with TIocpHttpAgentConnection(Client) do
  try
    TunnelLocker.Enter;
    if (HttpTunnelConnection = nil) then Exit;
    HttpTunnelConnection.Send(buf, len);

    with HttpTunnelConnection do
    begin
      FilePath := Format('%sForward\H=%s C=%s.txt', [gAppPath, RequestHostName, PeerIP]);
      ForceDirectories(ExtractFilePath(FilePath));
      Stream := TFile.AppendText(FilePath);
      Stream.WriteLine('Response: ->');
      Stream.BaseStream.WriteBuffer(buf^, len);
      Stream.WriteLine;
      Stream.WriteLine;
      Stream.Free;
    end;
  finally
    TunnelLocker.Leave;
  end;
end;

{ TIocpHttpTunnel }

constructor TIocpHttpTunnel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ConnectionClass := TIocpHttpTunnelConnection;
  FHttpAgent := TIocpHttpAgent.Create(nil);
end;

destructor TIocpHttpTunnel.Destroy;
begin
  FreeAndNil(FHttpAgent);

  inherited Destroy;
end;

{procedure TIocpHttpTunnel.DoOnRequest(Client: TIocpHttpConnection);
var
  RequestForward: Boolean;
  ServerAddr: string;
  ServerPort: Word;
  Tunnel: TIocpHttpTunnelConnection;
  Agent: TIocpHttpAgentConnection;
begin
  Tunnel := TIocpHttpTunnelConnection(Client);
  try
    Tunnel.AgentLocker.Enter;
    Agent := Tunnel.AgentConnection;

    if (Agent = nil) then
    begin
      ServerAddr := '';
      ServerPort := 0;
      RequestForward := TriggerConfirmForward(TIocpHttpTunnelConnection(Client), ServerAddr, ServerPort);
      if not RequestForward then Exit;
    end else
    begin
      ServerAddr := Agent.PeerAddr;
      ServerPort := Agent.PeerPort;
    end;
  finally
    Tunnel.AgentLocker.Leave;
  end;

  if not FHttpAgent.Request(TIocpHttpTunnelConnection(Client), ServerAddr, ServerPort) then
  begin
    Client.Answer506;
    Exit;
  end;
end;}

procedure TIocpHttpTunnel.DoOnRequest(Client: TIocpHttpConnection);
var
  RequestForward: Boolean;
  HttpTunnelConnection: TIocpHttpTunnelConnection;
  Success: Boolean;
begin
  HttpTunnelConnection := TIocpHttpTunnelConnection(Client);
  with HttpTunnelConnection do
  try
    AgentLocker.Enter;

    if (AgentConnection = nil) or (AgentConnection.IsClosed) then
    begin
      // 获取转发的目标服务器地址及端口
      RequestForward := TriggerConfirmForward(HttpTunnelConnection, DstHost, DstPort);
      if not RequestForward then Exit;

      // 新建代理连接，并在代理连接建立后(FHttpAgent.TriggerClientConnected)开始转发
      Success := FHttpAgent.NewConnect(HttpTunnelConnection);
    end else
    begin
      // 已有代理连接，直接转发
      Success := FHttpAgent.DoForward(HttpTunnelConnection);
    end;

    if not Success then
    begin
AppendLog('ER - %s : %s, forward to %s:%d', [Client.PeerIP, Client.RequestCmdLine, DstHost, DstPort]);
      Client.Answer506;
      Exit;
    end;
AppendLog('OK - %s : %s, forward to %s:%d', [Client.PeerIP, Client.RequestCmdLine, DstHost, DstPort]);
  finally
    AgentLocker.Leave;
  end;
end;

function TIocpHttpTunnel.TriggerConfirmForward(Client: TIocpHttpTunnelConnection;
  out ServerAddr: string; out ServerPort: Word): Boolean;
begin
  if Assigned(FConfirmForward) then
    Result := FConfirmForward(Self, Client, ServerAddr, ServerPort)
  else
  begin
    Client.Answer503;
    Result := False;
  end;
end;

procedure TIocpHttpTunnel.TriggerClientDisconnected(
  Client: TIocpSocketConnection);
begin
  with TIocpHttpTunnelConnection(Client) do
  try
    AgentLocker.Enter;
    if (AgentConnection = nil) then Exit;
    AgentConnection.TunnelLocker.Enter;
    AgentConnection.HttpTunnelConnection := nil;
    AgentConnection.TunnelLocker.Leave;
    AgentConnection.Disconnect;
    AgentConnection := nil;
  finally
    AgentLocker.Leave;
  end;
end;

{ TIocpHttpTunnelConnection }

constructor TIocpHttpTunnelConnection.Create(AOwner: TObject);
begin
  inherited Create(AOwner);
  AgentLocker := TCriticalSection.Create;
end;

destructor TIocpHttpTunnelConnection.Destroy;
begin
  AgentLocker.Free;
  inherited Destroy;
end;

{ TIocpHttpAgentConnection }

constructor TIocpHttpAgentConnection.Create(AOwner: TObject);
begin
  inherited Create(AOwner);
  TunnelLocker := TCriticalSection.Create;
end;

destructor TIocpHttpAgentConnection.Destroy;
begin
  TunnelLocker.Free;
  inherited Destroy;
end;

end.
