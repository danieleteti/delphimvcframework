{*******************************************************}
{                                                       }
{     Delphi DataSnap Framework Iocp Tcp Transport      }
{                                                       }
{                 soulawing@gmail.com                   }
{                                                       }
{*******************************************************}

unit Iocp.DSTCPServerTransport;

interface

uses
  System.Classes,
  Data.DBXTransport,
  Datasnap.DSAuth,
  Datasnap.DSCommonServer, Data.DBXTransportFilter,
  Datasnap.DSTransport, Data.DBXCommon, Data.DBXClientResStrs, Data.DBXMessageHandlerCommon,
  Datasnap.DSSession, Data.DBXMessageHandlerJSonServer,
  System.SysUtils, System.Math, Iocp.TcpSocket, Iocp.SimpleServer;

type
  TIocpDataSnapTransport = class;

  { TIocpDatasnapConnection }

  TIocpDatasnapConnection = class(TIocpSocketConnection)
  private
    FData: Pointer;
    FDataSize, FPosition: Integer;
    FProtocolHandler: TDBXProtocolHandler;
  public
    function Read(var Buf; Size: Integer): Integer;
  end;

  { TIocpDatasnapServer }

  TIocpDatasnapServer = class(TSimpleIocpTcpServer)
  private
    FTransport: TIocpDataSnapTransport;
  protected
    procedure TriggerClientConnected(Client: TIocpSocketConnection); override;
    procedure TriggerClientRecvData(Client: TIocpSocketConnection; Buf: Pointer; Len: Integer); override;
    procedure TriggerClientDisconnected(Client: TIocpSocketConnection); override;
  public
    constructor Create(Transport: TIocpDataSnapTransport); overload;
  published
  end;

  { TIocpDBXChannel }

  TIocpDBXChannel = class(TDBXChannel)
  private
    FContext: TIocpDatasnapConnection;
    FChannelInfo: TDBXSocketChannelInfo;
  protected
    function GetChannelInfo: TDBXChannelInfo; override;
  public
    constructor Create(AContext: TIocpDatasnapConnection);
    destructor Destroy; override;

    procedure Open; override;
    procedure Close; override;

    function Read(const Buffer: TArray<Byte>; const Offset: Integer;
      const Count: Integer): Integer; override;
    function Write(const Buffer: TArray<Byte>; const Offset: Integer;
      const Count: Integer): Integer; override;
  end;

  { TIocpDataSnapTransport }

  TIocpDataSnapTransport = class(TDSServerTransport)
  strict private
    FAuthenticationManager: TDSCustomAuthenticationManager;
    FProtocolHandlerFactory: TDSJSONProtocolHandlerFactory;
    FTCPServer: TIocpDatasnapServer;
  private
    function GetPort: Word;
    procedure SetPort(const Value: Word);
  protected
    procedure SetAuthenticationManager(const AuthManager: TDSCustomAuthenticationManager); virtual;
    { TDSServerTransport }
    procedure SetServer(const AServer: TDSCustomServer); override;
    { TComponent }
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { TDSServerComponent }
    procedure Start; override;
    procedure Stop; override;

    property ProtocolHandlerFactory: TDSJSONProtocolHandlerFactory read FProtocolHandlerFactory;
  published
    property Server;
    property BufferKBSize;
    property Filters;

    property AuthenticationManager: TDSCustomAuthenticationManager read FAuthenticationManager write SetAuthenticationManager;
    property Port: Word read GetPort write SetPort;
    property TCPServer: TIocpDatasnapServer read FTCPServer;
  end;

  procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Iocp', [TIocpDataSnapTransport]);
end;

{ TIocpDataSnapTransport }

constructor TIocpDataSnapTransport.Create(AOwner: TComponent);
begin
  inherited;

  FProtocolHandlerFactory := TDSJSONProtocolHandlerFactory.Create(Self);

  FTCPServer := TIocpDatasnapServer.Create(Self);
  FTCPServer.Name := 'TCPServer';

  FTCPServer.Port := 211;
  FTCPServer.SetSubComponent(True);

  FreeNotification(FTCPServer);
end;

destructor TIocpDataSnapTransport.Destroy;
begin
  //Stop;
  FreeAndNil(FProtocolHandlerFactory);

  inherited;
end;

function TIocpDataSnapTransport.GetPort: Word;
begin
  Result := FTCPServer.Port;
end;

procedure TIocpDataSnapTransport.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
  begin
    if (AComponent = FAuthenticationManager) then FAuthenticationManager := nil
    else if (AComponent = Server) then Server := nil
    else if (AComponent = FTCPServer) then FTCPServer := nil;
  end;
end;

procedure TIocpDataSnapTransport.SetAuthenticationManager(
  const AuthManager: TDSCustomAuthenticationManager);
begin
  if Assigned(FAuthenticationManager) then
    FAuthenticationManager.RemoveFreeNotification(Self);

  FAuthenticationManager := AuthManager;

  if Assigned(FAuthenticationManager) then
    FAuthenticationManager.FreeNotification(Self);
end;

procedure TIocpDataSnapTransport.SetPort(const Value: Word);
begin
  FTCPServer.Port := Value;
end;

procedure TIocpDataSnapTransport.SetServer(const AServer: TDSCustomServer);
begin
  if (AServer <> Server) then
  begin
    if Assigned(Server) then RemoveFreeNotification(Server);
    if Assigned(AServer) then
    begin
      FreeNotification(AServer);
      DBXContext := AServer.DBXContext;
    end;
  end;

  inherited SetServer(AServer);
end;

procedure TIocpDataSnapTransport.Start;
begin
  inherited;

  if Assigned(FTCPServer) then FTCPServer.Start;
end;

procedure TIocpDataSnapTransport.Stop;
begin
  if Assigned(FTCPServer) then FTCPServer.Stop;

  inherited;
end;

{ TIocpDatasnapServer }

constructor TIocpDatasnapServer.Create(Transport: TIocpDataSnapTransport);
begin
  inherited Create(Transport);

  ConnectionClass := TIocpDatasnapConnection;
  FTransport := Transport;
end;

procedure TIocpDatasnapServer.TriggerClientConnected(
  Client: TIocpSocketConnection);
var
  LDBXChannel: TIocpDBXChannel;
  LFilterChannel: TDBXFilterSocketChannel;
begin
  LDBXChannel := TIocpDBXChannel.Create(TIocpDatasnapConnection(Client));
  LDBXChannel.Open;

  LFilterChannel := TDBXFilterSocketChannel.Create(FTransport.Filters);
  // set the delegate
  LFilterChannel.Channel := LDBXChannel;

  with TIocpDatasnapConnection(Client) do
  begin
    FProtocolHandler := FTransport.ProtocolHandlerFactory.CreateProtocolHandler(LFilterChannel);

    if (FProtocolHandler is TDBXJSonServerProtocolHandler) and
      (TDBXJSonServerProtocolHandler(FProtocolHandler).DSSession = nil) then
    begin
      TDBXJSonServerProtocolHandler(FProtocolHandler).DSSession :=
        TDSSessionManager.Instance.CreateSession<TDSTCPSession>(
          function: TDSSession
          begin
            Result := TDSTCPSession.Create(FTransport.AuthenticationManager);
            Result.PutData('CommunicationProtocol', 'tcp/ip');
            Result.PutData('RemoteIP', Client.PeerIP);
          end,
          ''
        );
    end;

    TDBXProtocolHandler(FProtocolHandler).SetUp(FTransport.AuthenticationManager);
    Assert(TDSSessionManager.GetThreadSession <> nil);
    TDSSessionManager.GetThreadSession.ObjectCreator := Self;
  end;
end;

procedure TIocpDatasnapServer.TriggerClientDisconnected(
  Client: TIocpSocketConnection);
begin
  with TIocpDatasnapConnection(Client) do
  begin
    TDSSessionManager.ClearThreadSession;
    if Assigned(FProtocolHandler) then
      FreeAndNil(FProtocolHandler);
  end;
end;

procedure TIocpDatasnapServer.TriggerClientRecvData(
  Client: TIocpSocketConnection; Buf: Pointer; Len: Integer);
begin
  with TIocpDatasnapConnection(Client) do
  begin
    FData := Buf;
    FDataSize := Len;
    FPosition := 0;

    if (Len > 0) then
    begin
      try
        {
          Data.DBXMessageHandlerJSonServer.pas

          HandleNonBlockingProtocol 内部调用：
          FDbxStreamReader.ReadMethod -> HandleDbxRequest -> ...

          ReadMethod 内部调用：
          FillBuffer -> ...

          FillBuffer 实际就是调用 TDBXChannel.Read 读取数据，当没有数据时就触发异常
          这个设计极其脑残，这里只需要截获异常，让程序继续运行下去，下次数据到来时
          再度调用 HandleNonBlockingProtocol 就能完成以 IOCP 方式处理 Datasnap 请求了
        }
        TDBXProtocolHandler(FProtocolHandler).HandleNonBlockingProtocol;
      except
      end;
    end;
  end;
end;

{ TIocpDatasnapConnection }

function TIocpDatasnapConnection.Read(var Buf; Size: Integer): Integer;
var
  P: PByte;
begin
  Result := Min(Size, FDataSize - FPosition);
  if (Result <= 0) then Exit;
  
  P := FData;
  Inc(P, FPosition);
  Inc(FPosition, Result);
  Move(P^, Buf, Result);
end;

{ TIocpDBXChannel }

procedure TIocpDBXChannel.Close;
begin
  inherited;

  if Assigned(FContext) then FContext.Disconnect;
  if Assigned(FChannelInfo) then FreeAndNil(FChannelInfo);
end;

constructor TIocpDBXChannel.Create(AContext: TIocpDatasnapConnection);
begin
  inherited Create;

  FContext := AContext;
end;

destructor TIocpDBXChannel.Destroy;
begin
  Close;

  inherited;
end;

function TIocpDBXChannel.GetChannelInfo: TDBXChannelInfo;
begin
  Result := FChannelInfo;
end;

procedure TIocpDBXChannel.Open;
begin
  inherited;

  FreeAndNil(FChannelInfo);
  FChannelInfo := TDBXSocketChannelInfo.Create(NativeInt(FContext), FContext.PeerIP);
end;

function TIocpDBXChannel.Read(const Buffer: TArray<Byte>; const Offset,
  Count: Integer): Integer;
begin
  Result := FContext.Read(Buffer[Offset], Count - Offset);
end;

function TIocpDBXChannel.Write(const Buffer: TArray<Byte>; const Offset,
  Count: Integer): Integer;
begin
  Result := FContext.Send(Buffer[Offset], Count - Offset);
end;

end.
