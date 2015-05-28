unit Iocp.SimpleExt;

interface

uses
  Winapi.Windows, System.Classes, System.AnsiStrings, Iocp.Winsock2, Iocp.TcpSocket, Iocp.Buffer;

type
  TIocpLineSocketConnection = class(TIocpSocketConnection)
  private
    FLineText: TIocpStringStream;
  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;

    function Send(const s: RawByteString): Integer; reintroduce;

    property LineText: TIocpStringStream read FLineText;
  end;

  TIocpLineRecvEvent = procedure(Sender: TObject; Client: TIocpLineSocketConnection; Line: RawByteString) of object;
  TIocpLineSocket = class(TIocpTcpSocket)
  private
    FLineLimit: Integer;
    FLineEndTag: RawByteString;
    FOnRecvLine: TIocpLineRecvEvent;

    procedure SetLineEndTag(const Value: RawByteString);
  protected
    procedure TriggerClientRecvData(Client: TIocpSocketConnection; Buf: Pointer; Len: Integer); override;

    procedure ParseRecvData(Client: TIocpLineSocketConnection; Buf: Pointer; Len: Integer); virtual;

    // 重载这个方法，在里面处理接收到的文本行
    procedure DoOnRecvLine(Client: TIocpLineSocketConnection; Line: RawByteString); virtual;
  public
    constructor Create(AOwner: TComponent; IoThreadsNumber: Integer); override;

    function Connect(const RemoteAddr: string; RemotePort: Word; Tag: Pointer = nil; ConnectTimeout: DWORD = 10000): TIocpLineSocketConnection;
  published
    property LineEndTag: RawByteString read FLineEndTag write SetLineEndTag;
    property LineLimit: Integer read FLineLimit write FLineLimit default 65536;
    property OnRecvLine: TIocpLineRecvEvent read FOnRecvLine write FOnRecvLine;
  end;

  TIocpLineServer = class(TIocpLineSocket)
  private
    FAddr: string;
    FPort: Word;
    FListened: Boolean;
  public
    constructor Create(AOwner: TComponent; IoThreadsNumber: Integer); override;

    function Start: Boolean;
    function Stop: Boolean;
  published
    property Addr: string read FAddr write FAddr;
    property Port: Word read FPort write FPort;
  end;

  TSimpleIocpTcpClient = class(TIocpTcpSocket)
  private
    FServerPort: Word;
    FServerAddr: string;
  public
    function AsyncConnect(Tag: Pointer = nil): TSocket;
    function Connect(Tag: Pointer = nil; ConnectTimeout: DWORD = 10000): TIocpSocketConnection;
  published
    property ServerAddr: string read FServerAddr write FServerAddr;
    property ServerPort: Word read FServerPort write FServerPort;
  end;


implementation

{ TIocpLineSocketConnection }

constructor TIocpLineSocketConnection.Create(AOwner: TObject);
begin
  inherited Create(AOwner);

  FLineText := TIocpStringStream.Create('');
end;

destructor TIocpLineSocketConnection.Destroy;
begin
  FLineText.Free;

  inherited Destroy;
end;

function TIocpLineSocketConnection.Send(const s: RawByteString): Integer;
begin
  Result := inherited Send(s + TIocpLineSocket(Owner).LineEndTag);
end;

{ TIocpLineSocket }

function TIocpLineSocket.Connect(const RemoteAddr: string; RemotePort: Word;
  Tag: Pointer; ConnectTimeout: DWORD): TIocpLineSocketConnection;
begin
  Result := TIocpLineSocketConnection(inherited Connect(RemoteAddr, RemotePort, Tag, ConnectTimeout));
end;

constructor TIocpLineSocket.Create(AOwner: TComponent;
  IoThreadsNumber: Integer);
begin
  inherited Create(AOwner, IoThreadsNumber);

  ConnectionClass := TIocpLineSocketConnection;
  FLineEndTag := #13#10;
  FLineLimit := 65536;
end;

procedure TIocpLineSocket.DoOnRecvLine(Client: TIocpLineSocketConnection;
  Line: RawByteString);
begin
  if Assigned(FOnRecvLine) then
    FOnRecvLine(Self, Client, Line);
end;

procedure TIocpLineSocket.ParseRecvData(Client: TIocpLineSocketConnection;
  Buf: Pointer; Len: Integer);
var
  pch: PAnsiChar;
  Ch: AnsiChar;
  TagLen: Integer;
begin
  pch := Buf;
  TagLen := Length(FLineEndTag);
  while (Len > 0) do
  begin
    Ch := pch^;

    // 发现换行符
    if (TagLen > 0) and (Len >= TagLen) and (StrLIComp(pch, PAnsiChar(FLineEndTag), TagLen) = 0) then
    begin
      if (Client.LineText.Size > 0) then
      begin
        DoOnRecvLine(Client, Client.LineText.DataString);
        Client.LineText.Clear;
      end;
      Dec(Len, TagLen);
      Inc(pch, TagLen);
      Continue;
    end;

    Client.LineText.Write(Ch, 1);

    // 超出最大单行尺寸
    if (FLineLimit > 0) and (Client.LineText.Size >= FLineLimit) then
    begin
      DoOnRecvLine(Client, Client.LineText.DataString);
      Client.LineText.Clear;
    end;

    Dec(Len, SizeOf(Ch));
    Inc(pch);
  end;
end;

procedure TIocpLineSocket.SetLineEndTag(const Value: RawByteString);
begin
  if (Value <> '') then
    FLineEndTag := Value
  else
    FLineEndTag := #13#10;
end;

procedure TIocpLineSocket.TriggerClientRecvData(Client: TIocpSocketConnection;
  Buf: Pointer; Len: Integer);
begin
  ParseRecvData(TIocpLineSocketConnection(Client), Buf, Len);
end;

{ TIocpLineServer }

constructor TIocpLineServer.Create(AOwner: TComponent;
  IoThreadsNumber: Integer);
begin
  inherited Create(AOwner, IoThreadsNumber);

  FListened := False;
end;

function TIocpLineServer.Start: Boolean;
begin
  if FListened then Exit(True);

  StartupWorkers;
  FPort := inherited Listen(FAddr, FPort, 1);
  FListened := (FPort <> 0);
  Result := FListened;
end;

function TIocpLineServer.Stop: Boolean;
begin
  if not FListened then Exit(True);

  ShutdownWorkers;
  FListened := False;
  Result := True;
end;

{ TSimpleIocpTcpClient }

function TSimpleIocpTcpClient.AsyncConnect(Tag: Pointer): TSocket;
begin
  Result := inherited AsyncConnect(FServerAddr, FServerPort, Tag);
end;

function TSimpleIocpTcpClient.Connect(Tag: Pointer;
  ConnectTimeout: DWORD): TIocpSocketConnection;
begin
  Result := inherited Connect(FServerAddr, FServerPort, Tag, ConnectTimeout);
end;

end.
