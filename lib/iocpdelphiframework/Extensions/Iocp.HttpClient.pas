unit Iocp.HttpClient;

interface

uses
  Windows, Messages, Classes, SysUtils, StrUtils, SyncObjs, Math, IoUtils,
  Iocp.TcpSocket, Iocp.ThreadPool, Iocp.HttpUtils, Iocp.Buffer,
  Iocp.Utils, Iocp.Logger;

const
  IOCP_HTTP_CLIENT_VERSION = 'IocpHttpClient/1.0';

type
  TIocpHttpCliConnectionState = (hcHeader, hcBody);
  
  TIocpHttpCliConnection = class(TIocpSocketConnection)
  private
    FState: TIocpHttpCliConnectionState;
    FResponseEvent, FRequestEvent: THandle;
  protected
    FRawResponseHeader: TIocpStringStream;
    FRawResponseBody: TIocpStringStream;
    FResponseHeader: TStringList;
    FResponseHasContentLength: Boolean;
    FResponseContentLength: Int64;

    procedure ParseResponseData; virtual;
    procedure Reset;
  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;

    function IsIdle: Boolean;

    property RawResponseHeader: TIocpStringStream read FRawResponseHeader;
    property RawResponseBody: TIocpStringStream read FRawResponseBody;
  end;

  TIocpHttpClient = class(TIocpTcpSocket)
  private
    FServerPort: Word;
    FServerAddr: string;
    FTimeout: Cardinal;

    function GetIdleConnection: TIocpHttpCliConnection;
  protected
    procedure TriggerClientRecvData(Client: TIocpSocketConnection; buf: Pointer; len: Integer); override;
    procedure TriggerClientDisconnected(Client: TIocpSocketConnection); override;

    procedure ParseRecvData(Client: TIocpHttpCliConnection; buf: Pointer; len: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ServerAddr: string read FServerAddr write FServerAddr;
    property ServerPort: Word read FServerPort write FServerPort;
    property Timeout: Cardinal read FTimeout write FTimeout;

    // Method: GET, POST
    function Request(const Method, PathAndParams, Header, Data: string; out RecvHeader, RecvBody: RawByteString): Boolean;
  end;

implementation

{ TIocpHttpClient }

constructor TIocpHttpClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ConnectionClass := TIocpHttpCliConnection;
end;

destructor TIocpHttpClient.Destroy;
begin

  inherited Destroy;
end;

function TIocpHttpClient.GetIdleConnection: TIocpHttpCliConnection;
var
  ClientList: TIocpSocketConnectionDictionary;
  Client: TIocpSocketConnection;
begin
  ClientList := LockConnectionList;
  try
    for Client in ClientList.Values do
    begin
      if (TIocpHttpCliConnection(Client).IsIdle) then
      begin
      AppendLog('找到空闲Socket %d', [Client.Socket]);
        ResetEvent(TIocpHttpCliConnection(Client).FResponseEvent);
        Result := TIocpHttpCliConnection(Client);
        Exit;
      end;
    end;
  finally
    UnlockConnectionList;
  end;

  Result := TIocpHttpCliConnection(Connect(FServerAddr, FServerPort, nil, FTimeout));
  if (Result <> nil) then
  begin
    AppendLog('新建Socket %d', [Result.Socket]);
    ResetEvent(Result.FResponseEvent);
  end;
end;

procedure TIocpHttpClient.ParseRecvData(Client: TIocpHttpCliConnection;
  buf: Pointer; len: Integer);
var
  ResponseReady: Boolean;
  pch: PAnsiChar;
  ch: AnsiChar;
  CR, LF: Integer;
//  tmps: string;
begin
  ResponseReady := False;
  if (Client.FState = hcHeader) then
  begin
  AppendLog('ParseRecvData - Client.Reset');
    Client.Reset;
  end;

  // 在这里解析客户端浏览器发送过来的请求数据
  pch := buf;
  CR := 0;
  LF := 0;
  while (len > 0) do
  begin
    ch := pch^;

    if (ch = #13) then
      Inc(CR)
    else if (ch = #10) then
      Inc(LF)
    else
    begin
      CR := 0;
      LF := 0;
    end;

    if (Client.FState = hcHeader) then
    begin
      Client.RawResponseHeader.Write(ch, 1);
//AppendLog('Recv header %d bytes', [Client.RawResponseHeader.Size]);
      if (CR = 2) and (LF = 2) then
      begin
        Client.ParseResponseData;
//        AppendLog('Response Header, ResponseContentLength=%d, HeaderText=%s', [Client.FResponseContentLength, Client.RawResponseHeader.DataString]);
        if Client.FResponseHasContentLength and (Client.FResponseContentLength <= 0) then
        begin
          ResponseReady := True;
          Break;
        end;
        Client.FState := hcBody;
      end;

      Dec(len);
      Inc(pch);
    end else
    if (Client.FState = hcBody) then
    begin
//    SetLength(tmps, len);
//    Move(pch^, tmps[1], len);
//    AppendLog('Response BlockSize=%d,BlockText=%s', [len, tmps]);
      Client.RawResponseBody.Write(pch^, len);
//AppendLog('Recv body %d / %d bytes', [Client.RawResponseBody.Size, Client.FResponseContentLength]);
      if (Client.FResponseContentLength > 0) and (Client.RawResponseBody.Size >= Client.FResponseContentLength) then
      begin
AppendLog('RawResponseBody.Size=%d, Client.FResponseContentLength=%d', [Client.RawResponseBody.Size, Client.FResponseContentLength]);
        ResponseReady := True;
        Client.FState := hcHeader;
      end;
      Break;
    end;
  end;

AppendLog('Recv header %d bytes, body  %d bytes', [Client.RawResponseHeader.Size, Client.RawResponseBody.Size]);
  if (ResponseReady) then
  begin
//  AppendLog('Response done.');
    SetEvent(Client.FResponseEvent);
  end;
end;

function TIocpHttpClient.Request(const Method, PathAndParams, Header,
  Data: string; out RecvHeader, RecvBody: RawByteString): Boolean;

  function PortStr(const Port: Word): string;
  begin
    if (Port <> 80) then
      Result := ':' + IntToStr(Port)
    else
      Result := '';
  end;

var
  Conn: TIocpHttpCliConnection;
  Content: TIocpStringStream;
  n, x: Integer;
begin
  Result := False;

  Content := TIocpStringStream.Create('');
  try
    Content.WriteString(UpperCase(Method) + ' ' + PathAndParams + ' HTTP/1.1'#13#10);

    if (Header <> '') then
    begin
      Content.WriteString(FixHeader(Header));
    end else
    begin
      Content.WriteString(
        'User-Agent: ' + IOCP_HTTP_CLIENT_VERSION + #13#10 +
        'Host: ' + FServerAddr + PortStr(FServerPort) + #13#10 +
        'Accept: */*'#13#10);
      if (Data <> '') then
        Content.WriteString(Format('Content-Length: %d'#13#10, [Length(Data)]));
      Content.WriteString(#13#10);
    end;

    if (Data <> '') then
      Content.WriteString(Data);

    // 最多重试 10 次
    for x := 1 to 10 do
    begin
      Conn := GetIdleConnection;
      if (Conn = nil) then Exit;

AppendLog('Socket %d Request %s %s <%s>', [Conn.Socket, Method, PathAndParams, Data]);
AppendLog('Socket %d Request Header: %s', [Conn.Socket, Content.DataString]);

      //Conn.ReadLock;

      ResetEvent(Conn.FResponseEvent);
      ResetEvent(Conn.FRequestEvent);
      AppendLog('Request Socket %d Sendding', [Conn.Socket]);
      n := Conn.Send(Content.DataString);
      AppendLog('Request Socket %d Send %d / %d Bytes', [Conn.Socket, n, Content.Size]);
      if (n < 0) then
      begin
        AppendLog('Socket %d 发送数据失败,重新分配Socket', [Conn.Socket]);
        SetEvent(Conn.FRequestEvent);
        //Conn.ReadUnlock;
        Conn.Disconnect;
        Sleep(100);
        Continue;
      end;

      if (Timeout <= 0) then
        Timeout := INFINITE;

      AppendLog('Socket %d 等待返回数据...', [Conn.Socket]);

      if (WaitForSingleObject(Conn.FResponseEvent, Timeout) = WAIT_OBJECT_0) then
      begin
        AppendLog('Socket %d HEADER size=%d', [Conn.Socket, Conn.RawResponseHeader.Size]);
        AppendLog('Socket %d BODY size=%d', [Conn.Socket, Conn.RawResponseBody.Size]);
        RecvHeader := Conn.RawResponseHeader.DataString;
        RecvBody := Conn.RawResponseBody.DataString;
        Result := (RecvHeader <> '');
      end;

      Conn.Reset;
      SetEvent(Conn.FRequestEvent);
      //Conn.ReadUnlock;
      Break;
    end;
  finally
    Content.Free;
  end;
end;

procedure TIocpHttpClient.TriggerClientDisconnected(
  Client: TIocpSocketConnection);
begin
  with TIocpHttpCliConnection(Client) do
  begin
    SetEvent(FResponseEvent);

    // 等待数据被取走再断开连接
    AppendLog('Socket %d Waitting request event', [Client.Socket]);
    WaitForSingleObject(TIocpHttpCliConnection(Client).FRequestEvent, INFINITE);
    AppendLog('Socket %d Disconnect ok', [Client.Socket]);
  end;
end;

procedure TIocpHttpClient.TriggerClientRecvData(Client: TIocpSocketConnection;
  buf: Pointer; len: Integer);
begin
  ParseRecvData(TIocpHttpCliConnection(Client), buf, len);
end;

{ TIocpHttpCliConnection }

constructor TIocpHttpCliConnection.Create(AOwner: TObject);
begin
  inherited Create(AOwner);

  FRawResponseHeader := TIocpStringStream.Create('');
  FRawResponseBody := TIocpStringStream.Create('');
  FResponseHeader := TStringList.Create;
  FResponseEvent := CreateEvent(nil, True, False, nil);
  FRequestEvent := CreateEvent(nil, True, True, nil);
  FState := hcHeader;
end;

destructor TIocpHttpCliConnection.Destroy;
begin
  FRawResponseHeader.Free;
  FRawResponseBody.Free;
  FResponseHeader.Free;
  SetEvent(FResponseEvent);
  CloseHandle(FResponseEvent);
  SetEvent(FRequestEvent);
  CloseHandle(FRequestEvent);

  inherited Destroy;
end;

function TIocpHttpCliConnection.IsIdle: Boolean;
begin
  Result := not IsClosed and (WaitForSingleObject(FRequestEvent, 0) = WAIT_OBJECT_0) and (WaitForSingleObject(FResponseEvent, 0) = WAIT_OBJECT_0);
end;

procedure TIocpHttpCliConnection.ParseResponseData;
var
  I, J: Integer;
  ResponseLine, Field, Value: string;
begin
  TFile.WriteAllBytes('c:\1.txt', BytesOf(FRawResponseHeader.DataString));
  FResponseHeader.Text := string(FRawResponseHeader.DataString);

  FResponseHasContentLength := False;
  FResponseContentLength := -1;
  for I := 0 to FResponseHeader.Count - 1 do
  begin
    ResponseLine := FResponseHeader[I];
    J := Pos(':', ResponseLine);
    if (J <= 0) then Continue;

    Field := Copy(ResponseLine, 1, J - 1);
    Value := Copy(ResponseLine, J + 1, Length(ResponseLine));

    AppendLog('Header, %s = %s', [Field, Value]);
    if SameText(Field, 'content-length') then
    begin
      FResponseHasContentLength := True;
      FResponseContentLength := StrToInt64Def(Value, -1);
      AppendLog('content-length = %d', [FResponseContentLength]);
    end;
  end;
end;

procedure TIocpHttpCliConnection.Reset;
begin
  FRawResponseHeader.Size := 0;
  FRawResponseBody.Size := 0;
  FState := hcHeader;
end;

end.
