unit Iocp.SSLSocket;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs, Iocp.TcpSocket, Iocp.OpenSSL;

type
  TIocpSSLConnection = class(TIocpSocketConnection)
  private
    FSsl: PSSL;
    FRecvBIO, FSendBIO: PBIO;
    FBufSize: Integer;
    FRecvSslBuffer, FSendSslBuffer: Pointer;
    FSslHandshaking: Boolean;
    FLocker: TCriticalSection;

    procedure Lock;
    procedure Unlock;

    procedure SSLHandleshaking;
  protected
    function PostWrite(const Buf: Pointer; Size: Integer): Boolean; override;

    procedure TriggerConnected; override;
    procedure TriggerRecvData(Buf: Pointer; Len: Integer); override;
    procedure TriggerSentData(Buf: Pointer; Len: Integer); override;
  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;
  end;

  TIocpSSLSocket = class(TIocpTcpSocket)
  private
    FSslCtx: PSSL_CTX;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetCert(CertPKey: Pointer; CertPKeySize: Integer); overload;
    procedure SetCert(Cert: Pointer; CertSize: Integer; PKey: Pointer; PKeySize: Integer); overload;
    procedure SetCert(const CertPKeyFile: string); overload;
    procedure SetCert(const CertFile, PKeyFile: string); overload;
  end;

implementation

uses
  System.IOUtils;

{ TIocpSSLConnection }

constructor TIocpSSLConnection.Create(AOwner: TObject);
begin
  inherited Create(AOwner);

  FLocker := TCriticalSection.Create;
  FBufSize := Owner.IoCachePool.BlockSize;
  FRecvSslBuffer := Owner.IoCachePool.GetMemory(False);
  FSendSslBuffer := Owner.IoCachePool.GetMemory(False);

  FSsl := SSL_new(TIocpSSLSocket(Owner).FSslCtx);
  FRecvBIO := BIO_new(BIO_s_mem());
  FSendBIO := BIO_new(BIO_s_mem());
  SSL_set_bio(FSsl, FRecvBIO, FSendBIO);
end;

destructor TIocpSSLConnection.Destroy;
begin
  SSL_shutdown(FSsl);
  SSL_free(FSsl);
  Owner.IoCachePool.FreeMemory(FRecvSslBuffer);
  Owner.IoCachePool.FreeMemory(FSendSslBuffer);
  FLocker.Free;

  inherited Destroy;
end;

procedure TIocpSSLConnection.Lock;
begin
  FLocker.Enter;
end;

procedure TIocpSSLConnection.Unlock;
begin
  FLocker.Leave;
end;

procedure TIocpSSLConnection.SSLHandleshaking;
var
  bytes, error: Integer;
begin
  // 是否需要处理握手数据
  if (BIO_pending(FSendBIO) <> 0) then
  begin
    bytes := BIO_read(FSendBIO, FSendSslBuffer, FBufSize);
    if (bytes <= 0) then
    begin
      error := SSL_get_error(FSsl, bytes);
      if ssl_is_fatal_error(error) then
        Disconnect;
      Exit;
    end;
    inherited PostWrite(FSendSslBuffer, bytes);
  end;

  // 握手完成
  if FSslHandshaking and SSL_is_init_finished(FSsl) then
  begin
    FSslHandshaking := False;
    inherited TriggerConnected;
  end;
end;

function TIocpSSLConnection.PostWrite(const Buf: Pointer;
  Size: Integer): Boolean;
var
  bytes, error: Integer;
begin
  Result := False;
  Lock;
  try
    // 将待发送数据加密
    bytes := SSL_write(FSsl, Buf, Size);
    if (bytes <> Size) then
    begin
      error := SSL_get_error(FSsl, bytes);
      if ssl_is_fatal_error(error) then
      begin
        Disconnect;
        Exit;
      end;
    end;

    // 从 BIO 读取加密后的数据发送
    while (BIO_pending(FSendBIO) <> 0) do
    begin
      bytes := BIO_read(FSendBIO, FSendSslBuffer, FBufSize);
      if (bytes <= 0) then
      begin
        error := SSL_get_error(FSsl, bytes);
        if ssl_is_fatal_error(error) then
          Disconnect;
        Break;
      end;

      Result := inherited PostWrite(FSendSslBuffer, bytes);
      if not Result then Break;
    end;
  finally
    Unlock;
  end;
end;

procedure TIocpSSLConnection.TriggerConnected;
var
  bytes, error: Integer;
begin
  FSslHandshaking := True;
  Lock;
  try
    // 连接建立后调用 SSL_read 开始SSL握手
    if (ConnectionSource = csAccept) then
    begin
      SSL_set_accept_state(FSsl);
      bytes := SSL_read(FSsl, FRecvSslBuffer, FBufSize);
    end else
    begin
      SSL_set_connect_state(FSsl);
      bytes := SSL_read(FSsl, FRecvSslBuffer, FBufSize);
    end;

    if (bytes <= 0) then
    begin
      error := SSL_get_error(FSsl, bytes);
      if ssl_is_fatal_error(error) then
      begin
        Disconnect;
        Exit;
      end;
    end;

    SSLHandleshaking;
  finally
    Unlock;
  end;
end;

procedure TIocpSSLConnection.TriggerRecvData(Buf: Pointer; Len: Integer);
var
  error, bytes: Integer;
begin
  Lock;
  try
    // 将收到的加密数据写入 BIO
    // 这里收到的数据既有可能是握手数据也有可能是实际数据
    while True do
    begin
      bytes := BIO_write(FRecvBIO, Buf, Len);
      if (bytes > 0) then Break;

      if not BIO_should_retry(FRecvBIO) then
      begin
        Disconnect;
        Exit;
      end;
    end;

    // 读取解密后的数据
    // 在这里，实际数据能读取成功，
    // 握手数据必然读取失败，从而继续触发下一步握手
    while True do
    begin
      bytes := SSL_read(FSsl, FRecvSslBuffer, FBufSize);
      if (bytes > 0) then
        // 收到实际数据
        inherited TriggerRecvData(FRecvSslBuffer, bytes)
      else
      begin
        error := SSL_get_error(FSsl, bytes);
        if ssl_is_fatal_error(error) then
        begin
          Disconnect;
          Exit;
        end;

        Break;
      end;
    end;

    SSLHandleshaking;
  finally
    Unlock;
  end;
end;

procedure TIocpSSLConnection.TriggerSentData(Buf: Pointer; Len: Integer);
begin
  if not FSslHandshaking then
    inherited TriggerSentData(Buf, Len);
end;

{ TIocpSSLSocket }

constructor TIocpSSLSocket.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ConnectionClass := TIocpSSLConnection;

  TSSLTools.LoadSSL;
  FSslCtx := TSSLTools.NewCTX;
end;

destructor TIocpSSLSocket.Destroy;
begin
  inherited Destroy;

  TSSLTools.FreeCTX(FSslCtx);
  TSSLTools.UnloadSSL;
end;

procedure TIocpSSLSocket.SetCert(CertPKey: Pointer; CertPKeySize: Integer);
begin
  TSSLTools.SetCert(FSslCtx, CertPKey, CertPKeySize);
end;

procedure TIocpSSLSocket.SetCert(Cert: Pointer; CertSize: Integer;
  PKey: Pointer; PKeySize: Integer);
begin
  TSSLTools.SetCert(FSslCtx, Cert, CertSize, PKey, PKeySize);
end;

procedure TIocpSSLSocket.SetCert(const CertPKeyFile: string);
begin
  TSSLTools.SetCert(FSslCtx, CertPKeyFile);
end;

procedure TIocpSSLSocket.SetCert(const CertFile, PKeyFile: string);
begin
  TSSLTools.SetCert(FSslCtx, CertFile, PKeyFile);
end;

end.
