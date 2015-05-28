unit Iocp.HttpServer;

{$i Iocp.Ext.inc}

interface

uses
  Windows, Messages, Classes, SysUtils, StrUtils, SyncObjs, Math, IoUtils,
  System.Generics.Collections, System.RegularExpressions, System.Masks,
  Iocp.TcpSocket, Iocp.SimpleServer, Iocp.ThreadPool, Iocp.HttpUtils, Iocp.Buffer, Iocp.Logger
  {$ifdef __IOCP_SSL__},Iocp.SSLSocket{$endif};

const
  IOCP_HTTP_SERVER_VERSION  = 'IocpHttpServer/1.0';
  IOCP_HTTP_INIT_ACCEPT_NUM = 64;

type
  TIocpHttpServer = class;

  TIocpHttpConnectionState = (hcRequest, hcPostData, hcDone);
  TIocpHttpConnection = class({$ifdef __IOCP_SSL__}TIocpSSLConnection{$else}TIocpSocketConnection{$endif})
  private
    FHttpState: TIocpHttpConnectionState;
    FResponseSize, FResponseSent: Integer;
  protected
    FRawRequestText: TIocpStringStream;
    FMethod, FPath, FParams, FPathAndParams, FVersion: string;
    FRawPath, FRawParams, FRawPathAndParams: string;
    FHttpVerNum: Integer;
    FKeepAlive: Boolean;

    FRequestCmdLine: string;
    FRequestHeader: TStringList;
    FRequestContentType: string;
    FRequestHasContentLength: Boolean;
    FRequestContentLength: Int64;
    FRequestAccept: string;
    FRequestReferer: string;
    FRequestAcceptLanguage: string;
    FRequestAcceptEncoding: string;
    FRequestUserAgent: string;
    FRequestAuth: string;
    FRequestCookies: string;
    FRequestHost: string;
    FRequestHostName: string;
    FRequestHostPort: string;
    FRequestConnection: string;
    FXForwardedFor: string;
    FRequestPostData: TIocpStringStream;

    FAcceptPostData: Boolean;
    FPostDataSize: Integer;

    // 如果要更进一步解析HTTP请求头的数据，可以修改这个函数
    function ParseRequestData: Boolean; virtual;
    function MakeHeader(const Status, ContType, Header: string; ContSize: Integer): string;
  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;

    // 所有AnswerXXXX的基础函数
    function AnswerBuf(const Header: string; Buf: Pointer; Size: Integer): Boolean; overload;
    function AnswerStream(const Header: string; Stream: TStream): Boolean; overload;
    function AnswerStream(const Status, ContType, Header: string; Stream: TStream): Boolean; overload;

    // 以字节数组方式返回请求
    function AnswerBytes(const Status, ContType, Header: string; Data: TBytes): Boolean; overload;
    function AnswerBytes(const Header: string; Data: TBytes): Boolean; overload;

    // 以字符串方式返回请求
    function AnswerHTML(const Status, ContType, Header: string; HTML: RawByteString): Boolean; overload;
    function AnswerHTML(const Status, ContType, Header, HTML: string): Boolean; overload;
    function AnswerHTML(const Header: string; HTML: RawByteString): Boolean; overload;
    function AnswerHTML(const Header, HTML: string): Boolean; overload;

    // 以文件内容方式返回请求
    function AnswerDocument(const FileName, ContentType: string): Boolean; overload;
    function AnswerDocument(const FileName: string): Boolean; overload;

    // 返回错误信息
    procedure Answer400;
    procedure Answer401;
    procedure Answer403;
    procedure Answer404;
    procedure Answer501;
    procedure Answer503;
    procedure Answer506;
    function AnswerError(ErrCode: Integer): Boolean;

    procedure Reset;
    function GetRequestField(const Name: string): string;

    property RawRequestText: TIocpStringStream read FRawRequestText;
    property RequestCmdLine: string read FRequestCmdLine;
    property Method: string read FMethod;
    property RawPath: string read FRawPath;
    property RawParams: string read FRawParams;
    property RawPathAndParams: string read FRawPathAndParams;
    property Path: string read FPath;
    property Params: string read FParams;
    property PathAndParams: string read FPathAndParams;
    property Version: string read FVersion;
    property KeepAlive: Boolean read FKeepAlive;

    property RequestHeader: TStringList read FRequestHeader;
    property RequestContentType: string read FRequestContentType;
    property RequestHasContentLength: Boolean read FRequestHasContentLength;
    property RequestContentLength: Int64 read FRequestContentLength;
    property RequestAccept: string read FRequestAccept;
    property RequestReferer: string read FRequestReferer;
    property RequestAcceptLanguage: string read FRequestAcceptLanguage;
    property RequestAcceptEncoding: string read FRequestAcceptEncoding;
    property RequestUserAgent: string read FRequestUserAgent;
    property RequestAuth: string read FRequestAuth;
    property RequestCookies: string read FRequestCookies;
    property RequestHost: string read FRequestHost;
    property RequestHostName: string read FRequestHostName;
    property RequestHostPort: string read FRequestHostPort;
    property RequestConnection: string read FRequestConnection;
    property XForwardedFor: string read FXForwardedFor;

    property RequestPostData: TIocpStringStream read FRequestPostData;
  end;

  {
    *** Iocp逻辑(业务处理)请求对象 ***
  }
  TIocpHttpRequest = class(TIocpThreadRequest)
  private
    Client: TIocpHttpConnection;
  protected
    procedure Execute; override;
  public
    constructor Create(Client: TIocpHttpConnection);
  end;

  TIocpHttpHandlerProc = reference to procedure(Connection: TIocpHttpConnection);
  TIocpHttpHandlerMethod = procedure(Connection: TIocpHttpConnection) of object;
  TIocpHttpHandler = record
    Method: string;
    URI: string;
    HandlerProc: TIocpHttpHandlerProc;
    HandlerMethod: TIocpHttpHandlerMethod;
  end;

  TIocpHttpAcceptPostDataEvent = procedure(Sender: TObject; DataSize: Int64; var Accept: Boolean) of object;
  TIocpHttpRequestEvent = procedure(Sender: TObject; Client: TIocpHttpConnection) of object;
  TIocpHttpServer = class(TSimpleIocpTcpServer)
  private const
    {$ifdef __IOCP_SSL__}
    SSL_SERVER_CERT: AnsiString =
      '-----BEGIN CERTIFICATE-----' + sLineBreak +
      'MIIDRDCCAiwCAf8wDQYJKoZIhvcNAQEFBQAwaDELMAkGA1UEBhMCVVMxCzAJBgNV' + sLineBreak +
      'BAgMAkNBMQswCQYDVQQHDAJMQTEVMBMGA1UECgwMVGVzdCBSb290IENBMQswCQYD' + sLineBreak +
      'VQQLDAJJVDEbMBkGA1UEAwwSd3d3LnRlc3Ryb290Y2EuY29tMB4XDTEzMDIyNzE4' + sLineBreak +
      'MjE1NloXDTIzMDIyNTE4MjE1NlowaDELMAkGA1UEBhMCVVMxCzAJBgNVBAgMAkNB' + sLineBreak +
      'MQswCQYDVQQHDAJMQTEVMBMGA1UECgwMVGVzdCBDb21wYW55MQswCQYDVQQLDAJJ' + sLineBreak +
      'VDEbMBkGA1UEAwwSd3d3LnRlc3RzZXJ2ZXIuY29tMIIBIjANBgkqhkiG9w0BAQEF' + sLineBreak +
      'AAOCAQ8AMIIBCgKCAQEAzkHv+S30g5Dc+F1RJ1PUq9Hbh1YkEUJdYEj7ti+UfONV' + sLineBreak +
      'NOT24hXzg8zaNSVO2Bhm+l8vzOVYMnjK9xcGSq5R5I633+lEeFdxURfsSJv9Vymq' + sLineBreak +
      'tHUj5eNkmjzWBVrf4HvnZTJtRJljs941zYUgyJT9tkQXaerGFKJ6sfdXYfhGrkuK' + sLineBreak +
      'gA1e71TwpRFYcfyYbQ3htENTh2CFBv7l5gjrITcmEJwpcU3U4nx4ZTr0IPLmV2kr' + sLineBreak +
      'K8IJysY4dqgRcmduEI5ZgbYGkdG8L7QjggFXA6QNDPu8DfmXeeqS0gIffEm22bk7' + sLineBreak +
      'b2fMnPfnFsJLsDdyhgrdYktnWhtZNij0y80tV4YCTwIDAQABMA0GCSqGSIb3DQEB' + sLineBreak +
      'BQUAA4IBAQDMLn9VnUQt6BWx73J1lExYO/LWulMOnMR/WSVFy9dSwry+E807ekMY' + sLineBreak +
      'WC8b3gpgDIqfkZjmttE9VtAdss2Baten+oBW+K13339sxHvcn30OxOs/Bln0yvaZ' + sLineBreak +
      'Be+Zir7iE450b1IdYI98PMTSKgrK2e3vx/uUOCgG2yvs6/1v5rz5er/M1SQNzdMS' + sLineBreak +
      'blelHWRQ1/ExwoUWBfIBkx/A4lTPmLgoC9fnXSiLhHKbZdfCJD8KLzEV0Se+ocn/' + sLineBreak +
      'vl+6tlcUznap0TsRQpC67T/NGUimxdAhb6G1/U6z9bq0QQIuDxpOIpvwIgLvfRFx' + sLineBreak +
      'qZQxmxOcK28fejHngmek7ZJNYKQbNewP' + sLineBreak +
      '-----END CERTIFICATE-----' + sLineBreak;

    SSL_SERVER_PKEY: AnsiString =
      '-----BEGIN PRIVATE KEY-----' + sLineBreak +
      'MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQDOQe/5LfSDkNz4' + sLineBreak +
      'XVEnU9Sr0duHViQRQl1gSPu2L5R841U05PbiFfODzNo1JU7YGGb6Xy/M5VgyeMr3' + sLineBreak +
      'FwZKrlHkjrff6UR4V3FRF+xIm/1XKaq0dSPl42SaPNYFWt/ge+dlMm1EmWOz3jXN' + sLineBreak +
      'hSDIlP22RBdp6sYUonqx91dh+EauS4qADV7vVPClEVhx/JhtDeG0Q1OHYIUG/uXm' + sLineBreak +
      'COshNyYQnClxTdTifHhlOvQg8uZXaSsrwgnKxjh2qBFyZ24QjlmBtgaR0bwvtCOC' + sLineBreak +
      'AVcDpA0M+7wN+Zd56pLSAh98SbbZuTtvZ8yc9+cWwkuwN3KGCt1iS2daG1k2KPTL' + sLineBreak +
      'zS1XhgJPAgMBAAECggEAIT83s27Y7yw2skI4hqJYsamOPW6BOdb8vjyFdoSM5uSu' + sLineBreak +
      'I2yU7zSioCgxNEfjQaoNT2ZwihKd+OTHsrSfawJWaQUoVot/YfaWaX/1sm6Sk64/' + sLineBreak +
      'uf733mKdIM+VoB9Z3xGZ5xIN0vT2wVOcUJiZBDwf+XVYYNZbP5BBPtaj20LuAcIZ' + sLineBreak +
      'OmW9uigdXQkQ1dylUkRPitjJ92bbysrTr621JTBSmvKnF7ctcF/Ql6VfS5RcqzYI' + sLineBreak +
      '6U1vozoFkjmUnExlYZHC6qKCFG73Z+IcC7ojdMpzMp4/EqiveV/9EVdFlLRB1YAa' + sLineBreak +
      'tND93xU9mo7L26XQzy79Xf2dWRUgUvaJ/7EvLA1RoQKBgQD2ZhJ9ogqfQ0ahq0D6' + sLineBreak +
      '5neZo6bPbckEKshv1GKR5ixnYpPp1kCIxM8oIzb9fOvTX4MOMeRzPJyrJNwhVgfY' + sLineBreak +
      'otWLrvkNviGHXN0frmkdj/Y/WSWh7clzzwXmGbB/8NPG4yzREvQ8vhKBkAmZln6K' + sLineBreak +
      'ICl8J5NxOxF6GgYJ793GcsfZVQKBgQDWS3DYMVQ3eRgFajkQ/8+Gacgdu+8/SyM1' + sLineBreak +
      'WptHOlPvKfqg3nZYPlAjMnVmk0Q7l/d2EtFBPP07/Jz0IvC/pMz0S8XfW/NigcRn' + sLineBreak +
      '0R5Nci3BXbmQEjxNGt0m0sX4C4/Bx8ei8pugipX96OemT/bWP05RskL6tWsofGsb' + sLineBreak +
      '8zgIQcldEwKBgCyx90iyzBp3qahJ2E+q3qcP+IJH9965pAIlFHxCtGtMhmg0ZSBq' + sLineBreak +
      'EunE+YSh1GVTPgKlKjt9Ey44UXX6lRHG99WOt762bn6Pac0FZivmoVR8Z0coSxKm' + sLineBreak +
      'yvsiTdHnbYL2UnraZVNfZxv5dMRXeDy1+NB8nVI81L7BWbcTu7bzuyzBAoGAY0j4' + sLineBreak +
      's3HHbxwvwPKCFhovcDs6eGxGYLDTUzjzkIC5uqlccYQgmKnmPyh1tFyu1F2ITbBS' + sLineBreak +
      'O0OioFRd887sdB5KxzUELIRRs2YkNWVyALfR8zEVdGa+gYrcw8wL5OyWYlXJbPmy' + sLineBreak +
      'mSMcc1OhYDDUUFdsVfWdisLbLxrWFVEOuOSiAvkCgYEA2viHsxoFxOrhnZQOhaLT' + sLineBreak +
      'RPrgaSojv9pooHQ6fJwplewt91tb1OchDIeZk9Sl1hqPAXB0167of43GDOw2vfnq' + sLineBreak +
      'Ust7RtiyJhQhSkz0qp4aH4P9l+dZJIWnpgjcyWkcz893br9gEuVnQgh13V/lcxOn' + sLineBreak +
      'JtpaCFuHNTU3PcFiuQW+cN0=' + sLineBreak +
      '-----END PRIVATE KEY-----' + sLineBreak;
    {$endif}
  private type
    TIocpHttpHandlers = TList<TIocpHttpHandler>;
  private
    {$ifdef __IOCP_HTTP_SERVER_LOGIC_THREAD_POOL__}
    FJobThreadPool: TIocpThreadPool;
    {$endif}
    FRootDir: string;
    FHandlers: TIocpHttpHandlers;
    FHandlersLock: TCriticalSection;
    FAcceptPostData: TIocpHttpAcceptPostDataEvent;
    FOnRequest: TIocpHttpRequestEvent;

    function IsValidHttpRequest(buf: PAnsiChar; len: Integer): Boolean;
    procedure ParseRecvData(Client: TIocpHttpConnection; buf: Pointer; len: Integer);
    function GetRootDir: string;
    function HandlerIndex(const Method, URI: string): Integer;
    function UriIsMatch(const URI, Pattern: string): Boolean;
  protected
    {$ifdef __IOCP_HTTP_SERVER_LOGIC_THREAD_POOL__}
    procedure StartupWorkers; override;
    procedure ShutdownWorkers; override;
    {$endif}

    procedure TriggerClientRecvData(Client: TIocpSocketConnection; Buf: Pointer; Len: Integer); override;
    procedure TriggerClientSentData(Client: TIocpSocketConnection; Buf: Pointer; Len: Integer); override;
  protected
    procedure TriggerAcceptPostData(DataSize: Int64; var Accept: Boolean); virtual;

    // DoOnRequest 将会在线程池中被调用，可以在这个函数里处理用户自定义的回执数据
    procedure DoOnRequest(Client: TIocpHttpConnection); virtual;
  public
    constructor Create(AOwner: TComponent; IoThreadsNumber: Integer); override;
    destructor Destroy; override;

    ///	<summary>
    ///	  注册请求处理函数
    ///	</summary>
    ///	<param name="Method">
    ///	  请求方法, GET/POST/PUT等, 支持通配符和正则表达式, * 表示处理全部请求方法
    ///	</param>
    ///	<param name="URI">
    ///	  请求路径, 支持通配符和正则表达式, * 表示处理全部请求路径
    ///	</param>
    ///	<param name="HandlerProc">
    ///	  处理函数
    ///	</param>
    ///	<remarks>
    ///	  请求处理函数严格按照注册时的顺序被调用, 所以如果在注册了Method=*, URI=*的处理函数之后，再注册的其它处理函数将不会被调用.
    ///	  所以强烈建议把 "* 处理函数" 放到最后注册.
    ///	</remarks>
    procedure RegisterHandler(const Method, URI: string; HandlerProc: TIocpHttpHandlerProc); overload;
    procedure RegisterHandler(const Method, URI: string; HandlerMethod: TIocpHttpHandlerMethod); overload;
    procedure UnregisterHandler(const Method, URI: string);
    procedure UnregisterAllHandlers;
  published
    property InitAcceptNum default IOCP_HTTP_INIT_ACCEPT_NUM;
    property RootDir: string read GetRootDir write FRootDir;
    property AcceptPostData: TIocpHttpAcceptPostDataEvent read FAcceptPostData write FAcceptPostData;
    property OnRequest: TIocpHttpRequestEvent read FOnRequest write FOnRequest;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Iocp', [TIocpHttpServer]);
end;

{ TIocpHttpConnection }

constructor TIocpHttpConnection.Create(AOwner: TObject);
begin
  inherited Create(AOwner);

  FRawRequestText := TIocpStringStream.Create('');
  FRequestHeader := TStringList.Create;
  FRequestPostData := TIocpStringStream.Create('');

  Reset;
end;

destructor TIocpHttpConnection.Destroy;
begin
  FRawRequestText.Free;
  FRequestHeader.Free;
  FRequestPostData.Free;

  inherited Destroy;
end;

function TIocpHttpConnection.GetRequestField(const Name: string): string;
var
  RequestLine: string;
  NameSize, SpacePos: Integer;
begin
  NameSize := Length(Name);
  for RequestLine in FRequestHeader do
  begin
    if (RequestLine = '') then Continue;

    SpacePos := Pos(' ', RequestLine) + 1;

    if StrLIComp(@RequestLine[1], PChar(Name + ':'), NameSize + 1) = 0 then
     Exit(Copy(RequestLine, SpacePos, MaxInt));
  end;

  Result := '';
end;

function TIocpHttpConnection.MakeHeader(const Status, ContType,
  Header: string; ContSize: Integer): string;
begin
  Result := '';

  if (Status = '') then
    Result := Result + FVersion + ' 200 OK' + #13#10
  else
    Result := Result + FVersion + ' ' + Status + #13#10;

  if (ContType = '') then
    Result := Result + 'Content-Type: text/html' + #13#10
  else
    Result := Result + 'Content-Type: ' + ContType + #13#10;


  if (ContSize > 0) then
    Result := Result + 'Content-Length: ' + IntToStr(ContSize) + #13#10;
//    Result := Result + 'Cache-Control: no-cache'#13#10;

  if FKeepAlive then
    Result := Result + 'Connection: keep-alive'#13#10
  else
    Result := Result + 'Connection: close'#13#10;

  if (IOCP_HTTP_SERVER_VERSION <> '') then
    Result := Result + 'Server: ' + IOCP_HTTP_SERVER_VERSION + #13#10;

  if (Header <> '') then
    Result := Result + FixHeader(Header)
  else
    Result := Result + #13#10;
end;

function TIocpHttpConnection.ParseRequestData: Boolean;
var
  RequestLine: string;
  I, J, SpacePos: Integer;
begin
  FRequestHeader.Text := string(FRawRequestText.DataString);
  if (FRequestHeader.Count = 0) then Exit(False);

  // GET /test?v=abc HTTP/1.1
  FRequestCmdLine := FRequestHeader[0];
  FRequestHeader.Delete(0);

  I := 1;
  while (I <= Length(FRequestCmdLine)) and (FRequestCmdLine[I] <> ' ') do
    Inc(I);
  // 请求方法(GET, POST, PUT, HEAD...)
  FMethod := UpperCase(Copy(FRequestCmdLine, 1, I - 1));
  Inc(I);
  while (I <= Length(FRequestCmdLine)) and (FRequestCmdLine[I] = ' ') do
    Inc(I);
  J := I;
  while (I <= Length(FRequestCmdLine)) and (FRequestCmdLine[I] <> ' ') do
    Inc(I);
  // 请求参数及路径
  FRawPathAndParams := Copy(FRequestCmdLine, J, I - J);
  // 解析参数
  J := Pos('?', FRawPathAndParams);
  if (J <= 0) then
  begin
    FRawPath := FRawPathAndParams;
    FRawParams := '';

    FPath := URLDecode(FRawPath);
    FParams := '';
    FPathAndParams := FPath;
  end else
  begin
    FRawPath := Copy(FRawPathAndParams, 1, J - 1);
    FRawParams := Copy(FRawPathAndParams, J + 1, MaxInt);

    FPath := URLDecode(FRawPath);
    FParams := URLDecode(FRawParams);
    FPathAndParams := FPath + '?' + FParams;
  end;

  Inc(I);
  while (I <= Length(FRequestCmdLine)) and (FRequestCmdLine[I] = ' ') do
    Inc(I);
  J := I;
  while (I <= Length(FRequestCmdLine)) and (FRequestCmdLine[I] <> ' ') do
    Inc(I);
  // 请求的HTTP版本
  FVersion := Trim(UpperCase(Copy(FRequestCmdLine, J, I - J)));
  if (FVersion = '') then
    FVersion := 'HTTP/1.0';
  if (FVersion = 'HTTP/1.0') then
    FHttpVerNum := 10
  else
    FHttpVerNum := 11;
  FKeepAlive := (FHttpVerNum = 11);

  FRequestHasContentLength := False;
  FRequestContentLength := 0;
  for RequestLine in FRequestHeader do
  begin
    if (RequestLine = '') then Continue;

    SpacePos := Pos(' ', RequestLine) + 1;

    if StrLIComp(@RequestLine[1], 'Content-Type:', 13) = 0 then
      FRequestContentType := Copy(RequestLine, SpacePos, Length(RequestLine))
    else if StrLIComp(@RequestLine[1], 'Content-Length:', 15) = 0 then
    begin
      FRequestHasContentLength := TRUE;
      FRequestContentLength := StrToInt64Def(Copy(RequestLine, SpacePos, MaxInt), -1);
    end
    else if StrLIComp(@RequestLine[1], 'Accept:', 7) = 0 then
      FRequestAccept:= Copy(RequestLine, SpacePos, MaxInt)
    else if StrLIComp(@RequestLine[1], 'Referer:', 8) = 0 then
      FRequestReferer := Copy(RequestLine, SpacePos, MaxInt)
    else if StrLIComp(@RequestLine[1], 'Accept-Language:', 16) = 0 then
      FRequestAcceptLanguage := Copy(RequestLine, SpacePos, MaxInt)
    else if StrLIComp(@RequestLine[1], 'Accept-Encoding:', 16) = 0 then
      FRequestAcceptEncoding := Copy(RequestLine, SpacePos, MaxInt)
    else if StrLIComp(@RequestLine[1], 'User-Agent:', 11) = 0 then
      FRequestUserAgent := Copy(RequestLine, SpacePos, MaxInt)
    else if StrLIComp(@RequestLine[1], 'Authorization:', 14) = 0 then
      FRequestAuth := Copy(RequestLine, SpacePos, MaxInt)
    else if StrLIComp(@RequestLine[1], 'Cookie:', 7) = 0 then
      FRequestCookies := Copy(RequestLine, SpacePos, MaxInt)
    else if StrLIComp(@RequestLine[1], 'Host:', 5) = 0 then
    begin
      FRequestHost := Copy(RequestLine, SpacePos, MaxInt);
      J := Pos(':', FRequestHost);
      if J > 0 then
      begin
        FRequestHostName := Copy(FRequestHost, 1, J - 1);
        FRequestHostPort := Copy(FRequestHost, J + 1, 100);
      end else
      begin
        FRequestHostName := FRequestHost;
        FRequestHostPort := IntToStr(TIocpHttpServer(Owner).Port);
      end;
    end
    else if StrLIComp(@RequestLine[1], 'Connection:', 11) = 0 then
    begin
      FRequestConnection := Copy(RequestLine, SpacePos, MaxInt);
      // HTTP/1.0 默认KeepAlive=False，只有显示指定了Connection: keep-alive才认为KeepAlive=True
      // HTTP/1.1 默认KeepAlive=True，只有显示指定了Connection: close才认为KeepAlive=False
      if FHttpVerNum = 10 then
        FKeepAlive := SameText(FRequestConnection, 'keep-alive')
      else if SameText(FRequestConnection, 'close') then
        FKeepAlive := False;
    end
    else if StrLIComp(@RequestLine[1], 'X-Forwarded-For:', 16) = 0 then
      FXForwardedFor := Copy(RequestLine, SpacePos, MaxInt);
  end;

  Result := True;
end;

procedure TIocpHttpConnection.Reset;
begin
  FRawRequestText.Size := 0;
  FRequestPostData.Size := 0;

  FHttpState := hcRequest;
  FResponseSize := 0;
  FResponseSent := 0;

  FMethod := '';
  FPath := '';
  FParams := '';
  FPathAndParams := '';
  FVersion := '';
  FHttpVerNum := 0;
  FKeepAlive := False;

  FRequestCmdLine := '';
  FRequestHeader.Clear;
  FRequestContentType := '';
  FRequestHasContentLength := False;
  FRequestContentLength := 0;
  FRequestAccept := '';;
  FRequestReferer := '';
  FRequestAcceptLanguage := '';
  FRequestAcceptEncoding := '';
  FRequestUserAgent := '';
  FRequestAuth := '';
  FRequestCookies := '';
  FRequestHost := '';
  FRequestHostName := '';
  FRequestHostPort := '';
  FRequestConnection := '';
  FXForwardedFor := '';

  FAcceptPostData := False;
  FPostDataSize := 0;
end;

function TIocpHttpConnection.AnswerBuf(const Header: string;
  Buf: Pointer; Size: Integer): Boolean;
var
  FixedHeader: RawByteString;
  Len: Integer;
begin
  // FResponseSize必须准确指定发送的数据包大小
  // 用于在发送完之后(Owner.TriggerClientSentData)断开客户端连接
  if (Header <> '') then
  begin
    FixedHeader := RawByteString(FixHeader(Header));
    Len := Length(FixedHeader);
    Result := (Send(FixedHeader) = Len);
    if not Result then Exit;
    FResponseSize := Len + Size;
  end else
  begin
    if (Buf = nil) or (Size <= 0) then Exit(False);
    FResponseSize := Size;
  end;

  try
    Result := (Send(Buf, Size) = Size);
  except
    on e: Exception do
    begin
      Result := False;
      AppendLog('TIocpHttpConnection.AnswerBuf: %s=%s', [e.ClassName, e.Message], ltException);
    end;
  end;
end;

function TIocpHttpConnection.AnswerStream(const Header: string;
  Stream: TStream): Boolean;
var
  FixedHeader: RawByteString;
  Len: Integer;
begin
  // FResponseSize必须准确指定发送的数据包大小
  // 用于在发送完之后(Owner.TriggerClientSentData)断开客户端连接
  if (Header <> '') then
  begin
    FixedHeader := RawByteString(FixHeader(Header));
    Len := Length(FixedHeader);
    Result := (Send(FixedHeader) = Len);
    if not Result then Exit;
    FResponseSize := Len + Stream.Size;
  end else
  begin
    if not Assigned(Stream) or (Stream.Size <= 0) then Exit(False);
    FResponseSize := Stream.Size;
  end;

  try
    Result := (Send(Stream) = Stream.Size);
  except
    on e: Exception do
    begin
      Result := False;
      AppendLog('TIocpHttpConnection.AnswerStream: %s=%s', [e.ClassName, e.Message], ltException);
    end;
  end;
end;

function TIocpHttpConnection.AnswerBytes(const Status, ContType, Header: string;
  Data: TBytes): Boolean;
var
  Size: Integer;
begin
  Size := Length(Data);
  Result := AnswerBuf(MakeHeader(Status, ContType, Header, Size), Pointer(Data), Size);
end;

function TIocpHttpConnection.AnswerBytes(const Header: string;
  Data: TBytes): Boolean;
begin
  Result := AnswerBuf(Header, Pointer(Data), Length(Data));
end;

function TIocpHttpConnection.AnswerHTML(const Status, ContType, Header: string;
  HTML: RawByteString): Boolean;
var
  Size: Integer;
begin
  Size := Length(HTML);
  Result := AnswerBuf(MakeHeader(Status, ContType, Header, Size), Pointer(HTML), Size);
end;

function TIocpHttpConnection.AnswerHTML(const Status, ContType, Header, HTML: string): Boolean;
begin
  Result := AnswerHTML(Status, ContType, Header, RawByteString(HTML));
end;

function TIocpHttpConnection.AnswerHTML(const Header: string;
  HTML: RawByteString): Boolean;
begin
  Result := AnswerBuf(Header, Pointer(HTML), Length(HTML));
end;

function TIocpHttpConnection.AnswerHTML(const Header, HTML: string): Boolean;
begin
  Result := AnswerHTML(Header, RawByteString(HTML));
end;

function TIocpHttpConnection.AnswerStream(const Status, ContType,
  Header: string; Stream: TStream): Boolean;
var
  Size: Integer;
begin
  Size := Stream.Size;
  Result := AnswerStream(MakeHeader(Status, ContType, Header, Size), Stream);
end;

function TIocpHttpConnection.AnswerDocument(const FileName,
  ContentType: string): Boolean;
var
  Header: string;
  Stream: TFileStream;
begin
  Result := False;
  try
    Stream := Classes.TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  except
    AnswerError(404);
    Exit;
  end;

  try
    Header :=
      FVersion + ' 200 OK' + #13#10 +
      'Content-Type: ' + ContentType + #13#10 +
      'Content-Length: ' + IntToStr(Stream.Size) + #13#10 +
      'Date: ' + RFC1123_Date(Now) + #13#10 +
      'Accept-Ranges: bytes' + #13#10;

    Result := AnswerStream(Header, Stream);
  finally
    Stream.Free;
  end;
end;

function TIocpHttpConnection.AnswerDocument(const FileName: string): Boolean;
begin
  Result := AnswerDocument(FileName, DocumentToContentType(FileName));
end;

procedure TIocpHttpConnection.Answer400;
begin
  AnswerHTML('400 Bad Request', 'text/plain', '', '400 Bad Request');
end;

procedure TIocpHttpConnection.Answer401;
begin
  AnswerHTML('401 Access Denied', 'text/plain', '', '401 Access Denied');
end;

procedure TIocpHttpConnection.Answer403;
begin
  AnswerHTML('403 Forbidden', 'text/plain', '', '403 Forbidden');
end;

procedure TIocpHttpConnection.Answer404;
begin
  AnswerHTML('404 Not Found', 'text/plain', '', '404 Not Found');
end;

procedure TIocpHttpConnection.Answer501;
begin
  AnswerHTML('501 Unimplemented', 'text/plain', '', '501 Unimplemented');
end;

procedure TIocpHttpConnection.Answer503;
begin
  AnswerHTML('503 Server Unavailable', 'text/plain', '', '503 Server Unavailable');
end;

procedure TIocpHttpConnection.Answer506;
begin
  AnswerHTML('506 Server Unavailable', 'text/plain', '', '506 Request Failed');
end;

function TIocpHttpConnection.AnswerError(ErrCode: Integer): Boolean;
begin
  Result := True;

  case ErrCode of
    400: Answer400;
    401: Answer401;
    403: Answer403;
    404: Answer404;
    501: Answer501;
    503: Answer503;
    506: Answer506;
  else
    AnswerHTML(
      Format('%d System Error', [ErrCode]),
      'text/plain', '',
      Format('%d System Error', [ErrCode]));
  end;
end;

{ TIocpHttpRequest }

constructor TIocpHttpRequest.Create(Client: TIocpHttpConnection);
begin
  Self.Client := Client;
end;

procedure TIocpHttpRequest.Execute;
begin
  try
    TIocpHttpServer(Client.Owner).DoOnRequest(Client);
  finally
    Client.Release;
  end;
end;

{ TIocpHttpServer }

constructor TIocpHttpServer.Create(AOwner: TComponent;
  IoThreadsNumber: Integer);
begin
  FHandlers := TIocpHttpHandlers.Create;
  FHandlersLock := TCriticalSection.Create;

  inherited Create(AOwner, IoThreadsNumber);

  InitAcceptNum := IOCP_HTTP_INIT_ACCEPT_NUM;
  ConnectionClass := TIocpHttpConnection;

  {$ifdef __IOCP_SSL__}
  SetCert(PAnsiChar(SSL_SERVER_CERT), Length(SSL_SERVER_CERT),
    PAnsiChar(SSL_SERVER_PKEY), Length(SSL_SERVER_PKEY));
  {$endif}
end;

destructor TIocpHttpServer.Destroy;
begin
  FHandlersLock.Enter;
  FHandlers.Free;
  FHandlersLock.Leave;
  FHandlersLock.Free;

  inherited Destroy;
end;

procedure TIocpHttpServer.DoOnRequest(Client: TIocpHttpConnection);
  procedure Default;
  var
    RealPath, RequestPath: string;
  begin
    // 在这里响应客户端的请求
    if (Client.Method = 'GET') then
    begin
      RequestPath := StringReplace(Client.Path, '//', '/', [rfReplaceAll]);
      RealPath := ExpandFileName(RootDir) + UnixPathToDosPath(Copy(RequestPath, 2, Length(RequestPath)));
      if (IsDirectory(RealPath)) then
      begin
        if not Client.AnswerHTML('', '', '', RawByteString(BuildDirList(RealPath, RequestPath))) then
        begin
          Client.Answer404;
        end;
      end else
      begin
        if not FileExists(RealPath) or not Client.AnswerDocument(RealPath) then
        begin
          Client.Answer404;
        end;
      end;
    end else
    begin
      Client.AnswerHTML('', '', '', 'Hello World!');
    end;
  end;
var
  LHandled: Boolean;
  LHandler: TIocpHttpHandler;
begin
  LHandled := False;
  if Assigned(FOnRequest) then
  begin
    FOnRequest(Self, Client);
    LHandled := True;
  end;

  FHandlersLock.Enter;
  try
    for LHandler in FHandlers do
    begin
//      if (SameText(LHandler.Method, Client.Method) or (LHandler.Method = '*')) and
//         (SameText(LHandler.URI, Client.Path) or (LHandler.URI = '*')) then
      if UriIsMatch(Client.Method, LHandler.Method) and
         UriIsMatch(Client.Path, LHandler.URI) then
      begin
        if Assigned(LHandler.HandlerProc) then
          LHandler.HandlerProc(Client)
        else if Assigned(LHandler.HandlerMethod) then
          LHandler.HandlerMethod(Client);
        LHandled := True;
        Break;
      end;
    end;
  finally
    FHandlersLock.Leave;
  end;

  if not LHandled then
    Default;
end;

function TIocpHttpServer.GetRootDir: string;
begin
  if (FRootDir = '') then
    FRootDir := TDirectory.GetCurrentDirectory;
  if (FRootDir[Length(FRootDir)] <> TPath.DirectorySeparatorChar) then
    FRootDir := FRootDir + TPath.DirectorySeparatorChar;
  Result := FRootDir;
end;

function TIocpHttpServer.HandlerIndex(const Method, URI: string): Integer;
var
  I: Integer;
begin
  for I := 0 to FHandlers.Count - 1 do
  begin
    if SameText(FHandlers[I].Method, Method) and SameText(FHandlers[I].URI, URI) then
      Exit(I);
  end;
  Result := -1;
end;

function TIocpHttpServer.IsValidHttpRequest(buf: PAnsiChar;
  len: Integer): Boolean;
begin
  // HTTP 1.1 支持8种请求
  Result := (len > 7) and
    ((StrLIComp(buf, 'GET', 3) = 0) or
    (StrLIComp(buf, 'POST', 4) = 0) or
    (StrLIComp(buf, 'PUT', 3) = 0) or
    (StrLIComp(buf, 'HEAD', 4) = 0) or
    (StrLIComp(buf, 'OPTIONS', 7) = 0) or
    (StrLIComp(buf, 'DELETE', 6) = 0) or
    (StrLIComp(buf, 'TRACE', 5) = 0) or
    (StrLIComp(buf, 'CONNECT', 7) = 0));
end;

procedure TIocpHttpServer.ParseRecvData(Client: TIocpHttpConnection;
  buf: Pointer; len: Integer);
var
  pch: PAnsiChar;
  CR, LF: Integer;
begin
  // 在这里解析客户端浏览器发送过来的请求数据
  if (Client.FHttpState = hcDone) then
    Client.Reset;

  pch := buf;
  CR := 0;
  LF := 0;
  while (len > 0) do
  begin
    if (Client.FHttpState = hcRequest) then
    begin
      case pch^ of
        #13: Inc(CR);
        #10: Inc(LF);
      else
        CR := 0;
        LF := 0;
      end;

      // 写入请求数据
      Client.RawRequestText.Write(pch^, 1);

      // 如果不是有效的Http请求直接断开
      if (Client.RawRequestText.Size > 7) and
        not IsValidHttpRequest(PAnsiChar(Client.RawRequestText.DataString), Client.RawRequestText.Size) then
      begin
        Client.Disconnect;
        Exit;
      end;

      // 请求数据已接收完毕(#13#10#13#10是HTTP请求结束的标志)
      if (CR = 2) and (LF = 2) then
      begin
        if not Client.ParseRequestData then
        begin
          Client.Disconnect;
          Exit;
        end;

        if SameText(Client.Method, 'POST') or
          SameText(Client.Method, 'PUT') then
        begin
          // 无效的Post请求直接断开
          if (Client.FRequestContentLength <= 0) then
          begin
            Client.Disconnect;
            Exit;
          end;
          Client.FRequestPostData.Size := 0;
          Client.FPostDataSize := 0;
          Client.FHttpState := hcPostData;
          Client.FAcceptPostData := True;
          TriggerAcceptPostData(Client.FRequestContentLength, Client.FAcceptPostData);
        end else
        begin
          Client.FHttpState := hcDone;
          Break;
        end;
      end;

      Dec(len);
      Inc(pch);
    end else
    if (Client.FHttpState = hcPostData) then
    begin
      Inc(Client.FPostDataSize, len);
      if Client.FAcceptPostData then
        Client.FRequestPostData.Write(pch^, len);

      if (Client.FPostDataSize >= Client.FRequestContentLength) then
        Client.FHttpState := hcDone;

      // Post数据直接剩余部分整段处理，到这里就已经全部处理完了，直接跳出循环
      Break;
    end;
  end;

  // 在解析完请求数据之后再调用线程池
  if (Client.FHttpState = hcDone) then
  begin
    {$ifdef __IOCP_HTTP_SERVER_LOGIC_THREAD_POOL__}
    if (Client.AddRef = 1) then Exit;
    FJobThreadPool.AddRequest(TIocpHttpRequest.Create(Client));
    {$else}
    DoOnRequest(Client);
    {$endif}
  end;
end;

procedure TIocpHttpServer.RegisterHandler(const Method, URI: string;
  HandlerProc: TIocpHttpHandlerProc);
var
  I: Integer;
  LHandler: TIocpHttpHandler;
begin
  LHandler.Method := Method;
  LHandler.URI := URI;
  LHandler.HandlerProc := HandlerProc;
  LHandler.HandlerMethod := nil;

  FHandlersLock.Enter;
  I := HandlerIndex(Method, URI);
  if (I < 0) then
    FHandlers.Add(LHandler)
  else
    FHandlers[I] := LHandler;
  FHandlersLock.Leave;
end;

procedure TIocpHttpServer.RegisterHandler(const Method, URI: string;
  HandlerMethod: TIocpHttpHandlerMethod);
var
  I: Integer;
  LHandler: TIocpHttpHandler;
begin
  LHandler.Method := Method;
  LHandler.URI := URI;
  LHandler.HandlerProc := nil;
  LHandler.HandlerMethod := HandlerMethod;

  FHandlersLock.Enter;
  I := HandlerIndex(Method, URI);
  if (I < 0) then
    FHandlers.Add(LHandler)
  else
    FHandlers[I] := LHandler;
  FHandlersLock.Leave;
end;

procedure TIocpHttpServer.UnregisterHandler(const Method, URI: string);
var
  I: Integer;
begin
  FHandlersLock.Enter;
  I := HandlerIndex(Method, URI);
  if (I >= 0) then
    FHandlers.Delete(I);
  FHandlersLock.Leave;
end;

function TIocpHttpServer.UriIsMatch(const URI, Pattern: string): Boolean;
begin
  Result := MatchesMask(URI, Pattern) or TRegEx.IsMatch(URI, Pattern);
end;

procedure TIocpHttpServer.UnregisterAllHandlers;
begin
  FHandlersLock.Enter;
  FHandlers.Clear;
  FHandlersLock.Leave;
end;

{$ifdef __IOCP_HTTP_SERVER_LOGIC_THREAD_POOL__}
procedure TIocpHttpServer.StartupWorkers;
begin
  if not Assigned(FJobThreadPool) then
    FJobThreadPool := TIocpThreadPool.Create;

  inherited StartupWorkers;
end;

procedure TIocpHttpServer.ShutdownWorkers;
begin
  inherited ShutdownWorkers;

  if Assigned(FJobThreadPool) then
  begin
    FJobThreadPool.Shutdown;
    FreeAndNil(FJobThreadPool);
  end;
end;
{$endif}

procedure TIocpHttpServer.TriggerAcceptPostData(DataSize: Int64; var Accept: Boolean);
begin
  if Assigned(FAcceptPostData) then
    FAcceptPostData(Self, DataSize, Accept);
end;

procedure TIocpHttpServer.TriggerClientRecvData(Client: TIocpSocketConnection;
  Buf: Pointer; Len: Integer);
begin
  inherited TriggerClientRecvData(Client, Buf, Len);
  ParseRecvData(TIocpHttpConnection(Client), buf, len);
end;

procedure TIocpHttpServer.TriggerClientSentData(Client: TIocpSocketConnection;
  Buf: Pointer; Len: Integer);
begin
  inherited TriggerClientSentData(Client, Buf, Len);
  with TIocpHttpConnection(Client) do
  begin
    // 如果客户端是HTTP/1.0的请求，回复完数据之后需要断开连接
    // Apache提供的ab测试程序就是使用的HTTP/1.0，如果这里不断开，ab无法正常测试，
    // 会一直等到超时退出
    Inc(FResponseSent, len);
    if not KeepAlive and (FResponseSize > 0) and (FResponseSent >= FResponseSize) then
      Disconnect;
  end;
end;

end.
