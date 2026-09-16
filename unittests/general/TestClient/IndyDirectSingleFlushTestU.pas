unit IndyDirectSingleFlushTestU;

interface

uses
  DUnitX.TestFramework,
  System.Classes,
  System.SysUtils,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Server.Intf;

type
  [MVCPath('/sf')]
  TSingleFlushController = class(TMVCController)
  public
    [MVCPath('/small')]
    [MVCHTTPMethod([httpGET])]
    procedure Small;

    [MVCPath('/empty')]
    [MVCHTTPMethod([httpGET])]
    procedure Empty;

    [MVCPath('/binary')]
    [MVCHTTPMethod([httpGET])]
    procedure Binary;

    [MVCPath('/large')]
    [MVCHTTPMethod([httpGET])]
    procedure Large;

    [MVCPath('/hdrs')]
    [MVCHTTPMethod([httpGET])]
    procedure WithHeadersAndCookies;

    [MVCPath('/err')]
    [MVCHTTPMethod([httpGET])]
    procedure Err;
  end;

  [TestFixture]
  TIndyDirectSingleFlushTests = class
  private
    FEngine: TMVCEngine;
    FServer: IMVCIndyServer;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure Test_SmallTextBody;
    [Test]
    procedure Test_EmptyBody;
    [Test]
    procedure Test_BinaryStreamBody;
    [Test]
    procedure Test_LargeBody;
    [Test]
    procedure Test_HeadersAndCookies;
    [Test]
    procedure Test_ErrorResponse;
    [Test]
    procedure Test_KeepAliveTwoRequests_RawSocket;
  end;

implementation

uses
  System.NetEncoding,
  System.Net.HttpClient,
  MVCFramework.Server.Indy,
  MVCFramework.RESTClient,
  MVCFramework.RESTClient.Intf,
  IdTCPClient, IdGlobal;

const
  TEST_PORT = 9797;
  LARGE_BODY_SIZE = 50 * 1024;

{ TSingleFlushController }

procedure TSingleFlushController.Small;
begin
  Render('hello single-flush');
end;

procedure TSingleFlushController.Empty;
begin
  ResponseStatus(HTTP_STATUS.NoContent);
end;

procedure TSingleFlushController.Binary;
var
  lStream: TMemoryStream;
  I: Integer;
  B: Byte;
begin
  lStream := TMemoryStream.Create;
  for I := 0 to 1023 do
  begin
    B := I mod 256;
    lStream.Write(B, SizeOf(B));
  end;
  lStream.Position := 0;
  Context.Response.ContentType := 'application/octet-stream';
  Render(lStream);
end;

procedure TSingleFlushController.Large;
var
  lBuf: TStringBuilder;
  I: Integer;
begin
  lBuf := TStringBuilder.Create(LARGE_BODY_SIZE);
  try
    for I := 0 to (LARGE_BODY_SIZE div 10) - 1 do
      lBuf.Append('0123456789');
    Render(lBuf.ToString);
  finally
    lBuf.Free;
  end;
end;

procedure TSingleFlushController.WithHeadersAndCookies;
begin
  Context.Response.SetCustomHeader('X-SingleFlush-Marker', 'yes');
  Context.Response.Cookies.Add.Name := 'sf_cookie';
  Context.Response.Cookies[0].Value := 'sf_value';
  Render('with hdrs+cookies');
end;

procedure TSingleFlushController.Err;
begin
  raise EMVCException.Create(HTTP_STATUS.InternalServerError, 'boom');
end;

{ TIndyDirectSingleFlushTests }

procedure TIndyDirectSingleFlushTests.Setup;
begin
  FEngine := TMVCEngine.Create(nil);
  FEngine.AddController(TSingleFlushController);
  FServer := TMVCIndyServer.Create(FEngine);
  FServer.SingleFlushResponse := True;
  FServer.Listen(TEST_PORT);
end;

procedure TIndyDirectSingleFlushTests.TearDown;
begin
  if Assigned(FServer) then
    FServer.Stop;
  FServer := nil;
  FreeAndNil(FEngine);
end;

procedure TIndyDirectSingleFlushTests.Test_SmallTextBody;
var
  lClient: IMVCRESTClient;
  lResp: IMVCRESTResponse;
begin
  lClient := TMVCRESTClient.New.BaseURL('localhost', TEST_PORT);
  lResp := lClient.Get('/sf/small');
  Assert.AreEqual(HTTP_STATUS.OK, lResp.StatusCode);
  Assert.AreEqual('hello single-flush', lResp.Content);
end;

procedure TIndyDirectSingleFlushTests.Test_EmptyBody;
var
  lClient: IMVCRESTClient;
  lResp: IMVCRESTResponse;
begin
  lClient := TMVCRESTClient.New.BaseURL('localhost', TEST_PORT);
  lResp := lClient.Get('/sf/empty');
  Assert.AreEqual(HTTP_STATUS.NoContent, lResp.StatusCode);
  Assert.AreEqual('', lResp.Content);
end;

procedure TIndyDirectSingleFlushTests.Test_BinaryStreamBody;
var
  lClient: IMVCRESTClient;
  lResp: IMVCRESTResponse;
  lBytes: TBytes;
  I: Integer;
begin
  lClient := TMVCRESTClient.New.BaseURL('localhost', TEST_PORT);
  lResp := lClient.Get('/sf/binary');
  Assert.AreEqual(HTTP_STATUS.OK, lResp.StatusCode);
  lBytes := lResp.ContentRawBytes;
  Assert.AreEqual(Integer(1024), Integer(Length(lBytes)));
  for I := 0 to High(lBytes) do
    Assert.AreEqual(Integer(I mod 256), Integer(lBytes[I]),
      Format('byte %d mismatch', [I]));
end;

procedure TIndyDirectSingleFlushTests.Test_LargeBody;
var
  lClient: IMVCRESTClient;
  lResp: IMVCRESTResponse;
begin
  lClient := TMVCRESTClient.New.BaseURL('localhost', TEST_PORT);
  lResp := lClient.Get('/sf/large');
  Assert.AreEqual(HTTP_STATUS.OK, lResp.StatusCode);
  Assert.AreEqual(LARGE_BODY_SIZE, Length(lResp.Content));
  Assert.AreEqual('0123456789', Copy(lResp.Content, 1, 10));
  Assert.AreEqual('0123456789',
    Copy(lResp.Content, LARGE_BODY_SIZE - 9, 10));
end;

procedure TIndyDirectSingleFlushTests.Test_HeadersAndCookies;
var
  lClient: IMVCRESTClient;
  lResp: IMVCRESTResponse;
  lCookie: TCookie;
  lFoundCookie: Boolean;
begin
  lClient := TMVCRESTClient.New.BaseURL('localhost', TEST_PORT);
  lResp := lClient.Get('/sf/hdrs');
  Assert.AreEqual(HTTP_STATUS.OK, lResp.StatusCode);
  Assert.AreEqual('with hdrs+cookies', lResp.Content);
  Assert.AreEqual('yes', lResp.HeaderValue('X-SingleFlush-Marker'));

  lFoundCookie := False;
  for lCookie in lResp.Cookies do
    if SameText(lCookie.Name, 'sf_cookie') and
       (lCookie.Value = 'sf_value') then
    begin
      lFoundCookie := True;
      Break;
    end;
  Assert.IsTrue(lFoundCookie,
    'sf_cookie=sf_value not found in response cookies');
end;

procedure TIndyDirectSingleFlushTests.Test_ErrorResponse;
var
  lClient: IMVCRESTClient;
  lResp: IMVCRESTResponse;
begin
  lClient := TMVCRESTClient.New.BaseURL('localhost', TEST_PORT);
  lResp := lClient.Get('/sf/err');
  Assert.AreEqual(HTTP_STATUS.InternalServerError, lResp.StatusCode);
  Assert.Contains(lResp.Content, 'boom');
end;

procedure TIndyDirectSingleFlushTests.Test_KeepAliveTwoRequests_RawSocket;
// Raw-socket keep-alive: the definitive double-emission check.
// If the single-flush path left spurious body bytes on the wire, the
// second response parse would pick up garbage. Here we parse two
// responses back-to-back on the same TCP connection and assert each
// is intact.
var
  lTcp: TIdTCPClient;
  lStatus1, lStatus2: string;
  lBody1, lBody2: string;
  lContentLength1, lContentLength2: Integer;

  function ReadOneResponse(out AStatus, ABody: string;
    out AContentLength: Integer): Boolean;
  var
    lHeadersLocal: string;
    lFirstLine: string;
    lLineLocal: string;
    I: Integer;
    lBytes: TIdBytes;
  begin
    Result := False;
    lFirstLine := lTcp.IOHandler.ReadLn;
    if lFirstLine = '' then
      Exit;
    AStatus := lFirstLine;
    lHeadersLocal := '';
    AContentLength := 0;
    repeat
      lLineLocal := lTcp.IOHandler.ReadLn;
      lHeadersLocal := lHeadersLocal + lLineLocal + #13#10;
      if lLineLocal.StartsWith('Content-Length:', True) then
        AContentLength := StrToIntDef(
          Trim(Copy(lLineLocal, Pos(':', lLineLocal) + 1, MaxInt)), 0);
    until lLineLocal = '';
    if AContentLength > 0 then
    begin
      SetLength(lBytes, 0);
      lTcp.IOHandler.ReadBytes(lBytes, AContentLength, False);
      SetLength(ABody, AContentLength);
      for I := 0 to AContentLength - 1 do
        ABody[I + 1] := Char(lBytes[I]);
    end
    else
      ABody := '';
    Result := True;
  end;

begin
  lTcp := TIdTCPClient.Create(nil);
  try
    lTcp.Host := '127.0.0.1';
    lTcp.Port := TEST_PORT;
    lTcp.ConnectTimeout := 5000;
    lTcp.ReadTimeout := 5000;
    lTcp.Connect;
    try
      // First request: keep-alive
      lTcp.IOHandler.WriteLn('GET /sf/small HTTP/1.1');
      lTcp.IOHandler.WriteLn('Host: localhost');
      lTcp.IOHandler.WriteLn('Connection: keep-alive');
      lTcp.IOHandler.WriteLn('');

      Assert.IsTrue(ReadOneResponse(lStatus1, lBody1, lContentLength1),
        'first response not readable');
      Assert.Contains(lStatus1, '200', 'first status not 200');
      Assert.AreEqual('hello single-flush', lBody1,
        'first body mismatch');
      Assert.AreEqual(Length('hello single-flush'), lContentLength1,
        'first Content-Length mismatch');

      // Second request on SAME connection: close after
      lTcp.IOHandler.WriteLn('GET /sf/small HTTP/1.1');
      lTcp.IOHandler.WriteLn('Host: localhost');
      lTcp.IOHandler.WriteLn('Connection: close');
      lTcp.IOHandler.WriteLn('');

      Assert.IsTrue(ReadOneResponse(lStatus2, lBody2, lContentLength2),
        'second response not readable (likely framing corruption)');
      Assert.Contains(lStatus2, '200', 'second status not 200');
      Assert.AreEqual('hello single-flush', lBody2,
        'second body mismatch - possible double-emission from 1st response');
      Assert.AreEqual(Length('hello single-flush'), lContentLength2,
        'second Content-Length mismatch');
    finally
      lTcp.Disconnect;
    end;
  finally
    lTcp.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TIndyDirectSingleFlushTests);

end.
