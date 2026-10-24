// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// *************************************************************************** }

unit RuntimeFixesTestsU;

// Body binding, serialization and host parity, on the wire. The same engine
// runs in-process on Indy Direct, WebBroker (Indy bridge) and HTTP.sys, and
// every host case is asserted on all three: a difference between hosts is the
// defect these tests exist to catch.

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Nullables,
  MVCFramework.Serializer.Commons,
  MVCFramework.Server.Intf;

const
  RF_INDY_PORT = 8921;
  RF_WEBBROKER_PORT = 8922;
  RF_HTTPSYS_PORT = 8923;
  RF_RELISTEN_PORT1 = 9291;
  RF_RELISTEN_PORT2 = 9292;
  RF_MAX_REQUEST_SIZE = 4096;

type
  [MVCNameCase(ncCamelCase)]
  TRFIntegers = class
  private
    fAltitude: NullableInt32;
    fPort16: NullableUInt16;
    fBigN: NullableUInt64;
    fIntValue: Integer;
    fCardValue: Cardinal;
    fWordValue: Word;
    fByteValue: Byte;
    fShortValue: ShortInt;
    fSmallValue: SmallInt;
    fInt64Value: Int64;
    fHuge: UInt64;
  public
    property Altitude: NullableInt32 read fAltitude write fAltitude;
    property Port16: NullableUInt16 read fPort16 write fPort16;
    property BigN: NullableUInt64 read fBigN write fBigN;
    property IntValue: Integer read fIntValue write fIntValue;
    property CardValue: Cardinal read fCardValue write fCardValue;
    property WordValue: Word read fWordValue write fWordValue;
    property ByteValue: Byte read fByteValue write fByteValue;
    property ShortValue: ShortInt read fShortValue write fShortValue;
    property SmallValue: SmallInt read fSmallValue write fSmallValue;
    property Int64Value: Int64 read fInt64Value write fInt64Value;
    property Huge: UInt64 read fHuge write fHuge;
  end;

  [MVCNameCase(ncCamelCase)]
  TRFTimes = class
  private
    fTimeValue: TTime;
    fDateValue: TDate;
    fHuge: UInt64;
    fCardValue: Cardinal;
  public
    property TimeValue: TTime read fTimeValue write fTimeValue;
    property DateValue: TDate read fDateValue write fDateValue;
    property Huge: UInt64 read fHuge write fHuge;
    property CardValue: Cardinal read fCardValue write fCardValue;
  end;

  [MVCPath('/rf')]
  TRuntimeFixesController = class(TMVCController)
  public
    [MVCPath('/integers')]
    [MVCHTTPMethod([httpPOST])]
    function EchoIntegers(const [MVCFromBody] Obj: TRFIntegers): TRFIntegers;
    [MVCPath('/times/direct')]
    [MVCHTTPMethod([httpGET])]
    function TimesDirect: TRFTimes;
    [MVCPath('/times/okresponse')]
    [MVCHTTPMethod([httpGET])]
    function TimesOkResponse: IMVCResponse;
    [MVCPath('/form')]
    [MVCHTTPMethod([httpPOST])]
    function FormField: string;
    [MVCPath('/status422')]
    [MVCHTTPMethod([httpGET])]
    function Status422: string;
    [MVCPath('/html422')]
    [MVCHTTPMethod([httpGET])]
    function Html422: IMVCResponse;
    [MVCPath('/cookie')]
    [MVCHTTPMethod([httpGET])]
    function CookieEcho: string;
    [MVCPath('/query')]
    [MVCHTTPMethod([httpGET])]
    function QueryEcho: string;
    [MVCPath('/fromquery')]
    [MVCHTTPMethod([httpGET])]
    function FromQueryEcho(const [MVCFromQueryString('q')] Q: string): string;
    [MVCPath('/people/($ID)')]
    [MVCHTTPMethod([httpGET])]
    function Person(ID: Integer): string;
    [MVCPath('/raw/($P)')]
    [MVCHTTPMethod([httpGET])]
    function RawPath(P: string): string;
    [MVCPath('/empty/($Code)')]
    [MVCHTTPMethod([httpGET, httpPOST])]
    function Empty(Code: Integer): IMVCResponse;
    [MVCPath('/emptyraw/($Code)')]
    [MVCHTTPMethod([httpGET, httpPOST])]
    procedure EmptyRaw(Code: Integer);
  end;

  TRFRawResponse = record
    StatusLine: string;
    Status: Integer;
    Headers: string;
    Body: string;
  end;

  [TestFixture]
  TRuntimeFixesTests = class
  private
    fIndy: IMVCServer;
    fWebBroker: IMVCServer;
    fHttpSys: IMVCServer;
    function Raw(const APort: Integer; const ARequestLine: string; const AHeaders: array of string;
      const ABody: string = ''): TRFRawResponse;
  public
    [SetupFixture]
    procedure SetupFixture;
    [TeardownFixture]
    procedure TeardownFixture;

    // serializer, no server
    [Test]
    procedure IntegerTargetsRefuseOutOfRangeValues;
    [Test]
    procedure IntegerTargetsKeepTheirBoundaries;
    [Test]
    procedure UnsignedValuesAreNotSerializedAsSigned;

    // on the wire, every host
    [Test]
    [TestCase('IndyDirect', '8921')]
    [TestCase('WebBroker', '8922')]
    [TestCase('HttpSys', '8923')]
    procedure OutOfRangeIntegerInTheBodyIs400(const APort: Integer);
    [Test]
    [TestCase('IndyDirect', '8921')]
    [TestCase('WebBroker', '8922')]
    [TestCase('HttpSys', '8923')]
    procedure OkResponseAndDirectSerializeTimeDateAndUInt64Alike(const APort: Integer);
    [Test]
    [TestCase('IndyDirect', '8921')]
    [TestCase('WebBroker', '8922')]
    [TestCase('HttpSys', '8923')]
    procedure RepeatedFormFieldGivesTheLastValue(const APort: Integer);
    [Test]
    [TestCase('IndyDirect', '8921')]
    [TestCase('WebBroker', '8922')]
    [TestCase('HttpSys', '8923')]
    procedure Status422HasOneReasonPhrase(const APort: Integer);
    [Test]
    [TestCase('IndyDirect', '8921')]
    [TestCase('WebBroker', '8922')]
    [TestCase('HttpSys', '8923')]
    procedure CookieValueIsDecoded(const APort: Integer);
    [Test]
    [TestCase('IndyDirect', '8921')]
    [TestCase('WebBroker', '8922')]
    [TestCase('HttpSys', '8923')]
    procedure UnknownVerbIs501(const APort: Integer);
    [Test]
    [TestCase('IndyDirect', '8921')]
    [TestCase('WebBroker', '8922')]
    [TestCase('HttpSys', '8923')]
    procedure MalformedQueryEscapeIs400(const APort: Integer);
    [Test]
    [TestCase('IndyDirect', '8921')]
    [TestCase('WebBroker', '8922')]
    [TestCase('HttpSys', '8923')]
    procedure FromQueryStringIsDecodedOnce(const APort: Integer);
    [Test]
    [TestCase('IndyDirect', '8921')]
    [TestCase('WebBroker', '8922')]
    [TestCase('HttpSys', '8923')]
    procedure TooLargeBodyIs413WithReasonAndBody(const APort: Integer);
    [Test]
    [TestCase('IndyDirect', '8921')]
    [TestCase('WebBroker', '8922')]
    [TestCase('HttpSys', '8923')]
    procedure BadPathParamDoesNotLeakTheForeignException(const APort: Integer);
    [Test]
    [TestCase('IndyDirect', '8921')]
    [TestCase('WebBroker', '8922')]
    [TestCase('HttpSys', '8923')]
    procedure RawPathInfoIsTheUndecodedPath(const APort: Integer);
    [Test]
    [TestCase('IndyDirect', '8921')]
    [TestCase('WebBroker', '8922')]
    [TestCase('HttpSys', '8923')]
    procedure StatusLineNeverCarriesTheRequestText(const APort: Integer);
    [Test]
    [TestCase('IndyDirect', '8921')]
    [TestCase('WebBroker', '8922')]
    [TestCase('HttpSys', '8923')]
    procedure OneServerHeaderAndACharsetOnTheErrorBody(const APort: Integer);
    [Test]
    [TestCase('IndyDirect', '8921')]
    [TestCase('WebBroker', '8922')]
    [TestCase('HttpSys', '8923')]
    procedure WrongKindForAnIntegerFieldIs400WithAnAccurateMessage(const APort: Integer);
    [Test]
    [TestCase('IndyDirect', '8921')]
    [TestCase('WebBroker', '8922')]
    [TestCase('HttpSys', '8923')]
    procedure EmptyResponsesHaveNoBody(const APort: Integer);
  end;

  TRFRpc = class
  public
    function Add(const A, B: Integer): Integer;
  end;

  [MVCPath('/v')]
  TRFViewController = class(TMVCController)
  public
    [MVCPath('/broken')]
    [MVCHTTPMethod([httpGET])]
    function Broken: string;
  end;

  TRFAuthHandler = class(TInterfacedObject, IMVCAuthenticationHandler)
  public
    procedure OnRequest(const AContext: TWebContext; const AControllerQualifiedClassName: string;
      const AActionName: string; var AAuthenticationRequired: Boolean);
    procedure OnAuthentication(const AContext: TWebContext; const AUserName: string;
      const APassword: string; AUserRoles: TList<string>; var AIsValid: Boolean;
      const ASessionData: TSessionData);
    procedure OnAuthorization(const AContext: TWebContext; AUserRoles: TList<string>;
      const AControllerQualifiedClassName: string; const AActionName: string;
      var AIsAuthorized: Boolean);
  end;

  // JSON-RPC, views, the JWT endpoint filter and the plain log line: one host is
  // enough, none of these depends on it
  [TestFixture]
  TRuntimeFixesSingleHostTests = class
  private
    fServer: IMVCServer;
    fViewPath: string;
  public
    [SetupFixture]
    procedure SetupFixture;
    [TeardownFixture]
    procedure TeardownFixture;
    [Test]
    procedure JSONRPCInvalidVersionKeepsTheID;
    [Test]
    procedure TemplateCompileErrorGoesThroughTheViewErrorHandler;
    [Test]
    procedure JWTFilterFillsUserNameAndRolesForRequireRole;
    [Test]
    procedure PlainLogLineCarriesTheRequestContext;
  end;

  [TestFixture]
  TServerRelistenTests = class
  private
    function CanConnect(const APort: Integer): Boolean;
  public
    [Test]
    procedure IndyDirectListensOnTheNewPortAfterStop;
    [Test]
    procedure WebBrokerListensOnTheNewPortAfterStop;
    [Test]
    procedure HttpSysListenTwiceIsANoOpAndStopThenListenMoves;
  end;

implementation

uses
  System.DateUtils,
  IdTCPClient,
  IdGlobal,
  System.IOUtils,
  System.Net.HttpClient,
  JsonDataObjects,
  LoggerPro,
  MVCFramework.Serializer.JsonDataObjects,
  MVCFramework.View.Renderers.TemplatePro,
  MVCFramework.Logger.ColorConsoleRenderer,
  MVCFramework.MinimalAPI,
  MVCFramework.Filters,
  MVCFramework.JWT,
  MVCFramework.Server.Factory;

var
  gRFViewPath: string;

const
  RF_SINGLE_PORT = 8928;
  RF_JWT_SECRET = 'runtime-fixes-secret-long-enough-for-hs512-0123456789';

procedure ConfigureRFEngine(const AEngine: TMVCEngine);
begin
  AEngine.AddController(TRuntimeFixesController);
end;

procedure ConfigureRFConfig(AConfig: TMVCConfig);
begin
  AConfig[TMVCConfigKey.MaxRequestSize] := IntToStr(RF_MAX_REQUEST_SIZE);
end;

function NewRFEngine: TMVCEngine;
begin
  Result := TMVCEngine.Create(ConfigureRFConfig);
  ConfigureRFEngine(Result);
end;

function NewTimes: TRFTimes;
begin
  Result := TRFTimes.Create;
  Result.TimeValue := EncodeTime(20, 0, 0, 0);
  Result.DateValue := EncodeDate(2026, 10, 24);
  Result.Huge := High(UInt64);
  Result.CardValue := High(Cardinal);
end;

{ TRuntimeFixesController }

function TRuntimeFixesController.EchoIntegers(const Obj: TRFIntegers): TRFIntegers;
begin
  Result := Obj; // the framework sees the alias and frees it once
end;

function TRuntimeFixesController.TimesDirect: TRFTimes;
begin
  Result := NewTimes;
end;

function TRuntimeFixesController.TimesOkResponse: IMVCResponse;
begin
  Result := OKResponse(NewTimes);
end;

function TRuntimeFixesController.FormField: string;
begin
  Result := Context.Request.ContentParam('flag');
end;

function TRuntimeFixesController.Status422: string;
begin
  Context.Response.StatusCode := HTTP_STATUS.UnprocessableEntity;
  Result := 'direct';
end;

function TRuntimeFixesController.Html422: IMVCResponse;
var
  lResp: TMVCHTMLResponse;
begin
  lResp := TMVCHTMLResponse.Create;
  lResp.StatusCode := HTTP_STATUS.UnprocessableEntity;
  lResp.HTMLBody := '<p>html</p>';
  Result := lResp;
end;

function TRuntimeFixesController.CookieEcho: string;
begin
  Result := Context.Request.Cookie('greet');
end;

function TRuntimeFixesController.QueryEcho: string;
begin
  Result := Context.Request.QueryStringParam('q');
end;

function TRuntimeFixesController.FromQueryEcho(const Q: string): string;
begin
  Result := Q;
end;

function TRuntimeFixesController.Person(ID: Integer): string;
begin
  Result := IntToStr(ID);
end;

function TRuntimeFixesController.RawPath(P: string): string;
begin
  Result := Context.Request.RawPathInfo;
end;

function TRuntimeFixesController.Empty(Code: Integer): IMVCResponse;
begin
  Result := MVCFramework.Status(Code); // a status, no body
end;

procedure TRuntimeFixesController.EmptyRaw(Code: Integer);
begin
  Context.Response.StatusCode := Code; // the MCP notification shape: a status and nothing else
end;

{ TRuntimeFixesTests }

procedure TRuntimeFixesTests.SetupFixture;
begin
  fIndy := TMVCServerFactory.CreateIndyDirect(NewRFEngine);
  fIndy.Listen(RF_INDY_PORT, 'localhost');
  fWebBroker := TMVCServerFactory.CreateWebBroker(
    procedure(AConfig: TMVCConfig)
    begin
      ConfigureRFConfig(AConfig);
    end,
    procedure(AEngine: TMVCEngine)
    begin
      ConfigureRFEngine(AEngine);
    end);
  fWebBroker.Listen(RF_WEBBROKER_PORT, 'localhost');
  fHttpSys := TMVCServerFactory.CreateHttpSys(NewRFEngine);
  fHttpSys.Listen(RF_HTTPSYS_PORT, 'localhost');
end;

procedure TRuntimeFixesTests.TeardownFixture;
begin
  fIndy.Stop;
  fIndy := nil;
  fWebBroker.Stop;
  fWebBroker := nil;
  fHttpSys.Stop;
  fHttpSys := nil;
end;

function TRuntimeFixesTests.Raw(const APort: Integer; const ARequestLine: string;
  const AHeaders: array of string; const ABody: string): TRFRawResponse;
var
  lClient: TIdTCPClient;
  lHeader, lLine: string;
  lLength: Integer;
  lBody: TIdBytes;
begin
  // by hand: the verb, the escapes and the repeated fields are what is under test
  lClient := TIdTCPClient.Create(nil);
  try
    lClient.Host := '127.0.0.1';
    lClient.Port := APort;
    lClient.ConnectTimeout := 3000;
    lClient.ReadTimeout := 5000;
    lClient.Connect;
    lClient.IOHandler.WriteLn(ARequestLine);
    lClient.IOHandler.WriteLn('Host: localhost');
    for lHeader in AHeaders do
      lClient.IOHandler.WriteLn(lHeader);
    if ABody <> '' then
      lClient.IOHandler.WriteLn('Content-Length: ' + IntToStr(Length(UTF8String(ABody))));
    lClient.IOHandler.WriteLn('Connection: close');
    lClient.IOHandler.WriteLn('');
    if ABody <> '' then
      lClient.IOHandler.Write(ABody, IndyTextEncoding_UTF8);
    Result.StatusLine := lClient.IOHandler.ReadLn;
    Result.Status := StrToIntDef(Copy(Result.StatusLine, 10, 3), 0);
    Result.Headers := '';
    lLength := -1;
    repeat
      lLine := lClient.IOHandler.ReadLn;
      if lLine <> '' then
      begin
        Result.Headers := Result.Headers + lLine + #10;
        if lLine.StartsWith('Content-Length:', True) then
          lLength := StrToIntDef(Trim(Copy(lLine, 16, MaxInt)), -1);
      end;
    until lLine = '';
    SetLength(lBody, 0);
    try
      if lLength > 0 then
        lClient.IOHandler.ReadBytes(lBody, lLength, False)
      else if lLength < 0 then
        lClient.IOHandler.ReadBytes(lBody, -1, False);
    except
      // the server closed the connection: what was read is the body
    end;
    Result.Body := IndyTextEncoding_UTF8.GetString(lBody);
  finally
    lClient.Free;
  end;
end;

procedure TRuntimeFixesTests.IntegerTargetsRefuseOutOfRangeValues;
var
  lSer: TMVCJsonDataObjectsSerializer;
  lCase: string;
  lObj: TRFIntegers;
  lStatus: Integer;
begin
  lSer := TMVCJsonDataObjectsSerializer.Create;
  try
    for lCase in [
      '{"altitude":2147483648}', '{"altitude":-2147483649}',
      '{"port16":70000}', '{"port16":-1}',
      '{"bigN":-1}', '{"huge":-1}',
      '{"intValue":2147483648}', '{"intValue":-2147483649}',
      '{"cardValue":-1}', '{"cardValue":4294967296}',
      '{"wordValue":65536}', '{"wordValue":-1}',
      '{"byteValue":256}', '{"byteValue":-1}',
      '{"shortValue":128}', '{"shortValue":-129}',
      '{"smallValue":32768}', '{"smallValue":-32769}',
      '{"int64Value":9223372036854775808}'] do
    begin
      lObj := TRFIntegers.Create;
      try
        lStatus := 0;
        try
          lSer.DeserializeObject(lCase, lObj);
        except
          on E: EMVCException do
            lStatus := E.HTTPStatusCode;
        end;
        Assert.AreEqual(400, lStatus, lCase + ' must be refused as a client error');
      finally
        lObj.Free;
      end;
    end;
  finally
    lSer.Free;
  end;
end;

procedure TRuntimeFixesTests.IntegerTargetsKeepTheirBoundaries;
var
  lSer: TMVCJsonDataObjectsSerializer;
  lObj: TRFIntegers;
begin
  lSer := TMVCJsonDataObjectsSerializer.Create;
  lObj := TRFIntegers.Create;
  try
    lSer.DeserializeObject('{"altitude":-2147483648,"port16":65535,"bigN":18446744073709551615,' +
      '"intValue":2147483647,"cardValue":4294967295,"wordValue":65535,"byteValue":255,' +
      '"shortValue":-128,"smallValue":-32768,"int64Value":-9223372036854775808,' +
      '"huge":18446744073709551615}', lObj);
    Assert.AreEqual(Low(Int32), lObj.Altitude.Value);
    Assert.AreEqual(65535, Integer(lObj.Port16.Value));
    Assert.IsTrue(lObj.BigN.Value = High(UInt64), 'NullableUInt64 max');
    Assert.AreEqual(High(Integer), lObj.IntValue);
    Assert.IsTrue(lObj.CardValue = High(Cardinal), 'Cardinal max');
    Assert.AreEqual(65535, Integer(lObj.WordValue));
    Assert.AreEqual(255, Integer(lObj.ByteValue));
    Assert.AreEqual(-128, Integer(lObj.ShortValue));
    Assert.AreEqual(-32768, Integer(lObj.SmallValue));
    Assert.IsTrue(lObj.Int64Value = Low(Int64), 'Int64 min');
    Assert.IsTrue(lObj.Huge = High(UInt64), 'UInt64 max');
  finally
    lObj.Free;
    lSer.Free;
  end;
end;

procedure TRuntimeFixesTests.UnsignedValuesAreNotSerializedAsSigned;
var
  lSer: TMVCJsonDataObjectsSerializer;
  lObj: TRFIntegers;
  lJSON: string;
begin
  lSer := TMVCJsonDataObjectsSerializer.Create;
  lObj := TRFIntegers.Create;
  try
    lObj.Huge := High(UInt64);
    lObj.BigN := High(UInt64);
    lObj.CardValue := High(Cardinal);
    lJSON := lSer.SerializeObject(lObj);
    Assert.Contains(lJSON, '"huge":18446744073709551615', lJSON);
    Assert.Contains(lJSON, '"bigN":18446744073709551615', lJSON);
    Assert.Contains(lJSON, '"cardValue":4294967295', lJSON);
  finally
    lObj.Free;
    lSer.Free;
  end;
end;

procedure TRuntimeFixesTests.OutOfRangeIntegerInTheBodyIs400(const APort: Integer);
var
  lResp: TRFRawResponse;
begin
  lResp := Raw(APort, 'POST /rf/integers HTTP/1.1', ['Content-Type: application/json'],
    '{"altitude":2147483648}');
  Assert.AreEqual(400, lResp.Status, lResp.StatusLine + ' ' + lResp.Body);
  lResp := Raw(APort, 'POST /rf/integers HTTP/1.1', ['Content-Type: application/json'],
    '{"port16":70000}');
  Assert.AreEqual(400, lResp.Status, lResp.StatusLine + ' ' + lResp.Body);
  lResp := Raw(APort, 'POST /rf/integers HTTP/1.1', ['Content-Type: application/json'],
    '{"altitude":12,"huge":18446744073709551615}');
  Assert.AreEqual(200, lResp.Status, lResp.StatusLine + ' ' + lResp.Body);
  Assert.Contains(lResp.Body, '18446744073709551615', lResp.Body);
end;

procedure TRuntimeFixesTests.OkResponseAndDirectSerializeTimeDateAndUInt64Alike(const APort: Integer);
var
  lDirect, lOk: TRFRawResponse;
  lExpected: string;
begin
  lDirect := Raw(APort, 'GET /rf/times/direct HTTP/1.1', []);
  lOk := Raw(APort, 'GET /rf/times/okresponse HTTP/1.1', []);
  Assert.AreEqual(200, lDirect.Status, lDirect.Body);
  Assert.AreEqual(200, lOk.Status, lOk.Body);
  for lExpected in ['"timeValue":"20:00:00"', '"dateValue":"2026-10-24"', '"huge":18446744073709551615',
    '"cardValue":4294967295'] do
  begin
    Assert.Contains(lDirect.Body, lExpected, 'direct: ' + lDirect.Body);
    Assert.Contains(lOk.Body, lExpected, 'OKResponse: ' + lOk.Body);
  end;
end;

procedure TRuntimeFixesTests.RepeatedFormFieldGivesTheLastValue(const APort: Integer);
var
  lResp: TRFRawResponse;
begin
  // the forms-library checkbox: a hidden "false" then the checked "true"
  lResp := Raw(APort, 'POST /rf/form HTTP/1.1', ['Content-Type: application/x-www-form-urlencoded'],
    'flag=false&flag=true');
  Assert.AreEqual(200, lResp.Status, lResp.Body);
  Assert.AreEqual('true', lResp.Body);
end;

procedure TRuntimeFixesTests.Status422HasOneReasonPhrase(const APort: Integer);
var
  lResp: TRFRawResponse;
begin
  lResp := Raw(APort, 'GET /rf/status422 HTTP/1.1', []);
  Assert.AreEqual('HTTP/1.1 422 Unprocessable Content', lResp.StatusLine, 'Context.Response.StatusCode');
  lResp := Raw(APort, 'GET /rf/html422 HTTP/1.1', []);
  Assert.AreEqual('HTTP/1.1 422 Unprocessable Content', lResp.StatusLine, 'TMVCHTMLResponse');
end;

procedure TRuntimeFixesTests.CookieValueIsDecoded(const APort: Integer);
var
  lResp: TRFRawResponse;
begin
  // the encoded form every host writes for "ciao Bruce; 100%"
  lResp := Raw(APort, 'GET /rf/cookie HTTP/1.1', ['Cookie: greet=ciao+Bruce%3B+100%25']);
  Assert.AreEqual(200, lResp.Status, lResp.Body);
  Assert.AreEqual('ciao Bruce; 100%', lResp.Body);
  // a stray '%' no host wrote: the value as it came, not a 500
  lResp := Raw(APort, 'GET /rf/cookie HTTP/1.1', ['Cookie: greet=100%']);
  Assert.AreEqual(200, lResp.Status, lResp.Body);
  Assert.AreEqual('100%', lResp.Body);
end;

procedure TRuntimeFixesTests.UnknownVerbIs501(const APort: Integer);
var
  lVerb: string;
  lResp: TRFRawResponse;
begin
  for lVerb in ['PROPFIND', 'MOVE', 'BREW'] do
  begin
    lResp := Raw(APort, lVerb + ' /rf/form HTTP/1.1', []);
    Assert.AreEqual(501, lResp.Status, lVerb + ': ' + lResp.StatusLine);
  end;
end;

procedure TRuntimeFixesTests.MalformedQueryEscapeIs400(const APort: Integer);
var
  lResp: TRFRawResponse;
begin
  lResp := Raw(APort, 'GET /rf/query?q=100% HTTP/1.1', []);
  Assert.AreEqual(400, lResp.Status, lResp.StatusLine + ' ' + lResp.Body);
  lResp := Raw(APort, 'GET /rf/query?q=100%25 HTTP/1.1', []);
  Assert.AreEqual(200, lResp.Status, lResp.StatusLine + ' ' + lResp.Body);
  Assert.AreEqual('100%', lResp.Body);
end;

procedure TRuntimeFixesTests.FromQueryStringIsDecodedOnce(const APort: Integer);
var
  lResp: TRFRawResponse;
begin
  lResp := Raw(APort, 'GET /rf/fromquery?q=100%25 HTTP/1.1', []);
  Assert.AreEqual(200, lResp.Status, lResp.StatusLine + ' ' + lResp.Body);
  Assert.AreEqual('100%', lResp.Body);
  lResp := Raw(APort, 'GET /rf/fromquery?q=%2541 HTTP/1.1', []);
  Assert.AreEqual('%41', lResp.Body, 'decoded twice');
end;

procedure TRuntimeFixesTests.TooLargeBodyIs413WithReasonAndBody(const APort: Integer);
var
  lResp: TRFRawResponse;
begin
  lResp := Raw(APort, 'POST /rf/form HTTP/1.1', ['Content-Type: application/x-www-form-urlencoded'],
    'flag=' + StringOfChar('x', RF_MAX_REQUEST_SIZE * 2));
  Assert.AreEqual('HTTP/1.1 413 Content Too Large', lResp.StatusLine);
  Assert.Contains(lResp.Body, 'Request size exceeded the max allowed size', 'body: ' + lResp.Body);
end;

procedure TRuntimeFixesTests.BadPathParamDoesNotLeakTheForeignException(const APort: Integer);
var
  lResp: TRFRawResponse;
begin
  lResp := Raw(APort, 'GET /rf/people/abc HTTP/1.1', ['Accept: application/json']);
  Assert.AreEqual(400, lResp.Status, lResp.Body);
  Assert.Contains(lResp.Body, 'Invalid Integer value for param [ID]', lResp.Body);
  Assert.DoesNotContain(lResp.Body, 'EConvertError', lResp.Body);
  Assert.DoesNotContain(lResp.Body, '[MSG:', lResp.Body);
end;

procedure TRuntimeFixesTests.RawPathInfoIsTheUndecodedPath(const APort: Integer);
var
  lResp: TRFRawResponse;
begin
  lResp := Raw(APort, 'GET /rf/raw/a%41b HTTP/1.1', []);
  Assert.AreEqual(200, lResp.Status, lResp.Body);
  Assert.AreEqual('/rf/raw/a%41b', lResp.Body);
end;

procedure TRuntimeFixesTests.StatusLineNeverCarriesTheRequestText(const APort: Integer);
var
  lResp: TRFRawResponse;
begin
  lResp := Raw(APort, 'PROPFIND /rf/form HTTP/1.1', []);
  Assert.AreEqual('HTTP/1.1 501 Not Implemented', lResp.StatusLine);
end;

procedure TRuntimeFixesTests.OneServerHeaderAndACharsetOnTheErrorBody(const APort: Integer);
var
  lResp: TRFRawResponse;
  lLine: string;
  lServers: Integer;
  lContentType: string;
begin
  lResp := Raw(APort, 'GET /rf/query?q=100% HTTP/1.1', ['Accept: application/json']);
  Assert.AreEqual(400, lResp.Status, lResp.Body);
  lServers := 0;
  lContentType := '';
  for lLine in lResp.Headers.Split([#10]) do
  begin
    if lLine.StartsWith('Server:', True) then
      Inc(lServers);
    if lLine.StartsWith('Content-Type:', True) then
      lContentType := lLine;
  end;
  Assert.IsTrue(lServers <= 1, 'Server headers: ' + lResp.Headers);
  Assert.Contains(LowerCase(lContentType), 'charset=utf-8', lResp.Headers);
end;

procedure TRuntimeFixesTests.WrongKindForAnIntegerFieldIs400WithAnAccurateMessage(const APort: Integer);
var
  lResp: TRFRawResponse;
begin
  lResp := Raw(APort, 'POST /rf/integers HTTP/1.1', ['Content-Type: application/json', 'Accept: application/json'],
    '{"port16":"north"}');
  Assert.AreEqual(400, lResp.Status, lResp.Body);
  Assert.Contains(lResp.Body, 'port16', lResp.Body);
  lResp := Raw(APort, 'POST /rf/integers HTTP/1.1', ['Content-Type: application/json', 'Accept: application/json'],
    '{"intValue":"x"}');
  Assert.AreEqual(400, lResp.Status, lResp.Body);
  Assert.Contains(lResp.Body, 'intValue', lResp.Body);
  lResp := Raw(APort, 'POST /rf/integers HTTP/1.1', ['Content-Type: application/json', 'Accept: application/json'],
    '{"int64Value":"x"}');
  Assert.AreEqual(400, lResp.Status, lResp.Body);
  lResp := Raw(APort, 'POST /rf/integers HTTP/1.1', ['Content-Type: application/json', 'Accept: application/json'],
    '{"port16":12.5}');
  Assert.AreEqual(400, lResp.Status, lResp.Body);
  Assert.Contains(lResp.Body, 'Integer value expected for \"port16\"', lResp.Body);
  Assert.DoesNotContain(lResp.Body, 'out of range', lResp.Body);
end;

procedure TRuntimeFixesTests.EmptyResponsesHaveNoBody(const APort: Integer);
var
  lCode: Integer;
  lResp: TRFRawResponse;
begin
  for lCode in [202, 204, 200, 1202, 1204, 1200] do
  begin
    if lCode > 1000 then
      lResp := Raw(APort, 'GET /rf/emptyraw/' + IntToStr(lCode - 1000) + ' HTTP/1.1', [])
    else
      lResp := Raw(APort, 'GET /rf/empty/' + IntToStr(lCode) + ' HTTP/1.1', []);
    Assert.AreEqual(lCode mod 1000, lResp.Status, lResp.StatusLine);
    Assert.AreEqual('', lResp.Body, IntToStr(lCode) + ': ' + lResp.Headers);
    Assert.DoesNotContain(LowerCase(lResp.Headers), 'text/html', IntToStr(lCode) + ': ' + lResp.Headers);
    if lCode mod 1000 <> 204 then // a 204 carries no Content-Length at all on some hosts, and must not carry a body
      Assert.Contains(LowerCase(lResp.Headers), 'content-length: 0', IntToStr(lCode) + ': ' + lResp.Headers);
  end;
end;

{ TRFRpc }

function TRFRpc.Add(const A, B: Integer): Integer;
begin
  Result := A + B;
end;

{ TRFViewController }

function TRFViewController.Broken: string;
begin
  Result := RenderView('broken');
end;

{ TRFAuthHandler }

procedure TRFAuthHandler.OnRequest(const AContext: TWebContext; const AControllerQualifiedClassName,
  AActionName: string; var AAuthenticationRequired: Boolean);
begin
  AAuthenticationRequired := True;
end;

procedure TRFAuthHandler.OnAuthentication(const AContext: TWebContext; const AUserName, APassword: string;
  AUserRoles: TList<string>; var AIsValid: Boolean; const ASessionData: TSessionData);
begin
  AIsValid := APassword = 'pwd';
  if AIsValid and (AUserName = 'daniele') then
    AUserRoles.Add('admin');
  if AIsValid then
    AUserRoles.Add('reader');
end;

procedure TRFAuthHandler.OnAuthorization(const AContext: TWebContext; AUserRoles: TList<string>;
  const AControllerQualifiedClassName, AActionName: string; var AIsAuthorized: Boolean);
begin
  AIsAuthorized := True;
end;

type
  TRFColorRendererAccess = class(TMVCColorConsoleRenderer);

{ TRuntimeFixesSingleHostTests }

procedure ConfigureRFViewConfig(AConfig: TMVCConfig);
begin
  AConfig[TMVCConfigKey.ViewPath] := gRFViewPath;
  AConfig[TMVCConfigKey.DefaultViewFileExtension] := 'html';
end;

procedure TRuntimeFixesSingleHostTests.SetupFixture;
var
  lEngine: TMVCEngine;
  lLogin, lAdmin: TMVCRouteGroup<TObject>;
begin
  fViewPath := TPath.Combine(TPath.GetTempPath, 'dmvc_rf_views_' + IntToStr(TThread.CurrentThread.ThreadID));
  TDirectory.CreateDirectory(fViewPath);
  // an unterminated block: a compile error, not a render error
  TFile.WriteAllText(TPath.Combine(fViewPath, 'broken.html'), '{{if x}}never closed');
  gRFViewPath := fViewPath;
  lEngine := TMVCEngine.Create(ConfigureRFViewConfig);
  lEngine.SetViewEngine(TMVCTemplateProViewEngine);
  lEngine.AddController(TRFViewController);
  lEngine.PublishObject(
    function: TObject
    begin
      Result := TRFRpc.Create;
    end, '/rpc');
  lLogin := lEngine.Prefix('/secure').Use(JWT(TRFAuthHandler.Create,
    procedure(const AJWT: TJWT)
    begin
      AJWT.Claims.ExpirationTime := Now + 1;
    end, RF_JWT_SECRET, '/secure/login'));
  lLogin.MapGet<TWebContext>('/whoami',
    function(AContext: TWebContext): IMVCResponse
    begin
      Result := Ok(AContext.LoggedUser.UserName + '|' + string.Join(',', AContext.LoggedUser.Roles.ToArray) +
        '|' + BoolToStr(AContext.LoggedUser.IsValid, True));
    end);
  lLogin.MapPost('/login',
    function: IMVCResponse
    begin
      Result := Ok('never reached');
    end);
  lAdmin := lEngine.Prefix('/secure')
    .Use(JWT(TRFAuthHandler.Create, nil, RF_JWT_SECRET, '/secure/login'))
    .Use(RequireRole('admin'));
  lAdmin.MapGet<TWebContext>('/admin',
    function(AContext: TWebContext): IMVCResponse
    begin
      Result := Ok(AContext.LoggedUser.UserName);
    end);
  fServer := TMVCServerFactory.CreateIndyDirect(lEngine);
  fServer.Listen(RF_SINGLE_PORT, 'localhost');
end;

procedure TRuntimeFixesSingleHostTests.TeardownFixture;
begin
  fServer.Stop;
  fServer := nil;
  try
    TDirectory.Delete(fViewPath, True);
  except
    // a leftover temp folder is not a test failure
  end;
end;

procedure TRuntimeFixesSingleHostTests.JSONRPCInvalidVersionKeepsTheID;
var
  lClient: THTTPClient;
  lBody: TStringStream;
  lJSON: TJsonObject;
begin
  lClient := THTTPClient.Create;
  lBody := TStringStream.Create('{"jsonrpc":"1.0","id":12,"method":"add","params":[1,2]}', TEncoding.UTF8);
  try
    lClient.ContentType := 'application/json';
    lJSON := TJsonObject.Parse(lClient.Post(Format('http://localhost:%d/rpc', [RF_SINGLE_PORT]), lBody)
      .ContentAsString(TEncoding.UTF8)) as TJsonObject;
    try
      Assert.AreEqual(12, lJSON.I['id'], lJSON.ToJSON);
      Assert.IsTrue(lJSON.Contains('error'), lJSON.ToJSON);
    finally
      lJSON.Free;
    end;
  finally
    lBody.Free;
    lClient.Free;
  end;
end;

procedure TRuntimeFixesSingleHostTests.TemplateCompileErrorGoesThroughTheViewErrorHandler;
var
  lClient: THTTPClient;
  lResp: IHTTPResponse;
  lText: string;
begin
  lClient := THTTPClient.Create;
  try
    lClient.Accept := 'application/json';
    lResp := lClient.Get(Format('http://localhost:%d/v/broken', [RF_SINGLE_PORT]));
    lText := lResp.ContentAsString(TEncoding.UTF8);
    Assert.AreEqual(500, lResp.StatusCode, lText);
    // the handler that logs "View [x] error" and, in a release build, keeps the
    // detail (and the server path) away from the client
    Assert.Contains(lText, 'View [broken] error', lText);
    Assert.Contains(lText, 'EMVCViewError', lText);
  finally
    lClient.Free;
  end;
end;

function RFLogin(const AUser: string): string;
var
  lClient: THTTPClient;
  lResp: IHTTPResponse;
begin
  lClient := THTTPClient.Create;
  try
    lClient.CustomHeaders['jwtusername'] := AUser;
    lClient.CustomHeaders['jwtpassword'] := 'pwd';
    lResp := lClient.Post(Format('http://localhost:%d/secure/login', [RF_SINGLE_PORT]), TStream(nil));
    Assert.AreEqual(200, lResp.StatusCode, lResp.ContentAsString);
    Result := lResp.HeaderValue['Authorization'];
  finally
    lClient.Free;
  end;
end;

function RFGetPath(const AToken, APath: string; out AText: string): Integer;
var
  lClient: THTTPClient;
  lResp: IHTTPResponse;
begin
  lClient := THTTPClient.Create;
  try
    lClient.CustomHeaders['Authorization'] := AToken;
    lResp := lClient.Get(Format('http://localhost:%d%s', [RF_SINGLE_PORT, APath]));
    AText := lResp.ContentAsString(TEncoding.UTF8);
    Result := lResp.StatusCode;
  finally
    lClient.Free;
  end;
end;

function RFGetAdmin(const AToken: string; out AText: string): Integer;
begin
  Result := RFGetPath(AToken, '/secure/admin', AText);
end;

procedure TRuntimeFixesSingleHostTests.JWTFilterFillsUserNameAndRolesForRequireRole;
var
  lText: string;
begin
  RFGetPath(RFLogin('daniele'), '/secure/whoami', lText);
  Assert.Contains(lText, 'daniele|admin,reader|True', lText);
  Assert.AreEqual(200, RFGetAdmin(RFLogin('daniele'), lText), 'admin: ' + lText);
  Assert.Contains(lText, 'daniele', lText);
  Assert.AreEqual(403, RFGetAdmin(RFLogin('bruce'), lText), 'not admin: ' + lText);
end;

procedure TRuntimeFixesSingleHostTests.PlainLogLineCarriesTheRequestContext;
var
  lRenderer: TRFColorRendererAccess;
  lItem: TLogItem;
  lLine: string;
begin
  lRenderer := TRFColorRendererAccess.Create;
  try
    lRenderer.Setup;
    lItem := TLogItem.Create(TLogType.Info, '', 'dmvcframework', [
      LogParam.S('method', 'GET'), LogParam.I('status', 200), LogParam.S('path', '/people'),
      LogParam.S('ip', '127.0.0.1'), LogParam.S('duration', '3ms')]);
    try
      lLine := lRenderer.RenderPlain(lItem);
    finally
      lItem.Free;
    end;
    Assert.Contains(lLine, '200 | 3ms | 127.0.0.1 | GET "/people"', lLine);
    lItem := TLogItem.Create(TLogType.Info, 'saved', 'app', [LogParam.I('id', 42)]);
    try
      lLine := lRenderer.RenderPlain(lItem);
    finally
      lItem.Free;
    end;
    Assert.Contains(lLine, 'saved id=42', lLine);
  finally
    lRenderer.TearDown;
    lRenderer.Free;
  end;
end;

{ TServerRelistenTests }

procedure TServerRelistenTests.HttpSysListenTwiceIsANoOpAndStopThenListenMoves;
var
  lServer: IMVCServer;
begin
  lServer := TMVCServerFactory.CreateHttpSys(NewRFEngine);
  try
    lServer.Listen(RF_RELISTEN_PORT1, 'localhost');
    // a second Listen while listening is ignored, as on Indy Direct and WebBroker
    lServer.Listen(RF_RELISTEN_PORT1, 'localhost');
    Assert.IsTrue(CanConnect(RF_RELISTEN_PORT1), 'first port');
    lServer.Stop;
    lServer.Listen(RF_RELISTEN_PORT2, 'localhost');
    Assert.IsTrue(CanConnect(RF_RELISTEN_PORT2), 'second port');
  finally
    lServer.Stop;
  end;
end;

function TServerRelistenTests.CanConnect(const APort: Integer): Boolean;
var
  lClient: TIdTCPClient;
begin
  lClient := TIdTCPClient.Create(nil);
  try
    lClient.Host := '127.0.0.1';
    lClient.Port := APort;
    lClient.ConnectTimeout := 1000;
    try
      lClient.Connect;
      Result := True;
      lClient.Disconnect;
    except
      Result := False;
    end;
  finally
    lClient.Free;
  end;
end;

procedure TServerRelistenTests.IndyDirectListensOnTheNewPortAfterStop;
var
  lServer: IMVCServer;
begin
  lServer := TMVCServerFactory.CreateIndyDirect(NewRFEngine);
  try
    lServer.Listen(RF_RELISTEN_PORT1, '0.0.0.0');
    Assert.IsTrue(CanConnect(RF_RELISTEN_PORT1), 'first port');
    lServer.Stop;
    lServer.Listen(RF_RELISTEN_PORT2, '0.0.0.0');
    Assert.IsTrue(CanConnect(RF_RELISTEN_PORT2), 'second port');
    Assert.IsFalse(CanConnect(RF_RELISTEN_PORT1), 'the old port is still open');
  finally
    lServer.Stop;
  end;
end;

procedure TServerRelistenTests.WebBrokerListensOnTheNewPortAfterStop;
var
  lServer: IMVCServer;
begin
  lServer := TMVCServerFactory.CreateWebBroker(
    procedure(AConfig: TMVCConfig)
    begin
      ConfigureRFConfig(AConfig);
    end,
    procedure(AEngine: TMVCEngine)
    begin
      ConfigureRFEngine(AEngine);
    end);
  try
    lServer.Listen(RF_RELISTEN_PORT1, '0.0.0.0');
    Assert.IsTrue(CanConnect(RF_RELISTEN_PORT1), 'first port');
    lServer.Stop;
    lServer.Listen(RF_RELISTEN_PORT2, '0.0.0.0');
    Assert.IsTrue(CanConnect(RF_RELISTEN_PORT2), 'second port');
    Assert.IsFalse(CanConnect(RF_RELISTEN_PORT1), 'the old port is still open');
  finally
    lServer.Stop;
  end;
end;

initialization

TDUnitX.RegisterTestFixture(TRuntimeFixesTests);
TDUnitX.RegisterTestFixture(TServerRelistenTests);
TDUnitX.RegisterTestFixture(TRuntimeFixesSingleHostTests);

end.
