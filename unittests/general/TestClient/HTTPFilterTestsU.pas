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

unit HTTPFilterTestsU;

// The ready-made HTTP filters of MVCFramework.Filters (StaticFiles, SPA, RangeMedia, FileServerBundle,
// Compression, ETag, CORS, Redirect, Trace) against a real server. The server side is HTTPFilterSupportU (TestServer folder).
//
// Two kinds of server: the TestServer on 8888, which is whatever host the suite is run against (WebBroker,
// Indy Direct, HTTP.sys, ISAPI, Apache), and two in-process servers on Indy Direct and HTTP.sys. There is no
// in-process WebBroker server: that host keeps the engine configuration and a cache of web modules for the
// whole process.
//
// The TestServer serves the same folder with the classic middleware (/static, /spa) and with the filter
// (/fstatic, /fspa): the static tests run against both, so the two have to answer the same way.
//
// The session filters and the ActiveRecord filter are in THTTPFilterSessionTests, on the in-process servers
// only: the TestServer has a classic session middleware on every request, which would replace the session
// factory of a filter.

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Logger,
  MVCFramework.Server.Intf,
  MVCFramework.Filters, // TMVCTraceFlags
  LoggerPro.MemoryAppender;

const
  HF_TESTSERVER_PORT = 8888;
  HF_INDY_PORT = 8935;
  HF_HTTPSYS_PORT = 8937;
  HF_SESSION_INDY_PORT = 8939;
  HF_SESSION_HTTPSYS_PORT = 8941;
  HF_STREAM_INDY_PORT = 8945;
  HF_TRACE_PORT = 8947;

type
  [TestFixture]
  THTTPFilterTests = class
  private
    fWww: string;
    fIndyEngine: TMVCEngine;
    fHttpSysEngine: TMVCEngine;
    fIndy: IMVCServer;
    fHttpSys: IMVCServer;
  public
    [SetupFixture]
    procedure SetupFixture;
    [TeardownFixture]
    procedure TeardownFixture;

    // files, folders and the redirect of a folder: the classic middleware and the filter, the same answers
    [Test]
    [TestCase('middleware, TestServer', '8888,/static')]
    [TestCase('filter, TestServer', '8888,/fstatic')]
    [TestCase('filter, Indy Direct', '8935,/fstatic')]
    [TestCase('filter, HTTP.sys', '8937,/fstatic')]
    [TestCase('bundle, TestServer', '8888,/ffiles')]
    [TestCase('bundle, Indy Direct', '8935,/ffiles')]
    [TestCase('bundle, HTTP.sys', '8937,/ffiles')]
    procedure FilesFoldersAndTheRedirectOfAFolder(const APort: Integer; const APrefix: string);
    [Test]
    [TestCase('middleware, TestServer', '8888,/static')]
    [TestCase('filter, TestServer', '8888,/fstatic')]
    [TestCase('filter, Indy Direct', '8935,/fstatic')]
    [TestCase('filter, HTTP.sys', '8937,/fstatic')]
    [TestCase('bundle, TestServer', '8888,/ffiles')]
    [TestCase('bundle, Indy Direct', '8935,/ffiles')]
    [TestCase('bundle, HTTP.sys', '8937,/ffiles')]
    procedure ATraversalIsRefused(const APort: Integer; const APrefix: string);
    [Test]
    // Apache's URL validator answers 400 to a URL with backslashes before the request reaches the
    // application (see TestDirectoryTraversal1 in LiveServerTestU)
    [Category('staticfiles,NotOnApache')]
    [TestCase('middleware, TestServer', '8888,/static')]
    [TestCase('filter, TestServer', '8888,/fstatic')]
    [TestCase('filter, Indy Direct', '8935,/fstatic')]
    [TestCase('filter, HTTP.sys', '8937,/fstatic')]
    [TestCase('bundle, TestServer', '8888,/ffiles')]
    [TestCase('bundle, Indy Direct', '8935,/ffiles')]
    [TestCase('bundle, HTTP.sys', '8937,/ffiles')]
    procedure BackslashTraversalAnswers404Or403(const APort: Integer; const APrefix: string);
    [Test]
    [TestCase('middleware, TestServer', '8888,/spa')]
    [TestCase('filter, TestServer', '8888,/fspa')]
    [TestCase('filter, Indy Direct', '8935,/fspa')]
    [TestCase('filter, HTTP.sys', '8937,/fspa')]
    [TestCase('bundle, TestServer', '8888,/fapp')]
    [TestCase('bundle, Indy Direct', '8935,/fapp')]
    [TestCase('bundle, HTTP.sys', '8937,/fapp')]
    procedure ASinglePageApplicationAnswersItsClientRoutes(const APort: Integer; const APrefix: string);
    [Test]
    [TestCase('middleware, TestServer', '8888,/spa')]
    [TestCase('filter, TestServer', '8888,/fspa')]
    [TestCase('filter, Indy Direct', '8935,/fspa')]
    [TestCase('filter, HTTP.sys', '8937,/fspa')]
    [TestCase('bundle, TestServer', '8888,/fapp')]
    [TestCase('bundle, Indy Direct', '8935,/fapp')]
    [TestCase('bundle, HTTP.sys', '8937,/fapp')]
    procedure TheSPAFallbackDoesNotEscapeTheDocumentRoot(const APort: Integer; const APrefix: string);

    // what the filter does and the middleware does not
    [Test]
    [TestCase('TestServer', '8888')]
    [TestCase('IndyDirect', '8935')]
    [TestCase('HttpSys', '8937')]
    procedure TheStaticFilterSendsLastModifiedAndAnswers304(const APort: Integer);
    [Test]
    [TestCase('TestServer', '8888')]
    [TestCase('IndyDirect', '8935')]
    [TestCase('HttpSys', '8937')]
    procedure ThePrefixIsCaseInsensitive(const APort: Integer);
    [Test]
    [TestCase('TestServer', '8888')]
    [TestCase('IndyDirect', '8935')]
    [TestCase('HttpSys', '8937')]
    procedure TheRoutesOfTheApplicationAreNotHiddenByTheSPA(const APort: Integer);

    [Test]
    [TestCase('TestServer', '8888')]
    [TestCase('IndyDirect', '8935')]
    [TestCase('HttpSys', '8937')]
    procedure RangeMediaFindsAFolderWrittenWithASlash(const APort: Integer);

    // FileServerBundle: the files of StaticFiles, the 206 of RangeMedia, the SPA
    [Test]
    [TestCase('TestServer', '8888')]
    [TestCase('IndyDirect', '8935')]
    [TestCase('HttpSys', '8937')]
    procedure FileServerBundleSendsTheMIMETypeLastModifiedAnd304(const APort: Integer);
    [Test]
    [TestCase('TestServer', '8888')]
    [TestCase('IndyDirect', '8935')]
    [TestCase('HttpSys', '8937')]
    procedure FileServerBundleAnswersARangeWith206(const APort: Integer);
    [Test]
    [TestCase('TestServer', '8888')]
    [TestCase('IndyDirect', '8935')]
    [TestCase('HttpSys', '8937')]
    procedure FileServerBundleAnswersAHeadWithARangeWithoutABody(const APort: Integer);
    [Test]
    [TestCase('TestServer', '8888')]
    [TestCase('IndyDirect', '8935')]
    [TestCase('HttpSys', '8937')]
    procedure FileServerBundleSPAKeepsTheRoutesOfTheAPI(const APort: Integer);
    [Test]
    [TestCase('TestServer', '8888')]
    [TestCase('IndyDirect', '8935')]
    [TestCase('HttpSys', '8937')]
    procedure FileServerBundleAppliesItsOptions(const APort: Integer);
    [Test]
    // Apache and IIS negotiate / strip Content-Encoding independently of the application, so the
    // encoding that was chosen is not echoed back (see TestRenderWrappedListWithCompression)
    [Category('NotOnApache,NotOnIIS')]
    [TestCase('TestServer', '8888')]
    [TestCase('IndyDirect', '8935')]
    [TestCase('HttpSys', '8937')]
    procedure CompressionTakesTheFirstEncodingTheClientLists(const APort: Integer);
    [Test]
    [TestCase('TestServer', '8888')]
    [TestCase('IndyDirect', '8935')]
    [TestCase('HttpSys', '8937')]
    procedure ETagAnswers304ToIfNoneMatch(const APort: Integer);
    // the answer of a controller, not a file
    [Test]
    // Apache and IIS negotiate / strip Content-Encoding independently of the application (see the
    // test of the order of the encodings above)
    [Category('NotOnApache,NotOnIIS')]
    [TestCase('TestServer', '8888')]
    [TestCase('IndyDirect', '8935')]
    [TestCase('HttpSys', '8937')]
    procedure CompressionCompressesTheAnswerOfAController(const APort: Integer);
    [Test]
    [TestCase('TestServer', '8888')]
    [TestCase('IndyDirect', '8935')]
    [TestCase('HttpSys', '8937')]
    procedure ETagAnswers304ForTheAnswerOfAController(const APort: Integer);
    [Test]
    [TestCase('TestServer', '8888')]
    [TestCase('IndyDirect', '8935')]
    [TestCase('HttpSys', '8937')]
    procedure AnActionThatRaisesIsAnsweredThroughTheFilters(const APort: Integer);
    [Test]
    [TestCase('TestServer', '8888')]
    [TestCase('IndyDirect', '8935')]
    [TestCase('HttpSys', '8937')]
    procedure AFilterThatRaisesIsAnswered(const APort: Integer);
    // issue #933: the message without a "[ClassName]" prefix, in DEBUG too
    [Test]
    [TestCase('TestServer', '8888')]
    [TestCase('IndyDirect', '8935')]
    [TestCase('HttpSys', '8937')]
    procedure AFilterThatRaisesAnswersAsAnActionThatRaises(const APort: Integer);
    // an exception that is not an EMVCException: a 500, and in DEBUG its message without a "[ClassName]" prefix
    [Test]
    [TestCase('TestServer', '8888')]
    [TestCase('IndyDirect', '8935')]
    [TestCase('HttpSys', '8937')]
    procedure AFilterThatRaisesAnErrorAnswersAsAnActionThatRaisesIt(const APort: Integer);
    // the custom exception handler answers for a filter too, without a controller
    [Test]
    procedure TheExceptionHandlerAnswersWhatAFilterRaises;
    // no session filter: the session cannot be stopped, the exception is still answered
    [Test]
    procedure ASessionExpiredInAFilterWithoutSessionsIsAnswered;
    // a text body without a charset goes out as UTF-8, on Indy Direct as on HTTP.sys
    [Test]
    [TestCase('IndyDirect', 'IndyDirect')]
    [TestCase('HttpSys', 'HttpSys')]
    procedure ATextBodyWithoutCharsetIsUTF8(const AHost: string);
    [Test]
    [TestCase('TestServer', '8888')]
    [TestCase('IndyDirect', '8935')]
    [TestCase('HttpSys', '8937')]
    procedure RedirectAnswers301OrTheDefault302(const APort: Integer);
    [Test]
    [TestCase('TestServer', '8888')]
    [TestCase('IndyDirect', '8935')]
    [TestCase('HttpSys', '8937')]
    procedure TraceEchoesTheCorrelationId(const APort: Integer);
    // Default assigned to a variable that held other options starts from empty ones
    [Test]
    procedure RateLimitOptionsDefaultClearsWhatTheVariableHeld;
    // Compression and ETag replace the body of the response: the stream they replace is freed
    [Test]
    procedure AFilterThatReplacesTheResponseStreamFreesTheOldOne;
    // the classic ETag middleware empties the body of its 304 with Size := 0: the 206 of a file too.
    // An engine of its own (the middleware would act on every answer of the shared ones), on the port of the test
    // above
    [Test]
    [TestCase('IndyDirect', 'IndyDirect')]
    [TestCase('HttpSys', 'HttpSys')]
    procedure TheETagMiddlewareAnswersARangeWithA304WithoutABody(const AHost: string);
  end;

  [TestFixture]
  THTTPFilterSessionTests = class
  private
    fSessionFolder: string;
    fSessionDatabase: string;
    fIndyEngine: TMVCEngine;
    fHttpSysEngine: TMVCEngine;
    fIndy: IMVCServer;
    fHttpSys: IMVCServer;
  public
    [SetupFixture]
    procedure SetupFixture;
    [TeardownFixture]
    procedure TeardownFixture;

    [Test]
    [TestCase('IndyDirect', '8939')]
    [TestCase('HttpSys', '8941')]
    procedure AMemorySessionFilterKeepsTheSessionOfAController(const APort: Integer);
    [Test]
    [TestCase('IndyDirect', '8939')]
    [TestCase('HttpSys', '8941')]
    procedure AFileSessionFilterWritesTheSessionUnderItsFolder(const APort: Integer);
    [Test]
    [TestCase('IndyDirect', '8939')]
    [TestCase('HttpSys', '8941')]
    procedure TheCookieFlagsAreTheOnesAsked(const APort: Integer);
    [Test]
    [TestCase('IndyDirect', '8939')]
    [TestCase('HttpSys', '8941')]
    procedure ADatabaseSessionFilterKeepsTheSessionOfAController(const APort: Integer);
    [Test]
    [TestCase('IndyDirect', '8939')]
    [TestCase('HttpSys', '8941')]
    procedure ActiveRecordFilterGivesAControllerAConnectionAndTakesItBack(const APort: Integer);
  end;

  // What the Trace filter and the classic Trace middleware log of a body (issue #931): its bytes decoded with the
  // charset of its Content-Type, UTF-8 when it names none, and never an exception out of the trace. During each test
  // the global logger is one in memory, and the server an in-process Indy Direct of its own.
  [TestFixture]
  THTTPFilterTraceTests = class
  private
    fAppender: TLoggerProMemoryRingBufferAppender;
    fVerbosity: TLogLevel;
    fEngine: TMVCEngine;
    fServer: IMVCServer;
    procedure Listen(const AKind: string; const AMaxBodySize: Integer; const AHost: string = 'IndyDirect';
      const AFlags: TMVCTraceFlags = []);
    function LoggedBody(const AMarker: string): string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    [TestCase('filter', 'filter')]
    [TestCase('middleware', 'middleware')]
    procedure AUTF8BodyIsLoggedIntact(const AKind: string);
    // Indy keeps the charset apart from the ContentType: the adapter gives it back as the other hosts do
    [Test]
    [TestCase('filter.IndyDirect', 'filter,IndyDirect')]
    [TestCase('middleware.IndyDirect', 'middleware,IndyDirect')]
    [TestCase('filter.HttpSys', 'filter,HttpSys')]
    [TestCase('middleware.HttpSys', 'middleware,HttpSys')]
    procedure ABodyIsLoggedInTheCharsetOfItsContentType(const AKind, AHost: string);
    [Test]
    [TestCase('filter', 'filter')]
    [TestCase('middleware', 'middleware')]
    procedure ACharacterCutByTheSizeLimitIsLeftOut(const AKind: string);
    [Test]
    [TestCase('filter', 'filter')]
    [TestCase('middleware', 'middleware')]
    procedure ARequestBodyNotInItsCharsetIsAnswered(const AKind: string);
    // charset="utf-8" is as valid as charset=utf-8 (RFC 9110)
    [Test]
    [TestCase('filter', 'filter')]
    [TestCase('middleware', 'middleware')]
    procedure AQuotedCharsetIsDecoded(const AKind: string);
    [Test]
    [TestCase('filter', 'filter')]
    [TestCase('middleware', 'middleware')]
    procedure ABodyWithoutCharsetIsLoggedAsItWasWritten(const AKind: string);
    // Indy kept the previous charset of the response (the default one) when the new Content-Type had none
    [Test]
    [TestCase('IndyDirect', 'IndyDirect')]
    [TestCase('HttpSys', 'HttpSys')]
    procedure TheContentTypeIsSentAsItWasSet(const AHost: string);
    // the middleware takes an unsigned size: only the filter can get a negative one
    [Test]
    procedure ANegativeSizeLimitDoesNotBreakAStreamResponse;
    // passwords, tokens and secrets in a body are masked (the filter's tfLogBodySecrets logs them raw)
    [Test]
    [TestCase('filter', 'filter')]
    [TestCase('middleware', 'middleware')]
    procedure SecretsInAJSONBodyAreMasked(const AKind: string);
    [Test]
    [TestCase('filter', 'filter')]
    [TestCase('middleware', 'middleware')]
    procedure ASecretCutByTheSizeLimitIsMasked(const AKind: string);
    [Test]
    [TestCase('filter', 'filter')]
    [TestCase('middleware', 'middleware')]
    procedure SecretsInAFormBodyAreMasked(const AKind: string);
    [Test]
    [TestCase('filter', 'filter')]
    [TestCase('middleware', 'middleware')]
    procedure ASecretInAResponseIsMasked(const AKind: string);
    [Test]
    procedure LogBodySecretsLogsThemRaw;
    [Test]
    [TestCase('filter', 'filter')]
    [TestCase('middleware', 'middleware')]
    procedure SecretsInArraysObjectsAndXMLAreMasked(const AKind: string);
    [Test]
    procedure ASecretInTheQueryStringIsMasked;
  end;

implementation

uses
  System.Classes,
  System.IOUtils,
  System.Generics.Collections,
  IdTCPClient,
  LoggerPro,
  LoggerPro.Builder,
  MVCFramework.Middleware.Trace,
  FireDAC.Comp.Client,
  JsonDataObjects,
  FireDAC.Phys.SQLite,
  MVCFramework.SQLGenerators.Sqlite,
  MVCFramework.Server.Factory,
  MVCFramework.RESTClient,
  MVCFramework.RESTClient.Intf,
  MVCFramework.Filters.ActiveRecord,
  MVCFramework.ActiveRecord,
  MVCFramework.Middleware.ETag,
  MVCFramework.MinimalAPI,
  HTTPFilterSupportU;

type
  // counts its live instances: the response must free the one a filter replaces
  TCountedStream = class(TStringStream)
  public
    class var Alive: Integer;
    constructor Create;
    destructor Destroy; override;
  end;

  [MVCPath('/counted')]
  TCountedStreamController = class(TMVCController)
  public
    [MVCPath('/($Kind)')]
    [MVCHTTPMethod([httpGET])]
    function GetCounted(const Kind: string): TStream;
  end;

constructor TCountedStream.Create;
begin
  inherited Create(StringOfChar('x', 2048));
  AtomicIncrement(Alive);
end;

destructor TCountedStream.Destroy;
begin
  AtomicDecrement(Alive);
  inherited;
end;

function TCountedStreamController.GetCounted(const Kind: string): TStream;
begin
  ContentType := TMVCMediaType.TEXT_PLAIN;
  Result := TCountedStream.Create;
end;

const
  TEXT_OF_THE_FILES = 'This is a TEXT file';
  AR_CONDEF = 'httpfilter_tests_ar';
  AR_SESSION_CONDEF = 'httpfilter_tests_ar_sessions';

function Client(const APort: Integer): IMVCRESTClient;
begin
  Result := TMVCRESTClient.New.BaseURL('http://localhost:' + APort.ToString);
end;

// the folder that the TestServer serves, copied for the in-process servers
procedure WriteTheFiles(const ARoot: string);
var
  I: Integer;
  lBig: string;
begin
  TDirectory.CreateDirectory(TPath.Combine(ARoot, 'folder1'));
  TDirectory.CreateDirectory(TPath.Combine(ARoot, 'media'));
  TFile.WriteAllText(TPath.Combine(ARoot, 'index.html'), TEXT_OF_THE_FILES);
  TFile.WriteAllText(TPath.Combine(ARoot, 'folder1.html'), TEXT_OF_THE_FILES);
  TFile.WriteAllText(TPath.Combine(TPath.Combine(ARoot, 'folder1'), 'index.html'), TEXT_OF_THE_FILES);
  lBig := '';
  for I := 1 to 220 do
    lBig := lBig + 'Lorem ipsum dolor sit amet, consectetur adipiscing elit. ';
  TFile.WriteAllText(TPath.Combine(ARoot, 'big.txt'), lBig);
  TFile.WriteAllText(TPath.Combine(TPath.Combine(ARoot, 'media'), 'clip.txt'), StringOfChar('x', 1000));
end;

{ THTTPFilterTests }

procedure THTTPFilterTests.SetupFixture;
begin
  fWww := TPath.Combine(TPath.GetTempPath, 'dmvc_httpfilters_' + TGUID.NewGuid.ToString.Replace('{', '').Replace('}', ''));
  WriteTheFiles(fWww);
  fIndyEngine := TMVCEngine.Create;
  ConfigureHTTPFilterSupport(fIndyEngine, fWww);
  fIndy := TMVCServerFactory.CreateIndyDirect(fIndyEngine);
  fIndy.Listen(HF_INDY_PORT, 'localhost');
  fHttpSysEngine := TMVCEngine.Create;
  ConfigureHTTPFilterSupport(fHttpSysEngine, fWww);
  fHttpSys := TMVCServerFactory.CreateHttpSys(fHttpSysEngine);
  fHttpSys.Listen(HF_HTTPSYS_PORT, 'localhost');
end;

procedure THTTPFilterTests.TeardownFixture;
begin
  // the server does not own the engine
  fIndy.Stop;
  fIndy := nil;
  FreeAndNil(fIndyEngine);
  fHttpSys.Stop;
  fHttpSys := nil;
  FreeAndNil(fHttpSysEngine);
  try
    TDirectory.Delete(fWww, True);
  except
  end;
end;

procedure THTTPFilterTests.FilesFoldersAndTheRedirectOfAFolder(const APort: Integer; const APrefix: string);
var
  lRes: IMVCRESTResponse;
begin
  lRes := Client(APort).Get(APrefix + '/index.html');
  Assert.AreEqual(200, lRes.StatusCode, APrefix + '/index.html');

  lRes := Client(APort).Get(APrefix + '.html');
  Assert.AreEqual(404, lRes.StatusCode, APrefix + '.html is outside of the prefix');

  lRes := Client(APort).HandleRedirects(False).Get(APrefix);
  Assert.AreEqual(301, lRes.StatusCode, APrefix);
  Assert.AreEqual(APrefix + '/', lRes.HeaderValue('Location'), 'where the folder redirect goes');

  lRes := Client(APort).Get(APrefix + '/');
  Assert.AreEqual(200, lRes.StatusCode, APrefix + '/');

  lRes := Client(APort).HandleRedirects(False).Get(APrefix + '/folder1');
  Assert.AreEqual(301, lRes.StatusCode, APrefix + '/folder1');

  lRes := Client(APort).Get(APrefix + '/folder1/');
  Assert.AreEqual(200, lRes.StatusCode, APrefix + '/folder1/');

  lRes := Client(APort).Get(APrefix + '/folder1.html');
  Assert.AreEqual(200, lRes.StatusCode, APrefix + '/folder1.html');
  Assert.AreEqual(TEXT_OF_THE_FILES, lRes.Content, APrefix + '/folder1.html');
end;

procedure THTTPFilterTests.ATraversalIsRefused(const APort: Integer; const APrefix: string);

  procedure Refused(const APath: string);
  var
    lRes: IMVCRESTResponse;
  begin
    lRes := Client(APort).HandleRedirects(False).Get(APath);
    Assert.IsTrue(lRes.StatusCode >= 400,
      APath + ' must not be served (got ' + lRes.StatusCode.ToString + ')');
  end;

begin
  Refused(APrefix + '/../../secret.txt');
  Refused(APrefix + '/%2e%2e%2f%2e%2e%2fsecret.txt');
  Refused(APrefix + '/....//secret.txt');
  // a drive letter or a UNC share leaves the document root without a single dot
  Refused(APrefix + '/C:/Windows/win.ini');
  Refused(APrefix + '////attacker.example/share/');
  // and a legitimate file is still served
  Assert.AreEqual(200, Client(APort).Get(APrefix + '/index.html').StatusCode, 'a legitimate file is still served');
end;

procedure THTTPFilterTests.BackslashTraversalAnswers404Or403(const APort: Integer; const APrefix: string);
var
  lRes: IMVCRESTResponse;
  I: Integer;
  lUrl: string;
begin
  lRes := Client(APort).Get(APrefix + '/..\donotdeleteme.txt');
  Assert.IsTrue((lRes.StatusCode = 404) or (lRes.StatusCode = 403),
    Format('Directory traversal should return 404 or 403, got %d', [lRes.StatusCode]));
  lUrl := 'Windows\win.ini';
  for I := 1 to 30 do
  begin
    lUrl := '..\' + lUrl;
    lRes := Client(APort).Get(APrefix + '/' + lUrl);
    Assert.IsTrue((lRes.StatusCode = 404) or (lRes.StatusCode = 403),
      Format('Directory traversal should return 404 or 403, got %d for: %s/%s', [lRes.StatusCode, APrefix, lUrl]));
  end;
end;

procedure THTTPFilterTests.ASinglePageApplicationAnswersItsClientRoutes(const APort: Integer; const APrefix: string);
var
  lRes: IMVCRESTResponse;
begin
  lRes := Client(APort).Get(APrefix + '/index.html');
  Assert.AreEqual(200, lRes.StatusCode);
  Assert.Contains(lRes.Content, TEXT_OF_THE_FILES);

  lRes := Client(APort).Get(APrefix + '/');
  Assert.AreEqual(200, lRes.StatusCode, APrefix + '/');
  Assert.Contains(lRes.Content, TEXT_OF_THE_FILES);

  lRes := Client(APort).Get(APrefix + '/pippo/pluto/paperino');
  Assert.AreEqual(200, lRes.StatusCode, APrefix + '/pippo/pluto/paperino');
  Assert.Contains(lRes.Content, TEXT_OF_THE_FILES);
end;

procedure THTTPFilterTests.TheSPAFallbackDoesNotEscapeTheDocumentRoot(const APort: Integer; const APrefix: string);
var
  lRes: IMVCRESTResponse;
begin
  // on a miss the fallback walks up the tree looking for an index.html: from a path that has already
  // escaped the document root it would serve a file from above it
  lRes := Client(APort).HandleRedirects(False).Get(APrefix + '/%2e%2e%2f%2e%2e%2f');
  Assert.IsTrue(lRes.StatusCode >= 400,
    'the SPA fallback must not walk above the document root (got ' + lRes.StatusCode.ToString + ')');
  lRes := Client(APort).Get(APrefix + '/some/client/route');
  Assert.AreEqual(200, lRes.StatusCode, 'the SPA fallback must still work');
end;

procedure THTTPFilterTests.TheStaticFilterSendsLastModifiedAndAnswers304(const APort: Integer);
var
  lRes: IMVCRESTResponse;
  lLastModified: string;
begin
  lRes := Client(APort).Get('/fstatic/folder1.html');
  Assert.AreEqual(200, lRes.StatusCode);
  lLastModified := lRes.HeaderValue('Last-Modified');
  Assert.AreNotEqual('', lLastModified, 'Last-Modified');
  lRes := Client(APort).AddHeader('If-Modified-Since', lLastModified).Get('/fstatic/folder1.html');
  Assert.AreEqual(304, lRes.StatusCode, 'If-Modified-Since');
end;

procedure THTTPFilterTests.ThePrefixIsCaseInsensitive(const APort: Integer);
begin
  Assert.AreEqual(200, Client(APort).Get('/FSTATIC/folder1.html').StatusCode);
end;

procedure THTTPFilterTests.TheRoutesOfTheApplicationAreNotHiddenByTheSPA(const APort: Integer);
var
  lRes: IMVCRESTResponse;
begin
  // the application under the prefix of the SPA: the router goes first
  lRes := Client(APort).Get('/fspa/api/ping');
  Assert.AreEqual(200, lRes.StatusCode, 'a route of the application');
  Assert.Contains(lRes.Content, 'pong');

  // the 404 that an action chooses stays what it is
  lRes := Client(APort).Get('/fspa/api/gone');
  Assert.AreEqual(404, lRes.StatusCode, 'the 404 of an action');
  Assert.IsFalse(lRes.Content.Contains(TEXT_OF_THE_FILES), 'the 404 of an action is not index.html');

  // only a GET or a HEAD without a route becomes the page
  Assert.AreEqual(404, Client(APort).Post('/fspa/nothing-here', '').StatusCode, 'a POST without a route');
end;

procedure THTTPFilterTests.RangeMediaFindsAFolderWrittenWithASlash(const APort: Integer);
var
  lRes: IMVCRESTResponse;
begin
  lRes := Client(APort).AddHeader('Range', 'bytes=0-9').Get('/fmedia/clip.txt');
  Assert.AreEqual(206, lRes.StatusCode, 'bytes=0-9');
  Assert.AreEqual(10, Integer(Length(lRes.ContentRawBytes)), 'the ten bytes');
  Assert.AreEqual('bytes 0-9/1000', lRes.HeaderValue('Content-Range'), 'Content-Range');
  Assert.AreEqual(200, Client(APort).Get('/fmedia/clip.txt').StatusCode, 'no Range header');
end;

procedure THTTPFilterTests.FileServerBundleSendsTheMIMETypeLastModifiedAnd304(const APort: Integer);
var
  lRes: IMVCRESTResponse;
  lLastModified: string;
begin
  lRes := Client(APort).Get('/ffiles/folder1.html');
  Assert.AreEqual(200, lRes.StatusCode);
  Assert.IsTrue(lRes.ContentType.ToLower.StartsWith('text/html'), 'the MIME type, got ' + lRes.ContentType);
  Assert.AreEqual(TEXT_OF_THE_FILES, lRes.Content);
  lLastModified := lRes.HeaderValue('Last-Modified');
  Assert.AreNotEqual('', lLastModified, 'Last-Modified');
  lRes := Client(APort).AddHeader('If-Modified-Since', lLastModified).Get('/ffiles/folder1.html');
  Assert.AreEqual(304, lRes.StatusCode, 'If-Modified-Since');
end;

procedure THTTPFilterTests.FileServerBundleAnswersARangeWith206(const APort: Integer);
var
  lRes: IMVCRESTResponse;
begin
  // big.txt starts with 'Lorem ipsum'
  lRes := Client(APort).AddHeader('Range', 'bytes=6-10').Get('/ffiles/big.txt');
  Assert.AreEqual(206, lRes.StatusCode, 'bytes=6-10');
  Assert.AreEqual('bytes 6-10/12540', lRes.HeaderValue('Content-Range'), 'Content-Range');
  Assert.AreEqual('ipsum', lRes.Content, 'the five bytes');
  Assert.AreEqual('bytes', lRes.HeaderValue('Accept-Ranges'), 'Accept-Ranges');
  // the MIME table of the files, not the audio/video one of RangeMediaHTTPFilter
  Assert.IsTrue(lRes.ContentType.ToLower.StartsWith('text/plain'), 'the MIME type, got ' + lRes.ContentType);
  lRes := Client(APort).AddHeader('Range', 'bytes=20000-').Get('/ffiles/big.txt');
  Assert.AreEqual(416, lRes.StatusCode, 'a range past the end');
end;

// a HEAD with a Range: the 206 and the Content-Range of the GET, and no body; read by hand, a client library
// would drop a body. Not the Content-Length: on a HEAD no host sends the one of the GET (Indy writes 0, HTTP.sys
// none), for any answer
// sends ARequestLine and AHeaders by hand (a client library would drop a body it does not expect), reads the status
// line and the headers (lowercase), and answers how many bytes follow the headers within a second
function BytesAfterTheHeaders(const APort: Integer; const ARequestLine: string; const AHeaders: array of string;
  out AStatusLine, AResponseHeaders: string): Integer;
var
  lClient: TIdTCPClient;
  lLine: string;
begin
  lClient := TIdTCPClient.Create(nil);
  try
    lClient.Host := '127.0.0.1';
    lClient.Port := APort;
    lClient.ConnectTimeout := 3000;
    lClient.ReadTimeout := 3000;
    lClient.Connect;
    lClient.IOHandler.WriteLn(ARequestLine);
    lClient.IOHandler.WriteLn('Host: localhost');
    for lLine in AHeaders do
      lClient.IOHandler.WriteLn(lLine);
    lClient.IOHandler.WriteLn('Connection: close');
    lClient.IOHandler.WriteLn('');
    AStatusLine := lClient.IOHandler.ReadLn;
    AResponseHeaders := '';
    repeat
      lLine := lClient.IOHandler.ReadLn;
      AResponseHeaders := AResponseHeaders + LowerCase(lLine) + #10;
    until lLine = '';
    try
      lClient.IOHandler.CheckForDataOnSource(1000); // a body would come right after the headers
    except
      // closed by the server: what was read is what came
    end;
    Result := lClient.IOHandler.InputBuffer.Size;
  finally
    lClient.Free;
  end;
end;

procedure THTTPFilterTests.FileServerBundleAnswersAHeadWithARangeWithoutABody(const APort: Integer);
var
  lStatusLine, lHeaders: string;
  lRest: Integer;
begin
  lRest := BytesAfterTheHeaders(APort, 'HEAD /ffiles/big.txt HTTP/1.1', ['Range: bytes=6-10'], lStatusLine, lHeaders);
  Assert.Contains(lStatusLine, ' 206', lStatusLine);
  Assert.Contains(lHeaders, 'content-range: bytes 6-10/12540', lHeaders);
  Assert.AreEqual(0, lRest, 'no body after the headers of a HEAD');
end;

procedure THTTPFilterTests.FileServerBundleSPAKeepsTheRoutesOfTheAPI(const APort: Integer);
var
  lRes: IMVCRESTResponse;
begin
  lRes := Client(APort).Get('/fapp/api/info');
  Assert.AreEqual(200, lRes.StatusCode, 'the route of the API');
  Assert.IsTrue(lRes.ContentType.ToLower.StartsWith('application/json'), 'JSON, got ' + lRes.ContentType);
  Assert.Contains(lRes.Content, '"from":"api"');

  lRes := Client(APort).Get('/fapp/some/client/route');
  Assert.AreEqual(200, lRes.StatusCode, 'a client route');
  Assert.Contains(lRes.Content, TEXT_OF_THE_FILES);

  lRes := Client(APort).Post('/fapp/nothing-here', '');
  Assert.AreEqual(404, lRes.StatusCode, 'a POST without a route');
  Assert.IsFalse(lRes.Content.Contains(TEXT_OF_THE_FILES), 'a POST without a route is not index.html');
end;

procedure THTTPFilterTests.FileServerBundleAppliesItsOptions(const APort: Integer);
var
  lRes: IMVCRESTResponse;
begin
  Assert.AreEqual(200, Client(APort).Get('/fstrict/big.txt').StatusCode, 'the prefix as registered');
  Assert.AreEqual(404, Client(APort).Get('/FSTRICT/big.txt').StatusCode, 'fsfCaseSensitivePrefix');
  lRes := Client(APort).AddHeader('Range', 'bytes=0-3').Get('/fstrict/big.txt');
  Assert.AreEqual(206, lRes.StatusCode, 'fsfRanges');
  // the rules see the requests with Range too
  lRes := Client(APort).AddHeader('Range', 'bytes=0-3').Get('/fstrict/folder1.html');
  Assert.AreEqual(403, lRes.StatusCode, 'refused by the rule, with a Range header');
  Assert.AreEqual(403, Client(APort).Get('/fstrict/folder1.html').StatusCode, 'refused by the rule');
end;

procedure THTTPFilterTests.CompressionTakesTheFirstEncodingTheClientLists(const APort: Integer);
var
  lRes: IMVCRESTResponse;
begin
  lRes := Client(APort).AcceptEncoding('deflate, gzip').Get('/fzip/big.txt');
  Assert.AreEqual(200, lRes.StatusCode);
  Assert.AreEqual('deflate', lRes.HeaderValue('Content-Encoding').ToLower, 'deflate, gzip');
  Assert.AreEqual(12540, Integer(Length(lRes.ContentRawBytes)), 'the body decompresses to the file');

  lRes := Client(APort).AcceptEncoding('gzip, deflate').Get('/fzip/big.txt');
  Assert.AreEqual('gzip', lRes.HeaderValue('Content-Encoding').ToLower, 'gzip, deflate');

  lRes := Client(APort).AcceptEncoding('').Get('/fzip/big.txt');
  Assert.AreEqual('', lRes.HeaderValue('Content-Encoding'), 'no Accept-Encoding');
end;

procedure THTTPFilterTests.ETagAnswers304ToIfNoneMatch(const APort: Integer);
var
  lRes: IMVCRESTResponse;
  lETag: string;
begin
  lRes := Client(APort).Get('/fetag/folder1.html');
  Assert.AreEqual(200, lRes.StatusCode);
  lETag := lRes.HeaderValue('ETag');
  Assert.IsTrue(lETag.StartsWith('"') and lETag.EndsWith('"'), 'a quoted ETag, got ' + lETag);
  lRes := Client(APort).AddHeader('If-None-Match', lETag).Get('/fetag/folder1.html');
  Assert.AreEqual(304, lRes.StatusCode, 'If-None-Match');
end;

procedure THTTPFilterTests.CompressionCompressesTheAnswerOfAController(const APort: Integer);
var
  lRes: IMVCRESTResponse;
begin
  lRes := Client(APort).AcceptEncoding('gzip').Get('/fzip/api/big');
  Assert.AreEqual(200, lRes.StatusCode);
  Assert.AreEqual('gzip', lRes.HeaderValue('Content-Encoding').ToLower, 'the answer of the controller is compressed');
  Assert.AreEqual(12540, Integer(Length(lRes.ContentRawBytes)), 'and it decompresses to the whole text');
end;

procedure THTTPFilterTests.ETagAnswers304ForTheAnswerOfAController(const APort: Integer);
var
  lRes: IMVCRESTResponse;
  lETag: string;
begin
  lRes := Client(APort).Get('/fetag/api/big');
  Assert.AreEqual(200, lRes.StatusCode);
  lETag := lRes.HeaderValue('ETag');
  Assert.AreNotEqual('', lETag, 'an ETag for the answer of a controller');
  lRes := Client(APort).AddHeader('If-None-Match', lETag).Get('/fetag/api/big');
  Assert.AreEqual(304, lRes.StatusCode, 'If-None-Match');
end;

procedure THTTPFilterTests.AnActionThatRaisesIsAnsweredThroughTheFilters(const APort: Integer);
var
  lRes: IMVCRESTResponse;
begin
  // the engine turns the exception into a 500 inside the dispatcher, so the code of CORS after ANext()
  // still runs and a browser can read the error
  lRes := Client(APort).AddHeader('Origin', 'http://example.com').Get('/fexc/boom');
  Assert.AreEqual(500, lRes.StatusCode);
  Assert.AreEqual('*', lRes.HeaderValue('Access-Control-Allow-Origin'), 'CORS on the 500');
end;

procedure THTTPFilterTests.AFilterThatRaisesIsAnswered(const APort: Integer);
begin
  // nothing inside the dispatcher can catch what a filter raises: the engine answers
  Assert.AreEqual(403, Client(APort).Get('/fexc/filter-raises').StatusCode);
end;

procedure THTTPFilterTests.AFilterThatRaisesAnswersAsAnActionThatRaises(const APort: Integer);
var
  lAction, lFilter: IMVCRESTResponse;
  lActionBody, lFilterBody: TJsonObject;
begin
  lAction := Client(APort).Get('/fexc/toomany');
  lFilter := Client(APort).Get('/fexc/filter-toomany');
  Assert.AreEqual(429, lAction.StatusCode, 'the action');
  Assert.AreEqual(429, lFilter.StatusCode, 'the filter');
  Assert.AreEqual(lAction.ContentType, lFilter.ContentType, 'content type');
  lActionBody := StrToJSONObject(lAction.Content);
  lFilterBody := StrToJSONObject(lFilter.Content);
  try
    Assert.IsNotNull(lActionBody, lAction.Content);
    Assert.IsNotNull(lFilterBody, lFilter.Content);
    Assert.AreEqual('Too many attempts', lFilterBody.S['message'], lFilter.Content);
    Assert.AreEqual(lActionBody.S['message'], lFilterBody.S['message'], 'message');
    // the class name only in DEBUG builds, for the filter as for the action
    Assert.AreEqual(lActionBody.S['classname'], lFilterBody.S['classname'], 'classname');
  finally
    lActionBody.Free;
    lFilterBody.Free;
  end;
end;

procedure THTTPFilterTests.AFilterThatRaisesAnErrorAnswersAsAnActionThatRaisesIt(const APort: Integer);
var
  lAction, lFilter: IMVCRESTResponse;
  lActionBody, lFilterBody: TJsonObject;
begin
  lAction := Client(APort).Get('/fexc/boom');
  lFilter := Client(APort).Get('/fexc/filter-boom');
  Assert.AreEqual(500, lAction.StatusCode, 'the action');
  Assert.AreEqual(500, lFilter.StatusCode, 'the filter');
  lActionBody := StrToJSONObject(lAction.Content);
  lFilterBody := StrToJSONObject(lFilter.Content);
  try
    Assert.IsNotNull(lActionBody, lAction.Content);
    Assert.IsNotNull(lFilterBody, lFilter.Content);
    Assert.AreEqual(lActionBody.S['message'], lFilterBody.S['message'], 'message');
    Assert.AreEqual(lActionBody.S['classname'], lFilterBody.S['classname'], 'classname');
  finally
    lActionBody.Free;
    lFilterBody.Free;
  end;
end;

procedure THTTPFilterTests.TheExceptionHandlerAnswersWhatAFilterRaises;
var
  lEngine: TMVCEngine;
  lServer: IMVCServer;
  lRes: IMVCRESTResponse;
  lCalls: Integer;
  lWithoutController: Boolean;
  lRaise: TMVCHTTPFilter;
begin
  lCalls := 0;
  lWithoutController := False;
  lEngine := TMVCEngine.Create;
  try
    lEngine.SetExceptionHandler(
      procedure(E: Exception; SelectedController: TMVCController; WebContext: TWebContext; var Handled: Boolean)
      begin
        Inc(lCalls);
        lWithoutController := SelectedController = nil;
        WebContext.Response.ContentType := TMVCMediaType.TEXT_PLAIN;
        WebContext.Response.Content := 'handled: ' + E.Message;
        WebContext.Response.StatusCode := http_status.Conflict;
        Handled := True;
      end);
    lRaise :=
      procedure(const AContext: TWebContext; const ANext: TMVCHTTPFilterNext)
      begin
        raise EMVCException.Create(http_status.TooManyRequests, 'Too many attempts');
      end;
    lEngine.Use(lRaise);
    lServer := TMVCServerFactory.CreateIndyDirect(lEngine);
    try
      lServer.Listen(HF_STREAM_INDY_PORT, 'localhost');
      lRes := Client(HF_STREAM_INDY_PORT).Get('/anything');
    finally
      lServer.Stop;
      lServer := nil;
    end;
  finally
    lEngine.Free;
  end;
  Assert.AreEqual(1, lCalls, 'the handler is called once');
  Assert.IsTrue(lWithoutController, 'SelectedController = nil');
  Assert.AreEqual(409, lRes.StatusCode);
  Assert.AreEqual('handled: Too many attempts', lRes.Content);
end;

procedure THTTPFilterTests.ATextBodyWithoutCharsetIsUTF8(const AHost: string);
var
  lEngine: TMVCEngine;
  lServer: IMVCServer;
  lRes: IMVCRESTResponse;
  lAnswer: TMVCHTTPFilter;
begin
  lEngine := TMVCEngine.Create;
  try
    lAnswer :=
      procedure(const AContext: TWebContext; const ANext: TMVCHTTPFilterNext)
      begin
        AContext.Response.ContentType := TMVCMediaType.TEXT_PLAIN;
        AContext.Response.Content := 'Perch'#$00E9' '#$20AC;
      end;
    lEngine.Use(lAnswer);
    if AHost = 'HttpSys' then
      lServer := TMVCServerFactory.CreateHttpSys(lEngine)
    else
      lServer := TMVCServerFactory.CreateIndyDirect(lEngine);
    try
      lServer.Listen(HF_STREAM_INDY_PORT, 'localhost');
      lRes := Client(HF_STREAM_INDY_PORT).Get('/anything');
    finally
      lServer.Stop;
      lServer := nil;
    end;
  finally
    lEngine.Free;
  end;
  Assert.AreEqual(Length(TEncoding.UTF8.GetBytes('Perch'#$00E9' '#$20AC)), Length(lRes.ContentRawBytes), 'bytes');
  Assert.AreEqual('Perch'#$00E9' '#$20AC, TEncoding.UTF8.GetString(lRes.ContentRawBytes));
end;

procedure THTTPFilterTests.ASessionExpiredInAFilterWithoutSessionsIsAnswered;
var
  lEngine: TMVCEngine;
  lServer: IMVCServer;
  lRes: IMVCRESTResponse;
  lCalls: Integer;
  lRaise: TMVCHTTPFilter;
begin
  lCalls := 0;
  lEngine := TMVCEngine.Create;
  try
    lEngine.SetExceptionHandler(
      procedure(E: Exception; SelectedController: TMVCController; WebContext: TWebContext; var Handled: Boolean)
      begin
        Inc(lCalls);
      end);
    lRaise :=
      procedure(const AContext: TWebContext; const ANext: TMVCHTTPFilterNext)
      begin
        raise EMVCSessionExpiredException.Create;
      end;
    lEngine.Use(lRaise);
    lServer := TMVCServerFactory.CreateIndyDirect(lEngine);
    try
      lServer.Listen(HF_STREAM_INDY_PORT, 'localhost');
      lRes := Client(HF_STREAM_INDY_PORT).Get('/anything');
    finally
      lServer.Stop;
      lServer := nil;
    end;
  finally
    lEngine.Free;
  end;
  Assert.AreEqual(1, lCalls, 'the handler is called once');
  Assert.AreEqual(http_status.Unauthorized, lRes.StatusCode, lRes.Content);
end;

procedure THTTPFilterTests.RedirectAnswers301OrTheDefault302(const APort: Integer);
var
  lRes: IMVCRESTResponse;
begin
  lRes := Client(APort).HandleRedirects(False).Get('/fold');
  Assert.AreEqual(301, lRes.StatusCode, 'rdkPermanent');
  Assert.AreEqual('/fnew', lRes.HeaderValue('Location'));
  lRes := Client(APort).HandleRedirects(False).Get('/fold2');
  Assert.AreEqual(302, lRes.StatusCode, 'the default kind');
  Assert.AreEqual('/fnew', lRes.HeaderValue('Location'));
end;

procedure THTTPFilterTests.TraceEchoesTheCorrelationId(const APort: Integer);
var
  lRes: IMVCRESTResponse;
begin
  lRes := Client(APort).AddHeader('X-Request-ID', 'abc-123').Get('/ftrace/echo');
  Assert.AreEqual(200, lRes.StatusCode);
  Assert.AreEqual('abc-123', lRes.HeaderValue('X-Request-ID'), 'the id of the caller is echoed');
  lRes := Client(APort).Get('/ftrace/echo');
  Assert.AreNotEqual('', lRes.HeaderValue('X-Request-ID'), 'an id is generated when the caller sends none');
end;

procedure THTTPFilterTests.AFilterThatReplacesTheResponseStreamFreesTheOldOne;
var
  lEngine: TMVCEngine;
  lServer: IMVCServer;
  lRes: IMVCRESTResponse;
  lETag: string;
begin
  TCountedStream.Alive := 0;
  lEngine := TMVCEngine.Create;
  try
    lEngine.AddController(TCountedStreamController);
    lEngine.Use(OnlyFor('/counted/zip', CompressionHTTPFilter(16)));
    lEngine.Use(OnlyFor('/counted/etag', ETagHTTPFilter()));
    lServer := TMVCServerFactory.CreateIndyDirect(lEngine);
    try
      lServer.Listen(HF_STREAM_INDY_PORT, 'localhost');
      lRes := Client(HF_STREAM_INDY_PORT).AddHeader('Accept-Encoding', 'gzip').Get('/counted/zip');
      Assert.AreEqual(200, lRes.StatusCode, 'compressed');
      lRes := Client(HF_STREAM_INDY_PORT).Get('/counted/etag');
      lETag := lRes.HeaderValue('ETag');
      Assert.AreNotEqual('', lETag, 'an ETag');
      lRes := Client(HF_STREAM_INDY_PORT).AddHeader('If-None-Match', lETag).Get('/counted/etag');
      Assert.AreEqual(304, lRes.StatusCode, 'not modified');
    finally
      lServer.Stop; // the responses are freed once sent: stopped, every request is over
      lServer := nil;
    end;
  finally
    lEngine.Free;
  end;
  Assert.AreEqual(0, TCountedStream.Alive, 'response streams left alive');
end;

procedure THTTPFilterTests.TheETagMiddlewareAnswersARangeWithA304WithoutABody(const AHost: string);
var
  lEngine: TMVCEngine;
  lServer: IMVCServer;
  lRes: IMVCRESTResponse;
  lETag, lStatusLine, lHeaders: string;
  lRest: Integer;
begin
  lEngine := TMVCEngine.Create;
  try
    {$WARN SYMBOL_DEPRECATED OFF}
    lEngine.AddMiddleware(TMVCETagMiddleware.Create);
    {$WARN SYMBOL_DEPRECATED DEFAULT}
    lEngine.Use(FileServerBundle('/ffiles', fWww));
    if AHost = 'HttpSys' then
      lServer := TMVCServerFactory.CreateHttpSys(lEngine)
    else
      lServer := TMVCServerFactory.CreateIndyDirect(lEngine);
    try
      lServer.Listen(HF_STREAM_INDY_PORT, 'localhost');
      lRes := Client(HF_STREAM_INDY_PORT).AddHeader('Range', 'bytes=6-10').Get('/ffiles/big.txt');
      Assert.AreEqual(206, lRes.StatusCode, 'the range');
      Assert.AreEqual('ipsum', lRes.Content, 'the five bytes');
      lETag := lRes.HeaderValue('ETag');
      Assert.AreNotEqual('', lETag, 'the ETag of the middleware');
      lRest := BytesAfterTheHeaders(HF_STREAM_INDY_PORT, 'GET /ffiles/big.txt HTTP/1.1',
        ['Range: bytes=6-10', 'If-None-Match: ' + lETag], lStatusLine, lHeaders);
      Assert.Contains(lStatusLine, ' 304', lStatusLine + #10 + lHeaders);
      Assert.AreEqual(0, lRest, 'no body after the headers of a 304: ' + lHeaders);
    finally
      lServer.Stop;
      lServer := nil;
    end;
  finally
    lEngine.Free;
  end;
end;

procedure THTTPFilterTests.RateLimitOptionsDefaultClearsWhatTheVariableHeld;
var
  lOptions: TMVCRateLimitOptions;
begin
  lOptions := TMVCRateLimitOptions.Default;
  lOptions.ExcludedPaths := ['/health'];
  lOptions.APIKeyHeaderName := 'X-Key';
  lOptions.MaxRequests := 5;
  lOptions := TMVCRateLimitOptions.Default;
  Assert.AreEqual(0, Integer(Length(lOptions.ExcludedPaths)), 'ExcludedPaths');
  Assert.AreEqual('', lOptions.APIKeyHeaderName, 'APIKeyHeaderName');
  Assert.AreEqual(60, lOptions.MaxRequests, 'MaxRequests');
end;

type
  // the same controller under four prefixes: each one has a session filter of its own
  TFilterSessionController = class(TMVCController)
  public
    [MVCPath('/set/($AValue)')]
    [MVCHTTPMethod([httpGET])]
    function SetValue(const AValue: string): string;
    [MVCPath('/get')]
    [MVCHTTPMethod([httpGET])]
    function GetValue: string;
  end;

  [MVCPath('/far')]
  TFilterActiveRecordController = class(TMVCController)
  public
    // is there a connection for the action?
    [MVCPath('/has')]
    [MVCHTTPMethod([httpGET])]
    function Has: string;
    [MVCPath('/boom')]
    [MVCHTTPMethod([httpGET])]
    function Boom: string;
  end;

{ TFilterSessionController }

function TFilterSessionController.SetValue(const AValue: string): string;
begin
  Session['k'] := AValue;
  Result := 'set';
end;

function TFilterSessionController.GetValue: string;
begin
  Result := Session['k'];
end;

{ TFilterActiveRecordController }

function TFilterActiveRecordController.Has: string;
begin
  Result := BoolToStr(ActiveRecordConnectionsRegistry.GetCurrent(False) <> nil, True);
end;

function TFilterActiveRecordController.Boom: string;
begin
  raise Exception.Create('boom');
end;

{ THTTPFilterSessionTests }

procedure THTTPFilterSessionTests.SetupFixture;
var
  lParams: TStringList;
  lConnection: TFDConnection;

  function NewEngine: TMVCEngine;
  begin
    Result := TMVCEngine.Create;
    Result.AddController(TFilterSessionController, '/fsess');
    Result.AddController(TFilterSessionController, '/ffsess');
    Result.AddController(TFilterSessionController, '/fcookie');
    Result.AddController(TFilterSessionController, '/fcookiedefault');
    Result.AddController(TFilterSessionController, '/fdbsess');
    Result.AddController(TFilterActiveRecordController);
    Result.Use(OnlyFor('/fsess', MemorySessionHTTPFilter(10)));
    Result.Use(OnlyFor('/ffsess', FileSessionHTTPFilter(10, fSessionFolder)));
    Result.Use(OnlyFor('/fcookie/', MemorySessionHTTPFilter(10, [scfHttpOnly, scfSecure])));
    Result.Use(OnlyFor('/fcookiedefault', MemorySessionHTTPFilter(10)));
    Result.Use(OnlyFor('/far', ActiveRecordHTTPFilter(AR_CONDEF, '')));
    // the connection first, then the sessions that live in it
    Result.Use(OnlyFor('/fdbsess', ActiveRecordHTTPFilter(AR_SESSION_CONDEF, '')));
    Result.Use(OnlyFor('/fdbsess', DatabaseSessionHTTPFilter(10)));
  end;

begin
  fSessionFolder := TPath.Combine(TPath.GetTempPath, 'dmvc_httpfilters_sess_' + TGUID.NewGuid.ToString.Replace('{', '').Replace('}', ''));
  TDirectory.CreateDirectory(fSessionFolder);
  lParams := TStringList.Create;
  try
    lParams.Add('Database=:memory:');
    FDManager.AddConnectionDef(AR_CONDEF, 'SQLite', lParams);
  finally
    lParams.Free;
  end;
  // a database in a file: the sessions have to survive from one request to the next
  fSessionDatabase := TPath.Combine(TPath.GetTempPath, 'dmvc_httpfilters_sess_' + TGUID.NewGuid.ToString.Replace('{', '').Replace('}', '') + '.db');
  lParams := TStringList.Create;
  try
    lParams.Add('Database=' + fSessionDatabase);
    FDManager.AddConnectionDef(AR_SESSION_CONDEF, 'SQLite', lParams);
  finally
    lParams.Free;
  end;
  lConnection := TFDConnection.Create(nil);
  try
    lConnection.ConnectionDefName := AR_SESSION_CONDEF;
    lConnection.Connected := True;
    lConnection.ExecSQL('CREATE TABLE dmvc_sessions (session_id varchar(100) NOT NULL PRIMARY KEY, ' +
      'session_data varchar NOT NULL, session_expiration timestamp, ' +
      'session_created_at timestamp NOT NULL DEFAULT current_timestamp)');
  finally
    lConnection.Free;
  end;
  fIndyEngine := NewEngine;
  fIndy := TMVCServerFactory.CreateIndyDirect(fIndyEngine);
  fIndy.Listen(HF_SESSION_INDY_PORT, 'localhost');
  fHttpSysEngine := NewEngine;
  fHttpSys := TMVCServerFactory.CreateHttpSys(fHttpSysEngine);
  fHttpSys.Listen(HF_SESSION_HTTPSYS_PORT, 'localhost');
end;

procedure THTTPFilterSessionTests.TeardownFixture;
begin
  fIndy.Stop;
  fIndy := nil;
  FreeAndNil(fIndyEngine);
  fHttpSys.Stop;
  fHttpSys := nil;
  FreeAndNil(fHttpSysEngine);
  try
    TDirectory.Delete(fSessionFolder, True);
  except
  end;
  try
    FDManager.DeleteConnectionDef(AR_CONDEF);
    FDManager.DeleteConnectionDef(AR_SESSION_CONDEF);
  except
  end;
  try
    TFile.Delete(fSessionDatabase);
  except
  end;
end;

procedure THTTPFilterSessionTests.AMemorySessionFilterKeepsTheSessionOfAController(const APort: Integer);
var
  lClient: IMVCRESTClient;
begin
  // the same client across the calls, so that the session cookie sticks
  lClient := Client(APort);
  Assert.AreEqual(200, lClient.Get('/fsess/set/foo').StatusCode, 'set');
  Assert.Contains(lClient.Get('/fsess/get').Content, 'foo', 'the session survives to the next request');
end;

procedure THTTPFilterSessionTests.AFileSessionFilterWritesTheSessionUnderItsFolder(const APort: Integer);
var
  lClient: IMVCRESTClient;
begin
  lClient := Client(APort);
  Assert.AreEqual(200, lClient.Get('/ffsess/set/bar').StatusCode, 'set');
  Assert.Contains(lClient.Get('/ffsess/get').Content, 'bar', 'the session survives to the next request');
  Assert.IsTrue(Length(TDirectory.GetFiles(fSessionFolder)) > 0, 'a session file under ' + fSessionFolder);
end;

procedure THTTPFilterSessionTests.TheCookieFlagsAreTheOnesAsked(const APort: Integer);
var
  lCookies: TCookies;
begin
  lCookies := Client(APort).Get('/fcookie/set/x').Cookies;
  Assert.IsTrue(lCookies.Count > 0, 'a cookie');
  Assert.IsTrue(lCookies[0].HttpOnly and lCookies[0].Secure, '[scfHttpOnly, scfSecure]: HttpOnly and Secure');

  lCookies := Client(APort).Get('/fcookiedefault/set/x').Cookies;
  Assert.IsTrue(lCookies.Count > 0, 'a cookie');
  Assert.IsTrue(lCookies[0].HttpOnly and (not lCookies[0].Secure), 'the default: HttpOnly, not Secure');
end;

procedure THTTPFilterSessionTests.ADatabaseSessionFilterKeepsTheSessionOfAController(const APort: Integer);
var
  lClient: IMVCRESTClient;
begin
  lClient := Client(APort);
  Assert.AreEqual(200, lClient.Get('/fdbsess/set/baz').StatusCode, 'set');
  Assert.Contains(lClient.Get('/fdbsess/get').Content, 'baz', 'the session is read back from the database');
end;

procedure THTTPFilterSessionTests.ActiveRecordFilterGivesAControllerAConnectionAndTakesItBack(const APort: Integer);
var
  lRes: IMVCRESTResponse;
begin
  lRes := Client(APort).Get('/far/has');
  Assert.AreEqual(200, lRes.StatusCode);
  Assert.Contains(lRes.Content, 'True', 'the action has the default connection');

  Assert.AreEqual(500, Client(APort).Get('/far/boom').StatusCode, 'a raising action answers 500');

  // the connection that a skipped release would leave behind would make this call raise "already registered"
  lRes := Client(APort).Get('/far/has');
  Assert.AreEqual(200, lRes.StatusCode);
  Assert.Contains(lRes.Content, 'True', 'the connection is given back when the action raised');
end;

{ THTTPFilterTraceTests }

type
  [MVCPath('/trace931')]
  TTraceCharsetController = class(TMVCController)
  public
    [MVCPath('/json')]
    [MVCHTTPMethod([httpGET])]
    function Json: string;
    // in ISO-8859-9 (Turkish): read as the Windows ANSI code page, its bytes are other letters
    [MVCPath('/latin5')]
    [MVCHTTPMethod([httpGET])]
    function Latin5: TStream;
    [MVCPath('/body')]
    [MVCHTTPMethod([httpPOST])]
    function Body: string;
    // no charset in the Content-Type: Render(string) writes the Windows ANSI code page
    [MVCPath('/ansi')]
    [MVCHTTPMethod([httpGET])]
    function Ansi: string;
    [MVCPath('/token')]
    [MVCHTTPMethod([httpGET])]
    function Token: string;
  end;

const
  TRACE_TAG = 'trace931';
  // "Cikis yapildi" with its Turkish letters (the text of the issue), by code point: the unit has no BOM
  TRACE_TURKISH = #$00C7#$0131'k'#$0131#$015F' yap'#$0131'ld'#$0131;
  TRACE_JSON = '{"message":"' + TRACE_TURKISH + '"}';
  TRACE_ANSI = 'Perch'#$00E9' citt'#$00E0;
  TRACE_LOGIN = '{"user":"daniele","password":"s3\"cr3t","refresh_token":"abc.def","attempts":3,"api_key":42}';
  TRACE_LOGIN_MASKED = '{"user":"daniele","password":"***","refresh_token":"***","attempts":3,"api_key":"***"}';

function TTraceCharsetController.Json: string;
begin
  ContentType := TMVCMediaType.APPLICATION_JSON + '; charset=utf-8';
  Result := TRACE_JSON;
end;

function TTraceCharsetController.Latin5: TStream;
var
  lEncoding: TEncoding;
begin
  ContentType := TMVCMediaType.TEXT_PLAIN + '; charset=iso-8859-9';
  lEncoding := TEncoding.GetEncoding('iso-8859-9');
  try
    Result := TBytesStream.Create(lEncoding.GetBytes(TRACE_TURKISH));
  finally
    lEncoding.Free;
  end;
end;

function TTraceCharsetController.Body: string;
begin
  Result := 'ok';
end;

function TTraceCharsetController.Token: string;
begin
  ContentType := TMVCMediaType.APPLICATION_JSON + '; charset=utf-8';
  Result := '{"token":"xyz","user":"daniele"}';
end;

function TTraceCharsetController.Ansi: string;
begin
  ContentType := TMVCMediaType.TEXT_PLAIN;
  Result := TRACE_ANSI;
end;

// the line of the body, as the filter and the middleware write it
function ResponseBodyMarker(const AKind: string): string;
begin
  if AKind = 'middleware' then
    Result := '[AFTER ACTION][RESPONSE][BODY] '
  else
    Result := '[TRACE][RES][BODY] ';
end;

function RequestBodyMarker(const AKind: string): string;
begin
  if AKind = 'middleware' then
    Result := '[BEFORE ROUTING][REQUEST][BODY] '
  else
    Result := '[TRACE][REQ][BODY] ';
end;

procedure THTTPFilterTraceTests.Setup;
begin
  fVerbosity := UseLoggerVerbosityLevel;
  UseLoggerVerbosityLevel := TLogLevel.levDebug;
  ReleaseGlobalLogger;
  fAppender := TLoggerProMemoryRingBufferAppender.Create;
  SetDefaultLogger(LoggerProBuilder.WithMinimumLevel(TLogType.Debug).WriteToAppender(fAppender).Build);
end;

procedure THTTPFilterTraceTests.TearDown;
begin
  if fServer <> nil then
    fServer.Stop;
  fServer := nil;
  FreeAndNil(fEngine);
  ReleaseGlobalLogger; // frees the memory appender: the next Log builds the default logger again
  fAppender := nil;
  UseLoggerVerbosityLevel := fVerbosity;
end;

procedure THTTPFilterTraceTests.Listen(const AKind: string; const AMaxBodySize: Integer; const AHost: string;
  const AFlags: TMVCTraceFlags);
begin
  fEngine := TMVCEngine.Create;
  fEngine.AddController(TTraceCharsetController);
  if AKind = 'middleware' then
  begin
    {$WARN SYMBOL_DEPRECATED OFF}
    fEngine.AddMiddleware(TMVCTraceMiddleware.Create(AMaxBodySize, TRACE_TAG));
    {$WARN SYMBOL_DEPRECATED DEFAULT}
  end
  else
    fEngine.Use(TraceHTTPFilter(AMaxBodySize, TRACE_TAG, AFlags));
  if AHost = 'HttpSys' then
    fServer := TMVCServerFactory.CreateHttpSys(fEngine)
  else
    fServer := TMVCServerFactory.CreateIndyDirect(fEngine);
  fServer.Listen(HF_TRACE_PORT, 'localhost');
end;

function THTTPFilterTraceTests.LoggedBody(const AMarker: string): string;
var
  lItems: TList<TLogItem>;
  lItem: TLogItem;
  lPos, lTry: Integer;
begin
  Result := '';
  for lTry := 1 to 100 do // the logger writes on a thread of its own
  begin
    lItems := fAppender.GetLogItemsByTag(TRACE_TAG);
    try
      for lItem in lItems do
      begin
        lPos := lItem.LogMessage.IndexOf(AMarker);
        if lPos >= 0 then
          Exit(lItem.LogMessage.Substring(lPos + AMarker.Length));
      end;
    finally
      for lItem in lItems do
        lItem.Free;
      lItems.Free;
    end;
    Sleep(30);
  end;
  Assert.Fail('no "' + AMarker + '" line in the log');
end;

procedure THTTPFilterTraceTests.AUTF8BodyIsLoggedIntact(const AKind: string);
begin
  Listen(AKind, 1024);
  Assert.AreEqual(200, Client(HF_TRACE_PORT).Get('/trace931/json').StatusCode);
  Assert.AreEqual(TRACE_JSON, LoggedBody(ResponseBodyMarker(AKind)));
end;

procedure THTTPFilterTraceTests.ABodyIsLoggedInTheCharsetOfItsContentType(const AKind, AHost: string);
begin
  Listen(AKind, 1024, AHost);
  Assert.AreEqual(200, Client(HF_TRACE_PORT).Get('/trace931/latin5').StatusCode);
  Assert.AreEqual(TRACE_TURKISH, LoggedBody(ResponseBodyMarker(AKind)));
end;

procedure THTTPFilterTraceTests.ACharacterCutByTheSizeLimitIsLeftOut(const AKind: string);
var
  lRes: IMVCRESTResponse;
begin
  // 13 bytes: {"message":" and the first of the two bytes of the C with cedilla
  Listen(AKind, 13);
  lRes := Client(HF_TRACE_PORT).Get('/trace931/json');
  Assert.AreEqual(200, lRes.StatusCode);
  Assert.AreEqual(TRACE_JSON, lRes.Content, 'the response is whole');
  Assert.AreEqual('{"message":"', LoggedBody(ResponseBodyMarker(AKind)));
end;

procedure THTTPFilterTraceTests.ARequestBodyNotInItsCharsetIsAnswered(const AKind: string);
var
  lRes: IMVCRESTResponse;
begin
  Listen(AKind, 1024);
  lRes := Client(HF_TRACE_PORT)
    .AddBody(TBytesStream.Create(TBytes.Create($7B, $FF, $FE, $7D)), True, 'application/json; charset=utf-8')
    .Post('/trace931/body');
  Assert.AreEqual(200, lRes.StatusCode, lRes.Content);
  Assert.AreEqual('<4 bytes not decodable as utf-8>', LoggedBody(RequestBodyMarker(AKind)));
end;

procedure THTTPFilterTraceTests.AQuotedCharsetIsDecoded(const AKind: string);
var
  lRes: IMVCRESTResponse;
begin
  Listen(AKind, 1024);
  lRes := Client(HF_TRACE_PORT)
    .AddBody(TBytesStream.Create(TEncoding.UTF8.GetBytes(TRACE_JSON)), True, 'application/json; charset="utf-8"')
    .Post('/trace931/body');
  Assert.AreEqual(200, lRes.StatusCode, lRes.Content);
  Assert.AreEqual(TRACE_JSON, LoggedBody(RequestBodyMarker(AKind)));
end;

procedure THTTPFilterTraceTests.ABodyWithoutCharsetIsLoggedAsItWasWritten(const AKind: string);
begin
  Listen(AKind, 1024);
  Assert.AreEqual(200, Client(HF_TRACE_PORT).Get('/trace931/ansi').StatusCode);
  // as the ANSI code page of this machine has it (the text needs a Latin one to be meaningful)
  Assert.AreEqual(TEncoding.Default.GetString(TEncoding.Default.GetBytes(TRACE_ANSI)),
    LoggedBody(ResponseBodyMarker(AKind)));
end;

procedure THTTPFilterTraceTests.TheContentTypeIsSentAsItWasSet(const AHost: string);
var
  lRes: IMVCRESTResponse;
begin
  Listen('filter', 1024, AHost);
  lRes := Client(HF_TRACE_PORT).Get('/trace931/ansi');
  Assert.AreEqual(200, lRes.StatusCode);
  Assert.AreEqual(TMVCMediaType.TEXT_PLAIN, lRes.HeaderValue('Content-Type'));
end;

procedure THTTPFilterTraceTests.ANegativeSizeLimitDoesNotBreakAStreamResponse;
var
  lRes: IMVCRESTResponse;
begin
  Listen('filter', -1);
  lRes := Client(HF_TRACE_PORT).Get('/trace931/json');
  Assert.AreEqual(200, lRes.StatusCode, lRes.Content);
  Assert.AreEqual(TRACE_JSON, lRes.Content);
end;

function TraceBody(const AText: string): TStream;
begin
  Result := TBytesStream.Create(TEncoding.UTF8.GetBytes(AText));
end;

procedure THTTPFilterTraceTests.SecretsInAJSONBodyAreMasked(const AKind: string);
begin
  Listen(AKind, 1024);
  Client(HF_TRACE_PORT).AddBody(TraceBody(TRACE_LOGIN), True, 'application/json; charset=utf-8')
    .Post('/trace931/body');
  Assert.AreEqual(TRACE_LOGIN_MASKED, LoggedBody(RequestBodyMarker(AKind)));
end;

procedure THTTPFilterTraceTests.ASecretCutByTheSizeLimitIsMasked(const AKind: string);
begin
  // 33 characters: {"user":"daniele","password":"s3\ - the value is cut, it is masked all the same
  Listen(AKind, 33);
  Client(HF_TRACE_PORT).AddBody(TraceBody(TRACE_LOGIN), True, 'application/json; charset=utf-8')
    .Post('/trace931/body');
  Assert.AreEqual('{"user":"daniele","password":"***"', LoggedBody(RequestBodyMarker(AKind)));
end;

procedure THTTPFilterTraceTests.SecretsInAFormBodyAreMasked(const AKind: string);
begin
  Listen(AKind, 1024);
  Client(HF_TRACE_PORT)
    .AddBody(TraceBody('user=daniele&password=s3cr3t&client_secret=a%20b&page=2'), True,
      TMVCMediaType.APPLICATION_FORM_URLENCODED)
    .Post('/trace931/body');
  Assert.AreEqual('user=daniele&password=***&client_secret=***&page=2', LoggedBody(RequestBodyMarker(AKind)));
end;

procedure THTTPFilterTraceTests.ASecretInAResponseIsMasked(const AKind: string);
begin
  Listen(AKind, 1024);
  Assert.AreEqual(200, Client(HF_TRACE_PORT).Get('/trace931/token').StatusCode);
  Assert.AreEqual('{"token":"***","user":"daniele"}', LoggedBody(ResponseBodyMarker(AKind)));
end;

procedure THTTPFilterTraceTests.SecretsInArraysObjectsAndXMLAreMasked(const AKind: string);
begin
  Listen(AKind, 1024);
  Client(HF_TRACE_PORT).AddBody(TraceBody('{"authorization":"Bearer x","api_keys":["a","b"],' +
    '"secret":{"v":"x"},"sessionId":"s1","page":2}'), True, 'application/json; charset=utf-8')
    .Post('/trace931/body');
  Assert.AreEqual('{"authorization":"***","api_keys":"***","secret":"***","sessionId":"***","page":2}',
    LoggedBody(RequestBodyMarker(AKind)));
  TearDown;
  Setup;
  Listen(AKind, 1024);
  Client(HF_TRACE_PORT).AddBody(TraceBody('<login><user>d</user><password>x</password></login>'), True,
    TMVCMediaType.APPLICATION_XML).Post('/trace931/body');
  Assert.AreEqual('<login><user>d</user><password>***</password></login>', LoggedBody(RequestBodyMarker(AKind)));
end;

procedure THTTPFilterTraceTests.ASecretInTheQueryStringIsMasked;
begin
  Listen('filter', 1024);
  Assert.AreEqual(200, Client(HF_TRACE_PORT).Get('/trace931/json?access_token=abc&page=2').StatusCode);
  Assert.IsTrue(LoggedBody('[QS:').StartsWith('access_token=***,page=2]'), LoggedBody('[QS:'));
end;

procedure THTTPFilterTraceTests.LogBodySecretsLogsThemRaw;
begin
  Listen('filter', 1024, 'IndyDirect', [tfLogBodySecrets]);
  Client(HF_TRACE_PORT).AddBody(TraceBody(TRACE_LOGIN), True, 'application/json; charset=utf-8')
    .Post('/trace931/body');
  Assert.AreEqual(TRACE_LOGIN, LoggedBody(RequestBodyMarker('filter')));
end;

initialization
  TDUnitX.RegisterTestFixture(THTTPFilterTests);
  TDUnitX.RegisterTestFixture(THTTPFilterSessionTests);
  TDUnitX.RegisterTestFixture(THTTPFilterTraceTests);

end.
