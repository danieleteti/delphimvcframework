unit SerializerPrecedenceTestsU;

// Serializer precedence and the +json fallback.
//
// Rule 1: a serializer the user registered always wins, also for application/json: none of the
//         fast paths that write JSON directly may bypass it.
// Rule 2: nothing registered for application/<anything>+json -> the application/json serializer
//         answers (the built-in or the user's), the Content-Type stays the negotiated one.
//
// Three fixtures:
//  - TTestSerializerLookup        TMVCEngine.Serializer, no server
//  - TTestPlusJSONLive            the shared TestServer (every host of the matrix): application/vnd.api+json
//                                 with nothing registered, application/x-own+json with a serializer of its own,
//                                 application/json untouched, the request body under +json
//  - TTestUserJSONSerializerWins  an engine of its own (Indy Direct, in this process) whose application/json
//                                 serializer is the user's: it cannot live on the shared TestServer, where it
//                                 would change the answer of the rest of the suite

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Server.Intf,
  MVCFramework.RESTClient.Intf,
  LiveServerTestU;

type
  [TestFixture]
  TTestSerializerLookup = class(TObject)
  private
    fEngine: TMVCEngine;
  public
    [SetUp]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure PlusJSONFallsBackToTheJSONSerializer;
    [Test]
    procedure PlusJSONWithCharsetAndCaseFallsBack;
    [Test]
    procedure ExactRegistrationWinsOverTheFallback;
    [Test]
    procedure FallbackIsTheUserSerializerWhenTheUserReplacedJSON;
    [Test]
    procedure FallbackFollowsTheDictionaryToo;
    [Test]
    procedure OtherTypesAreNotAffected;
    [Test]
    procedure NotRaisingStillReturnsNilForUnknownTypes;
  end;

  [TestFixture]
  TTestPlusJSONLive = class(TBaseServerTest)
  private
    function PostBody(const AURL, ABody, AContentType: string): IMVCRESTResponse;
  public
    // application/vnd.api+json, nothing registered for it: the same body of the application/json twin
    [Test]
    [TestCase('simple (F2)', '/serprec/vnd/simple,/serprec/json/simple')]
    [TestCase('list (F2)', '/serprec/vnd/list,/serprec/json/list')]
    [TestCase('nested, not streamable', '/serprec/vnd/nested,/serprec/json/nested')]
    [TestCase('TJsonObject in OKResponse (F1)', '/serprec/vnd/jsonobject,/serprec/json/jsonobject')]
    [TestCase('TJsonObject returned (F3)', '/serprec/vnd/direct,/serprec/json/direct')]
    [TestCase('TDataSet returned (F4)', '/serprec/vnd/dataset,/serprec/json/dataset')]
    [TestCase('TDataSet in OKResponse (F2)', '/serprec/vnd/okdataset,/serprec/json/okdataset')]
    procedure PlusJSONHasTheBodyOfApplicationJSON(const AVndURL, AJsonURL: string);

    // application/x-own+json has a serializer of its own: that one answers
    [Test]
    [TestCase('simple', '/serprec/own/simple')]
    [TestCase('nested', '/serprec/own/nested')]
    [TestCase('TJsonObject', '/serprec/own/jsonobject')]
    procedure ARegisteredPlusJSONSerializerWins(const AURL: string);

    // application/json with the built-in serializer: the body is the one of the fast paths, unchanged
    [Test]
    [TestCase('TJsonObject in OKResponse (F1)', '/serprec/json/jsonobject|{"data":{"name":"Daniele","value":42}}', '|')]
    [TestCase('simple (F2)', '/serprec/json/simple|{"data":{"name":"Daniele","value":42}}', '|')]
    [TestCase('list (F2)', '/serprec/json/list|{"data":[{"name":"one","value":1},{"name":"two","value":2}]}', '|')]
    [TestCase('TJsonObject returned (F3)', '/serprec/json/direct|{"name":"Daniele","value":42}', '|')]
    [TestCase('nested', '/serprec/json/nested|{"data":{"name":"parent","child":{"name":"child","value":7}}}', '|')]
    [TestCase('TDataSet in OKResponse (F2)', '/serprec/json/okdataset|{"data":[{"id":1,"name":"one"},{"id":2,"name":"two"}]}', '|')]
    [TestCase('TDataSet returned (F4)', '/serprec/json/dataset|[{"id":1,"name":"one"},{"id":2,"name":"two"}]', '|')]
    procedure ApplicationJSONBodyIsUnchanged(const AURL, AExpectedBody: string);

    // Minimal API: the same rules (the handler sets application/vnd.api+json by itself)
    [Test]
    [TestCase('TJsonObject', '/serprec/min/vnd,/serprec/min/json')]
    [TestCase('simple', '/serprec/min/vndsimple,/serprec/min/simple')]
    procedure MinimalAPIPlusJSONHasTheBodyOfApplicationJSON(const AVndURL, AJsonURL: string);

    // The action sets Context.Response.ContentType directly (text/csv, nothing registered for it): the renderer
    // is not told, the type of the response is the one that counts. No JSON labelled text/csv: the serializer
    // lookup answers, as it does for any type with nothing registered
    [Test]
    [TestCase('TJsonObject in OKResponse (F1)', '/serprec/csv/jsonobject')]
    [TestCase('simple (F2)', '/serprec/csv/simple')]
    [TestCase('TJsonObject returned (F3)', '/serprec/csv/direct')]
    [TestCase('TDataSet returned (F4)', '/serprec/csv/dataset')]
    procedure AResponseContentTypeSetDirectlyToAnotherTypeIsNotWrittenAsJSON(const AURL: string);

    // The action sets Context.Response.ContentType to JSON by itself: the fast paths are still taken, and the
    // body is the one of the serializer path (same as the action that does not set it)
    [Test]
    [TestCase('TJsonObject in OKResponse (F1)', '/serprec/jsonct/jsonobject,/serprec/json/jsonobject')]
    [TestCase('simple (F2)', '/serprec/jsonct/simple,/serprec/json/simple')]
    [TestCase('TJsonObject returned (F3)', '/serprec/jsonct/direct,/serprec/json/direct')]
    [TestCase('TDataSet returned (F4)', '/serprec/jsonct/dataset,/serprec/json/dataset')]
    procedure AResponseContentTypeSetDirectlyToJSONHasTheSameBody(const AURL, ATwinURL: string);

    // the request body under +json is deserialized by the JSON serializer too
    [Test]
    [TestCase('MVCFromBody', '/serprec/req/frombody')]
    [TestCase('BodyAs', '/serprec/req/bodyas')]
    [TestCase('BodyFor', '/serprec/req/bodyfor')]
    procedure PlusJSONRequestBodyIsDeserialized(const AURL: string);
    [Test]
    [TestCase('BodyAsListOf', '/serprec/req/bodyaslist')]
    [TestCase('BodyForListOf', '/serprec/req/bodyforlist')]
    procedure PlusJSONRequestBodyIsDeserializedAsList(const AURL: string);
    // the same requests with application/json: unchanged
    [Test]
    [TestCase('MVCFromBody', '/serprec/req/frombody')]
    [TestCase('BodyAs', '/serprec/req/bodyas')]
    [TestCase('BodyFor', '/serprec/req/bodyfor')]
    procedure ApplicationJSONRequestBodyIsUnchanged(const AURL: string);
    // a body type nobody serves is still refused
    [Test]
    procedure UnknownRequestBodyTypeIsStillRefused;
  end;

  [TestFixture]
  TTestUserJSONSerializerWins = class(TObject)
  private
    fEngine: TMVCEngine;
    fServer: IMVCServer;
    procedure StartServer;
    function Get(const AURL: string): IMVCRESTResponse;
    procedure AssertCustom(const AURL: string);
  public
    [SetUp]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
    // nothing registered by the user: the built-in answers (control)
    [Test]
    procedure BuiltInAnswersWhenNothingIsRegistered;

    // AddSerializer('application/json', ...)
    [Test]
    [TestCase('TJsonObject in OKResponse (F1)', '/serprec/json/jsonobject')]
    [TestCase('simple (F2)', '/serprec/json/simple')]
    [TestCase('list (F2)', '/serprec/json/list')]
    [TestCase('TDataSet in OKResponse (F2)', '/serprec/json/okdataset')]
    [TestCase('TJsonObject returned (F3)', '/serprec/json/direct')]
    [TestCase('TDataSet returned (F4)', '/serprec/json/dataset')]
    [TestCase('Minimal API TJsonObject', '/serprec/min/json')]
    [TestCase('Minimal API simple', '/serprec/min/simple')]
    [TestCase('Response.ContentType set to JSON by the action (F1)', '/serprec/jsonct/jsonobject')]
    [TestCase('Response.ContentType set to JSON by the action (F2)', '/serprec/jsonct/simple')]
    [TestCase('Response.ContentType set to JSON by the action (F3)', '/serprec/jsonct/direct')]
    [TestCase('Response.ContentType set to JSON by the action (F4)', '/serprec/jsonct/dataset')]
    procedure AddSerializerWins(const AURL: string);

    // Serializers.AddOrSetValue('application/json', ...): the dictionary is public
    [Test]
    [TestCase('TJsonObject in OKResponse (F1)', '/serprec/json/jsonobject')]
    [TestCase('simple (F2)', '/serprec/json/simple')]
    [TestCase('TJsonObject returned (F3)', '/serprec/json/direct')]
    [TestCase('TDataSet returned (F4)', '/serprec/json/dataset')]
    procedure SerializersAddOrSetValueWins(const AURL: string);

    // application/vnd.api+json has nothing registered: it follows the user's application/json
    [Test]
    [TestCase('TJsonObject in OKResponse (F1)', '/serprec/vnd/jsonobject')]
    [TestCase('simple (F2)', '/serprec/vnd/simple')]
    [TestCase('nested', '/serprec/vnd/nested')]
    [TestCase('TJsonObject returned (F3)', '/serprec/vnd/direct')]
    [TestCase('TDataSet returned (F4)', '/serprec/vnd/dataset')]
    procedure PlusJSONFollowsTheUserSerializer(const AURL: string);

    // the user hands the built-in back: the fast paths are on again
    [Test]
    procedure RestoringTheBuiltInSerializerGivesTheFastPathsBack;
    // the user removes the serializer registered: no more custom answers
    [Test]
    procedure RemovingTheUserSerializerIsSeenToo;
  end;

implementation

uses
  System.Classes,
  System.Generics.Collections,
  JsonDataObjects,
  MVCFramework.Serializer.Intf,
  MVCFramework.Serializer.JsonDataObjects,
  MVCFramework.Server.Indy,
  MVCFramework.RESTClient,
  SerializerPrecedenceSupportU;

const
  TEST_USERJSON_PORT = 9880;

function NewEngine: TMVCEngine;
begin
  Result := TMVCEngine.Create(TProc<TMVCConfig>(nil));
end;

{ TTestSerializerLookup }

procedure TTestSerializerLookup.SetUp;
begin
  fEngine := NewEngine;
end;

procedure TTestSerializerLookup.TearDown;
begin
  FreeAndNil(fEngine);
end;

procedure TTestSerializerLookup.PlusJSONFallsBackToTheJSONSerializer;
begin
  Assert.IsTrue(fEngine.Serializer('application/vnd.api+json') = fEngine.Serializer('application/json'));
  Assert.IsTrue(fEngine.Serializer('application/ld+json') = fEngine.Serializer('application/json'));
  Assert.IsTrue(fEngine.Serializer('application/vnd.api+json', False) = fEngine.Serializer('application/json'));
end;

procedure TTestSerializerLookup.PlusJSONWithCharsetAndCaseFallsBack;
begin
  Assert.IsTrue(fEngine.Serializer('application/vnd.api+json; charset=utf-8') = fEngine.Serializer('application/json'));
  Assert.IsTrue(fEngine.Serializer('Application/VND.API+JSON') = fEngine.Serializer('application/json'));
end;

procedure TTestSerializerLookup.ExactRegistrationWinsOverTheFallback;
var
  lOwn: IMVCSerializer;
begin
  lOwn := TMarkerJSONSerializer.Create(fEngine.Config);
  fEngine.AddSerializer('application/vnd.api+json', lOwn);
  Assert.IsTrue(fEngine.Serializer('application/vnd.api+json') = lOwn);
  Assert.IsTrue(fEngine.Serializer('application/vnd.api+json; charset=utf-8') = lOwn);
  // the others still fall back
  Assert.IsTrue(fEngine.Serializer('application/ld+json') = fEngine.Serializer('application/json'));
end;

procedure TTestSerializerLookup.FallbackIsTheUserSerializerWhenTheUserReplacedJSON;
var
  lOwn: IMVCSerializer;
begin
  lOwn := TMarkerJSONSerializer.Create(fEngine.Config);
  fEngine.AddSerializer('application/json', lOwn);
  Assert.IsTrue(fEngine.Serializer('application/json') = lOwn);
  Assert.IsTrue(fEngine.Serializer('application/vnd.api+json') = lOwn);
end;

procedure TTestSerializerLookup.FallbackFollowsTheDictionaryToo;
var
  lOwn: IMVCSerializer;
begin
  // the dictionary is public: the fallback reads it, not a copy
  lOwn := TMarkerJSONSerializer.Create(fEngine.Config);
  fEngine.Serializers.AddOrSetValue('application/json', lOwn);
  Assert.IsTrue(fEngine.Serializer('application/vnd.api+json') = lOwn);
end;

procedure TTestSerializerLookup.OtherTypesAreNotAffected;
begin
  Assert.WillRaise(
    procedure
    begin
      fEngine.Serializer('application/vnd.api+xml');
    end, EMVCException);
  Assert.WillRaise(
    procedure
    begin
      fEngine.Serializer('text/vnd.foo+json');
    end, EMVCException);
  Assert.WillRaise(
    procedure
    begin
      fEngine.Serializer('application/json2');
    end, EMVCException);
  Assert.WillRaise(
    procedure
    begin
      fEngine.Serializer('+json');
    end, EMVCException);
end;

procedure TTestSerializerLookup.NotRaisingStillReturnsNilForUnknownTypes;
begin
  Assert.IsTrue(fEngine.Serializer('text/csv', False) = nil);
  Assert.IsTrue(fEngine.Serializer('application/vnd.api+xml', False) = nil);
end;

{ TTestPlusJSONLive }

function TTestPlusJSONLive.PostBody(const AURL, ABody, AContentType: string): IMVCRESTResponse;
begin
  Result := RESTClient.AddBody(ABody, AContentType).Post(AURL);
end;

procedure TTestPlusJSONLive.PlusJSONHasTheBodyOfApplicationJSON(const AVndURL, AJsonURL: string);
var
  lVnd, lJson: IMVCRESTResponse;
begin
  lJson := RESTClient.Get(AJsonURL);
  Assert.AreEqual(HTTP_STATUS.OK, lJson.StatusCode, lJson.Content);
  Assert.StartsWith('application/json', lJson.ContentType);
  lVnd := RESTClient.Accept(MEDIA_VND).Get(AVndURL);
  Assert.AreEqual(HTTP_STATUS.OK, lVnd.StatusCode, lVnd.Content);
  // the negotiated type, not application/json
  Assert.StartsWith('application/vnd.api+json', lVnd.ContentType);
  Assert.AreEqual(lJson.Content, lVnd.Content);
end;

procedure TTestPlusJSONLive.ARegisteredPlusJSONSerializerWins(const AURL: string);
var
  lRes: IMVCRESTResponse;
begin
  lRes := RESTClient.Accept(MEDIA_OWN).Get(AURL);
  Assert.AreEqual(HTTP_STATUS.OK, lRes.StatusCode, lRes.Content);
  Assert.StartsWith('application/x-own+json', lRes.ContentType);
  Assert.StartsWith(MARKER_PREFIX, lRes.Content);
end;

procedure TTestPlusJSONLive.ApplicationJSONBodyIsUnchanged(const AURL, AExpectedBody: string);
var
  lRes: IMVCRESTResponse;
begin
  lRes := RESTClient.Get(AURL);
  Assert.AreEqual(HTTP_STATUS.OK, lRes.StatusCode, lRes.Content);
  Assert.StartsWith('application/json', lRes.ContentType);
  Assert.AreEqual(AExpectedBody, lRes.Content);
end;

procedure TTestPlusJSONLive.MinimalAPIPlusJSONHasTheBodyOfApplicationJSON(const AVndURL, AJsonURL: string);
var
  lVnd, lJson: IMVCRESTResponse;
begin
  lJson := RESTClient.Get(AJsonURL);
  Assert.AreEqual(HTTP_STATUS.OK, lJson.StatusCode, lJson.Content);
  Assert.StartsWith('application/json', lJson.ContentType);
  lVnd := RESTClient.Accept(MEDIA_VND).Get(AVndURL);
  Assert.AreEqual(HTTP_STATUS.OK, lVnd.StatusCode, lVnd.Content);
  Assert.StartsWith('application/vnd.api+json', lVnd.ContentType);
  Assert.AreEqual(lJson.Content, lVnd.Content);
end;

procedure TTestPlusJSONLive.AResponseContentTypeSetDirectlyToAnotherTypeIsNotWrittenAsJSON(const AURL: string);
var
  lRes: IMVCRESTResponse;
begin
  lRes := RESTClient.Get(AURL);
  // no serializer for text/csv: an error, as in 3.4 and as for any type nobody serves, not JSON labelled text/csv
  Assert.AreEqual(HTTP_STATUS.InternalServerError, lRes.StatusCode, lRes.Content);
  Assert.Contains(lRes.Content, 'text/csv', False, lRes.Content);
end;

procedure TTestPlusJSONLive.AResponseContentTypeSetDirectlyToJSONHasTheSameBody(const AURL, ATwinURL: string);
var
  lRes, lTwin: IMVCRESTResponse;
begin
  lTwin := RESTClient.Get(ATwinURL);
  Assert.AreEqual(HTTP_STATUS.OK, lTwin.StatusCode, lTwin.Content);
  lRes := RESTClient.Get(AURL);
  Assert.AreEqual(HTTP_STATUS.OK, lRes.StatusCode, lRes.Content);
  Assert.StartsWith('application/json', lRes.ContentType);
  Assert.AreEqual(lTwin.Content, lRes.Content);
end;

procedure TTestPlusJSONLive.PlusJSONRequestBodyIsDeserialized(const AURL: string);
var
  lRes: IMVCRESTResponse;
begin
  lRes := PostBody(AURL, '{"name":"Daniele","value":42}', 'application/vnd.api+json');
  Assert.AreEqual(HTTP_STATUS.OK, lRes.StatusCode, lRes.Content);
  Assert.Contains(lRes.Content, 'Daniele:42', False, lRes.Content);
end;

procedure TTestPlusJSONLive.PlusJSONRequestBodyIsDeserializedAsList(const AURL: string);
var
  lRes: IMVCRESTResponse;
begin
  lRes := PostBody(AURL, '[{"name":"one","value":1},{"name":"two","value":2}]', 'application/vnd.api+json');
  Assert.AreEqual(HTTP_STATUS.OK, lRes.StatusCode, lRes.Content);
  Assert.Contains(lRes.Content, '2:one', False, lRes.Content);
end;

procedure TTestPlusJSONLive.ApplicationJSONRequestBodyIsUnchanged(const AURL: string);
var
  lRes: IMVCRESTResponse;
begin
  lRes := PostBody(AURL, '{"name":"Daniele","value":42}', 'application/json');
  Assert.AreEqual(HTTP_STATUS.OK, lRes.StatusCode, lRes.Content);
  Assert.Contains(lRes.Content, 'Daniele:42', False, lRes.Content);
end;

procedure TTestPlusJSONLive.UnknownRequestBodyTypeIsStillRefused;
var
  lRes: IMVCRESTResponse;
begin
  lRes := PostBody('/serprec/req/bodyas', '{"name":"Daniele","value":42}', 'application/vnd.api+xml');
  Assert.AreNotEqual(HTTP_STATUS.OK, lRes.StatusCode, lRes.Content);
end;

{ TTestUserJSONSerializerWins }

procedure TTestUserJSONSerializerWins.SetUp;
begin
  fEngine := nil;
  fServer := nil;
end;

procedure TTestUserJSONSerializerWins.TearDown;
begin
  if Assigned(fServer) then
  begin
    if fServer.IsRunning then
      fServer.Stop;
    fServer := nil;
  end;
  FreeAndNil(fEngine);
end;

procedure TTestUserJSONSerializerWins.StartServer;
begin
  fEngine.AddController(TSerializerPrecedenceController);
  RegisterSerializerPrecedenceRoutes(fEngine);
  fServer := TMVCIndyServer.Create(fEngine);
  fServer.Listen(TEST_USERJSON_PORT);
end;

function TTestUserJSONSerializerWins.Get(const AURL: string): IMVCRESTResponse;
var
  lClient: IMVCRESTClient;
begin
  lClient := TMVCRESTClient.New.BaseURL('localhost', TEST_USERJSON_PORT);
  if AURL.Contains('/vnd/') then
    lClient.Accept(MEDIA_VND);
  Result := lClient.Get(AURL);
end;

procedure TTestUserJSONSerializerWins.AssertCustom(const AURL: string);
var
  lRes: IMVCRESTResponse;
begin
  lRes := Get(AURL);
  Assert.AreEqual(HTTP_STATUS.OK, lRes.StatusCode, lRes.Content);
  Assert.StartsWith(MARKER_PREFIX, lRes.Content, False, 'the serializer of the user did not write the body: ' + lRes.Content);
end;

procedure TTestUserJSONSerializerWins.BuiltInAnswersWhenNothingIsRegistered;
var
  lRes: IMVCRESTResponse;
begin
  fEngine := NewEngine;
  StartServer;
  lRes := Get('/serprec/json/simple');
  Assert.AreEqual(HTTP_STATUS.OK, lRes.StatusCode, lRes.Content);
  Assert.AreEqual('{"data":{"name":"Daniele","value":42}}', lRes.Content);
end;

procedure TTestUserJSONSerializerWins.AddSerializerWins(const AURL: string);
begin
  fEngine := NewEngine;
  fEngine.AddSerializer(TMVCMediaType.APPLICATION_JSON, TMarkerJSONSerializer.Create(fEngine.Config));
  StartServer;
  AssertCustom(AURL);
end;

procedure TTestUserJSONSerializerWins.SerializersAddOrSetValueWins(const AURL: string);
begin
  fEngine := NewEngine;
  fEngine.Serializers.AddOrSetValue(TMVCMediaType.APPLICATION_JSON, TMarkerJSONSerializer.Create(fEngine.Config));
  StartServer;
  AssertCustom(AURL);
end;

procedure TTestUserJSONSerializerWins.PlusJSONFollowsTheUserSerializer(const AURL: string);
var
  lRes: IMVCRESTResponse;
begin
  fEngine := NewEngine;
  fEngine.AddSerializer(TMVCMediaType.APPLICATION_JSON, TMarkerJSONSerializer.Create(fEngine.Config));
  StartServer;
  lRes := Get(AURL);
  Assert.AreEqual(HTTP_STATUS.OK, lRes.StatusCode, lRes.Content);
  Assert.StartsWith('application/vnd.api+json', lRes.ContentType);
  Assert.StartsWith(MARKER_PREFIX, lRes.Content, False, lRes.Content);
end;

procedure TTestUserJSONSerializerWins.RestoringTheBuiltInSerializerGivesTheFastPathsBack;
var
  lBuiltIn: IMVCSerializer;
  lRes: IMVCRESTResponse;
begin
  fEngine := NewEngine;
  lBuiltIn := fEngine.Serializer(TMVCMediaType.APPLICATION_JSON);
  fEngine.AddSerializer(TMVCMediaType.APPLICATION_JSON, TMarkerJSONSerializer.Create(fEngine.Config));
  StartServer;
  AssertCustom('/serprec/json/simple');
  AssertCustom('/serprec/json/jsonobject');
  fEngine.Serializers.AddOrSetValue(TMVCMediaType.APPLICATION_JSON, lBuiltIn);
  lRes := Get('/serprec/json/simple');
  Assert.AreEqual('{"data":{"name":"Daniele","value":42}}', lRes.Content);
  lRes := Get('/serprec/json/jsonobject');
  Assert.AreEqual('{"data":{"name":"Daniele","value":42}}', lRes.Content);
end;

procedure TTestUserJSONSerializerWins.RemovingTheUserSerializerIsSeenToo;
var
  lBuiltIn: IMVCSerializer;
  lRes: IMVCRESTResponse;
begin
  fEngine := NewEngine;
  lBuiltIn := fEngine.Serializer(TMVCMediaType.APPLICATION_JSON);
  fEngine.AddSerializer(TMVCMediaType.APPLICATION_JSON, TMarkerJSONSerializer.Create(fEngine.Config));
  StartServer;
  AssertCustom('/serprec/json/simple');
  // Remove + Add of the built-in instance: the key holds the built-in again
  fEngine.Serializers.Remove(TMVCMediaType.APPLICATION_JSON);
  fEngine.Serializers.Add(TMVCMediaType.APPLICATION_JSON, lBuiltIn);
  lRes := Get('/serprec/json/simple');
  Assert.AreEqual('{"data":{"name":"Daniele","value":42}}', lRes.Content);
end;

end.
