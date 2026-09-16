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

unit RESTAdapterTestsU;

{$I dmvcframework.inc}

interface

uses
  MVCFramework.RESTAdapter,
  DUnitX.TestFramework,
  BusinessObjectsU,
  Generics.Collections,
  JsonDataObjects,
  MVCFramework.RESTClient,
  MVCFramework.Commons,
  MVCFramework.Serializer.Commons,
  MVCFramework.RESTClient.Intf;

type

  [Headers('User-Agent', 'RESTAdapter-Test')]
  ITESTService = interface(IInvokable)
    ['{58B9FA23-92F4-4B8E-814B-05232F32A41F}']

    [RESTResource(HttpGet, '/people')]
    [MVCListOf(TPerson)]
    function GetPeople: TObjectList<TPerson>;

    [RESTResource(HttpGet, '/people')]
    [MVCListOf(TPerson)]
    [Mapping(TPeople)]
    procedure GetPeopleAsynch(AAsynchRequest: IAsynchRequest);

    [RESTResource(HttpGet, '/people/1')]
    function GetTonyStark: TPerson;

    [RESTResource(HttpGet, '/people/1')]
    [Mapping(TPerson)]
    procedure GetTonyStarkAsynch(AAsynchRequest: IAsynchRequest);

    [RESTResource(HttpGet, '/people/{personid}')]
    function GetPersonByID([Param('personid')] APersonID: integer): TPerson;

    [RESTResource(httpPOST, '/people')]
    function SendPerson([Body] ABody: TPerson): TPerson;

    // Issue #897: single-object request body + collection response body.
    // The method-level [MVCListOf] describes the response array and must not
    // make the adapter serialize the [Body] object as a collection.
    [RESTResource(httpPOST, '/people/searchbysample')]
    [MVCListOf(TPerson)]
    function SearchPeopleBySample([Body] ACriteria: TPerson): TObjectList<TPerson>;

    [RESTResource(HttpGet, '/people')]
    function GetPersonInJSONArray: TJSONArray;

    [Headers('Accept', 'application/json')]
    [Headers('ContentType', 'application/json')]
    [RESTResource(HttpGet, '/adapter/testconsumejson')]
    function HeadersApplicationJSON: TJsonBaseObject;

    [Headers('Accept', 'text/plain')]
    [Headers('ContentType', 'text/plain')]
    [RESTResource(HttpGet, '/testconsumes')]
    function HeadersTextPlain: string;

    [Headers('Accept', 'text/plain')]
    [Headers('ContentType', 'text/plain')]
    [RESTResource(HttpGet, '/adapter/testconsumejson')]
    function ApplicationJSONWithTextPlainHeader: IMVCRESTResponse;

  end;

  [TestFixture]
  TTestRESTAdapter = class(TObject)
  private
    RESTAdapter: TRESTAdapter<ITESTService>;
    TESTService: ITESTService;
  public
    [SetUp]
    procedure SetUp;
    [Test]
    procedure TestGetPeople;
    [Test]
    procedure TestGetPeopleAsynch;
    [Test]
    procedure TestGetTonyStark;
    [Test]
    procedure TestGetTonyStarkAsynch;
    [Test]
    procedure TestPostPerson;
    [Test]
    procedure TestSearchPeopleBySample_ObjectBodyArrayResult;
    // Issue #900: the [Body] object must not be freed before the IAsynchRequest
    // inspection (which reads the last argument). Otherwise it is a use-after-free.
    [Test]
    procedure TestIssue900_BodyNotFreedBeforeAsyncCheck;
    [Test]
    procedure TestGetPersonByID;
    [Test]
    procedure TestHeadersApplicationJSON;
    [Test]
    procedure TestHeadersTextPlain;
    [Test]
    procedure TestApplicationJSONWithHeaderTextPlain;
    [Test]
    procedure TestGetPersonInJSONArray;
  end;

implementation

uses
  System.SysUtils,
  System.Rtti,
  System.SyncObjs,
  LiveServerTestU,
  TestConstsU;

{ TTestRESTAdapter }

procedure TTestRESTAdapter.SetUp;
begin
  inherited;
  RESTAdapter := TRESTAdapter<ITESTService>.Create;
  TESTService := RESTAdapter.Build(TEST_SERVER_ADDRESS, 8888);
end;

procedure TTestRESTAdapter.TestGetPersonByID;
var
  Person: TPerson;
begin;
  Person := TESTService.GetPersonByID(1);
  try
    Assert.AreEqual('Tony', Person.FirstName);
    Assert.AreEqual('Stark', Person.LastName);
    Assert.isTrue(Person.Married);
  finally
    Person.Free;
  end;
end;

procedure TTestRESTAdapter.TestGetPersonInJSONArray;
var
  JSONArray: TJSONArray;
begin
  JSONArray := TESTService.GetPersonInJSONArray;
  try
    Assert.isTrue(JSONArray.ToString.Contains('Tony'));
    Assert.isTrue(JSONArray.ToString.Contains('Stark'));
    Assert.isTrue(JSONArray.ToString.Contains('Bruce'));
    Assert.isTrue(JSONArray.ToString.Contains('Banner'));
  finally
    JSONArray.Free;
  end;
end;

procedure TTestRESTAdapter.TestGetTonyStark;
var
  Person: TPerson;
begin;
  Person := TESTService.GetTonyStark;
  try
    Assert.AreEqual('Tony', Person.FirstName);
    Assert.AreEqual('Stark', Person.LastName);
    Assert.isTrue(Person.Married);
  finally
    Person.Free;
  end;
end;

procedure TTestRESTAdapter.TestGetTonyStarkAsynch;
var
  AsynchRequest: IAsynchRequest;
  Person: TPerson;
  LEvt: TEvent;
begin
  LEvt := TEvent.Create;
  try
    AsynchRequest := TAsynchRequest.Create(
      procedure(AValue: TValue)
      begin
        Person := AValue.AsType<TPerson>;
        LEvt.SetEvent;
      end);
    TESTService.GetTonyStarkAsynch(AsynchRequest);
    // attend for max 5 seconds
    Assert.isTrue(TWaitResult.wrSignaled = LEvt.WaitFor(5000), 'Timeout request');
    Assert.IsNotNull(Person);
    try
      Assert.AreEqual('Tony', Person.FirstName);
      Assert.AreEqual('Stark', Person.LastName);
      Assert.isTrue(Person.Married);
    finally
      Person.Free;
    end;
  finally
    LEvt.Free;
  end;
end;

procedure TTestRESTAdapter.TestHeadersApplicationJSON;
var
  Res: TJSONObject;
begin
  Res := TESTService.HeadersApplicationJSON as TJSONObject;
  try
    Assert.AreEqual('Hello World', Res.S['key']);
  finally
    Res.Free;
  end;
end;

procedure TTestRESTAdapter.TestHeadersTextPlain;
var
  Res: string;
begin
  Res := TESTService.HeadersTextPlain;
  Assert.AreEqual('Hello World', Res);
end;

procedure TTestRESTAdapter.TestPostPerson;
var
  Person: TPerson;
  RetPerson: TPerson;
begin
  Person := TPerson.GetNew('Peter', 'Parker', 0, false);
  RetPerson := TESTService.SendPerson(Person);
  try
    Assert.AreEqual('Peter', RetPerson.FirstName);
    Assert.AreEqual('Parker', RetPerson.LastName);
    Assert.IsFalse(RetPerson.Married);
  finally
    RetPerson.Free;
  end;
end;

procedure TTestRESTAdapter.TestSearchPeopleBySample_ObjectBodyArrayResult;
var
  lCriteria: TPerson;
  lResult: TObjectList<TPerson>;
begin
  // Issue #897: a single-object [Body] combined with a method-level [MVCListOf]
  // (which describes the array RESPONSE) must NOT serialize the request body as
  // a collection. Pre-fix the server received a JSON array and BodyAs<TPerson>
  // could not bind it. The adapter owns and frees lCriteria ([Body] default).
  lCriteria := TPerson.GetNew('Search', 'Criteria', 0, False);
  lResult := TESTService.SearchPeopleBySample(lCriteria);
  try
    Assert.AreEqual<Integer>(1, lResult.Count, 'object body was not received as a single object');
    Assert.AreEqual('Search', lResult[0].FirstName);
    Assert.AreEqual('Criteria', lResult[0].LastName);
  finally
    lResult.Free;
  end;
end;

type
  // A [Body] probe whose destruction is globally observable. Its destructor only
  // touches a global counter, so the ordering assertion below never reads freed
  // memory and is therefore independent of the active memory manager.
  TProbePerson = class(TPerson)
  public
    destructor Destroy; override;
  end;

  // Instrumented adapter that snapshots how many probe bodies have been freed at
  // the exact moment the async-request inspection runs.
  TInstrumentedRESTAdapter = class(TRESTAdapter<ITESTService>)
  protected
    function GetAsynchRequest(const aArgs: TArray<TValue>): IAsynchRequest; override;
  end;

var
  GProbeFreeCount: Integer;
  GFreeCountAtAsyncCheck: Integer;

destructor TProbePerson.Destroy;
begin
  Inc(GProbeFreeCount);
  inherited;
end;

function TInstrumentedRESTAdapter.GetAsynchRequest(const aArgs: TArray<TValue>): IAsynchRequest;
begin
  GFreeCountAtAsyncCheck := GProbeFreeCount;
  Result := inherited GetAsynchRequest(aArgs);
end;

procedure TTestRESTAdapter.TestIssue900_BodyNotFreedBeforeAsyncCheck;
var
  lService: ITESTService;
  lProbe: TProbePerson;
  lRet: TPerson;
begin
  GProbeFreeCount := 0;
  GFreeCountAtAsyncCheck := -1;

  // TRESTAdapter is a TVirtualInterface: the built interface owns the adapter
  // instance through reference counting, so it must not be freed explicitly.
  lService := TInstrumentedRESTAdapter.Create.Build(TEST_SERVER_ADDRESS, 8888);

  lProbe := TProbePerson.Create;
  lProbe.FirstName := 'Peter';
  lProbe.LastName := 'Parker';
  // The adapter owns the [Body] object (default OwnsObject = True) and frees it
  // inside DoInvoke. lProbe is the last argument, so a premature free would make
  // the subsequent IAsynchRequest inspection a use-after-free (issue #900).
  lRet := lService.SendPerson(lProbe);
  try
    Assert.AreEqual<Integer>(0, GFreeCountAtAsyncCheck,
      'Body was already freed when the IAsynchRequest check ran (use-after-free, issue #900)');
    Assert.AreEqual<Integer>(1, GProbeFreeCount,
      'Owned body must be freed exactly once by the adapter');
    Assert.AreEqual('Peter', lRet.FirstName);
    Assert.AreEqual('Parker', lRet.LastName);
  finally
    lRet.Free;
  end;
end;

procedure TTestRESTAdapter.TestApplicationJSONWithHeaderTextPlain;
var
  Resp: IMVCRESTResponse;
begin
  // expected 404 because is not consumed text/plain
  Resp := TESTService.ApplicationJSONWithTextPlainHeader;
  Assert.AreEqual<Integer>(404, Resp.StatusCode);
end;

procedure TTestRESTAdapter.TestGetPeople;
var
  ListPerson: TObjectList<TPerson>;
begin
  ListPerson := TESTService.GetPeople;
  try
    ListPerson.OwnsObjects := True;
    Assert.isTrue(ListPerson.Count > 0);
    Assert.AreEqual('Tony', ListPerson[0].FirstName);
    Assert.AreEqual('Stark', ListPerson[0].LastName);
  finally
    ListPerson.Free;
  end;
end;

procedure TTestRESTAdapter.TestGetPeopleAsynch;
var
  AsynchRequest: IAsynchRequest;
  People: TPeople;
  LEvt: TEvent;
begin
  LEvt := TEvent.Create;
  try
    AsynchRequest := TAsynchRequest.Create(
      procedure(AValue: TValue)
      begin
        People := AValue.AsType<TPeople>;
        LEvt.SetEvent;
      end);
    TESTService.GetPeopleAsynch(AsynchRequest);

    // wait for max 5 seconds
    Assert.isTrue(TWaitResult.wrSignaled = LEvt.WaitFor(5000), 'Timeout request');
    Assert.IsNotNull(People);
    try
      People.OwnsObjects := True;
      Assert.isTrue(People.Count > 0);
      Assert.AreEqual('Tony', People[0].FirstName);
      Assert.AreEqual('Stark', People[0].LastName);
    finally
      People.Free;
    end;
  finally
    LEvt.Free;
  end;
end;

initialization

TDUnitX.RegisterTestFixture(TTestRESTAdapter);

finalization

end.
