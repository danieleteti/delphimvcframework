// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2017 Daniele Teti and the DMVCFramework Team
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
  MVCFramework.RESTAdapter, DUnitX.TestFramework, BusinessObjectsU,
  Generics.Collections,
{$IFDEF SYSTEMJSON}
  System.JSON,
{$ELSE}
  Data.DBXJSON,
  Data.SqlExpr,
  DBXCommon,
{$ENDIF}
  ObjectsMappers, MVCFramework.RESTClient, MVCFramework.Commons;

type

  [Headers('User-Agent', 'RESTAdapter-Test')]
  ITESTService = interface(IInvokable)
    ['{58B9FA23-92F4-4B8E-814B-05232F32A41F}']

    [RESTResource(HttpGet, '/people')]
    [MapperListOf(TPerson)]
    function GetPeople: TObjectList<TPerson>;

    [RESTResource(HttpGet, '/people')]
    [MapperListOf(TPerson)]
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

    [RESTResource(HttpGet, '/people')]
    function GetPersonInJSONArray: TJSONArray;

    [Headers('Accept', 'application/json')]
    [Headers('ContentType', 'application/json')]
    [RESTResource(HttpGet, '/testconsumejson')]
    function HeadersApplicationJSON: TJSONValue;

    [Headers('Accept', 'text/plain')]
    [Headers('ContentType', 'text/plain')]
    [RESTResource(HttpGet, '/testconsumes')]
    function HeadersTextPlain: string;

    [Headers('Accept', 'text/plain')]
    [Headers('ContentType', 'text/plain')]
    [RESTResource(HttpGet, '/testconsumejson')]
    function ApplicationJSONWithTextPlainHeader: IRESTResponse;

  end;

  [TestFixture]
  TTestRESTAdapter = class(TObject)
  private
    RESTAdapter: TRESTAdapter<ITESTService>;
    TESTService: ITESTService;
  protected
  [Setup]
	procedure SetUp;
  published
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

uses System.SysUtils, System.Rtti, System.SyncObjs;

{ TTestRESTAdapter }

procedure TTestRESTAdapter.SetUp;
begin
  inherited;
  RESTAdapter := TRESTAdapter<ITESTService>.Create;
  TESTService := RESTAdapter.Build('localhost', 9999);
end;

procedure TTestRESTAdapter.TestGetPersonByID;
var
  Person: TPerson;
begin;
  Person := TESTService.GetPersonByID(1);
  try
    Assert.areEqual('Tony', Person.FirstName);
    Assert.areEqual('Stark', Person.LastName);
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
    Assert.areEqual('Tony', Person.FirstName);
    Assert.areEqual('Stark', Person.LastName);
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
    Assert.isNotNull(Person);
    try
      Assert.areEqual('Tony', Person.FirstName);
      Assert.areEqual('Stark', Person.LastName);
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
    Assert.areEqual('Hello World', Res.GetValue('key').Value);
  finally
    Res.Free;
  end;
end;

procedure TTestRESTAdapter.TestHeadersTextPlain;
var
  Res: string;
begin
  Res := TESTService.HeadersTextPlain;
  Assert.areEqual('Hello World', Res);
end;

procedure TTestRESTAdapter.TestPostPerson;
var
  Person: TPerson;
  RetPerson: TPerson;
begin
  Person := TPerson.GetNew('Peter', 'Parker', 0, false);
  RetPerson := TESTService.SendPerson(Person);
  try
    Assert.areEqual('Peter', RetPerson.FirstName);
    Assert.areEqual('Parker', RetPerson.LastName);
    Assert.isFalse(RetPerson.Married);
  finally
    RetPerson.Free;
  end;
end;

procedure TTestRESTAdapter.TestApplicationJSONWithHeaderTextPlain;
var
  Resp: IRESTResponse;
begin
  // expected 404 because is not consumed text/plain
  Resp := TESTService.ApplicationJSONWithTextPlainHeader;
  Assert.areEqual(404, Resp.ResponseCode);
end;

procedure TTestRESTAdapter.TestGetPeople;
var
  ListPerson: TObjectList<TPerson>;
begin
  ListPerson := TESTService.GetPeople;
  try
    Assert.isTrue(ListPerson.Count > 0);
    Assert.areEqual('Tony', ListPerson[0].FirstName);
    Assert.areEqual('Stark', ListPerson[0].LastName);
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

    // attend for max 5 seconds
    Assert.isTrue(TWaitResult.wrSignaled = LEvt.WaitFor(5000), 'Timeout request');
    Assert.isNotNull(People);
    try
      Assert.isTrue(People.Count > 0);
      Assert.areEqual('Tony', People[0].FirstName);
      Assert.areEqual('Stark', People[0].LastName);
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
