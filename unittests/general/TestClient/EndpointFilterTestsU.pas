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

unit EndpointFilterTestsU;

// TMVCEngine.Use: the post-routing filters, engine-wide, around controller actions and
// Minimal API routes, and the filter bundles (IMVCFilterBundle) on the engine and on route groups.
// The server side is EndpointFilterSupportU (TestServer folder).
//
// Two kinds of server: the TestServer on 8888, which is whatever host the suite is run against
// (WebBroker, Indy Direct, HTTP.sys, ISAPI, Apache), and two in-process servers on Indy Direct and
// HTTP.sys. There is no in-process WebBroker server here: that host keeps the engine configuration
// and a cache of web modules for the whole process, so a second WebBroker server with another engine
// would answer the requests of the others.

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Server.Intf;

const
  EF_TESTSERVER_PORT = 8888;
  EF_INDY_PORT = 8931;
  EF_HTTPSYS_PORT = 8933;

type
  [TestFixture]
  TEndpointFilterTests = class
  private
    fIndy: IMVCServer;
    fHttpSys: IMVCServer;
    fIndyEngine: TMVCEngine;
    fHttpSysEngine: TMVCEngine;
  public
    [SetupFixture]
    procedure SetupFixture;
    [TeardownFixture]
    procedure TeardownFixture;

    [Test]
    [TestCase('TestServer', '8888')]
    [TestCase('IndyDirect', '8931')]
    [TestCase('HttpSys', '8933')]
    procedure TheFilterSeesTheControllerAndTheAction(const APort: Integer);
    [Test]
    [TestCase('TestServer', '8888')]
    [TestCase('IndyDirect', '8931')]
    [TestCase('HttpSys', '8933')]
    procedure TheFirstRegisteredIsTheOutermost(const APort: Integer);
    [Test]
    [TestCase('TestServer', '8888')]
    [TestCase('IndyDirect', '8931')]
    [TestCase('HttpSys', '8933')]
    procedure ALegacyProcedureActionGoesThroughTheFilters(const APort: Integer);
    [Test]
    [TestCase('TestServer', '8888')]
    [TestCase('IndyDirect', '8931')]
    [TestCase('HttpSys', '8933')]
    procedure AFilterThatAnswersStopsTheAction(const APort: Integer);
    [Test]
    [TestCase('TestServer', '8888')]
    [TestCase('IndyDirect', '8931')]
    [TestCase('HttpSys', '8933')]
    procedure AnActionThatRaisesAnswers500AndTheNextRequestIsFine(const APort: Integer);
    [Test]
    [TestCase('TestServer', '8888')]
    [TestCase('IndyDirect', '8931')]
    [TestCase('HttpSys', '8933')]
    procedure ARouteOfTheMinimalAPIGoesThroughTheFiltersToo(const APort: Integer);
    // the HTTP filters (pre-routing) wrap the action of a controller: what comes after ANext() sees its answer
    [Test]
    [TestCase('TestServer', '8888')]
    [TestCase('IndyDirect', '8931')]
    [TestCase('HttpSys', '8933')]
    procedure TheCodeAfterNextOfAnHTTPFilterRunsAfterTheControllerAction(const APort: Integer);

    // filter bundles
    [Test]
    [TestCase('TestServer', '8888')]
    [TestCase('IndyDirect', '8931')]
    [TestCase('HttpSys', '8933')]
    procedure ABundleRunsItsFiltersInTheOrderOfItsLists(const APort: Integer);
    [Test]
    [TestCase('TestServer', '8888')]
    [TestCase('IndyDirect', '8931')]
    [TestCase('HttpSys', '8933')]
    procedure ABundleOnTwoGroupsPutsItsHTTPPartOnTheEngineOnce(const APort: Integer);
    [Test]
    procedure TheSameBundleTwiceOnTheEngineRaises;
    [Test]
    procedure TheSameBundleOnTheEngineAndOnAGroupRaises;
    [Test]
    procedure AnEmptyBundleRegistersNothing;
    [Test]
    procedure ANilBundleRaises;
    // its HTTP part would run engine-wide and protect nothing of the group
    [Test]
    procedure ABundleWithoutAnEndpointPartOnAGroupRaises;
    // Prefix copies the filters of the parent: the endpoint part would run twice
    [Test]
    procedure TheSameBundleOnAGroupAndOnANestedGroupRaises;
  end;

implementation

uses
  MVCFramework.MinimalAPI,
  MVCFramework.Filters,
  MVCFramework.Server.Factory,
  MVCFramework.RESTClient,
  MVCFramework.RESTClient.Intf,
  EndpointFilterSupportU;

function NewEFEngine: TMVCEngine;
begin
  Result := TMVCEngine.Create;
  ConfigureEndpointFilterSupport(Result);
end;

function EFClient(const APort: Integer): IMVCRESTClient;
begin
  Result := TMVCRESTClient.New.BaseURL('http://localhost:' + APort.ToString);
end;

function RunsOf(const APort: Integer): Integer;
begin
  Result := StrToInt(EFClient(APort).Get('/ef/runs').Content);
end;

{ TEndpointFilterTests }

procedure TEndpointFilterTests.SetupFixture;
begin
  fIndyEngine := NewEFEngine;
  fIndy := TMVCServerFactory.CreateIndyDirect(fIndyEngine);
  fIndy.Listen(EF_INDY_PORT, 'localhost');
  fHttpSysEngine := NewEFEngine;
  fHttpSys := TMVCServerFactory.CreateHttpSys(fHttpSysEngine);
  fHttpSys.Listen(EF_HTTPSYS_PORT, 'localhost');
end;

procedure TEndpointFilterTests.TeardownFixture;
begin
  // the server does not own the engine
  fIndy.Stop;
  fIndy := nil;
  FreeAndNil(fIndyEngine);
  fHttpSys.Stop;
  fHttpSys := nil;
  FreeAndNil(fHttpSysEngine);
end;

procedure TEndpointFilterTests.TheFilterSeesTheControllerAndTheAction(const APort: Integer);
var
  lResp: IMVCRESTResponse;
begin
  lResp := EFClient(APort).Get('/ef/fine');
  Assert.AreEqual(200, lResp.StatusCode);
  Assert.AreEqual('fine', lResp.Content, 'the answer of the action is intact');
  Assert.IsTrue(lResp.HeaderValue('X-EF-Controller').EndsWith('TEndpointFilterController'),
    'controller: ' + lResp.HeaderValue('X-EF-Controller'));
  Assert.AreEqual('Fine', lResp.HeaderValue('X-EF-Action'));
end;

procedure TEndpointFilterTests.TheFirstRegisteredIsTheOutermost(const APort: Integer);
var
  lResp: IMVCRESTResponse;
begin
  lResp := EFClient(APort).Get('/ef/trace');
  Assert.AreEqual(200, lResp.StatusCode);
  Assert.AreEqual('A>B>', lResp.Content);
end;

procedure TEndpointFilterTests.ALegacyProcedureActionGoesThroughTheFilters(const APort: Integer);
var
  lResp: IMVCRESTResponse;
begin
  lResp := EFClient(APort).Get('/ef/legacy');
  Assert.AreEqual(200, lResp.StatusCode);
  Assert.AreEqual('legacy', lResp.Content);
  Assert.AreEqual('Legacy', lResp.HeaderValue('X-EF-Action'));
end;

procedure TEndpointFilterTests.AFilterThatAnswersStopsTheAction(const APort: Integer);
var
  lResp: IMVCRESTResponse;
  lRunsBefore: Integer;
begin
  lRunsBefore := RunsOf(APort);
  lResp := EFClient(APort).AddHeader('X-EF-Deny', '1').Get('/ef/fine');
  Assert.AreEqual(403, lResp.StatusCode);
  Assert.Contains(lResp.Content, 'denied by the filter');
  Assert.AreEqual(lRunsBefore, RunsOf(APort), 'the action did not run');
end;

procedure TEndpointFilterTests.AnActionThatRaisesAnswers500AndTheNextRequestIsFine(const APort: Integer);
var
  lClient: IMVCRESTClient;
begin
  lClient := EFClient(APort);
  Assert.AreEqual(500, lClient.Get('/ef/boom').StatusCode);
  Assert.AreEqual(200, lClient.Get('/ef/fine').StatusCode);
end;

procedure TEndpointFilterTests.ARouteOfTheMinimalAPIGoesThroughTheFiltersToo(const APort: Integer);
var
  lResp: IMVCRESTResponse;
begin
  lResp := EFClient(APort).Get('/efmin');
  Assert.AreEqual(200, lResp.StatusCode);
  Assert.Contains(lResp.Content, 'from the route');
  Assert.AreEqual('', lResp.HeaderValue('X-EF-Controller'), 'a route has no controller class');
  Assert.AreEqual('GET /efmin', lResp.HeaderValue('X-EF-Action'));
end;

procedure TEndpointFilterTests.TheCodeAfterNextOfAnHTTPFilterRunsAfterTheControllerAction(const APort: Integer);
var
  lResp: IMVCRESTResponse;
begin
  lResp := EFClient(APort).Get('/ef/hforder');
  Assert.AreEqual(200, lResp.StatusCode);
  Assert.AreEqual('order', lResp.Content);
  Assert.AreEqual('pre action post', lResp.HeaderValue('X-HF-Order'), 'pre, then the action, then post');
  Assert.AreNotEqual('0', lResp.HeaderValue('X-HF-Saw-Body'), 'the code after ANext() saw the body');
end;

procedure TEndpointFilterTests.ABundleRunsItsFiltersInTheOrderOfItsLists(const APort: Integer);
var
  lResp: IMVCRESTResponse;
begin
  lResp := EFClient(APort).Get('/ef/bundle');
  Assert.AreEqual(200, lResp.StatusCode);
  Assert.AreEqual('[h1 h2 e1 e2 ]', lResp.Content);
end;

procedure TEndpointFilterTests.ABundleOnTwoGroupsPutsItsHTTPPartOnTheEngineOnce(const APort: Integer);
begin
  Assert.Contains(EFClient(APort).Get('/efb/g1/trace').Content, '[h e ]', 'first group');
  Assert.Contains(EFClient(APort).Get('/efb/g2/trace').Content, '[h e ]', 'second group');
  Assert.Contains(EFClient(APort).Get('/efb/out').Content, '[h ]', 'outside the groups: only the HTTP part');
end;

function NoOpHTTPFilter: TMVCHTTPFilter;
begin
  Result :=
    procedure(const AContext: TWebContext; const ANext: TMVCHTTPFilterNext)
    begin
      ANext();
    end;
end;

function NoOpEndpointFilter: TMVCEndpointFilter;
begin
  Result :=
    function(const AContext: TWebContext; const ANext: TMVCEndpointFilterNext): IMVCResponse
    begin
      Result := ANext();
    end;
end;

procedure TEndpointFilterTests.TheSameBundleTwiceOnTheEngineRaises;
var
  lEngine: TMVCEngine;
  lBundle: IMVCFilterBundle;
begin
  lEngine := TMVCEngine.Create;
  try
    lBundle := TMVCFilterBundle.Create([NoOpHTTPFilter()], [NoOpEndpointFilter()]);
    lEngine.Use(lBundle);
    Assert.WillRaise(
      procedure
      begin
        lEngine.Use(lBundle);
      end, EMVCConfigException);
    Assert.AreEqual(1, Integer(Length(lEngine.HTTPFilters)));
    Assert.AreEqual(1, Integer(Length(lEngine.EndpointFilters)));
  finally
    lEngine.Free;
  end;
end;

procedure TEndpointFilterTests.TheSameBundleOnTheEngineAndOnAGroupRaises;
var
  lEngine: TMVCEngine;
  lEngineFirst, lGroupFirst: IMVCFilterBundle;
begin
  lEngine := TMVCEngine.Create;
  try
    lEngineFirst := TMVCFilterBundle.Create([NoOpHTTPFilter()], [NoOpEndpointFilter()]);
    lEngine.Use(lEngineFirst);
    Assert.WillRaise(
      procedure
      begin
        lEngine.Prefix('/a').Use(lEngineFirst);
      end, EMVCConfigException); // engine, then a group
    lGroupFirst := TMVCFilterBundle.Create([NoOpHTTPFilter()], [NoOpEndpointFilter()]);
    lEngine.Prefix('/b').Use(lGroupFirst);
    Assert.WillRaise(
      procedure
      begin
        lEngine.Use(lGroupFirst);
      end, EMVCConfigException); // a group, then the engine
  finally
    lEngine.Free;
  end;
end;

procedure TEndpointFilterTests.AnEmptyBundleRegistersNothing;
var
  lEngine: TMVCEngine;
  lEmpty: IMVCFilterBundle;
begin
  lEngine := TMVCEngine.Create;
  try
    lEmpty := TMVCFilterBundle.Create([], []);
    // nothing to register, so nothing to clash with either
    lEngine.Use(lEmpty).Use(lEmpty);
    lEngine.Prefix('/a').Use(lEmpty);
    // created in the call (a const parameter): the engine keeps no reference, the bundle must not leak
    lEngine.Use(TMVCFilterBundle.Create([], []));
    lEngine.Prefix('/a').Use(TMVCFilterBundle.Create([], []));
    Assert.AreEqual(0, Integer(Length(lEngine.HTTPFilters)));
    Assert.AreEqual(0, Integer(Length(lEngine.EndpointFilters)));
  finally
    lEngine.Free;
  end;
end;

procedure TEndpointFilterTests.ANilBundleRaises;
var
  lEngine: TMVCEngine;
  lNil: IMVCFilterBundle;
begin
  lNil := nil;
  lEngine := TMVCEngine.Create;
  try
    Assert.WillRaise(
      procedure
      begin
        lEngine.Use(lNil);
      end, EMVCConfigException); // engine
    Assert.WillRaise(
      procedure
      begin
        lEngine.Prefix('/a').Use(lNil);
      end, EMVCConfigException); // group
  finally
    lEngine.Free;
  end;
end;

procedure TEndpointFilterTests.ABundleWithoutAnEndpointPartOnAGroupRaises;
var
  lEngine: TMVCEngine;
begin
  lEngine := TMVCEngine.Create;
  try
    Assert.WillRaise(
      procedure
      begin
        lEngine.Prefix('/a').Use(TMVCFilterBundle.Create([NoOpHTTPFilter()], []));
      end, EMVCConfigException);
    Assert.WillRaise(
      procedure
      begin
        lEngine.Prefix('/reports').Use(FileServerBundle('/reports', '.'));
      end, EMVCConfigException, 'FileServerBundle goes on the engine');
    Assert.AreEqual(0, Integer(Length(lEngine.HTTPFilters)), 'nothing registered');
    lEngine.Use(FileServerBundle('/reports', '.'));
    Assert.AreEqual(1, Integer(Length(lEngine.HTTPFilters)), 'on the engine');
  finally
    lEngine.Free;
  end;
end;

procedure TEndpointFilterTests.TheSameBundleOnAGroupAndOnANestedGroupRaises;
var
  lEngine: TMVCEngine;
  lBundle: IMVCFilterBundle;
  lGroup: TMVCRouteGroup<TObject>;
begin
  lEngine := TMVCEngine.Create;
  try
    lBundle := TMVCFilterBundle.Create([NoOpHTTPFilter()], [NoOpEndpointFilter()]);
    lGroup := lEngine.Prefix('/a').Use(lBundle);
    Assert.WillRaise(
      procedure
      begin
        lGroup.Prefix('/b').Use(lBundle);
      end, EMVCConfigException);
    lEngine.Prefix('/c').Use(lBundle); // a group beside it: allowed
    Assert.AreEqual(1, Integer(Length(lEngine.HTTPFilters)), 'the HTTP part once');
  finally
    lEngine.Free;
  end;
end;

initialization

TDUnitX.RegisterTestFixture(TEndpointFilterTests);

end.
