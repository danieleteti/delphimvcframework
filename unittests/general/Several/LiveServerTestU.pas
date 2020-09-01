// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2020 Daniele Teti and the DMVCFramework Team
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

unit LiveServerTestU;

interface

uses
  DUnitX.TestFramework,
  MVCFramework.RESTClient.Intf,
  MVCFramework.RESTClient,
  MVCFramework.JSONRPC.Client,
  System.DateUtils,
  System.Hash;

type

  TBaseServerTest = class(TObject)
  protected
    RESTClient: IMVCRESTClient;
    procedure DoLoginWith(UserName: string);
    procedure DoLogout;
  public
    [Setup]
    procedure Setup; virtual;
    [TearDown]
    procedure TearDown;

  end;

  [TestFixture]
  TServerTest = class(TBaseServerTest)
  public
    [Test]
    [TestCase('request url /fault', '/exception/fault')]
    [TestCase('request url /fault2', '/exception/fault2')]
    procedure TestControllerWithExceptionInCreate(const URLSegment: string);

    [Test]
    procedure TestReqWithParams;

    // URL_MAPPED_PARAMS_ALLOWED_CHARS = ' ������@\[\]\{\}\(\)\=;&#\.\_\,%\w\d\x2D\x3A';
    [Test]
    [TestCase('1', ' �,�')]
    [TestCase('2', '�,�,�')]
    [TestCase('3', '�,@,[')]
    [TestCase('4', '],{,}')]
    [TestCase('5', '(,),\')]
    [TestCase('6', '=,;,&')]
    [TestCase('7', '#,.,_')]
    [TestCase('8', '%, , ')]
    procedure TestReqWithURLMappedParams(const par1, par2, par3: string);
    [Test]
    procedure TestPOSTWithParamsAndJSONBody;
    [Test]
    procedure TestPOSTWithObjectJSONBody;
    [Test]
    procedure TestCustomerEcho;
    [Test]
    procedure TestXHTTPMethodOverride_POST_as_PUT;
    [Test]
    procedure TestPUTWithParamsAndJSONBody;
    [Test]
    procedure TestCookies;
    [Test]
    procedure TestSessionWithLogin;
    [Test]
    procedure TestSession;
    [Test]
    procedure TestInvalidateSession;
    [Test]
    procedure TestAsynchRequestPOST;
    [Test]
    procedure TestAsynchRequestPUT;
    [Test]
    procedure TestAsynchRequestGET;
    [Test]
    procedure TestAsynchRequestDELETE;
    [Test]
    procedure TestEncodingRenderJSONValue;
    [Test]
    procedure TestRenderWrappedList;
    [Test]
    procedure TestRenderActionInCollections;
    [Test]
    procedure TestRenderWrappedListWithCompression;
    [Test]
    procedure TestRenderStreamAndFreeWithOwnerFalse;
    [Test]
    procedure TestRenderStreamAndFreeWithOwnerTrue;
    [Test]
    procedure TestObjectDict;
    [Test]
    procedure TestGetImagePng;
    [Test]
    procedure TestProducesConsumes01;
    [Test]
    procedure TestProducesConsumes02;
    [Test]
    procedure TestProducesConsumes03;
    [Test]
    procedure TestProducesConsumesWithWrongAcceptHeader;
    [Test]
    procedure TestExceptionInMVCAfterCreate;
    [Test]
    procedure TestExceptionInMVCBeforeDestroy;
    [Test]
    procedure TestActionFiltersOnBeforeAction;
    [Test]
    procedure TestMiddlewareSpeedMiddleware;
    [Test]
    procedure TestMiddlewareHandler;
    [Test]
    procedure TestPostAListOfObjects;
    // test authentication/authorization with BasicAuth
    [Test]
    procedure TestBasicAuth01;
    [Test]
    procedure TestBasicAuth02;
    [Test]
    procedure TestBasicAuth03;
    [Test]
    procedure TestBasicAuth04;
    [Test]
    procedure TestBasicAuth05;
    // test authentication/authorization with CustomAuth
    [Test]
    procedure TestCustomAuthRequestWithoutLogin;
    [Test]
    procedure TestCustomAuthRequestsWithValidLogin;
    [Test]
    procedure TestCustomAuthRequestsWithValidLogin_HTML;
    [Test]
    procedure TestCustomAuthWrongRequestBodies;
    [Test]
    procedure TestCustomAuthLoginLogout;

    // typed actions
    [Test]
    procedure TestTypedString1;
    [Test]
    procedure TestTypedInteger1;
    [Test]
    procedure TestTypedInt641;
    [Test]
    procedure TestTypedSingle1;
    [Test]
    procedure TestTypedDouble1;
    [Test]
    procedure TestTypedExtended1;
    [Test]
    procedure TestTypedAll;
    [Test]
    procedure TestTypedDateTimeTypes;
    [Test]
    procedure TestTypedBooleans;
    [Test]
    procedure TestTypedTGuid1;
    [Test]
    procedure TestStringDictionary;
    [Test]
    procedure TestWrongJSONBody;
    [Test]
    procedure TestTypedIntegerWrongParam1;

    // test nullables
    [Test]
    procedure TestDeserializeNullablesWithValue;
    [Test]
    procedure TestDeserializeNullablesWithNulls;
    [Test]
    procedure TestSerializeAndDeserializeNullables;
    [Test]
    procedure TestSerializeAndDeserializeNullables_ISSUE_362;
    [Test]
    procedure TestSerializeAndDeserializeNullables_Passing_Integers_InsteadOf_Floats;

    // test responses objects
    [Test]
    procedure TestResponseCreated;
    [Test]
    procedure TestResponseNoContent;
    [Test]
    procedure TestResponseAccepted;

    // test web server
    [Test]
    procedure TestDirectoryTraversal1;
    [Test]
    procedure TestDirectoryTraversal2;
    [Test]
    procedure TestDirectoryRedirect;
    [Test]
    procedure TestSPASupport;
    // test server side views
    [Test]
    procedure TestViewDataViewDataSet;

  end;

  [TestFixture]
  TJSONRPCServerTest = class(TObject)
  protected
    FExecutor: IMVCJSONRPCExecutor;
    FExecutor2: IMVCJSONRPCExecutor;
    FExecutor3: IMVCJSONRPCExecutor;
  public
    [Setup]
    procedure Setup;
    [Test]
    procedure TestRequestWithoutParams;
    [Test]
    procedure TestNotificationWithoutParams;
    [Test]
    procedure TestNotificationWhichRaisesError;
    [Test]
    procedure TestRequestToNotFoundMethod;
    [Test]
    procedure TestRequestWithParams_I_I_ret_I;
    [Test]
    procedure TestRequestWithNamedParams_I_I_ret_I;
    [Test]
    procedure TestRequestWithParams_I_I_I_ret_O;
    [Test]
    procedure TestRequestWithNamedParams_I_I_I_ret_O;
    [Test]
    procedure TestRequestWithWrongNamedParams;
    [Test]
    procedure TestRequest_S_I_ret_S;
    [Test]
    procedure TestRequest_NamedParams_S_I_ret_S;
    [Test]
    procedure TestRequestWithParams_I_I_ret_A;
    [Test]
    procedure TestRequestWithParams_DT_T_ret_DT;
    // hooks tests
    [Test]
    procedure TestHooks;
    [Test]
    procedure TestHooksWhenMethodRaisesError;
    [Test]
    procedure TestHooksWhenOnAfterCallHookRaisesError;
    [Test]
    procedure TestHooksNotif;
    [Test]
    procedure TestHooksNotifWhenOnBeforeRoutingHookRaisesError;
    [Test]
    procedure TestHooksNotifWhenOnBeforeCallHookRaisesError;
    [Test]
    procedure TestHooksNotifWhenOnAfterCallHookRaisesError;
    [Test]
    procedure TestHooksWhenOnBeforeCallHookRaisesError;
    [Test]
    procedure TestHooksWhenOnBeforeRoutingHookRaisesError;

  end;

implementation

uses
  System.Math,
  System.JSON,
  MVCFramework.Serializer.Defaults,
  JsonDataObjects,
  MVCFramework.Serializer.JsonDataObjects,
  MVCFramework.Commons,
  System.SyncObjs,
  System.Generics.Collections,
  System.SysUtils,
  BusinessObjectsU,
  MVCFramework.Serializer.Commons,
  Soap.EncdDecd,
  System.Classes,
  MVCFramework.SystemJSONUtils,
  IdCookie,
  MVCFramework.JSONRPC,
  MVCFramework.Serializer.Intf
{$IFDEF MSWINDOWS}
    ,
  MVCFramework.Serializer.JsonDataObjects.OptionalCustomTypes,
  Vcl.Graphics
{$ENDIF}
    , TestConstsU;

function GetServer: string;
begin
  Result := 'http://' + TEST_SERVER_ADDRESS + ':9999';
end;

{ TServerTest }

procedure TBaseServerTest.DoLogout;
var
  res: IMVCRESTResponse;
begin
  res := RESTClient.Get('/logout');
  Assert.isTrue(res.StatusCode = HTTP_STATUS.OK, 'Logout Failed');
end;

procedure TBaseServerTest.Setup;
begin
  inherited;
  RESTClient := TMVCRESTClient.New
    .BaseURL(TEST_SERVER_ADDRESS, 9999)
    .ReadTimeout(60 * 1000 * 30);
end;

procedure TBaseServerTest.TearDown;
begin
  inherited;
  RESTClient := nil;
end;

procedure TServerTest.TestActionFiltersOnBeforeAction;
var
  res: IMVCRESTResponse;
begin
  res := RESTClient.Get('/actionfilters/beforeaction/alwayscalled');
  Assert.areEqual<Integer>(HTTP_STATUS.OK, res.StatusCode);

  res := RESTClient.Get('/actionfilters/beforeaction/nevercalled');
  Assert.areEqual<Integer>(HTTP_STATUS.NotFound, res.StatusCode);
end;

procedure TServerTest.TestAsynchRequestDELETE;
var
  evt: TEvent;
  r: TWaitResult;
  OK: boolean;
begin
  OK := false;
  evt := TEvent.Create;
  try
    RESTClient.Async(
      procedure(Response: IMVCRESTResponse)
      begin
        OK := true;
        evt.SetEvent;
      end,
      procedure(E: Exception)
      begin
        OK := false;
      end)
      .AddPathParam('param1', '1')
      .AddPathParam('param2', '2')
      .AddPathParam('param3', '3')
      .Delete('/req/with/params/($param1)/($param2)/($param3)');

    // wait for thred finish
    repeat
      r := evt.WaitFor(2000);
    until r = TWaitResult.wrSignaled;

    Assert.areEqual(true, OK);
  finally
    evt.Free;
  end;
end;

procedure TServerTest.TestAsynchRequestGET;
var
  evt: TEvent;
  r: TWaitResult;
  j: System.JSON.TJSONObject;
begin
  j := nil;
  evt := TEvent.Create;
  try
    RESTClient.Async(
      procedure(Response: IMVCRESTResponse)
      begin
        try
          j := TSystemJSON.StringAsJSONObject(Response.Content);
        except
          // test should not block...never!
        end;
        evt.SetEvent;
      end,
      procedure(E: Exception)
      begin
      end)
      .AddPathParam('param1', '1')
      .AddPathParam('param2', '2')
      .AddPathParam('param3', '3')
      .Get('/req/with/params/{param1}/{param2}/{param3}');

    // wait for thred finish
    repeat
      r := evt.WaitFor(2000);
    until r = TWaitResult.wrSignaled;

    Assert.isTrue(Assigned(j));
    Assert.areEqual('1', j.Get('par1').JsonValue.Value);
    j.Free;
  finally
    evt.Free;
  end;
end;

procedure TServerTest.TestAsynchRequestPOST;
var
  evt: TEvent;
  r: TWaitResult;
  j: System.JSON.TJSONObject;
begin
  j := nil;
  evt := TEvent.Create;
  try
    RESTClient.Async(
      procedure(Response: IMVCRESTResponse)
      begin
        try
          j := TSystemJSON.StringAsJSONObject(Response.Content);
        except
          // test should not block...never!
        end;
        evt.SetEvent;
      end,
      procedure(E: Exception)
      begin
      end)
      .AddPathParam('param1', '1')
      .AddPathParam('param2', '2')
      .AddPathParam('param3', '3')
      .Post('/echo/{param1}/($param2)/{param3}',
      TSystemJSON.JSONValueToString(System.JSON.TJSONObject.Create(TJSONPair.Create('from client', 'hello world'))));

    // wait for thred finish
    repeat
      r := evt.WaitFor(2000);
    until r = TWaitResult.wrSignaled;

    Assert.isTrue(Assigned(j));
    Assert.areEqual('from server', j.Get('echo').JsonValue.Value);
    j.Free;
  finally
    evt.Free;
  end;
end;

procedure TServerTest.TestAsynchRequestPUT;
var
  evt: TEvent;
  r: TWaitResult;
  j: System.JSON.TJSONObject;
begin
  j := nil;
  evt := TEvent.Create;
  try
    RESTClient.Async(
      procedure(Response: IMVCRESTResponse)
      begin
        try
          j := TSystemJSON.StringAsJSONObject(Response.Content);
        except
          // test should not block...never!
        end;
        evt.SetEvent;
      end,
      procedure(E: Exception)
      begin
      end)
      .AddPathParam('param1', '1')
      .AddPathParam('param2', '2')
      .AddPathParam('param3', '3')
      .Put('/echo/{param1}/($param2)/{param3}',
      TSystemJSON.JSONValueToString(System.JSON.TJSONObject.Create(System.JSON.TJSONPair.Create('from client',
      'hello world'))));

    // wait for thred finish
    repeat
      r := evt.WaitFor(2000);
    until r = TWaitResult.wrSignaled;

    Assert.isTrue(Assigned(j));
    Assert.areEqual('from server', j.Get('echo').JsonValue.Value);
    j.Free;
  finally
    evt.Free;
  end;
end;

procedure TServerTest.TestBasicAuth01;
var
  lRes: IMVCRESTResponse;
begin
  RESTClient.SetBasicAuthorization('user1', 'user1');
  Assert.AreEqual('Basic dXNlcjE6dXNlcjE=', RESTClient.HeaderValue('Authorization'));
  lRes := RESTClient.Get('/private/role1');
  Assert.areEqual<Integer>(HTTP_STATUS.OK, lRes.StatusCode);
end;

procedure TServerTest.TestBasicAuth02;
var
  lRes: IMVCRESTResponse;
begin
  lRes := RESTClient.Get('/private/role1');
  Assert.areEqual<Integer>(HTTP_STATUS.Unauthorized, lRes.StatusCode);
end;

procedure TServerTest.TestBasicAuth03;
var
  lRes: IMVCRESTResponse;
begin
  RESTClient.SetBasicAuthorization('user1', 'user1');
  lRes := RESTClient.Get('/private/role2');
  Assert.areEqual<Integer>(HTTP_STATUS.Forbidden, lRes.StatusCode);
end;

procedure TServerTest.TestBasicAuth04;
var
  lRes: IMVCRESTResponse;
begin
  RESTClient.SetBasicAuthorization('user1', 'user1');

  lRes := RESTClient.Get('/private/role1');
  Assert.areEqual<Integer>(HTTP_STATUS.OK, lRes.StatusCode);

  lRes := RESTClient.Get('/people');
  Assert.areEqual<Integer>(HTTP_STATUS.OK, lRes.StatusCode);
end;

procedure TServerTest.TestBasicAuth05;
var
  lRes: IMVCRESTResponse;
begin
  RESTClient.SetBasicAuthorization('user1', 'user1');

  // first
  lRes := RESTClient.Get('/private/role1session?value=danieleteti');
  Assert.areEqual<Integer>(HTTP_STATUS.OK, lRes.StatusCode);
  lRes := RESTClient.Get('/private/role1session');
  Assert.areEqual<Integer>(HTTP_STATUS.OK, lRes.StatusCode);
  Assert.areEqual('danieleteti', lRes.Content);

  // second
  lRes := RESTClient.Get('/private/role1session?value=johndoe');
  Assert.areEqual<Integer>(HTTP_STATUS.OK, lRes.StatusCode);
  lRes := RESTClient.Get('/private/role1session');
  Assert.areEqual<Integer>(HTTP_STATUS.OK, lRes.StatusCode);
  Assert.areEqual('johndoe', lRes.Content);
end;

procedure TServerTest.TestControllerWithExceptionInCreate(const URLSegment: string);
var
  res: IMVCRESTResponse;
begin
  res := RESTClient.Get(URLSegment);
  Assert.areEqual(HTTP_STATUS.InternalServerError, res.StatusCode);
  // Assert.Contains(res.ContentType, 'text/plain', true, 'Is not a text/plain in case of error');
  Assert.Contains(res.ContentType, 'application/json', true, 'Is not a application/json in case of error');
  Assert.Contains(res.Content, 'Cannot create controller', true, 'Exception message in body is not correct');
  // Assert.Contains(res.Content, 'Cannot create controller', true, 'Exception message in body is not correct');
end;

procedure TServerTest.TestCookies;
var
  res: IMVCRESTResponse;
  I: Integer;
begin
  res := RESTClient.Get('/lotofcookies');
  Assert.areEqual<Integer>(HTTP_STATUS.OK, res.StatusCode);
  Assert.areEqual(4, res.Cookies.Count, 'Wrong number of cookies');
  for I := 0 to 3 do
  begin
    Assert.areEqual('usersettings' + IntToStr(I + 1), res.Cookies[I].Name);
    Assert.areEqual('usersettings' + IntToStr(I + 1) + '-value', res.Cookies[I].Value);
    Assert.areEqual('/usersettings' + IntToStr(I + 1) + '/', res.Cookies[I].Path);
  end;

end;

procedure TServerTest.TestCustomAuthRequestWithoutLogin;
var
  lRes: IMVCRESTResponse;
begin
  lRes := RESTClient.Get('/privatecustom/role1');
  Assert.areEqual<Integer>(HTTP_STATUS.Unauthorized, lRes.StatusCode);
  Assert.areEqual('/system/users/logged', lRes.HeaderValue('X-LOGIN-URL'));
  Assert.areEqual('POST', lRes.HeaderValue('X-LOGIN-METHOD'));

  lRes := RESTClient.Get('/privatecustom/role2');
  Assert.areEqual<Integer>(HTTP_STATUS.Unauthorized, lRes.StatusCode);
  Assert.areEqual('/system/users/logged', lRes.HeaderValue('X-LOGIN-URL'));
  Assert.areEqual('POST', lRes.HeaderValue('X-LOGIN-METHOD'));
end;

procedure TServerTest.TestCustomAuthRequestsWithValidLogin;
var
  lRes: IMVCRESTResponse;
  lJSON: System.JSON.TJSONObject;
  lCookieValue: string;
begin
  lJSON := System.JSON.TJSONObject.Create;
  try
    lJSON.AddPair('username', 'user1');
    lJSON.AddPair('password', 'user1');
    lRes := RESTClient.Post('/system/users/logged', TSystemJSON.JSONValueToString(lJSON, false));
    Assert.areEqual('application/json', lRes.ContentType);
    Assert.areEqual<Integer>(HTTP_STATUS.OK, lRes.StatusCode);
    Assert.areEqual('/system/users/logged', lRes.HeaderValue('X-LOGOUT-URL'));
    Assert.areEqual('DELETE', lRes.HeaderValue('X-LOGOUT-METHOD'));
    Assert.areEqual('{"status":"OK"}', lRes.Content);
    lCookieValue := lRes.CookieByName(TMVCConstants.SESSION_TOKEN_NAME).Value;
    Assert.AreNotEqual('', lCookieValue, 'Session cookie not returned after login');
    Assert.isFalse(lCookieValue.Contains('invalid'), 'Returned an invalid session token');

    lRes := RESTClient.Get('/privatecustom/role2');
    Assert.areEqual<Integer>(HTTP_STATUS.Forbidden, lRes.StatusCode,
      'Authorization not respected for not allowed action');

    lRes := RESTClient.Get('/privatecustom/role1');
    Assert.areEqual<Integer>(HTTP_STATUS.OK, lRes.StatusCode, 'Authorization not respected for allowed action');
  finally
    lJSON.Free;
  end;
end;

procedure TServerTest.TestCustomAuthRequestsWithValidLogin_HTML;
var
  lRes: IMVCRESTResponse;
  lJSON: System.JSON.TJSONObject;
  lCookieValue: string;
begin
  lJSON := System.JSON.TJSONObject.Create;
  try
    lJSON.AddPair('username', 'user1');
    lJSON.AddPair('password', 'user1');
    lRes := RESTClient.Accept('text/html').Post('/system/users/logged',
      TSystemJSON.JSONValueToString(lJSON, false));
    Assert.areEqual('application/json', lRes.ContentType);
    Assert.areEqual<Integer>(HTTP_STATUS.OK, lRes.StatusCode);
    Assert.areEqual('/system/users/logged', lRes.HeaderValue('X-LOGOUT-URL'));
    Assert.areEqual('DELETE', lRes.HeaderValue('X-LOGOUT-METHOD'));
    Assert.areEqual('{"status":"OK"}', lRes.Content);
    lCookieValue := lRes.CookieByName(TMVCConstants.SESSION_TOKEN_NAME).Value;
    Assert.AreNotEqual('', lCookieValue, 'Session cookie not returned after login');
    Assert.isFalse(lCookieValue.Contains('invalid'), 'Returned an invalid session token');

    lRes := RESTClient.Get('/privatecustom/role2');
    Assert.areEqual<Integer>(HTTP_STATUS.Forbidden, lRes.StatusCode,
      'Authorization not respected for not allowed action');

    lRes := RESTClient.Get('/privatecustom/role1');
    Assert.areEqual<Integer>(HTTP_STATUS.OK, lRes.StatusCode, 'Authorization not respected for allowed action');
  finally
    lJSON.Free;
  end;
end;

procedure TServerTest.TestCustomAuthWrongRequestBodies;
var
  lRes: IMVCRESTResponse;
  lJSON: System.JSON.TJSONObject;
begin
  lJSON := System.JSON.TJSONObject.Create;
  try
    // no request body
    lRes := RESTClient
      .AddBody('', False, TMVCMediaType.APPLICATION_JSON) // To define a Content-Type add an empty body with the type.
      .Post('/system/users/logged');
    Assert.areEqual<Integer>(HTTP_STATUS.BadRequest, lRes.StatusCode,
      'Empty request body doesn''t return HTTP 400 Bad Request');

    // wrong request body 1
    lRes := RESTClient.Post('/system/users/logged', TSystemJSON.JSONValueToString(lJSON, false));
    Assert.areEqual<Integer>(HTTP_STATUS.Unauthorized, lRes.StatusCode,
      'Invalid json doesn''t return HTTP 401 Unauthorized');

    // wrong request body 2
    lJSON.AddPair('username', '');
    lJSON.AddPair('password', '');
    lRes := RESTClient.Post('/system/users/logged', TSystemJSON.JSONValueToString(lJSON, false));
    Assert.areEqual<Integer>(HTTP_STATUS.Unauthorized, lRes.StatusCode,
      'Empty username and password doesn''t return HTTP 401 Unauthorized');

    // wrong username and password 3
    lJSON.RemovePair('username').Free;
    lJSON.RemovePair('password').Free;
    lJSON.AddPair('username', 'notvaliduser');
    lJSON.AddPair('password', 'notvalidpassword');
    lRes := RESTClient.Post('/system/users/logged', TSystemJSON.JSONValueToString(lJSON, false));
    Assert.areEqual<Integer>(HTTP_STATUS.Unauthorized, lRes.StatusCode,
      'Wrong username and password doesn''t return HTTP 401 Unauthorized');
  finally
    lJSON.Free;
  end;
end;

procedure TServerTest.TestCustomerEcho;
var
  r: IMVCRESTResponse;
  lCustomer: TCustomer;
  lSer: IMVCSerializer;
begin
  lCustomer := TCustomer.Create;
  try
    lCustomer.Name := 'bit Time Professionals';
    lCustomer.ContactFirst := 'Daniele'; // transient
    lCustomer.ContactLast := 'Teti'; // transient
    lCustomer.AddressLine1 := 'Via Roma 10';
    lCustomer.AddressLine2 := '00100, ROMA';
    lCustomer.Logo.SetSize(100, 100);
    lCustomer.Logo.Canvas.FillRect(Rect(0, 0, 100, 100));
    lCustomer.Logo.Canvas.Font.Color := clRed;
    lCustomer.Logo.Canvas.TextOut(10, 50, 'From Client');
    lCustomer.Logo.SaveToFile('pippo_client_before_send.bmp');
    lSer := GetDefaultSerializer;
    RegisterOptionalCustomTypesSerializers(lSer);
    r := RESTClient.Accept(TMVCMediaType.APPLICATION_JSON).Post('/customerecho', lSer.SerializeObject(lCustomer));
  finally
    lCustomer.Free;
  end;

  lCustomer := TCustomer.Create;
  try
    lSer := GetDefaultSerializer;
    RegisterOptionalCustomTypesSerializers(lSer);
    lSer.DeserializeObject(r.Content, lCustomer);
    Assert.areEqual('bit Time Professionals changed', lCustomer.Name);
    Assert.areEqual('', lCustomer.ContactFirst);
    Assert.areEqual('', lCustomer.ContactLast);
    lCustomer.Logo.SaveToFile('customer_logo.bmp');
    Assert.areEqual('de2a29ec62fc1f0b3abbb6b74223d214', THashMD5.GetHashStringFromFile('customer_logo.bmp'));
  finally
    lCustomer.Free;
  end;
end;

procedure TServerTest.TestCustomAuthLoginLogout;
var
  lRes: IMVCRESTResponse;
  lJSON: System.JSON.TJSONObject;
  lLogoutUrl: string;
  lPass: boolean;
begin
  lJSON := System.JSON.TJSONObject.Create;
  try
    lJSON.AddPair('username', 'user1');
    lJSON.AddPair('password', 'user1');
    lRes := RESTClient.Post('/system/users/logged', TSystemJSON.JSONValueToString(lJSON, false));

    Assert.areEqual<Integer>(HTTP_STATUS.OK, lRes.StatusCode);
    lLogoutUrl := lRes.HeaderValue('X-LOGOUT-URL');

    lRes := RESTClient.Delete(lLogoutUrl);
    lPass := false;
    if lRes.CookieByName(TMVCConstants.SESSION_TOKEN_NAME).Value.Contains('invalid') then
    begin
      lPass := True;
    end;

    Assert.isTrue(lPass, 'No session cookie cleanup in the response');
  finally
    lJSON.Free;
  end;
end;

procedure TServerTest.TestEncodingRenderJSONValue;
var
  res: IMVCRESTResponse;
  s: string;
  lJSONObj: System.JSON.TJSONObject;
begin
  res := RESTClient.Get('/encoding');

  lJSONObj := TSystemJSON.StringAsJSONObject(res.Content);
  s := lJSONObj.Get('name1').JsonValue.Value;
  Assert.areEqual('j�rn', s);
  lJSONObj.Free;

  lJSONObj := TSystemJSON.StringAsJSONObject(res.Content);
  s := lJSONObj.Get('name3').JsonValue.Value;
  Assert.areEqual('������', s);
  lJSONObj.Free;

  lJSONObj := TSystemJSON.StringAsJSONObject(res.Content);
  s := lJSONObj.Get('name2').JsonValue.Value;
  Assert.areEqual('�to je Unicode?', s, 'If this test fail, check http://qc.embarcadero.com/wc/qcmain.aspx?d=119779');
  lJSONObj.Free;
  { WARNING!!! }
  {
    If this test fail, check
    http://qc.embarcadero.com/wc/qcmain.aspx?d=119779
  }
end;

procedure TServerTest.TestRenderActionInCollections;
var
  lRes: IMVCRESTResponse;
  lJArr: TJDOJsonArray;
  I: Integer;
begin
  lRes := RESTClient.Get('/people/renderaction');
  lJArr := TJsonBaseObject.Parse(lRes.Content) as TJDOJsonArray;
  try
    for I := 0 to lJArr.Count - 1 do
    begin
      Assert.isTrue(lJArr[I].A[TMVCConstants.HATEOAS_PROP_NAME].Count = 2, '_links doesn''t exists');
      Assert.areEqual(lJArr[I].A[TMVCConstants.HATEOAS_PROP_NAME].O[0].s[HATEOAS.REL], 'test0');
      Assert.areEqual(lJArr[I].A[TMVCConstants.HATEOAS_PROP_NAME].O[1].s[HATEOAS.REL], 'test1');
    end;
  finally
    lJArr.Free;
  end;
end;

procedure TServerTest.TestRenderStreamAndFreeWithOwnerFalse;
var
  lRes: IMVCRESTResponse;
begin
  lRes := RESTClient.Get('/renderstreamandfreewithownerfalse');
  Assert.areEqual<Integer>(200, lRes.StatusCode);
end;

procedure TServerTest.TestRenderStreamAndFreeWithOwnerTrue;
var
  lRes: IMVCRESTResponse;
begin
  lRes := RESTClient.Get('/renderstreamandfreewithownertrue');
  Assert.areEqual<Integer>(200, lRes.StatusCode);
end;

procedure TServerTest.TestRenderWrappedList;
var
  lRes: IMVCRESTResponse;
  lJSONArr: System.JSON.TJSONArray;
  I: Integer;
  lJSONObj: System.JSON.TJSONObject;
begin
  lRes := RESTClient.Get('/wrappedpeople');

  lJSONArr := TSystemJSON.StringAsJSONArray(lRes.Content);
  try
    for I := 0 to lJSONArr.Count - 1 do
    begin
      lJSONObj := lJSONArr.Items[I] as System.JSON.TJSONObject;
      Assert.isFalse(lJSONObj.GetValue<string>('firstname').IsEmpty);
    end;
  finally
    lJSONArr.Free;
  end;

end;

procedure TServerTest.TestRenderWrappedListWithCompression;
var
  lRes: IMVCRESTResponse;
  lJSONArr: TJDOJsonArray;
  I: Integer;
  lCompType: string;
  j: Integer;
const
//  CompressionTypes: array [1 .. 9] of string = ('deflate', 'gzip', 'deflate,gzip', 'gzip,deflate', 'gzip,invalid',
//    'deflate,notvalid', 'notvalid,gzip', 'invalid', '');
//  CompressionTypeResult: array [1 .. 9] of string = ('deflate', 'gzip', 'deflate', 'gzip', 'gzip', 'deflate',
//    'gzip', '', '');

  // Delphi RESTClient does not support deflate compression. See ticket https://quality.embarcadero.com/browse/RSP-30486

  CompressionTypes: array [1 .. 6] of string = ('gzip', 'gzip,deflate', 'gzip,invalid', 'notvalid,gzip', 'invalid', '');
  CompressionTypeResult: array [1 .. 6] of string = ('gzip', 'gzip', 'gzip', 'gzip', '', '');
begin
  j := 1;
  for lCompType in CompressionTypes do
  begin
    RESTClient.AcceptEncoding(lCompType);
    lRes := RESTClient.AddQueryStringParam('count', 100).Get('/wrappedpeople');
    Assert.areEqual(CompressionTypeResult[j], lRes.HeaderValue('Content-Encoding'));
    lJSONArr := TMVCJsonDataObjectsSerializer.ParseArray(lRes.Content);
    try
      for I := 0 to lJSONArr.Count - 1 do
      begin
        Assert.isFalse(lJSONArr.O[I].s['firstname'].IsEmpty);
        Assert.isFalse(lJSONArr.O[I].s['lastname'].IsEmpty);
        Assert.isFalse(lJSONArr.O[I].s['dob'].IsEmpty);
        Assert.areEqual<TJsonDataType>(jdtBool, lJSONArr.O[I].Types['married']);
      end;
    finally
      lJSONArr.Free;
    end;
    Inc(j);
  end;
end;

procedure TServerTest.TestExceptionInMVCAfterCreate;
var
  res: IMVCRESTResponse;
begin
  res := RESTClient.Get('/exception/aftercreate/nevercalled');
  Assert.areEqual<Integer>(HTTP_STATUS.InternalServerError, res.StatusCode);
end;

procedure TServerTest.TestExceptionInMVCBeforeDestroy;
var
  res: IMVCRESTResponse;
begin
  res := RESTClient.Get('/exception/beforedestroy/nevercalled');
  Assert.areEqual<Integer>(HTTP_STATUS.InternalServerError, res.StatusCode);
end;

procedure TServerTest.TestGetImagePng;
var
  c1: IMVCRESTClient;
  lRes: IMVCRESTResponse;
begin
  c1 := TMVCRESTClient.New.BaseURL(TEST_SERVER_ADDRESS, 9999);
  try
    // c1.Accept(TMVCMediaType.IMAGE_PNG);
    lRes := c1.Get('/image/png');
    Assert.areEqual(200, lRes.StatusCode);
    Assert.areEqual('image/png', lRes.ContentType);
    Assert.areEqual(249, Integer(lRes.ContentLength));
  finally
    c1 := nil;
  end;
end;

procedure TServerTest.TestInvalidateSession;
var
  c1: IMVCRESTClient;
  res: IMVCRESTResponse;
begin
  c1 := TMVCRESTClient.New.BaseURL(TEST_SERVER_ADDRESS, 9999);
  try
    c1.Accept(TMVCMediaType.APPLICATION_JSON);
    c1.AddPathParam('name', 'daniele teti').Post('/session/{name}'); // imposto un valore in sessione
    res := c1.Get('/session'); // rileggo il valore dalla sessione
    Assert.areEqual('daniele teti', res.Content);

    c1.ClearCookies;
    res := c1.Get('/session'); // rileggo il valore dalla sessione
    Assert.areEqual('', res.Content);
  finally
    c1 := nil;
  end;
end;

procedure TServerTest.TestMiddlewareHandler;
var
  r: IMVCRESTResponse;
begin
  r := RESTClient.Accept(TMVCMediaType.APPLICATION_JSON).Get('/handledbymiddleware');
  Assert.areEqual('This is a middleware response', r.Content);
  Assert.areEqual<Integer>(HTTP_STATUS.OK, r.StatusCode);
end;

procedure TServerTest.TestMiddlewareSpeedMiddleware;
var
  r: IMVCRESTResponse;
  P: TPerson;
begin
  P := TPerson.Create;
  try
    P.FirstName := StringOfChar('*', 1000);
    P.LastName := StringOfChar('*', 1000);
    P.DOB := EncodeDate(1979, 1, 1);
    P.Married := true;
    r := RESTClient.Accept(TMVCMediaType.APPLICATION_JSON)
      .Post('/objects', GetDefaultSerializer.SerializeObject(P));
  finally
    P.Free;
  end;

  Assert.AreNotEqual('', r.HeaderValue('request_gen_time'));
end;

procedure TServerTest.TestObjectDict;
var
  lRes: IMVCRESTResponse;
  lJSON: TJSONObject;
begin
  lRes := RESTClient.Get('/objectdict');
  Assert.areEqual<Integer>(HTTP_STATUS.OK, lRes.StatusCode, lRes.Content);
  lJSON := StrToJSONObject(lRes.Content);
  try
    Assert.isTrue(lJSON.Contains('ncCamelCase_Single'), lJSON.ToJSON());
    Assert.isTrue(lJSON.Contains('ncLowerCase_Single'), lJSON.ToJSON());
    Assert.isTrue(lJSON.Contains('ncPascalCase_Single'), lJSON.ToJSON());
    Assert.isTrue(lJSON.Contains('ncUpperCase_Single'), lJSON.ToJSON());

    Assert.isTrue(lJSON.Contains('ncCamelCase_List'), lJSON.ToJSON());
    Assert.isTrue(lJSON.Contains('ncLowerCase_List'), lJSON.ToJSON());
    Assert.isTrue(lJSON.Contains('ncPascalCase_List'), lJSON.ToJSON());
    Assert.isTrue(lJSON.Contains('ncUpperCase_List'), lJSON.ToJSON());

    Assert.areEqual(jdtObject, lJSON.Types['ncCamelCase_Single']);
    Assert.areEqual(jdtObject, lJSON.Types['ncLowerCase_Single']);
    Assert.areEqual(jdtObject, lJSON.Types['ncPascalCase_Single']);
    Assert.areEqual(jdtObject, lJSON.Types['ncUpperCase_Single']);

    Assert.isTrue(lJSON.O['ncCamelCase_Single'].Contains('custNo'), lJSON.O['ncCamelCase_Single'].ToJSON());
    Assert.isTrue(lJSON.O['ncLowerCase_Single'].Contains('cust_no'), lJSON.O['ncLowerCase_Single'].ToJSON());
    Assert.isTrue(lJSON.O['ncPascalCase_Single'].Contains('CustNo'), lJSON.O['ncPascalCase_Single'].ToJSON());
    Assert.isTrue(lJSON.O['ncUpperCase_Single'].Contains('CUST_NO'), lJSON.O['ncUpperCase_Single'].ToJSON());

    Assert.areEqual(jdtArray, lJSON.Types['ncCamelCase_List']);
    Assert.areEqual(jdtArray, lJSON.Types['ncLowerCase_List']);
    Assert.areEqual(jdtArray, lJSON.Types['ncPascalCase_List']);
    Assert.areEqual(jdtArray, lJSON.Types['ncUpperCase_List']);

    Assert.isTrue(lJSON.A['ncCamelCase_List'][0].ObjectValue.Contains('custNo'),
      lJSON.A['ncCamelCase_List'][0].ObjectValue.ToJSON());
    Assert.isTrue(lJSON.A['ncLowerCase_List'][0].ObjectValue.Contains('cust_no'),
      lJSON.A['ncLowerCase_List'][0].ObjectValue.ToJSON());
    Assert.isTrue(lJSON.A['ncPascalCase_List'][0].ObjectValue.Contains('CustNo'),
      lJSON.A['ncPascalCase_List'][0].ObjectValue.ToJSON());
    Assert.isTrue(lJSON.A['ncUpperCase_List'][0].ObjectValue.Contains('CUST_NO'),
      lJSON.A['ncUpperCase_List'][0].ObjectValue.ToJSON());

  finally
    lJSON.Free;
  end;
end;

// procedure TServerTest.TestPATCHWithParamsAndJSONBody;
// var
// r: IMVCRESTResponse;
// json: TJSONObject;
// begin
// json := TJSONObject.Create;
// json.AddPair('client', 'clientdata');
// r := RESTClient.doPATCH('/echo', ['1', '2', '3'], json);
// Assert.AreEqual('clientdata', r.BodyAsJsonObject.Get('client').JsonValue.Value);
// Assert.AreEqual('from server', r.BodyAsJsonObject.Get('echo').JsonValue.Value);
// end;

procedure TServerTest.TestPostAListOfObjects;
var
  lRes: IMVCRESTResponse;
  LCustomers: TObjectList<TCustomer>;
  lSer: IMVCSerializer;
begin
  LCustomers := TCustomer.GetList;
  try
    lSer := GetDefaultSerializer;
    RegisterOptionalCustomTypesSerializers(lSer); // TBitmap
    lRes := RESTClient.Post('/customers/list', lSer.SerializeCollection(LCustomers));
    Assert.areEqual<Integer>(HTTP_STATUS.OK, lRes.StatusCode);
  finally
    LCustomers.Free;
  end;
end;

procedure TServerTest.TestPOSTWithObjectJSONBody;
var
  r: IMVCRESTResponse;
  P: TPerson;
begin
  P := TPerson.Create;
  try
    P.FirstName := 'Daniele';
    P.LastName := '������';
    P.DOB := EncodeDate(1979, 1, 1);
    P.Married := true;
    try
      r := RESTClient.Accept(TMVCMediaType.APPLICATION_JSON)
        .Post('/objects', GetDefaultSerializer.SerializeObject(P)
      { Mapper.ObjectToJSONObject(P) }
        );
    except
      Assert.Fail('If this test fail, check http://qc.embarcadero.com/wc/qcmain.aspx?d=119779');
      { WARNING!!! }
      {
        If this test fail, check
        http://qc.embarcadero.com/wc/qcmain.aspx?d=119779
      }
      raise;

    end;
  finally
    P.Free;
  end;

  P := TPerson.Create;
  try
    GetDefaultSerializer.DeserializeObject(r.Content, P);
    // P := Mapper.JSONObjectToObject<TPerson>(r.BodyAsJsonObject);
    Assert.areEqual('Daniele', P.FirstName);
    Assert.areEqual('������', P.LastName);
    Assert.areEqual(true, P.Married);
    Assert.areEqual(EncodeDate(1979, 1, 1), P.DOB);
  finally
    P.Free;
  end;
end;

procedure TServerTest.TestPOSTWithParamsAndJSONBody;
var
  r: IMVCRESTResponse;
  JSON: System.JSON.TJSONObject;
begin
  JSON := System.JSON.TJSONObject.Create;
  JSON.AddPair('client', 'clientdata');
  r := RESTClient
    .AddPathParam('param1', 1)
    .AddPathParam('param2', 2)
    .AddPathParam('param3', 3)
    .Post('/echo/($param1)/{param2}/{param3}', TSystemJSON.JSONValueToString(JSON));
  JSON := TSystemJSON.StringAsJSONObject(r.Content);
  try
    Assert.areEqual('clientdata', JSON.Get('client').JsonValue.Value);
    Assert.areEqual('from server', JSON.Get('echo').JsonValue.Value);
  finally
    JSON.Free;
  end;
end;

procedure TServerTest.TestProducesConsumesWithWrongAcceptHeader;
var
  res: IMVCRESTResponse;
begin
  res := RESTClient.Accept('text/plain')
  // action is waiting for a accept: application/json
    .Post('/testconsumes',
    TSystemJSON.JSONValueToString(TJSONString.Create('Hello World')));
  Assert.areEqual<Integer>(HTTP_STATUS.NotFound, res.StatusCode);
end;

procedure TServerTest.TestProducesConsumes01;
var
  res: IMVCRESTResponse;
begin
  res := RESTClient.Accept('application/json')
    .Post('/testconsumes', TSystemJSON.JSONValueToString(TJSONString.Create('Hello World')));
  Assert.areEqual<Integer>(HTTP_STATUS.OK, res.StatusCode);
  Assert.areEqual('Hello World', res.Content);
  Assert.areEqual(TMVCMediaType.APPLICATION_JSON, res.ContentType, True);
end;

procedure TServerTest.TestProducesConsumes02;
var
  res: IMVCRESTResponse;
begin
  res := RESTClient.Accept('text/plain').Post('/testconsumes', 'Hello World', False, TMVCMediaType.TEXT_PLAIN);
  Assert.areEqual('Hello World', res.Content);
  Assert.areEqual(TMVCMediaType.TEXT_PLAIN, res.ContentType, True);

  res := RESTClient.Accept('text/plain')
    .Post('/testconsumes', '{"name": "Daniele"}');
  Assert.areEqual<Integer>(HTTP_STATUS.NotFound, res.StatusCode);
end;

procedure TServerTest.TestProducesConsumes03;
var
  res: IMVCRESTResponse;
begin
//  res := RESTClient.Accept(TMVCMediaType.TEXT_PLAIN)
//    .Post('/testconsumes/textiso8859_1');
//  Assert.areEqual<Integer>(HTTP_STATUS.OK, res.StatusCode);
//  Assert.AreNotEqual('������', res.Content, 'non iso8859-1 text is rendered ok whan should not');
//  Assert.areEqual(BuildContentType(TMVCMediaType.TEXT_PLAIN, TMVCCharSet.ISO88591), res.ContentType, True);

  res := RESTClient.Accept(TMVCMediaType.TEXT_PLAIN)
    .Post('/testconsumes/textiso8859_1', 'this is an iso8859-1 text', False, TMVCMediaType.TEXT_PLAIN);
  Assert.areEqual<Integer>(HTTP_STATUS.OK, res.StatusCode);
  Assert.areEqual('this is an iso8859-1 text', res.Content);
  Assert.areEqual(TMVCMediaType.TEXT_PLAIN, res.ContentType, True);
end;

procedure TServerTest.TestPUTWithParamsAndJSONBody;
var
  r: IMVCRESTResponse;
  JSON: System.JSON.TJSONObject;
begin
  JSON := System.JSON.TJSONObject.Create;
  JSON.AddPair('client', 'clientdata');
  r := RESTClient
    .AddPathParam('param1', 1)
    .AddPathParam('param2', 2)
    .AddPathParam('param3', 3)
    .Put('/echo/($param1)/($param2)/($param3)', TSystemJSON.JSONValueToString(JSON));

  JSON := TSystemJSON.StringAsJSONObject(r.Content);
  try
    Assert.areEqual('clientdata', JSON.Get('client').JsonValue.Value);
    Assert.areEqual('from server', JSON.Get('echo').JsonValue.Value);
  finally
    JSON.Free;
  end;
end;

procedure TServerTest.TestXHTTPMethodOverride_POST_as_PUT;
var
  r: IMVCRESTResponse;
  JSON: System.JSON.TJSONObject;
begin
  JSON := System.JSON.TJSONObject.Create;
  JSON.AddPair('client', 'clientdata');
  r := RESTClient
    .AddHeader(TMVCConstants.X_HTTP_Method_Override, 'PUT')
    .AddPathParam('param1', 1)
    .AddPathParam('param2', 2)
    .AddPathParam('param3', 3)
    .Post('/echo/($param1)/($param2)/($param3)', TSystemJSON.JSONValueToString(JSON));

  JSON := TSystemJSON.StringAsJSONObject(r.Content);
  try
    Assert.areEqual('clientdata', JSON.Get('client').JsonValue.Value);
    Assert.areEqual('from server', JSON.Get('echo').JsonValue.Value);
  finally
    JSON.Free;
  end;
end;

procedure TServerTest.TestReqWithParams;
var
  ss: TStringStream;
  lJSON: System.JSON.TJSONObject;
  r: IMVCRESTResponse;
begin
  r := RESTClient.Get('/unknownurl/bla/bla');

  ss := TStringStream.Create;
  try
    r.SaveContentToStream(ss);
    Assert.areEqual(ss.DataString, r.Content,
      'In case of protocol error, the body doesn''t contain the same of BodyAsString');
  finally
    ss.Free;
  end;

  Assert.areEqual<Integer>(HTTP_STATUS.NotFound, r.StatusCode, '/unknownurl/bla/bla');

  r := RESTClient.Get('/req/with/params/');
  Assert.areEqual<Integer>(HTTP_STATUS.NotFound, r.StatusCode, '/req/with/params/');

  r := RESTClient.Get('/req/with/params');
  Assert.areEqual<Integer>(HTTP_STATUS.NotFound, r.StatusCode, '/req/with/params');

  r := RESTClient
    .AddPathParam('param1', 1)
    .AddPathParam('param2', 2)
    .AddPathParam('param3', 3)
    .Get('/req/with/params/($param1)/($param2)/($param3)');
  Assert.areEqual<Integer>(HTTP_STATUS.OK, r.StatusCode);

  lJSON := TSystemJSON.StringAsJSONObject(r.Content);
  try
    Assert.areEqual('1', lJSON.Get('par1').JsonValue.Value);
    Assert.areEqual('2', lJSON.Get('par2').JsonValue.Value);
    Assert.areEqual('3', lJSON.Get('par3').JsonValue.Value);
    Assert.areEqual('GET', lJSON.Get('method').JsonValue.Value);

    r := RESTClient
      .AddPathParam('param1', 1)
      .AddPathParam('param2', 2)
      .AddPathParam('param3', 3)
      .Post('/req/with/params/($param1)/($param2)/($param3)');
    Assert.areEqual<Integer>(HTTP_STATUS.NotFound, r.StatusCode);

    r := RESTClient
      .AddPathParam('param1', 1)
      .AddPathParam('param2', 2)
      .AddPathParam('param3', 3)
      .Put('/req/with/params/($param1)/($param2)/($param3)');
    Assert.areEqual<Integer>(HTTP_STATUS.NotFound, r.StatusCode);

    r := RESTClient
      .AddPathParam('param1', 1)
      .AddPathParam('param2', 2)
      .AddPathParam('param3', 3)
      .Delete('/req/with/params/($param1)/($param2)/($param3)' );
    Assert.areEqual<Integer>(HTTP_STATUS.OK, r.StatusCode);
  finally
    lJSON.Free;
  end;
end;

procedure TServerTest.TestReqWithURLMappedParams(const par1, par2, par3: string);
var
  r: IMVCRESTResponse;
begin
  r := RESTClient
    .AddPathParam('par1', par1)
    .AddPathParam('par2', par2)
    .AddPathParam('par3', par3)
    .Get('/req/with/params/($par1)/($par2)/($par3)');
  Assert.areEqual<Integer>(HTTP_STATUS.OK, r.StatusCode,
    Format('URL mapped fails for these characters: "%s","%s","%s"', [par1, par2, par3]));
end;

procedure TServerTest.TestResponseAccepted;
var
  r: IMVCRESTResponse;
  lJSON: TJDOJsonObject;
begin
  r := RESTClient.Post('/responses/accepted');
  Assert.areEqual<Integer>(HTTP_STATUS.Accepted, r.StatusCode);
  Assert.isTrue(r.StatusText.Contains('thisisthereason'));
  lJSON := StrToJSONObject(r.Content);
  try
    Assert.areEqual(2, lJSON.O['task'].Count);
    Assert.areEqual('http://pippo.it/1234', lJSON.O['task'].s['href']);
    Assert.areEqual('1234', lJSON.O['task'].s['id']);
  finally
    lJSON.Free;
  end;
end;

procedure TServerTest.TestResponseCreated;
var
  r: IMVCRESTResponse;
begin
  r := RESTClient.Post('/responses/created');
  Assert.areEqual<Integer>(HTTP_STATUS.Created, r.StatusCode);
  Assert.isTrue(r.StatusText.Contains('thisisthereason'));
  Assert.IsEmpty(r.Content);
end;

procedure TServerTest.TestResponseNoContent;
var
  r: IMVCRESTResponse;
begin
  r := RESTClient.Get('/responses/nocontent');
  Assert.areEqual<Integer>(HTTP_STATUS.NoContent, r.StatusCode);
  Assert.isTrue(r.StatusText.Contains('No Content'));
  Assert.IsEmpty(r.Content);
end;

// procedure TServerTest.TestSerializationType;
// var
// LResp: IMVCRESTResponse;
// LPersonProps, LPersonFlds: TPerson;
// LObj: TObject;
// begin
// LResp := RESTClient.Get('/people', ['1']);
// LPersonProps := Mapper.JSONObjectToObject<TPerson>(LResp.BodyAsJsonObject);
// try
// LResp := RESTClient.Get('/people', ['1', 'asfields']);
// LObj := Mapper.JSONObjectFieldsToObject(LResp.BodyAsJsonObject);
// try
// Assert.AreEqual('BusinessObjectsU.TPerson', LObj.QualifiedClassName);
// LPersonFlds := TPerson(LObj);
// Assert.isTrue(LPersonFlds.Equals(LPersonProps),
// 'Object tranferred using field serialization is different from the object serialized in the default way');
// finally
// LObj.Free;
// end;
// finally
// LPersonProps.Free;
// end;
// end;

procedure TServerTest.TestDeserializeNullablesWithNulls;
var
  lRes: IMVCRESTResponse;
  lSer: TMVCJsonDataObjectsSerializer;
  lNullableTest: TNullablesTest;
begin
  /// nullables/getsinglewithnulls

  lRes := RESTClient.Get('/nullables/getsinglewithnulls');
  lSer := TMVCJsonDataObjectsSerializer.Create;
  try
    lNullableTest := TNullablesTest.Create();
    try
      lSer.DeserializeObject(lRes.Content, lNullableTest);
      Assert.isFalse(lNullableTest.f_int2.HasValue);
      Assert.isFalse(lNullableTest.f_int4.HasValue);
      Assert.isFalse(lNullableTest.f_int8.HasValue);
      Assert.isFalse(lNullableTest.f_date.HasValue);
      Assert.isFalse(lNullableTest.f_time.HasValue);
      Assert.isFalse(lNullableTest.f_datetime.HasValue);
      Assert.isFalse(lNullableTest.f_bool.HasValue);
      Assert.isFalse(lNullableTest.f_float4.HasValue);
      Assert.isFalse(lNullableTest.f_float8.HasValue);
      Assert.isFalse(lNullableTest.f_string.HasValue);
      Assert.isFalse(lNullableTest.f_currency.HasValue);
      { TODO -oDanieleT -cGeneral : Compare streams too }
      // Assert.AreEqual('0123456789', lNullableTest.f_blob.Value, 0);
    finally
      lNullableTest.Free;
    end;
  finally
    lSer.Free;
  end;

end;

procedure TServerTest.TestDeserializeNullablesWithValue;
var
  lRes: IMVCRESTResponse;
  lSer: TMVCJsonDataObjectsSerializer;
  lNullableTest: TNullablesTest;
begin
  /// nullables/getsinglewithnulls

  lRes := RESTClient.Get('/nullables/getsingle');
  lSer := TMVCJsonDataObjectsSerializer.Create;
  try
    lNullableTest := TNullablesTest.Create();
    try
      lSer.DeserializeObject(lRes.Content, lNullableTest);
      Assert.areEqual<Int16>(2, lNullableTest.f_int2.Value);
      Assert.areEqual(4, lNullableTest.f_int4.Value);
      Assert.areEqual<Int32>(8, lNullableTest.f_int8.Value);
      Assert.areEqual('2011-11-17', DateToISODate(lNullableTest.f_date.Value));
      Assert.areEqual('12:24:36', TimeToISOTime(lNullableTest.f_time.Value));
      Assert.areEqual('2011-11-17T12:24:36.048Z', DateTimeToISOTimeStamp(lNullableTest.f_datetime.Value));
      Assert.areEqual<boolean>(true, lNullableTest.f_bool.Value);
      Assert.areEqual(10 / 4, lNullableTest.f_float4.Value, 0.0000009);
      Assert.areEqual(10 / 8, lNullableTest.f_float8.Value, 0.0000000000009);
      Assert.areEqual('0123456789', lNullableTest.f_string.Value);
      Assert.areEqual(98765.4321, lNullableTest.f_currency.Value, 0);
      { TODO -oDanieleT -cGeneral : Compare streams too }
      // Assert.AreEqual('0123456789', lNullableTest.f_blob.Value, 0);
    finally
      lNullableTest.Free;
    end;
  finally
    lSer.Free;
  end;
end;

procedure TServerTest.TestDirectoryRedirect;
var
  lRes: IMVCRESTResponse;
begin
  lRes := RESTClient.Accept(TMVCMediaType.TEXT_HTML).Get('/static/');
  Assert.areEqual(200, lRes.StatusCode, '/static/');

  lRes := RESTClient
    .Accept(TMVCMediaType.TEXT_HTML)
    .HandleRedirects(False)
    .Get('/static');
  Assert.areEqual(301, lRes.StatusCode, '/static');
  Assert.areEqual('/static/', lRes.HeaderValue('Location'), 'Wrong redirect');
end;

procedure TServerTest.TestDirectoryTraversal1;
var
  lRes: IMVCRESTResponse;
  I: Integer;
  lUrl: string;
begin
  lRes := RESTClient.Accept(TMVCMediaType.TEXT_HTML).Get('/index.html');
  Assert.areEqual(200, lRes.StatusCode);

  lRes := RESTClient
    .Accept(TMVCMediaType.TEXT_HTML).Get('/..\donotdeleteme.txt');
  Assert.areEqual(404, lRes.StatusCode);

  lUrl := 'Windows\win.ini';
  for I := 1 to 20 do
  begin
    lUrl := '..\' + lUrl;
    lRes := RESTClient.Accept(TMVCMediaType.TEXT_HTML).Get('/' + lUrl);
    Assert.areEqual(404, lRes.StatusCode, 'Fail with: ' + '/' + lUrl);
  end;
end;

procedure TServerTest.TestDirectoryTraversal2;
var
  lRes: IMVCRESTResponse;
  I: Integer;
  lUrl: string;
begin
  lRes := RESTClient.Accept(TMVCMediaType.TEXT_HTML).Get('/static/index.html');
  Assert.areEqual(200, lRes.StatusCode, '/static/index.html');

  lRes := RESTClient.ClearCookies.Accept(TMVCMediaType.TEXT_HTML).Get('/static.html');
  Assert.areEqual(200, lRes.StatusCode, '/static.html');

  lRes := RESTClient
    .Accept(TMVCMediaType.TEXT_HTML).Get('/static/..\..\donotdeleteme.txt');
  Assert.areEqual(404, lRes.StatusCode);

  lRes := RESTClient
    .Accept(TMVCMediaType.TEXT_HTML).Get('/static/../../donotdeleteme.txt');
  Assert.areEqual(404, lRes.StatusCode);

  lUrl := 'Windows\win.ini';
  for I := 1 to 30 do
  begin
    lUrl := '..\' + lUrl;
    lRes := RESTClient.Accept(TMVCMediaType.TEXT_HTML).Get('/' + lUrl);
    Assert.areEqual(404, lRes.StatusCode, 'Fail with: ' + '/' + lUrl);
  end;
end;

procedure TServerTest.TestSerializeAndDeserializeNullables;
var
  lRes: IMVCRESTResponse;
  lSer: TMVCJsonDataObjectsSerializer;
  lNullableTest: TNullablesTest;
begin
  lRes := RESTClient.Get('/nullables/getsingle');
  lSer := TMVCJsonDataObjectsSerializer.Create;
  try
    lNullableTest := TNullablesTest.Create();
    try
      lSer.DeserializeObject(lRes.Content, lNullableTest);
      lRes := RESTClient.Post('/nullables/pingpong', lSer.SerializeObject(lNullableTest));
    finally
      lNullableTest.Free;
    end;

    lNullableTest := TNullablesTest.Create();
    try
      lSer.DeserializeObject(lRes.Content, lNullableTest);
      Assert.areEqual<Int16>(2, lNullableTest.f_int2.Value);
      Assert.areEqual(4, lNullableTest.f_int4.Value);
      Assert.areEqual<Int32>(8, lNullableTest.f_int8.Value);
      Assert.areEqual('2011-11-17', DateToISODate(lNullableTest.f_date.Value));
      Assert.areEqual('12:24:36', TimeToISOTime(lNullableTest.f_time.Value));
      Assert.areEqual('2011-11-17T12:24:36.048Z', DateTimeToISOTimeStamp(lNullableTest.f_datetime.Value));
      Assert.areEqual<boolean>(true, lNullableTest.f_bool.Value);
      Assert.areEqual(10 / 4, lNullableTest.f_float4.Value, 0.0000009);
      Assert.areEqual(10 / 8, lNullableTest.f_float8.Value, 0.0000000000009);
      Assert.areEqual('0123456789', lNullableTest.f_string.Value);
      Assert.areEqual(98765.4321, lNullableTest.f_currency.Value, 0);
      { TODO -oDanieleT -cGeneral : Compare streams too }
      // Assert.AreEqual('0123456789', lNullableTest.f_blob.Value, 0);
    finally
      lNullableTest.Free;
    end;
  finally
    lSer.Free;
  end;
end;

procedure TServerTest.TestSerializeAndDeserializeNullables_ISSUE_362;
const
  JSON1: string = '{"f_int2":2,"f_int4":4,"f_int8":8,"f_string":"0123456789","f_bool":true, ' +
    '"f_date":"2011-11-17","f_time":"12:24:36","f_datetime":"2011-11-17T12:24:36.048Z",' +
    '"f_float4":2.5,"f_float8":1.25,"f_currency":98765.4321,"f_blob":"0123456789"}';
  JSON2: string = '{"f_int2":2,"f_int4":4,"f_int8":8,"f_string":"0123456789","f_bool":true, ' +
    '"f_date":"2011-11-17","f_time":"12:24:36","f_datetime":"2011-11-17T12:24:36.048Z",' +
    '"f_float4":2,"f_float8":3,"f_currency":4,"f_blob":"0123456789"}';
var
  lSer: TMVCJsonDataObjectsSerializer;
  lNullableTest: TNullablesTest;
begin
  lSer := TMVCJsonDataObjectsSerializer.Create;
  try
    lNullableTest := TNullablesTest.Create();
    try
      lSer.DeserializeObject(JSON1, lNullableTest);
    finally
      lNullableTest.Free;
    end;
    lNullableTest := TNullablesTest.Create();
    try
      { in this case nullable floats type actually StatusText integers... }
      lSer.DeserializeObject(JSON2, lNullableTest);
    finally
      lNullableTest.Free;
    end;
    Assert.Pass();
  finally
    lSer.Free;
  end;
end;

procedure TServerTest.TestSerializeAndDeserializeNullables_Passing_Integers_InsteadOf_Floats;
const
  JSON1: string = '{"f_int2":2,"f_int4":4,"f_int8":8,"f_string":"0123456789","f_bool":true, ' +
    '"f_date":"2011-11-17","f_time":"12:24:36","f_datetime":"2011-11-17T12:24:36.048Z",' +
    '"f_float4_not_null":1234,"f_float8_not_null":2345, ' +
    '"f_float4":2.5,"f_float8":1.25,"f_currency":98765.4321,"f_blob":"0123456789"}';
var
  lSer: TMVCJsonDataObjectsSerializer;
  lNullableTest: TNullablesTest;
begin
  lSer := TMVCJsonDataObjectsSerializer.Create;
  try
    lNullableTest := TNullablesTest.Create();
    try
      { in this case not nullable floats type actually StatusText integers... }
      lSer.DeserializeObject(JSON1, lNullableTest);
      Assert.areEqual(1234, lNullableTest.f_float4_not_null, 0.0001);
      Assert.areEqual(2345, lNullableTest.f_float8_not_null, 0.0001);
    finally
      lNullableTest.Free;
    end;
  finally
    lSer.Free;
  end;
end;

procedure TServerTest.TestSession;
var
  c1: IMVCRESTClient;
  res: IMVCRESTResponse;
  lCookie: TCookie;
begin
  c1 := TMVCRESTClient.New.BaseURL(TEST_SERVER_ADDRESS, 9999);
  try
    c1.Accept(TMVCMediaType.APPLICATION_JSON);
    res := c1
      .AddPathParam('name', 'daniele teti')
      .Post('/session/($name)'); // imposto un valore in sessione
    if res.Cookies.Count > 0 then
      lCookie := res.Cookies[0]
    else
      lCookie := Default(TCookie);


    Assert.isFalse(lCookie.Expires > 0, 'Session cookie contains "expires" attribute');
    res := c1.Get('/session'); // rileggo il valore dalla sessione
    Assert.areEqual('daniele teti', res.Content);
    c1.Accept(TMVCMediaType.TEXT_PLAIN);
    res := c1.Get('/session');
    // rileggo il valore dalla sessione
    Assert.areEqual('daniele teti', res.Content);

    // aggiungo altri cookies
    res := c1.Get('/lotofcookies'); // rileggo il valore dalla sessione
    Assert.areEqual<Integer>(HTTP_STATUS.OK, res.StatusCode);
    c1.Accept(TMVCMediaType.TEXT_PLAIN);
    res := c1.Get('/session'); // rileggo il valore dalla sessione
    Assert.areEqual('daniele teti', res.Content);
  finally
    c1 := nil;
  end;
end;

procedure TServerTest.TestSessionWithLogin;
begin
  DoLoginWith('daniele');
  DoLogout;
end;

procedure TServerTest.TestSPASupport;
var
  lRes: IMVCRESTResponse;
  I: Integer;
  lUrl: string;
begin
  lRes := RESTClient.Accept(TMVCMediaType.TEXT_HTML).Get('/spa/index.html');
  Assert.areEqual(200, lRes.StatusCode);
  Assert.Contains(lRes.Content, 'This is a TEXT file');

  lRes := RESTClient.Accept(TMVCMediaType.TEXT_HTML).Get('/spa/');
  Assert.areEqual(200, lRes.StatusCode, '/static/');
  Assert.Contains(lRes.Content, 'This is a TEXT file');

  lRes := RESTClient.Accept(TMVCMediaType.TEXT_HTML).Get('/spa/pippo/pluto/paperino');
  Assert.areEqual(200, lRes.StatusCode, '/spa/pippo/pluto/paperino');
  Assert.Contains(lRes.Content, 'This is a TEXT file');

  lUrl := 'Windows\win.ini';
  for I := 1 to 30 do
  begin
    lUrl := '..\' + lUrl;
    lRes := RESTClient.Accept(TMVCMediaType.TEXT_HTML).Get('/spa/' + lUrl);
    Assert.areEqual(200, lRes.StatusCode);
    Assert.Contains(lRes.Content, 'This is a TEXT file');
  end;
end;

procedure TServerTest.TestStringDictionary;
var
  lRes: IMVCRESTResponse;
  lSer: TMVCJsonDataObjectsSerializer;
  lDict: TMVCStringDictionary;
begin
  lRes := RESTClient.Post('/stringdictionary', '{"prop1":"value1","prop2":"value2"}');
  Assert.areEqual(200, lRes.StatusCode);
  lSer := TMVCJsonDataObjectsSerializer.Create;
  try
    lDict := TMVCStringDictionary.Create;
    try
      lSer.DeserializeObject(lRes.Content, lDict);
      Assert.areEqual(3, lDict.Count);
      Assert.areEqual('value1', lDict['prop1']);
      Assert.areEqual('value2', lDict['prop2']);
      Assert.areEqual('changed', lDict['fromserver']);
    finally
      lDict.Free;
    end;
  finally
    lSer.Free;
  end;
end;

procedure TServerTest.TestTypedAll;
var
  res: IMVCRESTResponse;
  lJObj: System.JSON.TJSONObject;
begin
  // ----------------------'/typed/all/($ParString)/($ParInteger)/($ParInt64)/($ParSingle)/($ParDouble)/($ParExtended)')');
  res := RESTClient.Get('/typed/all/mystring/1234/12345678/12.3/1234.5678/1234.5678');
  Assert.isTrue(res.StatusCode = HTTP_STATUS.OK, 'Cannot route');
  lJObj := TSystemJSON.StringAsJSONObject(res.Content);
  try
    Assert.areEqual('mystring', lJObj.GetValue('ParString').Value, 'ParString');
    Assert.areEqual(1234, TJSONNumber(lJObj.GetValue('ParInteger')).AsInt, 'ParInteger');
    Assert.areEqual(int64(12345678), TJSONNumber(lJObj.GetValue('ParInt64')).AsInt64, 'ParInt64');
    Assert.areEqual(12.3, RoundTo(TJSONNumber(lJObj.GetValue('ParSingle')).AsDouble, -1), 'ParSingle');
    Assert.areEqual(1234.5678, RoundTo(TJSONNumber(lJObj.GetValue('ParDouble')).AsDouble, -4), 'ParDouble');
    Assert.areEqual(1234.5678, RoundTo(TJSONNumber(lJObj.GetValue('ParExtended')).AsDouble, -4), 'ParExtended');
  finally
    lJObj.Free;
  end;
end;

procedure TServerTest.TestTypedBooleans;
var
  res: IMVCRESTResponse;
begin
  res := RESTClient.Get('/typed/booleans/true/false/1/0');
  Assert.isTrue(res.StatusCode = HTTP_STATUS.OK, 'Cannot route');
  Assert.areEqual('true.false.true.false', res.Content.ToLower);
end;

procedure TServerTest.TestTypedDouble1;
var
  res: IMVCRESTResponse;
begin
  res := RESTClient.Get('/typed/double1/1234.5678');
  Assert.isTrue(res.StatusCode = HTTP_STATUS.OK, 'Cannot route');
  Assert.areEqual('1234.5678 modified from server', res.Content);

end;

procedure TServerTest.TestTypedExtended1;
var
  res: IMVCRESTResponse;
begin
  res := RESTClient.Get('/typed/extended1/1234.5678');
  Assert.isTrue(res.StatusCode = HTTP_STATUS.OK, 'Cannot route');
  Assert.areEqual('1234.5678 modified from server', res.Content);

end;

procedure TServerTest.TestTypedInt641;
var
  res: IMVCRESTResponse;
begin
  res := RESTClient.Get('/typed/int641/12345678');
  Assert.isTrue(res.StatusCode = HTTP_STATUS.OK, 'Cannot route');
  Assert.areEqual('12345678 modified from server', res.Content);
end;

procedure TServerTest.TestTypedInteger1;
var
  res: IMVCRESTResponse;
begin
  res := RESTClient.Get('/typed/integer1/1234');
  Assert.isTrue(res.StatusCode = HTTP_STATUS.OK, 'Cannot route');
  Assert.areEqual('1234 modified from server', res.Content);
end;

procedure TServerTest.TestTypedIntegerWrongParam1;
var
  res: IMVCRESTResponse;
begin
  res := RESTClient.Get('/typed/integer1/boom');
  Assert.isTrue(res.StatusCode = HTTP_STATUS.BadRequest, 'Cannot route');
  Assert.Contains(res.Content, 'EConvertError');
  Assert.Contains(res.Content, '''boom'' is not a valid');
end;

procedure TServerTest.TestTypedSingle1;
var
  res: IMVCRESTResponse;
begin
  res := RESTClient.Get('/typed/single1/1234.5');
  Assert.isTrue(res.StatusCode = HTTP_STATUS.OK, 'Cannot route');
  Assert.areEqual('1234.5 modified from server', res.Content);

end;

procedure TServerTest.TestTypedString1;
var
  res: IMVCRESTResponse;
  lValues: array [0 .. 7] of string;
  s: string;
begin
  lValues[0] := 'daniele';
  lValues[1] := 'dan''iele';
  lValues[2] := '"daniele"';
  lValues[3] := '"daniele teti"';
  lValues[4] := '"daniele" "teti"';
  lValues[5] := '"daniele" "teti"!';
  lValues[6] := ' _\"daniele" "teti"!_ ';

  for s in lValues do
  begin
    res := RESTClient.AddPathParam('param1', s).Get('/typed/string1/($param1)');
    Assert.areEqual(HTTP_STATUS.OK, res.StatusCode, 'Cannot route when param is ' + s);
    Assert.areEqual('*' + s + '*', res.Content);
  end;

  // res := RESTClient.Get('/typed/string1/daniele');
  // Assert.isTrue(res.StatusCode = HTTP_STATUS.OK, 'Cannot route');
  // Assert.areEqual('daniele modified from server', res.Content);
  //
  // res := RESTClient.Get('/typed/string1/dan''iele');
  // Assert.isTrue(res.StatusCode = HTTP_STATUS.OK, 'Cannot route');
  // Assert.areEqual('dan''iele modified from server', res.Content);
  //
  // res := RESTClient.Get('/typed/string1/"the value"');
  // Assert.isTrue(res.StatusCode = HTTP_STATUS.OK, 'Cannot route');
  // Assert.areEqual('"the value" modified from server', res.Content);
  //
  // res := RESTClient.Get('/typed/string1/"the:value"');
  // Assert.isTrue(res.StatusCode = HTTP_STATUS.OK, 'Cannot route');
  // Assert.areEqual('"the value" modified from server', res.Content);
  //
  // res := RESTClient.Get('/typed/string1/"the:value!"');
  // Assert.isTrue(res.StatusCode = HTTP_STATUS.OK, 'Cannot route');
  // Assert.areEqual('"the value" modified from server', res.Content);
  //
  // res := RESTClient.Get('/typed/string1/"the:value!?"');
  // Assert.isTrue(res.StatusCode = HTTP_STATUS.OK, 'Cannot route');
  // Assert.areEqual('"the value" modified from server', res.Content);

end;

procedure TServerTest.TestTypedTGuid1;
var
  res: IMVCRESTResponse;
begin
  res := RESTClient.Get('/typed/tguid1/{161BEA56-480B-40A8-AF0E-7FDF6B08E121}');
  Assert.isTrue(res.StatusCode = HTTP_STATUS.OK, 'Cannot route');
  Assert.areEqual('{161BEA56-480B-40A8-AF0E-7FDF6B08E121} modified from server', res.Content);

  res := RESTClient.Get('/typed/tguid1/161BEA56-480B-40A8-AF0E-7FDF6B08E121');
  Assert.isTrue(res.StatusCode = HTTP_STATUS.OK, 'Cannot route');
  Assert.areEqual('{161BEA56-480B-40A8-AF0E-7FDF6B08E121} modified from server', res.Content);

  res := RESTClient.Get('/typed/tguid1/161BEA56480B40A8AF0E7FDF6B08E121');
  Assert.isTrue(res.StatusCode = HTTP_STATUS.OK, 'Cannot route');
  Assert.areEqual('{161BEA56-480B-40A8-AF0E-7FDF6B08E121} modified from server', res.Content);
end;

procedure TServerTest.TestViewDataViewDataSet;
var
  lRes: IMVCRESTResponse;
begin
  lRes := RESTClient.Accept(TMVCMediaType.TEXT_PLAIN).Get('/website/list');
  Assert.areEqual(HTTP_STATUS.OK, lRes.StatusCode, lRes.Content);
  var
  lLines := lRes.Content.Split([sLineBreak]);
  var
    lCount: Integer := 1001;
  for var lLine in lLines do
  begin
    var
    lLinePieces := lLine.Split(['|']);
    if Length(lLinePieces) = 1 then
    begin
      lCount := 1001;
      Continue;
    end;
    Assert.areEqual(9, Length(lLinePieces));
    Assert.areEqual(lCount, lLinePieces[0].ToInteger);
    Inc(lCount);
  end;
end;

procedure TServerTest.TestWrongJSONBody;
var
  lRes: IMVCRESTResponse;
begin
  lRes := RESTClient.Post('/stringdictionary', '{"prop1","value1"}');
  Assert.areEqual(HTTP_STATUS.BadRequest, lRes.StatusCode);
end;

procedure TServerTest.TestTypedDateTimeTypes;
var
  res: IMVCRESTResponse;
begin
  // TDate, wrong and correct
  res := RESTClient.Get('/typed/tdate1/20161012');
  Assert.areEqual<Integer>(HTTP_STATUS.BadRequest, res.StatusCode, 'wrong TDate');

  res := RESTClient.Get('/typed/tdate1/2016-10-12');
  Assert.areEqual<Integer>(HTTP_STATUS.OK, res.StatusCode);
  Assert.areEqual('2016-10-12 modified from server', res.Content);

  // TDateTime, wrong and correct
  res := RESTClient.Get('/typed/tdatetime1/20161');
  Assert.areEqual<Integer>(HTTP_STATUS.BadRequest, res.StatusCode, 'wrong TDateTime (1)');

  // Wrong
  res := RESTClient.Get('/typed/tdatetime1/20161012121212');
  Assert.areEqual<Integer>(HTTP_STATUS.BadRequest, res.StatusCode, 'wrong TDateTime (2)');

  // Correct without 'T'
  res := RESTClient.Get('/typed/tdatetime1/2016-10-12 12:12:12');
  Assert.areEqual<Integer>(HTTP_STATUS.OK, res.StatusCode, 'wrong TDateTime (3)');
  Assert.areEqual('2016-10-12T12:12:12.000Z modified from server', res.Content);

  // Correct in extended form
  res := RESTClient.Get('/typed/tdatetime1/2016-10-12T12:12:12');
  Assert.areEqual<Integer>(HTTP_STATUS.OK, res.StatusCode);
  Assert.areEqual('2016-10-12T12:12:12.000Z modified from server', res.Content);

  // Correct in extended form with timezone
  res := RESTClient.Get('/typed/tdatetime1/2016-10-12T12:12:12.000Z');
  Assert.areEqual<Integer>(HTTP_STATUS.OK, res.StatusCode);
  Assert.areEqual('2016-10-12T12:12:12.000Z modified from server', res.Content);

  // TTime, wrong and correct
  res := RESTClient.Get('/typed/ttime1/121212');
  Assert.areEqual<Integer>(HTTP_STATUS.BadRequest, res.StatusCode, 'wrong TTime');

  res := RESTClient.Get('/typed/ttime1/12:12:12');
  Assert.areEqual<Integer>(HTTP_STATUS.OK, res.StatusCode);
  Assert.areEqual('12:12:12 modified from server', res.Content);

end;

procedure TBaseServerTest.DoLoginWith(UserName: string);
var
  res: IMVCRESTResponse;
begin
  res := RESTClient.AddPathParam('user', UserName).Get('/login/{user}');
  Assert.isTrue(res.StatusCode = HTTP_STATUS.OK, 'Login Failed');
end;

{ TJSONRPCServerTest }

procedure TJSONRPCServerTest.Setup;
begin
  FExecutor := TMVCJSONRPCExecutor.Create('http://' + TEST_SERVER_ADDRESS + ':9999/jsonrpc', false);
  FExecutor2 := TMVCJSONRPCExecutor.Create('http://' + TEST_SERVER_ADDRESS + ':9999/jsonrpcclass', false);
  FExecutor3 := TMVCJSONRPCExecutor.Create('http://' + TEST_SERVER_ADDRESS + ':9999/jsonrpcclass1', false);
end;

procedure TJSONRPCServerTest.TestHooks;
begin
  var lRequest1: IJSONRPCRequest := TJSONRPCRequest.Create(1234, 'request1');
  var lResp := FExecutor3.ExecuteRequest(lRequest1);
  Assert.areEqual('OnBeforeRoutingHook|OnBeforeCallHook|OnAfterCallHook',
    FExecutor3.HTTPResponse.HeaderValue['x-history']);
end;

procedure TJSONRPCServerTest.TestHooksNotif;
begin
  var lNotif: IJSONRPCNotification := TJSONRPCNotification.Create('Notif1');
  var lResp := FExecutor3.ExecuteNotification(lNotif);
  Assert.areEqual('OnBeforeRoutingHook|OnBeforeCallHook|OnAfterCallHook', FExecutor3.HTTPResponse.HeaderValue['x-history']);
  Assert.IsFalse(lResp.IsError);
  Assert.WillRaise(
    procedure
    begin
      lResp.AsJSONString;
    end, EMVCJSONRPCException);
end;

procedure TJSONRPCServerTest.TestHooksNotifWhenOnAfterCallHookRaisesError;
begin
  var lNotif: IJSONRPCNotification := TJSONRPCNotification.Create('error_OnAfterCallHook');
  var lResp: IJSONRPCResponse := FExecutor3.ExecuteNotification(lNotif);
  Assert.areEqual('', FExecutor3.HTTPResponse.HeaderValue['x-history']);
  Assert.isTrue(lResp.IsError);
  Assert.WillNotRaise(
    procedure
    begin
      lResp.AsJSONString;
    end, EMVCJSONRPCException);
end;

procedure TJSONRPCServerTest.TestHooksNotifWhenOnBeforeCallHookRaisesError;
begin
  var lNotif: IJSONRPCNotification := TJSONRPCNotification.Create('error_OnBeforeCallHook');
  var lResp: IJSONRPCResponse := FExecutor3.ExecuteNotification(lNotif);
  Assert.areEqual('', FExecutor3.HTTPResponse.HeaderValue['x-history']);
  Assert.isTrue(lResp.IsError);
  Assert.WillNotRaise(
    procedure
    begin
      lResp.AsJSONString;
    end, EMVCJSONRPCException);
end;

procedure TJSONRPCServerTest.TestHooksNotifWhenOnBeforeRoutingHookRaisesError;
begin
  var lNotif: IJSONRPCNotification := TJSONRPCNotification.Create('error_OnBeforeRoutingHook');
  var lResp: IJSONRPCResponse := FExecutor3.ExecuteNotification(lNotif);
  Assert.areEqual('', FExecutor3.HTTPResponse.HeaderValue['x-history']);
  Assert.isTrue(lResp.IsError);
  Assert.WillNotRaise(
    procedure
    begin
      lResp.AsJSONString;
    end, EMVCJSONRPCException);
end;

procedure TJSONRPCServerTest.TestHooksWhenMethodRaisesError;
begin
  var lRequest1: IJSONRPCRequest := TJSONRPCRequest.Create(1234, 'RequestWithError');
  var lResp := FExecutor3.ExecuteRequest(lRequest1);
  Assert.areEqual('OnBeforeRoutingHook|OnBeforeCallHook|OnAfterCallHook|error',
    FExecutor3.HTTPResponse.HeaderValue['x-history']);
  Assert.isTrue(lResp.IsError, 'Method raised error but response is not an error');
end;

procedure TJSONRPCServerTest.TestHooksWhenOnAfterCallHookRaisesError;
begin
  var lRequest1: IJSONRPCRequest := TJSONRPCRequest.Create(1234, 'error_OnAfterCallHook');
  var lResp := FExecutor3.ExecuteRequest(lRequest1);
  Assert.isTrue(lResp.IsError, lResp.ToString(true));
  Assert.areEqual(lResp.Error.ErrMessage, 'error_OnAfterCallHook');
end;

procedure TJSONRPCServerTest.TestHooksWhenOnBeforeCallHookRaisesError;
begin
  var lRequest1: IJSONRPCRequest := TJSONRPCRequest.Create(1234, 'error_OnBeforeCallHook');
  var lResp := FExecutor3.ExecuteRequest(lRequest1);
  Assert.isTrue(lResp.IsError, lResp.ToString(true));
  Assert.areEqual(lResp.Error.ErrMessage, 'error_OnBeforeCallHook');

end;

procedure TJSONRPCServerTest.TestHooksWhenOnBeforeRoutingHookRaisesError;
begin
  var lRequest1: IJSONRPCRequest := TJSONRPCRequest.Create(1234, 'error_OnBeforeRoutingHook');
  var lResp := FExecutor3.ExecuteRequest(lRequest1);
  Assert.isTrue(lResp.IsError, lResp.ToString(true));
  Assert.areEqual(lResp.Error.ErrMessage, 'error_OnBeforeRoutingHook');
end;

procedure TJSONRPCServerTest.TestNotificationWhichRaisesError;
var
  lReq: IJSONRPCNotification;
begin
  lReq := TJSONRPCNotification.Create;
  lReq.Method := 'NotifWithError';
  var lResp := FExecutor3.ExecuteNotification(lReq);
  Assert.IsTrue(lResp.IsError);
  Assert.Contains(lResp.Error.ErrMessage, 'BOOM NOTIF');
end;

procedure TJSONRPCServerTest.TestNotificationWithoutParams;
var
  lReq: IJSONRPCNotification;
begin
  lReq := TJSONRPCNotification.Create;
  lReq.Method := 'mynotify';
  FExecutor.ExecuteNotification(lReq);
  FExecutor2.ExecuteNotification(lReq);
  Assert.Pass();
end;

procedure TJSONRPCServerTest.TestRequestToNotFoundMethod;
var
  lReq: IJSONRPCRequest;
  lResp: IJSONRPCResponse;
begin
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'nonexist';
  lReq.RequestID := 1234;
  lResp := FExecutor.ExecuteRequest(lReq);
  Assert.IsNotNull(lResp.Error);
  Assert.areEqual(-32601, lResp.Error.Code);
  Assert.isTrue(lResp.Error.ErrMessage.StartsWith('Method [nonexist] not found.'));
end;

procedure TJSONRPCServerTest.TestRequestWithParams_DT_T_ret_DT;
var
  lReq: IJSONRPCRequest;
  lRPCResp: IJSONRPCResponse;
  lRes: TDateTime;
  lYear: Word;
  lMonth: Word;
  lDay: Word;
  lHour: Word;
  lMinute: Word;
  lSecond: Word;
  lMillisecond: Word;
begin
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'addtimetodatetime';
  lReq.Params.Add(EncodeDate(2000, 10, 1) + EncodeTime(12, 0, 0, 0), TJSONRPCParamDataType.pdtDateTime);
  lReq.Params.Add(EncodeTime(1, 0, 0, 0), TJSONRPCParamDataType.pdtTime);
  lReq.RequestID := 1234;

  lRPCResp := FExecutor2.ExecuteRequest(lReq);
  lRes := ISOTimeStampToDateTime(lRPCResp.Result.AsString());
  DecodeDateTime(lRes, lYear, lMonth, lDay, lHour, lMinute, lSecond, lMillisecond);
  Assert.areEqual(2000, lYear);
end;

procedure TJSONRPCServerTest.TestRequestWithWrongNamedParams;
var
  lReq: IJSONRPCRequest;
  lRPCResp: IJSONRPCResponse;
begin
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'add';
  lReq.Params.AddByName('wrongname1', 3);
  lReq.Params.AddByName('wrongname2', 4);
  lReq.Params.AddByName('wrongname3', 5);
  lReq.RequestID := 1234;

  lRPCResp := FExecutor.ExecuteRequest(lReq);
  Assert.isTrue(lRPCResp.IsError);
  Assert.Contains(lRPCResp.Error.ErrMessage, 'cannot find parameter', true);
end;

procedure TJSONRPCServerTest.TestRequestWithNamedParams_I_I_I_ret_O;
var
  lReq: IJSONRPCRequest;
  lRPCResp: IJSONRPCResponse;
  lS: string;
begin
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'add';
  lReq.Params.AddByName('value1', 3);
  lReq.Params.AddByName('value2', 4);
  lReq.Params.AddByName('value3', 5);
  lReq.RequestID := 1234;

  lRPCResp := FExecutor.ExecuteRequest(lReq);
  lS := (lRPCResp.Result.AsObject as TJDOJsonObject).ToJSON();
  Assert.areEqual(12, TJDOJsonObject(lRPCResp.Result.AsObject).I['res']);

  lRPCResp := FExecutor2.ExecuteRequest(lReq);
  lS := (lRPCResp.Result.AsObject as TJDOJsonObject).ToJSON();
  Assert.areEqual(12, TJDOJsonObject(lRPCResp.Result.AsObject).I['res']);
end;

procedure TJSONRPCServerTest.TestRequestWithNamedParams_I_I_ret_I;
var
  lReq: IJSONRPCRequest;
  lResp: IJSONRPCResponse;
begin
  lReq := TJSONRPCRequest.Create;
  lReq.RequestID := 1234;
  lReq.Method := 'subtract';
  lReq.Params.AddByName('Value1', 18);
  lReq.Params.AddByName('Value2', 8);

  lResp := FExecutor.ExecuteRequest(lReq);
  Assert.areEqual(10, lResp.Result.AsInteger);
  Assert.areEqual(1234, lResp.RequestID.AsInteger);

  lResp := FExecutor2.ExecuteRequest(lReq);
  Assert.areEqual(10, lResp.Result.AsInteger);
  Assert.areEqual(1234, lResp.RequestID.AsInteger);
end;

procedure TJSONRPCServerTest.TestRequestWithoutParams;
var
  lReq: IJSONRPCRequest;
  lResp: IJSONRPCResponse;
begin
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'MyRequest';
  lReq.RequestID := 1234;
  lResp := FExecutor.ExecuteRequest(lReq);
  Assert.isFalse(lResp.IsError);
  Assert.isTrue(lResp.Result.AsBoolean);
end;

procedure TJSONRPCServerTest.TestRequestWithParams_I_I_ret_I;
var
  lReq: IJSONRPCRequest;
  lResp: IJSONRPCResponse;
begin
  lReq := TJSONRPCRequest.Create;
  lReq.RequestID := 1234;
  lReq.Method := 'subtract';
  lReq.Params.Add(18);
  lReq.Params.Add(8);

  lResp := FExecutor.ExecuteRequest(lReq);
  Assert.areEqual(10, lResp.Result.AsInteger);
  Assert.areEqual(1234, lResp.RequestID.AsInteger);

  lResp := FExecutor2.ExecuteRequest(lReq);
  Assert.areEqual(10, lResp.Result.AsInteger);
  Assert.areEqual(1234, lResp.RequestID.AsInteger);
end;

procedure TJSONRPCServerTest.TestRequestWithParams_I_I_ret_A;
var
  lReq: IJSONRPCRequest;
  lRPCResp: IJSONRPCResponse;
  lArr: TJDOJsonArray;
  I: Integer;
  x: Integer;
begin
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'GetListFromTo';
  lReq.Params.Add(1);
  lReq.Params.Add(5);
  lReq.RequestID := 1234;

  lRPCResp := FExecutor.ExecuteRequest(lReq);
  lArr := TJDOJsonArray(lRPCResp.Result.AsObject);
  x := 1;
  for I := 0 to lArr.Count - 1 do
  begin
    Assert.areEqual(x, lArr[I].IntValue);
    Inc(x);
  end;

  lRPCResp := FExecutor2.ExecuteRequest(lReq);
  lArr := TJDOJsonArray(lRPCResp.Result.AsObject);
  x := 1;
  for I := 0 to lArr.Count - 1 do
  begin
    Assert.areEqual(x, lArr[I].IntValue);
    Inc(x);
  end;

end;

procedure TJSONRPCServerTest.TestRequestWithParams_I_I_I_ret_O;
var
  lReq: IJSONRPCRequest;
  lRPCResp: IJSONRPCResponse;
  lS: string;
begin
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'add';
  lReq.Params.Add(3);
  lReq.Params.Add(4);
  lReq.Params.Add(5);
  lReq.RequestID := 1234;

  lRPCResp := FExecutor.ExecuteRequest(lReq);
  lS := (lRPCResp.Result.AsObject as TJDOJsonObject).ToJSON();
  Assert.areEqual(12, TJDOJsonObject(lRPCResp.Result.AsObject).I['res']);

  lRPCResp := FExecutor2.ExecuteRequest(lReq);
  lS := (lRPCResp.Result.AsObject as TJDOJsonObject).ToJSON();
  Assert.areEqual(12, TJDOJsonObject(lRPCResp.Result.AsObject).I['res']);
end;

procedure TJSONRPCServerTest.TestRequest_NamedParams_S_I_ret_S;
var
  lReq: IJSONRPCRequest;
  lRPCResp: IJSONRPCResponse;
begin
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'MultiplyString';
  lReq.Params.AddByName('aString', 'Daniele');
  lReq.Params.AddByName('Multiplier', 4);
  lReq.RequestID := 1234;
  lRPCResp := FExecutor.ExecuteRequest(lReq);
  Assert.isFalse(lRPCResp.IsError);
  Assert.areEqual('DanieleDanieleDanieleDaniele', lRPCResp.Result.AsString);

  lRPCResp := FExecutor2.ExecuteRequest(lReq);
  Assert.areEqual('DanieleDanieleDanieleDaniele', lRPCResp.Result.AsString);
end;

procedure TJSONRPCServerTest.TestRequest_S_I_ret_S;
var
  lReq: IJSONRPCRequest;
  lRPCResp: IJSONRPCResponse;
begin
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'MultiplyString';
  lReq.Params.Add('Daniele');
  lReq.Params.Add(4);
  lReq.RequestID := 1234;
  lRPCResp := FExecutor.ExecuteRequest(lReq);
  Assert.areEqual('DanieleDanieleDanieleDaniele', lRPCResp.Result.AsString);

  lRPCResp := FExecutor2.ExecuteRequest(lReq);
  Assert.areEqual('DanieleDanieleDanieleDaniele', lRPCResp.Result.AsString);
end;

initialization

TDUnitX.RegisterTestFixture(TServerTest);
TDUnitX.RegisterTestFixture(TJSONRPCServerTest);

end.
