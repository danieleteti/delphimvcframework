// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2018 Daniele Teti and the DMVCFramework Team
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
  MVCFramework.RESTClient,
  MVCFramework.JSONRPC.Client;

const

{$IFDEF LINUX_SERVER}
  TEST_SERVER_ADDRESS = '192.168.1.8';

{$ELSE}
  TEST_SERVER_ADDRESS = '127.0.0.1';

{$ENDIF}


type

  [TestFixture]
  TBaseServerTest = class(TObject)
  protected
    RESTClient: TRESTClient;
    procedure DoLoginWith(UserName: string);
    procedure DoLogout;

  public
    [SetUp]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

  end;

  TServerTest = class(TBaseServerTest)
  public
    [Test]
    [TestCase('/fault', '/fault')]
    [TestCase('/fault2', '/fault2')]
    procedure TestControllerWithExceptionInCreate(const URLSegment: string);

    [Test]
    procedure TestReqWithParams;

    // URL_MAPPED_PARAMS_ALLOWED_CHARS = ' אטישעל@\[\]\{\}\(\)\=;&#\.\_\,%\w\d\x2D\x3A';
    [Test]
    [TestCase('1', ' א,ט')]
    [TestCase('2', 'י,ש,ע')]
    [TestCase('2', 'ל,@,[')]
    [TestCase('2', '],{,}')]
    [TestCase('2', '(,),\')]
    [TestCase('2', '=,;,&')]
    [TestCase('2', '#,.,_')]
    [TestCase('2', '%, , ')]
    procedure TestReqWithURLMappedParams(const par1, par2, par3: string);
    [Test]
    procedure TestPOSTWithParamsAndJSONBody;
    [Test]
    // procedure TestPATCHWithParamsAndJSONBody;
    [Test]
    procedure TestPOSTWithObjectJSONBody;
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
    procedure TestRenderWrappedListWithCompression;
    [Test]
    procedure TestRenderStreamAndFreeWithOwnerFalse;
    [Test]
    procedure TestRenderStreamAndFreeWithOwnerTrue;
    [Test]
    // procedure TestSerializationType;
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
    procedure TestStringDictionary;
    [Test]
    procedure TestWrongJSONBody;
  end;

  [TestFixture]
  TJSONRPCServerTest = class(TBaseServerTest)
  protected
    FExecutor: IMVCJSONRPCExecutor;
  public
    [SetUp]
    procedure SetUp;
    [Test]
    procedure TestRequestWithoutParams;
    [Test]
    procedure TestRequestToNotFoundMethod;
    [Test]
    procedure TestRequestWithParams_I_I_ret_I;
    [Test]
    procedure TestRequestWithParams_I_I_I_ret_O;
    [Test]
    procedure TestRequest_S_I_ret_S;
    [Test]
    procedure TestRequestWithParams_I_I_ret_A;
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
  MVCFramework.JSONRPC;

{ TServerTest }

procedure TBaseServerTest.DoLogout;
var
  res: IRESTResponse;
begin
  res := RESTClient.doGET('/logout', []);
  Assert.isTrue(res.ResponseCode = HTTP_STATUS.OK, 'Logout Failed');
end;

procedure TBaseServerTest.SetUp;
begin
  inherited;
  RESTClient := TRESTClient.Create(TEST_SERVER_ADDRESS, 9999);
  RESTClient.ReadTimeout(60 * 1000 * 30);
end;

procedure TBaseServerTest.TearDown;
begin
  inherited;
  RESTClient.Free;
end;

procedure TServerTest.TestActionFiltersOnBeforeAction;
var
  res: IRESTResponse;
begin
  res := RESTClient.doGET('/actionfilters/beforeaction/alwayscalled', []);
  Assert.areEqual<Integer>(HTTP_STATUS.OK, res.ResponseCode);

  res := RESTClient.doGET('/actionfilters/beforeaction/nevercalled', []);
  Assert.areEqual<Integer>(HTTP_STATUS.NotFound, res.ResponseCode);
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
    RESTClient.Asynch(
      procedure(Response: IRESTResponse)
      begin
        OK := true;
        evt.SetEvent;
      end,
      procedure(E: Exception)
      begin
        OK := false;
      end).doDELETE('/req/with/params', ['1', '2', '3']);

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
    RESTClient.Asynch(
      procedure(Response: IRESTResponse)
      begin
        try
          j := TSystemJSON.StringAsJSONObject(Response.BodyAsString);
        except
          // test should not block...never!
        end;
        evt.SetEvent;
      end,
      procedure(E: Exception)
      begin
      end).doGET('/req/with/params', ['1', '2', '3']);

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

    RESTClient.Asynch(
      procedure(Response: IRESTResponse)
      begin
        try
          j := TSystemJSON.StringAsJSONObject(Response.BodyAsString);
        except
          // test should not block...never!
        end;
        evt.SetEvent;
      end,
      procedure(E: Exception)
      begin
      end).doPOST('/echo', ['1', '2', '3'],
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
    RESTClient.Asynch(
      procedure(Response: IRESTResponse)
      begin
        try
          j := TSystemJSON.StringAsJSONObject(Response.BodyAsString);
        except
          // test should not block...never!
        end;
        evt.SetEvent;
      end,
      procedure(E: Exception)
      begin
      end).doPUT('/echo', ['1', '2', '3'],
      TSystemJSON.JSONValueToString(System.JSON.TJSONObject.Create(System.JSON.TJSONPair.Create('from client', 'hello world'))));

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
  LRes: IRESTResponse;
begin
  RESTClient.Authentication('user1', 'user1');
  Assert.areEqual('user1', RESTClient.UserName);
  Assert.areEqual('user1', RESTClient.Password);
  LRes := RESTClient.doGET('/private/role1', []);
  Assert.areEqual<Integer>(HTTP_STATUS.OK, LRes.ResponseCode);
end;

procedure TServerTest.TestBasicAuth02;
var
  LRes: IRESTResponse;
begin
  RESTClient.UserName := '';
  RESTClient.Password := '';
  RESTClient.UseBasicAuthentication := false;
  LRes := RESTClient.doGET('/private/role1', []);
  Assert.areEqual<Integer>(HTTP_STATUS.Unauthorized, LRes.ResponseCode);
end;

procedure TServerTest.TestBasicAuth03;
var
  LRes: IRESTResponse;
begin
  RESTClient.UserName := 'user1';
  RESTClient.Password := 'user1';
  RESTClient.UseBasicAuthentication := true;
  LRes := RESTClient.doGET('/private/role2', []);
  Assert.areEqual<Integer>(HTTP_STATUS.Forbidden, LRes.ResponseCode);
end;

procedure TServerTest.TestBasicAuth04;
var
  LRes: IRESTResponse;
begin
  RESTClient.UserName := 'user1';
  RESTClient.Password := 'user1';
  RESTClient.UseBasicAuthentication := true;
  LRes := RESTClient.doGET('/private/role1', []);
  Assert.areEqual<Integer>(HTTP_STATUS.OK, LRes.ResponseCode);
  LRes := RESTClient.doGET('/people', []);
  Assert.areEqual<Integer>(HTTP_STATUS.OK, LRes.ResponseCode);
end;

procedure TServerTest.TestBasicAuth05;
var
  LRes: IRESTResponse;
begin
  RESTClient.UserName := 'user1';
  RESTClient.Password := 'user1';
  RESTClient.UseBasicAuthentication := true;

  // first
  LRes := RESTClient.doGET('/private/role1session?value=danieleteti', []);
  Assert.areEqual<Integer>(HTTP_STATUS.OK, LRes.ResponseCode);
  LRes := RESTClient.doGET('/private/role1session', []);
  Assert.areEqual<Integer>(HTTP_STATUS.OK, LRes.ResponseCode);
  Assert.areEqual('danieleteti', LRes.BodyAsString);

  // second
  LRes := RESTClient.doGET('/private/role1session?value=johndoe', []);
  Assert.areEqual<Integer>(HTTP_STATUS.OK, LRes.ResponseCode);
  LRes := RESTClient.doGET('/private/role1session', []);
  Assert.areEqual<Integer>(HTTP_STATUS.OK, LRes.ResponseCode);
  Assert.areEqual('johndoe', LRes.BodyAsString);
end;

procedure TServerTest.TestControllerWithExceptionInCreate(
  const URLSegment: string);
var
  res: IRESTResponse;
begin
  res := RESTClient.doGET(URLSegment, []);
  Assert.areEqual(HTTP_STATUS.InternalServerError, res.ResponseCode);
  Assert.Contains(res.ContentType, 'text/plain', true, 'Is not a text/plain in case of error');
  Assert.Contains(res.BodyAsString, 'Cannot create controller', true, 'Exception message in body is not correct');
end;

procedure TServerTest.TestCookies;
var
  res: IRESTResponse;
  I: Integer;
begin
  res := RESTClient.doGET('/lotofcookies', []);
  Assert.areEqual<Integer>(HTTP_STATUS.OK, res.ResponseCode);
  Assert.areEqual(4, res.Cookies.Count, 'Wrong number of cookies');
  for I := 0 to 3 do
  begin
    Assert.areEqual('usersettings' + IntToStr(I + 1), res.Cookies.Cookies[I].CookieName);
    Assert.areEqual('usersettings' + IntToStr(I + 1) + '-value', res.Cookies.Cookies[I].Value);
    Assert.areEqual('/usersettings' + IntToStr(I + 1), res.Cookies.Cookies[I].Path);
  end;

end;

procedure TServerTest.TestCustomAuthRequestWithoutLogin;
var
  LRes: IRESTResponse;
begin
  LRes := RESTClient.doGET('/privatecustom/role1', []);
  Assert.areEqual<Integer>(HTTP_STATUS.Unauthorized, LRes.ResponseCode);
  Assert.areEqual('/system/users/logged', LRes.HeaderValue('X-LOGIN-URL'));
  Assert.areEqual('POST', LRes.HeaderValue('X-LOGIN-METHOD'));

  LRes := RESTClient.doGET('/privatecustom/role2', []);
  Assert.areEqual<Integer>(HTTP_STATUS.Unauthorized, LRes.ResponseCode);
  Assert.areEqual('/system/users/logged', LRes.HeaderValue('X-LOGIN-URL'));
  Assert.areEqual('POST', LRes.HeaderValue('X-LOGIN-METHOD'));
end;

procedure TServerTest.TestCustomAuthRequestsWithValidLogin;
var
  LRes: IRESTResponse;
  lJSON: System.JSON.TJSONObject;
  lCookieValue: string;
begin
  lJSON := System.JSON.TJSONObject.Create;
  try
    lJSON.AddPair('username', 'user1');
    lJSON.AddPair('password', 'user1');
    LRes := RESTClient.doPOST('/system/users/logged', [], TSystemJSON.JSONValueToString(lJSON, false));
    Assert.areEqual('application/json', LRes.ContentType);
    Assert.areEqual<Integer>(HTTP_STATUS.OK, LRes.ResponseCode);
    Assert.areEqual('/system/users/logged', LRes.HeaderValue('X-LOGOUT-URL'));
    Assert.areEqual('DELETE', LRes.HeaderValue('X-LOGOUT-METHOD'));
    Assert.areEqual('{"status":"OK"}', LRes.BodyAsString);
    lCookieValue := LRes.Cookies[LRes.Cookies.GetCookieIndex(TMVCConstants.SESSION_TOKEN_NAME)].Value;
    Assert.AreNotEqual('', lCookieValue, 'Session cookie not returned after login');
    Assert.isFalse(lCookieValue.Contains('invalid'), 'Returned an invalid session token');

    LRes := RESTClient.doGET('/privatecustom/role2', []);
    Assert.areEqual<Integer>(HTTP_STATUS.Forbidden, LRes.ResponseCode,
      'Authorization not respected for not allowed action');

    LRes := RESTClient.doGET('/privatecustom/role1', []);
    Assert.areEqual<Integer>(HTTP_STATUS.OK, LRes.ResponseCode, 'Authorization not respected for allowed action');
  finally
    lJSON.Free;
  end;
end;

procedure TServerTest.TestCustomAuthRequestsWithValidLogin_HTML;
var
  LRes: IRESTResponse;
  lJSON: System.JSON.TJSONObject;
  lCookieValue: string;
begin
  lJSON := System.JSON.TJSONObject.Create;
  try
    lJSON.AddPair('username', 'user1');
    lJSON.AddPair('password', 'user1');
    LRes := RESTClient.Accept('text/html').doPOST('/system/users/logged', [],
      TSystemJSON.JSONValueToString(lJSON, false));
    Assert.areEqual('application/json', LRes.ContentType);
    Assert.areEqual<Integer>(HTTP_STATUS.OK, LRes.ResponseCode);
    Assert.areEqual('/system/users/logged', LRes.HeaderValue('X-LOGOUT-URL'));
    Assert.areEqual('DELETE', LRes.HeaderValue('X-LOGOUT-METHOD'));
    Assert.areEqual('{"status":"OK"}', LRes.BodyAsString);
    lCookieValue := LRes.Cookies[LRes.Cookies.GetCookieIndex(TMVCConstants.SESSION_TOKEN_NAME)].Value;
    Assert.AreNotEqual('', lCookieValue, 'Session cookie not returned after login');
    Assert.isFalse(lCookieValue.Contains('invalid'), 'Returned an invalid session token');

    LRes := RESTClient.doGET('/privatecustom/role2', []);
    Assert.areEqual<Integer>(HTTP_STATUS.Forbidden, LRes.ResponseCode,
      'Authorization not respected for not allowed action');

    LRes := RESTClient.doGET('/privatecustom/role1', []);
    Assert.areEqual<Integer>(HTTP_STATUS.OK, LRes.ResponseCode, 'Authorization not respected for allowed action');
  finally
    lJSON.Free;
  end;
end;

procedure TServerTest.TestCustomAuthWrongRequestBodies;
var
  LRes: IRESTResponse;
  lJSON: System.JSON.TJSONObject;
begin
  lJSON := System.JSON.TJSONObject.Create;
  try
    // no request body
    LRes := RESTClient.doPOST('/system/users/logged', []);
    Assert.areEqual<Integer>(HTTP_STATUS.BadRequest, LRes.ResponseCode,
      'Empty request body doesn''t return HTTP 400 Bad Request');

    // wrong request body 1
    LRes := RESTClient.doPOST('/system/users/logged', [], TSystemJSON.JSONValueToString(lJSON, false));
    Assert.areEqual<Integer>(HTTP_STATUS.Unauthorized, LRes.ResponseCode,
      'Invalid json doesn''t return HTTP 401 Unauthorized');

    // wrong request body 2
    lJSON.AddPair('username', '');
    lJSON.AddPair('password', '');
    LRes := RESTClient.doPOST('/system/users/logged', [], TSystemJSON.JSONValueToString(lJSON, false));
    Assert.areEqual<Integer>(HTTP_STATUS.Unauthorized, LRes.ResponseCode,
      'Empty username and password doesn''t return HTTP 401 Unauthorized');

    // wrong username and password 3
    lJSON.RemovePair('username').Free;
    lJSON.RemovePair('password').Free;
    lJSON.AddPair('username', 'notvaliduser');
    lJSON.AddPair('password', 'notvalidpassword');
    LRes := RESTClient.doPOST('/system/users/logged', [], TSystemJSON.JSONValueToString(lJSON, false));
    Assert.areEqual<Integer>(HTTP_STATUS.Unauthorized, LRes.ResponseCode,
      'Wrong username and password doesn''t return HTTP 401 Unauthorized');
  finally
    lJSON.Free;
  end;
end;

procedure TServerTest.TestCustomAuthLoginLogout;
var
  LRes: IRESTResponse;
  lJSON: System.JSON.TJSONObject;
  lLogoutUrl: string;
  lValue: string;
  I: Integer;
  lPieces: TArray<string>;
  lPass: boolean;
begin
  lJSON := System.JSON.TJSONObject.Create;
  try
    lJSON.AddPair('username', 'user1');
    lJSON.AddPair('password', 'user1');
    LRes := RESTClient.doPOST('/system/users/logged', [], TSystemJSON.JSONValueToString(lJSON, false));

    Assert.areEqual<Integer>(HTTP_STATUS.OK, LRes.ResponseCode);
    lLogoutUrl := LRes.HeaderValue('X-LOGOUT-URL');

    LRes := RESTClient.doDELETE(lLogoutUrl, []);
    lPass := false;
    for I := 0 to LRes.Headers.Count do
    begin
      lValue := LRes.Headers[I];
      if lValue.StartsWith('Set-Cookie') then
      begin
        lPieces := lValue.Split([':']);
        lValue := lPieces[1].Trim;
        if lValue.StartsWith(TMVCConstants.SESSION_TOKEN_NAME) and lValue.Contains('invalid') then
        begin
          lPass := true;
          Break;
        end;
      end;
    end;
    Assert.isTrue(lPass, 'No session cookie cleanup in the response');
  finally
    lJSON.Free;
  end;

end;

procedure TServerTest.TestEncodingRenderJSONValue;
var
  res: IRESTResponse;
  s: string;
  lJSONObj: System.JSON.TJSONObject;
begin
  res := RESTClient.doGET('/encoding', []);

  lJSONObj := TSystemJSON.StringAsJSONObject(res.BodyAsString);
  s := lJSONObj.Get('name1').JsonValue.Value;
  Assert.areEqual('jרrn', s);
  lJSONObj.Free;

  lJSONObj := TSystemJSON.StringAsJSONObject(res.BodyAsString);
  s := lJSONObj.Get('name3').JsonValue.Value;
  Assert.areEqual('אטילעש', s);
  lJSONObj.Free;

  lJSONObj := TSystemJSON.StringAsJSONObject(res.BodyAsString);
  s := lJSONObj.Get('name2').JsonValue.Value;
  Assert.areEqual('to je Unicode?', s, 'If this test fail, check http://qc.embarcadero.com/wc/qcmain.aspx?d=119779');
  lJSONObj.Free;
  { WARNING!!! }
  {
    If this test fail, check
    http://qc.embarcadero.com/wc/qcmain.aspx?d=119779
  }
end;

procedure TServerTest.TestRenderStreamAndFreeWithOwnerFalse;
var
  LRes: IRESTResponse;
begin
  LRes := RESTClient.doGET('/renderstreamandfreewithownerfalse', []);
  Assert.areEqual<Integer>(200, LRes.ResponseCode);
end;

procedure TServerTest.TestRenderStreamAndFreeWithOwnerTrue;
var
  LRes: IRESTResponse;
begin
  LRes := RESTClient.doGET('/renderstreamandfreewithownertrue', []);
  Assert.areEqual<Integer>(200, LRes.ResponseCode);
end;

procedure TServerTest.TestRenderWrappedList;
var
  LRes: IRESTResponse;
  lJSONArr: System.JSON.TJSONArray;
  I: Integer;
  lJSONObj: System.JSON.TJSONObject;
begin
  LRes := RESTClient.doGET('/wrappedpeople', []);

  lJSONArr := TSystemJSON.StringAsJSONArray(LRes.BodyAsString);
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
  LRes: IRESTResponse;
  lJSONArr: TJDOJSONArray;
  I: Integer;
  lCompType: string;
  j: Integer;
const
  CompressionTypes: array [1 .. 9] of string =
    ('deflate', 'gzip', 'deflate,gzip', 'gzip,deflate',
    'gzip,invalid', 'deflate,notvalid', 'notvalid,gzip', 'invalid', '');
  CompressionTypeResult: array [1 .. 9] of string =
    ('deflate', 'gzip', 'deflate', 'gzip',
    'gzip', 'deflate', 'gzip', '', '');
begin
  j := 1;
  for lCompType in CompressionTypes do
  begin
    RESTClient.RequestHeaders.Values['Accept-Encoding'] := lCompType;
    LRes := RESTClient.doGET('/wrappedpeople', [], ['count'], ['100']);
    Assert.areEqual(CompressionTypeResult[j], LRes.HeaderValue('Content-Encoding'));
    lJSONArr := TMVCJsonDataObjectsSerializer.ParseArray(LRes.BodyAsString);
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
  res: IRESTResponse;
begin
  res := RESTClient.doGET('/exception/aftercreate/nevercalled', []);
  Assert.areEqual<Integer>(HTTP_STATUS.InternalServerError, res.ResponseCode);
end;

procedure TServerTest.TestExceptionInMVCBeforeDestroy;
var
  res: IRESTResponse;
begin
  res := RESTClient.doGET('/exception/beforedestroy/nevercalled', []);
  Assert.areEqual<Integer>(HTTP_STATUS.InternalServerError, res.ResponseCode);
end;

procedure TServerTest.TestInvalidateSession;
var
  c1: TRESTClient;
  res: IRESTResponse;
begin
  c1 := TRESTClient.Create(TEST_SERVER_ADDRESS, 9999);
  try
    c1.Accept(TMVCMediaType.APPLICATION_JSON);
    c1.doPOST('/session', ['daniele teti']); // imposto un valore in sessione
    res := c1.doGET('/session', []); // rileggo il valore dalla sessione
    Assert.areEqual('daniele teti', res.BodyAsString);
    c1.SessionID := '';
    res := c1.doGET('/session', []); // rileggo il valore dalla sessione
    Assert.areEqual('', res.BodyAsString);
  finally
    c1.Free;
  end;
end;

procedure TServerTest.TestMiddlewareHandler;
var
  r: IRESTResponse;
begin
  r := RESTClient.Accept(TMVCMediaType.APPLICATION_JSON).doGET('/handledbymiddleware', []);
  Assert.areEqual('This is a middleware response', r.BodyAsString);
  Assert.areEqual<Integer>(HTTP_STATUS.OK, r.ResponseCode);
end;

procedure TServerTest.TestMiddlewareSpeedMiddleware;
var
  r: IRESTResponse;
  P: TPerson;
begin
  P := TPerson.Create;
  try
    P.FirstName := StringOfChar('*', 1000);
    P.LastName := StringOfChar('*', 1000);
    P.DOB := EncodeDate(1979, 1, 1);
    P.Married := true;
    r := RESTClient.Accept(TMVCMediaType.APPLICATION_JSON)
      .doPOST('/objects', [], GetDefaultSerializer.SerializeObject(P));
  finally
    P.Free;
  end;

  Assert.AreNotEqual('', r.HeaderValue('request_gen_time'));
end;

// procedure TServerTest.TestPATCHWithParamsAndJSONBody;
// var
// r: IRESTResponse;
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
  LRes: IRESTResponse;
  LCustomers: TObjectList<TCustomer>;
begin
  LCustomers := TCustomer.GetList;
  try
    LRes := RESTClient.doPOST('/customers/list', [], GetDefaultSerializer.SerializeCollection(LCustomers)
    { Mapper.ObjectListToJSONArray<TCustomer>(LCustomers) }
      );
    Assert.areEqual<Integer>(HTTP_STATUS.OK, LRes.ResponseCode);
  finally
    LCustomers.Free;
  end;
end;

procedure TServerTest.TestPOSTWithObjectJSONBody;
var
  r: IRESTResponse;
  P: TPerson;
begin
  P := TPerson.Create;
  try
    P.FirstName := 'Daniele';
    P.LastName := 'אעשטיל';
    P.DOB := EncodeDate(1979, 1, 1);
    P.Married := true;
    try
      r := RESTClient.Accept(TMVCMediaType.APPLICATION_JSON)
        .doPOST('/objects', [], GetDefaultSerializer.SerializeObject(P)
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
    GetDefaultSerializer.DeserializeObject(r.BodyAsString, P);
    // P := Mapper.JSONObjectToObject<TPerson>(r.BodyAsJsonObject);
    Assert.areEqual('Daniele', P.FirstName);
    Assert.areEqual('אעשטיל', P.LastName);
    Assert.areEqual(true, P.Married);
    Assert.areEqual(EncodeDate(1979, 1, 1), P.DOB);
  finally
    P.Free;
  end;
end;

procedure TServerTest.TestPOSTWithParamsAndJSONBody;
var
  r: IRESTResponse;
  JSON: System.JSON.TJSONObject;
begin
  JSON := System.JSON.TJSONObject.Create;
  JSON.AddPair('client', 'clientdata');
  r := RESTClient.doPOST('/echo', ['1', '2', '3'], TSystemJSON.JSONValueToString(JSON));
  JSON := TSystemJSON.StringAsJSONObject(r.BodyAsString);
  try
    Assert.areEqual('clientdata', JSON.Get('client').JsonValue.Value);
    Assert.areEqual('from server', JSON.Get('echo').JsonValue.Value);
  finally
    JSON.Free;
  end;
end;

procedure TServerTest.TestProducesConsumesWithWrongAcceptHeader;
var
  res: IRESTResponse;
begin
  res := RESTClient.Accept('text/plain')
  // action is waiting for a accept: application/json
    .ContentType('application/json').doPOST('/testconsumes', [],
    TSystemJSON.JSONValueToString(TJSONString.Create('Hello World')));
  Assert.areEqual<Integer>(HTTP_STATUS.NotFound, res.ResponseCode);
end;

procedure TServerTest.TestProducesConsumes01;
var
  res: IRESTResponse;
begin
  res := RESTClient.Accept('application/json').ContentType('application/json').ContentCharSet('utf-8')
    .doPOST('/testconsumes', [], TSystemJSON.JSONValueToString(TJSONString.Create('Hello World')));
  Assert.areEqual<Integer>(HTTP_STATUS.OK, res.ResponseCode);
  Assert.areEqual('Hello World', res.BodyAsString);
  Assert.areEqual('application/json', res.ContentType);
  Assert.areEqual('utf-8', res.ContentTypeCharset);
end;

procedure TServerTest.TestProducesConsumes02;
var
  res: IRESTResponse;
begin
  res := RESTClient.Accept('text/plain').ContentType('text/plain').doPOST('/testconsumes', [], 'Hello World');
  Assert.areEqual('Hello World', res.BodyAsString);
  Assert.areEqual('text/plain', res.ContentType);
  Assert.areEqual('UTF-8', res.ContentTypeCharset);

  res := RESTClient.Accept('text/plain').ContentType('application/json')
    .doPOST('/testconsumes', [], '{"name": "Daniele"}');
  Assert.areEqual<Integer>(HTTP_STATUS.NotFound, res.ResponseCode);
end;

procedure TServerTest.TestProducesConsumes03;
var
  res: IRESTResponse;
begin
  res := RESTClient.Accept(TMVCMediaType.TEXT_PLAIN)
    .ContentType(BuildContentType(TMVCMediaType.TEXT_PLAIN, TMVCCharSet.ISO88591)).doPOST('/testconsumes/textiso8859_1',
    [], 'אטילעש');
  Assert.areEqual<Integer>(HTTP_STATUS.OK, res.ResponseCode);
  Assert.AreNotEqual('אטילעש', res.BodyAsString, 'non iso8859-1 text is rendered ok whan should not');
  Assert.areEqual(TMVCMediaType.TEXT_PLAIN, res.ContentType);
  Assert.areEqual(TMVCCharSet.ISO88591, res.ContentTypeCharset.ToLower);

  res := RESTClient.Accept(TMVCMediaType.TEXT_PLAIN)
    .ContentType(BuildContentType(TMVCMediaType.TEXT_PLAIN, TMVCCharSet.ISO88591)).doPOST('/testconsumes/textiso8859_1',
    [], 'this is an iso8859-1 text');
  Assert.areEqual<Integer>(HTTP_STATUS.OK, res.ResponseCode);
  Assert.areEqual('this is an iso8859-1 text', res.BodyAsString);
  Assert.areEqual(TMVCMediaType.TEXT_PLAIN, res.ContentType);
  Assert.areEqual(TMVCCharSet.ISO88591, res.ContentTypeCharset.ToLower);

end;

procedure TServerTest.TestPUTWithParamsAndJSONBody;
var
  r: IRESTResponse;
  JSON: System.JSON.TJSONObject;
begin
  JSON := System.JSON.TJSONObject.Create;
  JSON.AddPair('client', 'clientdata');
  r := RESTClient.doPUT('/echo', ['1', '2', '3'], TSystemJSON.JSONValueToString(JSON));

  JSON := TSystemJSON.StringAsJSONObject(r.BodyAsString);
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
  r: IRESTResponse;
begin
  r := RESTClient.doGET('/unknownurl/bla/bla', []);

  ss := TStringStream.Create;
  try
    ss.CopyFrom(r.Body, 0);
    Assert.areEqual(ss.DataString, r.BodyAsString,
      'In case of protocol error, the body doesn''t contain the same of BodyAsString');
  finally
    ss.Free;
  end;

  Assert.areEqual<Integer>(HTTP_STATUS.NotFound, r.ResponseCode, '/unknownurl/bla/bla');

  r := RESTClient.doGET('/req/with/params/', []);
  Assert.areEqual<Integer>(HTTP_STATUS.NotFound, r.ResponseCode, '/req/with/params/');

  r := RESTClient.doGET('/req/with/params', []);
  Assert.areEqual<Integer>(HTTP_STATUS.NotFound, r.ResponseCode, '/req/with/params');

  r := RESTClient.doGET('/req/with/params', ['1', '2', '3']);
  Assert.areEqual<Integer>(HTTP_STATUS.OK, r.ResponseCode);

  lJSON := TSystemJSON.StringAsJSONObject(r.BodyAsString);
  try
    Assert.areEqual('1', lJSON.Get('par1').JsonValue.Value);
    Assert.areEqual('2', lJSON.Get('par2').JsonValue.Value);
    Assert.areEqual('3', lJSON.Get('par3').JsonValue.Value);
    Assert.areEqual('GET', lJSON.Get('method').JsonValue.Value);

    r := RESTClient.doPOST('/req/with/params', ['1', '2', '3']);
    Assert.areEqual<Integer>(HTTP_STATUS.NotFound, r.ResponseCode);

    r := RESTClient.doPUT('/req/with/params', ['1', '2', '3']);
    Assert.areEqual<Integer>(HTTP_STATUS.NotFound, r.ResponseCode);

    r := RESTClient.doDELETE('/req/with/params', ['1', '2', '3']);
    Assert.areEqual<Integer>(HTTP_STATUS.OK, r.ResponseCode);
    Assert.areEqual('', r.BodyAsString);
  finally
    lJSON.Free;
  end;

end;

procedure TServerTest.TestReqWithURLMappedParams(const par1, par2, par3: string);
var
  r: IRESTResponse;
begin
  r := RESTClient.doGET('/req/with/params', [par1, par2, par3]);
  Assert.areEqual<Integer>(HTTP_STATUS.OK, r.ResponseCode,
    Format('URL mapped fails for these characters: "%s","%s","%s"', [par1, par2, par3]));
end;

// procedure TServerTest.TestSerializationType;
// var
// LResp: IRESTResponse;
// LPersonProps, LPersonFlds: TPerson;
// LObj: TObject;
// begin
// LResp := RESTClient.doGET('/people', ['1']);
// LPersonProps := Mapper.JSONObjectToObject<TPerson>(LResp.BodyAsJsonObject);
// try
// LResp := RESTClient.doGET('/people', ['1', 'asfields']);
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

procedure TServerTest.TestSession;
var
  c1: TRESTClient;
  res: IRESTResponse;
  s: string;
begin
  c1 := TRESTClient.Create(TEST_SERVER_ADDRESS, 9999);
  try
    c1.Accept(TMVCMediaType.APPLICATION_JSON);
    res := c1.doPOST('/session', ['daniele teti']); // imposto un valore in sessione
    s := res.HeaderValue('Set-Cookie');
    Assert.isFalse(s.Contains('Expires'), 'Session cookie contains "expires" attribute');
    res := c1.doGET('/session', []); // rileggo il valore dalla sessione
    Assert.areEqual('daniele teti', res.BodyAsString);
    c1.Accept(TMVCMediaType.TEXT_PLAIN);
    res := c1.doGET('/session', []);
    // rileggo il valore dalla sessione
    Assert.areEqual('daniele teti', res.BodyAsString);

    // aggiungo altri cookies
    res := c1.doGET('/lotofcookies', []); // rileggo il valore dalla sessione
    Assert.areEqual<Integer>(HTTP_STATUS.OK, res.ResponseCode);
    c1.Accept(TMVCMediaType.TEXT_PLAIN);
    res := c1.doGET('/session', []); // rileggo il valore dalla sessione
    Assert.areEqual('daniele teti', res.BodyAsString);
  finally
    c1.Free;
  end;
end;

procedure TServerTest.TestSessionWithLogin;
begin
  DoLoginWith('daniele');
  DoLogout;
end;

procedure TServerTest.TestStringDictionary;
var
  LRes: IRESTResponse;
  lSer: TMVCJsonDataObjectsSerializer;
  lDict: TMVCStringDictionary;
begin
  LRes := RESTClient.doPOST('/stringdictionary', [], '{"prop1":"value1","prop2":"value2"}');
  Assert.areEqual(200, LRes.ResponseCode);
  lSer := TMVCJsonDataObjectsSerializer.Create;
  try
    lDict := TMVCStringDictionary.Create;
    try
      lSer.DeserializeObject(LRes.BodyAsString, lDict);
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
  res: IRESTResponse;
  lJObj: System.JSON.TJSONObject;
begin
  // ----------------------'/typed/all/($ParString)/($ParInteger)/($ParInt64)/($ParSingle)/($ParDouble)/($ParExtended)')', []);
  res := RESTClient.doGET('/typed/all/mystring/1234/12345678/12.3/1234.5678/1234.5678', []);
  Assert.isTrue(res.ResponseCode = HTTP_STATUS.OK, 'Cannot route');
  lJObj := TSystemJSON.StringAsJSONObject(res.BodyAsString);
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
  res: IRESTResponse;
begin
  res := RESTClient.doGET('/typed/booleans/true/false/1/0', []);
  Assert.isTrue(res.ResponseCode = HTTP_STATUS.OK, 'Cannot route');
  Assert.areEqual('true.false.true.false', res.BodyAsString.ToLower);
end;

procedure TServerTest.TestTypedDouble1;
var
  res: IRESTResponse;
begin
  res := RESTClient.doGET('/typed/double1/1234.5678', []);
  Assert.isTrue(res.ResponseCode = HTTP_STATUS.OK, 'Cannot route');
  Assert.areEqual('1234.5678 modified from server', res.BodyAsString);

end;

procedure TServerTest.TestTypedExtended1;
var
  res: IRESTResponse;
begin
  res := RESTClient.doGET('/typed/extended1/1234.5678', []);
  Assert.isTrue(res.ResponseCode = HTTP_STATUS.OK, 'Cannot route');
  Assert.areEqual('1234.5678 modified from server', res.BodyAsString);

end;

procedure TServerTest.TestTypedInt641;
var
  res: IRESTResponse;
begin
  res := RESTClient.doGET('/typed/int641/12345678', []);
  Assert.isTrue(res.ResponseCode = HTTP_STATUS.OK, 'Cannot route');
  Assert.areEqual('12345678 modified from server', res.BodyAsString);
end;

procedure TServerTest.TestTypedInteger1;
var
  res: IRESTResponse;
begin
  res := RESTClient.doGET('/typed/integer1/1234', []);
  Assert.isTrue(res.ResponseCode = HTTP_STATUS.OK, 'Cannot route');
  Assert.areEqual('1234 modified from server', res.BodyAsString);
end;

procedure TServerTest.TestTypedSingle1;
var
  res: IRESTResponse;
begin
  res := RESTClient.doGET('/typed/single1/1234.5', []);
  Assert.isTrue(res.ResponseCode = HTTP_STATUS.OK, 'Cannot route');
  Assert.areEqual('1234.5 modified from server', res.BodyAsString);

end;

procedure TServerTest.TestTypedString1;
var
  res: IRESTResponse;
begin
  res := RESTClient.doGET('/typed/string1/daniele', []);
  Assert.isTrue(res.ResponseCode = HTTP_STATUS.OK, 'Cannot route');
  Assert.areEqual('daniele modified from server', res.BodyAsString);
end;

procedure TServerTest.TestWrongJSONBody;
var
  LRes: IRESTResponse;
begin
  LRes := RESTClient.doPOST('/stringdictionary', [], '{"prop1","value1"}');
  Assert.areEqual(HTTP_STATUS.BadRequest, LRes.ResponseCode);
end;

procedure TServerTest.TestTypedDateTimeTypes;
var
  res: IRESTResponse;
begin
  // TDate, wrong and correct
  res := RESTClient.doGET('/typed/tdate1/20161012', []);
  Assert.areEqual<Integer>(HTTP_STATUS.InternalServerError, res.ResponseCode, 'wrong TDate');

  res := RESTClient.doGET('/typed/tdate1/2016-10-12', []);
  Assert.areEqual<Integer>(HTTP_STATUS.OK, res.ResponseCode);
  Assert.areEqual('2016-10-12 modified from server', res.BodyAsString);

  // TDateTime, wrong and correct
  res := RESTClient.doGET('/typed/tdatetime1/20161', []);
  Assert.areEqual<Integer>(HTTP_STATUS.InternalServerError, res.ResponseCode, 'wrong TDateTime (1)');

  // Wrong
  res := RESTClient.doGET('/typed/tdatetime1/20161012121212', []);
  Assert.areEqual<Integer>(HTTP_STATUS.InternalServerError, res.ResponseCode, 'wrong TDateTime (2)');

  // Correct without 'T'
  res := RESTClient.doGET('/typed/tdatetime1/2016-10-12 12:12:12', []);
  Assert.areEqual<Integer>(HTTP_STATUS.OK, res.ResponseCode, 'wrong TDateTime (3)');
  Assert.areEqual('2016-10-12T12:12:12.000Z modified from server', res.BodyAsString);

  // Correct in extended form
  res := RESTClient.doGET('/typed/tdatetime1/2016-10-12T12:12:12', []);
  Assert.areEqual<Integer>(HTTP_STATUS.OK, res.ResponseCode);
  Assert.areEqual('2016-10-12T12:12:12.000Z modified from server', res.BodyAsString);

  // Correct in extended form with timezone
  res := RESTClient.doGET('/typed/tdatetime1/2016-10-12T12:12:12.000Z', []);
  Assert.areEqual<Integer>(HTTP_STATUS.OK, res.ResponseCode);
  Assert.areEqual('2016-10-12T12:12:12.000Z modified from server', res.BodyAsString);

  // TTime, wrong and correct
  res := RESTClient.doGET('/typed/ttime1/121212', []);
  Assert.areEqual<Integer>(HTTP_STATUS.InternalServerError, res.ResponseCode, 'wrong TTime');

  res := RESTClient.doGET('/typed/ttime1/12:12:12', []);
  Assert.areEqual<Integer>(HTTP_STATUS.OK, res.ResponseCode);
  Assert.areEqual('12:12:12 modified from server', res.BodyAsString);

end;

procedure TBaseServerTest.DoLoginWith(UserName: string);
var
  res: IRESTResponse;
begin
  res := RESTClient.doGET('/login', [UserName]);
  Assert.isTrue(res.ResponseCode = HTTP_STATUS.OK, 'Login Failed');
end;

{ TJSONRPCServerTest }

procedure TJSONRPCServerTest.SetUp;
begin
  FExecutor := TMVCJSONRPCExecutor.Create('http://localhost:9999/jsonrpc', false);
end;

procedure TJSONRPCServerTest.TestRequestToNotFoundMethod;
var
  lReq: TJSONRPCRequest;
  lResp: TJSONRPCResponse;
begin
  lReq := TJSONRPCRequest.Create;
  try
    lReq.Method := 'nonexist';
    lReq.RequestID := 1234;

    lResp := FExecutor.ExecuteRequest(lReq);
    try
      Assert.IsNotNull(lResp.Error);
      Assert.areEqual(-32601, lResp.Error.Code);
      Assert.isTrue(lResp.Error.ErrMessage.StartsWith('Method "nonexist" not found.'));
    finally
      lResp.Free;
    end;
  finally
    lReq.Free;
  end;
end;

procedure TJSONRPCServerTest.TestRequestWithoutParams;
var
  lReq: TJSONRPCNotification;
begin
  lReq := TJSONRPCNotification.Create;
  try
    lReq.Method := 'mynotify';
    FExecutor.ExecuteNotification(lReq);
    Assert.Pass();
  finally
    lReq.Free;
  end;
end;

procedure TJSONRPCServerTest.TestRequestWithParams_I_I_ret_I;
var
  lReq: TJSONRPCRequest;
  lResp: TJSONRPCResponse;
begin
  lReq := TJSONRPCRequest.Create;
  try
    lReq.RequestID := 1234;
    lReq.Method := 'subtract';
    lReq.Params.Add(18);
    lReq.Params.Add(8);
    lResp := FExecutor.ExecuteRequest(lReq);
    try
      Assert.areEqual(10, lResp.Result.AsInteger);
      Assert.areEqual(1234, lResp.RequestID.AsInteger);
    finally
      lResp.Free;
    end;
  finally
    lReq.Free;
  end;
end;

procedure TJSONRPCServerTest.TestRequestWithParams_I_I_ret_A;
var
  lReq: TJSONRPCRequest;
  lRPCResp: TJSONRPCResponse;
  lArr: TJDOJSONArray;
  I: Integer;
  x: Integer;
begin
  lReq := TJSONRPCRequest.Create;
  try
    lReq.Method := 'GetListFromTo';
    lReq.Params.Add(1);
    lReq.Params.Add(5);
    lReq.RequestID := 1234;
    lRPCResp := FExecutor.ExecuteRequest(lReq);
    try
      lArr := TJDOJSONArray(lRPCResp.Result.AsObject);
      x := 1;
      for I := 0 to lArr.Count - 1 do
      begin
        Assert.areEqual(x, lArr[I].IntValue);
        Inc(x);
      end;
    finally
      lRPCResp.Free;
    end;
  finally
    lReq.Free;
  end;
end;

procedure TJSONRPCServerTest.TestRequestWithParams_I_I_I_ret_O;
var
  lReq: TJSONRPCRequest;
  lRPCResp: TJSONRPCResponse;
  lS: string;
begin
  lReq := TJSONRPCRequest.Create;
  try
    // lReq.AsJSONString := '{"jsonrpc": "2.0", "method": "add", "params": [3, 4, 5], "id": 1234}';
    lReq.Method := 'add';
    lReq.Params.Add(3);
    lReq.Params.Add(4);
    lReq.Params.Add(5);
    lReq.RequestID := 1234;
    // lResp := RESTClient.ContentType(TMVCMediaType.APPLICATION_JSON).Accept(TMVCMediaType.APPLICATION_JSON)
    // .doPOST('/jsonrpc', [], lReq.AsJSONString);
    lRPCResp := FExecutor.ExecuteRequest(lReq);
    // Assert.IsNotEmpty(lResp.BodyAsString);
    // Assert.areEqual(HTTP_STATUS.OK, Integer(lResp.ResponseCode));
    // lRPCResp := TJSONRPCResponse.Create;
    try
      // lRPCResp.AsJSONString := lResp.BodyAsString;
      lS := (lRPCResp.Result.AsObject as TJDOJsonObject).ToJSON();
      Assert.areEqual(12, TJDOJsonObject(lRPCResp.Result.AsObject).I['res']);
    finally
      lRPCResp.Free;
    end;
  finally
    lReq.Free;
  end;
end;

procedure TJSONRPCServerTest.TestRequest_S_I_ret_S;
var
  lReq: TJSONRPCRequest;
  lRPCResp: TJSONRPCResponse;
begin
  lReq := TJSONRPCRequest.Create;
  try
    lReq.Method := 'MultiplyString';
    lReq.Params.Add('Daniele');
    lReq.Params.Add(4);
    lReq.RequestID := 1234;
    lRPCResp := FExecutor.ExecuteRequest(lReq);
    // lReq.AsJSONString := '{"jsonrpc": "2.0", "method": "MultiplyString", "params": ["Daniele", 4], "id": 1234}';
    // lResp := RESTClient.ContentType(TMVCMediaType.APPLICATION_JSON).Accept(TMVCMediaType.APPLICATION_JSON)
    // .doPOST('/jsonrpc', [], lReq.AsJSONString);
    // Assert.IsNotEmpty(lResp.BodyAsString);
    // Assert.areEqual(HTTP_STATUS.OK, Integer(lResp.ResponseCode));
    // lRPCResp := TJSONRPCResponse.Create;
    try
      // lRPCResp.AsJSONString := lResp.BodyAsString;
      Assert.areEqual('DanieleDanieleDanieleDaniele', lRPCResp.Result.AsString);
    finally
      lRPCResp.Free;
    end;
  finally
    lReq.Free;
  end;
end;

initialization

TDUnitX.RegisterTestFixture(TServerTest);
TDUnitX.RegisterTestFixture(TJSONRPCServerTest);

end.
