unit LiveServerTestU;

interface

uses
  TestFramework,
  MVCFramework.RESTClient;

type
  TBaseServerTest = class(TTestCase)
  protected
    RESTClient: TRESTClient;
    procedure DoLoginWith(UserName: string);
    procedure DoLogout;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  end;

  TServerTest = class(TBaseServerTest)
  published
    procedure TestReqWithParams;
    procedure TestPOSTWithParamsAndJSONBody;
    // procedure TestPATCHWithParamsAndJSONBody;
    procedure TestPOSTWithObjectJSONBody;
    procedure TestPUTWithParamsAndJSONBody;
    procedure TestCookies;
    procedure TestSession;
    procedure TestInvalidateSession;
    procedure TestAsynchRequestPOST;
    procedure TestAsynchRequestPUT;
    procedure TestAsynchRequestGET;
    procedure TestAsynchRequestDELETE;
    procedure TestEncodingRenderJSONValue;
    procedure TestSerializationType;
    procedure TestProducesConsumes01;
    procedure TestProducesConsumes02;
    procedure TestProducesConsumesWithWrongAcceptHeader;
    procedure TestExceptionInMVCAfterCreate;
    procedure TestExceptionInMVCBeforeDestroy;
    procedure TestMiddlewareSpeedMiddleware;
    procedure TestMiddlewareHandler;
    procedure TestPostAListOfObjects;
    // test authentication/authorization
    procedure TestAuthentication01;
    procedure TestAuthentication02;
    procedure TestAuthentication03;
    procedure TestAuthentication04;
    procedure TestAuthentication05;
    // typed actions
    procedure TestTypedString1;
    procedure TestTypedInteger1;
    procedure TestTypedInt641;
    procedure TestTypedSingle1;
    procedure TestTypedDouble1;
    procedure TestTypedExtended1;
    procedure TestTypedAll;
    procedure TestTypedDateTimeTypes;
    procedure TestTypedBooleans;
  end;

implementation

uses
  System.Math,
{$IF CompilerVersion < 27}
  Data.DBXJSON,
{$ELSE}
  System.JSON,
{$ENDIF}
  MVCFramework.Commons,
  System.SyncObjs,
  System.Generics.Collections,
  System.SysUtils,
  BusinessObjectsU,
  ObjectsMappers,
  Soap.EncdDecd, System.Classes;

{ TServerTest }

procedure TBaseServerTest.DoLogout;
var
  res: IRESTResponse;
begin
  res := RESTClient.doGET('/logout', []);
  CheckTrue(res.ResponseCode = HTTP_STATUS.OK, 'Logout Failed');
end;

procedure TBaseServerTest.SetUp;
begin
  inherited;
  RESTClient := TRESTClient.Create('localhost', 9999);
  RESTClient.ReadTimeout(60 * 1000 * 30);
end;

procedure TBaseServerTest.TearDown;
begin
  inherited;
  RESTClient.Free;
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

    CheckEquals(true, OK);
  finally
    evt.Free;
  end;
end;

procedure TServerTest.TestAsynchRequestGET;
var
  evt: TEvent;
  r: TWaitResult;
  j: TJSONObject;
begin
  j := nil;
  evt := TEvent.Create;
  try
    RESTClient.Asynch(
      procedure(Response: IRESTResponse)
      begin
        try
          j := Response.BodyAsJsonObject.Clone as TJSONObject;
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

    CheckTrue(Assigned(j));
    CheckEquals('1', j.Get('par1').JsonValue.Value);
    j.Free;
  finally
    evt.Free;
  end;
end;

procedure TServerTest.TestAsynchRequestPOST;
var
  evt: TEvent;
  r: TWaitResult;
  j: TJSONObject;
begin
  j := nil;
  evt := TEvent.Create;
  try
    RESTClient.Asynch(
      procedure(Response: IRESTResponse)
      begin
        try
          j := Response.BodyAsJsonObject.Clone as TJSONObject;
        except
          // test should not block...never!
        end;
        evt.SetEvent;
      end,
      procedure(E: Exception)
      begin
      end).doPOST('/echo', ['1', '2', '3'],
      TJSONObject.Create(TJSONPair.Create('from client', 'hello world')), true);

    // wait for thred finish
    repeat
      r := evt.WaitFor(2000);
    until r = TWaitResult.wrSignaled;

    CheckTrue(Assigned(j));
    CheckEquals('from server', j.Get('echo').JsonValue.Value);
    j.Free;
  finally
    evt.Free;
  end;
end;

procedure TServerTest.TestAsynchRequestPUT;
var
  evt: TEvent;
  r: TWaitResult;
  j: TJSONObject;
begin
  j := nil;
  evt := TEvent.Create;
  try
    RESTClient.Asynch(
      procedure(Response: IRESTResponse)
      begin
        try
          j := Response.BodyAsJsonObject.Clone as TJSONObject;
        except
          // test should not block...never!
        end;
        evt.SetEvent;
      end,
      procedure(E: Exception)
      begin
      end).doPUT('/echo', ['1', '2', '3'],
      TJSONObject.Create(TJSONPair.Create('from client', 'hello world')), true);

    // wait for thred finish
    repeat
      r := evt.WaitFor(2000);
    until r = TWaitResult.wrSignaled;

    CheckTrue(Assigned(j));
    CheckEquals('from server', j.Get('echo').JsonValue.Value);
    j.Free;
  finally
    evt.Free;
  end;
end;

procedure TServerTest.TestAuthentication01;
var
  LRes: IRESTResponse;
begin
  RESTClient.Authentication('user1', 'user1');
  CheckEquals('user1', RESTClient.UserName);
  CheckEquals('user1', RESTClient.Password);
  LRes := RESTClient.doGET('/private/role1', []);
  CheckEquals(HTTP_STATUS.OK, LRes.ResponseCode);
end;

procedure TServerTest.TestAuthentication02;
var
  LRes: IRESTResponse;
begin
  RESTClient.UserName := '';
  RESTClient.Password := '';
  RESTClient.UseBasicAuthentication := false;
  LRes := RESTClient.doGET('/private/role1', []);
  CheckEquals(HTTP_STATUS.Unauthorized, LRes.ResponseCode);
end;

procedure TServerTest.TestAuthentication03;
var
  LRes: IRESTResponse;
begin
  RESTClient.UserName := 'user1';
  RESTClient.Password := 'user1';
  RESTClient.UseBasicAuthentication := true;
  LRes := RESTClient.doGET('/private/role2', []);
  CheckEquals(HTTP_STATUS.Forbidden, LRes.ResponseCode);
end;

procedure TServerTest.TestAuthentication04;
var
  LRes: IRESTResponse;
begin
  RESTClient.UserName := 'user1';
  RESTClient.Password := 'user1';
  RESTClient.UseBasicAuthentication := true;
  LRes := RESTClient.doGET('/private/role1', []);
  CheckEquals(HTTP_STATUS.OK, LRes.ResponseCode);
  LRes := RESTClient.doGET('/people', []);
  CheckEquals(HTTP_STATUS.OK, LRes.ResponseCode);
end;

procedure TServerTest.TestAuthentication05;
var
  LRes: IRESTResponse;
begin
  RESTClient.UserName := 'user1';
  RESTClient.Password := 'user1';
  RESTClient.UseBasicAuthentication := true;

  // first
  LRes := RESTClient.doGET('/private/role1session?value=danieleteti', []);
  CheckEquals(HTTP_STATUS.OK, LRes.ResponseCode);
  LRes := RESTClient.doGET('/private/role1session', []);
  CheckEquals(HTTP_STATUS.OK, LRes.ResponseCode);
  CheckEquals('"danieleteti"', LRes.BodyAsString);

  // second
  LRes := RESTClient.doGET('/private/role1session?value=johndoe', []);
  CheckEquals(HTTP_STATUS.OK, LRes.ResponseCode);
  LRes := RESTClient.doGET('/private/role1session', []);
  CheckEquals(HTTP_STATUS.OK, LRes.ResponseCode);
  CheckEquals('"johndoe"', LRes.BodyAsString);
end;

procedure TServerTest.TestCookies;
var
  res: IRESTResponse;
  I: Integer;
begin
  res := RESTClient.doGET('/lotofcookies', []);
  CheckEquals(HTTP_STATUS.OK, res.ResponseCode);
  CheckEquals(4, res.Cookies.Count, 'Wrong number of cookies');
  for I := 0 to 3 do
  begin
    CheckEquals('usersettings' + IntToStr(I + 1),
      res.Cookies.Cookies[I].CookieName);
    CheckEquals('usersettings' + IntToStr(I + 1) + '-value',
      res.Cookies.Cookies[I].Value);
    CheckEquals('/usersettings' + IntToStr(I + 1),
      res.Cookies.Cookies[I].Path);
  end;

end;

procedure TServerTest.TestEncodingRenderJSONValue;
var
  res: IRESTResponse;
  s: string;
begin
  res := RESTClient.doGET('/encoding', []);

  s := res.BodyAsJsonObject.Get('name1').JsonValue.Value;
  CheckEquals('j�rn', s);

  s := res.BodyAsJsonObject.Get('name3').JsonValue.Value;
  CheckEquals('������', s);

  s := res.BodyAsJsonObject.Get('name2').JsonValue.Value;
  CheckEquals('�to je Unicode?', s,
    'If this test fail, check http://qc.embarcadero.com/wc/qcmain.aspx?d=119779');
  { WARNING!!! }
  {
    If this test fail, check
    http://qc.embarcadero.com/wc/qcmain.aspx?d=119779
  }
end;

procedure TServerTest.TestExceptionInMVCAfterCreate;
var
  res: IRESTResponse;
begin
  res := RESTClient.doGET('/exception/aftercreate/nevercalled', []);
  CheckEquals(HTTP_STATUS.InternalServerError, res.ResponseCode);
end;

procedure TServerTest.TestExceptionInMVCBeforeDestroy;
var
  res: IRESTResponse;
begin
  res := RESTClient.doGET('/exception/beforedestroy/nevercalled', []);
  CheckEquals(HTTP_STATUS.InternalServerError, res.ResponseCode);
end;

procedure TServerTest.TestInvalidateSession;
var
  c1: TRESTClient;
  res: IRESTResponse;
begin
  c1 := TRESTClient.Create('localhost', 9999);
  try
    c1.Accept(TMVCMediaType.APPLICATION_JSON);
    c1.doPOST('/session', ['daniele teti']); // imposto un valore in sessione
    res := c1.doGET('/session', []); // rileggo il valore dalla sessione
    CheckEquals('"daniele teti"', res.BodyAsString);
    c1.SessionID := '';
    res := c1.doGET('/session', []); // rileggo il valore dalla sessione
    CheckEquals('""', res.BodyAsString);
  finally
    c1.Free;
  end;
end;

procedure TServerTest.TestMiddlewareHandler;
var
  r: IRESTResponse;
begin
  r := RESTClient.Accept(TMVCMediaType.APPLICATION_JSON)
    .doGET('/handledbymiddleware', []);
  CheckEquals('This is a middleware response', r.BodyAsString);
  CheckEquals(HTTP_STATUS.OK, r.ResponseCode);
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
      .doPOST('/objects', [], Mapper.ObjectToJSONObject(P));
  finally
    P.Free;
  end;

  CheckNotEquals('', r.HeaderValue('request_gen_time'));
end;

// procedure TServerTest.TestPATCHWithParamsAndJSONBody;
// var
// r: IRESTResponse;
// json: TJSONObject;
// begin
// json := TJSONObject.Create;
// json.AddPair('client', 'clientdata');
// r := RESTClient.doPATCH('/echo', ['1', '2', '3'], json);
// CheckEquals('clientdata', r.BodyAsJsonObject.Get('client').JsonValue.Value);
// CheckEquals('from server', r.BodyAsJsonObject.Get('echo').JsonValue.Value);
// end;

procedure TServerTest.TestPostAListOfObjects;
var
  LRes: IRESTResponse;
  LCustomers: TObjectList<TCustomer>;
begin
  LCustomers := TCustomer.GetList;
  try
    LRes := RESTClient.doPOST('/customers/list', [],
      Mapper.ObjectListToJSONArray<TCustomer>(LCustomers));
    CheckEquals(HTTP_STATUS.OK, LRes.ResponseCode);
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
    P.LastName := '������';
    P.DOB := EncodeDate(1979, 1, 1);
    P.Married := true;
    try
      r := RESTClient.Accept(TMVCMediaType.APPLICATION_JSON)
        .doPOST('/objects', [], Mapper.ObjectToJSONObject(P));
    except
      Fail('If this test fail, check http://qc.embarcadero.com/wc/qcmain.aspx?d=119779');
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
  P := Mapper.JSONObjectToObject<TPerson>(r.BodyAsJsonObject);
  try
    CheckEquals('Daniele', P.FirstName);
    CheckEquals('������', P.LastName);
    CheckEquals(true, P.Married);
    CheckEquals(EncodeDate(1979, 1, 1), P.DOB);
  finally
    P.Free;
  end;
end;

procedure TServerTest.TestPOSTWithParamsAndJSONBody;
var
  r: IRESTResponse;
  JSON: TJSONObject;
begin
  JSON := TJSONObject.Create;
  JSON.AddPair('client', 'clientdata');
  r := RESTClient.doPOST('/echo', ['1', '2', '3'], JSON);
  CheckEquals('clientdata', r.BodyAsJsonObject.Get('client').JsonValue.Value);
  CheckEquals('from server', r.BodyAsJsonObject.Get('echo').JsonValue.Value);
end;

procedure TServerTest.TestProducesConsumesWithWrongAcceptHeader;
var
  res: IRESTResponse;
begin
  res := RESTClient.Accept('text/plain')
  // action is waiting for a accept: application/json
    .ContentType('application/json').doPOST('/testconsumes', [],
    TJSONString.Create('Hello World'));
  CheckEquals(HTTP_STATUS.NotFound, res.ResponseCode);
end;

procedure TServerTest.TestProducesConsumes01;
var
  res: IRESTResponse;
begin
  res := RESTClient.Accept('application/json').ContentType('application/json')
    .ContentEncoding('utf-8').doPOST('/testconsumes', [],
    TJSONString.Create('Hello World'));
  CheckEquals(HTTP_STATUS.OK, res.ResponseCode);
  CheckEquals('"Hello World"', res.BodyAsJsonValue.ToString);
  CheckEquals('application/json', res.ContentType);
  CheckEquals('utf-8', res.ContentEncoding);
end;

procedure TServerTest.TestProducesConsumes02;
var
  res: IRESTResponse;
begin
  res := RESTClient.Accept('text/plain').ContentType('text/plain')
    .doPOST('/testconsumes', [], 'Hello World');
  CheckEquals('Hello World', res.BodyAsString);
  CheckEquals('text/plain', res.ContentType);
  CheckEquals('UTF-8', res.ContentEncoding);

  res := RESTClient.Accept('text/plain').ContentType('application/json')
    .doPOST('/testconsumes', [], '{"name": "Daniele"}');
  CheckEquals(HTTP_STATUS.NotFound, res.ResponseCode);
end;

procedure TServerTest.TestPUTWithParamsAndJSONBody;
var
  r: IRESTResponse;
  JSON: TJSONObject;
begin
  JSON := TJSONObject.Create;
  JSON.AddPair('client', 'clientdata');
  r := RESTClient.doPUT('/echo', ['1', '2', '3'], JSON);
  CheckEquals('clientdata', r.BodyAsJsonObject.Get('client').JsonValue.Value);
  CheckEquals('from server', r.BodyAsJsonObject.Get('echo').JsonValue.Value);
end;

procedure TServerTest.TestReqWithParams;
var
  r: IRESTResponse;
  ss: TStringStream;
begin
  r := RESTClient.doGET('/unknownurl/bla/bla', []);

  ss := TStringStream.Create;
  try
    ss.CopyFrom(r.Body, 0);
    CheckEquals(ss.DataString, r.BodyAsString,
      'In case of rotocol error, the body doesn''t contain the same of BodyAsString');
  finally
    ss.Free;
  end;

  CheckEquals(HTTP_STATUS.NotFound, r.ResponseCode, '/unknownurl/bla/bla');

  r := RESTClient.doGET('/req/with/params/', []);
  CheckEquals(HTTP_STATUS.NotFound, r.ResponseCode, '/req/with/params/');

  r := RESTClient.doGET('/req/with/params', []);
  CheckEquals(HTTP_STATUS.NotFound, r.ResponseCode, '/req/with/params');

  r := RESTClient.doGET('/req/with/params', ['1', '2', '3']);
  CheckEquals(HTTP_STATUS.OK, r.ResponseCode);
  CheckEquals('1', r.BodyAsJsonObject.Get('par1').JsonValue.Value);
  CheckEquals('2', r.BodyAsJsonObject.Get('par2').JsonValue.Value);
  CheckEquals('3', r.BodyAsJsonObject.Get('par3').JsonValue.Value);
  CheckEquals('GET', r.BodyAsJsonObject.Get('method').JsonValue.Value);

  r := RESTClient.doPOST('/req/with/params', ['1', '2', '3']);
  CheckEquals(HTTP_STATUS.NotFound, r.ResponseCode);

  r := RESTClient.doPUT('/req/with/params', ['1', '2', '3']);
  CheckEquals(HTTP_STATUS.NotFound, r.ResponseCode);

  r := RESTClient.doDELETE('/req/with/params', ['1', '2', '3']);
  CheckEquals(HTTP_STATUS.OK, r.ResponseCode);
  CheckNull(r.BodyAsJsonObject);
end;

procedure TServerTest.TestSerializationType;
var
  LResp: IRESTResponse;
  LPersonProps, LPersonFlds: TPerson;
  LObj: TObject;
begin
  LResp := RESTClient.doGET('/people', ['1']);
  LPersonProps := Mapper.JSONObjectToObject<TPerson>(LResp.BodyAsJsonObject);
  try
    LResp := RESTClient.doGET('/people', ['1', 'asfields']);
    LObj := Mapper.JSONObjectFieldsToObject(LResp.BodyAsJsonObject);
    try
      CheckEquals('BusinessObjectsU.TPerson', LObj.QualifiedClassName);
      LPersonFlds := TPerson(LObj);
      CheckTrue(LPersonFlds.Equals(LPersonProps),
        'Object tranferred using field serialization is different from the object serialized in the default way');
    finally
      LObj.Free;
    end;
  finally
    LPersonProps.Free;
  end;
end;

procedure TServerTest.TestSession;
var
  c1: TRESTClient;
  res: IRESTResponse;
begin
  c1 := TRESTClient.Create('localhost', 9999);
  try
    c1.Accept(TMVCMediaType.APPLICATION_JSON);
    c1.doPOST('/session', ['daniele teti']); // imposto un valore in sessione
    res := c1.doGET('/session', []); // rileggo il valore dalla sessione
    CheckEquals('"daniele teti"', res.BodyAsString);
    c1.Accept(TMVCMediaType.TEXT_PLAIN);
    res := c1.doGET('/session', []);
    // rileggo il valore dalla sessione
    CheckEquals('daniele teti', res.BodyAsString);

    // aggiungo altri cookies
    res := c1.doGET('/lotofcookies', []); // rileggo il valore dalla sessione
    CheckEquals(HTTP_STATUS.OK, res.ResponseCode);
    c1.Accept(TMVCMediaType.TEXT_PLAIN);
    res := c1.doGET('/session', []); // rileggo il valore dalla sessione
    CheckEquals('daniele teti', res.BodyAsString);
  finally
    c1.Free;
  end;
end;

procedure TServerTest.TestTypedAll;
var
  res: IRESTResponse;
  lJObj: TJSONObject;
begin
  // ----------------------'/typed/all/($ParString)/($ParInteger)/($ParInt64)/($ParSingle)/($ParDouble)/($ParExtended)')', []);
  res := RESTClient.doGET('/typed/all/mystring/1234/12345678/12.3/1234.5678/1234.5678', []);
  CheckTrue(res.ResponseCode = HTTP_STATUS.OK, 'Cannot route');
  lJObj := res.BodyAsJsonObject;
  CheckEquals('mystring', lJObj.GetValue('ParString').Value, 'ParString');
  CheckEquals(1234, TJSONNumber(lJObj.GetValue('ParInteger')).AsInt, 'ParInteger');
  CheckEquals(12345678, TJSONNumber(lJObj.GetValue('ParInt64')).AsInt64, 'ParInt64');
  CheckEquals(12.3, RoundTo(TJSONNumber(lJObj.GetValue('ParSingle')).AsDouble, -1), 'ParSingle');
  CheckEquals(1234.5678, RoundTo(TJSONNumber(lJObj.GetValue('ParDouble')).AsDouble, -4),
    'ParDouble');
  CheckEquals(1234.5678, RoundTo(TJSONNumber(lJObj.GetValue('ParExtended')).AsDouble, -4),
    'ParExtended');
end;

procedure TServerTest.TestTypedBooleans;
var
  res: IRESTResponse;
begin
  res := RESTClient.doGET('/typed/booleans/true/false/1/0', []);
  CheckTrue(res.ResponseCode = HTTP_STATUS.OK, 'Cannot route');
  CheckEquals('true.false.true.false', res.BodyAsString.ToLower);
end;

procedure TServerTest.TestTypedDouble1;
var
  res: IRESTResponse;
begin
  res := RESTClient.doGET('/typed/double1/1234.5678', []);
  CheckTrue(res.ResponseCode = HTTP_STATUS.OK, 'Cannot route');
  CheckEquals('1234.5678 modified from server', res.BodyAsString);

end;

procedure TServerTest.TestTypedExtended1;
var
  res: IRESTResponse;
begin
  res := RESTClient.doGET('/typed/extended1/1234.5678', []);
  CheckTrue(res.ResponseCode = HTTP_STATUS.OK, 'Cannot route');
  CheckEquals('1234.5678 modified from server', res.BodyAsString);

end;

procedure TServerTest.TestTypedInt641;
var
  res: IRESTResponse;
begin
  res := RESTClient.doGET('/typed/int641/12345678', []);
  CheckTrue(res.ResponseCode = HTTP_STATUS.OK, 'Cannot route');
  CheckEquals('12345678 modified from server', res.BodyAsString);
end;

procedure TServerTest.TestTypedInteger1;
var
  res: IRESTResponse;
begin
  res := RESTClient.doGET('/typed/integer1/1234', []);
  CheckTrue(res.ResponseCode = HTTP_STATUS.OK, 'Cannot route');
  CheckEquals('1234 modified from server', res.BodyAsString);
end;

procedure TServerTest.TestTypedSingle1;
var
  res: IRESTResponse;
begin
  res := RESTClient.doGET('/typed/single1/1234.5', []);
  CheckTrue(res.ResponseCode = HTTP_STATUS.OK, 'Cannot route');
  CheckEquals('1234.5 modified from server', res.BodyAsString);

end;

procedure TServerTest.TestTypedString1;
var
  res: IRESTResponse;
begin
  res := RESTClient.doGET('/typed/string1/daniele', []);
  CheckTrue(res.ResponseCode = HTTP_STATUS.OK, 'Cannot route');
  CheckEquals('daniele modified from server', res.BodyAsString);
end;

procedure TServerTest.TestTypedDateTimeTypes;
var
  res: IRESTResponse;
begin
  // TDate, wrong and correct
  res := RESTClient.doGET('/typed/tdate1/20161012', []);
  CheckEquals(HTTP_STATUS.InternalServerError, res.ResponseCode, 'wrong TDate');

  res := RESTClient.doGET('/typed/tdate1/2016-10-12', []);
  CheckEquals(HTTP_STATUS.OK, res.ResponseCode);
  CheckEquals('2016-10-12 modified from server', res.BodyAsString);

  // TDateTime, wrong and correct
  res := RESTClient.doGET('/typed/tdatetime1/20161012121212', []);
  CheckEquals(HTTP_STATUS.InternalServerError, res.ResponseCode, 'wrong TDateTime');

  res := RESTClient.doGET('/typed/tdatetime1/2016-10-12 12:12:12', []);
  CheckEquals(HTTP_STATUS.OK, res.ResponseCode);
  CheckEquals('2016-10-12 12:12:12 modified from server', res.BodyAsString);

  // TTime, wrong and correct
  res := RESTClient.doGET('/typed/ttime1/121212', []);
  CheckEquals(HTTP_STATUS.InternalServerError, res.ResponseCode, 'wrong TTime');

  res := RESTClient.doGET('/typed/ttime1/12:12:12', []);
  CheckEquals(HTTP_STATUS.OK, res.ResponseCode);
  CheckEquals('12:12:12 modified from server', res.BodyAsString);

end;

procedure TBaseServerTest.DoLoginWith(UserName: string);
var
  res: IRESTResponse;
begin
  res := RESTClient.doGET('/login', [UserName]);
  CheckTrue(res.ResponseCode = HTTP_STATUS.OK, 'Login Failed');
end;

initialization

RegisterTest(TServerTest.Suite);

end.
