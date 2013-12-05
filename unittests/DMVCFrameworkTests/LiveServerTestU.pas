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
    procedure TestPOSTWithObjectJSONBody;
    procedure TestPUTWithParamsAndJSONBody;
    procedure TestSession;
    procedure TestAsynchRequestPOST;
    procedure TestAsynchRequestPUT;
    procedure TestAsynchRequestGET;
    procedure TestAsynchRequestDELETE;
    procedure TestEncodingRenderJSONValue;

    procedure TestProducesConsumes01;
    procedure TestProducesConsumes02;
    procedure TestExceptionInMVCAfterCreate;
    procedure TestExceptionInMVCBeforeDestroy;
  end;

implementation

uses
  Data.DBXJSON,
  MVCFramework.Commons,
  System.SyncObjs,
  System.SysUtils, BusinessObjectsU, ObjectsMappers;

{ TServerTest }

procedure TBaseServerTest.DoLogout;
var
  res: IRESTResponse;
begin
  res := RESTClient.doGET('/logout', []);
  CheckTrue(res.ResponseCode = 200, 'Logout Failed');
end;

procedure TBaseServerTest.SetUp;
begin
  inherited;
  RESTClient := TRESTClient.Create('localhost', 8888);
  RESTClient.ReadTimeout := 60 * 1000 * 30;
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
  ok: boolean;
begin
  ok := false;
  evt := TEvent.Create;
  try
    RESTClient.Asynch(
      procedure(Response: IRESTResponse)
      begin
        ok := true;
        evt.SetEvent;
      end,
      procedure(E: Exception)
      begin
        ok := false;
      end).doDELETE('/req/with/params', ['1', '2', '3']);

    // wait for thred finish
    repeat
      r := evt.WaitFor(2000);
    until r = TWaitResult.wrSignaled;

    CheckEquals(true, ok);
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

procedure TServerTest.TestEncodingRenderJSONValue;
var
  res: IRESTResponse;
begin
  res := RESTClient.doGET('/encoding', []);
  CheckEquals('jørn', res.BodyAsJsonObject.Get('name1').JsonValue.Value);
  CheckEquals('Što je Unicode?', res.BodyAsJsonObject.Get('name2')
    .JsonValue.Value);
  CheckEquals('àèéìòù', res.BodyAsJsonObject.Get('name3').JsonValue.Value);
end;

procedure TServerTest.TestExceptionInMVCAfterCreate;
var
  res: IRESTResponse;
begin
  res := RESTClient.doGET('/exception/aftercreate/nevercalled', []);
  CheckEquals(500, res.ResponseCode);
end;

procedure TServerTest.TestExceptionInMVCBeforeDestroy;
var
  res: IRESTResponse;
begin
  res := RESTClient.doGET('/exception/beforedestroy/nevercalled', []);
  CheckEquals(500, res.ResponseCode);
end;

procedure TServerTest.TestPOSTWithObjectJSONBody;
var
  r: IRESTResponse;
  json: TJSONObject;
  P: TPerson;
begin
  P := TPerson.Create;
  try
    P.FirstName := 'Daniele';
    P.LastName := 'àòùèéì';
    P.DOB := EncodeDate(1979, 1, 1);
    P.Married := true;
    r := RESTClient.Accept(TMVCMimeType.APPLICATION_JSON).doPOST('/objects', [], mapper.ObjectToJSONObject(P));
  finally
    P.Free;
  end;
  P := mapper.JSONObjectToObject<TPerson>(r.BodyAsJsonObject);
  try
    CheckEquals('Daniele', P.FirstName);
    CheckEquals('àòùèéì', P.LastName);
    CheckEquals(true, P.Married);
    CheckEquals(EncodeDate(1979, 1, 1), P.DOB);
  finally
    P.Free;
  end;
end;

procedure TServerTest.TestPOSTWithParamsAndJSONBody;
var
  r: IRESTResponse;
  json: TJSONObject;
begin
  json := TJSONObject.Create;
  json.AddPair('client', 'clientdata');
  r := RESTClient.doPOST('/echo', ['1', '2', '3'], json);
  CheckEquals('clientdata', r.BodyAsJsonObject.Get('client').JsonValue.Value);
  CheckEquals('from server', r.BodyAsJsonObject.Get('echo').JsonValue.Value);
end;

procedure TServerTest.TestProducesConsumes01;
var
  res: IRESTResponse;
begin
  res := RESTClient.doPOST('/testconsumes', [],
    TJSONString.Create('Hello World'));
  CheckEquals(200, res.ResponseCode);
  CheckEquals('"Hello World"', res.BodyAsJsonValue.ToString);
  CheckEquals('application/json', res.GetContentType);
  CheckEquals('UTF-8', res.GetContentEncoding);
end;

procedure TServerTest.TestProducesConsumes02;
var
  res: IRESTResponse;
begin
  res := RESTClient.
    Accept('text/plain').
    doPOST('/testconsumes', [], 'Hello World');
  CheckEquals('Hello World', res.BodyAsString);
  CheckEquals('text/plain', res.GetContentType);
  CheckEquals('UTF-8', res.GetContentEncoding);
end;

procedure TServerTest.TestPUTWithParamsAndJSONBody;
var
  r: IRESTResponse;
  json: TJSONObject;
begin
  json := TJSONObject.Create;
  json.AddPair('client', 'clientdata');
  r := RESTClient.doPUT('/echo', ['1', '2', '3'], json);
  CheckEquals('clientdata', r.BodyAsJsonObject.Get('client').JsonValue.Value);
  CheckEquals('from server', r.BodyAsJsonObject.Get('echo').JsonValue.Value);
end;

procedure TServerTest.TestReqWithParams;
var
  r: IRESTResponse;
begin
  r := RESTClient.doGET('/unknownurl/bla/bla', []);
  CheckEquals(404, r.ResponseCode, '/unknownurl/bla/bla');

  r := RESTClient.doGET('/req/with/params/', []);
  CheckEquals(404, r.ResponseCode, '/req/with/params/');

  r := RESTClient.doGET('/req/with/params', []);
  CheckEquals(404, r.ResponseCode, '/req/with/params');

  r := RESTClient.doGET('/req/with/params', ['1', '2', '3']);
  CheckEquals(200, r.ResponseCode);
  CheckEquals('1', r.BodyAsJsonObject.Get('par1').JsonValue.Value);
  CheckEquals('2', r.BodyAsJsonObject.Get('par2').JsonValue.Value);
  CheckEquals('3', r.BodyAsJsonObject.Get('par3').JsonValue.Value);
  CheckEquals('GET', r.BodyAsJsonObject.Get('method').JsonValue.Value);

  r := RESTClient.doPOST('/req/with/params', ['1', '2', '3']);
  CheckEquals(404, r.ResponseCode);

  r := RESTClient.doPUT('/req/with/params', ['1', '2', '3']);
  CheckEquals(404, r.ResponseCode);

  r := RESTClient.doDELETE('/req/with/params', ['1', '2', '3']);
  CheckEquals(200, r.ResponseCode);
  CheckNull(r.BodyAsJsonObject);
end;

procedure TServerTest.TestSession;
var
  c1: TRESTClient;
  res: IRESTResponse;
begin
  c1 := TRESTClient.Create('localhost', 8888);
  try
    c1.Accept(TMVCMimeType.APPLICATION_JSON);
    c1.doPOST('/session', ['daniele teti']); // imposto un valore in sessione
    res := c1.doGET('/session', []); // rileggo il valore dalla sessione
    CheckEquals('"daniele teti"', res.BodyAsString);
    c1.Accept(TMVCMimeType.TEXT_PLAIN);
    res := c1.doGET('/session', []);
    // rileggo il valore dalla sessione
    CheckEquals('daniele teti', res.BodyAsString);

    // aggiungo altri cookies
    res := c1.doGET('/lotofcookies', []); // rileggo il valore dalla sessione
    CheckEquals(200, res.ResponseCode);
    c1.Accept(TMVCMimeType.TEXT_PLAIN);
    res := c1.doGET('/session', []); // rileggo il valore dalla sessione
    CheckEquals('daniele teti', res.BodyAsString);
  finally
    c1.Free;
  end;
end;

procedure TBaseServerTest.DoLoginWith(UserName: string);
var
  P: TJSONObject;
  res: IRESTResponse;
begin
  res := RESTClient.doGET('/login', [UserName]);
  CheckTrue(res.ResponseCode = 200, 'Login Failed');
end;

initialization

RegisterTest(TServerTest.Suite);

end.
