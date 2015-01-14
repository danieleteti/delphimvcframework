unit RESTAdapterTestsU;

interface

uses
  MVCFramework.RESTAdapter, TestFramework, BusinessObjectsU,
  Generics.Collections,
{$IF CompilerVersion < 27}
  Data.DBXJSON,
  Data.SqlExpr,
  DBXCommon,
{$ELSE}
  System.JSON,
{$ENDIF}
  ObjectsMappers, MVCFramework.RESTClient, MVCFramework;

type

  [Headers('User-Agent', 'RESTAdapter-Test')]
  ITESTService = interface(IInvokable)
    ['{58B9FA23-92F4-4B8E-814B-05232F32A41F}']

    [RESTResource(HttpGet, '/persons')]
    [MapperListOf(TPerson)]
    function GetListPerson: TObjectList<TPerson>;

    [RESTResource(HttpGet, '/persons/1')]
    function GetTonyStark: TPerson;

    [RESTResource(HttpGet, '/persons')]
    function GetPersonByID([Param] APersonID: integer): TPerson;

    [RESTResource(httpPOST, '/persons')]
    function SendPerson([Body] ABody: string): TPerson;

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

  TTestRESTAdapter = class(TTestCase)
  private
    RESTAdapter: TRESTAdapter<ITESTService>;
    TESTService: ITESTService;
  protected
    procedure SetUp; override;
  published
    procedure TestGetListPerson;
    procedure TestGetTonyStark;
    procedure TestPostPerson;
    procedure TestGetPersonByID;
    procedure TestHeadersApplicationJSON;
    procedure TestHeadersTextPlain;
    procedure TestApplicationJSONWithHeaderTextPlain;
  end;

implementation

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
  Person := TESTService.GetPersonByID(4);
  try
    CheckEquals('Tony', Person.FirstName);
    CheckEquals('Stark', Person.LastName);
    CheckTrue(Person.Married);
  finally
    Person.Free;
  end;
end;

procedure TTestRESTAdapter.TestGetTonyStark;
var
  Person: TPerson;
begin;
  Person := TESTService.GetTonyStark;
  try
    CheckEquals('Tony', Person.FirstName);
    CheckEquals('Stark', Person.LastName);
    CheckTrue(Person.Married);
  finally
    Person.Free;
  end;
end;

procedure TTestRESTAdapter.TestHeadersApplicationJSON;
var
  Res: TJSONObject;
begin
  Res := TESTService.HeadersApplicationJSON as TJSONObject;
  try
    CheckEquals('Hello World', Res.GetValue('key').Value);
  finally
    Res.Free;
  end;
end;

procedure TTestRESTAdapter.TestHeadersTextPlain;
var
  Res: string;
begin
  Res := TESTService.HeadersTextPlain;
  CheckEquals('Hello World', Res);
end;

procedure TTestRESTAdapter.TestPostPerson;
var
  Person: TPerson;
  RetPerson: TPerson;
begin
  Person := TPerson.GetNew('Peter', 'Parker', 0, false);
  try
    RetPerson := TESTService.SendPerson('person');
    try
      CheckEquals('Peter', RetPerson.FirstName);
      CheckEquals('Parker', RetPerson.LastName);
      CheckFalse(RetPerson.Married);
    finally
      RetPerson.Free;
    end;
  finally
    Person.Free;
  end;
end;

procedure TTestRESTAdapter.TestApplicationJSONWithHeaderTextPlain;
var
  Resp: IRESTResponse;
begin
  // expected 404 because is not consumed text/plain
  Resp := TESTService.ApplicationJSONWithTextPlainHeader;
  CheckEquals(404, Resp.ResponseCode);
end;

procedure TTestRESTAdapter.TestGetListPerson;
var
  ListPerson: TObjectList<TPerson>;
begin
  ListPerson := TESTService.GetListPerson;
  try
    CheckTrue(ListPerson.Count > 0);
    CheckEquals('Tony', ListPerson[0].FirstName);
    CheckEquals('Stark', ListPerson[0].LastName);
  finally
    ListPerson.Free;
  end;
end;

initialization

RegisterTest(TTestRESTAdapter.suite);

finalization

end.
