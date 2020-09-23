unit MVCFramework.Tests.RESTClient;

interface

uses
  DUnitX.TestFramework,
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  MVCFramework,
  MVCFramework.Server,
  MVCFramework.Server.Impl,
  MVCFramework.RESTClient,
  MVCFramework.RESTAdapter,
  MVCFramework.Commons,
  MVCFramework.Serializer.Defaults,
  MVCFramework.Serializer.Commons,
  MVCFramework.Tests.AppController,
  MVCFramework.RESTClient.Intf;

type

  IAppResource = interface(IInvokable)
    ['{D139CD79-CFE5-49E3-8CFB-27686621911B}']

    [RESTResource(TMVCHTTPMethodType.httpGET, '/hello')]
    function HelloWorld(): string;

    [RESTResource(TMVCHTTPMethodType.httpGET, '/user')]
    function GetUser(): TAppUser;

    [RESTResource(TMVCHTTPMethodType.httpPOST, '/user/save')]
    procedure PostUser([Body] pBody: TAppUser);

    [RESTResource(TMVCHTTPMethodType.httpGET, '/users')]
    [MVCListOf(TAppUser)]
    function GetUsers(): TObjectList<TAppUser>;

    [RESTResource(TMVCHTTPMethodType.httpPOST, '/users/save')]
    [MVCListOf(TAppUser)]
    procedure PostUsers([Body] pBody: TObjectList<TAppUser>);
  end;

  [TestFixture]
  TTestRESTClient = class(TObject)
  strict private
    FServerListener: IMVCListener;
    FRESTClient: IMVCRESTClient;
    FRESTAdapter: TRESTAdapter<IAppResource>;
    FAppResource: IAppResource;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestCreateAndDestroy();
    [Test]
    procedure TestInformation();
    [Test]
    procedure TestHelloWorld();
    [Test]
    procedure TestGetUser();
    [Test]
    procedure TestPostUser();
    [Test]
    procedure TestPostUsers();
    [Test]
    procedure TestGetUsers();
    [Test]
    procedure TestUploadFile();
    [Test]
    procedure TestBodyURLEncoded();
  end;

implementation

uses
  MVCFramework.Tests.WebModule1,
  LiveServerTestU,
  MVCFramework.SystemJSONUtils,
  System.JSON,
  System.IOUtils,
  JsonDataObjects;

{ TTestRESTClient }

procedure TTestRESTClient.SetUp;
begin
  inherited;
  FServerListener := TMVCListener.Create(TMVCListenerProperties.New
    .SetName('AppServer')
    .SetPort(3000)
    .SetMaxConnections(1024)
    .SetWebModuleClass(TestWebModuleClass)
    );
  FServerListener.Start;

  FRESTClient := TMVCRESTClient.New.BaseURL('localhost', 3000);

  FRESTAdapter := TRESTAdapter<IAppResource>.Create;
  FRESTAdapter.Build(FRESTClient);

  FAppResource := FRESTAdapter.ResourcesService;
end;

procedure TTestRESTClient.TearDown;
begin
  inherited;
  FServerListener.Stop;
  FRESTClient := nil;
end;

procedure TTestRESTClient.TestBodyURLEncoded;
var
  lResp: IMVCRESTResponse;
  lJsonResp: TJDOJsonObject;
begin
  lResp := FRESTClient
    .SetBasicAuthorizationHeader('dmvc', '123')
    .AddBodyFieldURLEncoded('field1', 'value1')
    .AddBodyFieldURLEncoded('field2', 'Jo„o AntÙnio')
    .AddBodyFieldURLEncoded('field3', 'Special characters: ˆ¸·‡Á„ı∫s')
    .Post('/body-url-encoded');

  Assert.AreEqual(lResp.StatusCode, 200);

  lJsonResp := TJDOJsonBaseObject.Parse(lResp.Content) as TJDOJsonObject;
  try
    Assert.AreEqual('value1', lJsonResp.S['field1']);
    Assert.AreEqual('Jo„o AntÙnio', lJsonResp.S['field2']);
    Assert.AreEqual('Special characters: ˆ¸·‡Á„ı∫s', lJsonResp.S['field3']);
  finally
    lJsonResp.Free;
  end;
end;

procedure TTestRESTClient.TestCreateAndDestroy;
var
  LClient: IMVCRESTClient;
begin
  LClient := TMVCRESTClient.New.BaseURL('', 80);
  Assert.IsTrue(LClient <> nil);
end;

procedure TTestRESTClient.TestGetUser;
var
  LUser: TAppUser;
  LResp: IMVCRESTResponse;
begin
  FRESTClient.Resource('/user');
  FRESTClient.SetBasicAuthorizationHeader('dmvc', '123');

  // String
  LResp := FRESTClient.Get;
  Assert.IsTrue(
    ('{"Cod":1,"Name":"Ezequiel","Pass":"123"}' = LResp.Content) and
    (LResp.StatusCode = 200)
    );

  // Object
  // lJObj := TSystemJSON.BodyAsJSONObject(FRESTClient.doGET);

  LUser := TAppUser.Create; // TSystemJSON.BodyAsJSONObject(FRESTClient.doGET).BodyAsJSONObject.AsObject<TAppUser>();
  try
    GetDefaultSerializer.DeserializeObject(FRESTClient.Get.Content, LUser);
    Assert.IsTrue((LUser <> nil) and (LUser.Cod > 0));
  finally
    FreeAndNil(LUser);
  end;

  // Adapter
  LUser := FAppResource.GetUser;
  try
    Assert.IsTrue((LUser <> nil) and (LUser.Cod > 0));
  finally
    FreeAndNil(LUser);
  end;
end;

procedure TTestRESTClient.TestGetUsers;
var
  LUsers: TObjectList<TAppUser>;
  lBody: string;
begin
  FRESTClient.Resource('/users');
  FRESTClient.SetBasicAuthorizationHeader('dmvc', '123');

  lBody := FRESTClient.Get.Content;
  // String
  Assert.AreEqual('[{"Cod":0,"Name":"Ezequiel 0","Pass":"0"},{"Cod":1,"Name":"Ezequiel 1","Pass":"1"},' +
    '{"Cod":2,"Name":"Ezequiel 2","Pass":"2"},{"Cod":3,"Name":"Ezequiel 3","Pass":"3"},{"Cod":4,"Name":"Ezequiel 4","Pass":"4"},' +
    '{"Cod":5,"Name":"Ezequiel 5","Pass":"5"},{"Cod":6,"Name":"Ezequiel 6","Pass":"6"},{"Cod":7,"Name":"Ezequiel 7","Pass":"7"},' +
    '{"Cod":8,"Name":"Ezequiel 8","Pass":"8"},{"Cod":9,"Name":"Ezequiel 9","Pass":"9"},{"Cod":10,"Name":"Ezequiel 10","Pass":"10"}]',
    lBody);

  // Objects
  LUsers := TObjectList<TAppUser>.Create(True);
  try
    GetDefaultSerializer.DeserializeCollection(lBody, lUsers, TAppUser); // BodyAsJSONArray.AsObjectList<TAppUser>;
    LUsers.OwnsObjects := True;
    Assert.IsTrue(LUsers.Count > 0);
  finally
    FreeAndNil(LUsers);
  end;

  // Adapter
  LUsers := FAppResource.GetUsers;
  try
    LUsers.OwnsObjects := True;
    Assert.IsTrue(LUsers.Count > 0);
  finally
    FreeAndNil(LUsers);
  end;
end;

procedure TTestRESTClient.TestHelloWorld;
begin
  FRESTClient.Resource('/hello');
  FRESTClient.SetBasicAuthorizationHeader('dmvc', '123');

  // String
  Assert.AreEqual('Hello World called with GET', FRESTClient.Get.Content);

  // Adapter
  Assert.AreEqual('Hello World called with GET', FAppResource.HelloWorld);
end;

procedure TTestRESTClient.TestInformation;
const
 JWT_TOKEN =
   'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.'+
   'eyJzdWIiOiIxMjMiLCJuYW1lIjoiVXNlciIsImlhdCI6MTUxNjIzOTAyMn0.'+
   'EdtbBz7jOucEVf5AV-wD_NwqlJzoCdZKYmMa6p54PVY';

var
  LClient: IMVCRESTClient;
begin
  LClient := TMVCRESTClient.New.BaseURL('localhost', 8080);
  LClient
    .ReadTimeOut(100)
    .ConnectTimeout(100)
    .SetBasicAuthorizationHeader('dmvc', 'dmvc')
    .Accept(TMVCMediaType.APPLICATION_JSON)
    .AcceptCharSet(TMVCCharset.UTF_8)
    .AcceptEncoding('gzip,deflate')
    .UserAgent('DMVCFramework RESTClient')
    .SetBasicAuthorizationHeader('username', 'password');

  Assert.AreEqual('http://localhost:8080', LClient.BaseURL);
  Assert.AreEqual(100, LClient.ReadTimeOut);
  Assert.AreEqual(100, LClient.ConnectTimeout);
  Assert.AreEqual('application/json', LClient.Accept);
  Assert.AreEqual('UTF-8', LClient.AcceptCharset );
  Assert.AreEqual('gzip,deflate', LClient.AcceptEncoding);
  Assert.AreEqual('DMVCFramework RESTClient', LClient.UserAgent);
  Assert.AreEqual('Basic dXNlcm5hbWU6cGFzc3dvcmQ=', LClient.HeaderValue('Authorization'));

  LClient.SetBearerAuthorizationHeader(JWT_TOKEN);

  Assert.AreEqual('Bearer ' + JWT_TOKEN, LClient.HeaderValue('Authorization'));
end;

procedure TTestRESTClient.TestPostUser;
var
  LUser: TAppUser;
  LResp: IMVCRESTResponse;
begin
  FRESTClient.Resource('/user/save');
  FRESTClient.SetBasicAuthorizationHeader('dmvc', '123');

  LUser := TAppUser.Create;
  LUser.Cod := 1;
  LUser.Name := 'Ezequiel';
  LUser.Pass := '123';
  LResp := FRESTClient.AddBody(LUser).Post;
  Assert.IsTrue(('Success!' = LResp.Content) and (LResp.StatusCode = 200));

  // Adapter
  LUser := TAppUser.Create;
  LUser.Cod := 1;
  LUser.Name := 'Ezequiel';
  LUser.Pass := '123';
  FAppResource.PostUser(LUser);
end;

procedure TTestRESTClient.TestPostUsers;
var
  LUsers: TObjectList<TAppUser>;
  LResp: IMVCRESTResponse;
  I: Integer;
  LUser: TAppUser;
begin
  FRESTClient.Resource('/users/save');
  FRESTClient.SetBasicAuthorizationHeader('dmvc', '123');
  FRESTClient.Accept('application/json;charset=utf-8');

  LUsers := TObjectList<TAppUser>.Create(True);
  try
    for I := 0 to 10 do
    begin
      LUser := TAppUser.Create;
      LUser.Cod := I;
      LUser.Name := 'Ezequiel ˆ¸·‡Á„ı∫s ' + IntToStr(I);
      LUser.Pass := IntToStr(I);
      LUsers.Add(LUser);
    end;
    LResp := FRESTClient.AddBody(LUsers, False).Post;
  finally
    LUsers.Free;
  end;
  Assert.IsTrue(('Success!' = LResp.Content) and (LResp.StatusCode = 200));

  // Adapter
  LUsers := TObjectList<TAppUser>.Create(True);
  for I := 0 to 10 do
  begin
    LUser := TAppUser.Create;
    LUser.Cod := I;
    LUser.Name := 'Ezequiel ˆ¸·‡Á„ı∫s ' + IntToStr(I);
    LUser.Pass := IntToStr(I);
    LUsers.Add(LUser);
  end;
  FAppResource.PostUsers(LUsers);
end;

procedure TTestRESTClient.TestUploadFile;
const
  TEXT_SAMPLE = 'This is a simple text for testing RESTClient file uploads.';
  TEXT_SAMPLE_MD5 = '2ba880c91bb822859595a0efa25666a5';
var
  lStringFile: TStringStream;
  lFilename: string;
  lResp: IMVCRESTResponse;
begin
  lFilename := ExtractFilePath(ParamStr(0)) + 'file_upload_sample.txt';
  if TFile.Exists(lFilename) then
    TFile.Delete(lFilename);

  lStringFile := TStringStream.Create(TEXT_SAMPLE);
  try
    lStringFile.SaveToFile(lFileName);
  finally
    LStringFile.Free;
  end;

  lResp := FRESTClient
    .SetBasicAuthorizationHeader('dmvc', '123')
    .AddFile(lFileName)
    .Post('/file/upload');

  Assert.AreEqual(lResp.StatusCode, 200);
  Assert.AreEqual(lResp.Content, TEXT_SAMPLE_MD5);
end;

initialization

TDUnitX.RegisterTestFixture(TTestRESTClient);

end.
