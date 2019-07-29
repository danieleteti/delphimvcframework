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
  MVCFramework.Tests.AppController;

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
    FRESTClient: TRESTClient;
    FRESTAdapter: TRESTAdapter<IAppResource>;
    FAppResource: IAppResource;
  public
    [SetUp]
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
  end;

implementation

uses
  MVCFramework.Tests.WebModule1,
  MVCFramework.SystemJSONUtils,
  System.JSON;

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

  FRESTClient := TRESTClient.Create('localhost', 3000);

  FRESTAdapter := TRESTAdapter<IAppResource>.Create;
  FRESTAdapter.Build(FRESTClient);

  FAppResource := FRESTAdapter.ResourcesService;
end;

procedure TTestRESTClient.TearDown;
begin
  inherited;
  FServerListener.Stop;
  FRESTClient.Free;
end;

procedure TTestRESTClient.TestCreateAndDestroy;
var
  LClient: TRESTClient;
begin
  LClient := TRESTClient.Create('', 80, nil);
  Assert.IsTrue(LClient <> nil);
  FreeAndNil(LClient);
  Assert.IsTrue(LClient = nil);
end;

procedure TTestRESTClient.TestGetUser;
var
  LUser: TAppUser;
  LResp: IRESTResponse;
begin
  FRESTClient.Resource('/user').Params([]);
  FRESTClient.Authentication('dmvc', '123');

  // String
  LResp := FRESTClient.doGET;
  Assert.IsTrue(
    ('{"Cod":1,"Name":"Ezequiel","Pass":"123"}' = LResp.BodyAsString) and
    (LResp.ResponseCode = 200)
    );

  // Object
  // lJObj := TSystemJSON.BodyAsJSONObject(FRESTClient.doGET);

  LUser := TAppUser.Create; // TSystemJSON.BodyAsJSONObject(FRESTClient.doGET).BodyAsJSONObject.AsObject<TAppUser>();
  try
    GetDefaultSerializer.DeserializeObject(FRESTClient.doGET.BodyAsString, LUser);
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
  FRESTClient.Resource('/users').Params([]);
  FRESTClient.Authentication('dmvc', '123');

  lBody := FRESTClient.doGET.BodyAsString;
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
  FRESTClient.Resource('/hello').Params([]);
  FRESTClient.Authentication('dmvc', '123');

  // String
  Assert.AreEqual('Hello World called with GET', FRESTClient.doGET.BodyAsString);

  // Adapter
  Assert.AreEqual('Hello World called with GET', FAppResource.HelloWorld);
end;

procedure TTestRESTClient.TestInformation;
var
  LClient: TRESTClient;
begin
  LClient := TRESTClient.Create('', 80, nil);
  LClient
    .ReadTimeOut(100)
    .ConnectionTimeOut(100)
    .Authentication('dmvc', 'dmvc', True)
    .Accept(TMVCMediaType.APPLICATION_JSON)
    .AcceptCharSet(TMVCCharset.UTF_8)
    .ContentType(TMVCMediaType.APPLICATION_JSON)
    .ContentCharSet(TMVCCharset.UTF_8)
    .ContentEncoding(TMVCCharset.UTF_8)
    .SSL
    .Compression;

  Assert.IsTrue(LClient.ReadTimeOut = 100);
  Assert.IsTrue(LClient.ConnectionTimeOut = 100);
  Assert.IsTrue(LClient.Username = 'dmvc');
  Assert.IsTrue(LClient.Password = 'dmvc');
  Assert.IsTrue(LClient.UseBasicAuthentication);
  Assert.IsTrue(LClient.Accept = 'application/json;charset=UTF-8');
  Assert.IsTrue(LClient.ContentType = 'application/json;charset=UTF-8');
  Assert.IsTrue(LClient.ContentEncoding = 'UTF-8');
  Assert.IsTrue(LClient.HasSSL);
  Assert.IsTrue(LClient.HasCompression);

  Assert.IsTrue(LClient.RawBody <> nil);
  Assert.IsTrue(LClient.MultiPartFormData <> nil);
  Assert.IsTrue(LClient.BodyParams <> nil);
  Assert.IsTrue(LClient.RequestHeaders <> nil);
  Assert.IsTrue(LClient.QueryStringParams <> nil);

  FreeAndNil(LClient);
end;

procedure TTestRESTClient.TestPostUser;
var
  LUser: TAppUser;
  LResp: IRESTResponse;
begin
  FRESTClient.Resource('/user/save').Params([]);
  FRESTClient.Authentication('dmvc', '123');

  LUser := TAppUser.Create;
  LUser.Cod := 1;
  LUser.Name := 'Ezequiel';
  LUser.Pass := '123';
  LResp := FRESTClient.doPOST<TAppUser>(LUser, True);
  Assert.IsTrue(('Success!' = LResp.BodyAsString) and (LResp.ResponseCode = 200));

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
  LResp: IRESTResponse;
  I: Integer;
  LUser: TAppUser;
begin
  FRESTClient.Resource('/users/save').Params([]);
  FRESTClient.Authentication('dmvc', '123');
  FRESTClient.Accept('application/json;charset=utf-8');
  FRESTClient.ContentType('application/json;charset=utf-8');

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
    LResp := FRESTClient.doPOST<TAppUser>(LUsers);
  finally
    LUsers.Free;
  end;
  Assert.IsTrue(('Success!' = LResp.BodyAsString) and (LResp.ResponseCode = 200));

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

initialization

TDUnitX.RegisterTestFixture(TTestRESTClient);

end.
