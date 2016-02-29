unit MVCFramework.Tests.RESTClient;

interface

uses
  TestFramework,
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  ObjectsMappers,
  MVCFramework,
  MVCFramework.Server,
  MVCFramework.RESTClient,
  MVCFramework.RESTAdapter,
  MVCFramework.Commons,
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
    [MapperListOf(TAppUser)]
    function GetUsers(): TObjectList<TAppUser>;

    [RESTResource(TMVCHTTPMethodType.httpPOST, '/users/save')]
    [MapperListOf(TAppUser)]
    procedure PostUsers([Body] pBody: TObjectList<TAppUser>);
  end;

  TTestRESTClient = class(TTestCase)
  strict private
    FRESTClient: TRESTClient;
    FRESTAdapter: TRESTAdapter<IAppResource>;
    FAppResource: IAppResource;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreateAndDestroy();
    procedure TestInformation();

    procedure TestHelloWorld();
    procedure TestGetUser();
    procedure TestPostUser();
    procedure TestPostUsers();
    procedure TestGetUsers();
  end;

implementation

uses
  MVCFramework.Tests.WebModule;

{ TTestRESTClient }

procedure TTestRESTClient.SetUp;
var
  vServerInfo: IMVCServerInfo;
  vOnAuthentication: TMVCAuthenticationDelegate;
begin
  inherited;
  vServerInfo := TMVCServerInfoFactory.Build;
  vServerInfo.ServerName := 'ServerApp';
  vServerInfo.Port := 3000;
  vServerInfo.MaxConnections := 1024;
  vServerInfo.WebModuleClass := TestWebModuleClass;

  vOnAuthentication := procedure(const AUserName, APassword: string;
      AUserRoles: TList<string>; var AIsValid: Boolean; const ASessionData: TDictionary<String, String>)
    begin
      AIsValid := AUserName.Equals('ezequiel') and APassword.Equals('123');
    end;

  vServerInfo.Security := TMVCDefaultSecurity.Create(vOnAuthentication, nil);

  MVCServerDefault.Container.CreateServer(vServerInfo);
  MVCServerDefault.Container.StartServers;

  FRESTClient := TRESTClient.Create('localhost', 3000);
  FRESTAdapter := TRESTAdapter<IAppResource>.Create;
  FRESTAdapter.Build(FRESTClient);
  FAppResource := FRESTAdapter.ResourcesService;
end;

procedure TTestRESTClient.TearDown;
begin
  inherited;
  MVCServerDefault.Container.StopServers;
  FreeAndNil(FRESTClient);
end;

procedure TTestRESTClient.TestCreateAndDestroy;
var
  vClient: TRESTClient;
begin
  vClient := TRESTClient.Create('', 80, nil);
  CheckTrue(vClient <> nil);
  FreeAndNil(vClient);
  CheckTrue(vClient = nil);
end;

procedure TTestRESTClient.TestGetUser;
var
  vUser: TAppUser;
  vResp: IRESTResponse;
begin
  FRESTClient.Resource('/user').Params([]);
  FRESTClient.Authentication('ezequiel', '123');

  // String
  vResp := FRESTClient.doGET;
  CheckTrue(
    ('{"Cod":1,"Name":"Ezequiel","Pass":"123"}' = vResp.BodyAsString) and
    (vResp.ResponseCode = 200)
    );

  // Object
  vUser := FRESTClient.doGET.BodyAsJSONObject.AsObject<TAppUser>();
  try
    CheckTrue((vUser <> nil) and (vUser.Cod > 0));
  finally
    FreeAndNil(vUser);
  end;

  // Adapter
  vUser := FAppResource.GetUser;
  try
    CheckTrue((vUser <> nil) and (vUser.Cod > 0));
  finally
    FreeAndNil(vUser);
  end;
end;

procedure TTestRESTClient.TestGetUsers;
var
  vUsers: TObjectList<TAppUser>;
begin
  FRESTClient.Resource('/users').Params([]);
  FRESTClient.Authentication('ezequiel', '123');

  // String
  CheckEqualsString('[{"Cod":0,"Name":"Ezequiel 0","Pass":"0"},{"Cod":1,"Name":"Ezequiel 1","Pass":"1"},' +
    '{"Cod":2,"Name":"Ezequiel 2","Pass":"2"},{"Cod":3,"Name":"Ezequiel 3","Pass":"3"},{"Cod":4,"Name":"Ezequiel 4","Pass":"4"},' +
    '{"Cod":5,"Name":"Ezequiel 5","Pass":"5"},{"Cod":6,"Name":"Ezequiel 6","Pass":"6"},{"Cod":7,"Name":"Ezequiel 7","Pass":"7"},' +
    '{"Cod":8,"Name":"Ezequiel 8","Pass":"8"},{"Cod":9,"Name":"Ezequiel 9","Pass":"9"},{"Cod":10,"Name":"Ezequiel 10","Pass":"10"}]',
    FRESTClient.doGET.BodyAsString);

  // Objects
  vUsers := FRESTClient.doGET.BodyAsJSONArray.AsObjectList<TAppUser>;
  try
    vUsers.OwnsObjects := True;
    CheckTrue(vUsers.Count > 0);
  finally
    FreeAndNil(vUsers);
  end;

  // Adapter
  vUsers := FAppResource.GetUsers;
  try
    vUsers.OwnsObjects := True;
    CheckTrue(vUsers.Count > 0);
  finally
    FreeAndNil(vUsers);
  end;
end;

procedure TTestRESTClient.TestHelloWorld;
begin
  FRESTClient.Resource('/hello').Params([]);
  FRESTClient.Authentication('ezequiel', '123');

  // String
  CheckEqualsString('"Hello World called with GET"', FRESTClient.doGET.BodyAsString);

  // Adapter
  CheckEqualsString('"Hello World called with GET"', FAppResource.HelloWorld);
end;

procedure TTestRESTClient.TestInformation;
var
  vClient: TRESTClient;
begin
  vClient := TRESTClient.Create('', 80, nil);
  vClient
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

  CheckTrue(vClient.ReadTimeOut = 100);
  CheckTrue(vClient.ConnectionTimeOut = 100);
  CheckTrue(vClient.Username = 'dmvc');
  CheckTrue(vClient.Password = 'dmvc');
  CheckTrue(vClient.UseBasicAuthentication);
  CheckTrue(vClient.Accept = 'application/json;charset=UTF-8');
  CheckTrue(vClient.ContentType = 'application/json;charset=UTF-8');
  CheckTrue(vClient.ContentEncoding = 'UTF-8');
  CheckTrue(vClient.HasSSL);
  CheckTrue(vClient.HasCompression);

  CheckTrue(vClient.RawBody <> nil);
  CheckTrue(vClient.MultiPartFormData <> nil);
  CheckTrue(vClient.BodyParams <> nil);
  CheckTrue(vClient.RequestHeaders <> nil);
  CheckTrue(vClient.QueryStringParams <> nil);

  FreeAndNil(vClient);
end;

procedure TTestRESTClient.TestPostUser;
var
  vUser: TAppUser;
  vResp: IRESTResponse;
begin
  FRESTClient.Resource('/user/save').Params([]);
  FRESTClient.Authentication('ezequiel', '123');

  vUser := TAppUser.Create;
  vUser.Cod := 1;
  vUser.Name := 'Ezequiel';
  vUser.Pass := '123';
  vResp := FRESTClient.doPOST<TAppUser>(vUser);
  CheckTrue(('"Sucess!"' = vResp.BodyAsString) and (vResp.ResponseCode = 200));

  // Adapter
  vUser := TAppUser.Create;
  vUser.Cod := 1;
  vUser.Name := 'Ezequiel';
  vUser.Pass := '123';
  FAppResource.PostUser(vUser);
end;

procedure TTestRESTClient.TestPostUsers;
var
  vUsers: TObjectList<TAppUser>;
  vResp: IRESTResponse;
  I: Integer;
  vUser: TAppUser;
begin
  FRESTClient.Resource('/users/save').Params([]);
  FRESTClient.Authentication('ezequiel', '123');
  FRESTClient.Accept('application/json;charset=utf-8');
  FRESTClient.ContentType('application/json;charset=utf-8');

  vUsers := TObjectList<TAppUser>.Create(True);
  for I := 0 to 10 do
  begin
    vUser := TAppUser.Create;
    vUser.Cod := I;
    vUser.Name := 'Ezequiel ˆ¸·‡Á„ı∫s ' + IntToStr(I);
    vUser.Pass := IntToStr(I);
    vUsers.Add(vUser);
  end;
  vResp := FRESTClient.doPOST<TAppUser>(vUsers);
  CheckTrue(('"Sucess!"' = vResp.BodyAsString) and (vResp.ResponseCode = 200));

  // Adapter
  vUsers := TObjectList<TAppUser>.Create(True);
  for I := 0 to 10 do
  begin
    vUser := TAppUser.Create;
    vUser.Cod := I;
    vUser.Name := 'Ezequiel ˆ¸·‡Á„ı∫s ' + IntToStr(I);
    vUser.Pass := IntToStr(I);
    vUsers.Add(vUser);
  end;
  FAppResource.PostUsers(vUsers);
end;

initialization

RegisterTest(TTestRESTClient.Suite);

end.
