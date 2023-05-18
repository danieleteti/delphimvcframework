// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
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
    [Test]
    procedure TestRequestHooksProc();
  end;

implementation

uses
  MVCFramework.Tests.WebModule1,
  LiveServerTestU,
  MVCFramework.SystemJSONUtils,
  System.JSON,
  System.IOUtils,
  JsonDataObjects, System.Hash, Vcl.Graphics;

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

  FRESTClient := TMVCRESTClient.New
    .BaseURL('localhost', 3000)
    .SetBasicAuthorization('dmvc', '123'); // Set the authorization only once and it will be stored for all requests

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

procedure TTestRESTClient.TestRequestHooksProc;
var
  lResponse: IMVCRESTResponse;
begin
  lResponse := FRESTClient
    .Resource('/hello')
    .SetBeforeRequestProc(
      procedure(aRequest: IHTTPRequest)
      begin
        Assert.AreEqual('/hello', aRequest.URL.Path);
      end
    )
    .SetRequestCompletedProc(
      procedure (aResponse: IHTTPResponse; var aHandled: Boolean)
      begin
        Assert.IsTrue(aResponse.ContentLength > 0);
        // Set Handled to True to not process TMVCRESTResponse
        aHandled := True;
      end
    )
    .Get;
  Assert.IsNull(lResponse);


  lResponse := FRESTClient
    .Resource('/hello')
    .SetRequestCompletedProc(
      procedure (aResponse: IHTTPResponse; var aHandled: Boolean)
      begin
        Assert.IsTrue(aResponse.ContentLength > 0);
      end
    )
    .SetResponseCompletedProc(
      procedure(aResponse: IMVCRESTResponse)
      begin
        Assert.AreEqual('Hello World called with GET', aResponse.Content);
      end
    )
    .Get;
end;

procedure TTestRESTClient.TestBodyURLEncoded;
var
  lResp: IMVCRESTResponse;
  lJsonResp: TJDOJsonObject;
begin
  lResp := FRESTClient
// .SetBasicAuthorization('dmvc', '123')
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
// FRESTClient.SetBasicAuthorization('dmvc', '123');

  // String
  LResp := FRESTClient.Get;
  Assert.IsTrue(
    ('{"Cod":1,"Name":"Ezequiel","Pass":"123"}' = LResp.Content) and
    (LResp.StatusCode = 200)
    );

  LUser := TAppUser.Create;
  try
    FRESTClient.Get.BodyFor(LUser);
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
  lResp: IMVCRESTResponse;
begin
  FRESTClient.Resource('/users');

  lResp := FRESTClient.Get;
  // String
  Assert.AreEqual('[{"Cod":0,"Name":"Ezequiel 0","Pass":"0"},{"Cod":1,"Name":"Ezequiel 1","Pass":"1"},' +
    '{"Cod":2,"Name":"Ezequiel 2","Pass":"2"},{"Cod":3,"Name":"Ezequiel 3","Pass":"3"},{"Cod":4,"Name":"Ezequiel 4","Pass":"4"},' +
    '{"Cod":5,"Name":"Ezequiel 5","Pass":"5"},{"Cod":6,"Name":"Ezequiel 6","Pass":"6"},{"Cod":7,"Name":"Ezequiel 7","Pass":"7"},' +
    '{"Cod":8,"Name":"Ezequiel 8","Pass":"8"},{"Cod":9,"Name":"Ezequiel 9","Pass":"9"},{"Cod":10,"Name":"Ezequiel 10","Pass":"10"}]',
    LResp.Content);

  // Objects
  LUsers := TObjectList<TAppUser>.Create(True);
  try
    lResp.BodyForListOf(LUsers, TAppUser);
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

  // String
  Assert.AreEqual('Hello World called with GET', FRESTClient.Get.Content);

  // Adapter
  Assert.AreEqual('Hello World called with GET', FAppResource.HelloWorld);
end;

procedure TTestRESTClient.TestInformation;
const
  JWT_TOKEN =
    'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.' +
    'eyJzdWIiOiIxMjMiLCJuYW1lIjoiVXNlciIsImlhdCI6MTUxNjIzOTAyMn0.' +
    'EdtbBz7jOucEVf5AV-wD_NwqlJzoCdZKYmMa6p54PVY';

var
  LClient: IMVCRESTClient;
begin
  LClient := TMVCRESTClient.New.BaseURL('localhost', 8080);
  LClient
    .ReadTimeOut(100)
    .ConnectTimeout(100)
    .SetBasicAuthorization('dmvc', 'dmvc')
    .Accept(TMVCMediaType.APPLICATION_JSON)
    .AcceptCharSet(TMVCCharset.UTF_8)
    .AcceptEncoding('gzip,deflate')
    .UserAgent('DMVCFramework RESTClient')
    .SetBasicAuthorization('username', 'password');

  Assert.AreEqual('http://localhost:8080', LClient.BaseURL);
  Assert.AreEqual(100, LClient.ReadTimeOut);
  Assert.AreEqual(100, LClient.ConnectTimeout);
  Assert.AreEqual('application/json', LClient.Accept);
  Assert.AreEqual('UTF-8', LClient.AcceptCharset);
  Assert.AreEqual('gzip,deflate', LClient.AcceptEncoding);
  Assert.AreEqual('DMVCFramework RESTClient', LClient.UserAgent);
  Assert.AreEqual('Basic dXNlcm5hbWU6cGFzc3dvcmQ=', LClient.Authorization);

  LClient.SetBearerAuthorization(JWT_TOKEN);

  Assert.AreEqual('Bearer ' + JWT_TOKEN, LClient.Authorization);
end;

procedure TTestRESTClient.TestPostUser;
var
  LUser: TAppUser;
  LResp: IMVCRESTResponse;
begin
  FRESTClient.Resource('/user/save');

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
var
  lStringFile: TStringStream;
  lFilename: string;
  lResp: IMVCRESTResponse;
  lBitmap: TBitmap;
begin
  // Text File
  lFilename := ExtractFilePath(ParamStr(0)) + 'text_file_upload_sample.txt';
  if TFile.Exists(lFilename) then
    TFile.Delete(lFilename);

  lStringFile := TStringStream.Create(TEXT_SAMPLE);
  try
    lStringFile.SaveToFile(lFilename);
  finally
    lStringFile.Free;
  end;

  lResp := FRESTClient
    .AddFile(lFilename)
    .Post('/file/upload');

  Assert.AreEqual(lResp.StatusCode, 200);
  Assert.AreEqual(THashMD5.GetHashStringFromFile(lFilename), lResp.Content);

  // Image File
  lFilename := ExtractFilePath(ParamStr(0)) + 'bitmap_file_upload_sample.bmp';
  if TFile.Exists(lFilename) then
    TFile.Delete(lFilename);

  lBitmap := TBitmap.Create;
  try
    lBitmap.SetSize(200, 200);
    lBitmap.Canvas.Brush.Color := clBlue;
    lBitmap.Canvas.FillRect(Rect(0, 0, 200, 200));
    lBitmap.Canvas.Font.Color := clRed;
    lBitmap.Canvas.Font.Style := [fsBold];
    lBitmap.Canvas.TextOut(10, 100, 'DelphiMVCFramework');
    lBitmap.SaveToFile(lFilename);
  finally
    lBitmap.Free;
  end;

  lResp := FRESTClient
    .AddFile(lFilename)
    .Post('/file/upload');

  Assert.AreEqual(lResp.StatusCode, 200);
  Assert.AreEqual(THashMD5.GetHashStringFromFile(lFilename), lResp.Content);
end;

initialization

TDUnitX.RegisterTestFixture(TTestRESTClient);

end.
