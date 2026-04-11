unit ServerAbstractionTestU;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Server.Intf,
  MVCFramework.Server.Indy,
  MVCFramework.Server,
  MVCFramework.Server.Impl;

type

  [MVCPath('/')]
  TTestAbstractionController = class(TMVCController)
  public
    [MVCPath('/hello')]
    [MVCHTTPMethod([httpGET])]
    function HelloWorld: IMVCResponse;

    [MVCPath('/echo')]
    [MVCHTTPMethod([httpPOST])]
    function EchoBody: IMVCResponse;

    [MVCPath('/headers')]
    [MVCHTTPMethod([httpGET])]
    function EchoHeaders: IMVCResponse;

    [MVCPath('/status/($code)')]
    [MVCHTTPMethod([httpGET])]
    function ReturnStatus(const code: Integer): IMVCResponse;
  end;

  [TestFixture]
  TTestIndyServer = class(TObject)
  private
    FEngine: TMVCEngine;
    FServer: IMVCServer;
    procedure StartServer;
  protected
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
  public
    [Test]
    procedure TestStartStop;
    [Test]
    procedure TestSimpleGET;
    [Test]
    procedure TestPOSTWithBody;
    [Test]
    procedure TestCustomHeaders;
    [Test]
    procedure TestStatusCodes;
    [Test]
    procedure TestKeepAliveDefault;
    [Test]
    procedure TestEngineStandaloneConstructor;
    [Test]
    procedure TestMultipleStartStop;
  end;

  [TestFixture]
  TTestWebBrokerBackwardCompat = class(TObject)
  protected
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
  public
    [Test]
    procedure TestOldListenerStillWorks;
    [Test]
    procedure TestOldEngineConstructorStillWorks;
  end;

const
  TEST_INDY_PORT = 9876;
  TEST_COMPAT_PORT = 9877;

implementation

uses
  MVCFramework.RESTClient,
  MVCFramework.RESTClient.Intf,
  StandAloneServerWebModuleTest;

{ TTestAbstractionController }

function TTestAbstractionController.HelloWorld: IMVCResponse;
begin
  Result := OKResponse('Hello World from Indy Direct');
end;

function TTestAbstractionController.EchoBody: IMVCResponse;
begin
  Result := OKResponse(Context.Request.Body);
end;

function TTestAbstractionController.EchoHeaders: IMVCResponse;
var
  lHeaderValue: string;
begin
  lHeaderValue := Context.Request.Headers['X-Custom-Header'];
  Context.Response.SetCustomHeader('X-Custom-Echo', lHeaderValue);
  Result := OKResponse('Header: ' + lHeaderValue);
end;

function TTestAbstractionController.ReturnStatus(const code: Integer): IMVCResponse;
begin
  Result := StatusResponse(code, 'Status ' + code.ToString);
end;

{ TTestIndyServer }

procedure TTestIndyServer.StartServer;
begin
  FEngine := TMVCEngine.Create(TProc<TMVCConfig>(nil));
  FEngine.AddController(TTestAbstractionController);
  FServer := TMVCIndyServer.Create(FEngine);
  FServer.Listen(TEST_INDY_PORT);
end;

procedure TTestIndyServer.SetUp;
begin
  FEngine := nil;
  FServer := nil;
end;

procedure TTestIndyServer.TearDown;
begin
  if Assigned(FServer) then
  begin
    if FServer.IsRunning then
      FServer.Stop;
    FServer := nil;
  end;
  FreeAndNil(FEngine);
end;

procedure TTestIndyServer.TestStartStop;
begin
  StartServer;
  Assert.IsTrue(FServer.IsRunning);
  FServer.Stop;
  Assert.IsFalse(FServer.IsRunning);
end;

procedure TTestIndyServer.TestSimpleGET;
var
  LClient: IMVCRESTClient;
  LResp: IMVCRESTResponse;
begin
  StartServer;
  LClient := TMVCRESTClient.New.BaseURL('localhost', TEST_INDY_PORT);
  LResp := LClient.Get('/hello');
  Assert.AreEqual(200, LResp.StatusCode);
  Assert.Contains(LResp.Content, 'Hello World from Indy Direct');
end;

procedure TTestIndyServer.TestPOSTWithBody;
var
  LClient: IMVCRESTClient;
  LResp: IMVCRESTResponse;
  lBody: string;
begin
  StartServer;
  lBody := '{"name":"DelphiMVCFramework","version":"4"}';
  LClient := TMVCRESTClient.New.BaseURL('localhost', TEST_INDY_PORT);
  LResp := LClient.Post('/echo', lBody, 'application/json');
  Assert.AreEqual(200, LResp.StatusCode);
  Assert.Contains(LResp.Content, 'DelphiMVCFramework');
end;

procedure TTestIndyServer.TestCustomHeaders;
var
  LClient: IMVCRESTClient;
  LResp: IMVCRESTResponse;
begin
  StartServer;
  LClient := TMVCRESTClient.New.BaseURL('localhost', TEST_INDY_PORT);
  LClient.AddHeader('X-Custom-Header', 'TestValue123');
  LResp := LClient.Get('/headers');
  Assert.AreEqual(200, LResp.StatusCode);
  Assert.Contains(LResp.Content, 'Header: TestValue123');
  Assert.AreEqual('TestValue123', LResp.HeaderValue('X-Custom-Echo'));
end;

procedure TTestIndyServer.TestStatusCodes;
var
  LClient: IMVCRESTClient;
  LResp: IMVCRESTResponse;
begin
  StartServer;
  LClient := TMVCRESTClient.New.BaseURL('localhost', TEST_INDY_PORT);

  LResp := LClient.Get('/status/201');
  Assert.AreEqual(201, LResp.StatusCode);

  LResp := LClient.Get('/status/404');
  Assert.AreEqual(404, LResp.StatusCode);

  LResp := LClient.Get('/status/500');
  Assert.AreEqual(500, LResp.StatusCode);
end;

procedure TTestIndyServer.TestKeepAliveDefault;
var
  LServer: IMVCServer;
begin
  LServer := TMVCIndyServer.Create;
  try
    Assert.IsTrue(LServer.KeepAlive, 'KeepAlive should be True by default');
  finally
    LServer := nil;
  end;
end;

procedure TTestIndyServer.TestEngineStandaloneConstructor;
var
  LEngine: TMVCEngine;
  LConfigAction: TProc<TMVCConfig>;
begin
  LConfigAction := nil;
  LEngine := TMVCEngine.Create(LConfigAction);
  try
    Assert.IsNotNull(LEngine, 'Engine created with standalone constructor should not be nil');
  finally
    LEngine.Free;
  end;
end;

procedure TTestIndyServer.TestMultipleStartStop;
begin
  StartServer;
  Assert.IsTrue(FServer.IsRunning);

  FServer.Stop;
  Assert.IsFalse(FServer.IsRunning);

  FServer.Listen(TEST_INDY_PORT);
  Assert.IsTrue(FServer.IsRunning);

  FServer.Stop;
  Assert.IsFalse(FServer.IsRunning, 'Server should not be running after second Stop');

  // Restart for TearDown
  FServer.Listen(TEST_INDY_PORT);
end;

{ TTestWebBrokerBackwardCompat }

procedure TTestWebBrokerBackwardCompat.SetUp;
begin
  inherited;
end;

procedure TTestWebBrokerBackwardCompat.TearDown;
begin
  inherited;
end;

procedure TTestWebBrokerBackwardCompat.TestOldListenerStillWorks;
var
  lListener: IMVCListener;
  LClient: IMVCRESTClient;
  LResp: IMVCRESTResponse;
begin
  lListener := TMVCListener.Create(
    TMVCListenerProperties.New
      .SetName('CompatListener')
      .SetPort(TEST_COMPAT_PORT)
      .SetMaxConnections(512)
      .SetWebModuleClass(TestWebModuleClass)
  );

  Assert.IsTrue(Assigned(lListener), 'Listener should be assigned');

  lListener.Start;
  Assert.IsTrue(lListener.Active, 'Listener should be active after Start');

  LClient := TMVCRESTClient.New.BaseURL('localhost', TEST_COMPAT_PORT);
  LClient.SetBasicAuthorization('dmvc', '123');
  LResp := LClient.Get('/hello');
  Assert.AreEqual('Hello World called with GET', LResp.Content);

  lListener.Stop;
  Assert.IsFalse(lListener.Active, 'Listener should not be active after Stop');
end;

procedure TTestWebBrokerBackwardCompat.TestOldEngineConstructorStillWorks;
var
  LEngine: TMVCEngine;
  LWebModule: TTestWebModule2;
begin
  LWebModule := TTestWebModule2.Create(nil);
  try
    {$WARN SYMBOL_DEPRECATED OFF}
    LEngine := TMVCEngine.Create(LWebModule);
    {$WARN SYMBOL_DEPRECATED ON}
    try
      Assert.IsNotNull(LEngine, 'Engine created with old WebModule constructor should not be nil');
    finally
      LEngine.Free;
    end;
  finally
    LWebModule.Free;
  end;
end;

initialization

TDUnitX.RegisterTestFixture(TTestIndyServer);
TDUnitX.RegisterTestFixture(TTestWebBrokerBackwardCompat);

end.
