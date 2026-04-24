unit StandaloneServerTestU;

interface

uses
  DUnitX.TestFramework,
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Server,
  MVCFramework.Server.Impl;

type

  [MVCPath('/')]
  TTestController = class(TMVCController)
  public
    [MVCPath('/hello')]
    [MVCHTTPMethod([httpGET])]
    procedure HelloWorld(ctx: TWebContext);
  end;

  [TestFixture]
  TTestServerContainer = class(TObject)
  protected
    [SetUp]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
  public
    [Test]
    procedure TestListener;
    [Test]
    procedure TestListenerContext;
    [Test]
    procedure TestServerListenerAndClient;
  end;

implementation

uses
  MVCFramework.RESTClient,
  MVCFramework.RESTClient.Intf,
  StandAloneServerWebModuleTest;

{ TTestServerContainer }

procedure TTestServerContainer.SetUp;
begin
  inherited;

end;

procedure TTestServerContainer.TearDown;
begin
  inherited;

end;

procedure TTestServerContainer.TestListener;
var
  lListener: IMVCListener;
begin
  // Unique high-range port per test: low ports (5000/6000/7000) are
  // common service defaults and - more importantly - sharing a port
  // between tests in this fixture meant a failure in one left a socket
  // in TIME_WAIT that blocked the next test for ~60 seconds.
  lListener := TMVCListener.Create(TMVCListenerProperties.New.SetName('Listener1').SetPort(15100).SetMaxConnections(512)
    .SetWebModuleClass(TestWebModuleClass));

  Assert.IsTrue(Assigned(lListener));

  lListener.Start;
  try
    Assert.IsTrue(lListener.Active);
  finally
    lListener.Stop;
  end;
  Assert.IsFalse(lListener.Active);
end;

procedure TTestServerContainer.TestServerListenerAndClient;
var
  lListener: IMVCListener;
  LClient: IMVCRESTClient;
begin
  lListener := TMVCListener.Create(TMVCListenerProperties.New.SetName('Listener1').SetPort(15200).SetMaxConnections(1024)
    .SetWebModuleClass(TestWebModuleClass));

  Assert.IsTrue(Assigned(lListener));

  lListener.Start;
  try
    Assert.IsTrue(lListener.Active);

    LClient := TMVCRESTClient.New.BaseURL('localhost', 15200);
    LClient.SetBasicAuthorization('dmvc', '123');
    Assert.AreEqual('Hello World called with GET', LClient.Get('/hello').Content);
  finally
    lListener.Stop;
  end;
  Assert.IsFalse(lListener.Active);
end;

procedure TTestServerContainer.TestListenerContext;
var
  LListenerCtx: IMVCListenersContext;
begin
  LListenerCtx := TMVCListenersContext.Create;

  LListenerCtx.Add(TMVCListenerProperties.New.SetName('Listener2').SetPort(15300).SetMaxConnections(1024)
    .SetWebModuleClass(TestWebModuleClass));

  LListenerCtx.Add(TMVCListenerProperties.New.SetName('Listener3').SetPort(15400).SetMaxConnections(1024)
    .SetWebModuleClass(TestWebModuleClass2));

  Assert.IsTrue(Assigned(LListenerCtx.FindByName('Listener2')));
  Assert.IsTrue(Assigned(LListenerCtx.FindByName('Listener3')));

  LListenerCtx.StartAll;
  try
    Assert.IsTrue(LListenerCtx.Count = 2);
    Assert.IsTrue(LListenerCtx.FindByName('Listener2').Active);
    Assert.IsTrue(LListenerCtx.FindByName('Listener3').Active);
  finally
    LListenerCtx.StopAll;
  end;

  Assert.IsFalse(LListenerCtx.FindByName('Listener2').Active);
  Assert.IsFalse(LListenerCtx.FindByName('Listener3').Active);

  LListenerCtx.Remove('Listener2').Remove('Listener3');

  Assert.IsTrue(LListenerCtx.Count = 0);
end;

{ TTestController }

procedure TTestController.HelloWorld(ctx: TWebContext);
begin
  Render('Hello World called with GET');
end;

initialization

TDUnitX.RegisterTestFixture(TTestServerContainer);

end.
