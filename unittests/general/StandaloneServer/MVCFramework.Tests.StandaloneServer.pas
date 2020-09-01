unit MVCFramework.Tests.StandaloneServer;

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
  TTestMVCFrameworkServer = class(TObject)
  private

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
  MVCFramework.Tests.WebModule2,
  MVCFramework.RESTClient,
  MVCFramework.RESTClient.Intf;

{ TTestMVCFrameworkServer }

procedure TTestMVCFrameworkServer.SetUp;
begin
  inherited;

end;

procedure TTestMVCFrameworkServer.TearDown;
begin
  inherited;

end;

procedure TTestMVCFrameworkServer.TestListener;
var
  LListener: IMVCListener;
begin
  LListener := TMVCListener.Create(TMVCListenerProperties.New
    .SetName('Listener1')
    .SetPort(5000)
    .SetMaxConnections(512)
    .SetWebModuleClass(TestWebModuleClass)
    );

  Assert.isTrue(Assigned(LListener));

  LListener.Start;
  Assert.isTrue(LListener.Active);

  LListener.Stop;
  Assert.isFalse(LListener.Active);
end;

procedure TTestMVCFrameworkServer.TestServerListenerAndClient;
var
  lListener: IMVCListener;
  lRes: IMVCRESTResponse;
begin
  lListener := TMVCListener.Create(TMVCListenerProperties.New
    .SetName('Listener1')
    .SetPort(6000)
    .SetMaxConnections(1024)
    .SetWebModuleClass(TestWebModuleClass)
    );

  Assert.isTrue(Assigned(lListener));

  lListener.Start;
  Assert.isTrue(lListener.Active);

  lRes := TMVCRESTClient.New
    .BaseURL('localhost', 6000)
    .SetBasicAuthorization('dmvc', '123')
    .Get('/hello');
  Assert.AreEqual('Hello World called with GET', lRes.Content);

  lListener.Stop;
  Assert.isFalse(lListener.Active);
end;

procedure TTestMVCFrameworkServer.TestListenerContext;
var
  LListenerCtx: IMVCListenersContext;
begin
  LListenerCtx := TMVCListenersContext.Create;

  LListenerCtx.Add(TMVCListenerProperties.New
    .SetName('Listener2')
    .SetPort(6000)
    .SetMaxConnections(1024)
    .SetWebModuleClass(TestWebModuleClass)
    );

  LListenerCtx.Add(TMVCListenerProperties.New
    .SetName('Listener3')
    .SetPort(7000)
    .SetMaxConnections(1024)
    .SetWebModuleClass(TestWebModuleClass2)
    );

  Assert.isTrue(Assigned(LListenerCtx.FindByName('Listener2')));
  Assert.isTrue(Assigned(LListenerCtx.FindByName('Listener3')));

  LListenerCtx.StartAll;

  Assert.isTrue(LListenerCtx.Count = 2);
  Assert.isTrue(LListenerCtx.FindByName('Listener2').Active);
  Assert.isTrue(LListenerCtx.FindByName('Listener3').Active);

  LListenerCtx.StopAll;

  Assert.isFalse(LListenerCtx.FindByName('Listener2').Active);
  Assert.isFalse(LListenerCtx.FindByName('Listener3').Active);

  LListenerCtx
    .Remove('Listener2')
    .Remove('Listener3');

  Assert.isTrue(LListenerCtx.Count = 0);
end;

{ TTestController }

procedure TTestController.HelloWorld(ctx: TWebContext);
begin
  Render('Hello World called with GET');
end;

initialization

TDUnitX.RegisterTestFixture(TTestMVCFrameworkServer);

end.
