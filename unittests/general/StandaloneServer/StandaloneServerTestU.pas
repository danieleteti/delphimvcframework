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

procedure TTestServerContainer.TestServerListenerAndClient;
var
  LListener: IMVCListener;
  LClient: IMVCRESTClient;
begin
  LListener := TMVCListener.Create(TMVCListenerProperties.New
    .SetName('Listener1')
    .SetPort(6000)
    .SetMaxConnections(1024)
    .SetWebModuleClass(TestWebModuleClass)
    );

  Assert.isTrue(Assigned(LListener));

  LListener.Start;
  Assert.isTrue(LListener.Active);

  LClient := TMVCRESTClient.New.BaseURL('localhost', 6000);
  LClient.SetBasicAuthorization('dmvc', '123');
  Assert.AreEqual('Hello World called with GET', LClient.Get('/hello').Content);

  LListener.Stop;
  Assert.isFalse(LListener.Active);
end;

procedure TTestServerContainer.TestListenerContext;
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

TDUnitX.RegisterTestFixture(TTestServerContainer);

end.

