unit MVCFramework.Tests.StandaloneServer;

interface

uses
  TestFramework,
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

  TTestMVCFrameworkServer = class(TTestCase)
  private

  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestListener;
    procedure TestListenerContext;
    procedure TestServerListenerAndClient;
  end;

implementation

uses
  MVCFramework.Tests.WebModule2,
  MVCFramework.RESTClient;

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

  CheckTrue(Assigned(LListener));

  LListener.Start;
  CheckTrue(LListener.Active);

  LListener.Stop;
  CheckFalse(LListener.Active);
end;

procedure TTestMVCFrameworkServer.TestServerListenerAndClient;
var
  LListener: IMVCListener;
  LClient: TRESTClient;
begin
  LListener := TMVCListener.Create(TMVCListenerProperties.New
    .SetName('Listener1')
    .SetPort(6000)
    .SetMaxConnections(1024)
    .SetWebModuleClass(TestWebModuleClass)
    );

  CheckTrue(Assigned(LListener));

  LListener.Start;
  CheckTrue(LListener.Active);

  LClient := TRESTClient.Create('localhost', 6000);
  try
    LClient.UserName := 'dmvc';
    LClient.Password := '123';
    CheckEqualsString('"Hello World called with GET"', LClient.doGET('/hello', []).BodyAsString);
  finally
    FreeAndNil(LClient);
  end;

  LListener.Stop;
  CheckFalse(LListener.Active);
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

  CheckTrue(Assigned(LListenerCtx.FindByName('Listener2')));
  CheckTrue(Assigned(LListenerCtx.FindByName('Listener3')));

  LListenerCtx.StartAll;

  CheckTrue(LListenerCtx.Count = 2);
  CheckTrue(LListenerCtx.FindByName('Listener2').Active);
  CheckTrue(LListenerCtx.FindByName('Listener3').Active);

  LListenerCtx.StopAll;

  CheckFalse(LListenerCtx.FindByName('Listener2').Active);
  CheckFalse(LListenerCtx.FindByName('Listener3').Active);

  LListenerCtx
    .Remove('Listener2')
    .Remove('Listener3');

  CheckTrue(LListenerCtx.Count = 0);
end;

{ TTestController }

procedure TTestController.HelloWorld(ctx: TWebContext);
begin
  Render('Hello World called with GET');
end;

initialization

RegisterTest(TTestMVCFrameworkServer.Suite);

end.
