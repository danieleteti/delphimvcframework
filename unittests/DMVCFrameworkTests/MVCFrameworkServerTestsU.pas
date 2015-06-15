unit MVCFrameworkServerTestsU;

interface

uses
  TestFramework,
  System.Classes,
  System.SysUtils,
  MVCFramework.Server,
  System.Generics.Collections,
  MVCFramework;

type

  [MVCPath('/')]
  TTestAppController = class(TMVCController)
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
    procedure TestCreateServer();
    procedure TestServerContainer();

    procedure TestServerAndClient();
  end;

var
  ServerContainer: IMVCServerContainer;

implementation

uses
  TestWebModuleU,
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

procedure TTestMVCFrameworkServer.TestCreateServer;
var
  vServerInfo: IMVCServerInfo;
  vServer: IMVCServer;
begin
  vServerInfo := TMVCServerInfoFactory.Build;
  vServerInfo.ServerName := 'ServerTemp';
  vServerInfo.Port := 4000;
  vServerInfo.MaxConnections := 1024;
  vServerInfo.WebModuleClass := TestWebModuleClass;

  vServer := TMVCServerFactory.Build(vServerInfo);

  CheckTrue(vServer.Info <> nil);

  vServer.Start;
  vServer.Stop;
end;

procedure TTestMVCFrameworkServer.TestServerAndClient;
var
  vServerInfo: IMVCServerInfo;
  vOnAuthentication: TMVCAuthenticationDelegate;
  vRESTCli: TRESTClient;
begin
  vServerInfo := TMVCServerInfoFactory.Build;
  vServerInfo.ServerName := 'ServerTemp';
  vServerInfo.Port := 6000;
  vServerInfo.MaxConnections := 1024;
  vServerInfo.WebModuleClass := TestWebModuleClass;

  vOnAuthentication := procedure(const pUserName, pPassword: string;
      pUserRoles: TList<string>; var pIsValid: Boolean)
    begin
      pIsValid := pUserName.Equals('ezequiel') and pPassword.Equals('123');
    end;

  vServerInfo.Security := TMVCDefaultSecurity.Create(vOnAuthentication, nil);

  if (ServerContainer.FindServerByName('ServerTemp') <> nil) then
    ServerContainer.DestroyServer('ServerTemp');

  ServerContainer.CreateServer(vServerInfo);
  ServerContainer.StartServers;

  vRESTCli := TRESTClient.Create('localhost', 6000);
  try
    vRESTCli.UserName := 'ezequiel';
    vRESTCli.Password := '123';
    CheckEqualsString('"Hello World called with GET"', vRESTCli.doGET('/hello', []).BodyAsString);
  finally
    FreeAndNil(vRESTCli);
  end;

  ServerContainer.StopServers;
end;

procedure TTestMVCFrameworkServer.TestServerContainer;
var
  vServerInfo: IMVCServerInfo;
  vContainer: IMVCServerContainer;
begin
  vServerInfo := TMVCServerInfoFactory.Build;
  vServerInfo.ServerName := 'ServerTemp';
  vServerInfo.Port := 4000;
  vServerInfo.MaxConnections := 1024;
  vServerInfo.WebModuleClass := TestWebModuleClass;

  vContainer := TMVCServerContainerFactory.Build();
  vContainer.CreateServer(vServerInfo);

  CheckTrue(vContainer.FindServerByName('ServerTemp') <> nil);

  vContainer.DestroyServer('ServerTemp');

  CheckTrue(vContainer.FindServerByName('ServerTemp') = nil);
end;

{ TTestAppController }

procedure TTestAppController.HelloWorld(ctx: TWebContext);
begin
  Render('Hello World called with GET');
end;

initialization

RegisterTest(TTestMVCFrameworkServer.Suite);

ServerContainer := MVCServerDefault.Container;

end.
