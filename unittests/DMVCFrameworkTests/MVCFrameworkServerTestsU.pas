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
  vOnAuthentication: TMVCAuthenticationDelegate;
  vServer: IMVCServer;
begin
  vServerInfo := TMVCServerInfoFactory.Build;
  vServerInfo.ServerName := 'ServerTemp';
  vServerInfo.Port := 4000;
  vServerInfo.MaxConnections := 1024;
  vServerInfo.WebModuleClass := TestWebModuleClass;

  vOnAuthentication := procedure(const pUserName, pPassword: string;
      pUserRoles: TList<string>; var pIsValid: Boolean)
    begin
      pIsValid := pUserName.Equals('ezequiel') and pPassword.Equals('123');
    end;

  vServerInfo.Security := TMVCDefaultSecurity.Create(vOnAuthentication, nil);

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

  MVCServerDefault.Container.CreateServer(vServerInfo);
  MVCServerDefault.Container.StartServers;

  vRESTCli := TRESTClient.Create('localhost', 6000);
  try
    vRESTCli.UserName := 'ezequiel';
    vRESTCli.Password := '123';
    CheckEqualsString('"Hello World called with GET"', vRESTCli.doGET('/hello', []).BodyAsString);
  finally
    FreeAndNil(vRESTCli);
  end;

  MVCServerDefault.Container.StopServers;
end;

procedure TTestMVCFrameworkServer.TestServerContainer;
var
  vServerInfo: IMVCServerInfo;
  vOnAuthentication: TMVCAuthenticationDelegate;
  FContainer: IMVCServerContainer;
begin
  vServerInfo := TMVCServerInfoFactory.Build;
  vServerInfo.ServerName := 'ServerTemp';
  vServerInfo.Port := 4000;
  vServerInfo.MaxConnections := 1024;
  vServerInfo.WebModuleClass := TestWebModuleClass;

  vOnAuthentication := procedure(const pUserName, pPassword: string;
      pUserRoles: TList<string>; var pIsValid: Boolean)
    begin
      pIsValid := pUserName.Equals('ezequiel') and pPassword.Equals('123');
    end;

  vServerInfo.Security := TMVCDefaultSecurity.Create(vOnAuthentication, nil);

  FContainer := TMVCServerContainerFactory.Build();
  FContainer.CreateServer(vServerInfo);

  CheckTrue(FContainer.FindServerByName('ServerTemp') <> nil);

  FContainer.DestroyServer('ServerTemp');

  CheckTrue(FContainer.FindServerByName('ServerTemp') = nil);
end;

{ TTestAppController }

procedure TTestAppController.HelloWorld(ctx: TWebContext);
begin
  Render('Hello World called with GET');
end;

initialization

RegisterTest(TTestMVCFrameworkServer.Suite);

end.
