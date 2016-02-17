unit MVCFramework.Tests.WebModule;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework;

type

  TTestWebModule = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    FMVCEngine: TMVCEngine;
  public
    { Public declarations }
  end;

var
  TestWebModuleClass: TComponentClass = TTestWebModule;

implementation

uses
  MVCFramework.Tests.RESTClient,
  MVCFramework.Middleware.Authentication,
  MVCFramework.Tests.AppController,
  MVCFramework.Server;

{$R *.dfm}

procedure TTestWebModule.WebModuleCreate(Sender: TObject);
var
  vServer: IMVCServer;
begin
  FMVCEngine := TMVCEngine.Create(Self);

  // Add Controller
  FMVCEngine.AddController(TAppController);

  // Add Security Middleware
  vServer := MVCServerDefault.Container.FindServerByName('ServerApp');
  if (vServer <> nil) and (vServer.Info.Security <> nil) then
    FMVCEngine.AddMiddleware(TMVCBasicAuthenticationMiddleware.Create(vServer.Info.Security));
end;

procedure TTestWebModule.WebModuleDestroy(Sender: TObject);
begin
  FreeAndNil(FMVCEngine);
end;

end.
