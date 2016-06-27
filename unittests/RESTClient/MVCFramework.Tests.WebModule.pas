unit MVCFramework.Tests.WebModule;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework,
  System.Generics.Collections;

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
  MVCFramework.Server,
  MVCFramework.Server.Impl;

{$R *.dfm}

procedure TTestWebModule.WebModuleCreate(Sender: TObject);
begin
  FMVCEngine := TMVCEngine.Create(Self);

  // Add Controller
  FMVCEngine.AddController(TAppController);

  FMVCEngine.AddMiddleware(TMVCBasicAuthenticationMiddleware.Create(
    TMVCDefaultAuthenticationHandler.New
    .SetOnAuthentication(
    procedure(const AUserName, APassword: string;
      AUserRoles: TList<string>; var IsValid: Boolean; const ASessionData: TDictionary<String, String>)
    begin
      IsValid := AUserName.Equals('dmvc') and APassword.Equals('123');
    end
    )
    ));
end;

procedure TTestWebModule.WebModuleDestroy(Sender: TObject);
begin
  FMVCEngine.Free;
end;

end.
