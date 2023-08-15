unit WebModuleUnit1;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework,
  MVCFramework.Commons, FireDAC.Phys.PGDef, FireDAC.Stan.Intf, FireDAC.Phys,
  FireDAC.Phys.PG;

type
  TWebModule1 = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);

  private
    MVC: TMVCEngine;

  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

{$R *.dfm}


uses
  AppControllerU,
  System.Generics.Collections,
  MVCFramework.Middleware.Authentication,
  MVCFramework.Middleware.ActiveRecord,
  MVCFramework.Middleware.StaticFiles,
  AuthenticationU;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  MVC := TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.SessionTimeout] := '30';
      Config[TMVCConfigKey.DefaultContentType] := 'text/html';
    end);
  MVC
    .AddController(TApp1MainController)
    .AddController(TAdminController)
    .AddMiddleware(TMVCBasicAuthenticationMiddleware.Create(TAuthenticationSample.Create))
    .AddMiddleware(TMVCStaticFilesMiddleware.Create(
    '/static', { StaticFilesPath }
    'www', { DocumentRoot }
    'index.html',
    False { not serving a SPA }
    ));
end;

end.
