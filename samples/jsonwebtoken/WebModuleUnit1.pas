unit WebModuleUnit1;

interface

uses System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework;

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


uses AppControllerU, System.Generics.Collections,
  AuthenticationU,
  MVCFramework.Middleware.JWT;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  MVC := TMVCEngine.Create(Self);
  MVC.Config[TMVCConfigKey.DocumentRoot] := '..\..\www';
  MVC.Config[TMVCConfigKey.SessionTimeout] := '30';
  MVC.Config[TMVCConfigKey.DefaultContentType] := 'text/html';
  MVC.AddController(TApp1MainController).AddController(TAdminController)
    .AddMiddleware(TMVCJWTAuthenticationMiddleware.Create(
    TAuthenticationSample.Create,
    'mys3cr37'));
end;

end.
