unit WebModuleUnit1;

interface

uses System.SysUtils, System.Classes, Web.HTTPApp, MVCFramework, MVCFramework.Commons;

type
  TWebModule1 = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
  private
    FMVC: TMVCEngine;
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

{$R *.dfm}

uses AppControllerU, System.Generics.Collections, AuthenticationU, MVCFramework.Middleware.JWT, MVCFramework.JWT,
  System.DateUtils;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
var
  LClaimsSetup: TJWTClaimsSetup;
begin
  LClaimsSetup := procedure(const JWT: TJWT)
    begin
      JWT.Claims.Issuer := 'Delphi MVC Framework JWT Middleware Sample';
      JWT.Claims.ExpirationTime := Now + OneHour; // valid for 1 hour
      JWT.Claims.NotBefore := Now - OneMinute * 5; // valid since 5 minutes ago
      JWT.Claims.IssuedAt := Now;
      JWT.CustomClaims['mycustomvalue'] := 'hello there';
    end;

  FMVC := TMVCEngine.Create(Self);
  FMVC.Config[TMVCConfigKey.DocumentRoot] := '..\..\www';
  FMVC.Config[TMVCConfigKey.SessionTimeout] := '30';
  FMVC.Config[TMVCConfigKey.DefaultContentType] := 'text/html';

  FMVC.AddController(TApp1MainController).AddController(TAdminController)
    .AddMiddleware(TMVCJWTAuthenticationMiddleware.Create(
      TAuthenticationSample.Create, 'mys3cr37', '/login', LClaimsSetup,
      [TJWTCheckableClaim.ExpirationTime, TJWTCheckableClaim.NotBefore, TJWTCheckableClaim.IssuedAt], 300));
end;

end.
