unit WebModuleUnit1;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework,
  MVCFramework.Commons;

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
  AuthenticationU,
  MVCFramework.Middleware.JWT,
  MVCFramework.JWT,
  MVCFramework.HMAC,
  MVCFramework.Middleware.StaticFiles,
  System.DateUtils;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
var
  lClaimsSetup: TJWTClaimsSetup;
begin
  lClaimsSetup := procedure(const JWT: TJWT)
    begin
      JWT.Claims.Issuer := 'Delphi MVC Framework JWT Middleware Sample';
      JWT.Claims.NotBefore := Now - OneMinute * 5; // valid since 5 minutes ago
      JWT.Claims.IssuedAt := Now;
      JWT.Claims.ExpirationTime := Now + OneSecond * 30;
      JWT.CustomClaims['mycustomvalue'] := 'hello there';
      // Here we dont use a fixed ExpirationTime but a LiveValidityWindowInSeconds
      // to make the ExpirationTime dynamic, incrementing the
      // ExpirationTime by LiveValidityWindowInSeconds seconds at each request
      JWT.LiveValidityWindowInSeconds := 10; // 60 * 60;
    end;

  MVC := TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.SessionTimeout] := '30';
      Config[TMVCConfigKey.DefaultContentType] := 'text/html';
    end);
  MVC
    .AddController(TApp1MainController)
    .AddController(TAdminController)
    .AddMiddleware(TMVCJWTAuthenticationMiddleware.Create(
      TAuthenticationSample.Create,
      lClaimsSetup,
      'mys3cr37',
      '/login',
      [TJWTCheckableClaim.ExpirationTime, TJWTCheckableClaim.NotBefore, TJWTCheckableClaim.IssuedAt],
      0,
      HMAC_HS512
    // just for test, Leeway seconds is zero.
    ));
end;

end.
