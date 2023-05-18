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
  MVCFramework.Middleware.StaticFiles,
  MVCFramework.JWT,
  System.DateUtils;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
var
  lClaimsSetup: TJWTClaimsSetup;
begin
  lClaimsSetup := procedure(const JWT: TJWT)
    begin
      JWT.Claims.Issuer := 'Delphi MVC Framework JWT Middleware Sample';
      if TMVCWebRequest(JWT.Data).QueryStringParamExists('rememberme') then
      begin
        JWT.Claims.ExpirationTime := Now + (OneHour * 10); // valid for 10 hour
      end
      else
      begin
        JWT.Claims.ExpirationTime := Now + OneHour; // valid for 1 hour
      end;
      JWT.Claims.NotBefore := Now - OneMinute * 5; // valid since 5 minutes ago
      JWT.Claims.IssuedAt := Now;
      JWT.CustomClaims['mycustomvalue'] := 'hello there';
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
    .AddMiddleware(
      TMVCJWTAuthenticationMiddleware.Create(
        TAuthenticationSample.Create,
        lClaimsSetup,
        'mys3cr37',
        '/login',
        [
          TJWTCheckableClaim.ExpirationTime,
          TJWTCheckableClaim.NotBefore,
          TJWTCheckableClaim.IssuedAt
        ], 300))
    .AddMiddleware(TMVCStaticFilesMiddleware.Create(
    '/static', { StaticFilesPath }
    '..\..\www' { DocumentRoot }
    ));
end;

end.
