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
  // Configure JWT claims
  lClaimsSetup := procedure(const JWT: TJWT)
    begin
      JWT.Claims.Issuer := 'DMVCFramework JWT Cookie Sample';
      JWT.Claims.Subject := 'JWT Cookie Authentication';
      JWT.Claims.ExpirationTime := Now + OneHour; // valid for 1 hour
      JWT.Claims.NotBefore := Now - OneMinute * 5; // valid since 5 minutes ago
      JWT.Claims.IssuedAt := Now;
      JWT.CustomClaims['app'] := 'JWTCookieSample';
    end;

  MVC := TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.DefaultContentType] := 'text/html';
    end);

  MVC
    .AddController(TPublicController)
    .AddController(TAdminController)
    // Use the new secure JWT Cookie middleware
    // Default settings: Secure=True, SameSite=Strict, HttpOnly=True
    .AddMiddleware(
      UseJWTCookieAuthentication(
        TAuthenticationSample.Create,
        lClaimsSetup,
        'MySecretKeyForJWT_ChangeInProduction!',  // Secret key
        '/login',   // Login URL
        '/logout',  // Logout URL
        [
          TJWTCheckableClaim.ExpirationTime,
          TJWTCheckableClaim.NotBefore,
          TJWTCheckableClaim.IssuedAt
        ],
        300  // Leeway seconds
      )
      // For local HTTP development, disable Secure flag
      // In production with HTTPS, remove this line!
      .SetCookieSecure(False)
      // You can also customize cookie name if needed:
      // .SetCookieName('my_jwt_token')
      // Or SameSite policy:
      // .SetCookieSameSite(ssLax)
    )
    .AddMiddleware(UseStaticFilesMiddleware('/static', '..\..\www'));
end;

end.
