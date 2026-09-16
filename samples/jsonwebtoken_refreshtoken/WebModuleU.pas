unit WebModuleU;

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
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

{$R *.dfm}

uses
  AuthControllerU,
  AuthenticationU,
  SharedU,
  MVCFramework.JWT,
  MVCFramework.HMAC,
  MVCFramework.Middleware.JWT,
  MVCFramework.JWT.RefreshToken;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  MVC := TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.DefaultContentType] := TMVCMediaType.APPLICATION_JSON;
    end);

  MVC
    .AddController(TAuthController)
    .AddController(TProtectedController)
    // Validates the access token on protected routes. We pass an unused login
    // segment: login is performed by TAuthController so it can return BOTH
    // tokens (the middleware auto-login renders only the access token).
    .AddMiddleware(TMVCJWTAuthenticationMiddleware.Create(
      TAuthenticationSample.Create,
      procedure(const JWT: TJWT)
      begin
        JWT.Claims.Issuer := gRefreshCfg.Issuer;
      end,
      JWT_SECRET,
      '/__jwt_autologin_unused',
      [TJWTCheckableClaim.ExpirationTime],
      0,
      HMAC_HS512))
    // Owns POST /auth/refresh: rotates the refresh token and returns a new pair.
    .AddMiddleware(TMVCJWTRefreshTokenMiddleware.Create(gRefreshCfg, gRefreshStore));
end;

end.
