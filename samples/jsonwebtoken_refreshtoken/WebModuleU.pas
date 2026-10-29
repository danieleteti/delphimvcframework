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
  ProtectedControllerU,
  AuthenticationU,
  SharedU,
  MVCFramework.Filters.Authentication;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
var
  lOptions: TMVCJWTAuthenticationOptions;
begin
  MVC := TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.DefaultContentType] := TMVCMediaType.APPLICATION_JSON;
    end);

  // With a refresh token store the bundle answers, on one secret:
  //   POST /auth/login    credentials -> {access_token, refresh_token, token_type, expires_in}
  //   POST /auth/refresh  {"refresh_token"} -> a new pair (the old refresh token is spent)
  // and checks the access token on the protected routes.
  lOptions := TMVCJWTAuthenticationOptions.Default;
  lOptions.Handler := TAuthenticationSample.Create;
  lOptions.Secret := JWT_SECRET;
  lOptions.LoginURL := '/auth/login';
  lOptions.LeewaySeconds := 0;                              // no grace: expiry is observable at 30 s
  lOptions.RefreshToken.AccessTokenTTLSeconds := 30;        // short, to make refresh observable
  lOptions.RefreshToken.RefreshTokenTTLSeconds := 24 * 3600; // 1 day
  lOptions.RefreshTokenStore := gRefreshStore;              // RefreshToken.RefreshURLSegment = '/auth/refresh'

  MVC
    .AddController(TProtectedController)
    .Use(JWTAuthenticationBundle(lOptions));
end;

end.
