unit WebModuleU;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Web.HTTPApp,
  MVCFramework,
  JsonDataObjects;

type
  TOIDCSampleWebModule = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
  private
    FEngine: TMVCEngine;
    procedure HandleOIDCUser(
      const AContext: TWebContext;
      const AIDTokenClaims: TJsonObject;
      const AUserInfo: TJsonObject;
      const AUserRoles: TList<string>;
      const ASessionData: TDictionary<string, string>);
    procedure HandleAuthRequired(
      const AContext: TWebContext;
      const AControllerQualifiedClassName: string;
      const AActionName: string;
      var AAuthenticationRequired: Boolean);
  end;

var
  WebModuleClass: TComponentClass = TOIDCSampleWebModule;

implementation

{$R *.dfm}

uses
  MVCFramework.Commons,
  MVCFramework.DotEnv,
  MVCFramework.JWT,
  MVCFramework.Middleware.OIDC,
  MVCFramework.View.Renderers.TemplatePro,
  HomeControllerU;

procedure TOIDCSampleWebModule.WebModuleCreate(Sender: TObject);
begin
  FEngine := TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.DefaultContentType] := TMVCMediaType.TEXT_HTML;
      Config[TMVCConfigKey.DefaultContentCharset] := TMVCConstants.DEFAULT_CONTENT_CHARSET;
      Config[TMVCConfigKey.DefaultViewFileExtension] := 'html';
      Config[TMVCConfigKey.ViewPath] := dotEnv.Env('DMVC_VIEW_PATH', 'templates');
    end);

  FEngine.SetViewEngine(TMVCTemplateProViewEngine);
  FEngine.AddController(THomeController);

  FEngine.AddMiddleware(
    UseOIDCAuthentication(
      HandleOIDCUser,
      HandleAuthRequired,
      dotEnv.Env('OIDC_ISSUER', ''),
      dotEnv.Env('OIDC_CLIENT_ID', ''),
      dotEnv.Env('OIDC_CLIENT_SECRET', ''),
      dotEnv.Env('OIDC_REDIRECT_URI', 'http://localhost:8080/auth/callback'),
      dotEnv.Env('JWT_SECRET', 'oidc-sample-dev-secret-change-me'),
      '/auth/login',
      '/auth/callback',
      '/auth/logout',
      '/dashboard',
      '/',
      'openid email profile',
      dotEnv.Env('JWT_EXPIRATION_MINUTES', 480),
      [TJWTCheckableClaim.ExpirationTime, TJWTCheckableClaim.IssuedAt],
      300
    )
    .SetCookieName('oidc_sample_session')
    .SetCookieSecure(False)
  );
end;

procedure TOIDCSampleWebModule.HandleOIDCUser(
  const AContext: TWebContext;
  const AIDTokenClaims: TJsonObject;
  const AUserInfo: TJsonObject;
  const AUserRoles: TList<string>;
  const ASessionData: TDictionary<string, string>);
var
  LDisplayName: string;
begin
  ASessionData.AddOrSetValue('sub', AIDTokenClaims.S['sub']);

  if Assigned(AUserInfo) then
  begin
    ASessionData.AddOrSetValue('email', AUserInfo.S['email']);
    LDisplayName := AUserInfo.S['name'];
    if LDisplayName.IsEmpty then
      LDisplayName := AUserInfo.S['preferred_username'];
    ASessionData.AddOrSetValue('display_name', LDisplayName);
  end
  else
  begin
    ASessionData.AddOrSetValue('email', AIDTokenClaims.S['email']);
    ASessionData.AddOrSetValue('display_name', AIDTokenClaims.S['name']);
  end;

  AUserRoles.Add('user');
end;

procedure TOIDCSampleWebModule.HandleAuthRequired(
  const AContext: TWebContext;
  const AControllerQualifiedClassName: string;
  const AActionName: string;
  var AAuthenticationRequired: Boolean);
begin
  AAuthenticationRequired := not SameText(AActionName, 'Index');
end;

end.
