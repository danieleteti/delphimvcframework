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
  TOIDCJWKSSampleWebModule = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
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
  WebModuleClass: TComponentClass = TOIDCJWKSSampleWebModule;

implementation

{$R *.dfm}

uses
  MVCFramework.Commons,
  MVCFramework.DotEnv,
  MVCFramework.JWT,
  MVCFramework.Middleware.OIDC,
  // JWKS support - opt-in, requires TaurusTLS + OpenSSL DLLs at runtime.
  // Without this unit, OIDC works but relies on TLS trust for ID token validation.
  // With this unit, ID token signatures are cryptographically verified against
  // the provider's public keys fetched from the JWKS endpoint.
  MVCFramework.OIDC.JWKS,
  MVCFramework.View.Renderers.TemplatePro,
  HomeControllerU;

procedure TOIDCJWKSSampleWebModule.WebModuleCreate(Sender: TObject);
var
  lOIDCMiddleware: TMVCOIDCAuthenticationMiddleware;
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

  // Create the OIDC middleware
  lOIDCMiddleware := UseOIDCAuthentication(
    HandleOIDCUser,
    HandleAuthRequired,
    dotEnv.Env('OIDC_ISSUER', ''),
    dotEnv.Env('OIDC_CLIENT_ID', ''),
    dotEnv.Env('OIDC_CLIENT_SECRET', ''),
    dotEnv.Env('BASE_URL', 'http://localhost:8080') + '/auth/callback',
    dotEnv.Env('JWT_SECRET', ''),
    '/auth/login',
    '/auth/callback',
    '/auth/logout',
    '/dashboard',
    '/',
    'openid email profile',
    dotEnv.Env('JWT_EXPIRATION_HOURS', 8) * 60,
    [TJWTCheckableClaim.ExpirationTime, TJWTCheckableClaim.IssuedAt],
    300
  );

  // Cookie settings
  lOIDCMiddleware
    .SetCookieName('oidc_jwks_session')
    .SetCookieSecure(False)  // Set True for HTTPS in production
    .SetBaseURL(dotEnv.Env('BASE_URL', 'http://localhost:8080'));

  // ---------------------------------------------------------------
  // JWKS SIGNATURE VERIFICATION (opt-in)
  // ---------------------------------------------------------------
  // When enabled, the middleware verifies the cryptographic signature
  // of ID tokens against the OIDC provider's public keys.
  //
  // TMVCJWKSClient.CreateFromIssuer() does:
  //   1. Fetches .well-known/openid-configuration from the issuer
  //   2. Extracts the jwks_uri field
  //   3. Downloads public keys (RSA, EC, Ed25519) in JWK format
  //   4. Caches them (default: 1 hour TTL, auto-refetches on key rotation)
  //   5. Converts JWK to PEM and creates the appropriate IJWTSigner
  //
  // This requires:
  //   - MVCFramework.OIDC.JWKS in the uses clause
  //   - TaurusTLS in the search path
  //   - OpenSSL DLLs (1.1.1+ or 3.x) in the application directory
  // ---------------------------------------------------------------
  if dotEnv.Env('OIDC_VERIFY_SIGNATURE', True) then
    lOIDCMiddleware.SetJWKSProvider(
      TMVCJWKSClient.CreateFromIssuer(
        dotEnv.Env('OIDC_ISSUER', ''),
        3600  // Cache TTL: 1 hour
      )
    );

  FEngine.AddMiddleware(lOIDCMiddleware);
end;

procedure TOIDCJWKSSampleWebModule.HandleOIDCUser(
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

procedure TOIDCJWKSSampleWebModule.HandleAuthRequired(
  const AContext: TWebContext;
  const AControllerQualifiedClassName: string;
  const AActionName: string;
  var AAuthenticationRequired: Boolean);
begin
  AAuthenticationRequired := not (SameText(AActionName, 'Index') and
    SameText(AControllerQualifiedClassName, 'HomeControllerU.THomeController'));
end;

procedure TOIDCJWKSSampleWebModule.WebModuleDestroy(Sender: TObject);
begin
  FEngine.Free;
end;

end.
