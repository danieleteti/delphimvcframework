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
  TOIDCDockerSampleWebModule = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    FEngine: TMVCEngine;
    procedure ConfigureDatabase;
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
  WebModuleClass: TComponentClass = TOIDCDockerSampleWebModule;

implementation

{$R *.dfm}

uses
  System.DateUtils,
  MVCFramework.Commons,
  MVCFramework.DotEnv,
  MVCFramework.JWT,
  MVCFramework.Logger,
  MVCFramework.View.Renderers.TemplatePro,
  MVCFramework.Middleware.OIDC,
  MVCFramework.Middleware.ActiveRecord,
  MVCFramework.ActiveRecord,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Phys.PGDef,
  FireDAC.Phys.PG,
  FireDAC.DApt,
  FireDAC.Comp.Client,
  AppConfigU,
  MigrationServiceU,
  UserEntityU,
  HomeControllerU,
  DashboardControllerU,
  UserControllerU,
  HealthControllerU;

procedure TOIDCDockerSampleWebModule.ConfigureDatabase;
var
  LParams: TStringList;
begin
  LParams := TStringList.Create;
  try
    LParams.Add('Database=' + TAppConfig.DBName);
    LParams.Add('Server=' + TAppConfig.DBHost);
    LParams.Add('Port=' + TAppConfig.DBPort.ToString);
    LParams.Add('User_Name=' + TAppConfig.DBUser);
    LParams.Add('Password=' + TAppConfig.DBPassword);
    LParams.Add('Pooled=True');
    LParams.Add('POOL_MaximumItems=20');
    FDManager.AddConnectionDef('pgconnection', 'PG', LParams);
  finally
    LParams.Free;
  end;
end;

procedure TOIDCDockerSampleWebModule.WebModuleCreate(Sender: TObject);
begin
  // Database connection pool
  ConfigureDatabase;

  // Run database migrations
  try
    var LConn := TFDConnection.Create(nil);
    try
      LConn.ConnectionDefName := 'pgconnection';
      LConn.Connected := True;
      TMigrationService.ApplyMigrations(LConn);
    finally
      LConn.Free;
    end;
  except
    on E: Exception do
      LogE('Database migration failed: ' + E.Message);
  end;

  FEngine := TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.DefaultContentType] := TMVCMediaType.TEXT_HTML;
      Config[TMVCConfigKey.DefaultContentCharset] := TMVCConstants.DEFAULT_CONTENT_CHARSET;
      Config[TMVCConfigKey.AllowUnhandledAction] := 'false';
      Config[TMVCConfigKey.DefaultViewFileExtension] := 'html';
      Config[TMVCConfigKey.ViewPath] := dotEnv.Env('DMVC_VIEW_PATH', 'templates');
      Config[TMVCConfigKey.ViewCache] := dotEnv.Env('DMVC_VIEW_CACHE', 'false');
      Config[TMVCConfigKey.ExposeServerSignature] := 'false';
      Config[TMVCConfigKey.ExposeXPoweredBy] := 'false';
    end);

  // View engine
  FEngine.SetViewEngine(TMVCTemplateProViewEngine);

  // Controllers
  FEngine.AddController(THomeController);
  FEngine.AddController(TDashboardController);
  FEngine.AddController(TUserController);
  FEngine.AddController(THealthController);

  // Middleware stack (order matters)
  // 1. ActiveRecord (DB connection per request - must be before OIDC)
  FEngine.AddMiddleware(TMVCActiveRecordMiddleware.Create('pgconnection'));
  // 2. OIDC authentication
  FEngine.AddMiddleware(
    UseOIDCAuthentication(
      HandleOIDCUser, HandleAuthRequired,
      TAppConfig.OIDCIssuer, TAppConfig.OIDCClientId,
      TAppConfig.OIDCClientSecret, TAppConfig.OIDCRedirectUri,
      TAppConfig.JWTSecret,
      '/auth/login', '/auth/callback', '/auth/logout',
      '/dashboard', '/',
      'openid email profile',
      TAppConfig.JWTExpirationHours * 60,
      [TJWTCheckableClaim.ExpirationTime, TJWTCheckableClaim.IssuedAt],
      300
    )
    .SetCookieName('oidc_docker_session')
    .SetCookieSecure(TAppConfig.BaseURL.StartsWith('https'))
  );
end;

procedure TOIDCDockerSampleWebModule.HandleOIDCUser(
  const AContext: TWebContext;
  const AIDTokenClaims: TJsonObject;
  const AUserInfo: TJsonObject;
  const AUserRoles: TList<string>;
  const ASessionData: TDictionary<string, string>);
var
  LUser: TUser;
  LEmail, LDisplayName: string;
  LUserCount: Int64;
begin
  LEmail := '';
  LDisplayName := '';
  if Assigned(AUserInfo) then
  begin
    LEmail := AUserInfo.S['email'];
    LDisplayName := AUserInfo.S['name'];
    if LDisplayName.IsEmpty then
      LDisplayName := AUserInfo.S['preferred_username'];
  end;

  // Try to find existing user by OIDC subject
  LUser := TMVCActiveRecord.GetFirstByWhere<TUser>(
    'oidc_subject = ?', [AIDTokenClaims.S['sub']], False);

  if Assigned(LUser) then
  begin
    LUser.Email := LEmail;
    LUser.DisplayName := LDisplayName;
    LUser.LastLoginAt := Now;
    LUser.Update;
  end
  else
  begin
    LUser := TUser.Create;
    LUser.OidcSubject := AIDTokenClaims.S['sub'];
    LUser.Email := LEmail;
    LUser.DisplayName := LDisplayName;
    LUser.LastLoginAt := Now;

    // First user to register becomes admin
    LUserCount := TMVCActiveRecord.Count<TUser>;
    if LUserCount = 0 then
      LUser.Role := 'admin'
    else
      LUser.Role := 'viewer';

    LUser.Insert;
    LogI('New user created: ' + LEmail + ' with role: ' + LUser.Role);
  end;

  try
    AUserRoles.Add(LUser.Role);
    ASessionData.AddOrSetValue('user_id', LUser.Id.Value.ToString);
    ASessionData.AddOrSetValue('display_name', LUser.DisplayName.Value);
    ASessionData.AddOrSetValue('email', LEmail);
    ASessionData.AddOrSetValue('role', LUser.Role);
    ASessionData.AddOrSetValue('sub', AIDTokenClaims.S['sub']);
  finally
    LUser.Free;
  end;
end;

procedure TOIDCDockerSampleWebModule.HandleAuthRequired(
  const AContext: TWebContext;
  const AControllerQualifiedClassName: string;
  const AActionName: string;
  var AAuthenticationRequired: Boolean);
begin
  AAuthenticationRequired := True;

  if AControllerQualifiedClassName.Contains('THealthController') then
    AAuthenticationRequired := False
  else if AControllerQualifiedClassName.Contains('THomeController') then
    AAuthenticationRequired := False;
end;

procedure TOIDCDockerSampleWebModule.WebModuleDestroy(Sender: TObject);
begin
  FEngine.Free;
end;

end.
