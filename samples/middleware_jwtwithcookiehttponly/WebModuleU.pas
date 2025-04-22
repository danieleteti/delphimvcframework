unit WebModuleU;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework;

type
  TMyWebModule = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    FMVC: TMVCEngine;
  end;

var
  WebModuleClass: TComponentClass = TMyWebModule;

implementation

{$R *.dfm}

uses
  System.IOUtils,
  MVCFramework.Commons,
  MVCFramework.View.Renderers.TemplatePro,
  MVCFramework.Middleware.ActiveRecord,
  MVCFramework.Middleware.StaticFiles,
  MVCFramework.Middleware.Analytics,
  MVCFramework.Middleware.Trace,
  MVCFramework.Middleware.CORS,
  MVCFramework.Middleware.ETag,
  MVCFramework.Middleware.JWTCookie,
  MVCFramework.Middleware.Compression,
  ControllerU, AuthenticationU,
  MVCFramework.JWT, System.DateUtils, MVCFramework.HMAC;

procedure TMyWebModule.WebModuleCreate(Sender: TObject);
var
  LClaimsSetup: TJWTClaimsSetup;
begin
  LClaimsSetup := procedure(const JWT: TJWT)
    begin
      JWT.Claims.Issuer := 'Delphi MVC Framework JWT Middleware Sample';
      JWT.Claims.NotBefore := Now - OneMinute * 5; // valid since 5 minutes ago
      JWT.Claims.IssuedAt := Now;
      JWT.Claims.ExpirationTime := Now + OneMinute * 5;
      JWT.CustomClaims['mycustomvalue'] := 'THIS IS A CUSTOM CLAIM!';
      // Here we dont use a fixed ExpirationTime but a LiveValidityWindowInSeconds
      // to make the ExpirationTime dynamic, incrementing the
      // ExpirationTime by LiveValidityWindowInSeconds seconds at each request
      JWT.LiveValidityWindowInSeconds := 10; // 60 * 60;
    end;

  FMVC := TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      // default content-type
      Config[TMVCConfigKey.DefaultContentType] := DotEnv.Env('dmvc.default.content_type', TMVCMediaType.TEXT_HTML);
      // default content charset
      Config[TMVCConfigKey.DefaultContentCharset] := DotEnv.Env('dmvc.default.content_charset', TMVCConstants.DEFAULT_CONTENT_CHARSET);
      // unhandled actions are permitted?
      Config[TMVCConfigKey.AllowUnhandledAction] := DotEnv.Env('dmvc.allow_unhandled_actions', 'false');
      // enables or not system controllers loading (available only from localhost requests)
      Config[TMVCConfigKey.LoadSystemControllers] := DotEnv.Env('dmvc.load_system_controllers', 'true');
      // default view file extension
      Config[TMVCConfigKey.DefaultViewFileExtension] := DotEnv.Env('dmvc.default.view_file_extension', 'html');
      // view path
      Config[TMVCConfigKey.ViewPath] := DotEnv.Env('dmvc.view_path', 'templates');
      // use cache for server side views (use "false" in debug and "true" in production for faster performances
      Config[TMVCConfigKey.ViewCache] := DotEnv.Env('dmvc.view_cache', 'false');
      // Max Record Count for automatic Entities CRUD
      Config[TMVCConfigKey.MaxEntitiesRecordCount] := DotEnv.Env('dmvc.max_entities_record_count',
        IntToStr(TMVCConstants.MAX_RECORD_COUNT));
      // Enable Server Signature in response
      Config[TMVCConfigKey.ExposeServerSignature] := DotEnv.Env('dmvc.expose_server_signature', 'false');
      // Enable X-Powered-By Header in response
      Config[TMVCConfigKey.ExposeXPoweredBy] := DotEnv.Env('dmvc.expose_x_powered_by', 'true');
      // Max request size in bytes
      Config[TMVCConfigKey.MaxRequestSize] := DotEnv.Env('dmvc.max_request_size', IntToStr(TMVCConstants.DEFAULT_MAX_REQUEST_SIZE));
    end);

  // Controllers
  FMVC.AddController(TMyController);
  // Controllers - END

  // Middlewares
  FMVC.AddMiddleware(TMVCJWTCookieAuthenticationMiddleware.Create(
    TAuthenticationSample.Create,
    LClaimsSetup,
    '/login',
    '/logoff',
    'mys3cr37',
    'email',
    'password',
    [TJWTCheckableClaim.ExpirationTime, TJWTCheckableClaim.NotBefore, TJWTCheckableClaim.IssuedAt],
    0,     // just for test, Leeway seconds is zero.
    HMAC_HS512
    ));
  // Middlewares - END

  // Server Side View
  FMVC.SetViewEngine(TMVCTemplateProViewEngine);
  // Server Side View - END

end;

procedure TMyWebModule.WebModuleDestroy(Sender: TObject);
begin
  FMVC.Free;
end;

end.
