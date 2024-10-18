unit WebModuleMainU;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework;

type
  TWebModule1 = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    { Private declarations }
    FEngine: TMVCEngine;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

uses
  MVCFramework.Commons,
  MVCFramework.Controllers.Register,
  MVCFramework.Middleware.Swagger,
  MVCFramework.Swagger.Commons,
  MVCFramework.Middleware.JWT,
  MVCFramework.Middleware.StaticFiles,
  AuthHandler,
  MVCFramework.JWT,
  System.DateUtils,
  ControllersU, BaseControllerU, MVCFramework.Middleware.Redirect;

{%CLASSGROUP 'Vcl.Controls.TControl'}
{$R *.dfm}


procedure TWebModule1.WebModuleCreate(Sender: TObject);
var
  LSwagInfo: TMVCSwaggerInfo;
  LClaimsSetup: TJWTClaimsSetup;
begin
  FEngine := TMVCEngine.Create(Self,
    procedure(AConfig: TMVCConfig)
    begin
      AConfig[TMVCConfigKey.PathPrefix] := '/api';
      AConfig[TMVCConfigKey.LoadSystemControllers] := 'false';
    end
    );

  LSwagInfo.Title := 'Sample Swagger API - Extended';
  LSwagInfo.Version := 'v1';
  LSwagInfo.TermsOfService := 'http://www.apache.org/licenses/LICENSE-2.0.txt';
  LSwagInfo.Description := 'Swagger Documentation Example';
  LSwagInfo.ContactName := 'Daniele Teti';
  LSwagInfo.ContactEmail := 'd.teti@bittime.it';
  LSwagInfo.ContactUrl := 'https://github.com/danieleteti';
  LSwagInfo.LicenseName := 'Apache License - Version 2.0, January 2004';
  LSwagInfo.LicenseUrl := 'http://www.apache.org/licenses/LICENSE-2.0';
  FEngine.AddMiddleware(TMVCSwaggerMiddleware.Create(FEngine, LSwagInfo, '/api/swagger.json',
    'Method for authentication using JSON Web Token (JWT)',
    False
//    ,'api.dmvcframework.com', '/'  { Define a custom host and BasePath when your API uses a dns for external access }
    ));

  LClaimsSetup := procedure(const JWT: TJWT)
    begin
      JWT.Claims.Issuer := 'Delphi MVC Framework Swagger Documentation';
      JWT.Claims.ExpirationTime := Now + OneHour;  // valid for 1 hour
      JWT.Claims.NotBefore := Now - OneMinute * 5; // valid since 5 minutes ago
      JWT.Claims.IssuedAt := Now;
    end;

  FEngine.AddMiddleware(TMVCJWTAuthenticationMiddleware.Create(
    TAuthHandler.Create,
    LClaimsSetup,
    'D3lph1MVCFram3w0rk',
    '/api/login',
    [TJWTCheckableClaim.ExpirationTime, TJWTCheckableClaim.NotBefore, TJWTCheckableClaim.IssuedAt]
    ));
  FEngine.AddMiddleware(TMVCStaticFilesMiddleware.Create(
    '/swagger',  { StaticFilesPath }
    '.\www',     { DocumentRoot }
    'index.html' { IndexDocument }
    ));
  FEngine.AddMiddleware(TMVCRedirectMiddleware.Create(['/'], '/swagger'));

  FEngine.AddController(TPeopleController);
  FEngine.AddController(TTallPeopleController);
  FEngine.AddController(TMyPeopleController);
  /// Add your registered controllers to engine.
end;

procedure TWebModule1.WebModuleDestroy(Sender: TObject);
begin
  FEngine.Free;
end;

end.
