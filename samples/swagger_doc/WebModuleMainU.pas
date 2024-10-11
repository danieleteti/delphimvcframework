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
  System.DateUtils;

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

  LSwagInfo.Title := 'Sample Swagger API';
  LSwagInfo.Version := 'v1';
  LSwagInfo.TermsOfService := 'http://www.apache.org/licenses/LICENSE-2.0.txt';
  LSwagInfo.Description := 'Swagger Documentation Example';
  LSwagInfo.ContactName := 'João Antônio Duarte';
  LSwagInfo.ContactEmail := 'joao.antonioduarte@hotmail.com';
  LSwagInfo.ContactUrl := 'https://github.com/joaoduarte19';
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
    '/swagger',         { StaticFilesPath }
    '.\www',     { DocumentRoot }
    'index.html' { IndexDocument - Before it was named fallbackresource }
    ));

  /// Add your registered controllers to engine.
  /// Only registered controls such as "MyServerName" will be added
  TControllersRegister.Instance.AddControllersInEngine(FEngine, 'MyServerName');
end;

procedure TWebModule1.WebModuleDestroy(Sender: TObject);
begin
  FEngine.Free;
end;

end.
