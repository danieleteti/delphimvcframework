unit WebModuleU;

interface

uses System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework, MVCFramework.Swagger.Commons;

type
  TMyWebModule = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    FMVC: TMVCEngine;
    function GetSwagInfoV1: TMVCSwaggerInfo;
    function GetSwagInfoV2: TMVCSwaggerInfo;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TMyWebModule;

implementation

{$R *.dfm}

uses MyControllerU, System.IOUtils, MVCFramework.Commons, MVCFramework.Middleware.Compression,
  MVCFramework.Middleware.Swagger, MVCFramework.Middleware.CORS,
  MVCFramework.Middleware.StaticFiles;

function TMyWebModule.GetSwagInfoV1: TMVCSwaggerInfo;
begin
  Result.Title := 'DMVCFramework Swagger Sample (Version1)';
  Result.Version := 'v1';
  Result.Description := 'SwaggerAPI Versioning V1' + DMVCFRAMEWORK_VERSION;
  Result.ContactName := 'Daniele Teti';
  Result.ContactEmail := 'd.teti@bittime.it';
  Result.ContactUrl := 'http://www.danieleteti.it';
  Result.LicenseName := 'Apache v2';
  Result.LicenseUrl := 'https://www.apache.org/licenses/LICENSE-2.0';
end;

function TMyWebModule.GetSwagInfoV2: TMVCSwaggerInfo;
begin
  Result.Title := 'DMVCFramework Swagger Sample (Version2)';
  Result.Version := 'v2';
  Result.Description := 'SwaggerAPI Versioning V1' + DMVCFRAMEWORK_VERSION;
  Result.ContactName := 'Daniele Teti';
  Result.ContactEmail := 'd.teti@bittime.it';
  Result.ContactUrl := 'http://www.danieleteti.it';
  Result.LicenseName := 'Apache v2';
  Result.LicenseUrl := 'https://www.apache.org/licenses/LICENSE-2.0';
end;

procedure TMyWebModule.WebModuleCreate(Sender: TObject);
begin
  FMVC := TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      // session timeout (0 means session cookie)
      Config[TMVCConfigKey.SessionTimeout] := '0';
      // default content-type
      Config[TMVCConfigKey.DefaultContentType] := TMVCConstants.DEFAULT_CONTENT_TYPE;
      // default content charset
      Config[TMVCConfigKey.DefaultContentCharset] := TMVCConstants.DEFAULT_CONTENT_CHARSET;
      // unhandled actions are permitted?
      Config[TMVCConfigKey.AllowUnhandledAction] := 'false';
      // default view file extension
      Config[TMVCConfigKey.DefaultViewFileExtension] := 'html';
      // view path
      Config[TMVCConfigKey.ViewPath] := 'templates';
      // Max Record Count for automatic Entities CRUD
      Config[TMVCConfigKey.MaxEntitiesRecordCount] := '20';
      // Enable Server Signature in response
      Config[TMVCConfigKey.ExposeServerSignature] := 'true';
      // Max request size in bytes
      Config[TMVCConfigKey.MaxRequestSize] := IntToStr(TMVCConstants.DEFAULT_MAX_REQUEST_SIZE);
    end);
  FMVC.AddController(TMyControllerV1);
  FMVC.AddController(TMyControllerV2);

  FMVC.AddMiddleware(TMVCCORSMiddleware.Create);
  FMVC.AddMiddleware(TMVCStaticFilesMiddleware.Create(
    '/swagger', { StaticFilesPath }
    '.\www', { DocumentRoot }
    'index.html' { IndexDocument }
    ));
  FMVC.AddMiddleware(TMVCSwaggerMiddleware.Create(FMVC, GetSwagInfoV1, '/api/swagger-v1.json',
    'Method for authentication using JSON Web Token (JWT)', False, '','','/api/v1'));
  FMVC.AddMiddleware(TMVCSwaggerMiddleware.Create(FMVC, GetSwagInfoV2, '/api/swagger-v2.json',
    'Method for authentication using JSON Web Token (JWT)', False, '','','/api/v2', [psHTTP, psHTTPS]));
end;

procedure TMyWebModule.WebModuleDestroy(Sender: TObject);
begin
  FMVC.Free;
end;

end.
