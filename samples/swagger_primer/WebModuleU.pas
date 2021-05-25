unit WebModuleU;

interface

uses System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework;

type
  TMyWebModule = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    FMVC: TMVCEngine;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TMyWebModule;

implementation

{$R *.dfm}

uses MyControllerU, System.IOUtils, MVCFramework.Commons, MVCFramework.Middleware.Compression,
  MVCFramework.Middleware.Swagger, MVCFramework.Swagger.Commons, MVCFramework.Middleware.CORS,
  MVCFramework.Middleware.StaticFiles;

procedure TMyWebModule.WebModuleCreate(Sender: TObject);
var
  lSwagInfo: TMVCSwaggerInfo;
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
  FMVC.AddController(TMyController);

  lSwagInfo.Title := 'DMVCFramework Swagger Sample';
  lSwagInfo.Version := 'v1';
  lSwagInfo.Description := 'You know, this is a demo of DMVCFramework ' + DMVCFRAMEWORK_VERSION +
    ' Swagger Support';
  lSwagInfo.ContactName := 'Daniele Teti';
  lSwagInfo.ContactEmail := 'd.teti@bittime.it';
  lSwagInfo.ContactUrl := 'http://www.danieleteti.it';
  lSwagInfo.LicenseName := 'Apache v2';
  lSwagInfo.LicenseUrl := 'https://www.apache.org/licenses/LICENSE-2.0';
  FMVC.AddMiddleware(TMVCCORSMiddleware.Create);
  FMVC.AddMiddleware(TMVCStaticFilesMiddleware.Create(
    '/swagger', { StaticFilesPath }
    '.\www', { DocumentRoot }
    'index.html' { IndexDocument }
    ));
  FMVC.AddMiddleware(TMVCSwaggerMiddleware.Create(FMVC, lSwagInfo, '/api/swagger.json',
    'Method for authentication using JSON Web Token (JWT)'));

end;

procedure TMyWebModule.WebModuleDestroy(Sender: TObject);
begin
  FMVC.Free;
end;

end.
