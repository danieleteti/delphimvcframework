unit WebModule.Main;

interface

uses 
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework;

type
  TwmMain = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    FMVC: TMVCEngine;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TwmMain;

implementation

{$R *.dfm}

uses 
  Controller.Customers,
  System.IOUtils, 
  MVCFramework.Commons, 
  MVCFramework.Middleware.StaticFiles, 
  MVCFramework.Middleware.Compression,
  MVCFramework.Middleware.CORS;

procedure TwmMain.WebModuleCreate(Sender: TObject);
begin
  FMVC := TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      // session timeout (0 means session cookie)
      Config[TMVCConfigKey.SessionTimeout] := '0';
      //default content-type
      Config[TMVCConfigKey.DefaultContentType] := TMVCConstants.DEFAULT_CONTENT_TYPE;
      //default content charset
      Config[TMVCConfigKey.DefaultContentCharset] := TMVCConstants.DEFAULT_CONTENT_CHARSET;
      //unhandled actions are permitted?
      Config[TMVCConfigKey.AllowUnhandledAction] := 'false';
      //enables or not system controllers loading (available only from localhost requests)
      Config[TMVCConfigKey.LoadSystemControllers] := 'true';
      //default view file extension
      Config[TMVCConfigKey.DefaultViewFileExtension] := 'html';
      //view path
      Config[TMVCConfigKey.ViewPath] := 'templates';
      //Max Record Count for automatic Entities CRUD
      Config[TMVCConfigKey.MaxEntitiesRecordCount] := '200';
      //Enable Server Signature in response
      Config[TMVCConfigKey.ExposeServerSignature] := 'true';
      // Max request size in bytes
      Config[TMVCConfigKey.MaxRequestSize] := IntToStr(TMVCConstants.DEFAULT_MAX_REQUEST_SIZE);
    end);
  FMVC.AddController(TCustomersController);
  FMVC.AddMiddleware(TMVCCORSMiddleware.Create());

  // Required to enable serving of static files 
  // Remove the following middleware declaration if you don't  
  // serve static files from this dmvcframework service.
  FMVC.AddMiddleware(TMVCStaticFilesMiddleware.Create( 
      '/static', 
      TPath.Combine(ExtractFilePath(GetModuleName(HInstance)), 'www')) 
    );	

  // To enable compression (deflate, gzip) just add this middleware as the last one 
  FMVC.AddMiddleware(TMVCCompressionMiddleware.Create);
end;

procedure TwmMain.WebModuleDestroy(Sender: TObject);
begin
  FMVC.Free;
end;

end.
