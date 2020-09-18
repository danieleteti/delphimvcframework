unit WebModuleU;

interface

uses System.SysUtils,
     System.Classes,
     Web.HTTPApp,
     MVCFramework;

type
  TMainWebModule = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    FMVC: TMVCEngine;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TMainWebModule;

implementation

{$R *.dfm}

uses
  MainControllerU,
  System.IOUtils,
  MVCFramework.Commons,
  MVCFramework.Middleware.Compression,
  MVCFramework.Middleware.StaticFiles;

procedure TMainWebModule.WebModuleCreate(Sender: TObject);
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
      //default view file extension
      Config[TMVCConfigKey.DefaultViewFileExtension] := 'html';
      //view path
      Config[TMVCConfigKey.ViewPath] := 'templates';
      //Max Record Count for automatic Entities CRUD
      Config[TMVCConfigKey.MaxEntitiesRecordCount] := '20';
      //Enable Server Signature in response
      Config[TMVCConfigKey.ExposeServerSignature] := 'true';
      // Max request size in bytes
      Config[TMVCConfigKey.MaxRequestSize] := IntToStr(TMVCConstants.DEFAULT_MAX_REQUEST_SIZE);
    end);
//  FMVC.AddController(TMyController);
  FMVC.AddMiddleware(TMVCStaticFilesMiddleware.Create(
    '/swagger', { StaticFilesPath }
    TPath.Combine(ExtractFilePath(GetModuleName(HInstance)), '..\www'), { DocumentRoot }
    'index.html' {IndexDocument - Before it was named fallbackresource}
    ));
  // To enable compression (deflate, gzip) just add this middleware as the last one
  FMVC.AddMiddleware(TMVCCompressionMiddleware.Create);
  FMVC.AddController(TRedirectController);
end;

procedure TMainWebModule.WebModuleDestroy(Sender: TObject);
begin
  FMVC.Free;
end;

end.
