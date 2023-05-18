unit WebModuleU;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework,
  MVCFramework.Logger;

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


uses
  MainControllerU,
  System.IOUtils,
  MVCFramework.Commons,
  MVCFramework.Middleware.StaticFiles,
  MVCFramework.Middleware.Compression, SPARedirectController;

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
      Config[TMVCConfigKey.LoadSystemControllers] := 'false';
    end);
  FMVC
    .AddController(TMyController)
    .AddController(TSPARedirectController);

  // Required to enable serving of static files
  FMVC.AddMiddleware(TMVCStaticFilesMiddleware.Create(
    '/static',
    TPath.Combine(ExtractFilePath(GetModuleName(HInstance)), 'www'))
    );

  FMVC.AddMiddleware(TMVCStaticFilesMiddleware.Create(
    '/static2',
    TPath.Combine(ExtractFilePath(GetModuleName(HInstance)), 'www2'))
    );

  FMVC.AddMiddleware(TMVCStaticFilesMiddleware.Create(
    '/static3',
    TPath.Combine(ExtractFilePath(GetModuleName(HInstance)), 'www3'),
      'index.html',True,'UTF-8',
      procedure(const Context: TWebContext; var PathInfo: String; var Allow: Boolean)
      begin
        // This rule disallow any .txt file and translates file1.html into file2.html
        Allow := not PathInfo.EndsWith('.txt', True);
        if Allow and PathInfo.Contains('file1.html') then
          PathInfo := PathInfo.Replace('file1.html','file2.html');
        if not Allow then
        begin
          Context.Response.StatusCode := HTTP_STATUS.NotFound;
        end;
      end,
      procedure(const MediaTypes: TMVCStringDictionary)
      begin
        //https://github.com/danieleteti/delphimvcframework/issues/607
        //Firefox browser extension - this is an *.xpi file with a mime type of "application/x-xpinstall"
        MediaTypes.Add('.xpi', 'application/x-xpinstall');
      end)
    );

  // To enable compression (deflate, gzip) just add this middleware as the last one
  FMVC.AddMiddleware(TMVCCompressionMiddleware.Create);
end;

procedure TMyWebModule.WebModuleDestroy(Sender: TObject);
begin
  FMVC.Free;
end;

end.
