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
  MVCFramework.Middleware.Redirect,
  MVCFramework.Middleware.Compression,
  MVCFramework.Middleware.CORS;

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
  FMVC
    .AddController(TMyController);



  { **************************************************************************** }
  { Enable one of the following lines to configure CORS support in different way }
  { **************************************************************************** }

  { // Allows all origins -> * }
  FMVC.AddMiddleware(TMVCCORSMiddleware.Create);

  { // Allows 2 origins }
  // FMVC.AddMiddleware(TMVCCORSMiddleware.Create('https://anotherserver.com,http://localhost:9090'));

  { // Disallow localhost:9090 }
  // FMVC.AddMiddleware(TMVCCORSMiddleware.Create('https://anotherserver.com'));
end;

procedure TMyWebModule.WebModuleDestroy(Sender: TObject);
begin
  FMVC.Free;
end;

end.
