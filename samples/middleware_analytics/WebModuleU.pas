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
  LoggerPro.FileAppender,
  LoggerPro,
  MVCFramework.Middleware.Analytics;

var
  GLogWriter: ILogWriter = nil;
  GLock: TObject = nil;

function GetLoggerForAnalytics: ILogWriter;
begin
  if GLogWriter = nil then
  begin
    TMonitor.Enter(GLock);
    try
      if GLogWriter = nil then // double check locking (https://en.wikipedia.org/wiki/Double-checked_locking)
      begin
        GLogWriter := BuildLogWriter([TLoggerProFileAppender.Create(5, 2000, AppPath + 'analytics')]);
      end;
    finally
      TMonitor.Exit(GLock);
    end;
  end;
  Result := GLogWriter;
end;

procedure TMyWebModule.WebModuleCreate(Sender: TObject);
begin
  FMVC := TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      // enable static files
      Config[TMVCConfigKey.DocumentRoot] := TPath.Combine(ExtractFilePath(GetModuleName(HInstance)), 'www');
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
      // Enable Server Signature in response
      Config[TMVCConfigKey.ExposeServerSignature] := 'false';
      // Define a default URL for requests that don't map to a route or a file (useful for client side web app)
      Config[TMVCConfigKey.FallbackResource] := 'index.html';
    end);
  FMVC.AddController(TMainController).AddMiddleware(TMVCAnalyticsMiddleware.Create(GetLoggerForAnalytics));
end;

procedure TMyWebModule.WebModuleDestroy(Sender: TObject);
begin
  FMVC.Free;
end;

initialization

GLock := TObject.Create;

finalization

GLock.Free;

end.
