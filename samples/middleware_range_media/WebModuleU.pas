unit WebModuleU;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework;

type
  TRangeMediaWebModule = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    FEngine: TMVCEngine;
  end;

var
  WebModuleClass: TComponentClass = TRangeMediaWebModule;

implementation

{$R *.dfm}

uses
  MVCFramework.Commons,
  MVCFramework.DotEnv,
  MVCFramework.View.Renderers.TemplatePro,
  MVCFramework.Middleware.RangeMedia,
  HomeControllerU;

procedure TRangeMediaWebModule.WebModuleCreate(Sender: TObject);
begin
  FEngine := TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.DefaultContentType] := TMVCMediaType.TEXT_HTML;
      Config[TMVCConfigKey.DefaultContentCharset] := TMVCConstants.DEFAULT_CONTENT_CHARSET;
      Config[TMVCConfigKey.DefaultViewFileExtension] := 'html';
      Config[TMVCConfigKey.ViewPath] := dotEnv.Env('DMVC_VIEW_PATH', 'templates');
    end);

  FEngine.SetViewEngine(TMVCTemplateProViewEngine);
  FEngine.AddController(THomeController);

  // Serve media files at /media/* with HTTP Range support (RFC 7233).
  // This enables seeking in HTML5 <audio> and <video> elements.
  FEngine.AddMiddleware(
    UseRangeMediaMiddleware('/media', 'media')
  );
end;

procedure TRangeMediaWebModule.WebModuleDestroy(Sender: TObject);
begin
  FEngine.Free;
end;

end.
