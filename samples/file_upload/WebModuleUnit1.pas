unit WebModuleUnit1;

interface

uses System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework;

type
  TWebModule1 = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);

  private
    MVC: TMVCEngine;

    { Private declarations }
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

uses
  FileUploadControllerU,
  MVCFramework.Commons,
  MVCFramework.View.Renderers.Mustache,
  MVCFramework.Middleware.Trace,
  MVCFramework.Middleware.StaticFiles;

{$R *.dfm}


procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  MVC := TMVCEngine.Create(self,
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.ViewPath] :=
        ExtractFilePath(GetModuleName(HInstance)) + '..\templates';
      Config[TMVCConfigKey.DefaultContentType] := TMVCMediaType.TEXT_HTML;
    end);
  MVC.AddController(TFileUploadController);
  MVC.AddMiddleware(TMVCStaticFilesMiddleware.Create(
    '/static', { StaticFilesPath }
    ExtractFilePath(GetModuleName(HInstance)) + '..\www', { DocumentRoot }
    'index.html' { IndexDocument - Before it was named fallbackresource }
    ));
  MVC.SetViewEngine(TMVCMustacheViewEngine);

end;

end.
