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
  MVCFramework.View.Renderers.TemplatePro,
  MVCFramework.Middleware.Trace,
  MVCFramework.Middleware.StaticFiles;

{$R *.dfm}


procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  MVC := TMVCEngine.Create(self);
  MVC.AddController(TFileUploadController);
  MVC.AddMiddleware(TMVCTraceMiddleware.Create);
  MVC.AddMiddleware(TMVCStaticFilesMiddleware.Create(
    '/', { StaticFilesPath }
    ExtractFilePath(GetModuleName(HInstance)) + '..\..\document_root', { DocumentRoot }
    'index.html' {IndexDocument - Before it was named fallbackresource}
    ));
  MVC.SetViewEngine(TMVCTemplateProViewEngine);
  MVC.Config[TMVCConfigKey.ViewPath] := ExtractFilePath(GetModuleName(HInstance)
    ) + '..\..\templates';
  MVC.Config[TMVCConfigKey.DefaultContentType] := TMVCMediaType.TEXT_HTML;

end;

end.
