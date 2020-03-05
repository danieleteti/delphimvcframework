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
  MVCFramework.Middleware.Trace;

{$R *.dfm}


procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  MVC := TMVCEngine.Create(self);
  MVC.AddController(TFileUploadController);
  MVC.AddMiddleware(TMVCTraceMiddleware.Create);
  MVC.SetViewEngine(TMVCTemplateProViewEngine);
  MVC.Config[TMVCConfigKey.ViewPath] := ExtractFilePath(GetModuleName(HInstance)
    ) + '..\..\templates';
  MVC.Config[TMVCConfigKey.DocumentRoot] := ExtractFilePath(GetModuleName(HInstance)
    ) + '..\..\document_root';
  MVC.Config[TMVCConfigKey.DefaultContentType] := TMVCMediaType.TEXT_HTML;

end;

end.
