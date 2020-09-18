unit WebModuleU;

interface

uses System.SysUtils, System.Classes, Web.HTTPApp, MVCFramework;

type
  TWebModule1 = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    FMVCEngine: TMVCEngine;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

uses
  MVCFramework.View.Renderers.TemplatePro,
  WebSiteControllerU,
  MVCFramework.Commons,
  MVCFramework.Middleware.StaticFiles;

{ %CLASSGROUP 'Vcl.Controls.TControl' }

{$R *.dfm}

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  FMVCEngine := TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      // session timeout (0 means session cookie)
      Config[TMVCConfigKey.SessionTimeout] := '0';
      // default content-type
      Config[TMVCConfigKey.DefaultContentType] :=
        TMVCConstants.DEFAULT_CONTENT_TYPE;
      // default content charset
      Config[TMVCConfigKey.DefaultContentCharset] :=
        TMVCConstants.DEFAULT_CONTENT_CHARSET;
      // unhandled actions are permitted?
      Config[TMVCConfigKey.AllowUnhandledAction] := 'false';
      // default view file extension
      Config[TMVCConfigKey.DefaultViewFileExtension] := 'html';
      // view path
      Config[TMVCConfigKey.ViewPath] := 'templates';
      // Enable Server Signature in response
      Config[TMVCConfigKey.ExposeServerSignature] := 'true';
    end)
    .AddController(TWebSiteController)
    .SetViewEngine(TMVCTemplateProViewEngine);
end;

procedure TWebModule1.WebModuleDestroy(Sender: TObject);
begin
  FMVCEngine.Free;
end;

end.
