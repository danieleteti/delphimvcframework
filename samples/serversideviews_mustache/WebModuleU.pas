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
  MVCFramework.View.Renderers.Mustache,
  WebSiteControllerU,
  System.IOUtils,
  MVCFramework.Commons,
  MVCFramework.Middleware.StaticFiles, SynMustache, CustomMustacheHelpersU;

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
      Config[TMVCConfigKey.DefaultViewFileExtension] := 'mustache';
      // view path
      Config[TMVCConfigKey.ViewPath] := 'templates';
      // Enable Server Signature in response
      Config[TMVCConfigKey.ExposeServerSignature] := 'true';
      Config[TMVCConfigKey.ViewCache] := 'false';
    end)
    .AddController(TWebSiteController)
    .SetViewEngine(TMVCMustacheViewEngine);

  // these helpers will be available to the mustache views as if they were the standard ones
  TMVCMustacheHelpers.OnLoadCustomHelpers := procedure(var MustacheHelpers: TSynMustacheHelpers)
  begin
    TSynMustache.HelperAdd(MustacheHelpers, 'MyHelper1', TMyMustacheHelpers.MyHelper1);
    TSynMustache.HelperAdd(MustacheHelpers, 'MyHelper2', TMyMustacheHelpers.MyHelper2);
  end
end;

procedure TWebModule1.WebModuleDestroy(Sender: TObject);
begin
  FMVCEngine.Free;
end;

end.
