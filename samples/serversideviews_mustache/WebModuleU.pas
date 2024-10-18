unit WebModuleU;

interface

uses System.SysUtils, System.Classes, Web.HTTPApp, MVCFramework, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type
  TWebModule1 = class(TWebModule)
    FDMemTable1: TFDMemTable;
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
  MVCFramework.Middleware.StaticFiles,
  CustomMustacheHelpersU,
  MVCFramework.Serializer.URLEncoded;

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
    .SetViewEngine(TMVCMustacheViewEngine)
    .AddSerializer(TMVCMediaType.APPLICATION_FORM_URLENCODED, TMVCURLEncodedSerializer.Create);
end;

procedure TWebModule1.WebModuleDestroy(Sender: TObject);
begin
  FMVCEngine.Free;
end;

end.
