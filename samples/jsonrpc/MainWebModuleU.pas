unit MainWebModuleU;

interface

uses System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client;

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
  System.IOUtils,
  MVCFramework.Commons,
  MainControllerU,
  MVCFramework.Middleware.StaticFiles;

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
      // Enable Server Signature in response
      Config[TMVCConfigKey.ExposeServerSignature] := 'true';
    end);
  FMVC.AddController(TMyJSONRPCController, '/jsonrpc');
  FMVC.AddMiddleware(TMVCStaticFilesMiddleware.Create(
    '/', { StaticFilesPath }
    TPath.Combine(ExtractFilePath(GetModuleName(HInstance)), 'www'), { DocumentRoot }
    'index.html' {IndexDocument - Before it was named fallbackresource}
    ));
end;

procedure TMyWebModule.WebModuleDestroy(Sender: TObject);
begin
  FMVC.Free;
end;

end.
