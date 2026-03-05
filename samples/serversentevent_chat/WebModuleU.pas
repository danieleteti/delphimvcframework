unit WebModuleU;

interface

uses
  System.SysUtils, System.Classes, Web.HTTPApp, MVCFramework;

type
  TMyWebModule = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    FMVC: TMVCEngine;
  end;

var
  WebModuleClass: TComponentClass = TMyWebModule;

implementation

{$R *.dfm}

uses
  ChatSSEControllerU,
  ChatApiControllerU,
  MVCFramework.Commons,
  MVCFramework.Middleware.CORS;

procedure TMyWebModule.WebModuleCreate(Sender: TObject);
begin
  FMVC := TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.DefaultContentType] := TMVCConstants.DEFAULT_CONTENT_TYPE;
      Config[TMVCConfigKey.DefaultContentCharset] := TMVCConstants.DEFAULT_CONTENT_CHARSET;
      Config[TMVCConfigKey.AllowUnhandledAction] := 'false';
      Config[TMVCConfigKey.ExposeServerSignature] := 'true';
    end);
  FMVC.AddController(TChatSSEController);
  FMVC.AddController(TChatApiController);
  FMVC.AddMiddleware(TMVCCORSMiddleware.Create);
end;

procedure TMyWebModule.WebModuleDestroy(Sender: TObject);
begin
  FMVC.Free;
end;

end.
