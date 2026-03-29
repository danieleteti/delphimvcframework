unit WebModuleU;

interface

uses
  System.SysUtils, System.Classes,
  Web.HTTPApp, MVCFramework;

type
  TWebModule1 = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    FMVC: TMVCEngine;
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

{$R *.dfm}

uses
  StreamingControllerU,
  MVCFramework.Commons,
  MVCFramework.Middleware.StaticFiles;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  FMVC := TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.DefaultContentType] := TMVCMediaType.APPLICATION_JSON;
    end);
  FMVC.AddController(TStreamingController);
  FMVC.AddMiddleware(TMVCStaticFilesMiddleware.Create('/static', 'www'));
end;

procedure TWebModule1.WebModuleDestroy(Sender: TObject);
begin
  FMVC.Free;
end;

end.
