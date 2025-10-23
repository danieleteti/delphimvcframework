unit WebModuleU;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework;

type
  TWebModule1 = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
  private
    FMVC: TMVCEngine;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

{$R *.dfm}

uses
  ChatControllerU,
  MVCFramework.Commons,
  MVCFramework.Middleware.CORS;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  FMVC := TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.DefaultContentType] := TMVCConstants.DEFAULT_CONTENT_TYPE;
      Config[TMVCConfigKey.DefaultContentCharset] := TMVCConstants.DEFAULT_CONTENT_CHARSET;
      Config[TMVCConfigKey.DefaultViewFileExtension] := 'html';
    end);

  // Add CORS middleware for browser compatibility
  FMVC.AddMiddleware(TMVCCORSMiddleware.Create);

  // Add WebSocket chat controller
  FMVC.AddController(TChatController);
end;

end.
