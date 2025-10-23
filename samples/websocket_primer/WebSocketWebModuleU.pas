unit WebSocketWebModuleU;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework;

type
  TWebSocketWebModule = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    FMVC: TMVCEngine;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebSocketWebModule;

implementation

{$R *.dfm}

uses
  WebSocketControllerU,
  MVCFramework.Commons,
  MVCFramework.Middleware.StaticFiles,
  MVCFramework.Middleware.Compression;

procedure TWebSocketWebModule.WebModuleCreate(Sender: TObject);
begin
  FMVC := TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.DefaultContentType] := 'text/html';
      Config[TMVCConfigKey.DefaultContentCharset] := 'UTF-8';
      Config[TMVCConfigKey.DefaultViewFileExtension] := '.html';
    end);

  // Add WebSocket controllers
  FMVC.AddController(TWebSocketEchoController);
  FMVC.AddController(TWebSocketChatController);

  // Add middleware for serving static files (HTML client)
  FMVC.AddMiddleware(TMVCStaticFilesMiddleware.Create('/static', 'www'));
  FMVC.AddMiddleware(TMVCCompressionMiddleware.Create);
end;

procedure TWebSocketWebModule.WebModuleDestroy(Sender: TObject);
begin
  FMVC.Free;
end;

end.
