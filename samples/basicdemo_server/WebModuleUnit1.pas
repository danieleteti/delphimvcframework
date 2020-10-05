unit WebModuleUnit1;

interface

uses System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework;

type
  TWebModule1 = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);

  private
    MVC: TMVCEngine;

  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

{$R *.dfm}


uses
  App1MainControllerU,
  MVCFramework.Commons,
  MVCFramework.Middleware.StaticFiles;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  MVC := TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.ViewPath] := '.\www\public_html';
    end);

  // Web files
  MVC.AddMiddleware(TMVCStaticFilesMiddleware.Create('/app', '.\www\public_html'));

  // Image files
  MVC.AddMiddleware(TMVCStaticFilesMiddleware.Create('/images', '.\www\public_images', 'database.png'));

  MVC.AddController(TApp1MainController);
end;

procedure TWebModule1.WebModuleDestroy(Sender: TObject);
begin
  MVC.free;
end;

end.
