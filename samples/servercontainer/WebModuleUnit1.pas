unit WebModuleUnit1;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework;

type

  TWebModule1 = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    FMVCEngine: TMVCEngine;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

{$R *.dfm}

uses
  App1MainControllerU,
  MVCFramework.Middleware.StaticFiles;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  FMVCEngine := TMVCEngine.Create(Self);
  FMVCEngine.Config['view_path'] := '..\Debug\HTML5Application';
  FMVCEngine.AddController(TApp1MainController);
  FMVCEngine.AddMiddleware(TMVCStaticFilesMiddleware.Create(
    '/', { StaticFilesPath }
    'HTML5Application\public_html', { DocumentRoot }
    'index.html' {IndexDocument - Before it was named fallbackresource}
    ));
end;

procedure TWebModule1.WebModuleDestroy(Sender: TObject);
begin
  FMVCEngine.free;
end;

end.
