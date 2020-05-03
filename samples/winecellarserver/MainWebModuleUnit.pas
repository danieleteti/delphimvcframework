unit MainWebModuleUnit;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework;

type
  Twm = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);

  private
    MVCEngine: TMVCEngine;

  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = Twm;

implementation

uses
  WineCellarAppControllerU,
  MVCFramework.Commons,
  MVCFramework.Middleware.StaticFiles,
  System.IOUtils;

{$R *.dfm}

procedure Twm.WebModuleCreate(Sender: TObject);
begin
  MVCEngine := TMVCEngine.Create(self);
  MVCEngine.AddController(TWineCellarApp);
  MVCEngine.AddMiddleware(TMVCStaticFilesMiddleware.Create(
    '/', { StaticFilesPath }
    TPath.Combine(AppPath, '..\..\www'), { DocumentRoot }
    'index.html' {IndexDocument - Before it was named fallbackresource}
    ));
end;

end.
