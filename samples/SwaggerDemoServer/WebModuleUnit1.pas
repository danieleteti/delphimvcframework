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
    App1MainControllerU
    , MVCFramework.Commons
    , MVCFramework.Controllers.Swagger
    ;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  MVC := TMVCEngine.Create(Self);
  MVC.Config[TMVCConfigKey.ViewPath] := '.\www\public_html';
  MVC.Config[TMVCConfigKey.DocumentRoot] := '.\www\public_html';
  MVC.Config[TMVCConfigKey.AllowUnhandledAction] := 'true';
  MVC.AddController(TApp1MainController);
  MVC.AddController(TMVCSwaggerController);
end;

procedure TWebModule1.WebModuleDestroy(Sender: TObject);
begin
  MVC.free;
end;

end.
