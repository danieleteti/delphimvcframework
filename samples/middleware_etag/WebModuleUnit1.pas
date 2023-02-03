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
    FEngine: TMVCEngine;

  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

uses
  App1MainControllerU,
  MVCFramework.Commons,
  MVCFramework.Middleware.StaticFiles,
  MVCFramework.Middleware.ETag;

{$R *.dfm}


procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  FEngine := TMVCEngine.Create(Self);
  FEngine.AddMiddleware(TMVCETagMiddleware.Create);
  FEngine.AddController(TApp1MainController);
end;

procedure TWebModule1.WebModuleDestroy(Sender: TObject);
begin
  FEngine.free;
end;

end.
