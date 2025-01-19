unit WebModuleUnit1;

interface

uses System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework;

type
  TWebModule1 = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
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
  AppControllerU,
  MVCFramework.Commons,
  MVCFramework.Middleware.Session,
  MVCFramework.Middleware.ActiveRecord;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  MVC := TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.DefaultContentType] := TMVCMediaType.TEXT_HTML;
    end);
  MVC.AddController(TApp1MainController);

  {To use memory session}
  MVC.AddMiddleware(UseMemorySessionMiddleware);

  {To use file based session}
  //MVC.AddMiddleware(UseFileSessionMiddleware);

  {To use database based session (firebird)}
  MVC.AddMiddleware(TMVCActiveRecordMiddleware.Create('firebirddb'));
  MVC.AddMiddleware(UseDatabaseSessionMiddleware(0));
end;

end.
