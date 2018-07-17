unit WebModuleUnit1;

interface

uses System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework,
  MVCFramework.Commons;

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


uses AppControllerU;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  MVC := TMVCEngine.Create(Self);
  MVC.Config[TMVCConfigKey.SessionTimeout] := '10'; // 10minutes
  MVC.Config[TMVCConfigKey.DefaultContentType] := 'text/plain';

  // comment the line to use default session type (memory)
  MVC.Config[TMVCConfigKey.SessionType] := 'memoryController';

  MVC.AddController(TApp1MainController);
end;

end.
