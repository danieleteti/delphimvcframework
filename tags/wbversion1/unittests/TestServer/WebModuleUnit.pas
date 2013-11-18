unit WebModuleUnit;

interface

uses System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework;

type
  Twm = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
  private
    MVCEngine: TMVCEngine;
  end;

var
  WebModuleClass: TComponentClass = Twm;

implementation

{$R *.dfm}


uses
  TestServerControllerU;

procedure Twm.WebModuleCreate(Sender: TObject);
begin
  MVCEngine := TMVCEngine.Create(self);
  MVCEngine.AddController(TTestServerController);
  MVCEngine.Config['stompserver'] := 'localhost';
  MVCEngine.Config['stompserverport'] := '61613';
  MVCEngine.Config['stompusername'] := 'guest';
  MVCEngine.Config['stomppassword'] := 'guest';
end;

end.
