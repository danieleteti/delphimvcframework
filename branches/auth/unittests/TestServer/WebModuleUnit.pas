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
  TestServerControllerU, TestServerControllerExceptionU, SpeedMiddlewareU;

procedure Twm.WebModuleCreate(Sender: TObject);
begin
  MVCEngine := TMVCEngine.Create(self);
  MVCEngine
    .AddController(TTestServerController)
    .AddController(TTestServerControllerExceptionAfterCreate)
    .AddController(TTestServerControllerExceptionBeforeDestroy)
    .AddMiddleware(TMVCSpeedMiddleware.Create);
  MVCEngine.Config[TMVCConfigKey.StompServer] := 'localhost';
  MVCEngine.Config[TMVCConfigKey.StompServerPort] := '61613';
  MVCEngine.Config[TMVCConfigKey.StompUserName] := 'guest';
  MVCEngine.Config[TMVCConfigKey.StompPassword] := 'guest';
  MVCEngine.Config[TMVCConfigKey.Messaging] := 'false';
end;

end.
