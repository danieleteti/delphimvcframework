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
  TestServerControllerU, TestServerControllerExceptionU, SpeedMiddlewareU,
  MVCFramework.Middleware.Authentication, System.Generics.Collections,
  MVCFramework.Commons, TestServerControllerPrivateU, AuthHandlersU;

procedure Twm.WebModuleCreate(Sender: TObject);
begin
  MVCEngine := TMVCEngine.Create(self,
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.Messaging] := 'true';
    end, nil);
  MVCEngine.AddController(TTestServerController)
    .AddController(TTestPrivateServerController)
    .AddController(TTestServerControllerExceptionAfterCreate)
    .AddController(TTestServerControllerExceptionBeforeDestroy)
    .AddController(TTestPrivateServerControllerCustomAuth)
    .AddMiddleware(TMVCSpeedMiddleware.Create)
    .AddMiddleware(TMVCBasicAuthenticationMiddleware.Create(TBasicAuthHandler.Create))
    .AddMiddleware(TMVCCustomAuthenticationMiddleware.Create(TCustomAuthHandler.Create, '/system/users/logged','/login.html'));

  // MVCEngine.Config[TMVCConfigKey.Messaging] := 'false';
end;

{ TSampleAuth }
{ TBasicAuthHandler }
{ TCustomAuthHandler }

end.
