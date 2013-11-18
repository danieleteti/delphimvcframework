unit CallbackControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Logger;

type

  [MVCPath('/')]
  TCallbackController = class(TMVCController)
  public
    [MVCPath('/login/($username)')]
    // this is only for test!!!!
    procedure Login(CTX: TWebContext);

    [MVCPath('/logout')]
    // this is only for test!!!!
    procedure Logout(CTX: TWebContext);

    [MVCPath('/')]
    procedure Index(CTX: TWebContext);

  end;

implementation

uses
  system.ioutils,
  system.Classes,
  system.SysUtils,
  system.Types;

{ TCallbackController }

procedure TCallbackController.Index(CTX: TWebContext);
begin
  Redirect('/callbackdemo.html');
end;

procedure TCallbackController.Login(CTX: TWebContext);
begin
  Session['username'] := CTX.Request.Params['username'];
end;

procedure TCallbackController.Logout(CTX: TWebContext);
begin
  SessionStop(false);
end;

end.
