unit MiddlewareSample1;

interface

uses
  MVCFramework, MVCFramework.Logger;

type
  TMVCSalutationMiddleware = class(TInterfacedObject, IMVCMiddleware)
  protected
    procedure OnAfterControllerAction(Context: TWebContext; const AActionNAme: string;
      const Handled: Boolean);
    procedure OnBeforeRouting(Context: TWebContext; var Handled: Boolean);
    procedure OnBeforeControllerAction(Context: TWebContext;
      const AControllerQualifiedClassName: string; const AActionNAme: string; var Handled: Boolean);
  end;

  TMVCRedirectAndroidDeviceOnPlayStore = class(TInterfacedObject, IMVCMiddleware)
  protected
    procedure OnBeforeRouting(Context: TWebContext; var Handled: Boolean);
    procedure OnAfterControllerAction(Context: TWebContext; const AActionNAme: string;
      const Handled: Boolean);
    procedure OnBeforeControllerAction(Context: TWebContext;
      const AControllerQualifiedClassName: string; const AActionNAme: string; var Handled: Boolean);
  end;

implementation

uses
  System.SysUtils;

{ TMVCSalutationMiddleware }

procedure TMVCSalutationMiddleware.OnAfterControllerAction(Context: TWebContext;
  const AActionNAme: string; const Handled: Boolean);
begin
  Context.Response.CustomHeaders.Values['X-PROUD-HEADER'] :=
    'Proudly served by DelphiMVCFramework (https://code.google.com/p/delphimvcframework/)';
end;

procedure TMVCRedirectAndroidDeviceOnPlayStore.OnAfterControllerAction(Context: TWebContext;
  const AActionNAme: string; const Handled: Boolean);
begin

end;

procedure TMVCRedirectAndroidDeviceOnPlayStore.OnBeforeControllerAction(
  Context: TWebContext; const AControllerQualifiedClassName,
  AActionNAme: string; var Handled: Boolean);
begin

end;

procedure TMVCRedirectAndroidDeviceOnPlayStore.OnBeforeRouting(Context: TWebContext;
  var Handled: Boolean);
begin
  Log(Context.Request.Headers['User-Agent']);
  if Context.Request.Headers['User-Agent'].Contains('Android') then
  begin
    Context.Response.Location := 'http://play.google.com';
    Context.Response.StatusCode := 307; // temporary redirect
    Handled := True;
  end;
end;

procedure TMVCSalutationMiddleware.OnBeforeControllerAction(Context: TWebContext;
  const AControllerQualifiedClassName, AActionNAme: string; var Handled: Boolean);
begin

end;

procedure TMVCSalutationMiddleware.OnBeforeRouting(Context: TWebContext; var Handled: Boolean);
begin

end;

end.
