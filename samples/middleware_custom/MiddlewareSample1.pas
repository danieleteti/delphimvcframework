unit MiddlewareSample1;

interface

uses
  MVCFramework, MVCFramework.Logger;

type
  TMVCSalutationMiddleware = class(TInterfacedObject, IMVCMiddleware)
  protected
    procedure OnBeforeRouting(Context: TWebContext; var Handled: Boolean);
    procedure OnBeforeControllerAction(Context: TWebContext;
      const AControllerQualifiedClassName: string; const AActionNAme: string; var Handled: Boolean);
    procedure OnAfterRouting(Context: TWebContext; const AHandled: Boolean);
    procedure OnAfterControllerAction(Context: TWebContext; const AControllerQualifiedClassName: string; const AActionName: string; const AHandled: Boolean);
  end;

  TMVCRedirectAndroidDeviceOnPlayStore = class(TInterfacedObject, IMVCMiddleware)
  protected
    procedure OnBeforeRouting(Context: TWebContext; var Handled: Boolean);
    procedure OnBeforeControllerAction(Context: TWebContext;
      const AControllerQualifiedClassName: string; const AActionNAme: string; var Handled: Boolean);
    procedure OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
    procedure OnAfterControllerAction(AContext: TWebContext; const AControllerQualifiedClassName: string; const AActionName: string; const AHandled: Boolean);
  end;

implementation

uses
  System.SysUtils, MVCFramework.Commons;

{ TMVCSalutationMiddleware }

procedure TMVCSalutationMiddleware.OnAfterControllerAction(
  Context: TWebContext; const AControllerQualifiedClassName: string;
  const AActionName: string; const AHandled: Boolean);
begin
  Context.Response.CustomHeaders.Values['X-PROUD-HEADER'] :=
    'Proudly served by DelphiMVCFramework (https://github.com/danieleteti/delphimvcframework)';
end;

procedure TMVCRedirectAndroidDeviceOnPlayStore.OnAfterControllerAction(
  AContext: TWebContext; const AControllerQualifiedClassName: string;
  const AActionName: string; const AHandled: Boolean);
begin
  // do nothing
end;

procedure TMVCRedirectAndroidDeviceOnPlayStore.OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
begin
  // do nothing
end;

procedure TMVCRedirectAndroidDeviceOnPlayStore.OnBeforeControllerAction(
  Context: TWebContext; const AControllerQualifiedClassName,
  AActionNAme: string; var Handled: Boolean);
begin
  // do nothing
end;

procedure TMVCRedirectAndroidDeviceOnPlayStore.OnBeforeRouting(Context: TWebContext;
  var Handled: Boolean);
begin
  Log(Context.Request.Headers['User-Agent']);
  if Context.Request.Headers['User-Agent'].Contains('Android') then
  begin
    Context.Response.Location := 'http://play.google.com';
    Context.Response.StatusCode := HTTP_STATUS.TemporaryRedirect; // 307 - temporary redirect
    Handled := True;
  end;
end;

procedure TMVCSalutationMiddleware.OnAfterRouting(Context: TWebContext; const AHandled: Boolean);
begin
  // do nothing
end;

procedure TMVCSalutationMiddleware.OnBeforeControllerAction(Context: TWebContext;
  const AControllerQualifiedClassName, AActionNAme: string; var Handled: Boolean);
begin
  // do nothing
end;

procedure TMVCSalutationMiddleware.OnBeforeRouting(Context: TWebContext; var Handled: Boolean);
begin
  // do nothing
end;

end.
