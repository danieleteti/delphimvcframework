unit MiddlewareSample1;

interface

uses
  MVCFramework, MVCFramework.Logger;

type
  TMVCSalutationControllerFilter = class(TCustomControllerFilter)
  protected
    procedure DoFilter(const Context: TWebContext; const Router: IMVCRouter); override;
  end;

  TMVCRedirectAndroidToPlayStoreProtocolFilter = class(TCustomProtocolFilter)
  protected
    procedure DoFilter(Context: TWebContext); override;
  end;

implementation

uses
  System.SysUtils, MVCFramework.Commons;

{ TMVCSalutationControllerFilter }

procedure TMVCSalutationControllerFilter.DoFilter(const Context: TWebContext;
  const Router: IMVCRouter);
begin
  DoNext(Context, Router);
  Context.Response.CustomHeaders.Values['X-PROUD-HEADER'] :=
    'Proudly served by DelphiMVCFramework (https://github.com/danieleteti/delphimvcframework)';
end;

procedure TMVCRedirectAndroidToPlayStoreProtocolFilter.DoFilter(Context: TWebContext);
begin
  Log(Context.Request.Headers['User-Agent']);
  if Context.Request.Headers['User-Agent'].Contains('Android') then
  begin
    Context.Response.Location := 'http://play.google.com';
    Context.Response.StatusCode := HTTP_STATUS.TemporaryRedirect; // 307 - temporary redirect
  end
  else
  begin
    DoNext(Context);
  end;
end;

end.
