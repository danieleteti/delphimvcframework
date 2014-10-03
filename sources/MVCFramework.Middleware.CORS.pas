unit MVCFramework.Middleware.CORS;

interface

uses
  MVCFramework;

type
  TCORSMiddleware = class(TInterfacedObject, IMVCMiddleware)
  public
    procedure OnBeforeRouting(Context: TWebContext; var Handled: Boolean);
    procedure OnAfterControllerAction(Context: TWebContext;
      const AActionNAme: string; const Handled: Boolean);

  end;

implementation

{ TCORSMiddleware }

procedure TCORSMiddleware.OnAfterControllerAction(Context: TWebContext;
  const AActionNAme: string; const Handled: Boolean);
begin
end;

procedure TCORSMiddleware.OnBeforeRouting(Context: TWebContext;
  var Handled: Boolean);
begin
  Context.Response.RawWebResponse.CustomHeaders.Values['Access-Control-Allow-Origin'] := '*';
  Context.Response.RawWebResponse.CustomHeaders.Values['Access-Control-Allow-Methods'] :=
    'POST, GET, OPTIONS, PUT, DELETE';
  Context.Response.RawWebResponse.CustomHeaders.Values['Access-Control-Allow-Headers'] := 'content-type';

  if Context.Request.HTTPMethod = httpOPTIONS then
  begin
    Context.Response.StatusCode := 200;
    Handled := True;
  end;
end;

end.
