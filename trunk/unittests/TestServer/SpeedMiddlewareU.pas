unit SpeedMiddlewareU;

interface

uses
  MVCFramework;

type
  TMVCSpeedMiddleware = class(TInterfacedObject, IMVCMiddleware)
  public
    procedure OnBeforeRouting(Context: TWebContext; var Handled: Boolean);
    procedure OnAfterControllerAction(Context: TWebContext; const AActionNAme: string; const Handled: Boolean);
  end;

implementation

uses
  ObjectsMappers, System.SysUtils, DateUtils;

{ TMVCSpeedMiddleware }

procedure TMVCSpeedMiddleware.OnAfterControllerAction(Context: TWebContext; const AActionNAme: string;
  const Handled: Boolean);
begin
  Context.Response.CustomHeaders.Values['request_gen_time'] :=
    MilliSecondsBetween(Now, ISOStrToDateTime(Context.Data[classname + 'startup'])).ToString
end;

procedure TMVCSpeedMiddleware.OnBeforeRouting(Context: TWebContext; var Handled: Boolean);
begin
  if Context.Request.PathInfo = '/handledbymiddleware' then
  begin
    Handled := True;
    Context.Response.RawWebResponse.Content := 'This is a middleware response';
    Context.Response.StatusCode := 200;
  end
  else

    Context.Data.Add(classname + 'startup', ISODateTimeToString(Now));
end;

end.
