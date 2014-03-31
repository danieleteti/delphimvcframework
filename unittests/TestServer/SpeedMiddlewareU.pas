unit SpeedMiddlewareU;

interface

uses
  MVCFramework;

type
  TMVCSpeedMiddleware = class(TInterfacedObject, IMVCMiddleware)
  public
    procedure OnBeforeControllerAction(Context: TWebContext; const AActionNAme: string; var Handled: Boolean);
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

procedure TMVCSpeedMiddleware.OnBeforeControllerAction(Context: TWebContext; const AActionNAme: string;
  var Handled: Boolean);
begin
  Context.Data.Add(classname + 'startup', ISODateTimeToString(Now));
end;

end.
