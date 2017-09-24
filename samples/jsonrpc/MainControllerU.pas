unit MainControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.JSONRPC, JsonDataObjects;

type
  TMyDerivedController = class(TMVCJSONRPCController)
  public
    function Subtract(aParams: TMVCJSONArray): Integer;
    procedure DoSomething;

  end;

implementation

uses
  System.SysUtils, MVCFramework.Logger, System.StrUtils;

{ TMyDerivedController }

procedure TMyDerivedController.DoSomething;
begin

end;

function TMyDerivedController.Subtract(aParams: TMVCJSONArray): Integer;
begin
  if aParams.Count <> 2 then
    raise EMVCJSONRPCException.Create('Method "subtract" expects exactly 2 params');
  Result := aParams[0].IntValue - aParams[1].IntValue;
end;

end.
