unit TestServerControllerJSONRPCU;

interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.JSONRPC, JsonDataObjects;

type
  TTestJSONRPCController = class(TMVCJSONRPCController)
  public
    function Subtract(aValue1, aValue2: Int64): Integer;
    procedure MyNotify;
    function Add(aParams: TMVCJSONObject): TJsonObject;
  end;

implementation

uses
  System.SysUtils, MVCFramework.Logger, System.StrUtils;

{ TTestJSONRPCController }

function TTestJSONRPCController.Add(aParams: TMVCJSONObject): TJsonObject;
begin
  Result := TJsonObject.Create;
  Result.I['risultato'] := aParams.I['op1'] + aParams.I['op2'];
end;

procedure TTestJSONRPCController.MyNotify;
begin
  // this is a notify with no parameters and no result code
  Self.ClassName;
end;

function TTestJSONRPCController.Subtract(aValue1, aValue2: Int64): Integer;
begin
  Result := aValue1 - aValue2;
end;

end.
