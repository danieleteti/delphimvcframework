unit TestServerControllerJSONRPCU;

interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.JSONRPC, JsonDataObjects;

type
  TTestJSONRPCController = class(TMVCJSONRPCController)
  public
    function Subtract(aValue1, aValue2: Int64): Integer;
    procedure MyNotify;
    function Add(aValue1, aValue2, aValue3: Int64): TJsonObject;
    function GetListFromTo(aFrom, aTo: Int64): TJsonArray;
    function MultiplyString(aString: string; Multiplier: Int64): string;
  end;

implementation

uses
  System.SysUtils, MVCFramework.Logger, System.StrUtils;

{ TTestJSONRPCController }

function TTestJSONRPCController.Add(aValue1, aValue2, aValue3: Int64): TJsonObject;
begin
  Result := TJsonObject.Create;
  Result.I['res'] := aValue1 + aValue2 + aValue3;
end;

function TTestJSONRPCController.GetListFromTo(aFrom, aTo: Int64): TJsonArray;
var
  I: Int64;
begin
  Result := TJsonArray.Create;
  for I := aFrom to aTo do
    Result.Add(I);
end;

function TTestJSONRPCController.MultiplyString(aString: string;
  Multiplier: Int64): string;
var
  I: Integer;
begin
  Result := aString;
  for I := 2 to Multiplier do
  begin
    Result := Result + aString;
  end;
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
