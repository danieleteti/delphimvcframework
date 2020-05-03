unit TestServerControllerJSONRPCU;

interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.JSONRPC, JsonDataObjects;

type
  TTestJSONRPCController = class(TMVCJSONRPCController)
  public
    function Subtract(Value1, Value2: Int64): Integer;
    procedure MyNotify;
    function Add(Value1, Value2, Value3: Int64): TJsonObject;
    function GetListFromTo(aFrom, aTo: Int64): TJsonArray;
    function MultiplyString(aString: string; Multiplier: Int64): string;
  end;

  TTestJSONRPCClass = class(TObject)
  public
    function Subtract(Value1, Value2: Int64): Integer;
    procedure MyNotify;
    function Add(Value1, Value2, Value3: Int64): TJsonObject;
    function GetListFromTo(aFrom, aTo: Int64): TJsonArray;
    function MultiplyString(aString: string; Multiplier: Int64): string;
    function AddTimeToDateTime(aDateTime: TDateTime; aTime: TTime): TDateTime;
  end;

implementation

uses
  System.SysUtils, MVCFramework.Logger, System.StrUtils;

{ TTestJSONRPCController }

function TTestJSONRPCController.Add(Value1, Value2, Value3: Int64): TJsonObject;
begin
  Result := TJsonObject.Create;
  Result.I['res'] := Value1 + Value2 + Value3;
end;

function TTestJSONRPCController.GetListFromTo(aFrom, aTo: Int64): TJsonArray;
var
  I: Cardinal;
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

function TTestJSONRPCController.Subtract(Value1, Value2: Int64): Integer;
begin
  Result := Value1 - Value2;
end;

{ TTestJSONRPCClass }

function TTestJSONRPCClass.Add(Value1, Value2, Value3: Int64): TJsonObject;
begin
  Result := TJsonObject.Create;
  Result.I['res'] := Value1 + Value2 + Value3;
end;

function TTestJSONRPCClass.AddTimeToDateTime(aDateTime: TDateTime;
  aTime: TTime): TDateTime;
begin
  Result := aDateTime + aTime;
end;

function TTestJSONRPCClass.GetListFromTo(aFrom, aTo: Int64): TJsonArray;
var
  I: Cardinal;
begin
  Result := TJsonArray.Create;
  for I := aFrom to aTo do
    Result.Add(I);
end;

function TTestJSONRPCClass.MultiplyString(aString: string;
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

procedure TTestJSONRPCClass.MyNotify;
begin
  // this is a notify with no parameters and no result code
  Self.ClassName;
end;

function TTestJSONRPCClass.Subtract(Value1, Value2: Int64): Integer;
begin
  Result := Value1 - Value2;
end;

end.
