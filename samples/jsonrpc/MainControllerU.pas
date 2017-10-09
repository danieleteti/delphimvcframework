unit MainControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.JSONRPC, JsonDataObjects,
  Data.DB, BusinessObjectsU;

type
  TMyJSONRPCController = class(TMVCJSONRPCController)
  public
    function Subtract(aValue1, aValue2: Integer): Integer;
    function ReverseString(aString: string): string;
    function GetCustomers(aString: string): TDataSet;
    function GetUser(aUserName: string): TPerson;
    procedure DoSomething;

  end;

implementation

uses
  System.SysUtils, MVCFramework.Logger, System.StrUtils, FireDAC.Comp.Client;

{ TMyDerivedController }

procedure TMyJSONRPCController.DoSomething;
begin

end;

function TMyJSONRPCController.GetCustomers(aString: string): TDataSet;
var
  lMT: TFDMemTable;
begin
  lMT := TFDMemTable.Create(nil);
  try
    lMT.FieldDefs.Clear;

    lMT.FieldDefs.Add('Code', ftInteger);
    lMT.FieldDefs.Add('Name', ftString, 20);
    lMT.Active := True;
    lMT.AppendRecord([1, 'Ford']);
    lMT.AppendRecord([2, 'Ferrari']);
    lMT.AppendRecord([3, 'Lotus']);
    lMT.AppendRecord([4, 'FCA']);
    lMT.AppendRecord([5, 'Hyundai']);
    lMT.AppendRecord([6, 'De Tomaso']);
    lMT.AppendRecord([7, 'Dodge']);
    lMT.AppendRecord([8, 'Tesla']);
    lMT.AppendRecord([9, 'Kia']);
    lMT.AppendRecord([10, 'Tata']);
    lMT.AppendRecord([11, 'Volkswagen']);
    lMT.AppendRecord([12, 'Audi']);
    lMT.AppendRecord([13, 'Skoda']);
    if not aString.IsEmpty then
    begin
      lMT.Filter := aString;
      lMT.Filtered := True;
    end;
    lMT.First;
    Result := lMT;
  except
    lMt.Free;
    raise;
  end;
end;

function TMyJSONRPCController.GetUser(aUserName: string): TPerson;
begin
  Result := TPerson.Create;
  Result.FirstName := 'Daniele (a.k.a. ' + aUserName + ')';
  Result.LastName := 'Teti';
  Result.DOB := EncodeDate(1932, 11, 4); // hey, it is a joke :-)
  Result.Married := True;
end;

function TMyJSONRPCController.ReverseString(aString: string): string;
begin
  Result := System.StrUtils.ReverseString(aString);
end;

function TMyJSONRPCController.Subtract(aValue1, aValue2: Integer): Integer;
begin
  Result := aValue1 - aValue2;
end;

end.
