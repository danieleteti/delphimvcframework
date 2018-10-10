unit MainControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.JSONRPC,
  JsonDataObjects,
  Data.DB,
  BusinessObjectsU;

type
  TMyJSONRPCController = class(TMVCJSONRPCController)
  protected
    procedure OnBeforeAction(AContext: TWebContext; const AActionName: string; var AHandled: Boolean); override;
  public
    function Subtract(aValue1, aValue2: Integer): Integer;
    function ReverseString(aString: string): string;
    function GetNextMonday(const aDate: TDate): TDate;
    function GetCustomers(aString: string): TDataSet;
    function GetUser(aUserName: string): TPerson;
    function SavePerson(const aPerson: TJsonObject): Integer;
    procedure DoSomething;
    // invalid parameters modifiers
    procedure InvalidMethod1(var MyVarParam: Integer);
    procedure InvalidMethod2(out MyOutParam: Integer);

  end;

implementation

uses
  System.SysUtils,
  MVCFramework.Logger,
  System.StrUtils,
  FireDAC.Comp.Client,
  System.DateUtils;

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
    lMT.Free;
    raise;
  end;
end;

function TMyJSONRPCController.GetNextMonday(const aDate: TDate): TDate;
var
  lDate: TDate;
begin
  lDate := aDate + 1;
  while DayOfTheWeek(lDate) <> 1 do
  begin
    lDate := lDate + 1;
  end;
  Result := lDate;
end;

function TMyJSONRPCController.GetUser(aUserName: string): TPerson;
begin
  Result := TPerson.Create;
  Result.FirstName := 'Daniele (a.k.a. ' + aUserName + ')';
  Result.LastName := 'Teti';
  Result.DOB := EncodeDate(1932, 11, 4); // hey, it is a joke :-)
  Result.Married := True;
end;

procedure TMyJSONRPCController.InvalidMethod1(var MyVarParam: Integer);
begin
  // do nothing
end;

procedure TMyJSONRPCController.InvalidMethod2(out MyOutParam: Integer);
begin
  // do nothing
end;

procedure TMyJSONRPCController.OnBeforeAction(AContext: TWebContext; const AActionName: string; var AHandled: Boolean);
begin
  inherited;
  AHandled := False;
  if AContext.Request.Headers['x-token'] = '' then
  begin
    raise EMVCException.Create('Token is required');
  end;
end;

function TMyJSONRPCController.ReverseString(aString: string): string;
begin
  Result := System.StrUtils.ReverseString(aString);
end;

function TMyJSONRPCController.SavePerson(const aPerson: TJsonObject): Integer;
var
  lPerson: TPerson;
begin
  lPerson := JSONObjectAs<TPerson>(aPerson);
  try
    // do something with lPerson
  finally
    lPerson.Free;
  end;

  // this maybe the id of the newly created person
  Result := Random(1000);
end;

function TMyJSONRPCController.Subtract(aValue1, aValue2: Integer): Integer;
begin
  Result := aValue1 - aValue2;
end;

end.
