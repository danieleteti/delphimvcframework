unit DAL;

interface

uses
  System.JSON;

type
  IPeopleDAL = interface
    ['{3E534A3E-EAEB-44ED-B74E-EFBBAAAE11B4}']
    function GetPeople: TJSONArray;
    procedure AddPerson(FirstName, LastName: String; Age: Integer);
  end;

  TPeopleDAL = class(TInterfacedObject, IPeopleDAL)
  private const
    DATAFILE: String = 'people.data';
  public
    function GetPeople: TJSONArray;
    procedure AddPerson(FirstName, LastName: String; Age: Integer);
  end;

  TServicesFactory = class sealed
    class function GetPeopleDAL: IPeopleDAL;
  end;

implementation

uses
  System.IOUtils, System.SysUtils, System.SyncObjs;

var
  // Hey! The storage is a simple json file, so some synchronization is needed
  _CS: TCriticalSection = nil;

  { TSimpleDAL }

procedure TPeopleDAL.AddPerson(FirstName, LastName: String; Age: Integer);
var
  LJPeople: TJSONArray;
  LJPerson: TJSONObject;
begin
  _CS.Enter;
  try
    LJPeople := GetPeople;
    try
      LJPerson := TJSONObject.Create;
      LJPeople.AddElement(LJPerson);
      LJPerson.AddPair('first_name', FirstName).AddPair('last_name', LastName)
        .AddPair('age', TJSONNumber.Create(Age));
      TFile.WriteAllText(DATAFILE, LJPeople.ToJSON);
    finally
      LJPeople.Free;
    end;
  finally
    _CS.Leave;
  end;
end;

class function TServicesFactory.GetPeopleDAL: IPeopleDAL;
begin
  Result := TPeopleDAL.Create;
end;

function TPeopleDAL.GetPeople: TJSONArray;
var
  LData: string;
begin
  _CS.Enter;
  try
    if TFile.Exists(DATAFILE) then
      LData := TFile.ReadAllText(DATAFILE).Trim;
    if not LData.IsEmpty then
    begin
      Result := TJSONObject.ParseJSONValue(LData) as TJSONArray;
    end
    else
    begin
      Result := TJSONArray.Create;
    end;
  finally
    _CS.Leave;
  end;
end;

initialization

_CS := TCriticalSection.Create;

finalization

_CS.Free;

end.
