unit DAL;

interface

uses
  System.JSON;

type
  IPeopleDAL = interface(IInterface)
    ['{3E534A3E-EAEB-44ED-B74E-EFBBAAAE11B4}']
    function GetPeople: TJSONArray;
    function GetPersonByID(ID: string): TJSONObject;
    procedure AddPerson(FirstName, LastName: string; Age: Integer);
    procedure DeleteById(const ID: string);
  end;

  TPeopleDAL = class(TInterfacedObject, IPeopleDAL)
  private const
    DATAFILE: string = 'people.data';
  public
    function GetPeople: TJSONArray;
    function GetPersonByID(ID: string): TJSONObject;
    procedure AddPerson(FirstName, LastName: string; Age: Integer);
    procedure DeleteById(const ID: string);
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

procedure TPeopleDAL.AddPerson(FirstName, LastName: string; Age: Integer);
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
        .AddPair('age', TJSONNumber.Create(Age))
        .AddPair('id', TGUID.NewGuid.ToString.Replace('{', '').Replace('}', ''));
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

procedure TPeopleDAL.DeleteById(const ID: string);
var
  LData: string;
  LJPeople: TJSONArray;
  I: Integer;
begin
  _CS.Enter;
  try
    if TFile.Exists(DATAFILE) then
      LData := TFile.ReadAllText(DATAFILE).Trim;
    if not LData.IsEmpty then
    begin
      LJPeople := TJSONObject.ParseJSONValue(LData) as TJSONArray;
      try
        for I := 0 to LJPeople.Count - 1 do
        begin
          if (LJPeople.Items[I] as TJSONObject).GetValue('id').Value = ID then
          begin
            LJPeople.Remove(I);
            Break;
          end;
        end;
        TFile.WriteAllText(DATAFILE, LJPeople.ToJSON);
      finally
        LJPeople.Free;
      end;
    end
  finally
    _CS.Leave;
  end;
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

function TPeopleDAL.GetPersonByID(ID: string): TJSONObject;
var
  lPeople: TJSONArray;
  I: Integer;
begin
  Result := nil;
  lPeople := GetPeople;
  try
    for I := 0 to lPeople.Count - 1 do
    begin
      if lPeople.Items[I].GetValue<TJSONString>('id').Value = ID then
      begin
        Result := lPeople.Items[I].Clone as TJSONObject;
        Break;
      end;

    end;
  finally
    lPeople.Free;
  end;
end;

initialization

_CS := TCriticalSection.Create;

finalization

_CS.Free;

end.
