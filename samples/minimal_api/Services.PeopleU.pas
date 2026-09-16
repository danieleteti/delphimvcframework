unit Services.PeopleU;

interface

uses
  System.Generics.Collections,
  System.SyncObjs,
  Entities.PersonU;

type
  IPeopleService = interface
    ['{8E2D9C44-25C6-4A8E-9C09-7A1C9C76FA01}']
    function GetAll: TPeopleList;
    function GetByID(const ID: Integer): TPerson;
    function Add(const Person: TPerson): TPerson;
    procedure Update(const ID: Integer; const Person: TPerson);
    procedure Delete(const ID: Integer);
  end;

  TPeopleService = class(TInterfacedObject, IPeopleService)
  strict private
    fStore: TObjectDictionary<Integer, TPerson>;
    fNextID: Integer;
    fLock: TCriticalSection;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function GetAll: TPeopleList;
    function GetByID(const ID: Integer): TPerson;
    function Add(const Person: TPerson): TPerson;
    procedure Update(const ID: Integer; const Person: TPerson);
    procedure Delete(const ID: Integer);
  end;

implementation

uses
  System.SysUtils;

constructor TPeopleService.Create;
var
  lPerson: TPerson;
begin
  inherited Create;
  fLock := TCriticalSection.Create;
  fStore := TObjectDictionary<Integer, TPerson>.Create([doOwnsValues]);
  fNextID := 1;

  // Seed
  lPerson := TPerson.Create;
  lPerson.ID := fNextID;
  lPerson.FirstName := 'Daniele';
  lPerson.LastName := 'Teti';
  lPerson.Age := 47;
  fStore.Add(fNextID, lPerson);
  Inc(fNextID);

  lPerson := TPerson.Create;
  lPerson.ID := fNextID;
  lPerson.FirstName := 'Cesar';
  lPerson.LastName := 'Romero';
  lPerson.Age := 50;
  fStore.Add(fNextID, lPerson);
  Inc(fNextID);
end;

destructor TPeopleService.Destroy;
begin
  fStore.Free;
  fLock.Free;
  inherited;
end;

function TPeopleService.GetAll: TPeopleList;
var
  lPerson: TPerson;
  lCopy: TPerson;
begin
  Result := TPeopleList.Create(True);
  fLock.Enter;
  try
    for lPerson in fStore.Values do
    begin
      lCopy := TPerson.Create;
      lCopy.ID := lPerson.ID;
      lCopy.FirstName := lPerson.FirstName;
      lCopy.LastName := lPerson.LastName;
      lCopy.Age := lPerson.Age;
      Result.Add(lCopy);
    end;
  finally
    fLock.Leave;
  end;
end;

function TPeopleService.GetByID(const ID: Integer): TPerson;
var
  lPerson: TPerson;
begin
  Result := nil;
  fLock.Enter;
  try
    if fStore.TryGetValue(ID, lPerson) then
    begin
      Result := TPerson.Create;
      Result.ID := lPerson.ID;
      Result.FirstName := lPerson.FirstName;
      Result.LastName := lPerson.LastName;
      Result.Age := lPerson.Age;
    end;
  finally
    fLock.Leave;
  end;
end;

function TPeopleService.Add(const Person: TPerson): TPerson;
var
  lStored: TPerson;
begin
  fLock.Enter;
  try
    lStored := TPerson.Create;
    lStored.ID := fNextID;
    lStored.FirstName := Person.FirstName;
    lStored.LastName := Person.LastName;
    lStored.Age := Person.Age;
    fStore.Add(fNextID, lStored);
    Inc(fNextID);

    Result := TPerson.Create;
    Result.ID := lStored.ID;
    Result.FirstName := lStored.FirstName;
    Result.LastName := lStored.LastName;
    Result.Age := lStored.Age;
  finally
    fLock.Leave;
  end;
end;

procedure TPeopleService.Update(const ID: Integer; const Person: TPerson);
var
  lExisting: TPerson;
begin
  fLock.Enter;
  try
    if not fStore.TryGetValue(ID, lExisting) then
      raise Exception.CreateFmt('Person with ID %d not found', [ID]);
    lExisting.FirstName := Person.FirstName;
    lExisting.LastName := Person.LastName;
    lExisting.Age := Person.Age;
  finally
    fLock.Leave;
  end;
end;

procedure TPeopleService.Delete(const ID: Integer);
begin
  fLock.Enter;
  try
    if not fStore.ContainsKey(ID) then
      raise Exception.CreateFmt('Person with ID %d not found', [ID]);
    fStore.Remove(ID);
  finally
    fLock.Leave;
  end;
end;

end.
