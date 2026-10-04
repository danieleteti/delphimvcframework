// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// ***************************************************************************

unit PeopleSampleU;

// The data behind the People pages (table, edit and new person forms): the row
// class, an in-memory store, the table query and the form check.
// The People pages have no login and no CSRF protection: add both before
// reusing them for real data.

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  MVCFramework.Validators;

const
  PEOPLE_ROLES = 'Admin,Analyst,Designer,Developer,Support';
  PEOPLE_STATUSES = 'Active,Invited,Suspended';

type
  { One row of the People table. Sample data kept in memory: replace it with a
    table (e.g. TMVCActiveRecord) in a real app. }
  TPersonRow = class
  private
    fID: Integer;
    fName, fEmail, fRole, fCity, fStatus, fNotes: String;
    fJoined: TDate;
    fProjects: Integer;
    fRemote: Boolean;
    function GetInitials: String;
  public
    constructor Create(AID: Integer; const AName, AEmail, ARole, ACity, AStatus: String; AJoined: TDate;
      AProjects: Integer; ARemote: Boolean = False; const ANotes: String = '');
    function Clone: TPersonRow;
    property ID: Integer read fID;
    [MVCRequired('Enter the name')]
    [MVCMinLength(2, 'Enter at least 2 characters')]
    [MVCMaxLength(60, 'Keep the name under 60 characters')]
    property Name: String read fName write fName;
    property Initials: String read GetInitials;
    [MVCRequired('Enter the email address')]
    [MVCEmail('Enter a valid email address')]
    [MVCMaxLength(254, 'Keep the email under 254 characters')]
    property Email: String read fEmail write fEmail;
    [MVCRequired('Choose a role')]
    [MVCIn(PEOPLE_ROLES, 'Choose a role')]
    property Role: String read fRole write fRole;
    [MVCMaxLength(40, 'Keep the city under 40 characters')]
    property City: String read fCity write fCity;
    [MVCRequired('Choose a status')]
    [MVCIn(PEOPLE_STATUSES, 'Choose a status')]
    property Status: String read fStatus write fStatus;
    [MVCPastOrPresent('Enter a date that is not in the future')]
    property Joined: TDate read fJoined write fJoined;
    [MVCRange(0, 100000, 'Enter a number from 0 to 100000')]
    property Projects: Integer read fProjects write fProjects;
    property Remote: Boolean read fRemote write fRemote;
    [MVCMaxLength(500, 'Keep the notes under 500 characters')]
    property Notes: String read fNotes write fNotes;
  end;

  // What the People page shows: the rows and the counters of the status filter
  TPeoplePage = record
    Rows: TObjectList<TPersonRow>; // owns the rows: free it after rendering
    Sort, Dir: String;             // normalized: a known column, asc or desc
    CountActive, CountInvited, CountSuspended: Integer;
  end;

// Search first (it drives the counters), then the status filter, then the order
function BuildPeoplePage(const Search, Status, Sort, Dir: String): TPeoplePage;
// A copy of one row (the caller frees it), nil when the ID does not exist
function FindPerson(ID: Integer): TPersonRow;
// An empty person with the defaults of the "New person" form (the caller frees it)
function NewPerson: TPersonRow;
procedure StorePerson(APerson: TPersonRow);
// Stores a copy of APerson under the next free ID and returns that ID (422 when the store is full)
function AddPerson(APerson: TPersonRow): Integer;
// Copies the posted fields into APerson and validates it: one message per invalid field,
// keyed by the lowercase field name the form uses
procedure ReadPerson(APerson: TPersonRow; const AFields, AErrors: TDictionary<String, String>);
// Options of the Role and Status selects
function PeopleRoles: TList<String>;
function PeopleStatuses: TList<String>;

implementation

uses
  System.StrUtils,
  System.DateUtils,
  System.Math,
  System.Generics.Defaults,
  MVCFramework.Commons,
  MVCFramework.ValidationEngine;

const
  MAX_PEOPLE = 500; // demo ceiling: the store lives in memory and anyone can add to it

var
  // The sample "table", shared by every request: read and written under TMonitor
  GPeople: TObjectList<TPersonRow>;
  GRoles, GStatuses: TList<String>;

constructor TPersonRow.Create(AID: Integer; const AName, AEmail, ARole, ACity, AStatus: String; AJoined: TDate;
  AProjects: Integer; ARemote: Boolean; const ANotes: String);
begin
  inherited Create;
  fID := AID;
  fName := AName;
  fEmail := AEmail;
  fRole := ARole;
  fCity := ACity;
  fStatus := AStatus;
  fJoined := AJoined;
  fProjects := AProjects;
  fRemote := ARemote;
  fNotes := ANotes;
end;

function TPersonRow.GetInitials: String;
var
  lParts: TArray<String>;
begin
  lParts := fName.Split([' ']);
  Result := Copy(lParts[0], 1, 1);
  if Length(lParts) > 1 then
    Result := Result + Copy(lParts[High(lParts)], 1, 1);
end;

function TPersonRow.Clone: TPersonRow;
begin
  Result := TPersonRow.Create(fID, fName, fEmail, fRole, fCity, fStatus, fJoined, fProjects, fRemote, fNotes);
end;

function PeopleRoles: TList<String>;
begin
  Result := GRoles;
end;

function PeopleStatuses: TList<String>;
begin
  Result := GStatuses;
end;

// A copy of every row: the caller owns it and reads it without the lock
function SamplePeople: TObjectList<TPersonRow>;
var
  lRow: TPersonRow;
begin
  Result := TObjectList<TPersonRow>.Create(True);
  TMonitor.Enter(GPeople);
  try
    for lRow in GPeople do
      Result.Add(lRow.Clone);
  finally
    TMonitor.Exit(GPeople);
  end;
end;

function FindPerson(ID: Integer): TPersonRow;
var
  lRow: TPersonRow;
begin
  Result := nil;
  TMonitor.Enter(GPeople);
  try
    for lRow in GPeople do
      if lRow.ID = ID then
        Exit(lRow.Clone);
  finally
    TMonitor.Exit(GPeople);
  end;
end;

function NewPerson: TPersonRow;
begin
  Result := TPersonRow.Create(0, '', '', '', '', 'Invited', Date, 0);
end;

procedure StorePerson(APerson: TPersonRow);
var
  I: Integer;
begin
  TMonitor.Enter(GPeople);
  try
    for I := 0 to GPeople.Count - 1 do
      if GPeople[I].ID = APerson.ID then
        GPeople[I] := APerson.Clone; // the list owns its rows: the old one is freed
  finally
    TMonitor.Exit(GPeople);
  end;
end;

function AddPerson(APerson: TPersonRow): Integer;
var
  lRow: TPersonRow;
begin
  TMonitor.Enter(GPeople);
  try
    if GPeople.Count >= MAX_PEOPLE then
      raise EMVCException.Create(HTTP_STATUS.UnprocessableEntity,
        Format('The sample store is full (%d people)', [MAX_PEOPLE]));
    Result := 0;
    for lRow in GPeople do
      Result := Max(Result, lRow.ID);
    Inc(Result);
    GPeople.Add(TPersonRow.Create(Result, APerson.Name, APerson.Email, APerson.Role, APerson.City,
      APerson.Status, APerson.Joined, APerson.Projects, APerson.Remote, APerson.Notes));
  finally
    TMonitor.Exit(GPeople);
  end;
end;

function BuildPeoplePage(const Search, Status, Sort, Dir: String): TPeoplePage;
var
  I, lSign: Integer;
  lRow: TPersonRow;
  lSort: String;
begin
  Result.Rows := SamplePeople;
  try
    Result.CountActive := 0;
    Result.CountInvited := 0;
    Result.CountSuspended := 0;
    for I := Result.Rows.Count - 1 downto 0 do
    begin
      lRow := Result.Rows[I];
      if (Search <> '') and not (ContainsText(lRow.Name, Search) or ContainsText(lRow.Email, Search) or
        ContainsText(lRow.Role, Search) or ContainsText(lRow.City, Search)) then
      begin
        Result.Rows.Delete(I);
        Continue;
      end;
      if SameText(lRow.Status, 'Active') then
        Inc(Result.CountActive)
      else if SameText(lRow.Status, 'Invited') then
        Inc(Result.CountInvited)
      else
        Inc(Result.CountSuspended);
      if (Status <> '') and not SameText(lRow.Status, Status) then
        Result.Rows.Delete(I);
    end;

    // Only known columns are accepted: anything else sorts by name
    lSort := LowerCase(Sort);
    if not MatchText(lSort, ['name', 'role', 'city', 'joined', 'projects']) then
      lSort := 'name';
    Result.Sort := lSort;
    if SameText(Dir, 'desc') then
    begin
      Result.Dir := 'desc';
      lSign := -1;
    end
    else
    begin
      Result.Dir := 'asc';
      lSign := 1;
    end;
    Result.Rows.Sort(TComparer<TPersonRow>.Construct(
      function(const L, R: TPersonRow): Integer
      begin
        if lSort = 'role' then
          Result := CompareText(L.Role, R.Role)
        else if lSort = 'city' then
          Result := CompareText(L.City, R.City)
        else if lSort = 'joined' then
          Result := CompareValue(L.Joined, R.Joined)
        else if lSort = 'projects' then
          Result := CompareValue(L.Projects, R.Projects)
        else
          Result := 0;
        if Result = 0 then
          Result := CompareText(L.Name, R.Name);
        Result := Result * lSign;
      end));
  except
    Result.Rows.Free;
    raise;
  end;
end;

// The keys of Request.ContentFields are lowercase: so are the keys of AErrors.
procedure ReadPerson(APerson: TPersonRow; const AFields, AErrors: TDictionary<String, String>);

  function Field(const AName: String): String;
  begin
    if not AFields.TryGetValue(AName, Result) then
      Result := '';
  end;

  function IsWholeNumber(const AText: String): Boolean;
  var
    C: Char;
  begin
    Result := (AText.Length > 0) and (AText.Length <= 9); // 9 digits always fit an Integer
    for C in AText do
      if not CharInSet(C, ['0'..'9']) then
        Exit(False);
  end;

var
  lJoined: TDateTime;
  lText: String;
  lErrors: TDictionary<String, String>;
  lPair: TPair<String, String>;
begin
  APerson.Name := Field('name').Trim;
  APerson.Email := Field('email').Trim;
  APerson.Role := Field('role');
  APerson.City := Field('city').Trim;
  APerson.Status := Field('status');
  // <input type="date"> posts yyyy-mm-dd, a local date: True reads it as is (False would shift it
  // as if it were UTC, and today would become "in the future" east of Greenwich)
  if TryISO8601ToDate(Field('joined'), lJoined, True) then
    APerson.Joined := DateOf(lJoined)
  else
    AErrors.AddOrSetValue('joined', 'Enter a valid date');
  lText := Field('projects').Trim;
  if IsWholeNumber(lText) then
    APerson.Projects := lText.ToInteger
  else
    AErrors.AddOrSetValue('projects', 'Enter a whole number, 0 or more');
  // The checkbox macro posts "false" from a hidden input, then "true" when checked:
  // ContentFields keeps the last value of a repeated field
  APerson.Remote := SameText(Field('remote'), 'true');
  APerson.Notes := Field('notes').Trim;

  // The rules are the validators on TPersonRow; a field that did not parse keeps its message
  if not TMVCValidationEngine.Validate(APerson, lErrors) then
    try
      for lPair in lErrors do
        if not AErrors.ContainsKey(LowerCase(lPair.Key)) then
          AErrors.Add(LowerCase(lPair.Key), lPair.Value);
    finally
      lErrors.Free;
    end;
end;

initialization

GPeople := TObjectList<TPersonRow>.Create(True);
GPeople.Add(TPersonRow.Create(1, 'Ada Rossi', 'ada.rossi@example.com', 'Admin', 'Rome', 'Active', EncodeDate(2021, 3, 14), 12, True, 'Owns the release process.'));
GPeople.Add(TPersonRow.Create(2, 'Bruno Ferri', 'bruno.ferri@example.com', 'Developer', 'Milan', 'Active', EncodeDate(2022, 7, 2), 7));
GPeople.Add(TPersonRow.Create(3, 'Daniele Teti', 'daniele.teti@example.com', 'Admin', 'Rome', 'Active', EncodeDate(2010, 1, 11), 42, True));
GPeople.Add(TPersonRow.Create(4, 'Chiara Neri', 'chiara.neri@example.com', 'Designer', 'Turin', 'Invited', EncodeDate(2025, 11, 20), 0));
GPeople.Add(TPersonRow.Create(5, 'Diego Alves', 'diego.alves@example.com', 'Developer', 'Lisbon', 'Active', EncodeDate(2020, 1, 9), 18, True));
GPeople.Add(TPersonRow.Create(6, 'Elena Petrova', 'elena.petrova@example.com', 'Analyst', 'Berlin', 'Active', EncodeDate(2023, 5, 30), 5));
GPeople.Add(TPersonRow.Create(7, 'Farid Haddad', 'farid.haddad@example.com', 'Support', 'Paris', 'Suspended', EncodeDate(2019, 9, 17), 3));
GPeople.Add(TPersonRow.Create(8, 'Grace Kim', 'grace.kim@example.com', 'Developer', 'Toronto', 'Active', EncodeDate(2024, 2, 5), 9, True));
GPeople.Add(TPersonRow.Create(9, 'Hugo Martin', 'hugo.martin@example.com', 'Designer', 'Lyon', 'Active', EncodeDate(2022, 10, 11), 6));
GPeople.Add(TPersonRow.Create(10, 'Ines Duarte', 'ines.duarte@example.com', 'Analyst', 'Porto', 'Invited', EncodeDate(2026, 1, 8), 0));
GPeople.Add(TPersonRow.Create(11, 'Jonas Weber', 'jonas.weber@example.com', 'Developer', 'Munich', 'Active', EncodeDate(2018, 6, 25), 24));
GPeople.Add(TPersonRow.Create(12, 'Keiko Sato', 'keiko.sato@example.com', 'Admin', 'Osaka', 'Active', EncodeDate(2021, 12, 1), 11, True));
GPeople.Add(TPersonRow.Create(13, 'Luca Bianchi', 'luca.bianchi@example.com', 'Support', 'Bologna', 'Suspended', EncodeDate(2020, 4, 19), 2));
GPeople.Add(TPersonRow.Create(14, 'Maya Patel', 'maya.patel@example.com', 'Developer', 'Austin', 'Active', EncodeDate(2023, 8, 14), 8));
GPeople.Add(TPersonRow.Create(15, 'Noah Brown', 'noah.brown@example.com', 'Analyst', 'Dublin', 'Invited', EncodeDate(2026, 2, 27), 0));
GRoles := TList<String>.Create;
GRoles.AddRange(PEOPLE_ROLES.Split([',']));
GStatuses := TList<String>.Create;
GStatuses.AddRange(PEOPLE_STATUSES.Split([',']));

finalization

GStatuses.Free;
GRoles.Free;
GPeople.Free;

end.
