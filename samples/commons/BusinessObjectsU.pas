unit BusinessObjectsU;

interface

uses
  ObjectsMappers;

type

  [MapperJSONNaming(JSONNameLowerCase)]
  TPerson = class
  private
    FLastName: string;
    FDOB: TDate;
    FFirstName: string;
    FMarried: boolean;
    procedure SetDOB(const Value: TDate);
    procedure SetFirstName(const Value: string);
    procedure SetLastName(const Value: string);
    procedure SetMarried(const Value: boolean);
  public
    // [MapperJsonSer('nome')]
    property FirstName: string read FFirstName write SetFirstName;
    // [DoNotSerialize]
    property LastName: string read FLastName write SetLastName;
    property DOB: TDate read FDOB write SetDOB;
    property Married: boolean read FMarried write SetMarried;
  end;

  [MapperJSONNaming(JSONNameLowerCase)]
  TCustomer = class
  private
    FName: string;
    FAddressLine2: string;
    FAddressLine1: string;
    FContactFirst: string;
    FCity: string;
    FContactLast: string;
    procedure SetAddressLine1(const Value: string);
    procedure SetAddressLine2(const Value: string);
    procedure SetCity(const Value: string);
    procedure SetContactFirst(const Value: string);
    procedure SetContactLast(const Value: string);
    procedure SetName(const Value: string);
  public
    property name: string read FName write SetName;
    [MapperTransient]
    property ContactFirst: string read FContactFirst write SetContactFirst;
    [MapperTransient]
    property ContactLast: string read FContactLast write SetContactLast;
    property AddressLine1: string read FAddressLine1 write SetAddressLine1;
    property AddressLine2: string read FAddressLine2 write SetAddressLine2;
    property City: string read FCity write SetCity;
  end;

  [MapperJSONNaming(JSONNameLowerCase)]
  TProgrammer = class(TPerson)
  private
    FSkills: string;
    procedure SetSkills(const Value: string);
  public
    property Skills: string read FSkills write SetSkills;
  end;

  [MapperJSONNaming(JSONNameLowerCase)]
  TPhilosopher = class(TPerson)
  private
    FMentors: String;
    procedure SetMentors(const Value: String);
  public
    property Mentors: String read FMentors write SetMentors;
  end;

implementation

{ TPerson }

procedure TPerson.SetDOB(const Value: TDate);
begin
  FDOB := Value;
end;

procedure TPerson.SetFirstName(const Value: string);
begin
  FFirstName := Value;
end;

procedure TPerson.SetLastName(const Value: string);
begin
  FLastName := Value;
end;

procedure TPerson.SetMarried(const Value: boolean);
begin
  FMarried := Value;
end;

{ TCustomer }

procedure TCustomer.SetAddressLine1(const Value: string);
begin
  FAddressLine1 := Value;
end;

procedure TCustomer.SetAddressLine2(const Value: string);
begin
  FAddressLine2 := Value;
end;

procedure TCustomer.SetCity(const Value: string);
begin
  FCity := Value;
end;

procedure TCustomer.SetContactFirst(const Value: string);
begin
  FContactFirst := Value;
end;

procedure TCustomer.SetContactLast(const Value: string);
begin
  FContactLast := Value;
end;

procedure TCustomer.SetName(const Value: string);
begin
  FName := Value;
end;

{ TProgrammer }

procedure TProgrammer.SetSkills(const Value: string);
begin
  FSkills := Value;
end;

{ TPhilosopher }

procedure TPhilosopher.SetMentors(const Value: String);
begin
  FMentors := Value;
end;

end.
