unit BusinessObjectsU;

interface

uses
  ObjectsMappers;

type

  [MapperJSONNaming(JSONNameLowerCase)]
  TPerson = class
  private
    FLastName: String;
    FDOB: TDate;
    FFirstName: String;
    FMarried: boolean;
    procedure SetDOB(const Value: TDate);
    procedure SetFirstName(const Value: String);
    procedure SetLastName(const Value: String);
    procedure SetMarried(const Value: boolean);
  public
    property FirstName: String read FFirstName write SetFirstName;
    property LastName: String read FLastName write SetLastName;
    property DOB: TDate read FDOB write SetDOB;
    property Married: boolean read FMarried write SetMarried;
  end;

  [MapperJSONNaming(JSONNameLowerCase)]
  TCustomer = class
  private
    FName: String;
    FAddressLine2: String;
    FAddressLine1: String;
    FContactFirst: String;
    FCity: String;
    FContactLast: String;
    procedure SetAddressLine1(const Value: String);
    procedure SetAddressLine2(const Value: String);
    procedure SetCity(const Value: String);
    procedure SetContactFirst(const Value: String);
    procedure SetContactLast(const Value: String);
    procedure SetName(const Value: String);
  public
    property Name: String read FName write SetName;
    [MapperTransient]
    property ContactFirst: String read FContactFirst write SetContactFirst;
    [MapperTransient]
    property ContactLast: String read FContactLast write SetContactLast;
    property AddressLine1: String read FAddressLine1 write SetAddressLine1;
    property AddressLine2: String read FAddressLine2 write SetAddressLine2;
    property City: String read FCity write SetCity;
  end;

implementation

{ TPerson }

procedure TPerson.SetDOB(const Value: TDate);
begin
  FDOB := Value;
end;

procedure TPerson.SetFirstName(const Value: String);
begin
  FFirstName := Value;
end;

procedure TPerson.SetLastName(const Value: String);
begin
  FLastName := Value;
end;

procedure TPerson.SetMarried(const Value: boolean);
begin
  FMarried := Value;
end;

{ TCustomer }

procedure TCustomer.SetAddressLine1(const Value: String);
begin
  FAddressLine1 := Value;
end;

procedure TCustomer.SetAddressLine2(const Value: String);
begin
  FAddressLine2 := Value;
end;

procedure TCustomer.SetCity(const Value: String);
begin
  FCity := Value;
end;

procedure TCustomer.SetContactFirst(const Value: String);
begin
  FContactFirst := Value;
end;

procedure TCustomer.SetContactLast(const Value: String);
begin
  FContactLast := Value;
end;

procedure TCustomer.SetName(const Value: String);
begin
  FName := Value;
end;

end.
