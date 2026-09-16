unit BusinessObjectsU;

interface

uses
  MVCFramework.Serializer.Commons;

type
  [MVCNameCase(ncLowerCase)]
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
    procedure Validate;
    property FirstName: String read FFirstName write SetFirstName;
    property LastName: String read FLastName write SetLastName;
    property DOB: TDate read FDOB write SetDOB;
    property Married: boolean read FMarried write SetMarried;
  end;

  { The criteria of a customer search. This is the shape a query string cannot
    carry honestly: Cities is a list, and PriceRange is a nested object. With
    GET you would end up encoding them by hand - cities=rome,milan&min=10&max=90 -
    and writing a parser for your own ad-hoc format on the server. }
  [MVCNameCase(ncLowerCase)]
  TPriceRange = class
  private
    FMin: Integer;
    FMax: Integer;
  public
    property Min: Integer read FMin write FMin;
    property Max: Integer read FMax write FMax;
  end;

  [MVCNameCase(ncLowerCase)]
  TCustomerSearch = class
  private
    FSearchText: String;
    FCities: TArray<String>;
    FPriceRange: TPriceRange;
    FOrderBy: String;
    FPage: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    property SearchText: String read FSearchText write FSearchText;
    property Cities: TArray<String> read FCities write FCities;
    property PriceRange: TPriceRange read FPriceRange write FPriceRange;
    property OrderBy: String read FOrderBy write FOrderBy;
    property Page: Integer read FPage write FPage;
  end;

implementation

uses
  System.SysUtils;

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

procedure TPerson.Validate;
begin
  if FirstName.Trim.IsEmpty or LastName.Trim.IsEmpty then
    raise Exception.Create('Invalid person. First name and last name required');
end;

{ TCustomerSearch }

constructor TCustomerSearch.Create;
begin
  inherited;
  { The deserializer fills a nested object, it does not create one. }
  FPriceRange := TPriceRange.Create;
  FPage := 1;
end;

destructor TCustomerSearch.Destroy;
begin
  FPriceRange.Free;
  inherited;
end;

end.
