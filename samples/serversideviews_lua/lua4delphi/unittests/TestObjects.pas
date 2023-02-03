unit TestObjects;

interface

type
  TPerson = class(TObject)
  private
    FLastName: String;
    FFirstName: String;
    FAge: Integer;
    FChild: TPerson;
    procedure SetLastName(const Value: String);
    procedure SetFirstName(const Value: String);
    procedure SetAge(const Value: Integer);
    procedure SetChild(const Value: TPerson);
  public
    constructor Create;
    function GetFullName(ASalutation: String): String;
    procedure BecomeOlder(HowManyYear: Integer);
    property Child: TPerson read FChild write SetChild;
    property LastName: String read FLastName write SetLastName;
    property FirstName: String read FFirstName write SetFirstName;
    property Age: Integer read FAge write SetAge;
  end;

implementation

procedure TPerson.BecomeOlder(HowManyYear: Integer);
begin
  FAge := FAge + HowManyYear;
end;

constructor TPerson.Create;
begin
  inherited;
end;

function TPerson.GetFullName(ASalutation: String): String;
begin
  Result := ASalutation + ' ' + FFirstName + ' ' + FLastName;
end;

procedure TPerson.SetAge(const Value: Integer);
begin
  FAge := Value;
end;

procedure TPerson.SetChild(const Value: TPerson);
begin
  FChild := Value;
end;

procedure TPerson.SetFirstName(const Value: String);
begin
  FFirstName := Value;
end;

procedure TPerson.SetLastName(const Value: String);
begin
  FLastName := Value;
end;

end.
