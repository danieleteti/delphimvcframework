unit MyObjectsU;

interface

type
  TPerson = class
  private
    FFirstName: String;
    FLastName: String;
    FAge: Integer;
    procedure SetFirstName(const Value: String);
    procedure SetLastName(const Value: String);
    procedure SetAge(const Value: Integer);
  public
    constructor Create(AFirstName, ALastName: String; AAge: Integer); virtual;
    property FirstName: String read FFirstName write SetFirstName;
    property LastName: String read FLastName write SetLastName;
    property Age: Integer read FAge write SetAge;
  end;

implementation

{ TPerson }

constructor TPerson.Create(AFirstName, ALastName: string; AAge: Integer);
begin
  inherited Create;
  FFirstName := AFirstName;
  FLastName := ALastName;
  FAge := AAge;
end;

procedure TPerson.SetAge(const Value: Integer);
begin
  FAge := Value;
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
