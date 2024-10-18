unit BusinessObjectsU;

interface

type
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

end.
