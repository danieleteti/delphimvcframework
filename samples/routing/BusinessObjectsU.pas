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

end.
