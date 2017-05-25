unit PersonU;

interface

uses
  MVCFramework.Serializer.Commons;

type
  TPerson = class
  private
    FLastName: string;
    FDateOfBirth: TDate;
    FFirstName: string;
    procedure SetDateOfBirth(const Value: TDate);
    procedure SetFirstName(const Value: string);
    procedure SetLastName(const Value: string);
  public
    property FirstName: string read FFirstName write SetFirstName;
    property LastName: string read FLastName write SetLastName;
    property DateOfBirth: TDate read FDateOfBirth write SetDateOfBirth;
  end;

  [MapperJSONNaming(TJSONNameCase.JSONNameUpperCase)]
  TPersonUpperCase = class(TPerson)

  end;

  [MapperJSONNaming(TJSONNameCase.JSONNameLowerCase)]
  TPersonLowerCase = class(TPerson)

  end;

  TPersonCustomCase = class
  private
    FPhoneNumber: string;
    FWorkEmail: string;
    FLastName: string;
    FFirstName: string;
    FDateOfBirth: TDate;
    procedure SetPhoneNumber(const Value: string);
    procedure SetWorkEmail(const Value: string);
    procedure SetFirstName(const Value: string);
    procedure SetLastName(const Value: string);
    procedure SetDateOfBirth(const Value: TDate);
  public
    [MapperJSONSer('first_name')]
    property FirstName: string read FFirstName write SetFirstName;
    [MapperJSONSer('last_name')]
    property LastName: string read FLastName write SetLastName;
    [MapperJSONSer('date_of_birth')]
    property DateOfBirth: TDate read FDateOfBirth write SetDateOfBirth;
    [MapperJSONSer('phoneNumber')]
    property PhoneNumber: string read FPhoneNumber write SetPhoneNumber;
    [MapperJSONSer('workEmail')]
    property WorkEmail: string read FWorkEmail write SetWorkEmail;
  end;

implementation

{ TPerson }

procedure TPerson.SetDateOfBirth(const Value: TDate);
begin
  FDateOfBirth := Value;
end;

procedure TPerson.SetFirstName(const Value: string);
begin
  FFirstName := Value;
end;

procedure TPerson.SetLastName(const Value: string);
begin
  FLastName := Value;
end;

{ TPersonCustomCase }

procedure TPersonCustomCase.SetDateOfBirth(const Value: TDate);
begin
  FDateOfBirth := Value;
end;

procedure TPersonCustomCase.SetFirstName(const Value: string);
begin
  FFirstName := Value;
end;

procedure TPersonCustomCase.SetLastName(const Value: string);
begin
  FLastName := Value;
end;

procedure TPersonCustomCase.SetPhoneNumber(const Value: string);
begin
  FPhoneNumber := Value;
end;

procedure TPersonCustomCase.SetWorkEmail(const Value: string);
begin
  FWorkEmail := Value;
end;

end.
