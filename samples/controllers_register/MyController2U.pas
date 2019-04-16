unit MyController2U;

interface

uses
  MVCFramework,
  MVCFramework.Commons;

type

  [MVCPath('/api/person')]
  TMyController2 = class(TMVCController)
  public
    [MVCPath('')]
    [MVCHTTPMethod([httpGET])]
    procedure GetPerson;
  end;

implementation

uses
  MVCFramework.Controllers.Register;

{ TMyController2 }

type
  TPerson = class
  private
    FName: string;
    FAge: Integer;
    FCountry: string;
  public
    property Name: string read FName write FName;
    property Age: Integer read FAge write FAge;
    property Country: string read FCountry write FCountry;
  end;

procedure TMyController2.GetPerson;
var
  LPerson: TPerson;
begin
  LPerson := TPerson.Create;
  LPerson.Name := 'João Antônio Duarte';
  LPerson.Age := 26;
  LPerson.Country := 'Brasil';
  Render(LPerson);
end;

initialization

TControllersRegister.Instance.RegisterController(TMyController2, 'MyServerName');

end.
