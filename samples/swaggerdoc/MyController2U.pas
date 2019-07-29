unit MyController2U;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Swagger.Commons,
  MVCFramework.Serializer.Commons;

type
  [MVCNameCase(ncLowerCase)]
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

  [MVCPath('/person')]
  TMyController2 = class(TMVCController)
  public
    [MVCPath('/($Id)')]
    [MVCSwagSummary('Person', 'List Persons')]
    [MVCSwagParam(plPath, 'Id', 'Person id', ptInteger)]
    [MVCSwagParam(plQuery, 'filter', 'Search filter', ptString)]
    [MVCSwagParam(plQuery, 'per_page', 'Items per page', ptInteger)]
    [MVCSwagResponses(200, 'Sucess')]
    [MVCSwagResponses(500, 'Internal Server Error')]
    [MVCHTTPMethod([httpGET])]
    procedure GetPerson(const Id: Integer);

    [MVCPath('')]
    [MVCSwagSummary('Person', 'Insert Person')]
    [MVCSwagParam(plBody, '', 'Person object', TPerson, ptNotDefined, True)]
    [MVCSwagResponses(201, 'Created')]
    [MVCSwagResponses(500, 'Internal Server Error')]
    [MVCConsumes(TMVCMediaType.APPLICATION_JSON)]
    [MVCHTTPMethod([httpPOST])]
    procedure InsertPerson;
  end;

implementation

uses
  MVCFramework.Controllers.Register;

{ TMyController2 }

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

procedure TMyController2.InsertPerson;
begin
  ResponseStatus(201, 'Created');
end;

initialization

TControllersRegister.Instance.RegisterController(TMyController2, 'MyServerName');

end.
