unit MyController2U;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Swagger.Commons,
  MVCFramework.Serializer.Commons,
  MVCFramework.Middleware.Authentication.RoleBasedAuthHandler;

type
  [MVCNameCase(ncLowerCase)]
  TPerson = class
  private
    FName: string;
    FAge: Integer;
    FCountry: string;
    FCode: Integer;
  public
    [MVCSwagJsonSchemaField(stInteger, 'code', 'person id', True, False)]
    property Code: Integer read FCode write FCode;
    [MVCSwagJsonSchemaField('name', 'person name', True, False)]
    property Name: string read FName write FName;
    [MVCSwagJsonSchemaField('age', 'person age', True, False)]
    property Age: Integer read FAge write FAge;
    [MVCSwagJsonSchemaField('country', 'Nationality of the person', True, False)]
    property Country: string read FCountry write FCountry;
  end;

  [MVCPath('/person')]
  TMyController2 = class(TMVCController)
  public
    [MVCPath('/($Id)')]
    [MVCHTTPMethod([httpGET])]
    [MVCSwagSummary('Person', 'List Persons', '66e83aa7-d170-44a7-a502-8f25ddd2a18a')]
    [MVCSwagParam(plPath, 'Id', 'Person id', ptInteger)]
    [MVCSwagParam(plQuery, 'filter', 'Search filter', ptString)]
    [MVCSwagParam(plQuery, 'per_page', 'Items per page', ptInteger)]
    [MVCSwagResponses(200, 'Success', TPerson)]
    [MVCSwagResponses(500, 'Internal Server Error')]
    procedure GetPerson(const Id: Integer);

    [MVCPath('')]
    [MVCHTTPMethod([httpPOST])]
    [MVCSwagSummary('Person', 'Insert Person')]
    [MVCSwagParam(plBody, 'entity', 'Person object', TPerson, ptNotDefined, True)]
    [MVCSwagResponses(201, 'Created')]
    [MVCSwagResponses(401, 'Requires Authentication')]
    [MVCSwagResponses(500, 'Internal Server Error')]
    [MVCConsumes(TMVCMediaType.APPLICATION_JSON)]
    [MVCRequiresAuthentication]
    procedure InsertPerson;
  end;

implementation

uses
  MVCFramework.Controllers.Register;

{ TMyController2 }

procedure TMyController2.GetPerson(const Id: Integer);
var
  LPerson: TPerson;
begin
  LPerson := TPerson.Create;
  LPerson.Code := Id;
  LPerson.Name := 'João Antônio Duarte';
  LPerson.Age := 26;
  LPerson.Country := 'Brasil';
  Render(LPerson);
end;

procedure TMyController2.InsertPerson;
var
  LPerson: TPerson;
begin
  LPerson := Context.Request.BodyAs<TPerson>;
  Render(LPerson);
  ResponseStatus(201, 'Created');
end;

initialization

TControllersRegister.Instance.RegisterController(TMyController2, 'MyServerName');

end.
