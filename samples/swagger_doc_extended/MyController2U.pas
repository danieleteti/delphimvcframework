unit MyController2U;

interface

uses
  System.Generics.Collections,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Swagger.Commons,
  MVCFramework.Serializer.Commons,
  MVCFramework.Middleware.Authentication.RoleBasedAuthHandler,
  MVCFramework.Nullables;

type

  [MVCNameCase(ncLowerCase)]
  TAddress = class(TInterfacedObject)
  private
    FStreet: string;
    FNumber: Integer;
    FCity: string;
    FPostalCode: NullableString;
  public
    property Street: string read FStreet write FStreet;
    property Number: Integer read FNumber write FNumber;
    property City: string read FCity write FCity;
    property PostalCode: NullableString read FPostalCode write FPostalCode;
  end;

  [MVCNameCase(ncLowerCase)]
  TPhone = class
  private
    FDescription: string;
    FNumber: string;
  public
    property Description: string read FDescription write FDescription;
    property Number: string read FNumber write FNumber;
  end;

  [MVCNameCase(ncLowerCase)]
  TPhones = class(TObjectList<TPhone>)
  end;

  TGender = (Male, Female);

  [MVCNameCase(ncLowerCase)]
  TPerson = class
  private
    FName: string;
    FAge: Integer;
    FCountry: string;
    FCode: Integer;
    FAddress: TAddress;
    FPhones: TPhones;
    FGender: TGender;
    FArrayField: TArray<string>;
    FListField: TList<string>;
  public
    constructor Create;
    destructor Destroy; override;

    [MVCSwagJsonSchemaField(stInteger, 'code', 'person id', True, False)]
    property Code: Integer read FCode write FCode;
    [MVCSwagJsonSchemaField('name', 'person name', True, False, 5, 100)]
    property Name: string read FName write FName;
    [MVCSwagJsonSchemaField('gender', 'person gender', True, False)]
    [MVCEnumSerialization(estEnumName)]
    property Gender: TGender read FGender write FGender;
    [MVCSwagJsonSchemaField('age', 'person age', True, False)]
    property Age: Integer read FAge write FAge;
    [MVCSwagJsonSchemaField('country', 'Nationality of the person', True, False, 0, 100)]
    property Country: string read FCountry write FCountry;
    property Address: TAddress read FAddress write FAddress;
    property Phones: TPhones read FPhones write FPhones;
    [MVCSwagJsonSchemaField(stArray, 'arrayfield', 'Array Field', True, False)]
    property ArrayField: TArray<string> read FArrayField write FArrayField;
    [MVCSwagJsonSchemaField(stArray, 'listfield', 'List Field', True, False)]
    property ListField: TList<string> read FListField write FListField;
  end;

  [MVCNameCase(ncLowerCase)]
  TPeople = class(TObjectList<TPerson>)
  end;

//  [MVCSwagIgnorePath] { Ignore all methods of controller }
  [MVCPath('/people')]
  [MVCSwagAuthentication(atJsonWebToken)]
  TMyController2 = class(TMVCController)
  public
    [MVCPath('')]
    [MVCHTTPMethod([httpGET])]
    [MVCSwagSummary('People', 'List all persons', 'getPeople')]
    [MVCSwagParam(plQuery, 'per_page', 'Items per page', ptInteger, False, '50')]
    [MVCSwagParam(plQuery, 'enumparam', 'Enum param sample', ptString, False, 'enumvalue1',
      'enumvalue1,enumvalue2,enumvalue3,enumvalue4')]
    [MVCSwagResponses(200, 'Success', TPerson, True)]
    [MVCSwagResponses(500, 'Internal Server Error')]
    procedure GetAllPeople;

    [MVCPath('/($Id)')]
    [MVCHTTPMethod([httpGET])]
    [MVCSwagSummary('People', 'List Persons by Id', 'getPersonById')]
    [MVCSwagParam(plPath, 'Id', 'Person id', ptInteger)]
    [MVCSwagResponses(200, 'Success', TPerson)]
    [MVCSwagResponses(500, 'Internal Server Error')]
    procedure GetPerson(const Id: Integer);

//    [MVCSwagIgnorePath]  { Ignore this method only }
    [MVCPath('')]
    [MVCHTTPMethod([httpPOST])]
    [MVCSwagSummary('People', 'Insert Person', 'createPerson')]
    [MVCSwagParam(plBody, 'entity', 'Person object', TPerson)]
    [MVCSwagResponses(201, 'Created')]
    [MVCSwagResponses(401, 'Requires Authentication')]
    [MVCSwagResponses(500, 'Internal Server Error')]
    [MVCConsumes(TMVCMediaType.APPLICATION_JSON)]
    procedure InsertPerson;
  end;

implementation

uses MVCFramework.Controllers.Register;

{ TMyController2 }

procedure TMyController2.GetAllPeople;
var
  LPerson: TPerson;
  LPersons: TObjectList<TPerson>;
begin
  LPersons := TObjectList<TPerson>.Create;
  LPerson := TPerson.Create;
  LPerson.Code := 1;
  LPerson.Name := 'João Antônio Duarte';
  LPerson.Age := 26;
  LPerson.Country := 'Brasil';
  LPersons.Add(LPerson);

  Render<TPerson>(LPersons);
end;

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

{ TPerson }

constructor TPerson.Create;
begin
  inherited;
  FAddress := TAddress.Create;
  FPhones := TPhones.Create;
end;

destructor TPerson.Destroy;
begin
  FAddress.Free;
  FPhones.Free;
  inherited;
end;

initialization

//TControllersRegister.Instance.RegisterController(TMyController2, 'MyServerName');

end.
