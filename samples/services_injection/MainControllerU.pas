unit MainControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.Serializer.Commons, System.Generics.Collections, Services.InterfacesU,
  Entities;

type
  [MVCPath('/api')]
  TMyController = class(TMVCController)
  private
    fPeopleService: IPeopleService;
  public
    [MVCInject]
    constructor Create(const PeopleService: IPeopleService); reintroduce;
//    constructor Create; reintroduce;
    //Sample CRUD Actions for a "People" entity
    [MVCPath('/people')]
    [MVCHTTPMethod([httpGET])]
    function GetPeople: TObjectList<TPerson>;

    [MVCPath('/people/($ID)')]
    [MVCHTTPMethod([httpGET])]
    function GetPerson(ID: Integer): TPerson;

    [MVCPath('/people')]
    [MVCHTTPMethod([httpPOST])]
    function CreatePerson([MVCFromBody] Person: TPerson): IMVCResponse;

    [MVCPath('/people/($ID)')]
    [MVCHTTPMethod([httpPUT])]
    function UpdatePerson(ID: Integer; [MVCFromBody] Person: TPerson): IMVCResponse;

    [MVCPath('/people/($ID)')]
    [MVCHTTPMethod([httpDELETE])]
    function DeletePerson(ID: Integer): IMVCResponse;

  end;

implementation

uses
  System.SysUtils, MVCFramework.Logger, System.StrUtils;



//Sample CRUD Actions for a "People" entity
function TMyController.GetPeople: TObjectList<TPerson>;
begin
  Result := fPeopleService.GetAll;
end;

function TMyController.GetPerson(ID: Integer): TPerson;
var
  lPeople: TObjectList<TPerson>;
begin
  lPeople := GetPeople;
  try
    Result := lPeople.ExtractAt(ID mod lPeople.Count);
  finally
    lPeople.Free;
  end;
end;

//constructor TMyController.Create;
//begin
//  inherited Create;
//end;

constructor TMyController.Create(const PeopleService: IPeopleService);
begin
  inherited Create;
  Assert(PeopleService <> nil, 'PeopleService not injected');
  fPeopleService := PeopleService;
end;


function TMyController.CreatePerson([MVCFromBody] Person: TPerson): IMVCResponse;
begin
  LogI('Created ' + Person.FirstName + ' ' + Person.LastName);
  Result := MVCResponseBuilder
      .StatusCode(HTTP_STATUS.Created)
      .Body('Person created')
      .Build;
end;

function TMyController.UpdatePerson(ID: Integer; [MVCFromBody] Person: TPerson): IMVCResponse;
begin
  LogI('Updated ' + Person.FirstName + ' ' + Person.LastName);
  Result := MVCResponseBuilder
    .StatusCode(HTTP_STATUS.NoContent)
    .Build;
end;

function TMyController.DeletePerson(ID: Integer): IMVCResponse;
begin
  LogI('Deleted person with id ' + ID.ToString);
  Result := MVCResponseBuilder
    .StatusCode(HTTP_STATUS.NoContent)
    .Build;
end;


end.
