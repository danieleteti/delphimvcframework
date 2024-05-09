unit ControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.Nullables, MVCFramework.Serializer.Commons, System.Generics.Collections,
  ServicesU, EntityU;

type
  [MVCPath('/api')]
  TMyController = class(TMVCController)
  public
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    function Index: String;

    [MVCPath('/reversedstrings/($Value)')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_PLAIN)]
    function GetReversedString(const Value: String): String;

    //Sample CRUD Actions for a "People" entity
    [MVCPath('/people')]
    [MVCHTTPMethod([httpGET])]
    function GetPeople([MVCInject] PeopleService: IPeopleService): IMVCResponse;

    [MVCPath('/people/($ID:sqid)')]
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
  System.StrUtils, System.SysUtils, MVCFramework.Logger;


function TMyController.Index: String;
begin
  //use Context property to access to the HTTP request and response
  Result := 'Hello DelphiMVCFramework World';
end;

function TMyController.GetReversedString(const Value: String): String;
begin
  Result := System.StrUtils.ReverseString(Value.Trim);
end;

//Sample CRUD Actions for a "People" entity (with service injection)
function TMyController.GetPeople(PeopleService: IPeopleService): IMVCResponse;
begin
  Result := OkResponse(PeopleService.GetAll);
end;

function TMyController.GetPerson(ID: Integer): TPerson;
begin
  Result := TPerson.Create(ID, 'Daniele', 'Teti', EncodeDate(1979, 11, 4));
end;

function TMyController.CreatePerson([MVCFromBody] Person: TPerson): IMVCResponse;
begin
  LogI('Created ' + Person.FirstName + ' ' + Person.LastName);
  Result := CreatedResponse('', 'Person created');
end;

function TMyController.UpdatePerson(ID: Integer; [MVCFromBody] Person: TPerson): IMVCResponse;
begin
  LogI('Updated ' + Person.FirstName + ' ' + Person.LastName);
  Result := NoContentResponse();
end;

function TMyController.DeletePerson(ID: Integer): IMVCResponse;
begin
  LogI('Deleted person with id ' + ID.ToString);
  Result := NoContentResponse();
end;

end.
