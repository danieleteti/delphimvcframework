unit MainControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.Serializer.Commons,
  System.Generics.Collections, Services.InterfacesU,
  Entities, MVCFramework.Container;

type
  [MVCPath('/api')]
  TMyController = class(TMVCController)
  private
    fPeopleService: IPeopleService;
  public
    [MVCInject]
    constructor Create(const PeopleService: IPeopleService); reintroduce;

    [MVCPath('/people')]
    [MVCHTTPMethod([httpGET])]
    function GetPeople: TObjectList<TPerson>;

    [MVCPath('/people2')]
    [MVCHTTPMethod([httpGET])]
    function GetPeople2([MVCInject] OtherPeopleService: IPeopleService): TObjectList<TPerson>;

    [MVCPath('/people/($ID)')]
    [MVCHTTPMethod([httpGET])]
    function GetPerson(ID: Integer): TPerson;

    [MVCPath('/people')]
    [MVCHTTPMethod([httpPOST])]
    function CreatePerson([MVCInject] PeopleService: IPeopleService; [MVCFromBody] Person: TPerson): IMVCResponse;

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

function TMyController.GetPeople2(OtherPeopleService: IPeopleService): TObjectList<TPerson>;
begin
  LogI('PeopleService in GetPeople2: ' + IntToHex(NativeUInt(Pointer(OtherPeopleService))));
  Result := OtherPeopleService.GetAll;
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
  LogI('PeopleService in constructor: ' + IntToHex(NativeUInt(Pointer(PeopleService))));
end;

function TMyController.CreatePerson(PeopleService: IPeopleService; Person: TPerson): IMVCResponse;
begin
  LogI('Created ' + Person.FirstName + ' ' + Person.LastName);
  Result := CreatedResponse('', 'Person created (' + Person.ToString + ')' );
end;

function TMyController.UpdatePerson(ID: Integer; [MVCFromBody] Person: TPerson): IMVCResponse;
begin
  LogI('Updated ' + Person.FirstName + ' ' + Person.LastName);
  Result := NoContentResponse;
end;

function TMyController.DeletePerson(ID: Integer): IMVCResponse;
begin
  LogI('Deleted person with id ' + ID.ToString);
  Result := NoContentResponse;
end;


end.
