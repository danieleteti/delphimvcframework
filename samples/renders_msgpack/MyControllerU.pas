unit MyControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.Serializer.Commons, System.Generics.Collections;

type
  [MVCNameCase(ncCamelCase)]
  TPerson = class
  private
    fFirstName: String;
    fLastName: String;
    fDOB: TDate;
  public
    property FirstName: String read fFirstName write fFirstName;
    property LastName: String read fLastName write fLastName;
    property DOB: TDate read fDOB write fDOB;  
    constructor Create(FirstName, LastName: String; DOB: TDate);
  end;

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
  protected
    procedure OnBeforeAction(Context: TWebContext; const AActionName: string; var Handled: Boolean); override;
    procedure OnAfterAction(Context: TWebContext; const AActionName: string); override;

  public
    //Sample CRUD Actions for a "People" entity
    [MVCPath('/people')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(TMVCMediaType.APPLICATION_MESSAGE_PACK)]
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

function TMyController.Index: String;
begin
  //use Context property to access to the HTTP request and response 
  Result := 'Hello DelphiMVCFramework World';
end;

function TMyController.GetReversedString(const Value: String): String;
begin
  Result := System.StrUtils.ReverseString(Value.Trim);
end;

procedure TMyController.OnAfterAction(Context: TWebContext; const AActionName: string); 
begin
  { Executed after each action }
  inherited;
end;

procedure TMyController.OnBeforeAction(Context: TWebContext; const AActionName: string; var Handled: Boolean);
begin
  { Executed before each action
    if handled is true (or an exception is raised) the actual
    action will not be called }
  inherited;
end;

//Sample CRUD Actions for a "People" entity
function TMyController.GetPeople: TObjectList<TPerson>;
var
  lPeople: TObjectList<TPerson>;
begin
  lPeople := TObjectList<TPerson>.Create(True);
  try
    lPeople.Add(TPerson.Create('Peter','Parker', EncodeDate(1965, 10, 4)));
    lPeople.Add(TPerson.Create('Bruce','Banner', EncodeDate(1945, 9, 6)));
    lPeople.Add(TPerson.Create('Reed','Richards', EncodeDate(1955, 3, 7)));
    Result := lPeople;
  except
    lPeople.Free;
    raise;
  end;
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



constructor TPerson.Create(FirstName, LastName: String; DOB: TDate);
begin
  inherited Create;
  fFirstName := FirstName;
  fLastName := LastName;
  fDOB := DOB;
end;

end.
