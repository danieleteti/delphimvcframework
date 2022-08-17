unit MainControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.Serializer.Commons;

type
  [MVCNameCase(ncCamelCase)]
  TPerson = class
  private
    fName: String;
    fSurname: String;
    fID: Integer;
  public
    function GetHash: String;
    class function GetNew(const id: Integer; const Name, Surname: String): TPerson;
    property ID: Integer read fID write fID;
    property Name: String read fName write fName;
    property Surname: String read fSurname write fSurname;
  end;


  [MVCPath('/api/people')]
  TMyController = class(TMVCController)
  private
    function GetPersonByID(const ID: Integer): TPerson;
    procedure UpdatePersonByID(const ID: Integer; const Person: TPerson);
  public
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    procedure Index;
  public
    // Sample CRUD Actions for a "person" entity
    [MVCPath('/($id)')]
    [MVCHTTPMethod([httpGET])]
    procedure Getperson(id: Integer);

    [MVCPath]
    [MVCHTTPMethod([httpPOST])]
    procedure Createperson([MVCFromBody] const Person: TPerson);

    [MVCPath('/($id)')]
    [MVCHTTPMethod([httpPUT])]
    procedure UpdatePerson(id: Integer; [MVCFromBody] const Person: TPerson);

    [MVCPath('/($id)')]
    [MVCHTTPMethod([httpDELETE])]
    procedure DeletePerson(id: Integer);

  end;


implementation

uses
  System.SysUtils, MVCFramework.Logger, System.StrUtils, MVCFramework.Cache,
  System.Rtti, MVCFramework.Rtti.Utils;

procedure TMyController.Index;
begin
  // use Context property to access to the HTTP request and response
  Render('Hello DelphiMVCFramework World');
end;

function TMyController.GetPersonByID(const ID: Integer): TPerson;
var
  lPerson: TPerson;
begin
  lPerson := nil;
  if not TMVCCacheSingleton.Instance.ExecOnItemWithWriteLock(id.ToString,
    procedure(Value: TValue)
    begin
      lPerson := TRttiUtils.Clone(Value.AsObject) as TPerson;
    end) then
  begin
    raise EMVCException.Create(HTTP_STATUS.NotFound, 'Person not found');
  end;
  Result := lPerson;
end;

procedure TMyController.Getperson(id: Integer);
var
  lItem: TMVCCacheItem;
  lPerson: TPerson;
begin
  lPerson := GetPersonByID(id);
  SetETag(lPerson.GetHash);
  Render(lPerson, True);
end;

procedure TMyController.Createperson([MVCFromBody] const Person: TPerson);
var
  lValue: TValue;
begin
  TMVCCacheSingleton.Instance.BeginWrite;
  try
    if not TMVCCacheSingleton.Instance.Contains(Person.ID.ToString, lValue) then
    begin
      TMVCCacheSingleton.Instance.SetValue(Person.ID.ToString, TRttiUtils.Clone(Person));
    end
    else
    begin
      raise EMVCException.Create(HTTP_STATUS.NotAcceptable, 'Duplicate ID for person');
    end;
  finally
    TMVCCacheSingleton.Instance.EndWrite;
  end;
  Render201Created();
end;

procedure TMyController.UpdatePerson(id: Integer; [MVCFromBody] const Person: TPerson);
var
  lItem: TMVCCacheItem;
  lPerson: TPerson;
begin
  // retrieve data from storage
  lPerson := GetPersonByID(id);

  //check if the client modified the current version (a.k.a. mid-air collisions)
  //raises an exception if client send a wrong If-Match header value
  CheckIfMatch(lPerson.GetHash);

  //perform the actual update and save to the storage
  lPerson.Name := Person.Name;
  lPerson.Surname := Person.Surname;
  UpdatePersonByID(lPerson.ID, lPerson);

  //set the new ETag value base on the data status
  SetETag(lPerson.GetHash);

  //reply with a 200 OK
  Render(HTTP_STATUS.OK);
end;

procedure TMyController.UpdatePersonByID(const ID: Integer;
  const Person: TPerson);
begin
  TMVCCacheSingleton.Instance.SetValue(ID.ToString, Person);
end;

procedure TMyController.DeletePerson(id: Integer);
var
  lPerson: TPerson;
begin
  lPerson := GetPersonByID(ID);
  CheckIfMatch(lPerson.GetHash);
  TMVCCacheSingleton.Instance.RemoveItem(ID.ToString);
  Render204NoContent();
end;

{ TPerson }

function TPerson.GetHash: String;
begin
  Result := Format('%d|%s|%s', [fID, fName, fSurname]);
end;

class function TPerson.GetNew(const id: Integer; const Name, Surname: String): TPerson;
begin
  Result := TPerson.Create;
  Result.fID := id;
  Result.fName := Name;
  Result.fSurname := Surname;
end;

end.
