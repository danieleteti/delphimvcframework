unit MainControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.Serializer.Commons, Entities;

type
  [MVCPath('/api/people')]
  TMyController = class(TMVCController)
  public
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    function GetPeople: IMVCObjectDictionary;

    [MVCPath('/($id)')]
    [MVCHTTPMethod([httpGET])]
    function GetPersonByID(id: Integer): TPerson;

    [MVCPath]
    [MVCHTTPMethod([httpPOST])]
    function Createperson([MVCFromBody] const Person: TPerson): IMVCResponse;

    [MVCPath('/($id)')]
    [MVCHTTPMethod([httpPUT])]
    function UpdatePerson(id: Integer; [MVCFromBody] const Person: TPerson): IMVCResponse;

    [MVCPath('/($id)')]
    [MVCHTTPMethod([httpDELETE])]
    function DeletePerson(id: Integer): IMVCResponse;
  end;


implementation

uses
  System.SysUtils, MVCFramework.Logger, System.StrUtils, MVCFramework.Cache,
  System.Rtti, MVCFramework.Rtti.Utils, MVCFramework.ActiveRecord;

function TMyController.GetPeople: IMVCObjectDictionary;
begin
  Result := ObjectDict().Add('people', TMVCActiveRecord.All<TPerson>);
end;

function TMyController.GetPersonByID(id: Integer): TPerson;
begin
  Result := TMVCActiveRecord.GetByPK<TPerson>(ID);
  SetETag(Result.GetUniqueString);
end;

function TMyController.Createperson([MVCFromBody] const Person: TPerson): IMVCResponse;
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
  Result := CreatedResponse();
end;

function TMyController.UpdatePerson(id: Integer; [MVCFromBody] const Person: TPerson): IMVCResponse;
var
  lPerson: TPerson;
begin
  // retrieve data from storage
  lPerson := TMVCActiveRecord.GetByPK<TPerson>(ID);
  try
    //check if the client modified the current version (a.k.a. mid-air collisions)
    //raises an exception if client send a wrong If-Match header value
    CheckIfMatch(lPerson.GetUniqueString);

    //perform the actual update and save to the storage
    lPerson.Assign(Person);
    lPerson.Update();

    //(optional) set the new ETag value base on the data status
    SetETag(lPerson.GetUniqueString);

    //reply with a 200 OK
    Result := OKResponse();
  finally
    lPerson.Free;
  end;
end;

function TMyController.DeletePerson(id: Integer): IMVCResponse;
var
  lPerson: TPerson;
begin
  lPerson := TMVCActiveRecord.GetByPK<TPerson>(ID);
  try
    CheckIfMatch(lPerson.GetUniqueString);
    lPerson.Delete();
    Result := NoContentResponse();
  except
    lPerson.Free;
    raise;
  end;
end;

end.
