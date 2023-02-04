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
    procedure GetPeople;


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
  System.Rtti, MVCFramework.Rtti.Utils, MVCFramework.ActiveRecord;

procedure TMyController.GetPeople;
begin
  Render(ObjectDict().Add('people', TMVCActiveRecord.All<TPerson>));
end;

procedure TMyController.Getperson(id: Integer);
var
  lPerson: TPerson;
begin
  lPerson := TMVCActiveRecord.GetByPK<TPerson>(ID);
  SetETag(lPerson.GetUniqueString);
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
    RenderStatusMessage(HTTP_STATUS.OK);
  finally
    lPerson.Free;
  end;
end;


procedure TMyController.DeletePerson(id: Integer);
var
  lPerson: TPerson;
begin
  lPerson := TMVCActiveRecord.GetByPK<TPerson>(ID);
  try
    CheckIfMatch(lPerson.GetUniqueString);
    lPerson.Delete();
    Render204NoContent();
  except
    lPerson.Free;
    raise;
  end;
end;

end.
