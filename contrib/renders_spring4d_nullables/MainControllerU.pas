unit MainControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons;

type

  [MVCPath('/api')]
  TMyController = class(TMVCController)
  public
    [MVCPath('/')]
    [MVCHTTPMethod([httpGET])]
    procedure Index;
  public
    // Sample CRUD Actions for a "Customer" entity
    [MVCPath('/dictionary')]
    [MVCHTTPMethod([httpGET])]
    procedure GetDictionary;

    [MVCPath('/people')]
    [MVCHTTPMethod([httpPOST])]
    procedure CreatePerson;

    [MVCPath('/people/($id)')]
    [MVCHTTPMethod([httpPUT])]
    procedure UpdatePerson(id: Integer);

    [MVCPath('/people/($id)')]
    [MVCHTTPMethod([httpGET])]
    procedure GetPerson(id: Integer);

    [MVCPath('/people/($id)')]
    [MVCHTTPMethod([httpDELETE])]
    procedure DeletePerson(id: Integer);

    [MVCPath('/parent/($id)')]
    [MVCHTTPMethod([httpGET])]
    procedure GetParent(id: Integer);

  end;

implementation

uses
  System.SysUtils,
  MVCFramework.Logger,
  System.StrUtils,
  BusinessObjectsU,
  Spring;

procedure TMyController.Index;
begin
  // use Context property to access to the HTTP request and response
  Render('Hello DelphiMVCFramework World');
end;

// Sample CRUD Actions for a "Customer" entity
procedure TMyController.GetDictionary;
var
  lDict: TMVCStringDictionary;
begin
  lDict := TMVCStringDictionary.Create;
  lDict.Add('prop1', 'one');
  lDict.Add('prop2', 'two');
  lDict.Add('prop3', 'three');
  Render(lDict);
end;

procedure TMyController.GetParent(id: Integer);
var
  lParent: TParent;
begin
  lParent := TParent.Create;
  try
    lParent.FullName := 'Hello World';
    lParent.Person.id := id;
    lParent.Person.FirstName := 'Daniele';
    lParent.Person.LastName := 'Teti';
    lParent.Person.MiddleName := nil;
    Render(lParent, False);
  finally
    lParent.Free;
  end;
end;

procedure TMyController.GetPerson(id: Integer);
var
  lPerson: TPerson;
begin
  lPerson := TPerson.Create;
  try
    lPerson.id := id;
    lPerson.FirstName := 'Daniele';
    lPerson.LastName := 'Teti';
    lPerson.MiddleName := nil;
    Render(lPerson, False);
  finally
    lPerson.Free;
  end;
end;

procedure TMyController.CreatePerson;
var
  lPerson: TPerson;
begin
  lPerson := Context.Request.BodyAs<TPerson>;
  try
    Render(lPerson, False);
  finally
    lPerson.Free;
  end;
end;

procedure TMyController.UpdatePerson(id: Integer);
var
  lPerson: TPerson;
begin
  lPerson := Context.Request.BodyAs<TPerson>;
  try
    lPerson.id := id;
    lPerson.FirstName := lPerson.FirstName + ' (updated)';
    lPerson.LastName := lPerson.LastName + ' (updated)';
    Render(lPerson, False);
  finally
    lPerson.Free;
  end;
end;

procedure TMyController.DeletePerson(id: Integer);
begin
  Render(http_status.OK, 'Deleted');
end;

end.
