unit RoutingSampleControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, ObjectsMappers;

type

  [MVCPath('/')]
  TRoutingSampleController = class(TMVCController)
  public
    [MVCPath('/')]
    procedure Index(CTX: TWebContext);

    [MVCHTTPMethod([httpGet])]
    [MVCPath('/searches/($searchtext)')]
    [MVCProduces('text/plain', 'UTF-8')]
    procedure SearchCustomers(CTX: TWebContext);

    [MVCHTTPMethod([httpGet])]
    [MVCPath('/people/($id)')]
    { double MVCPath }
    [MVCPath('/($id)')]
    [MVCProduces('application/json')]
    procedure GetPerson(CTX: TWebContext);

    [MVCHTTPMethod([httpDelete])]
    [MVCPath('/people/($id)')]
    procedure DeletePerson(CTX: TWebContext);

  end;

implementation

uses
  System.SysUtils, BusinessObjectsU, Data.DBXJSON;

{ TRoutingSampleController }

procedure TRoutingSampleController.DeletePerson(CTX: TWebContext);
var
  IDPerson: Integer;
begin
  IDPerson := CTX.Request.ParamsAsInteger['id'];
  // RemovePerson(IDPerson)
  Render(204 { 'No content' } , 'Person deleted');
end;

procedure TRoutingSampleController.GetPerson(CTX: TWebContext);
var
  P: TPerson;
  IDPerson: Integer;
begin
  IDPerson := CTX.Request.ParamsAsInteger['id'];
  {
    Use IDPerson to load the person from a database...
    In this example, we're creating a fake person
  }
  P := TPerson.Create;
  P.FirstName := 'Daniele';
  P.LastName := 'Teti';
  P.DOB := EncodeDate(1975, 5, 2);
  P.Married := True;
  Render(P);
end;

procedure TRoutingSampleController.Index(CTX: TWebContext);
begin
  Render('This is the root path');
end;

procedure TRoutingSampleController.SearchCustomers(CTX: TWebContext);
var
  search: string;
  Page: Integer;
  orderby: string;
  S: string;
begin
  search := CTX.Request.Params['searchtext'];
  Page := 1;
  if CTX.Request.QueryStringParamExists('page') then
    Page := CTX.Request.QueryStringParam('page').ToInteger;
  orderby := '';
  if CTX.Request.QueryStringParamExists('order') then
    orderby := CTX.Request.QueryStringParam('order');
  S := Format(
    'SEARCHTEXT: "%s" - PAGE: %d - ORDER BY FIELD: "%s"',
    [search, Page, orderby]);
  ResponseStream
    .AppendLine(S)
    .AppendLine(StringOfChar('*', 30))
    .AppendLine('1. Daniele Teti')
    .AppendLine('2. John Doe')
    .AppendLine('3. Mark Rossi')
    .AppendLine('4. Jack Verdi')
    .AppendLine(StringOfChar('*', 30));
  Render;
end;

end.
