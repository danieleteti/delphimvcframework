unit RoutingSampleControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons;

type

  [MVCPath('/')]
  [MVCPath('/api')]
  TRoutingSampleController = class(TMVCController)
  public
    [MVCPath]
    procedure Index;

    { This action requires that the ACCEPT header is text/plain to be invocated }
    [MVCHTTPMethod([httpGet])]
    [MVCPath('/searches/($searchtext)')]
    [MVCProduces('text/plain', 'UTF-8')]
    procedure SearchCustomers;

    { This action requires that the ACCEPT header is application/json to be invocated }
    [MVCHTTPMethod([httpGet])]
    [MVCPath('/people/($id)')]
    { double MVCPath }
    [MVCPath('/($id)')]
    [MVCProduces('application/json')]
    procedure GetPerson(const id: Integer);

    [MVCHTTPMethod([httpDelete])]
    [MVCPath('/people/($id)')]
    procedure DeletePerson(const id: Integer);

    { To be invocated this action requires that:
      - the CONTENT-TYPE header is application/json and
      - that the ACCEPT header is application/json
    }
    [MVCHTTPMethod([httpPOST])]
    [MVCPath('/people')]
    [MVCProduces(TMVCMediaType.APPLICATION_JSON)]
    [MVCConsumes(TMVCMediaType.APPLICATION_JSON)]
    procedure CreatePerson;

    { To be invocated this action requires that:
      - the CONTENT-TYPE header is application/json and
      - that the ACCEPT header is application/json
    }
    [MVCHTTPMethod([httpPOST])]
    [MVCPath('/people2')]
    [MVCProduces(TMVCMediaType.APPLICATION_JSON)]
    [MVCConsumes(TMVCMediaType.APPLICATION_JSON)]
    procedure CreatePerson2;

  end;

implementation

uses
  System.SysUtils, BusinessObjectsU, JsonDataObjects;

{ TRoutingSampleController }

procedure TRoutingSampleController.CreatePerson;
var
  lPerson: TPerson;
begin
  lPerson := Context.Request.BodyAs<TPerson>;
  try
    lPerson.Validate;
    // SavePerson(lPerson);
  finally
    lPerson.Free;
  end;
  Render(HTTP_STATUS.Created, 'Person created');
end;

procedure TRoutingSampleController.CreatePerson2;
var
  lJPerson: TJSONObject;
begin
  lJPerson := StrToJSONObject(Context.Request.Body);
  try
    // SavePerson(lJPerson);
  finally
    lJPerson.Free;
  end;
  Render(HTTP_STATUS.Created, 'Person created JSON');
end;

procedure TRoutingSampleController.DeletePerson(const id: Integer);
begin
  { Here you should do something with id }
  // RemovePerson(ID)

  Render(HTTP_STATUS.NoContent { 'No content' } , 'Person deleted');

  // Render204NoContent(); { Using the response shortcut methods }
end;

procedure TRoutingSampleController.GetPerson(const id: Integer);
var
  P: TPerson;
begin
  {
    Use ID to load the person from a database...
    In this example, we're creating a fake person
  }
  P := TPerson.Create;
  P.FirstName := 'Daniele';
  P.LastName := 'Teti';
  P.DOB := EncodeDate(1975, 5, 2);
  P.Married := True;
  Render(P);
end;

procedure TRoutingSampleController.Index;
begin
  Render('This is the root path');
end;

procedure TRoutingSampleController.SearchCustomers;
var
  search: string;
  Page: Integer;
  orderby: string;
  S: string;
begin
  search := Context.Request.Params['searchtext'];
  Page := 1;
  if Context.Request.QueryStringParamExists('page') then
    Page := StrToInt(Context.Request.QueryStringParam('page'));
  orderby := '';
  if Context.Request.QueryStringParamExists('order') then
    orderby := Context.Request.QueryStringParam('order');
  S := Format('SEARCHTEXT: "%s" - PAGE: %d - ORDER BY FIELD: "%s"', [search, Page, orderby]);
  ResponseStream
    .AppendLine(S)
    .AppendLine(StringOfChar('*', 30))
    .AppendLine('1. Daniele Teti')
    .AppendLine('2. John Doe')
    .AppendLine('3. Mark Rossi')
    .AppendLine('4. Jack Verdi')
    .AppendLine(StringOfChar('*', 30));
  RenderResponseStream;
end;

end.
