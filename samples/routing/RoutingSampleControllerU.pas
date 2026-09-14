unit RoutingSampleControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons,
  System.Generics.Collections, BusinessObjectsU;

type

  [MVCPath('/')]
  [MVCPath('/api')]
  TRoutingSampleController = class(TMVCController)
  public
    [MVCPath]
    procedure Index;

    { This action requires that the ACCEPT header is text/plain to be invocated.

      Note how the search criteria have to be squeezed into the URL: the text in
      a path segment, everything else in the query string. It works, until the
      criteria stop being flat - see the QUERY action below. }
    [MVCHTTPMethod([httpGet])]
    [MVCPath('/searches/($SearchText)')]
    [MVCProduces('text/plain', 'UTF-8')]
    function SearchCustomers(
      const SearchText: String;
      const [MVCFromQueryString('page', 1)] Page: Integer;
      const [MVCFromQueryString('order', '')] OrderBy: String): String;

    // The same search, using the QUERY method (RFC 10008).
    //
    // QUERY is the only HTTP method that is safe and idempotent *and* carries a
    // body: a read that is free to be retried or prefetched, like GET, but whose
    // criteria travel as JSON instead of as an ad-hoc URL encoding. Which is why
    // the parameter below is a real object with a list and a nested record in
    // it, and there is no parsing code in the action.
    //
    // Two things worth knowing, neither of them obvious:
    //
    //   - For CSRF purposes treat QUERY like POST, never like GET. The method is
    //     safe by specification, but it carries a body and a same-origin request
    //     is not preflighted.
    //   - Cross-origin callers need QUERY added to the allowed methods
    //     explicitly: the CORS default does not include it.
    //
    // A browser address bar cannot issue this one - see README.md for a curl
    // invocation and for the rest of the caveats.
    [MVCHTTPMethod([httpQUERY])]
    [MVCPath('/customers/searches')]
    [MVCConsumes(TMVCMediaType.APPLICATION_JSON)]
    [MVCProduces(TMVCMediaType.APPLICATION_JSON)]
    function SearchCustomersUsingQuery(
      const [MVCFromBody] Criteria: TCustomerSearch): TObjectList<TPerson>;

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
  System.SysUtils, JsonDataObjects, MVCFramework.Logger;

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

function TRoutingSampleController.SearchCustomersUsingQuery(
  const Criteria: TCustomerSearch): TObjectList<TPerson>;

  function NewPerson(const AFirstName, ALastName: String): TPerson;
  begin
    Result := TPerson.Create;
    Result.FirstName := AFirstName;
    Result.LastName := ALastName;
    Result.DOB := EncodeDate(1975, 5, 2);
  end;

begin
  { Criteria arrives already deserialized, nested object and list included -
    there is no parsing code here, which is the whole point. In a real service
    this is where it would become a WHERE clause. }
  LogI(Format('QUERY search: "%s" in [%s], price %d..%d, page %d ordered by "%s"',
    [Criteria.SearchText, String.Join(', ', Criteria.Cities),
     Criteria.PriceRange.Min, Criteria.PriceRange.Max,
     Criteria.Page, Criteria.OrderBy]));

  Result := TObjectList<TPerson>.Create(True);
  Result.Add(NewPerson('Daniele', 'Teti'));
  Result.Add(NewPerson('John', 'Doe'));
  Result.Add(NewPerson('Mark', 'Rossi'));

  { The framework frees the object returned by a functional action - do not
    wrap Result in ToFree<T>, that would be a double free. }
end;

function TRoutingSampleController.SearchCustomers(
  const SearchText: String;
  const Page: Integer;
  const OrderBy: String): String;
begin
  { The path segment binds by name to SearchText, the two query-string
    parameters bind through [MVCFromQueryString] with their defaults. There is
    nothing to read out of Context.Request here, and nothing to convert. }
  Result :=
    Format('SEARCHTEXT: "%s" - PAGE: %d - ORDER BY FIELD: "%s"',
      [SearchText, Page, OrderBy]) + sLineBreak +
    StringOfChar('*', 30) + sLineBreak +
    '1. Daniele Teti' + sLineBreak +
    '2. John Doe' + sLineBreak +
    '3. Mark Rossi' + sLineBreak +
    '4. Jack Verdi' + sLineBreak +
    StringOfChar('*', 30);
end;

end.
