unit RoutingSampleControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons;

type

  [MVCPath('/')]
  TRoutingSampleController = class(TMVCController)
  public
    [MVCPath('/')]
    procedure Index(CTX: TWebContext);

    [MVCHTTPMethod([httpGet])]
    [MVCPath('/search/($searchtext)/($page)')]
    [MVCProduces('text/plain', 'UTF-8')]
    [MVCConsumes('text/html')]
    procedure SearchCustomers(CTX: TWebContext);

    [MVCHTTPMethod([httpGet])]
    [MVCPath('/people/($id)')]
    [MVCProduces('application/json')]
    procedure GetPerson(const id: Integer);

  end;

implementation

uses
  System.SysUtils, BusinessObjectsU, Data.DBXJSON;

{ TRoutingSampleController }

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

procedure TRoutingSampleController.Index(CTX: TWebContext);
begin
  Render('This is the root path');
end;

procedure TRoutingSampleController.SearchCustomers(CTX: TWebContext);
var
  search: string;
  P: Integer;
  orderby: string;
begin
  search := CTX.Request.Params['searchtext'];
  P := CTX.Request.ParamsAsInteger['page'];
  orderby := '';
  if CTX.Request.QueryStringParamExists('order') then
    orderby := CTX.Request.QueryStringParam('order');
  Render(Format(
    'SEARCHTEXT: "%s"' + sLineBreak +
    'PAGE: %d' + sLineBreak +
    'ORDERBYFIELD: "%s"',
    [search, P, orderby]));
end;

end.
