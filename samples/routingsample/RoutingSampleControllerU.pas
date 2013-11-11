unit RoutingSampleControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, ObjectsMappers;

type

  [MVCPath('/')]
  TRoutingSampleController = class(TMVCController)
  public
    [MVCHTTPMethod([httpGet])]
    [MVCPath('/search/($searchtext)/($page)')]
    [MVCProduces('text/plain')]
    procedure SearchCustomers(CTX: TWebContext);

    [MVCHTTPMethod([httpGet])]
    [MVCPath('/people/($id)')]
    [MVCProduces('application/json')]
    procedure GetPerson(CTX: TWebContext);

  end;

implementation

uses
  System.SysUtils, BusinessObjectsU, Data.DBXJSON;

{ TRoutingSampleController }

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
  Render(Format('SEARCHTEXT: "%s", PAGE: %d, ORDERBYFIELD: "%s"', [search, P, orderby]));
end;

end.
