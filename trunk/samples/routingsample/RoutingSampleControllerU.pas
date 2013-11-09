unit RoutingSampleControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons;

type

  [MVCPath('/')]
  TRoutingSampleController = class(TMVCController)
  public
    [MVCHTTPMethod([httpGet])]
    [MVCPath('/($searchtext)/($page)')]
    [MVCProduce('text/plain')]
    procedure SearchCustomers(CTX: TWebContext);
  end;

implementation

uses
  System.SysUtils;

{ TRoutingSampleController }

procedure TRoutingSampleController.SearchCustomers(CTX: TWebContext);
var
  search: string;
  p: Integer;
  orderby: string;
begin
  search := CTX.Request.Params['searchtext'];
  p := CTX.Request.ParamsAsInteger['page'];
  orderby := '';
  if CTX.Request.QueryStringParamExists('order') then
    orderby := CTX.Request.QueryStringParam('order');
  Render(Format('SEARCHTEXT: "%s", PAGE: %d, ORDERBYFIELD: "%s"', [search, p, orderby]));
end;

end.
