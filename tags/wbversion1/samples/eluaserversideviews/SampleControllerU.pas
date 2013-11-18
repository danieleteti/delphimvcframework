unit SampleControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons;

type

  [MVCPath('/')]
  TSampleController = class(TMVCController)
  public
    [MVCHTTPMethod([httpGet])]
    [MVCPath('/($searchtext)/($page)')]
    [MVCProduces('text/plain')]
    procedure SearchCustomers(CTX: TWebContext);

    [MVCHTTPMethod([httpPost])]
    [MVCPath('/customers')]
    [MVCProduces('text/plain')]
    procedure CreateCustomer(CTX: TWebContext);

  end;

implementation

uses
  System.SysUtils;

{ TRoutingSampleController }

procedure TSampleController.CreateCustomer(CTX: TWebContext);
begin
  ResponseStream.Append('First name: ' + CTX.Request.Params['first_name'] +
    sLineBreak + 'Last name: ' + CTX.Request.Params['last_name']);
  Render;
end;

procedure TSampleController.SearchCustomers(CTX: TWebContext);
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
  Render(Format('SEARCHTEXT: "%s", PAGE: %d, ORDERBYFIELD: "%s"',
    [search, p, orderby]));
end;

end.
