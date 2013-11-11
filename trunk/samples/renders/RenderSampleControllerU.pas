unit RenderSampleControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, ObjectsMappers;

type

  [MVCPath('/')]
  TRenderSampleController = class(TMVCController)
  public
    [MVCHTTPMethod([httpGet])]
    [MVCPath('/customers')]
    [MVCProduces('application/json')]
    procedure GetCustomers(CTX: TWebContext);

    [MVCHTTPMethod([httpGet])]
    [MVCPath('/customers/($id)')]
    [MVCProduces('application/json')]
    procedure GetCustomerByID(CTX: TWebContext);

  end;

implementation

uses
  System.SysUtils, BusinessObjectsU, Data.DBXJSON, WebModuleU;

{ TRoutingSampleController }

procedure TRenderSampleController.GetCustomerByID(CTX: TWebContext);
var
  Cust: TCustomer;
begin
  if CTX.Request.ParamsAsInteger['id'] = 7 then
    Render(404, 'Customer Not Found')
  else
  begin
    Cust := TCustomer.Create;
    Cust.Name := 'Daniele Teti Inc.';
    Cust.ContactFirst := 'Daniele';
    Cust.ContactLast := 'Teti';
    Cust.AddressLine1 := 'Rome Street 12';
    Cust.AddressLine2 := '00100';
    Cust.City := 'ROME';
    Render(Cust);
  end;
end;

procedure TRenderSampleController.GetCustomers(CTX: TWebContext);
var
  wm: TWebModule1;
begin
  wm := GetCurrentWebModule as TWebModule1;
  wm.qryCustomers.Open;
  Render(wm.qryCustomers);
end;

end.
