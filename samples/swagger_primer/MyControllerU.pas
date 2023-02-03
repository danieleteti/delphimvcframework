unit MyControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.DataSet.Utils, MVCFramework.Swagger.Commons, EntitiesU;

type
  [MVCPath('/api')]
  TMyController = class(TMVCController)
  public
    [MVCSwagSummary('Customers', 'Get all customers', 'getCustomers')]
    [MVCSwagResponses(200, 'Customers', TCustomer, True)]
    [MVCPath('/customers')]
    [MVCHTTPMethod([httpGET])]
    procedure GetCustomers;

    [MVCSwagSummary('Customers', 'Get a customer', 'getCustomerById')]
    [MVCSwagParam(plPath, 'id', 'Customer ID', ptInteger, True)]
    [MVCSwagResponses(200, 'Customer', TCustomer)]
    [MVCSwagResponses(404, 'Customer not found', TMVCErrorResponse)]
    [MVCPath('/customers/($id)')]
    [MVCHTTPMethod([httpGET])]
    procedure GetCustomer(id: Integer);

    [MVCSwagSummary('Customers', 'Create a customer', 'createCustomers')]
    [MVCSwagParam(plBody, 'Customer', 'Customer JSON Object', TCustomer)]
    [MVCSwagResponses(201, 'Customer created')]
    [MVCSwagResponses(HTTP_STATUS.BadRequest, 'Invalid request', TMVCErrorResponse)]
    [MVCPath('/customers')]
    [MVCHTTPMethod([httpPOST])]
    procedure CreateCustomer;

    [MVCSwagSummary('Customers', 'Update a customer', 'updateCustomer')]
    [MVCSwagParam(plBody, 'Customer', 'Customer JSON Object', TCustomer, ptNotDefined, True)]
    [MVCSwagParam(plPath, 'id', 'Customer ID to update', ptInteger, True)]
    [MVCSwagResponses(200, 'Customer updated')]
    [MVCSwagResponses(HTTP_STATUS.BadRequest, 'Invalid request', TMVCErrorResponse)]
    [MVCPath('/customers/($id)')]
    [MVCHTTPMethod([httpPUT])]
    procedure UpdateCustomer(id: Integer);

    [MVCSwagSummary('Customers', 'Delete a customer', 'deleteCustomer')]
    [MVCPath('/customers/($id)')]
    [MVCHTTPMethod([httpDELETE])]
    procedure DeleteCustomer(id: Integer);

  end;

implementation

uses
  System.SysUtils, MVCFramework.Logger, System.StrUtils, JsonDataObjects;

procedure TMyController.GetCustomers;
begin
  Render<TCustomer>(EntitiesU.GetCustomers);
end;

procedure TMyController.GetCustomer(id: Integer);
begin
  if id = 42 then
  begin
    raise EMVCException.Create(HTTP_STATUS.NotFound, 'Customer not found');
  end;
  Render(EntitiesU.GetCustomer(id));
end;

procedure TMyController.CreateCustomer;
var
  lCustomer: TCustomer;
begin
  lCustomer := Self.Context.Request.BodyAs<TCustomer>;
  try
    if not lCustomer.IsValid then
    begin
      raise EMVCException.Create(HTTP_STATUS.BadRequest, 'Customer not valid');
    end;
    // do something smart with lCustomer...
    Render201Created();
  finally
    lCustomer.Free;
  end;
end;

procedure TMyController.UpdateCustomer(id: Integer);
var
  lCustomer: TCustomer;
begin
  lCustomer := Self.Context.Request.BodyAs<TCustomer>;
  try
    lCustomer.ID := id; //dont be confident of the user!
    if not lCustomer.IsValid then
    begin
      raise EMVCException.Create(HTTP_STATUS.BadRequest, 'Customer not valid');
    end;
    // do something smart with lCustomer...
    Render(lCustomer, False);
  finally
    lCustomer.Free;
  end;
end;

procedure TMyController.DeleteCustomer(id: Integer);
begin
  ResponseStatus(200);
end;

end.
