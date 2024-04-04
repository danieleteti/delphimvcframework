unit CustomersControllerU;

interface

uses
  MVCFramework,
  MVCFramework.ActiveRecord,
  MVCFramework.Commons,
  System.Generics.Collections,
  EntitiesU;

type

  [MVCPath('/api/customers')]
  TCustomersController = class(TMVCController)
  public
    [MVCPath]
    [MVCHTTPMethods([httpGET])]
    function GetCustomers([MVCFromQueryString('rql','')] RQLFilter: String): IMVCResponse;

    [MVCPath('/($ID)')]
    [MVCHTTPMethods([httpGET])]
    function GetCustomerByID(const ID: Integer): IMVCResponse;

    [MVCPath('/($ID)')]
    [MVCHTTPMethods([httpPUT])]
    function UpdateCustomerByID(const [MVCFromBody] Customer: TCustomer; const ID: Integer): IMVCResponse;

    [MVCPath]
    [MVCHTTPMethods([httpPOST])]
    function CreateCustomer([MVCFromBody] const Customer: TCustomer): IMVCResponse;

    [MVCPath('/_bulk')]
    [MVCHTTPMethods([httpPOST])]
    function BulkCreateCustomers([MVCFromBody] const Customers: TObjectList<TCustomer>): IMVCResponse;
  end;

implementation

uses
  System.SysUtils;

{ TCustomersController }

function TCustomersController.CreateCustomer(const Customer: TCustomer): IMVCResponse;
begin
  Customer.Insert;
  Result := CreatedResponse('/api/customers/' + Customer.ID.Value.ToString);
end;

function TCustomersController.GetCustomerByID(const ID: Integer): IMVCResponse;
begin
  Result := OKResponse(TMVCActiveRecord.GetByPK<TCustomer>(ID));
end;

function TCustomersController.GetCustomers([MVCFromQueryString('rql','')] RQLFilter: String): IMVCResponse;
begin
  Result := OKResponse(TMVCActiveRecord.SelectRQL<TCustomer>(RQLFilter, 1000));
end;

function TCustomersController.UpdateCustomerByID(const Customer: TCustomer; const ID: Integer): IMVCResponse;
begin
  Customer.ID := ID;
  Customer.Update();
  Result := OKResponse;
end;

function TCustomersController.BulkCreateCustomers(const Customers: TObjectList<TCustomer>): IMVCResponse;
begin
  begin var lCtx := TMVCActiveRecord.UseTransactionContext;
    for var lCustomer in Customers do
    begin
      lCustomer.Insert;
    end;
  end;
  Result := CreatedResponse();
end;

end.
