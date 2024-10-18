unit CustomersControllerU;

interface

uses
  MVCFramework,
  MVCFramework.ActiveRecord,
  MVCFramework.Commons,
  System.Generics.Collections,
  EntitiesU,
  CustomerServiceU;

type

  [MVCPath('/api/customers')]
  TCustomersController = class(TMVCController)
  public
    [MVCPath]
    [MVCHTTPMethods([httpGET])]
    function GetCustomers(
      [MVCFromQueryString('rql','')] RQLFilter: String;
      [MVCInject] CustomersService: ICustomersService): IMVCResponse;

    [MVCPath('/($ID:sqids)')]
    [MVCHTTPMethods([httpGET])]
    function GetCustomerByID(
      const ID: Integer;
      [MVCInject] CustomersService: ICustomersService): IMVCResponse;

    [MVCPath('/($ID:sqids)')]
    [MVCHTTPMethods([httpPUT])]
    function UpdateCustomerByID(
      const ID: Integer;
      [MVCFromBody] const Customer: TCustomer;
      [MVCInject] CustomersService: ICustomersService): IMVCResponse;

    [MVCPath]
    [MVCHTTPMethods([httpPOST])]
    function CreateCustomer(
      [MVCFromBody] const Customer: TCustomer;
      [MVCInject] CustomersService: ICustomersService): IMVCResponse;

    [MVCPath('/_bulk')]
    [MVCHTTPMethods([httpPOST])]
    function BulkCreateCustomers(
      [MVCFromBody] const Customers: TObjectList<TCustomer>;
      [MVCInject] CustomersService: ICustomersService): IMVCResponse;
  end;

implementation

uses
  System.SysUtils;

{ TCustomersController }

function TCustomersController.CreateCustomer(const Customer: TCustomer; CustomersService: ICustomersService): IMVCResponse;
begin
  var lID := CustomersService.CreateCustomer(Customer);
  Result := CreatedResponse('/api/customers/' + lID.ToString);
end;

function TCustomersController.GetCustomerByID(const ID: Integer; CustomersService: ICustomersService): IMVCResponse;
begin
  Result := OKResponse(CustomersService.GetByID(ID));
end;

function TCustomersController.GetCustomers(RQLFilter: String; CustomersService: ICustomersService): IMVCResponse;
begin
  Result := OKResponse(CustomersService.GetByRQL(RQLFilter));
end;

function TCustomersController.UpdateCustomerByID(const ID: Integer; const Customer: TCustomer; CustomersService: ICustomersService): IMVCResponse;
begin
  CustomersService.UpdateByID(ID, Customer);
  Result := OKResponse;
end;

function TCustomersController.BulkCreateCustomers(const Customers: TObjectList<TCustomer>; CustomersService: ICustomersService): IMVCResponse;
begin
  CustomersService.CreateCustomers(Customers);
  Result := CreatedResponse();
end;

end.
