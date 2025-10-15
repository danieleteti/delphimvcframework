unit CustomersControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Repository,
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
      [MVCInject] CustomersRepository: IMVCRepository<TCustomer>): IMVCResponse;

    [MVCPath('/($ID:sqids)')]
    [MVCHTTPMethods([httpGET])]
    function GetCustomerByID(
      const ID: Integer;
      [MVCInject] CustomersRepository: IMVCRepository<TCustomer>): IMVCResponse;

    [MVCPath('/($ID:sqids)')]
    [MVCHTTPMethods([httpDELETE])]
    function DeleteCustomerByID(
      const ID: Integer;
      [MVCInject] CustomersRepository: IMVCRepository<TCustomer>): IMVCResponse;

    [MVCPath('/($ID:sqids)')]
    [MVCHTTPMethods([httpPUT])]
    function UpdateCustomerByID(
      const ID: Integer;
      [MVCFromBody] const Customer: TCustomer;
      [MVCInject] CustomersRepository: IMVCRepository<TCustomer>): IMVCResponse;

    [MVCPath]
    [MVCHTTPMethods([httpPOST])]
    function CreateCustomer(
      [MVCFromBody] const Customer: TCustomer;
      [MVCInject] CustomersRepository: IMVCRepository<TCustomer>): IMVCResponse;

    [MVCPath('/_bulk')]
    [MVCHTTPMethods([httpPOST])]
    function BulkCreateCustomers(
      [MVCFromBody] const Customers: TObjectList<TCustomer>;
      [MVCInject] CustomersRepository: IMVCRepository<TCustomer>): IMVCResponse;
  end;

implementation

uses
  System.SysUtils;

{ TCustomersController }

function TCustomersController.CreateCustomer(const Customer: TCustomer; CustomersRepository: IMVCRepository<TCustomer>): IMVCResponse;
begin
  CustomersRepository.Store(Customer);
  Result := CreatedResponse('/api/customers/' + Customer.ID.Value.ToString);
end;

function TCustomersController.DeleteCustomerByID(const ID: Integer;
  CustomersRepository: IMVCRepository<TCustomer>): IMVCResponse;
begin
  var lTx := TMVCRepository.UseTransactionContext;
  var lCustomer := CustomersRepository.GetByPK(ID);
  CustomersRepository.Delete(lCustomer);
  Result := OKResponse;
end;

function TCustomersController.GetCustomerByID(const ID: Integer; CustomersRepository: IMVCRepository<TCustomer>): IMVCResponse;
begin
  Result := OKResponse(CustomersRepository.GetByPK(ID));
end;

function TCustomersController.GetCustomers(RQLFilter: String; CustomersRepository: IMVCRepository<TCustomer>): IMVCResponse;
begin
  Result := OKResponse(CustomersRepository.SelectRQL(RQLFilter));
end;

function TCustomersController.UpdateCustomerByID(const ID: Integer; const Customer: TCustomer; CustomersRepository: IMVCRepository<TCustomer>): IMVCResponse;
begin
  Customer.ID := ID;
  CustomersRepository.Update(Customer);
  Result := OKResponse;
end;

function TCustomersController.BulkCreateCustomers(const Customers: TObjectList<TCustomer>; CustomersRepository: IMVCRepository<TCustomer>): IMVCResponse;
begin
  begin var lCtx := TMVCRepository.UseTransactionContext;
    for var lCustomer in Customers do
    begin
      CustomersRepository.Insert(lCustomer);
    end;
  end;
  Result := CreatedResponse();
end;

end.
