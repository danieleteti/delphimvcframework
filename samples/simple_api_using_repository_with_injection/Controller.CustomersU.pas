unit Controller.CustomersU;

interface

uses
  MVCFramework,
  MVCFramework.Repository,
  MVCFramework.Commons,
  System.Generics.Collections,
  EntitiesU;

type

  [MVCPath('/api/customers')]
  TCustomersController = class(TMVCController)
  protected
    fCustomersRepository: IMVCRepository<TCustomer>;
  public
    [MVCInject]
    constructor Create(CustomersRepository: IMVCRepository<TCustomer>); reintroduce;

    [MVCPath]
    [MVCHTTPMethods([httpGET])]
    function GetCustomers([MVCFromQueryString('rql', '')] RQLFilter: string): IMVCResponse;

    [MVCPath('/($ID:sqids)')]
    [MVCHTTPMethods([httpGET])]
    function GetCustomerByID(const ID: Integer): IMVCResponse;

    [MVCPath('/($ID:sqids)')]
    [MVCHTTPMethods([httpDELETE])]
    function DeleteCustomerByID(const ID: Integer): IMVCResponse;

    [MVCPath('/($ID:sqids)')]
    [MVCHTTPMethods([httpPUT])]
    function UpdateCustomerByID(
        const ID: Integer;
        [MVCFromBody] const Customer: TCustomer
    ): IMVCResponse;

    [MVCPath]
    [MVCHTTPMethods([httpPOST])]
    function CreateCustomer([MVCFromBody] const Customer: TCustomer): IMVCResponse;

    [MVCPath('/_bulk')]
    [MVCHTTPMethods([httpPOST])]
    function BulkCreateCustomers(
        [MVCFromBody] const Customers: TObjectList<TCustomer>
    ): IMVCResponse;
  end;

implementation

uses
  System.SysUtils;

{ TCustomersController }

constructor TCustomersController.Create(CustomersRepository: IMVCRepository<TCustomer>);
begin
  inherited Create;
  fCustomersRepository := CustomersRepository;
end;

function TCustomersController.CreateCustomer(const Customer: TCustomer): IMVCResponse;
begin
  fCustomersRepository.Store(Customer);
  Result := CreatedResponse('/api/customers/' + Customer.ID.Value.ToString);
end;

function TCustomersController.DeleteCustomerByID(const ID: Integer): IMVCResponse;
begin
  // Automatic transaction handling
  var lTx := TMVCRepository.UseTransactionContext;
  var lCustomer := fCustomersRepository.GetByPK(ID);
  fCustomersRepository.Delete(lCustomer);
  Result := OKResponse;
end;

function TCustomersController.GetCustomerByID(const ID: Integer): IMVCResponse;
begin
  Result := OKResponse(fCustomersRepository.GetByPK(ID));
end;

function TCustomersController.GetCustomers(RQLFilter: string): IMVCResponse;
begin
  Result := OKResponse(fCustomersRepository.SelectRQL(RQLFilter));
end;

function TCustomersController.UpdateCustomerByID(
    const ID: Integer;
    const Customer: TCustomer
): IMVCResponse;
begin
  Customer.ID := ID;
  fCustomersRepository.Update(Customer);
  Result := OKResponse;
end;

function TCustomersController.BulkCreateCustomers(
    const Customers: TObjectList<TCustomer>
): IMVCResponse;
begin
  begin
    var lCtx := TMVCRepository.UseTransactionContext;
    for var lCustomer in Customers do
    begin
      fCustomersRepository.Insert(lCustomer);
    end;
  end;
  Result := CreatedResponse();
end;

end.
