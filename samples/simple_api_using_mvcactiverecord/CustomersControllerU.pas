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
    [MVCPath('/($ID)')]
    [MVCHTTPMethods([httpGET])]
    function GetCustomerByID(const ID: Integer): TCustomer;

    [MVCPath]
    [MVCHTTPMethods([httpGET])]
    function GetCustomers([MVCFromQueryString('rql','')] RQLFilter: String): TObjectList<TCustomer>;

    [MVCPath]
    [MVCHTTPMethods([httpPOST])]
    function CreateCustomer([MVCFromBody] const Customer: TCustomer): IMVCResponse;

    [MVCPath('/_bulk')]
    [MVCHTTPMethods([httpPOST])]
    procedure BulkCreateCustomers([MVCFromBody] const Customers: TObjectList<TCustomer>);
  end;

implementation

uses
  System.SysUtils,
  FireDAC.Comp.Client,
  FireDAC.Stan.Param,
  MVCFramework.Logger,
  MVCFramework.Serializer.Commons,
  JsonDataObjects;

{ TCustomersController }

function TCustomersController.CreateCustomer(const Customer: TCustomer): IMVCResponse;
begin
  Customer.Insert;
  Result := CreatedResponse('/api/customers/' + Customer.ID.Value.ToString);
end;

function TCustomersController.GetCustomerByID(const ID: Integer): TCustomer;
begin
  Result := TMVCActiveRecord.GetByPK<TCustomer>(ID);
end;

function TCustomersController.GetCustomers([MVCFromQueryString('rql','')] RQLFilter: String): TObjectList<TCustomer>;
begin
  Result := TMVCActiveRecord.SelectRQL<TCustomer>(RQLFilter, 1000);
end;

procedure TCustomersController.BulkCreateCustomers(const Customers: TObjectList<TCustomer>);
begin
  TMVCActiveRecord.CurrentConnection.StartTransaction;
  try
    for var lCustomer in Customers do
    begin
      lCustomer.Insert;
    end;
    TMVCActiveRecord.CurrentConnection.Commit;
    Render201Created();
  except
    TMVCActiveRecord.CurrentConnection.Rollback;
    raise;
  end;
end;

end.
