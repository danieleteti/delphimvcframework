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
    procedure GetCustomers([MVCFromQueryString('rql','')] RQLFilter: String);

    [MVCPath('/($ID)')]
    [MVCHTTPMethods([httpGET])]
    procedure GetCustomerByID(const ID: Integer);

    [MVCPath]
    [MVCHTTPMethods([httpPOST])]
    procedure CreateCustomer([MVCFromBody] const Customer: TCustomer);

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

procedure TCustomersController.CreateCustomer(const Customer: TCustomer);
begin
  Customer.Insert;
  Render201Created('/api/customers/' + Customer.ID.Value.ToString);
end;

procedure TCustomersController.GetCustomerByID(const ID: Integer);
begin
  Render(ObjectDict().Add('data', TMVCActiveRecord.GetByPK<TCustomer>(ID)));
end;

procedure TCustomersController.GetCustomers([MVCFromQueryString('rql','')] RQLFilter: String);
begin
  if RQLFilter.IsEmpty then
    Render(ObjectDict().Add('data', TMVCActiveRecord.All<TCustomer>))
  else
    Render(ObjectDict().Add('data', TMVCActiveRecord.SelectRQL<TCustomer>(RQLFilter, 1000)));
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
  end;
end;

end.
