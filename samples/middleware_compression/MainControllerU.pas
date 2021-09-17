unit MainControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons;

type

  [MVCPath('/')]
  TMyController = class(TMVCController)
  public
    [MVCPath('/customers')]
    [MVCHTTPMethod([httpGET])]
    procedure GetCustomers;

    [MVCPath('/tallcustomers')]
    [MVCHTTPMethod([httpGET])]
    procedure GetCustomersSmallList;

    [MVCPath('/customers/($id)')]
    [MVCHTTPMethod([httpGET])]
    procedure GetCustomer(id: Integer);

    [MVCPath('/customers')]
    [MVCHTTPMethod([httpPOST])]
    procedure CreateCustomer;

    [MVCPath('/customers/($id)')]
    [MVCHTTPMethod([httpPUT])]
    procedure UpdateCustomer(id: Integer);

    [MVCPath('/customers/($id)')]
    [MVCHTTPMethod([httpDELETE])]
    procedure DeleteCustomer(id: Integer);

  end;

implementation

uses
  System.SysUtils, MVCFramework.Logger, System.StrUtils, InMemoryDataU,
  BusinessObjectsU;

// Sample CRUD Actions for a "Customer" entity
procedure TMyController.GetCustomers;
begin
  Render<TPerson>(GetPeopleList, False);
end;

procedure TMyController.GetCustomersSmallList;
begin
  Render<TPerson>(GetPeopleSmallList, False);
end;

procedure TMyController.GetCustomer(id: Integer);
begin
  // todo: render the customer by id
end;

procedure TMyController.CreateCustomer;

begin
  // todo: create a new customer
end;

procedure TMyController.UpdateCustomer(id: Integer);
begin
  // todo: update customer by id
end;

procedure TMyController.DeleteCustomer(id: Integer);
begin
  // todo: delete customer by id
end;

end.
