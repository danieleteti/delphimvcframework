unit CustomersControllerU;

interface

uses
  MVCFramework,
  MVCFramework.ActiveRecord,
  MVCFramework.Commons;

type

  [MVCPath('/api/customers')]
  TCustomersController = class(TMVCController)
  public
    [MVCPath]
    [MVCHTTPMethods([httpGET])]
    procedure GetCustomers;

    [MVCPath('/($ID)')]
    [MVCHTTPMethods([httpGET])]
    procedure GetCustomerByID(const ID: Integer);

    [MVCPath]
    [MVCHTTPMethods([httpPOST])]
    procedure CreateCustomers;
  end;

implementation

uses
  System.SysUtils,
  FireDAC.Comp.Client,
  FireDAC.Stan.Param,
  MVCFramework.Logger,
  MVCFramework.Serializer.Commons,
  JsonDataObjects, EntitiesU;

{ TCustomersController }

procedure TCustomersController.CreateCustomers;
var
  lCustomer: TCustomer;
begin
  lCustomer := Context.Request.BodyAs<TCustomer>;
  try
    lCustomer.Insert;
    Render201Created('/api/customers/' + lCustomer.ID.Value.ToString);
  finally
    lCustomer.Free;
  end;
end;

procedure TCustomersController.GetCustomerByID(const ID: Integer);
begin
  Render(ObjectDict().Add('data', TMVCActiveRecord.GetByPK<TCustomer>(ID)));
end;

procedure TCustomersController.GetCustomers;
begin
  Render(ObjectDict().Add('data', TMVCActiveRecord.All<TCustomer>));
end;

end.
