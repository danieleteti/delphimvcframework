unit UController;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Serializer.Commons,
  Spring.Collections,
  Entities;

type

  [MVCPath('/api')]
  TMyController = class(TMVCController)
  private
    function GetCustomerList: IList<TCustomer>;
  public
    [MVCPath('')]
    [MVCHTTPMethod([httpGET])]
    procedure Index;

    // Sample CRUD Actions for a "Customer" entity
    [MVCPath('/customers')]
    [MVCHTTPMethod([httpGET])]
    procedure GetCustomers;

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
  System.SysUtils,
  MVCFramework.Logger,
  System.StrUtils;

procedure TMyController.Index;
begin
  Render('Hello DelphiMVCFramework World');
end;

procedure TMyController.GetCustomers;
begin
  Render(GetCustomerList);
end;

procedure TMyController.GetCustomer(id: Integer);
var
  lCustomers: IList<TCustomer>;
begin
  lCustomers := GetCustomerList;
  Render(lCustomers.First(
    function (const ACustomer: TCustomer): Boolean
    begin
      Result := ACustomer.Id = id;
    end),
    False
    );
end;

function TMyController.GetCustomerList: IList<TCustomer>;
var
  lCustomer: TCustomer;
begin
  Result := TCollections.CreateObjectList<TCustomer>;
  lCustomer := TCustomer.Create;
  lCustomer.Id := 1;
  lCustomer.FirstName := 'João Antônio';
  lCustomer.LastName := 'Duarte';
  lCustomer.Phones.Add(TPhone.Create(1, 'Phone 01', '11111111'));
  lCustomer.Phones.Add(TPhone.Create(2, 'Phone 02', '22222222'));
  Result.Add(lCustomer);

  lCustomer := TCustomer.Create;
  lCustomer.Id := 2;
  lCustomer.FirstName := 'Daniele';
  lCustomer.LastName := 'Teti';
  lCustomer.Phones.Add(TPhone.Create(3, 'Phone 01', '33333333'));
  lCustomer.Phones.Add(TPhone.Create(4, 'Phone 02', '44444444'));
  Result.Add(lCustomer);

  lCustomer := TCustomer.Create;
  lCustomer.Id := 3;
  lCustomer.FirstName := 'Isaac';
  lCustomer.LastName := 'Newton';
  lCustomer.Phones.Add(TPhone.Create(5, 'Phone 01', '55555555'));
  lCustomer.Phones.Add(TPhone.Create(6, 'Phone 02', '66666666'));
  Result.Add(lCustomer);

  lCustomer := TCustomer.Create;
  lCustomer.Id := 4;
  lCustomer.FirstName := 'Nikola';
  lCustomer.LastName := 'Tesla';
  lCustomer.Phones.Add(TPhone.Create(7, 'Phone 01', '77777777'));
  lCustomer.Phones.Add(TPhone.Create(8, 'Phone 02', '88888888'));
  Result.Add(lCustomer);

end;

procedure TMyController.CreateCustomer;
var
  lCustomer: TCustomer;
begin
  lCustomer := Context.Request.BodyAs<TCustomer>;
  ResponseStatus(HTTP_STATUS.Created, 'Created');
  Render(lCustomer);
end;

procedure TMyController.UpdateCustomer(id: Integer);
var
  lCustomer: TCustomer;
begin
  lCustomer := Context.Request.BodyAs<TCustomer>;
  ResponseStatus(HTTP_STATUS.OK, 'Updated');
  Render(lCustomer);
end;

procedure TMyController.DeleteCustomer(id: Integer);
begin
  Render(HTTP_STATUS.OK, 'Deleted');
end;

end.
