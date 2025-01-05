unit CustomerServiceU;

interface

uses
  MVCFramework.Container, System.Generics.Collections, EntitiesU;

type
  ICustomersService = interface
    ['{0499A9BA-1DC4-48CB-8514-3B3E062C6481}']
    function GetByRQL(const RQL: String): TObjectList<TCustomer>;
    function GetByID(const ID: Integer): TCustomer;
    procedure UpdateByID(const ID: Integer; const Customer: TCustomer);
    function CreateCustomer(const Customer: TCustomer): Integer;
    procedure CreateCustomers(const Customers: TObjectList<TCustomer>);
  end;

  TCustomersService = class(TInterfacedObject, ICustomersService)
  protected
    function GetByRQL(const RQL: String): TObjectList<TCustomer>;
    function GetByID(const ID: Integer): TCustomer;
    procedure UpdateByID(const ID: Integer; const Customer: TCustomer);
    function CreateCustomer(const Customer: TCustomer): Integer;
    procedure CreateCustomers(const Customers: TObjectList<TCustomer>);
  end;

procedure RegisterServices(Container: IMVCServiceContainer);

implementation

uses
  System.SysUtils, MVCFramework.ActiveRecord;

procedure RegisterServices(Container: IMVCServiceContainer);
begin
  Container.RegisterType(TCustomersService, ICustomersService, TRegistrationType.SingletonPerRequest);
end;

function TCustomersService.GetByRQL(const RQL: String): TObjectList<TCustomer>;
begin
  Result := TMVCActiveRecord.SelectRQL<TCustomer>(RQL, 1000);
end;

procedure TCustomersService.UpdateByID(const ID: Integer;
  const Customer: TCustomer);
begin
  Customer.ID := ID;
  Customer.Update;
end;

function TCustomersService.CreateCustomer(const Customer: TCustomer): Integer;
begin
  Customer.Insert;
  Result := Customer.ID.Value;
end;

procedure TCustomersService.CreateCustomers(
  const Customers: TObjectList<TCustomer>);
begin
  begin var lCtx := TMVCActiveRecord.UseTransactionContext;
    for var lCustomer in Customers do
    begin
      lCustomer.Insert;
    end;
  end;
end;

function TCustomersService.GetByID(const ID: INteger): TCustomer;
begin
  Result := TMVCActiveRecord.GetByPK<TCustomer>(ID);
end;

end.
