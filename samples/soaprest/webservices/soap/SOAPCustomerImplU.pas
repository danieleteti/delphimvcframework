{ Invokable implementation File for TSOAPCustomer which implements ISOAPCustomer }

unit SOAPCustomerImplU;

interface

uses Soap.InvokeRegistry, System.Types, Soap.XSBuiltIns, SOAPCustomerIntfU;

type

  { TSOAPCustomer }
  TSOAPCustomer = class(TInvokableClass, ISOAPCustomer)
  public
    function GetCustomers: string; stdcall;
  end;

implementation

uses
  BOCustomersU, WSHelperCustomersU, System.SysUtils, System.JSON, MVCFramework.Serializer.Defaults;

function TSOAPCustomer.GetCustomers: string;
var
  WSHelperCustomers: TWSHelperCustomers;
  Customers: TCustomers;
begin
  WSHelperCustomers := TWSHelperCustomers.Create;
  try
    begin
      Customers := WSHelperCustomers.GetCustomers;
      try
        Result := GetDefaultSerializer.SerializeCollection(Customers);
      finally
        FreeAndNil(Customers);
      end;
    end;
  finally
    FreeAndNil(WSHelperCustomers);
  end;
end;

initialization

{ Invokable classes must be registered }
InvRegistry.RegisterInvokableClass(TSOAPCustomer);

end.
