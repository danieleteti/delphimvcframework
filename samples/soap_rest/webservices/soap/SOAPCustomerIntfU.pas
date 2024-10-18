{ Invokable interface ISOAPCustomer }

unit SOAPCustomerIntfU;

interface

uses Soap.InvokeRegistry, System.Types, Soap.XSBuiltIns;

type

  { Invokable interfaces must derive from IInvokable }
  ISOAPCustomer = interface(IInvokable)
    ['{9D4C2E66-F0AB-470E-9A48-2084DAD75FD3}']
    function GetCustomers: string; stdcall;
    { Methods of Invokable interface must not use the default }
    { calling convention; stdcall is recommended }
  end;

implementation

initialization

{ Invokable interfaces must be registered }
InvRegistry.RegisterInterface(TypeInfo(ISOAPCustomer));

end.
