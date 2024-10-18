unit WSHelperCustomersU;

interface

uses
  BOCustomersU;

type
  TWSHelperCustomers = class
  public
    function GetCustomers: TCustomers; overload;
  end;

implementation

uses
   System.SysUtils;

{ TWSHelperCustomers }

function TWSHelperCustomers.GetCustomers: TCustomers;
var
  Customer: TCustomer;
begin
  Result := TCustomers.Create;

  Customer := TCustomer.Create;
  Customer.FirstName := 'Joe';
  Customer.MiddleName := 'M';
  Customer.Surname := 'Bloggs';

  Result.Add(Customer);

  Customer := TCustomer.Create;
  Customer.FirstName := 'Mary';
  Customer.MiddleName := 'J';
  Customer.Surname := 'Jones';

  Result.Add(Customer);

end;


end.
