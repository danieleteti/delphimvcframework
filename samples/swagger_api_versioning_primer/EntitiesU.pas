unit EntitiesU;

interface

uses
  MVCFramework.Serializer.Commons,
  System.Generics.Collections,
  MVCFramework.Swagger.Commons;

type

  [MVCNameCase(ncLowerCase)]
  TCustomer = class
  private
    fID: Integer;
    fCustomerName: String;
    fCountry: String;
    fContactName: String;
  public
    function IsValid: Boolean;
    [MVCSwagJSONSchemaField(stInteger, 'id', 'Customer ID', True)]
    property ID: Integer read fID write fID;
    [MVCSwagJSONSchemaField(stString, 'customername', 'The registered name of the customer', True)]
    property CustomerName: String read fCustomerName write fCustomerName;
    [MVCSwagJSONSchemaField(stString, 'contactname', 'Fullname of the customer''s contact', False)]
    property ContactName: String read fContactName write fContactName;
    [MVCSwagJSONSchemaField(stString, 'country', 'The country where the company is registered', True)]
    property Country: String read fCountry write fCountry;
  end;

  [MVCNameCase(ncLowerCase)]
  TCustomers = class(TObjectList<TCustomer>)
  end;

function GetCustomer(const ID: Integer): TCustomer;
function GetCustomers: TCustomers;

implementation

uses
  System.Math, System.SysUtils;

const
  CONTACT_NAMES: array [0 .. 2] of string = ('Daniele Teti', 'Peter Parker', 'Bruce Banner');
  CONTRIES: array [0 .. 2] of string = ('ITALY', 'USA', 'United Kingdom');
  CUSTOMER_NAMES: array [0 .. 2] of string = ('bit Time Professionals s.r.l.', 'Spidey Ltd.', 'Green Power Corp.');

function GetCustomer(const ID: Integer): TCustomer;
begin
  Result := TCustomer.Create;
  Result.ID := ID;
  Result.CustomerName := CUSTOMER_NAMES[Random(3)];
  Result.Country := CONTRIES[Random(3)];
  Result.ContactName := CONTACT_NAMES[Random(3)];
end;

function GetCustomers: TCustomers;
var
  I: Integer;
begin
  Result := TCustomers.Create(True);
  for I := 1 to 3 do
  begin
    Result.Add(GetCustomer(I));
  end;
end;

{ TCustomer }

function TCustomer.IsValid: Boolean;
begin
  Result := not(CustomerName.IsEmpty or Country.IsEmpty);
end;

end.
