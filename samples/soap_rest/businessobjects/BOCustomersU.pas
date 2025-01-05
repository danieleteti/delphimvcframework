unit BOCustomersU;

interface

uses MVCFramework.Serializer.Commons, System.Generics.Collections;

type

  {$M+}

  [MVCNameCase(ncLowerCase)]
  TCustomer = class
  private
    FSurname: string;
    FMiddleName: string;
    FFirstName: string;
    procedure SetFirstName(const Value: string);
    procedure SetMiddleName(const Value: string);
    procedure SetSurname(const Value: string);
  published
    property FirstName: string read FFirstName write SetFirstName;
    property MiddleName: string read FMiddleName write SetMiddleName;
    property Surname: string read FSurname write SetSurname;

  end;

type
  TCustomers = TObjectList<TCustomer>;

implementation

{ TCustomer }

procedure TCustomer.SetFirstName(const Value: string);
begin
  FFirstName := Value;
end;

procedure TCustomer.SetMiddleName(const Value: string);
begin
  FMiddleName := Value;
end;

procedure TCustomer.SetSurname(const Value: string);
begin
  FSurname := Value;
end;

end.
