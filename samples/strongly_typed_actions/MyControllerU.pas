unit MyControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons;

type
  TPerson = class
  private
    fAge: Integer;
    fLastName: String;
    fFirstName: String;
  public
    property FirstName: String read fFirstName write fFirstName;
    property LastName: String read fLastName write fLastName;
    property Age: Integer read fAge write fAge;
  end;

  [MVCPath('/api')]
  TMyController = class(TMVCController)
  public
    [MVCPath('/')]
    [MVCHTTPMethod([httpGET])]
    procedure Index;

    [MVCPath('/people/($person)')]
    [MVCHTTPMethod([httpGET])]
    procedure ComplexParameter(const Value: TPerson);
  protected
    procedure OnBeforeAction(Context: TWebContext; const AActionName: string;
      var Handled: Boolean); override;
    procedure OnAfterAction(Context: TWebContext; const AActionName: string); override;

  public
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
  // use Context property to access to the HTTP request and response
  Render('Hello DelphiMVCFramework World');
end;

procedure TMyController.ComplexParameter(const Value: TPerson);
begin
  Render(Value);
end;

procedure TMyController.OnAfterAction(Context: TWebContext; const AActionName: string);
begin
  { Executed after each action }
  inherited;
end;

procedure TMyController.OnBeforeAction(Context: TWebContext; const AActionName: string;
  var Handled: Boolean);
begin
  { Executed before each action
    if handled is true (or an exception is raised) the actual
    action will not be called }
  inherited;
end;

// Sample CRUD Actions for a "Customer" entity
procedure TMyController.GetCustomers;
begin
  // todo: render a list of customers
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
