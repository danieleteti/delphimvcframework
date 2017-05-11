unit CustomersControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, CustomersTDGU;

type

  [MVCPath('/api')]
  TCustomersController = class(TMVCController)
  private
    FCustomersTDG: TCustomersTDG;
  protected
    function GetDAL: TCustomersTDG;
    procedure MVCControllerBeforeDestroy; override;
  public
    [MVCPath('/customers')]
    [MVCHTTPMethod([httpGET])]
    procedure GetCustomers;

    [MVCPath('/customers/($ID)')]
    [MVCHTTPMethod([httpGET])]
    procedure GetCustomer(const ID: UInt64);

    procedure OnBeforeAction(Context: TWebContext; const AActionName: string;
      var Handled: Boolean); override;
    procedure OnAfterAction(Context: TWebContext; const AActionName: string); override;

  end;

implementation

uses
  MVCFramework.Serializer.Commons;

procedure TCustomersController.GetCustomer(const ID: UInt64);
begin
  Render(GetDAL.GetCustomerById(ID), true, dstSingleRecord);
end;

procedure TCustomersController.GetCustomers;
begin
  Render(GetDAL.GetCustomers, true);
end;

function TCustomersController.GetDAL: TCustomersTDG;
begin
  if not Assigned(FCustomersTDG) then
    FCustomersTDG := TCustomersTDG.Create(nil);
  Result := FCustomersTDG;
end;

procedure TCustomersController.MVCControllerBeforeDestroy;
begin
  inherited;
  FCustomersTDG.Free;
end;

procedure TCustomersController.OnAfterAction(Context: TWebContext; const AActionName: string);
begin
  { Executed after each action }
  inherited;
end;

procedure TCustomersController.OnBeforeAction(Context: TWebContext; const AActionName: string;
  var Handled: Boolean);
begin
  { Executed before each action
    if handled is true (or an exception is raised) the actual
    action will not be called }
  inherited;
end;

end.
