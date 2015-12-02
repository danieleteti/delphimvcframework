unit RESTControllerCustomerU;

interface

uses
  MVCFramework, WSHelperCustomersU;

type

  [MVCPath('/customers')]
  TControllerCustomer = class(TMVCController)
  private
    FWSHelperCustomers: TWSHelperCustomers;
  protected
    procedure OnBeforeAction(Context: TWebContext; const AActionNAme: string;
      var Handled: Boolean); override;
    procedure OnAfterAction(Context: TWebContext;
      const AActionNAme: string); override;

  public
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    procedure GetCustomers(ctx: TWebContext);

  end;

implementation

uses
  System.SysUtils, BOCustomersU;

procedure TControllerCustomer.OnBeforeAction(Context: TWebContext;
  const AActionNAme: string; var Handled: Boolean);
begin
  FWSHelperCustomers := TWSHelperCustomers.Create;
end;

procedure TControllerCustomer.OnAfterAction(Context: TWebContext;
  const AActionNAme: string);
begin
  FreeAndNil(FWSHelperCustomers);
end;

procedure TControllerCustomer.GetCustomers(ctx: TWebContext);
begin
  Render<TCustomer>(FWSHelperCustomers.GetCustomers);
end;

end.
