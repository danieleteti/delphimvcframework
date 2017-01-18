unit RESTControllerCustomerU;

interface

uses
  MVCFramework, MVCFramework.Commons, WSHelperCustomersU;

type

  [MVCPath('/')]
  TControllerCustomer = class(TMVCController)
  private
    FWSHelperCustomers: TWSHelperCustomers;
  protected
    procedure OnBeforeAction(Context: TWebContext; const AActionNAme: string;
      var Handled: Boolean); override;
    procedure OnAfterAction(Context: TWebContext;
      const AActionNAme: string); override;

  public
    [MVCPath('/customers')]
    [MVCHTTPMethod([httpGET])]
    procedure GetCustomers(ctx: TWebContext);

    [MVCPath('/')]
    [MVCHTTPMethod([httpGET])]
    procedure Index(ctx: TWebContext);

  end;

implementation

uses
  System.SysUtils, BOCustomersU;

procedure TControllerCustomer.OnBeforeAction(Context: TWebContext;
  const AActionNAme: string; var Handled: Boolean);
begin
  FWSHelperCustomers := TWSHelperCustomers.Create;
end;

procedure TControllerCustomer.Index(ctx: TWebContext);
begin
  Redirect('/index.html');
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
