unit SampleControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons;

type

  [MVCPath('/')]
  TSampleController = class(TMVCController)
  public
    [MVCHTTPMethod([httpPost])]
    [MVCPath('/customers')]
    [MVCProduces('text/html')]
    procedure CreateCustomer(CTX: TWebContext);

  end;

implementation

uses
  System.SysUtils;

{ TRoutingSampleController }

procedure TSampleController.CreateCustomer(CTX: TWebContext);
begin
  LoadView('customer_show');
end;

end.
