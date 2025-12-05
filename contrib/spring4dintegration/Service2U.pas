unit Service2U;

interface

uses
  ServicesInterfaceU, Spring.Container.Common;

type
  TCustomersService = class(TInterfacedObject, ICustomersService)
  protected
    [Inject]
    fCommonService: ICommonService;
  public
    function GetCustomerNameByID(const ID: Integer): string;
  end;

implementation

uses
  System.SysUtils;

{ TCustomersService }

function TCustomersService.GetCustomerNameByID(const ID: Integer): string;
begin
  Result := Format('Customer #%d (CommonServiceID = %s)', [ID, fCommonService.GetID]);
end;

end.
