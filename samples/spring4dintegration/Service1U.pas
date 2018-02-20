unit Service1U;

interface

uses ServicesInterfaceU, Spring.Container.Common;

type
  TUsersService = class(TInterfacedObject, IUsersService)
  protected
    [Inject]
    fCommonService: ICommonService;
  public
    function GetUserNameByID(const ID: Integer): string;
  end;

implementation

uses
  System.SysUtils;

{ TUsersService }

function TUsersService.GetUserNameByID(const ID: Integer): string;
begin
  Result := Format('User #%d (CommonServiceID = %s)', [ID, fCommonService.GetID]);
end;

end.
