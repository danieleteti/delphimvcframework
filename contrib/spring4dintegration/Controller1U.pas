unit Controller1U;

interface

uses
  MVCFramework, MVCFramework.Commons, ServicesInterfaceU, Spring.Container.Common;

type

  [MVCPath('/controller1')]
  TMyController1 = class(TMVCController)
  protected
    [Inject]
    fUsersService: IUsersService;
    [Inject]
    fCustomersService: ICustomersService;
  public
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    procedure Index;
  end;

implementation

uses
  System.SysUtils, MVCFramework.Logger, System.StrUtils;

procedure TMyController1.Index;
begin
  ContentType := BuildContentType(TMVCMediaType.TEXT_PLAIN, TMVCCharSet.ISO88591);
  ResponseStream.AppendLine('THIS IS A TEST FOR SPRING4D INTEGRATION');
  ResponseStream.AppendLine('===============================================================');
  ResponseStream.AppendLine('fUsersService.GetUserNameByID(1234)          => ' +
    fUsersService.GetUserNameByID(1234));
  ResponseStream.AppendLine('fCustomersService.GetCustomerNameByID(1234)  => ' +
    fCustomersService.GetCustomerNameByID(1234));
  RenderResponseStream;
end;

end.
