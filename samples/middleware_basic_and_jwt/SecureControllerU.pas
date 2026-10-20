unit SecureControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons;

type
  [MVCPath('/api')]
  TSecureController = class(TMVCController)
  public
    // reached with Basic credentials or with a Bearer token: the action cannot tell the difference
    [MVCPath('/secure')]
    [MVCHTTPMethod([httpGET])]
    function Secure: string;
  end;

implementation

function TSecureController.Secure: string;
begin
  Result := 'Hello ' + Context.LoggedUser.UserName;
end;

end.
