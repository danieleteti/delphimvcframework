unit ProtectedControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  JsonDataObjects;

type
  // Requires a valid access token (the check of JWTAuthenticationBundle, WebModuleU).
  // Login (/auth/login) and refresh (/auth/refresh) are answered by the bundle itself:
  // no controller of ours issues tokens.
  [MVCPath('/api')]
  TProtectedController = class(TMVCController)
  public
    [MVCPath('/profile')]
    [MVCHTTPMethod([httpGET])]
    function Profile: TJSONObject;
  end;

implementation

uses
  System.SysUtils;

{ TProtectedController }

function TProtectedController.Profile: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.S['message'] := 'protected content';
  Result.S['username'] := Context.LoggedUser.UserName;
  Result.S['roles'] := string.Join(',', Context.LoggedUser.Roles.ToArray);
end;

end.
