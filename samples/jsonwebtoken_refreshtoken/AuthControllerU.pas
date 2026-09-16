unit AuthControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  JsonDataObjects;

type
  // Issues the access+refresh pair. The refresh endpoint itself (/auth/refresh)
  // is owned by TMVCJWTRefreshTokenMiddleware, not by a controller action.
  [MVCPath('/auth')]
  TAuthController = class(TMVCController)
  public
    [MVCPath('/login')]
    [MVCHTTPMethod([httpPOST])]
    function Login: TJSONObject;
  end;

  // Requires a valid access token (enforced by TMVCJWTAuthenticationMiddleware).
  [MVCPath('/api')]
  TProtectedController = class(TMVCController)
  public
    [MVCPath('/profile')]
    [MVCHTTPMethod([httpGET])]
    function Profile: TJSONObject;
  end;

implementation

uses
  System.SysUtils,
  MVCFramework.Serializer.Commons,
  MVCFramework.Serializer.JsonDataObjects,
  SharedU;

{ TAuthController }

function TAuthController.Login: TJSONObject;
var
  lBody: TJSONObject;
  lUser, lPass, lAccess, lRefresh: string;
begin
  lBody := StrToJSONObject(Context.Request.Body);
  try
    lUser := lBody.S['username'];
    lPass := lBody.S['password'];
  finally
    lBody.Free;
  end;

  // demo credential check: username must equal password
  if lUser.IsEmpty or not lUser.Equals(lPass) then
    raise EMVCException.Create(HTTP_STATUS.Unauthorized, 'Invalid credentials');

  gRefreshCore.NewTokenPair(lUser, 'role1,role2', lAccess, lRefresh);

  Result := TJSONObject.Create;
  Result.S['access_token'] := lAccess;
  Result.S['refresh_token'] := lRefresh;
  Result.S['token_type'] := 'bearer';
  Result.I['expires_in'] := gRefreshCfg.AccessTokenTTLSeconds;
end;

{ TProtectedController }

function TProtectedController.Profile: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.S['message'] := 'protected content';
  Result.S['username'] := Context.LoggedUser.UserName;
  Result.S['roles'] := string.Join(',', Context.LoggedUser.Roles.ToArray);
end;

end.
