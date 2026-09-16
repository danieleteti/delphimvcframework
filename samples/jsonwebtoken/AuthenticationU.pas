unit AuthenticationU;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  MVCFramework.Commons,
  MVCFramework;

type
  TAuthenticationSample = class(TInterfacedObject, IMVCAuthenticationHandler)
  protected
    procedure OnRequest(const AContext: TWebContext;
      const ControllerQualifiedClassName: string;
      const ActionName: string;
      var AuthenticationRequired: Boolean);
    procedure OnAuthentication(const AContext: TWebContext;
      const UserName: string;
      const Password: string;
      UserRoles: TList<string>;
      var IsValid: Boolean;
      const SessionData: TDictionary<string, string>);
    procedure OnAuthorization(const AContext: TWebContext;
      UserRoles: TList<string>;
      const ControllerQualifiedClassName: string;
      const ActionName: string;
      var IsAuthorized: Boolean);
  end;

implementation

procedure TAuthenticationSample.OnAuthentication(const AContext: TWebContext;
  const UserName, Password: string;
  UserRoles: TList<string>;
  var IsValid: Boolean;
  const SessionData: TDictionary<string, string>);
begin
  // Demo: username must equal password (don't do this in production!)
  IsValid := (not UserName.IsEmpty) and UserName.Equals(Password);
  if IsValid then
  begin
    if UserName = 'user_raise_exception' then
      raise EMVCException.Create(HTTP_STATUS.InternalServerError, 1024,
        'This is a custom exception raised in OnAuthentication');

    if UserName = 'user1' then
      UserRoles.Add('role1');
    if UserName = 'user2' then
      UserRoles.Add('role2');
    if UserName = 'user3' then
    begin
      UserRoles.Add('role1');
      UserRoles.Add('role2');
    end;

    SessionData.AddOrSetValue('customkey1', 'customvalue1');
    SessionData.AddOrSetValue('customkey2', 'customvalue2');
  end
  else
    UserRoles.Clear;
end;

procedure TAuthenticationSample.OnAuthorization(const AContext: TWebContext;
  UserRoles: TList<string>;
  const ControllerQualifiedClassName, ActionName: string;
  var IsAuthorized: Boolean);
begin
  IsAuthorized := False;
  if ActionName = 'Logout' then
    IsAuthorized := True;
  if ActionName = 'OnlyRole2' then
    IsAuthorized := UserRoles.Contains('role2');
  if ActionName = 'OnlyRole1' then
    IsAuthorized := UserRoles.Contains('role1');
end;

procedure TAuthenticationSample.OnRequest(const AContext: TWebContext;
  const ControllerQualifiedClassName, ActionName: string;
  var AuthenticationRequired: Boolean);
begin
  AuthenticationRequired :=
    ControllerQualifiedClassName = 'AppControllerU.TAdminController';
end;

end.
