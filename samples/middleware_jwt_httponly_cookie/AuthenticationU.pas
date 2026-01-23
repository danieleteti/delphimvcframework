unit AuthenticationU;

interface

uses
  System.SysUtils,
  MVCFramework.Commons,
  System.Generics.Collections,
  MVCFramework;

type
  TAuthenticationSample = class(TInterfacedObject, IMVCAuthenticationHandler)
  protected
    procedure OnRequest(
      const AContext: TWebContext;
      const ControllerQualifiedClassName: string;
      const ActionName: string;
      var AuthenticationRequired: Boolean);

    procedure OnAuthentication(
      const AContext: TWebContext;
      const UserName: string;
      const Password: string;
      UserRoles: TList<System.string>;
      var IsValid: Boolean;
      const SessionData: TSessionData);

    procedure OnAuthorization(
      const AContext: TWebContext;
      UserRoles: TList<System.string>;
      const ControllerQualifiedClassName: string;
      const ActionName: string;
      var IsAuthorized: Boolean);
  end;

implementation

{ TAuthenticationSample }

procedure TAuthenticationSample.OnAuthentication(
  const AContext: TWebContext;
  const UserName: string;
  const Password: string;
  UserRoles: TList<System.string>;
  var IsValid: Boolean;
  const SessionData: TSessionData);
begin
  // Simple demo authentication: username must equal password
  // In production, validate against database, LDAP, etc.
  IsValid := (not UserName.IsEmpty) and UserName.Equals(Password);

  if IsValid then
  begin
    // Assign roles based on username
    if UserName = 'user1' then
    begin
      UserRoles.Add('role1');
    end
    else if UserName = 'user2' then
    begin
      UserRoles.Add('role2');
    end
    else if UserName = 'user3' then
    begin
      // user3 has both roles
      UserRoles.Add('role1');
      UserRoles.Add('role2');
    end
    else if UserName = 'admin' then
    begin
      UserRoles.Add('role1');
      UserRoles.Add('role2');
      UserRoles.Add('admin');
    end;

    // Add custom session data (will be included in JWT)
    SessionData.AddOrSetValue('department', 'IT');
    SessionData.AddOrSetValue('level', '5');
  end
  else
  begin
    UserRoles.Clear;
  end;
end;

procedure TAuthenticationSample.OnAuthorization(
  const AContext: TWebContext;
  UserRoles: TList<System.string>;
  const ControllerQualifiedClassName: string;
  const ActionName: string;
  var IsAuthorized: Boolean);
begin
  IsAuthorized := False;

  // UserProfile is accessible by any authenticated user
  if ActionName = 'UserProfile' then
    IsAuthorized := True
  // OnlyRole1 requires role1
  else if ActionName = 'OnlyRole1' then
    IsAuthorized := UserRoles.Contains('role1')
  // OnlyRole2 requires role2
  else if ActionName = 'OnlyRole2' then
    IsAuthorized := UserRoles.Contains('role2');
end;

procedure TAuthenticationSample.OnRequest(
  const AContext: TWebContext;
  const ControllerQualifiedClassName: string;
  const ActionName: string;
  var AuthenticationRequired: Boolean);
begin
  // Only TAdminController requires authentication
  AuthenticationRequired := ControllerQualifiedClassName = 'AppControllerU.TAdminController';
end;

end.
