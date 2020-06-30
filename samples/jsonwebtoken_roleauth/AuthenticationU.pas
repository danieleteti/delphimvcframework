unit AuthenticationU;

interface

uses
  System.SysUtils,
  MVCFramework.Commons,
  System.Generics.Collections,
  MVCFramework,
  MVCFramework.Middleware.Authentication.RoleBasedAuthHandler;

type
  TAuthenticationSample = class(TRoleBasedAuthHandler)
  public
    procedure OnAuthentication(
      const AContext: TWebContext;
      const UserName: string;
      const Password: string;
      UserRoles: TList<string>;
      var IsValid: Boolean;
      const SessionData: TDictionary<string, string>
      ); override;
  end;

implementation


{ TAuthenticationSample }

procedure TAuthenticationSample.OnAuthentication(const AContext: TWebContext; const UserName, Password: string;
  UserRoles: TList<string>; var IsValid: Boolean; const SessionData: TDictionary<string, string>);
begin
  IsValid := (not UserName.IsEmpty) and UserName.Equals(Password); // hey!, this is just a demo!!!
  if IsValid then
  begin
    if UserName = 'user_raise_exception' then
    begin
      raise EMVCException.Create(500, 1024, 'This is a custom exception raised in "TAuthenticationSample.OnAuthentication"');
    end;

    // Add here all the roles that the user has. These roles will be added to the JWT token

    if UserName = 'user1' then
    begin
      UserRoles.Add('role1');
    end;
    if UserName = 'user2' then
    begin
      UserRoles.Add('role2');
    end;
    if UserName = 'user3' then // all the roles
    begin
      UserRoles.Add('role1');
      UserRoles.Add('role2');
    end;

    // You can add custom data to the logged user
    SessionData.AddOrSetValue('customkey1', 'customvalue1');
    SessionData.AddOrSetValue('customkey2', 'customvalue2');

  end
  else
  begin
    UserRoles.Clear;
  end;

end;

end.
