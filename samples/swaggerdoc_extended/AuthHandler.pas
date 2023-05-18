unit AuthHandler;

interface

uses
  MVCFramework,
  System.Generics.Collections,
  MVCFramework.Middleware.Authentication.RoleBasedAuthHandler;

type
  TAuthHandler = class(TRoleBasedAuthHandler)
  public
    procedure OnAuthentication(
      const AContext: TWebContext;
      const UserName: string;
      const Password: string;
      UserRoles: TList<string>;
      var IsValid: Boolean;
      const SessionData: TDictionary<string, string>); override;
  end;

implementation

uses
  System.SysUtils;

{ TAuthHandler }

procedure TAuthHandler.OnAuthentication(const AContext: TWebContext; const UserName, Password: string;
  UserRoles: TList<string>; var IsValid: Boolean; const SessionData: TDictionary<string, string>);
begin
  IsValid := UserName.Equals(Password);
  if IsValid then
  begin
    UserRoles.Add('role1');
    UserRoles.Add('role2');
    UserRoles.Add('role3');
    UserRoles.Add('role4');
    SessionData.AddOrSetValue('custom1', 'value1');
    SessionData.AddOrSetValue('custom2', 'value2');
    SessionData.AddOrSetValue('custom3', 'value3');
    SessionData.AddOrSetValue('custom4', 'value4');
  end;
end;

end.
