unit AuthenticationU;

interface

uses
  System.SysUtils, MVCFramework.Commons, System.Generics.Collections,
  MVCFramework;

type
  TAuthenticationSample = class(TInterfacedObject, IMVCAuthenticationHandler)
  protected
    procedure OnRequest(const AContext: TWebContext; const ControllerQualifiedClassName: string;
      const ActionName: string; var AuthenticationRequired: Boolean);
    procedure OnAuthentication(const AContext: TWebContext; const UserName: string; const Password: string;
      UserRoles: System.Generics.Collections.TList<System.string>;
      var IsValid: Boolean; const SessionData: TSessionData);
    procedure OnAuthorization(
      const AContext: TWebContext;
      UserRoles: System.Generics.Collections.TList<System.string>;
      const ControllerQualifiedClassName: string; const ActionName: string;
      var IsAuthorized: Boolean);
  end;

implementation

{ TMVCAuthorization }

procedure TAuthenticationSample.OnAuthentication(const AContext: TWebContext; const UserName: string; const Password: string;
      UserRoles: System.Generics.Collections.TList<System.string>;
      var IsValid: Boolean; const SessionData: TSessionData);
begin
  IsValid := UserName.Equals(Password); // hey!, this is just a demo!!!
  if IsValid then
  begin
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
  end
  else
  begin
    UserRoles.Clear;
  end;
end;

procedure TAuthenticationSample.OnAuthorization
  (
      const AContext: TWebContext;
      UserRoles: System.Generics.Collections.TList<System.string>;
      const ControllerQualifiedClassName: string; const ActionName: string;
      var IsAuthorized: Boolean);
begin
  IsAuthorized := False;
  if ActionName = 'Logout' then
    IsAuthorized := True; // you can always call logout
  if ActionName = 'OnlyRole2' then
    IsAuthorized := UserRoles.Contains('role2');
  if ActionName = 'OnlyRole1' then
    IsAuthorized := UserRoles.Contains('role1');
  if ActionName = 'OnlyRole1EmittingJSON' then
    IsAuthorized := UserRoles.Contains('role1');
end;

procedure TAuthenticationSample.OnRequest(const AContext: TWebContext; const ControllerQualifiedClassName: string;
      const ActionName: string; var AuthenticationRequired: Boolean);
begin
  AuthenticationRequired := ControllerQualifiedClassName =
    'AppControllerU.TAdminController';

end;

end.
