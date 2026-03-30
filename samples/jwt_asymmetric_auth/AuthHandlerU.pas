// ***************************************************************************
//
// Delphi MVC Framework - JWT Asymmetric Auth Sample
//
// Simple authentication handler for demo purposes.
// In production, check credentials against a database.
//
// ***************************************************************************

unit AuthHandlerU;

interface

uses
  System.Generics.Collections,
  MVCFramework, MVCFramework.Commons;

type
  TSampleAuthHandler = class(TInterfacedObject, IMVCAuthenticationHandler)
  public
    procedure OnRequest(const AContext: TWebContext;
      const AControllerQualifiedClassName, AActionName: string;
      var AAuthenticationRequired: Boolean);
    procedure OnAuthentication(const AContext: TWebContext;
      const AUserName, APassword: string;
      AUserRoles: TList<string>;
      var AIsValid: Boolean;
      const ASessionData: TDictionary<string, string>);
    procedure OnAuthorization(const AContext: TWebContext;
      AUserRoles: TList<string>;
      const AControllerQualifiedClassName: string;
      const AActionName: string;
      var AIsAuthorized: Boolean);
  end;

implementation

uses
  System.SysUtils;

procedure TSampleAuthHandler.OnRequest(const AContext: TWebContext;
  const AControllerQualifiedClassName, AActionName: string;
  var AAuthenticationRequired: Boolean);
begin
  // Public endpoints don't require authentication
  AAuthenticationRequired :=
    not AActionName.ToLower.Contains('public');
end;

procedure TSampleAuthHandler.OnAuthentication(const AContext: TWebContext;
  const AUserName, APassword: string;
  AUserRoles: TList<string>;
  var AIsValid: Boolean;
  const ASessionData: TDictionary<string, string>);
begin
  // Demo credentials - in production use a real user store
  AIsValid := False;
  if SameText(AUserName, 'admin') and (APassword = 'admin') then
  begin
    AIsValid := True;
    AUserRoles.Add('admin');
    AUserRoles.Add('user');
  end
  else if SameText(AUserName, 'user') and (APassword = 'user') then
  begin
    AIsValid := True;
    AUserRoles.Add('user');
  end;
end;

procedure TSampleAuthHandler.OnAuthorization(const AContext: TWebContext;
  AUserRoles: TList<string>;
  const AControllerQualifiedClassName, AActionName: string;
  var AIsAuthorized: Boolean);
begin
  AIsAuthorized := True; // All authenticated users are authorized in this demo
end;

end.
