unit AuthenticationU;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  MVCFramework,
  MVCFramework.Commons;

type
  TAuthenticationSample = class(TInterfacedObject, IMVCAuthenticationHandler)
  protected
    procedure OnRequest(const AContext: TWebContext; const ControllerQualifiedClassName: string;
      const ActionName: string; var AuthenticationRequired: Boolean);
    procedure OnAuthentication(const AContext: TWebContext; const UserName: string; const Password: string;
      UserRoles: TList<string>; var IsValid: Boolean; const SessionData: TSessionData);
    procedure OnAuthorization(const AContext: TWebContext; UserRoles: TList<string>;
      const ControllerQualifiedClassName: string; const ActionName: string; var IsAuthorized: Boolean);
  end;

implementation

procedure TAuthenticationSample.OnRequest(const AContext: TWebContext;
  const ControllerQualifiedClassName: string; const ActionName: string;
  var AuthenticationRequired: Boolean);
begin
  // Only the protected controller requires a valid access token. Login and the
  // refresh endpoint (handled by the refresh middleware) stay public.
  AuthenticationRequired := ControllerQualifiedClassName = 'AuthControllerU.TProtectedController';
end;

procedure TAuthenticationSample.OnAuthentication(const AContext: TWebContext;
  const UserName: string; const Password: string; UserRoles: TList<string>;
  var IsValid: Boolean; const SessionData: TSessionData);
begin
  // Reached only by the JWT middleware auto-login segment, which this sample
  // does not use (login is done by TAuthController). Kept for completeness.
  IsValid := UserName.Equals(Password);
end;

procedure TAuthenticationSample.OnAuthorization(const AContext: TWebContext;
  UserRoles: TList<string>; const ControllerQualifiedClassName: string;
  const ActionName: string; var IsAuthorized: Boolean);
begin
  IsAuthorized := True; // any authenticated user is authorized in this demo
end;

end.
