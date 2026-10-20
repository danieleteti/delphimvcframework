unit AuthHandlerU;

// One handler class, two instances: each middleware asks its handler, through
// OnRequest, whether the request is for it. The Basic middleware takes the
// requests that carry "Authorization: Basic ...", the JWT middleware takes all
// the others (a Bearer token, or none at all, which it refuses with a 401).

interface

uses
  System.Generics.Collections,
  MVCFramework;

type
  TSchemeAuthHandler = class(TInterfacedObject, IMVCAuthenticationHandler)
  private
    fIsBasic: Boolean;
  protected
    procedure OnRequest(const AContext: TWebContext; const AControllerQualifiedClassName: string;
      const AActionName: string; var AAuthenticationRequired: Boolean);
    procedure OnAuthentication(const AContext: TWebContext; const AUserName: string;
      const APassword: string; AUserRoles: TList<string>; var AIsValid: Boolean;
      const ASessionData: TDictionary<string, string>);
    procedure OnAuthorization(const AContext: TWebContext; AUserRoles: TList<string>;
      const AControllerQualifiedClassName: string; const AActionName: string;
      var AIsAuthorized: Boolean);
  public
    constructor Create(const AIsBasic: Boolean);
  end;

implementation

uses
  System.SysUtils;

constructor TSchemeAuthHandler.Create(const AIsBasic: Boolean);
begin
  inherited Create;
  fIsBasic := AIsBasic;
end;

procedure TSchemeAuthHandler.OnRequest(const AContext: TWebContext;
  const AControllerQualifiedClassName: string; const AActionName: string;
  var AAuthenticationRequired: Boolean);
var
  lIsBasicRequest: Boolean;
begin
  lIsBasicRequest := AContext.Request.Headers['Authorization'].StartsWith('Basic ', True);
  AAuthenticationRequired := lIsBasicRequest = fIsBasic;
end;

procedure TSchemeAuthHandler.OnAuthentication(const AContext: TWebContext;
  const AUserName: string; const APassword: string; AUserRoles: TList<string>;
  var AIsValid: Boolean; const ASessionData: TDictionary<string, string>);
begin
  // demo credentials: look the user up in your own store
  AIsValid := (AUserName = 'daniele') and (APassword = 'secret');
  if AIsValid then
    AUserRoles.Add('user');
end;

procedure TSchemeAuthHandler.OnAuthorization(const AContext: TWebContext;
  AUserRoles: TList<string>; const AControllerQualifiedClassName: string;
  const AActionName: string; var AIsAuthorized: Boolean);
begin
  AIsAuthorized := AUserRoles.Contains('user');
end;

end.
