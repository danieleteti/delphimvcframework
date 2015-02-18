unit MVCFramework.Middleware.Authentication;

interface

uses
  MVCFramework, MVCFramework.Logger, System.Generics.Collections;

type
  TOnAuthenticationEvent = reference to procedure(const AUserName, APassword: string;
    const AControllerQualifiedClassName, AActionNAme: string; AUserRoles: TList<string>;
    var AIsValid: Boolean);
  TOnAuthorizationEvent = reference to procedure(const AControllerQualifiedClassName: string;
    const AActionNAme: string; AUserRoles: TList<string>; var AIsAuthorized: Boolean);

  TMVCAuthenticationMiddleware = class(TInterfacedObject, IMVCMiddleware)
  protected
    FOnAuthentication: TOnAuthenticationEvent;
    FOnAuthorization: TOnAuthorizationEvent;
    procedure OnBeforeRouting(Context: TWebContext; var Handled: Boolean);
    procedure OnAfterControllerAction(Context: TWebContext; const AActionNAme: string;
      const Handled: Boolean);
    procedure OnBeforeControllerAction(Context: TWebContext;
      const AControllerQualifiedClassName: string; const AActionNAme: string; var Handled: Boolean);
  public
    constructor Create(AOnAuthentication: TOnAuthenticationEvent;
      AOnAuthorization: TOnAuthorizationEvent); virtual;
  end;

implementation

uses
  System.SysUtils, Soap.EncdDecd, MVCFramework.Commons, MVCFramework.Session;

{ TMVCSalutationMiddleware }

constructor TMVCAuthenticationMiddleware.Create(AOnAuthentication: TOnAuthenticationEvent;
  AOnAuthorization: TOnAuthorizationEvent);
begin
  inherited Create;
  FOnAuthentication := AOnAuthentication;
  FOnAuthorization := AOnAuthorization;
end;

procedure TMVCAuthenticationMiddleware.OnAfterControllerAction(Context: TWebContext;
  const AActionNAme: string; const Handled: Boolean);
begin

end;

procedure TMVCAuthenticationMiddleware.OnBeforeControllerAction(Context: TWebContext;
  const AControllerQualifiedClassName, AActionNAme: string; var Handled: Boolean);
var
  LAuth: string;
  LPieces: TArray<string>;
  LRoles: TList<string>;
  LIsValid: Boolean;
  LWebSession: TWebSession;
  LSessionID: string;
  procedure SendWWWAuthenticate;
  begin
    Context.LoggedUser.Clear;
    Context.Response.StatusCode := 401;
    Context.Response.SetCustomHeader('WWW-Authenticate', 'Basic realm="DMVCFramework TEST"');
    Handled := true;
  end;

begin
  if not Context.Request.Headers['Authorization'].IsEmpty then
  begin
    LWebSession := TMVCEngine.GetCurrentSession
      (Context.Config.AsInt64[TMVCConfigKey.SessionTimeout],
      TMVCEngine.ExtractSessionIDFromWebRequest(Context.Request.RawWebRequest), False);
    Context.LoggedUser.LoadFromSession(LWebSession);
    if not Context.LoggedUser.IsValid then
    begin
      LAuth := Context.Request.Headers['Authorization'];
      LAuth := DecodeString(LAuth.Remove(0, 'Basic'.Length).Trim);
      LPieces := LAuth.Split([':']);
      if Length(LPieces) <> 2 then
        raise EMVCException.Create('Invalid Basic Authentication', '', 400);
      if Assigned(FOnAuthentication) then
      begin
        LRoles := TList<string>.Create;
        try
          FOnAuthentication(LPieces[0], LPieces[1], AControllerQualifiedClassName, AActionNAme,
            LRoles, LIsValid);
          if LIsValid then
          begin
            Context.LoggedUser.Roles.AddRange(LRoles);
            Context.LoggedUser.UserName := LPieces[0];
            Context.LoggedUser.LoggedSince := Now;
            LSessionID := TMVCEngine.SendSessionCookie(Context);
            LWebSession := TMVCEngine.AddSessionToTheSessionList(LSessionID,
              Context.Config.AsInt64[TMVCConfigKey.SessionTimeout]);
            Context.LoggedUser.SaveToSession(LWebSession);
            Handled := False;
          end
          else
            SendWWWAuthenticate;
        finally
          LRoles.Free;
        end;
      end;
    end
    else
    begin
      Handled := False;
    end;
  end
  else
    SendWWWAuthenticate;
end;

procedure TMVCAuthenticationMiddleware.OnBeforeRouting(Context: TWebContext; var Handled: Boolean);
begin

end;

end.
