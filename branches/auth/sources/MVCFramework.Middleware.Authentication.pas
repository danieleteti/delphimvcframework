unit MVCFramework.Middleware.Authentication;

interface

uses
  MVCFramework, MVCFramework.Logger, System.Generics.Collections;

type
  TOnAuthenticationEvent = reference to procedure(const AUserName, APassword: string;
    AControllerQualifiedClassName, AActionName: string; AUserRoles: TList<string>;
    var AIsValid: Boolean);
  TOnAuthorizationEvent = reference to procedure(AContext: TWebContext;
    const AControllerQualifiedClassName: string; const AActionName: string;
    var AIsAuthorized: Boolean);

  TMVCAuthenticationMiddleware = class(TInterfacedObject, IMVCMiddleware)
  protected
    FOnAuthentication: TOnAuthenticationEvent;
    FOnAuthorization: TOnAuthorizationEvent;
    procedure OnBeforeRouting(Context: TWebContext; var Handled: Boolean);
    procedure OnAfterControllerAction(Context: TWebContext; const AActionName: string;
      const Handled: Boolean);
    procedure OnBeforeControllerAction(Context: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string; var Handled: Boolean);
  public
    constructor Create(AOnAuthentication: TOnAuthenticationEvent;
      AOnAuthorization: TOnAuthorizationEvent); virtual;
  end;

implementation

uses
  System.SysUtils, Soap.EncdDecd, MVCFramework.Commons, MVCFramework.Session;

{

  401 Unauthorized response should be used for missing or bad authentication, and a
  403 Forbidden response should be used afterwards, when the user is authenticated
  but isn’t authorized to perform the requested operation on the given resource.

}

{ TMVCSalutationMiddleware }

constructor TMVCAuthenticationMiddleware.Create(AOnAuthentication: TOnAuthenticationEvent;
  AOnAuthorization: TOnAuthorizationEvent);
begin
  inherited Create;
  FOnAuthentication := AOnAuthentication;
  FOnAuthorization := AOnAuthorization;
end;

procedure TMVCAuthenticationMiddleware.OnAfterControllerAction(Context: TWebContext;
  const AActionName: string; const Handled: Boolean);
begin

end;

procedure TMVCAuthenticationMiddleware.OnBeforeControllerAction(Context: TWebContext;
  const AControllerQualifiedClassName, AActionName: string; var Handled: Boolean);
var
  LAuth: string;
  LPieces: TArray<string>;
  LRoles: TList<string>;
  LIsValid: Boolean;
  LWebSession: TWebSession;
  LSessionID: string;
  LIsAuthorized: Boolean;
  LSessionIDFromWebRequest: string;
  procedure SendWWWAuthenticate;
  begin
    Context.LoggedUser.Clear;
    Context.Response.StatusCode := 401;
    Context.Response.SetCustomHeader('WWW-Authenticate', 'Basic realm="DMVCFramework TEST"');
    Handled := true;
  end;

  procedure Send403;
  begin
    Context.LoggedUser.Clear;
    Context.Response.StatusCode := 403;
    Handled := true;
  end;

begin
  LSessionIDFromWebRequest := TMVCEngine.ExtractSessionIDFromWebRequest
    (Context.Request.RawWebRequest);
  LWebSession := TMVCEngine.GetCurrentSession(Context.Config.AsInt64[TMVCConfigKey.SessionTimeout],
    LSessionIDFromWebRequest, False);

  if (not LSessionIDFromWebRequest.IsEmpty) and (not Assigned(LWebSession)) then
  begin
    // The sessionid is present but is not valid and there is an authentication header.
    // In this case, an exception is raised because the sessionid is not valid
    raise EMVCSessionExpiredException.Create('Session expired');
  end;

  Context.LoggedUser.LoadFromSession(LWebSession);
  if (not Context.LoggedUser.IsValid) or (LWebSession[TMVCConstants.LAST_AUTHORIZATION_HEADER_VALUE]
    <> Context.Request.Headers['Authorization']) then
  begin
    LAuth := Context.Request.Headers['Authorization'];
    LAuth := DecodeString(LAuth.Remove(0, 'Basic'.Length).Trim);
    LPieces := LAuth.Split([':']);
    if (not LAuth.IsEmpty) and (Length(LPieces) <> 2) then
      raise EMVCException.Create('Invalid Basic Authentication', '', 400);
    if Assigned(FOnAuthentication) then
    begin
      LRoles := TList<string>.Create;
      try
        if Length(LPieces) = 0 then
        begin
          SetLength(LPieces, 2);
          LPieces[0] := '';
          LPieces[1] := '';
        end;

        FOnAuthentication(LPieces[0], LPieces[1], AControllerQualifiedClassName, AActionName,
          LRoles, LIsValid);
        if LIsValid then
        begin
          Context.LoggedUser.Roles.AddRange(LRoles);
          Context.LoggedUser.UserName := LPieces[0];
          Context.LoggedUser.LoggedSince := Now;
          LSessionID := TMVCEngine.SendSessionCookie(Context);
          LWebSession := TMVCEngine.AddSessionToTheSessionList(LSessionID,
            Context.Config.AsInt64[TMVCConfigKey.SessionTimeout]);
          LWebSession[TMVCConstants.LAST_AUTHORIZATION_HEADER_VALUE] := Context.Request.Headers
            ['Authorization'];
          Context.LoggedUser.SaveToSession(LWebSession);
          if Assigned(FOnAuthorization) then
            FOnAuthorization(Context, AControllerQualifiedClassName, AActionName, LIsAuthorized)
          else
            raise EMVCException.Create('OnAuthorization event not set');
          if LIsAuthorized then
            Handled := False
          else
          begin
            Send403;
          end;
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
end;

procedure TMVCAuthenticationMiddleware.OnBeforeRouting(Context: TWebContext; var Handled: Boolean);
begin

end;

end.
