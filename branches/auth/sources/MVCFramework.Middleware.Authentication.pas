unit MVCFramework.Middleware.Authentication;

interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.Logger, System.Generics.Collections;

type
  TMVCBasicAuthenticationMiddleware = class(TInterfacedObject, IMVCMiddleware)
  strict private
    FMVCAuthenticationHandler: IMVCAuthenticationHandler;
  protected
    FRealm: string;
    procedure OnBeforeRouting(Context: TWebContext; var Handled: Boolean);
    procedure OnAfterControllerAction(Context: TWebContext; const AActionName: string;
      const Handled: Boolean);
    procedure OnBeforeControllerAction(Context: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string; var Handled: Boolean);
  public
    constructor Create(AMVCAuthenticationHandler: IMVCAuthenticationHandler;
      Realm: string = 'DelphiMVCFramework REALM'); virtual;
  end;

implementation

uses
  System.SysUtils, Soap.EncdDecd, MVCFramework.Session;

{

  401 Unauthorized response should be used for missing or bad authentication, and a
  403 Forbidden response should be used afterwards, when the user is authenticated
  but isn’t authorized to perform the requested operation on the given resource.

}

const
  CONTENT_HTML_FORMAT = '<html><body><h1>%s</h1></body></html>';
  CONTENT_401_NOT_AUTHORIZED = '401: Not authorized';
  CONTENT_403_FORBIDDEN = '403: Forbidden';

  { TMVCSalutationMiddleware }

constructor TMVCBasicAuthenticationMiddleware.Create(AMVCAuthenticationHandler
  : IMVCAuthenticationHandler; Realm: string);
begin
  inherited Create;
  FMVCAuthenticationHandler := AMVCAuthenticationHandler;
  FRealm := Realm;
end;

procedure TMVCBasicAuthenticationMiddleware.OnAfterControllerAction(Context: TWebContext;
  const AActionName: string; const Handled: Boolean);
begin

end;

procedure TMVCBasicAuthenticationMiddleware.OnBeforeControllerAction(Context: TWebContext;
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
  LSessionIsNeeded: Boolean;
  LAuthRequired: Boolean;
  procedure SendWWWAuthenticate;
  begin
    Context.LoggedUser.Clear;
    if Context.Request.ClientPreferHTML then
    begin
      Context.Response.ContentType := 'text/html';
      Context.Response.RawWebResponse.Content :=
        Format(CONTENT_HTML_FORMAT, [CONTENT_401_NOT_AUTHORIZED]);
    end
    else
    begin
      Context.Response.ContentType := 'text/plain';
      Context.Response.RawWebResponse.Content := CONTENT_401_NOT_AUTHORIZED;
    end;
    Context.Response.StatusCode := 401;
    Context.Response.SetCustomHeader('WWW-Authenticate', 'Basic realm=' + FRealm.QuotedString);

    Handled := true;
  end;

  procedure Send403Forbidden;
  begin
    Context.LoggedUser.Clear;
    if Context.Request.ClientPreferHTML then
    begin
      Context.Response.ContentType := 'text/html';
      Context.Response.RawWebResponse.Content :=
        Format(CONTENT_HTML_FORMAT, [CONTENT_403_FORBIDDEN]);
    end
    else
    begin
      Context.Response.ContentType := 'text/plain';
      Context.Response.RawWebResponse.Content := CONTENT_403_FORBIDDEN;
    end;
    Context.Response.StatusCode := 403;
    Handled := true;
  end;

begin
  // check if the resource is protected
  FMVCAuthenticationHandler.OnRequest(AControllerQualifiedClassName, AActionName, LAuthRequired);
  if not LAuthRequired then
  begin
    Handled := False;
    Exit;
  end;

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
  if not Context.LoggedUser.IsValid then
  begin
    // check if the resource is protected
    // FMVCAuthenticationHandler.OnRequest(AControllerQualifiedClassName, AActionName, LAuthRequired);
    // if not LAuthRequired then
    // begin
    // Handled := False;
    // Exit;
    // end;

    // we NEED authentication
    LAuth := Context.Request.Headers['Authorization'];
    LAuth := DecodeString(LAuth.Remove(0, 'Basic'.Length).Trim);
    LPieces := LAuth.Split([':']);
    if LAuth.IsEmpty or (Length(LPieces) <> 2) then
    begin
      SendWWWAuthenticate;
      Exit;
    end;

    // now, we have username and password.
    // check the authorization for the requested resource
    LRoles := TList<string>.Create;
    try
      FMVCAuthenticationHandler.OnAuthentication(LPieces[0], LPieces[1], LRoles, LIsValid);
      if LIsValid then
      begin
        Context.LoggedUser.Roles.AddRange(LRoles);
        Context.LoggedUser.UserName := LPieces[0];
        Context.LoggedUser.LoggedSince := Now;
        Context.LoggedUser.Realm := FRealm;
        LSessionID := TMVCEngine.SendSessionCookie(Context);
        LWebSession := TMVCEngine.AddSessionToTheSessionList(LSessionID,
          Context.Config.AsInt64[TMVCConfigKey.SessionTimeout]);
        // LWebSession[TMVCConstants.LAST_AUTHORIZATION_HEADER_VALUE] := Context.Request.Headers
        // ['Authorization'];
        Context.LoggedUser.SaveToSession(LWebSession);
      end;
    finally
      LRoles.Free;
    end;
  end;

  // authorization
  LIsAuthorized := False;
  if LIsValid then
  begin
    FMVCAuthenticationHandler.OnAuthorization(Context.LoggedUser.Roles,
      AControllerQualifiedClassName, AActionName, LIsAuthorized)
  end;

  if LIsAuthorized then
    Handled := False
  else
  begin
    if LIsValid then
      Send403Forbidden
    else
      SendWWWAuthenticate;
  end;
end;

procedure TMVCBasicAuthenticationMiddleware.OnBeforeRouting(Context: TWebContext;
  var Handled: Boolean);
begin

end;

end.
