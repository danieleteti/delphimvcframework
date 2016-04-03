{ *************************************************************************** }
{ }
{ Delphi MVC Framework }
{ }
{ Copyright (c) 2010-2015 Daniele Teti and the DMVCFramework Team }
{ }
{ https://github.com/danieleteti/delphimvcframework }
{ }
{ *************************************************************************** }
{ }
{ Licensed under the Apache License, Version 2.0 (the "License"); }
{ you may not use this file except in compliance with the License. }
{ You may obtain a copy of the License at }
{ }
{ http://www.apache.org/licenses/LICENSE-2.0 }
{ }
{ Unless required by applicable law or agreed to in writing, software }
{ distributed under the License is distributed on an "AS IS" BASIS, }
{ WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{ See the License for the specific language governing permissions and }
{ limitations under the License. }
{ }
{ *************************************************************************** }

unit MVCFramework.Middleware.Authentication;

interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.Logger,
  System.Generics.Collections;

type
  TMVCBasicAuthenticationMiddleware = class(TInterfacedObject, IMVCMiddleware)
  strict private
    FMVCAuthenticationHandler: IMVCAuthenticationHandler;
  protected
    FRealm: string;
    procedure OnBeforeRouting(Context: TWebContext; var Handled: Boolean);
    procedure OnAfterControllerAction(Context: TWebContext;
      const AActionName: string; const Handled: Boolean);
    procedure OnBeforeControllerAction(Context: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      var Handled: Boolean);
  public
    constructor Create(AMVCAuthenticationHandler: IMVCAuthenticationHandler;
      Realm: string = 'DelphiMVCFramework REALM'); virtual;
  end;

implementation

uses
  System.SysUtils, MVCFramework.Session
{$IF CompilerVersion >= 21}
    , System.NetEncoding
{$ELSE}
    , Soap.EncdDecd
{$ENDIF};

{

  401 Unauthorized response should be used for missing or bad authentication, and a
  403 Forbidden response should be used afterwards, when the user is authenticated
  but isn’t authorized to perform the requested operation on the given resource.

}

const
  CONTENT_HTML_FORMAT = '<html><body><h1>%s</h1><p>%s</p></body></html>';
  CONTENT_401_NOT_AUTHORIZED = '401: Not authorized';
  CONTENT_403_FORBIDDEN = '403: Forbidden';

function Base64DecodeString(const Value: String): String; inline;
begin
{$IF CompilerVersion >= 21}
  Result := TNetEncoding.Base64.Decode(Value);
{$ELSE}
  Result := DecodeString(Value);
{$ENDIF}
end;

{ TMVCSalutationMiddleware }

constructor TMVCBasicAuthenticationMiddleware.Create(AMVCAuthenticationHandler
  : IMVCAuthenticationHandler; Realm: string);
begin
  inherited Create;
  FMVCAuthenticationHandler := AMVCAuthenticationHandler;
  FRealm := Realm;
end;

procedure TMVCBasicAuthenticationMiddleware.OnAfterControllerAction
  (Context: TWebContext; const AActionName: string; const Handled: Boolean);
begin
  // do nothing
end;

procedure TMVCBasicAuthenticationMiddleware.OnBeforeControllerAction
  (Context: TWebContext; const AControllerQualifiedClassName,
  AActionName: string; var Handled: Boolean);
var
  LAuth: string;
  LPieces: TArray<string>;
  LRoles: TList<string>;
  LIsValid: Boolean;
  LIsAuthorized: Boolean;
  LAuthRequired: Boolean;
  LSessionData: TSessionData;
  LPair: TPair<String, String>;
  procedure SendWWWAuthenticate;
  begin
    Context.LoggedUser.Clear;
    if Context.Request.ClientPreferHTML then
    begin
      Context.Response.ContentType := 'text/html';
      Context.Response.RawWebResponse.Content :=
        Format(CONTENT_HTML_FORMAT, [CONTENT_401_NOT_AUTHORIZED, Context.Config[TMVCConfigKey.ServerName]]);
    end
    else
    begin
      Context.Response.ContentType := 'text/plain';
      Context.Response.RawWebResponse.Content := CONTENT_401_NOT_AUTHORIZED + sLineBreak + Context.Config[TMVCConfigKey.ServerName];
    end;
    Context.Response.StatusCode := 401;
    Context.Response.SetCustomHeader('WWW-Authenticate',
      'Basic realm=' + QuotedStr(FRealm));

    Handled := true;
  end;

  procedure Send403Forbidden;
  begin
    Context.LoggedUser.Clear;
    if Context.Request.ClientPreferHTML then
    begin
      Context.Response.ContentType := 'text/html';
      Context.Response.RawWebResponse.Content :=
        Format(CONTENT_HTML_FORMAT, [CONTENT_403_FORBIDDEN, Context.Config[TMVCConfigKey.ServerName]]);
    end
    else
    begin
      Context.Response.ContentType := 'text/plain';
      Context.Response.RawWebResponse.Content := CONTENT_403_FORBIDDEN + sLineBreak + Context.Config[TMVCConfigKey.ServerName];
    end;
    Context.Response.StatusCode := 403;
    Handled := true;
  end;

begin
  // check if the resource is protected
  FMVCAuthenticationHandler.OnRequest(AControllerQualifiedClassName,
    AActionName, LAuthRequired);
  if not LAuthRequired then
  begin
    Handled := False;
    Exit;
  end;

  Context.LoggedUser.LoadFromSession(Context.Session);
  if not Context.LoggedUser.IsValid then
  begin
    // We NEED authentication
    LAuth := Context.Request.Headers['Authorization'];
    LAuth := Base64DecodeString(LAuth.Remove(0, 'Basic'.Length).Trim);
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
      LSessionData := TSessionData.Create;
      try
        FMVCAuthenticationHandler.OnAuthentication(LPieces[0], LPieces[1],
          LRoles, LIsValid, LSessionData);
        if LIsValid then
        begin
          Context.LoggedUser.Roles.AddRange(LRoles);
          Context.LoggedUser.UserName := LPieces[0];
          Context.LoggedUser.LoggedSince := Now;
          Context.LoggedUser.Realm := FRealm;
          Context.LoggedUser.SaveToSession(Context.Session);

          // save sessiondata to the actual session
          for LPair in LSessionData do
          begin
            Context.Session[LPair.Key] := LPair.Value;
          end;
        end;
      finally
        LSessionData.Free;
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

procedure TMVCBasicAuthenticationMiddleware.OnBeforeRouting
  (Context: TWebContext; var Handled: Boolean);
begin
  // do nothing
end;

end.
