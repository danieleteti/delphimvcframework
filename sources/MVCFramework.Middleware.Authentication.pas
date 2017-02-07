// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2017 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// *************************************************************************** }

unit MVCFramework.Middleware.Authentication;

interface

{$I dmvcframework.inc}


uses
  MVCFramework, MVCFramework.Logger,
  System.Generics.Collections, MVCFramework.Commons;

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

  TMVCCustomAuthenticationMiddleware = class(TInterfacedObject, IMVCMiddleware)
  strict private
    FMVCAuthenticationHandler: IMVCAuthenticationHandler;
  private
    FLoginUrl: string;
    procedure SendResponse(const Context: TWebContext; var Handled: Boolean;
      HTTPStatus: Word = HTTP_STATUS.Unauthorized);
  protected
    procedure OnBeforeRouting(Context: TWebContext; var Handled: Boolean);
    procedure OnAfterControllerAction(Context: TWebContext;
      const AActionName: string; const Handled: Boolean);
    procedure OnBeforeControllerAction(Context: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      var Handled: Boolean);
    procedure DoLogin(Context: TWebContext; var Handled: Boolean);
    procedure DoLogout(Context: TWebContext; var Handled: Boolean);
  public
    constructor Create(
      AMVCAuthenticationHandler: IMVCAuthenticationHandler;
      aLoginUrl: string = '/system/users/logged'
      ); virtual;
  end;

implementation

uses
  System.SysUtils, MVCFramework.Session, ObjectsMappers, System.StrUtils, System.Classes
{$IFDEF SYSTEMNETENCODING}
    , System.NetEncoding
{$ELSE}
    , Soap.EncdDecd
{$ENDIF}
{$IFDEF SYSTEMJSON}
    , System.JSON
{$ELSE}
    , Data.DBXJSON
{$ENDIF}
    ;

{

  401 Unauthorized response should be used for missing or bad authentication, and a
  403 Forbidden response should be used afterwards, when the user is authenticated
  but isn’t authorized to perform the requested operation on the given resource.

}

const
  CONTENT_HTML_FORMAT = '<html><body><h1>%s</h1><p>%s</p></body></html>';
  CONTENT_401_NOT_AUTHORIZED = '401: Not authorized';
  CONTENT_403_FORBIDDEN = '403: Forbidden';

function Base64DecodeString(const Value: string): string; inline;
begin
{$IFDEF SYSTEMNETENCODING}
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
  LPair: TPair<string, string>;
  procedure SendWWWAuthenticate;
  begin
    Context.LoggedUser.Clear;
    if Context.Request.ClientPreferHTML then
    begin
      Context.Response.ContentType := 'text/html';
      Context.Response.RawWebResponse.Content :=
        Format(CONTENT_HTML_FORMAT, [CONTENT_401_NOT_AUTHORIZED,
        Context.Config[TMVCConfigKey.ServerName]]);
    end
    else
    begin
      Context.Response.ContentType := 'text/plain';
      Context.Response.RawWebResponse.Content := CONTENT_401_NOT_AUTHORIZED + sLineBreak +
        Context.Config[TMVCConfigKey.ServerName];
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
        Format(CONTENT_HTML_FORMAT, [CONTENT_403_FORBIDDEN,
        Context.Config[TMVCConfigKey.ServerName]]);
    end
    else
    begin
      Context.Response.ContentType := 'text/plain';
      Context.Response.RawWebResponse.Content := CONTENT_403_FORBIDDEN + sLineBreak + Context.Config
        [TMVCConfigKey.ServerName];
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
  LIsValid := Context.LoggedUser.IsValid;
  if not LIsValid then
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

{ TMVCFormAuthenticationMiddleware }

constructor TMVCCustomAuthenticationMiddleware.Create(
  AMVCAuthenticationHandler: IMVCAuthenticationHandler;
  aLoginUrl: string = '/system/users/logged');
begin
  inherited Create;
  FMVCAuthenticationHandler := AMVCAuthenticationHandler;
  FLoginUrl := aLoginUrl.ToLower;
end;

procedure TMVCCustomAuthenticationMiddleware.DoLogin(Context: TWebContext;
  var Handled: Boolean);
var
  lJObj: TJSONObject;
  lUserName: string;
  lPassword: string;
  LRoles: TList<string>;
  LPair: TPair<string, string>;
  LSessionData: TSessionData;
  LIsValid: Boolean;
begin
  Context.SessionStop(False);
  Context.LoggedUser.Clear;
  if not Context.Request.ThereIsRequestBody then
  begin
    Handled := true;
    Context.Response.StatusCode := HTTP_STATUS.BadRequest;
    Context.Response.ContentType := TMVCMediaType.APPLICATION_JSON;
    Context.Response.RawWebResponse.Content :=
      '{"status":"KO", "message":"username and password are mandatory in the body request as json object"}';
    Exit;
  end;

  lJObj := Context.Request.BodyAsJSONObject;
  if not Assigned(lJObj) then
  begin
    Handled := true;
    SendResponse(Context, Handled, HTTP_STATUS.BadRequest);
    Exit;
  end;

  lUserName := Mapper.GetStringDef(lJObj, 'username', '');
  lPassword := Mapper.GetStringDef(lJObj, 'password', '');

  if lUserName.IsEmpty or lPassword.IsEmpty then
  begin
    Handled := true;
    SendResponse(Context, Handled);
    Exit;
  end;

  // now, we have username and password.
  // check the authorization for the requested resource

  LRoles := TList<string>.Create;
  try
    LSessionData := TSessionData.Create;
    try
      LIsValid := False;
      FMVCAuthenticationHandler.OnAuthentication(lUserName, lPassword,
        LRoles, LIsValid, LSessionData);
      if not LIsValid then
      begin
        SendResponse(Context, Handled);
        Exit;
      end;

      // create the session
      Context.LoggedUser.Roles.AddRange(LRoles);
      Context.LoggedUser.UserName := lUserName;
      Context.LoggedUser.LoggedSince := Now;
      Context.LoggedUser.Realm := 'custom';
      Context.LoggedUser.SaveToSession(Context.Session);

      // save sessiondata to the actual session
      for LPair in LSessionData do
      begin
        Context.Session[LPair.Key] := LPair.Value;
      end;

      Context.Response.StatusCode := HTTP_STATUS.OK;
      Context.Response.CustomHeaders.Values['X-LOGOUT-URL'] := FLoginUrl;
      Context.Response.CustomHeaders.Values['X-LOGOUT-METHOD'] := 'DELETE';
      Context.Response.ContentType := TMVCMediaType.APPLICATION_JSON;
      Context.Response.RawWebResponse.Content := '{"status":"OK"}';
      Handled := true;
    finally
      LSessionData.Free;
    end;
  finally
    LRoles.Free;
  end;
end;

procedure TMVCCustomAuthenticationMiddleware.DoLogout(Context: TWebContext;
  var Handled: Boolean);
begin
  Context.SessionStop(False);
  SendResponse(Context, Handled, HTTP_STATUS.OK);
end;

procedure TMVCCustomAuthenticationMiddleware.OnAfterControllerAction(
  Context: TWebContext; const AActionName: string; const Handled: Boolean);
begin
  // do nothing
end;

procedure TMVCCustomAuthenticationMiddleware.SendResponse(const Context: TWebContext;
  var Handled: Boolean; HTTPStatus: Word);
var
  lIsPositive: Boolean;
  lMsg: string;
begin
  Context.LoggedUser.Clear;
  Context.Response.CustomHeaders.Values['X-LOGIN-URL'] := FLoginUrl;
  Context.Response.CustomHeaders.Values['X-LOGIN-METHOD'] := 'POST';
  Context.Response.StatusCode := HTTPStatus;
  if Context.Request.ClientPreferHTML then
  begin
    Context.Response.ContentType := 'text/html';
    Context.Response.RawWebResponse.Content :=
      Format(CONTENT_HTML_FORMAT, [IntToStr(HTTPStatus),
      Context.Config[TMVCConfigKey.ServerName]]);
  end
  else
  begin
    lIsPositive := (HTTPStatus div 100) = 2;
    lMsg := ifthen(lIsPositive, 'OK', 'KO');
    Context.Response.ContentType := 'application/json';
    Context.Response.RawWebResponse.Content :=
      '{"status":"' + lMsg + '", "message":"' + IntToStr(HTTPStatus) + '"}';
  end;
  Handled := true;
end;

procedure TMVCCustomAuthenticationMiddleware.OnBeforeControllerAction(
  Context: TWebContext; const AControllerQualifiedClassName,
  AActionName: string; var Handled: Boolean);
var
  LIsValid: Boolean;
  LIsAuthorized: Boolean;
  LAuthRequired: Boolean;
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
  LIsValid := Context.LoggedUser.IsValid;
  if not LIsValid then
  begin
    Context.SessionStop(False);
    SendResponse(Context, Handled);
    Exit;
  end;

  // authorization
  LIsAuthorized := False;
  FMVCAuthenticationHandler.OnAuthorization(Context.LoggedUser.Roles,
    AControllerQualifiedClassName, AActionName, LIsAuthorized);

  if LIsAuthorized then
    Handled := False
  else
  begin
    if LIsValid then
      SendResponse(Context, Handled, HTTP_STATUS.Forbidden)
    else
      SendResponse(Context, Handled, HTTP_STATUS.Unauthorized);
  end;
end;

procedure TMVCCustomAuthenticationMiddleware.OnBeforeRouting(Context: TWebContext;
  var Handled: Boolean);
begin
  if (Context.Request.PathInfo.ToLower = FLoginUrl) then
  begin
    Handled := False;
    if (Context.Request.HTTPMethod = httpPOST)
      and (Context.Request.ContentType.StartsWith(TMVCMediaType.APPLICATION_JSON))
    then
    begin
      DoLogin(Context, Handled);
    end;
    if Context.Request.HTTPMethod = httpDELETE then
    begin
      DoLogout(Context, Handled);
    end;
  end;

  {
    Context.LoggedUser.LoadFromSession(Context.Session);
    if not Context.LoggedUser.IsValid then
    begin
    Send401Unauthorized;
    Exit;
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


  }
end;

end.
