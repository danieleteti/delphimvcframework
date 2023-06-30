// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.Filters.Authentication;

{$I dmvcframework.inc}

interface

uses
  System.SysUtils,
  System.StrUtils,
  System.Generics.Collections,
  System.JSON,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Serializer.Commons;

type

  TMVCBasicAuthenticationControllerFilter = class(TCustomControllerFilter)
  private
    FAuthenticationHandler: IMVCAuthenticationHandler;
    FRealm: string;
  protected
    procedure DoFilter(const Context: TWebContext; const Router: IMVCRouter); override;
  public
    constructor Create(
      const AAuthenticationHandler: IMVCAuthenticationHandler;
      const ARealm: string = 'DelphiMVCFramework REALM'
      ); virtual;
  end;

implementation

const
  CONTENT_HTML_FORMAT = '<html><body><h1>%s</h1><p>%s</p></body></html>';
  CONTENT_401_NOT_AUTHORIZED = '401: Not authorized';
  CONTENT_403_FORBIDDEN = '403: Forbidden';

  { TMVCBasicAuthenticationControllerFilter }

constructor TMVCBasicAuthenticationControllerFilter.Create(
  const AAuthenticationHandler: IMVCAuthenticationHandler;
  const ARealm: string);
begin
  inherited Create;
  FAuthenticationHandler := AAuthenticationHandler;
  FRealm := ARealm;
end;

procedure TMVCBasicAuthenticationControllerFilter.DoFilter(
  const Context: TWebContext; const Router: IMVCRouter);

var
  AuthRequired: Boolean;
  IsValid, IsAuthorized: Boolean;
  AuthHeader, Token: string;
  AuthPieces: TArray<string>;
  RolesList: TList<string>;
  SessionData: TSessionData;
  SessionPair: TPair<string, string>;
  lHandled: Boolean;

  procedure SendWWWAuthenticate;
  begin
    Context.LoggedUser.Clear;
    if Context.Request.ClientAcceptHTML then
    begin
      Context.Response.ContentType := TMVCMediaType.TEXT_HTML;
      Context.Response.RawWebResponse.Content :=
        Format(CONTENT_HTML_FORMAT, [CONTENT_401_NOT_AUTHORIZED, Context.Config[TMVCConfigKey.ServerName]]);
    end
    else
    begin
      Context.Response.ContentType := TMVCMediaType.TEXT_PLAIN;
      Context.Response.RawWebResponse.Content := CONTENT_401_NOT_AUTHORIZED + sLineBreak + Context.Config
        [TMVCConfigKey.ServerName];
    end;
    Context.Response.StatusCode := HTTP_STATUS.Unauthorized;
    Context.Response.SetCustomHeader('WWW-Authenticate', 'Basic realm=' + QuotedStr(FRealm));
    Context.SessionStop(False);
    lHandled := True;
  end;

  procedure Send403Forbidden;
  begin
    Context.LoggedUser.Clear;
    if Context.Request.ClientAcceptHTML then
    begin
      Context.Response.ContentType := TMVCMediaType.TEXT_HTML;
      Context.Response.RawWebResponse.Content :=
        Format(CONTENT_HTML_FORMAT, [CONTENT_403_FORBIDDEN, Context.Config[TMVCConfigKey.ServerName]]);
    end
    else if Context.Request.ContentMediaType.StartsWith(TMVCMediaType.APPLICATION_JSON) then
    begin
      Context.Response.ContentType := TMVCMediaType.APPLICATION_JSON;
      Context.Response.RawWebResponse.Content :=
        '{"status":"error", "message":"' + CONTENT_403_FORBIDDEN.Replace('"', '\"') + '"}';
    end
    else
    begin
      Context.Response.ContentType := TMVCMediaType.TEXT_PLAIN;
      Context.Response.RawWebResponse.Content := CONTENT_403_FORBIDDEN + sLineBreak + Context.Config
        [TMVCConfigKey.ServerName];
    end;
    Context.Response.StatusCode := HTTP_STATUS.Forbidden;
    Context.Response.ReasonString := Context.Config[TMVCConfigKey.ServerName];
    lHandled := True;
  end;


begin
  lHandled := False;
  FAuthenticationHandler.OnRequest(
    Context,
    Router.ControllerClazz.QualifiedClassName,
    Router.ActionMethod.Name,
    AuthRequired);
  if not AuthRequired then
  begin
    lHandled := False;
    DoNext(Context, Router);
    Exit;
  end;

  Context.LoggedUser.LoadFromSession(Context.Session);
  IsValid := Context.LoggedUser.IsValid;
  if not IsValid then
  begin
    AuthHeader := Context.Request.Headers['Authorization'];
    if AuthHeader.IsEmpty or (not AuthHeader.StartsWith('Basic ', True)) then
    begin
      SendWWWAuthenticate;
      Exit;
    end;
    Token := AuthHeader.Remove(0, 'Basic '.Length).Trim;
    AuthHeader := TMVCSerializerHelper.DecodeString(Token);
    AuthPieces := AuthHeader.Split([':']);
    if Length(AuthPieces) <> 2 then
    begin
      SendWWWAuthenticate;
      Exit;
    end;

    RolesList := TList<string>.Create;
    try
      SessionData := TSessionData.Create;
      try
        FAuthenticationHandler.OnAuthentication(Context, AuthPieces[0], AuthPieces[1], RolesList, IsValid,
          SessionData);
        if IsValid then
        begin
          Context.LoggedUser.Roles.AddRange(RolesList);
          Context.LoggedUser.UserName := AuthPieces[0];
          Context.LoggedUser.LoggedSince := Now;
          Context.LoggedUser.Realm := FRealm;
          Context.LoggedUser.SaveToSession(Context.Session);
          for SessionPair in SessionData do
            Context.Session[SessionPair.Key] := SessionPair.Value;
        end;
      finally
        SessionData.Free;
      end;
    finally
      RolesList.Free;
    end;
  end;

  IsAuthorized := False;
  if IsValid then
    FAuthenticationHandler.OnAuthorization(
      Context,
      Context.LoggedUser.Roles,
      Router.ControllerClazz.QualifiedClassName,
      Router.ActionMethod.Name,
      IsAuthorized);

  if IsAuthorized then
    lHandled := False
  else
  begin
    if IsValid then
      Send403Forbidden
    else
    begin
      SendWWWAuthenticate;
    end;
  end;

  if not lHandled then
  begin
    DoNext(Context, Router);
  end;
end;

end.
