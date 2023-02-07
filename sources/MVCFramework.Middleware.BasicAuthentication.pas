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

unit MVCFramework.Middleware.BasicAuthentication;

{$I dmvcframework.inc}

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Serializer.Commons;

type
  TMVCBasicAuthenticationMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    FAuthenticationHandler: IMVCAuthenticationHandler;
    FRealm: string;

    procedure OnBeforeRouting(
      AContext: TWebContext;
      var AHandled: Boolean
      );

    procedure OnBeforeControllerAction(
      AContext: TWebContext;
      const AControllerQualifiedClassName: string;
      const AActionName: string;
      var AHandled: Boolean
      );

    procedure OnAfterControllerAction(
      AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      const AHandled: Boolean
      );
    procedure OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
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

  { TMVCBasicAuthenticationMiddleware }

constructor TMVCBasicAuthenticationMiddleware.Create(
  const AAuthenticationHandler: IMVCAuthenticationHandler;
  const ARealm: string);
begin
  inherited Create;
  FAuthenticationHandler := AAuthenticationHandler;
  FRealm := ARealm;
end;

procedure TMVCBasicAuthenticationMiddleware.OnAfterControllerAction(
      AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      const AHandled: Boolean
      );
begin
  // Implement as needed
end;

procedure TMVCBasicAuthenticationMiddleware.OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
begin

end;

procedure TMVCBasicAuthenticationMiddleware.OnBeforeControllerAction(
  AContext: TWebContext;
  const AControllerQualifiedClassName, AActionName: string;
  var AHandled: Boolean);

  procedure SendWWWAuthenticate;
  begin
    AContext.LoggedUser.Clear;
    if AContext.Request.ClientPreferHTML then
    begin
      AContext.Response.ContentType := TMVCMediaType.TEXT_HTML;
      AContext.Response.RawWebResponse.Content :=
        Format(CONTENT_HTML_FORMAT, [CONTENT_401_NOT_AUTHORIZED, AContext.Config[TMVCConfigKey.ServerName]]);
    end
    else
    begin
      AContext.Response.ContentType := TMVCMediaType.TEXT_PLAIN;
      AContext.Response.RawWebResponse.Content := CONTENT_401_NOT_AUTHORIZED + sLineBreak + AContext.Config
        [TMVCConfigKey.ServerName];
    end;
    AContext.Response.StatusCode := HTTP_STATUS.Unauthorized;
    AContext.Response.SetCustomHeader('WWW-Authenticate', 'Basic realm=' + QuotedStr(FRealm));
    AContext.SessionStop(False);
    AHandled := True;
  end;

  procedure Send403Forbidden;
  begin
    AContext.LoggedUser.Clear;
    if AContext.Request.ClientPreferHTML then
    begin
      AContext.Response.ContentType := TMVCMediaType.TEXT_HTML;
      AContext.Response.RawWebResponse.Content :=
        Format(CONTENT_HTML_FORMAT, [CONTENT_403_FORBIDDEN, AContext.Config[TMVCConfigKey.ServerName]]);
    end
    else if AContext.Request.ContentMediaType.StartsWith(TMVCMediaType.APPLICATION_JSON) then
    begin
      AContext.Response.ContentType := TMVCMediaType.APPLICATION_JSON;
      AContext.Response.RawWebResponse.Content :=
        '{"status":"error", "message":"' + CONTENT_403_FORBIDDEN.Replace('"', '\"') + '"}';
    end
    else
    begin
      AContext.Response.ContentType := TMVCMediaType.TEXT_PLAIN;
      AContext.Response.RawWebResponse.Content := CONTENT_403_FORBIDDEN + sLineBreak + AContext.Config
        [TMVCConfigKey.ServerName];
    end;
    AContext.Response.StatusCode := HTTP_STATUS.Forbidden;
    AContext.Response.ReasonString := AContext.Config[TMVCConfigKey.ServerName];
    AHandled := True;
  end;

var
  AuthRequired: Boolean;
  IsValid, IsAuthorized: Boolean;
  AuthHeader, Token: string;
  AuthPieces: TArray<string>;
  RolesList: TList<string>;
  SessionData: TSessionData;
  SessionPair: TPair<string, string>;
begin
  FAuthenticationHandler.OnRequest(AContext, AControllerQualifiedClassName, AActionName, AuthRequired);
  if not AuthRequired then
  begin
    AHandled := False;
    Exit;
  end;

  AContext.LoggedUser.LoadFromSession(AContext.Session);
  IsValid := AContext.LoggedUser.IsValid;
  if not IsValid then
  begin
    AuthHeader := AContext.Request.Headers['Authorization'];
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
        FAuthenticationHandler.OnAuthentication(AContext, AuthPieces[0], AuthPieces[1], RolesList, IsValid,
          SessionData);
        if IsValid then
        begin
          AContext.LoggedUser.Roles.AddRange(RolesList);
          AContext.LoggedUser.UserName := AuthPieces[0];
          AContext.LoggedUser.LoggedSince := Now;
          AContext.LoggedUser.Realm := FRealm;
          AContext.LoggedUser.SaveToSession(AContext.Session);
          for SessionPair in SessionData do
            AContext.Session[SessionPair.Key] := SessionPair.Value;
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
    FAuthenticationHandler.OnAuthorization(AContext, AContext.LoggedUser.Roles, AControllerQualifiedClassName,
      AActionName, IsAuthorized);

  if IsAuthorized then
    AHandled := False
  else
  begin
    if IsValid then
      Send403Forbidden
    else
    begin
      SendWWWAuthenticate;
    end;
  end;
end;

procedure TMVCBasicAuthenticationMiddleware.OnBeforeRouting(
  AContext: TWebContext;
  var AHandled: Boolean);
begin
  AHandled := False;
end;

end.
