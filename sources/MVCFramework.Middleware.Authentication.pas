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

{$I dmvcframework.inc}

interface

uses
  System.SysUtils,
  System.StrUtils,
  System.Generics.Collections,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Serializer.Commons,
  MVCFramework.TypesAliases;

type

  TMVCBasicAuthenticationMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    FAuthenticationHandler: IMVCAuthenticationHandler;
    FRealm: string;
  protected
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
      const AActionName: string;
      const AHandled: Boolean
      );
  public
    constructor Create(
      const AAuthenticationHandler: IMVCAuthenticationHandler;
      const ARealm: string = 'DelphiMVCFramework REALM'
      ); virtual;
  end;

  TMVCCustomAuthenticationMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    FAuthenticationHandler: IMVCAuthenticationHandler;
    FLoginUrl: string;
  protected
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
      const AActionName: string;
      const AHandled: Boolean
      );

    procedure SendResponse(AContext: TWebContext; var AHandled: Boolean; AHttpStatus: Word = HTTP_STATUS.Unauthorized);
    procedure DoLogin(AContext: TWebContext; var AHandled: Boolean);
    procedure DoLogout(AContext: TWebContext; var AHandled: Boolean);
  public
    constructor Create(
      const AAuthenticationHandler: IMVCAuthenticationHandler;
      const ALoginUrl: string = '/system/users/logged'
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
  const AActionName: string;
  const AHandled: Boolean);
begin
  // Implement as needed
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
      AContext.Response.RawWebResponse.Content := Format(CONTENT_HTML_FORMAT, [CONTENT_401_NOT_AUTHORIZED, AContext.Config[TMVCConfigKey.ServerName]]);
    end
    else
    begin
      AContext.Response.ContentType := TMVCMediaType.TEXT_PLAIN;
      AContext.Response.RawWebResponse.Content := CONTENT_401_NOT_AUTHORIZED + sLineBreak + AContext.Config[TMVCConfigKey.ServerName];
    end;
    AContext.Response.StatusCode := HTTP_STATUS.Unauthorized;
    AContext.Response.SetCustomHeader('WWW-Authenticate', 'Basic realm=' + QuotedStr(FRealm));
    AHandled := True;
  end;

  procedure Send403Forbidden;
  begin
    AContext.LoggedUser.Clear;
    if AContext.Request.ClientPreferHTML then
    begin
      AContext.Response.ContentType := TMVCMediaType.TEXT_HTML;
      AContext.Response.RawWebResponse.Content := Format(CONTENT_HTML_FORMAT, [CONTENT_403_FORBIDDEN, AContext.Config[TMVCConfigKey.ServerName]]);
    end
    else
    begin
      AContext.Response.ContentType := TMVCMediaType.TEXT_PLAIN;
      AContext.Response.RawWebResponse.Content := CONTENT_403_FORBIDDEN + sLineBreak + AContext.Config[TMVCConfigKey.ServerName];
    end;
    AContext.Response.StatusCode := HTTP_STATUS.Forbidden;
    AHandled := True;
  end;

var
  AuthRequired: Boolean;
  IsValid, IsAuthorized: Boolean;
  AuthHeader: string;
  AuthPieces: TArray<string>;
  RolesList: TList<string>;
  SessionData: TSessionData;
  SessionPair: TPair<string, string>;
begin
  FAuthenticationHandler.OnRequest(AControllerQualifiedClassName, AActionName, AuthRequired);
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
    AuthHeader := TMVCSerializerHelpful.DecodeString(AuthHeader.Remove(0, 'Basic'.Length).Trim);
    AuthPieces := AuthHeader.Split([':']);
    if AuthHeader.IsEmpty or (Length(AuthPieces) <> 2) then
    begin
      SendWWWAuthenticate;
      Exit;
    end;

    RolesList := TList<string>.Create;
    try
      SessionData := TSessionData.Create;
      try
        FAuthenticationHandler.OnAuthentication(AuthPieces[0], AuthPieces[1], RolesList, IsValid, SessionData);
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
    FAuthenticationHandler.OnAuthorization(AContext.LoggedUser.Roles, AControllerQualifiedClassName, AActionName, IsAuthorized);

  if IsAuthorized then
    AHandled := False
  else
  begin
    if IsValid then
      Send403Forbidden
    else
      SendWWWAuthenticate;
  end;
end;

procedure TMVCBasicAuthenticationMiddleware.OnBeforeRouting(
  AContext: TWebContext;
  var AHandled: Boolean);
begin
  // Implement as needed
end;

{ TMVCCustomAuthenticationMiddleware }

constructor TMVCCustomAuthenticationMiddleware.Create(
  const AAuthenticationHandler: IMVCAuthenticationHandler;
  const ALoginUrl: string);
begin
  inherited Create;
  FAuthenticationHandler := AAuthenticationHandler;
  FLoginUrl := ALoginUrl.ToLower;
end;

procedure TMVCCustomAuthenticationMiddleware.DoLogin(
  AContext: TWebContext;
  var AHandled: Boolean);
var
  Jo: TJSONObject;
  UserName, Password: string;
  RolesList: TList<string>;
  SessionPair: TPair<string, string>;
  SessionData: TSessionData;
  IsValid: Boolean;
begin
  AContext.SessionStop(False);
  AContext.LoggedUser.Clear;
  if not AContext.Request.ThereIsRequestBody then
  begin
    AHandled := True;
    AContext.Response.StatusCode := HTTP_STATUS.BadRequest;
    AContext.Response.ContentType := TMVCMediaType.APPLICATION_JSON;
    AContext.Response.RawWebResponse.Content := '{"status":"KO", "message":"username and password are mandatory in the body request as json object"}';
    Exit;
  end;

  Jo := TJSONObject.ParseJSONValue(AContext.Request.Body) as TJSONObject;
  try
    if not Assigned(Jo) then
    begin
      AHandled := True;
      SendResponse(AContext, AHandled, HTTP_STATUS.BadRequest);
      Exit;
    end;

    UserName := EmptyStr;
    if (Jo.Get('username') <> nil) then
      UserName := Jo.Get('username').JsonValue.Value;

    Password := EmptyStr;
    if (Jo.Get('password') <> nil) then
      Password := Jo.Get('password').JsonValue.Value;

    if UserName.IsEmpty or Password.IsEmpty then
    begin
      AHandled := True;
      SendResponse(AContext, AHandled);
      Exit;
    end;

    RolesList := TList<string>.Create;
    try
      SessionData := TSessionData.Create;
      try
        IsValid := False;
        FAuthenticationHandler.OnAuthentication(UserName, Password, RolesList, IsValid, SessionData);
        if not IsValid then
        begin
          SendResponse(AContext, AHandled);
          Exit;
        end;

        AContext.LoggedUser.Roles.AddRange(RolesList);
        AContext.LoggedUser.UserName := UserName;
        AContext.LoggedUser.LoggedSince := Now;
        AContext.LoggedUser.Realm := 'custom';
        AContext.LoggedUser.SaveToSession(AContext.Session);

        for SessionPair in SessionData do
          AContext.Session[SessionPair.Key] := SessionPair.Value;

        AContext.Response.StatusCode := HTTP_STATUS.OK;
        AContext.Response.CustomHeaders.Values['X-LOGOUT-URL'] := FLoginUrl;
        AContext.Response.CustomHeaders.Values['X-LOGOUT-METHOD'] := 'DELETE';
        AContext.Response.ContentType := TMVCMediaType.APPLICATION_JSON;
        AContext.Response.RawWebResponse.Content := '{"status":"OK"}';

        AHandled := True;
      finally
        SessionData.Free;
      end;
    finally
      RolesList.Free;
    end;
  finally
    Jo.Free;
  end;
end;

procedure TMVCCustomAuthenticationMiddleware.DoLogout(
  AContext: TWebContext; var AHandled: Boolean);
begin
  AContext.SessionStop(False);
  SendResponse(AContext, AHandled, HTTP_STATUS.OK);
end;

procedure TMVCCustomAuthenticationMiddleware.OnAfterControllerAction(
  AContext: TWebContext; const AActionName: string;
  const AHandled: Boolean);
begin
  // Implement as needed
end;

procedure TMVCCustomAuthenticationMiddleware.OnBeforeControllerAction(
  AContext: TWebContext;
  const AControllerQualifiedClassName, AActionName: string;
  var AHandled: Boolean);
var
  IsValid: Boolean;
  IsAuthorized: Boolean;
  AuthRequired: Boolean;
begin
  FAuthenticationHandler.OnRequest(AControllerQualifiedClassName, AActionName, AuthRequired);
  if not AuthRequired then
  begin
    AHandled := False;
    Exit;
  end;

  AContext.LoggedUser.LoadFromSession(AContext.Session);
  IsValid := AContext.LoggedUser.IsValid;
  if not IsValid then
  begin
    AContext.SessionStop(False);
    SendResponse(AContext, AHandled);
    Exit;
  end;

  IsAuthorized := False;
  FAuthenticationHandler.OnAuthorization(AContext.LoggedUser.Roles, AControllerQualifiedClassName, AActionName, IsAuthorized);
  if IsAuthorized then
    AHandled := False
  else
  begin
    if IsValid then
      SendResponse(AContext, AHandled, HTTP_STATUS.Forbidden)
    else
      SendResponse(AContext, AHandled, HTTP_STATUS.Unauthorized);
  end;
end;

procedure TMVCCustomAuthenticationMiddleware.OnBeforeRouting(
  AContext: TWebContext; var AHandled: Boolean);
begin
  if (AContext.Request.PathInfo.ToLower = FLoginUrl) then
  begin
    AHandled := False;

    if (AContext.Request.HTTPMethod = httpPOST) and (AContext.Request.ContentType.StartsWith(TMVCMediaType.APPLICATION_JSON)) then
      DoLogin(AContext, AHandled);

    if (AContext.Request.HTTPMethod = httpDELETE) then
      DoLogout(AContext, AHandled);
  end;
end;

procedure TMVCCustomAuthenticationMiddleware.SendResponse(
  AContext: TWebContext; var AHandled: Boolean; AHttpStatus: Word);
var
  IsPositive: Boolean;
  Msg: string;
begin
  AContext.LoggedUser.Clear;
  AContext.Response.CustomHeaders.Values['X-LOGIN-URL'] := FLoginUrl;
  AContext.Response.CustomHeaders.Values['X-LOGIN-METHOD'] := 'POST';
  AContext.Response.StatusCode := AHttpStatus;
  if AContext.Request.ClientPreferHTML then
  begin
    AContext.Response.ContentType := TMVCMediaType.TEXT_HTML;
    AContext.Response.RawWebResponse.Content := Format(CONTENT_HTML_FORMAT, [IntToStr(AHttpStatus), AContext.Config[TMVCConfigKey.ServerName]]);
  end
  else
  begin
    IsPositive := (AHttpStatus div 100) = 2;
    Msg := IfThen(IsPositive, 'OK', 'KO');
    AContext.Response.ContentType := TMVCMediaType.APPLICATION_JSON;
    AContext.Response.RawWebResponse.Content := '{"status":"' + Msg + '", "message":"' + IntToStr(AHttpStatus) + '"}';
  end;
  AHandled := True;
end;

end.
