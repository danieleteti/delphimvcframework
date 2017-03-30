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

unit MVCFramework.Middleware.JWT;

{$I dmvcframework.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.JWT,
  MVCFramework.TypesAliases;

type

  TJWTClaimsSetup = reference to procedure(const JWT: TJWT);

  TMVCJWTAuthenticationMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    FAuthenticationHandler: IMVCAuthenticationHandler;
    FClaimsToChecks: TJWTCheckableClaims;
    FSetupJWTClaims: TJWTClaimsSetup;
    FSecret: string;
  protected
    procedure InternalRender(
      AJSONValue: TJSONValue;
      AContentType: string;
      AContentEncoding: string;
      AContext: TWebContext;
      AInstanceOwner: Boolean = True
      );

    procedure RenderError(
      const AErrorCode: UInt16;
      const AErrorMessage: string;
      const AContext: TWebContext;
      const AErrorClassName: string = ''
      );

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
    constructor Create(AAuthenticationHandler: IMVCAuthenticationHandler;
      AConfigClaims: TJWTClaimsSetup;
      ASecret: string = 'D3lph1MVCFram3w0rk';
      AClaimsToCheck: TJWTCheckableClaims = [
      TJWTCheckableClaim.ExpirationTime,
      TJWTCheckableClaim.NotBefore,
      TJWTCheckableClaim.IssuedAt
      ]); virtual;
  end;

implementation

{ TMVCJWTAuthenticationMiddleware }

constructor TMVCJWTAuthenticationMiddleware.Create(
  AAuthenticationHandler: IMVCAuthenticationHandler;
  AConfigClaims: TJWTClaimsSetup; ASecret: string;
  AClaimsToCheck: TJWTCheckableClaims);
begin
  inherited Create;
  FAuthenticationHandler := AAuthenticationHandler;
  FSetupJWTClaims := AConfigClaims;
  FClaimsToChecks := AClaimsToCheck;
  FSecret := ASecret;
end;

procedure TMVCJWTAuthenticationMiddleware.InternalRender(
  AJSONValue: TJSONValue; AContentType, AContentEncoding: string;
  AContext: TWebContext; AInstanceOwner: Boolean);
var
  Encoding: TEncoding;
  ContentType, JValue: string;
begin
  JValue := AJSONValue.ToJSON;

  AContext.Response.RawWebResponse.ContentType := AContentType + '; charset=' + AContentEncoding;
  ContentType := AContentType + '; charset=' + AContentEncoding;

  Encoding := TEncoding.GetEncoding(AContentEncoding);
  try
    AContext.Response.SetContentStream(
      TBytesStream.Create(TEncoding.Convert(TEncoding.Default, Encoding, TEncoding.Default.GetBytes(JValue))),
      ContentType);
  finally
    Encoding.Free;
  end;

  if AInstanceOwner then
    FreeAndNil(AJSONValue)
end;

procedure TMVCJWTAuthenticationMiddleware.OnAfterControllerAction(
  AContext: TWebContext; const AActionName: string;
  const AHandled: Boolean);
begin
  // Implement as needed
end;

procedure TMVCJWTAuthenticationMiddleware.OnBeforeControllerAction(
  AContext: TWebContext; const AControllerQualifiedClassName,
  AActionName: string; var AHandled: Boolean);
var
  AuthRequired: Boolean;
  IsAuthorized: Boolean;
  JWTValue: TJWT;
  AuthHeader: string;
  AuthToken: string;
  ErrorMsg: String;
begin
  // check if the resource is protected
  FAuthenticationHandler.OnRequest(AControllerQualifiedClassName, AActionName, AuthRequired);

  if not AuthRequired then
  begin
    AHandled := False;
    Exit;
  end;

  // Checking token in subsequent requests
  // ***************************************************
  JWTValue := TJWT.Create(FSecret);
  try
    JWTValue.RegClaimsToChecks := Self.FClaimsToChecks;
    AuthHeader := AContext.Request.Headers['Authentication'];
    if AuthHeader.IsEmpty then
    begin
      RenderError(HTTP_STATUS.Unauthorized, 'Authentication Required', AContext);
      AHandled := True;
      Exit;
    end;

    // retrieve the token from the "authentication bearer" header
    AuthToken := '';
    if AuthHeader.StartsWith('bearer', True) then
      AuthToken := AuthHeader.Remove(0, 'bearer'.Length).Trim;

    // check the jwt
    if not JWTValue.IsValidToken(AuthToken, ErrorMsg) then
    begin
      RenderError(HTTP_STATUS.Unauthorized, ErrorMsg, AContext);
      AHandled := True;
    end
    else
    begin
      JWTValue.LoadToken(AuthToken);
      if JWTValue.CustomClaims['username'].IsEmpty then
      begin
        RenderError(HTTP_STATUS.Unauthorized, 'Invalid Token, Authentication Required', AContext);
        AHandled := True;
      end
      else
      begin
        IsAuthorized := False;

        AContext.LoggedUser.UserName := JWTValue.CustomClaims['username'];
        AContext.LoggedUser.Roles.AddRange(JWTValue.CustomClaims['roles'].Split([',']));
        AContext.LoggedUser.LoggedSince := JWTValue.Claims.IssuedAt;

        FAuthenticationHandler.OnAuthorization(AContext.LoggedUser.Roles, AControllerQualifiedClassName, AActionName, IsAuthorized);

        if IsAuthorized then
          AHandled := False
        else
        begin
          RenderError(HTTP_STATUS.Forbidden, 'Authorization Forbidden', AContext);
          AHandled := True;
        end;
      end;
    end;
  finally
    JWTValue.Free;
  end;
end;

procedure TMVCJWTAuthenticationMiddleware.OnBeforeRouting(
  AContext: TWebContext; var AHandled: Boolean);
var
  UserName: string;
  Password: string;
  RolesList: TList<String>;
  SessionData: TSessionData;
  IsValid: Boolean;
  JWTValue: TJWT;
begin
  if SameText(AContext.Request.PathInfo, '/login') and (AContext.Request.HTTPMethod = httpPOST) then
  begin
    UserName := AContext.Request.Headers['jwtusername'];
    Password := AContext.Request.Headers['jwtpassword'];
    if (UserName.IsEmpty) or (Password.IsEmpty) then
    begin
      RenderError(HTTP_STATUS.Unauthorized, 'Username and password Required', AContext);
      AHandled := True;
      Exit;
    end;

    // check the authorization for the requested resource
    RolesList := TList<string>.Create;
    try
      SessionData := TSessionData.Create;
      try
        FAuthenticationHandler.OnAuthentication(UserName, Password, RolesList, IsValid, SessionData);
        if IsValid then
        begin
          JWTValue := TJWT.Create(FSecret);
          try
            // let's user config claims and custom claims
            FSetupJWTClaims(JWTValue);

            // these claims are mandatory and managed by the middleware
            if not JWTValue.CustomClaims['username'].IsEmpty then
              raise EMVCJWTException.Create('Custom claim "username" is reserved and cannot be modified in the JWT setup');

            if not JWTValue.CustomClaims['roles'].IsEmpty then
              raise EMVCJWTException.Create('Custom claim "roles" is reserved and cannot be modified in the JWT setup');

            JWTValue.CustomClaims['username'] := UserName;
            JWTValue.CustomClaims['roles'] := String.Join(',', RolesList.ToArray);

            // setup the current logged user from the JWT
            AContext.LoggedUser.Roles.AddRange(RolesList);
            AContext.LoggedUser.UserName := JWTValue.CustomClaims['username'];
            AContext.LoggedUser.LoggedSince := JWTValue.Claims.IssuedAt;
            AContext.LoggedUser.Realm := JWTValue.Claims.Subject;

            InternalRender(
              TJSONObject.Create(TJSONPair.Create('token', JWTValue.GetToken)),
              TMVCMediaType.APPLICATION_JSON,
              TMVCConstants.DEFAULT_CONTENT_CHARSET,
              AContext
              );
            AHandled := True;
          finally
            JWTValue.Free;
          end;
        end
        else
        begin
          RenderError(HTTP_STATUS.Forbidden, 'Forbidden', AContext);
          AHandled := True;
        end;
      finally
        SessionData.Free;
      end;
    finally
      RolesList.Free;
    end;
  end;
end;

procedure TMVCJWTAuthenticationMiddleware.RenderError(const AErrorCode: UInt16;
  const AErrorMessage: string; const AContext: TWebContext;
  const AErrorClassName: string);
var
  Jo: TJSONObject;
  Status: string;
begin
  AContext.Response.StatusCode := AErrorCode;
  AContext.Response.ReasonString := AErrorMessage;

  Status := 'error';
  if (AErrorCode div 100) = 2 then
    Status := 'ok';

  Jo := TJSONObject.Create;
  Jo.AddPair('status', Status);

  if AErrorClassName = '' then
    Jo.AddPair('classname', TJSONNull.Create)
  else
    Jo.AddPair('classname', AErrorClassName);

  Jo.AddPair('message', AErrorMessage);

  InternalRender(Jo, TMVCConstants.DEFAULT_CONTENT_TYPE, TMVCConstants.DEFAULT_CONTENT_CHARSET, AContext);
end;

end.
