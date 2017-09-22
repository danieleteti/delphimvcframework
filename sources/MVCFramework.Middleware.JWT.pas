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
    FLeewaySeconds: Cardinal;
    FLoginURLSegment: string;
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
      ALoginURLSegment: string = '/login';
      AClaimsToCheck: TJWTCheckableClaims = [
      TJWTCheckableClaim.ExpirationTime,
      TJWTCheckableClaim.NotBefore,
      TJWTCheckableClaim.IssuedAt
      ];
      ALeewaySeconds: Cardinal = 300); virtual;
  end;

implementation

uses System.NetEncoding, System.DateUtils;

{ TMVCJWTAuthenticationMiddleware }

constructor TMVCJWTAuthenticationMiddleware.Create(AAuthenticationHandler: IMVCAuthenticationHandler;
  AConfigClaims: TJWTClaimsSetup;
  ASecret: string = 'D3lph1MVCFram3w0rk';
  ALoginURLSegment: string = '/login';
  AClaimsToCheck: TJWTCheckableClaims = [
  TJWTCheckableClaim.ExpirationTime,
  TJWTCheckableClaim.NotBefore,
  TJWTCheckableClaim.IssuedAt
  ];
  ALeewaySeconds: Cardinal = 300);
begin
  inherited Create;
  FAuthenticationHandler := AAuthenticationHandler;
  FSetupJWTClaims := AConfigClaims;
  FClaimsToChecks := AClaimsToCheck;
  FSecret := ASecret;
  FLoginURLSegment := ALoginURLSegment;
  FLeewaySeconds := ALeewaySeconds;
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
  ErrorMsg: string;
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
  JWTValue := TJWT.Create(FSecret, FLeewaySeconds);
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
    begin
      AuthToken := AuthHeader.Remove(0, 'bearer'.Length).Trim;
      AuthToken := Trim(TNetEncoding.URL.URLDecode(AuthToken));
    end;

    // check the jwt
    // if not JWTValue.IsValidToken(AuthToken, ErrorMsg) then
    // begin
    // RenderError(HTTP_STATUS.Unauthorized, ErrorMsg, AContext);
    // AHandled := True;
    // end
    // else

    if not JWTValue.LoadToken(AuthToken, ErrorMsg) then
    begin
      RenderError(HTTP_STATUS.Unauthorized, ErrorMsg, AContext);
      AHandled := True;
      Exit;
    end;

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
      AContext.LoggedUser.CustomData := JWTValue.CustomClaims.AsCustomData;

      FAuthenticationHandler.OnAuthorization(AContext.LoggedUser.Roles, AControllerQualifiedClassName, AActionName, IsAuthorized);

      if IsAuthorized then
      begin
        if JWTValue.LiveValidityWindowInSeconds > 0 then
        begin
          JWTValue.Claims.ExpirationTime := Now + JWTValue.LiveValidityWindowInSeconds * OneSecond;
          AContext.Response.SetCustomHeader('Authentication', 'bearer ' + JWTValue.GetToken);
        end;
        AHandled := False
      end
      else
      begin
        RenderError(HTTP_STATUS.Forbidden, 'Authorization Forbidden', AContext);
        AHandled := True;
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
  RolesList: TList<string>;
  SessionData: TSessionData;
  IsValid: Boolean;
  JWTValue: TJWT;
  lCustomPair: TPair<string, string>;
begin
  if SameText(AContext.Request.PathInfo, FLoginURLSegment) and (AContext.Request.HTTPMethod = httpPOST) then
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
          JWTValue := TJWT.Create(FSecret, FLeewaySeconds);
          try
            // let's user config claims and custom claims
            if not Assigned(FSetupJWTClaims) then
              raise EMVCJWTException.Create('SetupJWTClaims not set');
            FSetupJWTClaims(JWTValue);

            // these claims are mandatory and managed by the middleware
            if not JWTValue.CustomClaims['username'].IsEmpty then
              raise EMVCJWTException.Create('Custom claim "username" is reserved and cannot be modified in the JWT setup');

            if not JWTValue.CustomClaims['roles'].IsEmpty then
              raise EMVCJWTException.Create('Custom claim "roles" is reserved and cannot be modified in the JWT setup');

            JWTValue.CustomClaims['username'] := UserName;
            JWTValue.CustomClaims['roles'] := string.Join(',', RolesList.ToArray);

            if JWTValue.LiveValidityWindowInSeconds > 0 then
            begin
              JWTValue.Claims.ExpirationTime := Now + (JWTValue.LeewaySeconds + JWTValue.LiveValidityWindowInSeconds) * OneSecond;
            end;

            // setup the current logged user from the JWT
            AContext.LoggedUser.Roles.AddRange(RolesList);
            AContext.LoggedUser.UserName := JWTValue.CustomClaims['username'];
            AContext.LoggedUser.LoggedSince := JWTValue.Claims.IssuedAt;
            AContext.LoggedUser.Realm := JWTValue.Claims.Subject;

            if SessionData.Count > 0 then
            begin
              AContext.LoggedUser.CustomData := TMVCCustomData.Create;
              for lCustomPair in SessionData do
              begin
                AContext.LoggedUser.CustomData.AddOrSetValue(lCustomPair.Key, lCustomPair.Value);
                if not JWTValue.CustomClaims.Items[lCustomPair.Key].IsEmpty then
                  raise EMVCJWTException.CreateFmt('JWT Error: "%s" is a reserved key name', [lCustomPair.Key]);
                JWTValue.CustomClaims.Items[lCustomPair.Key] := lCustomPair.Value;
              end;
            end;

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
