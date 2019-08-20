// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2019 Daniele Teti and the DMVCFramework Team
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
  JsonDataObjects;

type
  TMVCJWTDefaults = class sealed
  public const
    /// <summary>
    /// Default authorization header name (RFC 6750)
    /// </summary>
    AUTHORIZATION_HEADER = 'Authorization';
    /// <summary>
    /// Default username header name
    /// </summary>
    USERNAME_HEADER = 'jwtusername';
    /// <summary>
    /// Default password header name
    /// </summary>
    PASSWORD_HEADER = 'jwtpassword';
  end;

  TJWTClaimsSetup = reference to procedure(const JWT: TJWT);

  TMVCJWTAuthenticationMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    FAuthenticationHandler: IMVCAuthenticationHandler;
    FClaimsToChecks: TJWTCheckableClaims;
    FSetupJWTClaims: TJWTClaimsSetup;
    FSecret: string;
    FLeewaySeconds: Cardinal;
    FLoginURLSegment: string;
    FAuthorizationHeaderName: string;
    FUserNameHeaderName: string;
    FPasswordHeaderName: string;
  protected
    function NeedsToBeExtended(const JWTValue: TJWT): Boolean;
    procedure ExtendExpirationTime(const JWTValue: TJWT);
    procedure InternalRender(AJSONOb: TJDOJsonObject; AContentType: string; AContentEncoding: string; AContext: TWebContext;
      AInstanceOwner: Boolean = True);
    procedure RenderError(const AHTTPStatusCode: UInt16; const AErrorMessage: string; const AContext: TWebContext;
      const AErrorClassName: string = ''; const AErrorNumber: Integer = 0);
    procedure OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
    procedure OnBeforeControllerAction(AContext: TWebContext; const AControllerQualifiedClassName: string;
      const AActionName: string; var AHandled: Boolean);
    procedure OnAfterControllerAction(AContext: TWebContext; const AActionName: string; const AHandled: Boolean);
  public
    /// <remarks>
    ///   The AAuthorizationHeaderName, AUserNameHeaderName, and APasswordHeaderName parameters do not follow
    ///   the IETF national convention - RFC 6750;
    /// </remarks>
    constructor Create(
      AAuthenticationHandler: IMVCAuthenticationHandler;
      AConfigClaims: TJWTClaimsSetup;
      ASecret: string = 'D3lph1MVCFram3w0rk';
      ALoginURLSegment: string = '/login';
      AClaimsToCheck: TJWTCheckableClaims = [];
      ALeewaySeconds: Cardinal = 300;
      AAuthorizationHeaderName: string = TMVCJWTDefaults.AUTHORIZATION_HEADER;
      AUserNameHeaderName: string = TMVCJWTDefaults.USERNAME_HEADER;
      APasswordHeaderName: string = TMVCJWTDefaults.PASSWORD_HEADER); overload; virtual;
      deprecated 'Issue #244: IETF (RFC-6750) - This constructor will be removed soon, please use the new one';
    constructor Create(
      AAuthenticationHandler: IMVCAuthenticationHandler;
      ASecret: string = 'D3lph1MVCFram3w0rk';
      ALoginURLSegment: string = '/login';
      AConfigClaims: TJWTClaimsSetup = nil;
      AClaimsToCheck: TJWTCheckableClaims = [];
      ALeewaySeconds: Cardinal = 300); overload; virtual;
  end;

implementation

uses
  System.NetEncoding,
  System.DateUtils,
  System.Math,
  MVCFramework.Logger;

{ TMVCJWTAuthenticationMiddleware }

constructor TMVCJWTAuthenticationMiddleware.Create(AAuthenticationHandler: IMVCAuthenticationHandler;
  AConfigClaims: TJWTClaimsSetup;
  ASecret: string = 'D3lph1MVCFram3w0rk';
  ALoginURLSegment: string = '/login';
  AClaimsToCheck: TJWTCheckableClaims = [];
  ALeewaySeconds: Cardinal = 300;
  AAuthorizationHeaderName: string = TMVCJWTDefaults.AUTHORIZATION_HEADER;
  AUserNameHeaderName: string = TMVCJWTDefaults.USERNAME_HEADER;
  APasswordHeaderName: string = TMVCJWTDefaults.PASSWORD_HEADER);
begin
  inherited Create;
  FAuthenticationHandler := AAuthenticationHandler;
  FSetupJWTClaims := AConfigClaims;
  FClaimsToChecks := AClaimsToCheck;
  FSecret := ASecret;
  FLoginURLSegment := ALoginURLSegment;
  FLeewaySeconds := ALeewaySeconds;
  FAuthorizationHeaderName := AAuthorizationHeaderName;
  FUserNameHeaderName := AUserNameHeaderName;
  FPasswordHeaderName := APasswordHeaderName;
end;

constructor TMVCJWTAuthenticationMiddleware.Create(AAuthenticationHandler: IMVCAuthenticationHandler; ASecret,
  ALoginURLSegment: string; AConfigClaims: TJWTClaimsSetup; AClaimsToCheck: TJWTCheckableClaims; ALeewaySeconds: Cardinal);
begin
  inherited Create;
  FAuthenticationHandler := AAuthenticationHandler;
  FSetupJWTClaims := AConfigClaims;
  FClaimsToChecks := AClaimsToCheck;
  FSecret := ASecret;
  FLoginURLSegment := ALoginURLSegment;
  FLeewaySeconds := ALeewaySeconds;
  FAuthorizationHeaderName := TMVCJWTDefaults.AUTHORIZATION_HEADER;
  FUserNameHeaderName := TMVCJWTDefaults.USERNAME_HEADER;
  FPasswordHeaderName := TMVCJWTDefaults.PASSWORD_HEADER;
end;

procedure TMVCJWTAuthenticationMiddleware.ExtendExpirationTime(const JWTValue: TJWT);
begin
  JWTValue.Claims.ExpirationTime := Max(JWTValue.Claims.ExpirationTime, Now) +
    (JWTValue.LeewaySeconds + JWTValue.LiveValidityWindowInSeconds) * OneSecond;
end;

procedure TMVCJWTAuthenticationMiddleware.InternalRender(AJSONOb: TJDOJsonObject;
  AContentType, AContentEncoding: string; AContext: TWebContext; AInstanceOwner: Boolean);
var
  Encoding: TEncoding;
  ContentType, JValue: string;
begin
  JValue := AJSONOb.ToJSON;

  AContext.Response.RawWebResponse.ContentType := AContentType + '; charset=' + AContentEncoding;
  ContentType := AContentType + '; charset=' + AContentEncoding;

  Encoding := TEncoding.GetEncoding(AContentEncoding);
  try
    AContext.Response.SetContentStream(TBytesStream.Create(TEncoding.Convert(TEncoding.Default, Encoding,
      TEncoding.Default.GetBytes(JValue))), ContentType);
  finally
    Encoding.Free;
  end;

  if AInstanceOwner then
    FreeAndNil(AJSONOb)
end;

function TMVCJWTAuthenticationMiddleware.NeedsToBeExtended(const JWTValue: TJWT): Boolean;
var
  lWillExpireIn: Int64;
begin
  lWillExpireIn := SecondsBetween(Now, JWTValue.Claims.ExpirationTime);
  Result := lWillExpireIn <= JWTValue.LiveValidityWindowInSeconds;
end;

procedure TMVCJWTAuthenticationMiddleware.OnAfterControllerAction(AContext: TWebContext; const AActionName: string;
  const AHandled: Boolean);
begin
  // Implement as needed
end;

procedure TMVCJWTAuthenticationMiddleware.OnBeforeControllerAction(AContext: TWebContext;
  const AControllerQualifiedClassName, AActionName: string; var AHandled: Boolean);
var
  AuthRequired: Boolean;
  IsAuthorized: Boolean;
  JWTValue: TJWT;
  AuthHeader: string;
  AuthToken: string;
  ErrorMsg: string;
const
  AUTH_SCHEMA = 'Bearer';
begin
  // check if the resource is protected
  if Assigned(FAuthenticationHandler) then
  begin
    FAuthenticationHandler.OnRequest(AContext, AControllerQualifiedClassName, AActionName, AuthRequired);
    if not AuthRequired then
    begin
      AHandled := False;
      Exit;
    end;
  end;

  // Checking token in subsequent requests
  // ***************************************************
  JWTValue := TJWT.Create(FSecret, FLeewaySeconds);
  try
    JWTValue.RegClaimsToChecks := Self.FClaimsToChecks;
    AuthHeader := AContext.Request.Headers[FAuthorizationHeaderName];
    if AuthHeader.IsEmpty then
    begin
      RenderError(HTTP_STATUS.Unauthorized, 'Authorization Required', AContext);
      AHandled := True;
      Exit;
    end;

    // retrieve the token from the "authentication Bearer" header
    AuthToken := '';
    if AuthHeader.Substring(0, AUTH_SCHEMA.Length).ToLower = 'bearer' then
    begin
      AuthToken := AuthHeader.Remove(0, AUTH_SCHEMA.Length).Trim;
      AuthToken := Trim(TNetEncoding.URL.Decode(AuthToken));
    end;

    if not JWTValue.LoadToken(AuthToken, ErrorMsg) then
    begin
      RenderError(HTTP_STATUS.Unauthorized, ErrorMsg, AContext);
      AHandled := True;
      Exit;
    end;

    if JWTValue.CustomClaims['username'].IsEmpty then
    begin
      RenderError(HTTP_STATUS.Unauthorized, 'Invalid Token, Authorization Required', AContext);
      AHandled := True;
      Exit;
    end;

    AContext.LoggedUser.UserName := JWTValue.CustomClaims['username'];
    AContext.LoggedUser.Roles.AddRange(JWTValue.CustomClaims['roles'].Split([',']));
    AContext.LoggedUser.LoggedSince := JWTValue.Claims.IssuedAt;
    AContext.LoggedUser.CustomData := JWTValue.CustomClaims.AsCustomData;

    if Assigned(FAuthenticationHandler) then
    begin
      FAuthenticationHandler.OnAuthorization(AContext, AContext.LoggedUser.Roles, AControllerQualifiedClassName, AActionName, IsAuthorized);
      if not IsAuthorized then
      begin
        RenderError(HTTP_STATUS.Forbidden, 'Authorization Forbidden', AContext);
        AHandled := True;
        Exit;
      end;
    end;

    if JWTValue.LiveValidityWindowInSeconds > 0 then
    begin
      if NeedsToBeExtended(JWTValue) then
      begin
        ExtendExpirationTime(JWTValue);
        AContext.Response.SetCustomHeader(FAuthorizationHeaderName, 'Bearer ' + JWTValue.GetToken);
      end;
    end;

    AHandled := False;
  finally
    JWTValue.Free;
  end;
end;

procedure TMVCJWTAuthenticationMiddleware.OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
var
  LUsername, LPassword, LBasicAuthenticationEncode: string;
  LBasicAuthenticationDecode: TStringList;
  LRolesList: TList<string>;
  LSessionData: TSessionData;
  LIsValid: Boolean;
  LJWTValue: TJWT;
  LCustomPair: TPair<string, string>;
  LJsonObject: TJDOJsonObject;
begin
  if SameText(AContext.Request.PathInfo, FLoginURLSegment) then
  begin
    LBasicAuthenticationEncode := AContext.Request.Headers[FAuthorizationHeaderName];
    if LBasicAuthenticationEncode.IsEmpty then
    begin
      LUsername := TNetEncoding.URL.Decode(AContext.Request.Headers[FUserNameHeaderName]);
      LPassword := TNetEncoding.URL.Decode(AContext.Request.Headers[FPasswordHeaderName]);
      if (LUsername.IsEmpty) or (LPassword.IsEmpty) then
      begin
        RenderError(HTTP_STATUS.Unauthorized, 'Username and password Required', AContext);
        AHandled := True;
        Exit;
      end;
    end
    else
    begin
      if not LBasicAuthenticationEncode.StartsWith('basic', True) then
      begin
        RenderError(HTTP_STATUS.Unauthorized, 'Invalid authorization type', AContext);
        AHandled := True;
        Exit;
      end;
      LBasicAuthenticationDecode := TStringList.Create;
      try
        LBasicAuthenticationDecode.Delimiter := ':';
        LBasicAuthenticationDecode.DelimitedText := TBase64Encoding.Base64.Decode(LBasicAuthenticationEncode.Replace('basic ', '', [rfIgnoreCase]));
        LUsername := LBasicAuthenticationDecode.Strings[0];
        LPassword := LBasicAuthenticationDecode.Strings[1];
      finally
        LBasicAuthenticationDecode.Free;
      end;
    end;

    // check the authorization for the requested resource
    LRolesList := TList<string>.Create;
    try
      LSessionData := TSessionData.Create;
      try
        try
          if Assigned(FAuthenticationHandler) then
          begin
            FAuthenticationHandler.OnAuthentication(AContext, LUsername, LPassword, LRolesList, LIsValid, LSessionData);
            if not LIsValid then
            begin
              RenderError(HTTP_STATUS.Forbidden, 'Forbidden', AContext);
              AHandled := True;
              Exit;
            end;
          end;

          LJWTValue := TJWT.Create(FSecret, FLeewaySeconds);
          try
            // let's user config claims and custom claims
            if not Assigned(FSetupJWTClaims) then
              raise EMVCJWTException.Create('SetupJWTClaims not set');

            FSetupJWTClaims(LJWTValue);

            // these claims are mandatory and managed by the middleware
            if not LJWTValue.CustomClaims['username'].IsEmpty then
              raise EMVCJWTException.Create('Custom claim "username" is reserved and cannot be modified in the JWT setup');

            if not LJWTValue.CustomClaims['roles'].IsEmpty then
              raise EMVCJWTException.Create('Custom claim "roles" is reserved and cannot be modified in the JWT setup');

            LJWTValue.CustomClaims['username'] := LUsername;
            LJWTValue.CustomClaims['roles'] := string.Join(',', LRolesList.ToArray);

            if LJWTValue.LiveValidityWindowInSeconds > 0 then
              if NeedsToBeExtended(LJWTValue) then
                ExtendExpirationTime(LJWTValue);

            // setup the current logged user from the JWT
            AContext.LoggedUser.Roles.AddRange(LRolesList);
            AContext.LoggedUser.UserName := LJWTValue.CustomClaims['username'];
            AContext.LoggedUser.LoggedSince := LJWTValue.Claims.IssuedAt;
            AContext.LoggedUser.Realm := LJWTValue.Claims.Subject;

            if LSessionData.Count > 0 then
            begin
              AContext.LoggedUser.CustomData := TMVCCustomData.Create;
              for LCustomPair in LSessionData do
              begin
                AContext.LoggedUser.CustomData.AddOrSetValue(LCustomPair.Key, LCustomPair.Value);
                if not LJWTValue.CustomClaims.Items[LCustomPair.Key].IsEmpty then
                  raise EMVCJWTException.CreateFmt('JWT Error: "%s" is a reserved key name', [LCustomPair.Key]);
                LJWTValue.CustomClaims.Items[LCustomPair.Key] := LCustomPair.Value;
              end;
            end;

            LJsonObject := TJDOJsonObject.Create;
            try
              LJsonObject.S['token'] := LJWTValue.GetToken;
              InternalRender(LJsonObject, TMVCMediaType.APPLICATION_JSON, TMVCConstants.DEFAULT_CONTENT_CHARSET, AContext, False);
            finally
              LJsonObject.Free;
            end;
            AHandled := True;
          finally
            LJWTValue.Free;
          end;
        except
          on Err: EMVCException do
          begin
            RenderError(Err.HttpErrorCode, Err.Message, AContext, Err.ClassName, Err.ApplicationErrorCode);
            AHandled := True;
          end;
          on e: Exception do
          begin
            RenderError(HTTP_STATUS.Forbidden, e.Message, AContext);
            AHandled := True;
          end;
        end;
      finally
        LSessionData.Free;
      end;
    finally
      LRolesList.Free;
    end;
  end;
end;

procedure TMVCJWTAuthenticationMiddleware.RenderError(const AHTTPStatusCode: UInt16; const AErrorMessage: string;
  const AContext: TWebContext; const AErrorClassName: string; const AErrorNumber: Integer);
var
  LJObj: TJDOJsonObject;
  LStatus: string;
begin
  AContext.Response.StatusCode := AHTTPStatusCode;
  AContext.Response.ReasonString := AErrorMessage;

  LStatus := 'error';
  if (AHTTPStatusCode div 100) = 2 then
  begin
    LStatus := 'ok';
  end;

  LJObj := TJDOJsonObject.Create;
  LJObj.S['status'] := LStatus;
  LJObj.I['statuscode'] := AHTTPStatusCode;
  LJObj.S['message'] := AErrorMessage;

  LJObj.Values['classname'] := nil;
  if AErrorClassName <> '' then
  begin
    LJObj.S['classname'] := AErrorClassName;
  end;

  if AErrorNumber <> 0 then
  begin
    LJObj.I['errornumber'] := AErrorNumber;
  end;

  InternalRender(LJObj, TMVCConstants.DEFAULT_CONTENT_TYPE, TMVCConstants.DEFAULT_CONTENT_CHARSET, AContext);
end;

end.
