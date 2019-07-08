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
    /// Default authorization header name
    /// </summary>
    AUTHORIZATION_HEADER = 'Authentication';
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
    procedure InternalRender(AJSONOb: TJDOJsonObject; AContentType: string; AContentEncoding: string;
      AContext: TWebContext; AInstanceOwner: Boolean = True);

    procedure RenderError(const AHTTPStatusCode: UInt16; const AErrorMessage: string;
  const AContext: TWebContext; const AErrorClassName: string = ''; const AErrorNumber: Integer = 0);

    procedure OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);

    procedure OnBeforeControllerAction(AContext: TWebContext; const AControllerQualifiedClassName: string;
      const AActionName: string; var AHandled: Boolean);

    procedure OnAfterControllerAction(AContext: TWebContext; const AActionName: string; const AHandled: Boolean);
  public
    constructor Create(
      AAuthenticationHandler: IMVCAuthenticationHandler;
      AConfigClaims: TJWTClaimsSetup;
      ASecret: string = 'D3lph1MVCFram3w0rk';
      ALoginURLSegment: string = '/login';
      AClaimsToCheck: TJWTCheckableClaims = [TJWTCheckableClaim.ExpirationTime, TJWTCheckableClaim.NotBefore,
      TJWTCheckableClaim.IssuedAt];
      ALeewaySeconds: Cardinal = 300;
      AAuthorizationHeaderName: string = TMVCJWTDefaults.AUTHORIZATION_HEADER;
      AUserNameHeaderName: string = TMVCJWTDefaults.USERNAME_HEADER;
      APasswordHeaderName: string = TMVCJWTDefaults.PASSWORD_HEADER); virtual;
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
  AClaimsToCheck: TJWTCheckableClaims = [TJWTCheckableClaim.ExpirationTime, TJWTCheckableClaim.NotBefore,
  TJWTCheckableClaim.IssuedAt];
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
begin
  // check if the resource is protected
  FAuthenticationHandler.OnRequest(AContext, AControllerQualifiedClassName, AActionName, AuthRequired);

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
    AuthHeader := AContext.Request.Headers[FAuthorizationHeaderName];
    if AuthHeader.IsEmpty then
    begin
      RenderError(HTTP_STATUS.Unauthorized, 'Authorization Required', AContext);
      AHandled := True;
      Exit;
    end;

    // retrieve the token from the "authentication bearer" header
    AuthToken := '';
    if AuthHeader.StartsWith('bearer', True) then
    begin
      AuthToken := AuthHeader.Remove(0, 'bearer'.Length).Trim;
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
    end
    else
    begin
      IsAuthorized := False;

      AContext.LoggedUser.UserName := JWTValue.CustomClaims['username'];
      AContext.LoggedUser.Roles.AddRange(JWTValue.CustomClaims['roles'].Split([',']));
      AContext.LoggedUser.LoggedSince := JWTValue.Claims.IssuedAt;
      AContext.LoggedUser.CustomData := JWTValue.CustomClaims.AsCustomData;

      FAuthenticationHandler.OnAuthorization(AContext, AContext.LoggedUser.Roles, AControllerQualifiedClassName,
        AActionName, IsAuthorized);

      if IsAuthorized then
      begin
        if JWTValue.LiveValidityWindowInSeconds > 0 then
        begin
          if NeedsToBeExtended(JWTValue) then
          begin
            ExtendExpirationTime(JWTValue);
            AContext.Response.SetCustomHeader(FAuthorizationHeaderName, 'bearer ' + JWTValue.GetToken);
          end;
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

procedure TMVCJWTAuthenticationMiddleware.OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
var
  UserName: string;
  Password: string;
  RolesList: TList<string>;
  SessionData: TSessionData;
  IsValid: Boolean;
  JWTValue: TJWT;
  lCustomPair: TPair<string, string>;
  LObj: TJDOJsonObject;
begin
  if SameText(AContext.Request.PathInfo, FLoginURLSegment) and (AContext.Request.HTTPMethod = httpPOST) then
  begin
    UserName := TNetEncoding.URL.Decode(AContext.Request.Headers[FUserNameHeaderName]);
    Password := TNetEncoding.URL.Decode(AContext.Request.Headers[FPasswordHeaderName]);
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
        try
          FAuthenticationHandler.OnAuthentication(AContext, UserName, Password, RolesList, IsValid, SessionData);
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
                raise EMVCJWTException.Create
                  ('Custom claim "username" is reserved and cannot be modified in the JWT setup');

              if not JWTValue.CustomClaims['roles'].IsEmpty then
                raise EMVCJWTException.Create
                  ('Custom claim "roles" is reserved and cannot be modified in the JWT setup');

              JWTValue.CustomClaims['username'] := UserName;
              JWTValue.CustomClaims['roles'] := string.Join(',', RolesList.ToArray);

              if JWTValue.LiveValidityWindowInSeconds > 0 then
              begin
                if NeedsToBeExtended(JWTValue) then
                begin
                  ExtendExpirationTime(JWTValue);
                end;
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

              LObj := TJDOJsonObject.Create;
              try
                LObj.S['token'] := JWTValue.GetToken;
                InternalRender(LObj, TMVCMediaType.APPLICATION_JSON, TMVCConstants.DEFAULT_CONTENT_CHARSET,
                  AContext, False);
              finally
                LObj.Free;
              end;
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
        SessionData.Free;
      end;
    finally
      RolesList.Free;
    end;
  end;
end;

procedure TMVCJWTAuthenticationMiddleware.RenderError(const AHTTPStatusCode: UInt16; const AErrorMessage: string;
  const AContext: TWebContext; const AErrorClassName: string; const AErrorNumber: Integer);
var
  lJObj: TJDOJsonObject;
  lStatus: string;
begin
  AContext.Response.StatusCode := AHTTPStatusCode;
  AContext.Response.ReasonString := AErrorMessage;

  lStatus := 'error';
  if (AHTTPStatusCode div 100) = 2 then
    lStatus := 'ok';

  lJObj := TJDOJsonObject.Create;
  lJObj.S['status'] := lStatus;
  lJObj.I['statuscode'] := AHTTPStatusCode;
  lJObj.S['message'] := AErrorMessage;

  if AErrorClassName = '' then
  begin
    lJObj.Values['classname'] := nil
  end
  else
  begin
    lJObj.S['classname'] := AErrorClassName;
  end;


  if AErrorNumber <> 0 then
  begin
    lJObj.I['errornumber'] := AErrorNumber;
  end;

  InternalRender(lJObj, TMVCConstants.DEFAULT_CONTENT_TYPE, TMVCConstants.DEFAULT_CONTENT_CHARSET, AContext);
end;

end.
