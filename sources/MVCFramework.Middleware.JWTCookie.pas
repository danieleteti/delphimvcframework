// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2025 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.Middleware.JWTCookie;

{$I dmvcframework.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.JWT,
  JsonDataObjects,
  MVCFramework.HMAC,
  Web.HTTPApp;

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
    /// <summary>
    /// Default AUTH schema
    /// </summary>
    AUTH_SCHEMA = 'Bearer';
    /// <summary>
    /// Default url authorization token
    /// </summary>
    AUTHORIZATION_ACCESS_TOKEN = 'access_token';
  end;

  TJWTClaimsSetup = reference to procedure(const JWT: TJWT);

  TMVCJWTCookieAuthenticationMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    FAuthenticationHandler: IMVCAuthenticationHandler;
    FClaimsToChecks: TJWTCheckableClaims;
    FSetupJWTClaims: TJWTClaimsSetup;
    FSecret: string;
    FLeewaySeconds: Cardinal;
    FLoginURLSegment: string;
    FAuthorizationAccessToken: string;
    FUserNameElement: string;
    FPasswordElement: string;
    FHMACAlgorithm: String;
    FUseHttpOnly: Boolean;
    FTokenHttpOnlyExpires: TDateTime;
    FLogoffURLSegment: string;
    FOnRedirectURL: string;
    procedure SendLogoffRender(AContext: TWebContext);
    procedure SendAuthCookie(JWT: String; AContext: TWebContext);
  protected
    function GetJWTCookie(AContext: TWebContext): String;
    procedure RedirectToLoginURL(const AContext: TWebContext; const Message: String);
    function NeedsToBeExtended(const JWTValue: TJWT): Boolean;
    procedure ExtendExpirationTime(const JWTValue: TJWT);
    procedure InternalRender(AJSONOb: TJDOJsonObject; AContentType: string; AContentEncoding: string;
      AContext: TWebContext; AInstanceOwner: Boolean = True); virtual;
    procedure OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean); virtual;
    procedure OnBeforeControllerAction(AContext: TWebContext; const AControllerQualifiedClassName: string;
      const AActionName: string; var AHandled: Boolean); virtual;
    procedure OnAfterControllerAction(AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      const AHandled: Boolean); virtual;
    procedure OnAfterRouting(AContext: TWebContext; const AHandled: Boolean); virtual;
  public
    constructor Create(AAuthenticationHandler: IMVCAuthenticationHandler;
      AConfigClaims: TJWTClaimsSetup;
      ALoginURLSegment: string = '/login';
      ALogoffURLSegment: string = '/logoff';
      ASecret: string = 'D3lph1MVCFram3w0rk';
      AUserNameElement: String = 'username';
      APasswordElement: String = 'password';
      AClaimsToCheck: TJWTCheckableClaims = [];
      ALeewaySeconds: Cardinal = 300;
      AHMACAlgorithm: String = HMAC_HS512); overload; virtual;
  end;

  TMVCOnAcceptTokenProc = reference to procedure(AContext: TWebContext; AJWTToken: String;
    var AAccepted: Boolean);
  TMVCOnNewJWTToBlackList = reference to procedure(AContext: TWebContext; AJWTToken: String);

  TMVCJWTBlackListMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    fOnAcceptToken: TMVCOnAcceptTokenProc;
    fOnNewJWTToBlackList: TMVCOnNewJWTToBlackList;
    fBlackListRequestURLSegment: string;
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
      const AControllerQualifiedClassName: string; const AActionName: string;
      const AHandled: Boolean);

    procedure OnAfterRouting(
      AContext: TWebContext;
      const AHandled: Boolean
      );
  public
    constructor Create(
      OnAcceptToken: TMVCOnAcceptTokenProc;
      OnNewJWTToBlackList: TMVCOnNewJWTToBlackList;
      BlackListRequestURLSegment: string = '/logout');
  end;


implementation

uses
  System.NetEncoding,
  System.DateUtils,
  System.Math,
  MVCFramework.Logger;

{ TMVCJWTCookieAuthenticationMiddleware }

constructor TMVCJWTCookieAuthenticationMiddleware.Create(AAuthenticationHandler: IMVCAuthenticationHandler;
  AConfigClaims: TJWTClaimsSetup; ALoginURLSegment, ALogoffURLSegment, ASecret, AUserNameElement, APasswordElement : string;
  AClaimsToCheck: TJWTCheckableClaims; ALeewaySeconds: Cardinal; AHMACAlgorithm: String);
begin
  inherited Create;
  FAuthenticationHandler := AAuthenticationHandler;
  FSetupJWTClaims := AConfigClaims;
  FClaimsToChecks := AClaimsToCheck;
  FSecret := ASecret;
  FLoginURLSegment := ALoginURLSegment;
  FLeewaySeconds := ALeewaySeconds;
  FAuthorizationAccessToken := TMVCJWTDefaults.AUTHORIZATION_ACCESS_TOKEN;
  FUserNameElement := AUserNameElement;
  FPasswordElement := APasswordElement;
  FOnRedirectURL := '/login';
  FHMACAlgorithm := AHMACAlgorithm;
  FTokenHttpOnlyExpires := Now;
  FLogoffURLSegment := ALogoffURLSegment;
end;

procedure TMVCJWTCookieAuthenticationMiddleware.SendAuthCookie(JWT: String; AContext: TWebContext);
var
  Cookie: TCookie;
begin
  Cookie := AContext.Response.Cookies.Add;
  Cookie.Expires := FTokenHttpOnlyExpires;
  Cookie.Path := '/';
  Cookie.Name := TMVCJWTDefaults.AUTHORIZATION_ACCESS_TOKEN;
  Cookie.Value := JWT;
  Cookie.HttpOnly := True;
  // Cookie.Secure := True;
  // Cookie.SameSite := 'none';
end;

procedure TMVCJWTCookieAuthenticationMiddleware.ExtendExpirationTime(const JWTValue: TJWT);
begin
  JWTValue.Claims.ExpirationTime := Max(JWTValue.Claims.ExpirationTime, Now) +
    (JWTValue.LeewaySeconds + JWTValue.LiveValidityWindowInSeconds) * OneSecond;
  if FUseHttpOnly then
  begin
    FTokenHttpOnlyExpires := JWTValue.Claims.ExpirationTime;
  end;
end;

function TMVCJWTCookieAuthenticationMiddleware.GetJWTCookie(AContext: TWebContext): String;
begin
  Result := TNetEncoding.URL.Decode(AContext.Request.Cookie(TMVCJWTDefaults.AUTHORIZATION_ACCESS_TOKEN)).Trim;
end;

procedure TMVCJWTCookieAuthenticationMiddleware.InternalRender(AJSONOb: TJDOJsonObject;
  AContentType, AContentEncoding: string; AContext: TWebContext; AInstanceOwner: Boolean);
var
  JValue: string;
begin
  try
    JValue := AJSONOb.ToJSON;
    SendAuthCookie(AJSONOb.S['token'], AContext);
    AContext.Response.Content := 'OK';
    AContext.Response.ContentType := BuildContentType(TMVCMediaType.TEXT_PLAIN, DefaultCharSet);
  finally
    if AInstanceOwner then
      FreeAndNil(AJSONOb)
  end;
end;

procedure TMVCJWTCookieAuthenticationMiddleware.SendLogoffRender(AContext: TWebContext);
const
  returnMessage = '{ "message": "Successful logout" }';
  ContentType = 'application/json; charset=UTF-8';
  AContentEncoding = 'UTF-8';
var
  Encoding: TEncoding;
  Cookie: TCookie;
begin
  Cookie := AContext.Response.Cookies.Add;
  Cookie.Name := 'token';
  Cookie.Path := '/';

  Encoding := TEncoding.GetEncoding(AContentEncoding);
  try
    AContext.Response.SetContentStream(TBytesStream.Create(TEncoding.Convert(TEncoding.Default, Encoding, TEncoding.Default.GetBytes(returnMessage))), ContentType);
  finally
    Encoding.Free;
  end;
end;

function TMVCJWTCookieAuthenticationMiddleware.NeedsToBeExtended(const JWTValue: TJWT): Boolean;
var
  lWillExpireIn: Int64;
begin
  lWillExpireIn := SecondsBetween(Now, JWTValue.Claims.ExpirationTime);
  Result := lWillExpireIn <= JWTValue.LiveValidityWindowInSeconds;
end;

procedure TMVCJWTCookieAuthenticationMiddleware.OnAfterControllerAction(AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      const AHandled: Boolean);
begin
  // Implement as needed
end;

procedure TMVCJWTCookieAuthenticationMiddleware.OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
begin
  // Implement as needed
end;

procedure TMVCJWTCookieAuthenticationMiddleware.OnBeforeControllerAction(AContext: TWebContext;
  const AControllerQualifiedClassName, AActionName: string; var AHandled: Boolean);
var
  AuthRequired: Boolean;
  IsAuthorized: Boolean;
  JWTValue: TJWT;
  AuthToken: string;
  ErrorMsg: string;
label
  AfterCheck;
begin
  // check if the resource is protected
  if Assigned(FAuthenticationHandler) then
  begin
    FAuthenticationHandler.OnRequest(AContext, AControllerQualifiedClassName, AActionName, AuthRequired);
    if not AuthRequired then
    begin
      AHandled := False;
      { this section handles the case when the authenticated user (with a token) need to call an action which doesn't require
        authentication. To make Context.LoggerdUser.IsValid works we need to load the JWT if present. In such way
        the "public" action can distriminate is has been called by a not-authnticated user or an authenticated user.
        If there isn't a token, we don't have to raise exceptions, just make sure that the LoggedUser doesn't contain
        information.
      }
      // retrieve the token from cookie
      AuthToken := GetJWTCookie(AContext);

      if not AuthToken.IsEmpty then
      begin
        { load and verify token even for an action that doesn't require it }
        JWTValue := TJWT.Create(FSecret, FLeewaySeconds);
        try
          JWTValue.RegClaimsToChecks := Self.FClaimsToChecks;
          if JWTValue.LoadToken(AuthToken, ErrorMsg) then
          begin
            { load token info only if the token is still valid }
            AContext.LoggedUser.UserName := JWTValue.CustomClaims['username'];
            AContext.LoggedUser.Roles.AddRange(JWTValue.CustomClaims['roles'].Split([',']));
            AContext.LoggedUser.LoggedSince := JWTValue.Claims.IssuedAt;
            AContext.LoggedUser.CustomData := JWTValue.CustomClaims.AsCustomData;
          end;
        finally
          JWTValue.Free;
        end;
      end;
      Exit;
    end;
  end;

  // Checking token in subsequent requests
  // ***************************************************
  JWTValue := TJWT.Create(FSecret, FLeewaySeconds);
  try
    JWTValue.RegClaimsToChecks := Self.FClaimsToChecks;
    AuthToken := GetJWTCookie(AContext);
    if AuthToken.IsEmpty then
    begin
      RedirectToLoginURL(AContext, 'Authorization Required');
      Exit;
    end;
    if not JWTValue.LoadToken(AuthToken, ErrorMsg) then
    begin
      RedirectToLoginURL(AContext, ErrorMsg);
      Exit;
    end;
    if JWTValue.CustomClaims['username'].IsEmpty then
    begin
      RedirectToLoginURL(AContext, 'Invalid Token, Authorization Required');
      Exit;
    end;

    AContext.LoggedUser.UserName := JWTValue.CustomClaims['username'];
    AContext.LoggedUser.Roles.AddRange(JWTValue.CustomClaims['roles'].Split([',']));
    AContext.LoggedUser.LoggedSince := JWTValue.Claims.IssuedAt;
    AContext.LoggedUser.CustomData := JWTValue.CustomClaims.AsCustomData;

    if Assigned(FAuthenticationHandler) then
    begin
      FAuthenticationHandler.OnAuthorization(AContext, AContext.LoggedUser.Roles, AControllerQualifiedClassName,
        AActionName, IsAuthorized);
      if not IsAuthorized then
      begin
        RedirectToLoginURL(AContext, 'Forbidden');
      end;
    end;

    if JWTValue.LiveValidityWindowInSeconds > 0 then
    begin
      if NeedsToBeExtended(JWTValue) then
      begin
        ExtendExpirationTime(JWTValue);
        SendAuthCookie(JWTValue.GetToken, AContext);
      end;
    end;

    AHandled := False;
  finally
    JWTValue.Free;
  end;
end;

procedure TMVCJWTCookieAuthenticationMiddleware.OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
var
  LUsername: string;
  LPassword: string;
  LBasicAuthHeader: string;
  LBasicAuthParts: TArray<string>;
  LRolesList: TList<string>;
  LSessionData: TSessionData;
  LIsValid: Boolean;
  LJWTValue: TJWT;
  LCustomPair: TPair<string, string>;
  LJsonObject: TJDOJsonObject;
  lJObj: TJsonObject;
begin
  if SameText(AContext.Request.PathInfo, FLoginURLSegment) and (AContext.Request.HTTPMethod in [httpPOST, httpPUT]) then
  begin
    LBasicAuthHeader := AContext.Request.Headers[TMVCJWTDefaults.AUTHORIZATION_HEADER];
    if LBasicAuthHeader.IsEmpty then
    begin
      // read from headers
      LUsername := TNetEncoding.URL.Decode(AContext.Request.Headers[FUserNameElement]);
      LPassword := TNetEncoding.URL.Decode(AContext.Request.Headers[FPasswordElement]);

      // read from content
      if LUsername.IsEmpty and not SameText(AContext.Request.ContentMediaType, TMVCMediaType.APPLICATION_JSON) then
      begin
        LUsername := AContext.Request.ContentParam(FUserNameElement);
        LPassword := AContext.Request.ContentParam(FPasswordElement);
      end;

      // read from json content
      if LUsername.IsEmpty then
      begin
        lJObj := StrToJSONObject(AContext.Request.Body, False);
        try
          if Assigned(lJObj) then
          begin
            LUsername := lJObj.S[FUserNameElement];
            LPassword := lJObj.S[FPasswordElement];
            if LUsername.IsEmpty then
            begin
              LUsername := lJObj.S['username'];
              LPassword := lJObj.S['password'];
            end;
          end;
        finally
          lJObj.Free;
        end;
      end;

      if (LUsername.IsEmpty) or (LPassword.IsEmpty) then
        raise EMVCJWTException.Create(HTTP_STATUS.Unauthorized, 'Username and password required');
    end
    else
    begin
      if not LBasicAuthHeader.StartsWith('basic', True) then
      begin
        RedirectToLoginURL(AContext, 'Invalid authorization type');
      end;

      LBasicAuthHeader := LBasicAuthHeader.Remove(0, 'basic'.Length).Trim;
      LBasicAuthParts := TBase64Encoding.Base64.Decode(LBasicAuthHeader).Split([':']);

      if Length(LBasicAuthParts) <> 2 then
      begin
        RedirectToLoginURL(AContext, 'Invalid authorization type');
      end;

      LUsername := LBasicAuthParts[0];
      LPassword := LBasicAuthParts[1];
    end;

    // check the authorization for the requested resource
    LRolesList := TList<string>.Create;
    try
      LSessionData := TSessionData.Create;
      try
        if Assigned(FAuthenticationHandler) then
        begin
          FAuthenticationHandler.OnAuthentication(AContext, LUsername, LPassword, LRolesList, LIsValid, LSessionData);
          if not LIsValid then
          begin
            RedirectToLoginURL(AContext, 'Unauthorized');
          end;
        end;

        LJWTValue := TJWT.Create(FSecret, FLeewaySeconds);
        try
          // let's user config claims and custom claims
          if not Assigned(FSetupJWTClaims) then
            raise EMVCJWTException.Create('SetupJWTClaims not set');
          LJWTValue.Data := AContext.Request;
          FSetupJWTClaims(LJWTValue);
          FTokenHttpOnlyExpires := LJWTValue.Claims.ExpirationTime;

          // these claims are mandatory and managed by the middleware
          if not LJWTValue.CustomClaims['username'].IsEmpty then
            raise EMVCJWTException.Create
              ('Custom claim "username" is reserved and cannot be modified in the JWT setup');

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
            InternalRender(LJsonObject, TMVCMediaType.APPLICATION_JSON, TMVCConstants.DEFAULT_CONTENT_CHARSET,
              AContext, False);
          finally
            LJsonObject.Free;
          end;
          AHandled := True;
        finally
          LJWTValue.Free;
        end;
      finally
        LSessionData.Free;
      end;
    finally
      LRolesList.Free;
    end;
  end
  else
  begin
    if SameText(AContext.Request.PathInfo, FLogoffURLSegment) and (FUseHttpOnly) then
    begin
      SendLogoffRender(AContext);
      AHandled := True;
    end;
  end;
end;


procedure TMVCJWTCookieAuthenticationMiddleware.RedirectToLoginURL(const AContext: TWebContext; const Message: String);
begin
  if not SameText(AContext.Request.PathInfo, FOnRedirectURL) then
  begin
    AContext.Response.RawWebResponse.SendRedirect(FOnRedirectURL + '?msg=' + TNetEncoding.URL.Encode(Message));
  end;
end;

constructor TMVCJWTBlackListMiddleware.Create(
  OnAcceptToken: TMVCOnAcceptTokenProc;
  OnNewJWTToBlackList: TMVCOnNewJWTToBlackList;
  BlackListRequestURLSegment: string = '/logout');
begin
  inherited Create;
  fOnAcceptToken := OnAcceptToken;
  fOnNewJWTToBlackList := OnNewJWTToBlackList;
  fBlackListRequestURLSegment := BlackListRequestURLSegment;
  Assert(Assigned(fOnAcceptToken));
  Assert(not fBlackListRequestURLSegment.IsEmpty);
end;

procedure TMVCJWTBlackListMiddleware.OnAfterControllerAction(
      AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      const AHandled: Boolean);
begin
  // Implement as needed
end;

procedure TMVCJWTBlackListMiddleware.OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
begin
  // Implement as needed
end;

procedure TMVCJWTBlackListMiddleware.OnBeforeControllerAction(
  AContext: TWebContext; const AControllerQualifiedClassName,
  AActionName: string; var AHandled: Boolean);
begin
  // Implement as needed
end;

procedure TMVCJWTBlackListMiddleware.OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
var
  lAuthHeader: string;
  lAuthToken: string;
  lAccepted: Boolean;
begin
  lAuthToken := '';
  lAuthHeader := AContext.Request.Headers[TMVCJWTDefaults.AUTHORIZATION_HEADER];
  if not lAuthHeader.IsEmpty then
  begin
    // retrieve the token from the "authentication Bearer" header
    if lAuthHeader.Substring(0, TMVCJWTDefaults.AUTH_SCHEMA.Length).ToLower = 'bearer' then
    begin
      lAuthToken := lAuthHeader.Remove(0, TMVCJWTDefaults.AUTH_SCHEMA.Length).Trim;
      lAuthToken := Trim(TNetEncoding.URL.Decode(lAuthToken));
    end;
  end;

  if SameText(AContext.Request.PathInfo, fBlackListRequestURLSegment) then
  begin
    // add the token in the blacklist
    if lAuthToken.IsEmpty then
    begin
      raise EMVCException.Create(HTTP_STATUS.BadRequest,
        'JWTToken required - cannot blacklist an unknown token');
    end;
    fOnNewJWTToBlackList(AContext, lAuthToken);
    AContext.Response.StatusCode := HTTP_STATUS.NoContent;
    AHandled := True;
  end
  else
  begin
    // just check if token is blacklisted.
    // if the token is not available, just ignore the check
    // remember, here jwtmiddleware already did its job.
    if lAuthToken.IsEmpty then
    begin
      AHandled := False;
    end
    else
    begin
      lAccepted := True;
      fOnAcceptToken(AContext, lAuthToken, lAccepted);
      if not lAccepted then
      begin
        raise EMVCJWTException.Create(HTTP_STATUS.Forbidden, 'JWT not accepted');
      end;
    end;
  end;
end;


end.
