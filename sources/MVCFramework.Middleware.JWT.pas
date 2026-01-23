// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
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
  JsonDataObjects,
  MVCFramework.HMAC,
  Web.HTTPApp;

type
  /// <summary>
  /// SameSite cookie attribute for CSRF protection
  /// </summary>
  TMVCJWTCookieSameSite = (
    /// <summary>
    /// Cookie only sent in first-party context (maximum security)
    /// </summary>
    ssStrict,
    /// <summary>
    /// Cookie sent with top-level navigations and GET from third-party sites
    /// </summary>
    ssLax,
    /// <summary>
    /// Cookie sent in all contexts (requires Secure=True)
    /// </summary>
    ssNone
  );

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

  TMVCJWTAuthenticationMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    FAuthenticationHandler: IMVCAuthenticationHandler;
    FClaimsToChecks: TJWTCheckableClaims;
    FSetupJWTClaims: TJWTClaimsSetup;
    FSecret: string;
    FLeewaySeconds: Cardinal;
    FLoginURLSegment: string;
    FAuthorizationHeaderName: string;
    FAuthorizationAccessToken: string;
    FUserNameHeaderName: string;
    FPasswordHeaderName: string;
    FHMACAlgorithm: String;
    FUseHttpOnly: Boolean;
    FTokenHttpOnlyExpires: TDateTime;
    FLogoffURLSegment: string;
    procedure SendLogoffRender(AContext: TWebContext);
  protected
    function NeedsToBeExtended(const JWTValue: TJWT): Boolean;
    procedure ExtendExpirationTime(const JWTValue: TJWT);
    procedure InternalRender(AJSONOb: TJDOJsonObject; AMediaType: string; AContentCharset: string;
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
      ASecret: string = 'D3lph1MVCFram3w0rk';
      ALoginURLSegment: string = '/login';
      AClaimsToCheck: TJWTCheckableClaims = [];
      ALeewaySeconds: Cardinal = 300;
      AHMACAlgorithm: String = HMAC_HS512); overload; virtual;
    constructor Create(AAuthenticationHandler: IMVCAuthenticationHandler;
      AConfigClaims: TJWTClaimsSetup;
      AUseHttpOnly: Boolean;
      ALogoffURLSegment: string = '/logoff';
      ASecret: string = 'D3lph1MVCFram3w0rk';
      ALoginURLSegment: string = '/login';
      AClaimsToCheck: TJWTCheckableClaims = [];
      ALeewaySeconds: Cardinal = 300;
      AHMACAlgorithm: String = HMAC_HS512); overload; virtual;
      deprecated 'Use TMVCJWTCookieAuthenticationMiddleware for secure cookie-based JWT authentication';
    property AuthorizationHeaderName: string read FAuthorizationHeaderName;
    property UserNameHeaderName: string read FUserNameHeaderName;
    property PasswordHeaderName: string read FPasswordHeaderName;
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

  /// <summary>
  /// Secure JWT authentication middleware using HTTP-only cookies.
  /// This middleware provides secure-by-default settings for cookie-based JWT authentication.
  /// Default settings: Secure=True, SameSite=Strict, HttpOnly=True
  /// </summary>
  TMVCJWTCookieAuthenticationMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    FAuthenticationHandler: IMVCAuthenticationHandler;
    FClaimsToChecks: TJWTCheckableClaims;
    FSetupJWTClaims: TJWTClaimsSetup;
    FSecret: string;
    FLeewaySeconds: Cardinal;
    FLoginURLSegment: string;
    FLogoutURLSegment: string;
    FHMACAlgorithm: String;
    // Cookie settings with secure defaults
    FCookieName: string;
    FCookieSecure: Boolean;
    FCookieSameSite: TMVCJWTCookieSameSite;
    FCookiePath: string;
    FCookieDomain: string;
    FTokenExpires: TDateTime;
  protected
    procedure SetCookie(AContext: TWebContext; const AToken: string; AExpires: TDateTime);
    procedure InvalidateCookie(AContext: TWebContext);
    function GetTokenFromCookie(AContext: TWebContext): string;
    function NeedsToBeExtended(const JWTValue: TJWT): Boolean;
    procedure ExtendExpirationTime(const JWTValue: TJWT);
    procedure RenderLoginResponse(AContext: TWebContext; const AToken: string);
    procedure RenderLogoutResponse(AContext: TWebContext);
    // IMVCMiddleware
    procedure OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
    procedure OnBeforeControllerAction(AContext: TWebContext;
      const AControllerQualifiedClassName: string;
      const AActionName: string; var AHandled: Boolean);
    procedure OnAfterControllerAction(AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      const AHandled: Boolean);
    procedure OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
  public
    constructor Create(
      AAuthenticationHandler: IMVCAuthenticationHandler;
      AConfigClaims: TJWTClaimsSetup;
      ASecret: string;
      ALoginURLSegment: string = '/login';
      ALogoutURLSegment: string = '/logout';
      AClaimsToCheck: TJWTCheckableClaims = [];
      ALeewaySeconds: Cardinal = 300;
      AHMACAlgorithm: String = HMAC_HS512
    ); virtual;
    /// <summary>
    /// Sets the cookie name (default: 'jwt_token')
    /// </summary>
    function SetCookieName(const AName: string): TMVCJWTCookieAuthenticationMiddleware;
    /// <summary>
    /// Sets the Secure flag (default: True - requires HTTPS)
    /// Set to False only for local development over HTTP
    /// </summary>
    function SetCookieSecure(ASecure: Boolean): TMVCJWTCookieAuthenticationMiddleware;
    /// <summary>
    /// Sets the SameSite attribute (default: ssStrict - maximum CSRF protection)
    /// Use ssLax if you need cross-site top-level navigation
    /// Use ssNone only if absolutely necessary (requires Secure=True)
    /// </summary>
    function SetCookieSameSite(ASameSite: TMVCJWTCookieSameSite): TMVCJWTCookieAuthenticationMiddleware;
    /// <summary>
    /// Sets the cookie path (default: '/')
    /// </summary>
    function SetCookiePath(const APath: string): TMVCJWTCookieAuthenticationMiddleware;
    /// <summary>
    /// Sets the cookie domain (default: '' - current domain)
    /// </summary>
    function SetCookieDomain(const ADomain: string): TMVCJWTCookieAuthenticationMiddleware;
  end;

  function UseJWTMiddleware(
      aAuthenticationHandler: IMVCAuthenticationHandler;
      aConfigClaims: TJWTClaimsSetup;
      aSecret: string = 'D3lph1MVCFram3w0rk';
      aLoginURLSegment: string = '/loginff';
      aClaimsToCheck: TJWTCheckableClaims = [];
      aLeewaySeconds: Cardinal = 300;
      aHMACAlgorithm: String = HMAC_HS512): IMVCMiddleware;

  function UseJWTMiddlewareWithHTTPOnlyCookie(
      aAuthenticationHandler: IMVCAuthenticationHandler;
      aConfigClaims: TJWTClaimsSetup;
      aSecret: string = 'D3lph1MVCFram3w0rk';
      aLoginURLSegment: string = '/login';
      aLogoutURLSegment: string = '/logoff';
      aClaimsToCheck: TJWTCheckableClaims = [];
      aLeewaySeconds: Cardinal = 300;
      aHMACAlgorithm: String = HMAC_HS512): IMVCMiddleware;
      deprecated 'Use UseJWTCookieAuthentication for secure cookie-based JWT authentication';

  /// <summary>
  /// Creates a secure JWT authentication middleware using HTTP-only cookies.
  /// This is the recommended way to implement cookie-based JWT authentication.
  /// Default settings provide maximum security: Secure=True, SameSite=Strict, HttpOnly=True
  /// </summary>
  /// <example>
  /// Engine.AddMiddleware(
  ///   UseJWTCookieAuthentication(MyAuthHandler, MyClaimsSetup, 'my-secret')
  /// );
  ///
  /// // For local development over HTTP:
  /// Engine.AddMiddleware(
  ///   UseJWTCookieAuthentication(MyAuthHandler, MyClaimsSetup, 'my-secret')
  ///     .SetCookieSecure(False)
  /// );
  /// </example>
  function UseJWTCookieAuthentication(
      AAuthenticationHandler: IMVCAuthenticationHandler;
      AConfigClaims: TJWTClaimsSetup;
      ASecret: string;
      ALoginURLSegment: string = '/login';
      ALogoutURLSegment: string = '/logout';
      AClaimsToCheck: TJWTCheckableClaims = [];
      ALeewaySeconds: Cardinal = 300;
      AHMACAlgorithm: String = HMAC_HS512
  ): TMVCJWTCookieAuthenticationMiddleware;

  function UseJWTBlackListMiddleware(
      OnAcceptToken: TMVCOnAcceptTokenProc;
      OnNewJWTToBlackList: TMVCOnNewJWTToBlackList;
      BlackListRequestURLSegment: string = '/logout'
    ): IMVCMiddleware;

implementation

uses
  System.NetEncoding,
  System.DateUtils,
  System.Math,
  MVCFramework.Logger;

function UseJWTMiddleware(
      aAuthenticationHandler: IMVCAuthenticationHandler;
      aConfigClaims: TJWTClaimsSetup;
      aSecret: string = 'D3lph1MVCFram3w0rk';
      aLoginURLSegment: string = '/loginff';
      aClaimsToCheck: TJWTCheckableClaims = [];
      aLeewaySeconds: Cardinal = 300;
      aHMACAlgorithm: String = HMAC_HS512): IMVCMiddleware;
begin
  Result := TMVCJWTAuthenticationMiddleware.Create(
    aAuthenticationHandler, aConfigClaims, aSecret, aLoginURLSegment, aClaimsToCheck, aLeewaySeconds, aHMACAlgorithm);
end;


function UseJWTMiddlewareWithHTTPOnlyCookie(
    aAuthenticationHandler: IMVCAuthenticationHandler;
    aConfigClaims: TJWTClaimsSetup;
    aSecret: string = 'D3lph1MVCFram3w0rk';
    aLoginURLSegment: string = '/login';
    aLogoutURLSegment: string = '/logoff';
    aClaimsToCheck: TJWTCheckableClaims = [];
    aLeewaySeconds: Cardinal = 300;
    aHMACAlgorithm: String = HMAC_HS512): IMVCMiddleware;
begin
  Result := TMVCJWTAuthenticationMiddleware.Create(
    aAuthenticationHandler, aConfigClaims, True, aLogoutURLSegment, aSecret, aLoginURLSegment, aClaimsToCheck, aLeewaySeconds, aHMACAlgorithm);
end;

function UseJWTBlackListMiddleware(
    OnAcceptToken: TMVCOnAcceptTokenProc;
    OnNewJWTToBlackList: TMVCOnNewJWTToBlackList;
    BlackListRequestURLSegment: string = '/logout'
  ): IMVCMiddleware;
begin
  Result := TMVCJWTBlackListMiddleware.Create(OnAcceptToken, OnNewJWTToBlackList, BlackListRequestURLSegment);
end;

function UseJWTCookieAuthentication(
    AAuthenticationHandler: IMVCAuthenticationHandler;
    AConfigClaims: TJWTClaimsSetup;
    ASecret: string;
    ALoginURLSegment: string = '/login';
    ALogoutURLSegment: string = '/logout';
    AClaimsToCheck: TJWTCheckableClaims = [];
    ALeewaySeconds: Cardinal = 300;
    AHMACAlgorithm: String = HMAC_HS512
): TMVCJWTCookieAuthenticationMiddleware;
begin
  Result := TMVCJWTCookieAuthenticationMiddleware.Create(
    AAuthenticationHandler,
    AConfigClaims,
    ASecret,
    ALoginURLSegment,
    ALogoutURLSegment,
    AClaimsToCheck,
    ALeewaySeconds,
    AHMACAlgorithm
  );
end;


{ TMVCJWTAuthenticationMiddleware }

constructor TMVCJWTAuthenticationMiddleware.Create(
  AAuthenticationHandler: IMVCAuthenticationHandler;
  AConfigClaims: TJWTClaimsSetup;
  ASecret, ALoginURLSegment: string;
  AClaimsToCheck: TJWTCheckableClaims;
  ALeewaySeconds: Cardinal;
  AHMACAlgorithm: String
  );
begin
  inherited Create;
  FAuthenticationHandler := AAuthenticationHandler;
  FSetupJWTClaims := AConfigClaims;
  FClaimsToChecks := AClaimsToCheck;
  FSecret := ASecret;
  FLoginURLSegment := ALoginURLSegment;
  FLeewaySeconds := ALeewaySeconds;
  FAuthorizationHeaderName := TMVCJWTDefaults.AUTHORIZATION_HEADER;
  FAuthorizationAccessToken := TMVCJWTDefaults.AUTHORIZATION_ACCESS_TOKEN;
  FUserNameHeaderName := TMVCJWTDefaults.USERNAME_HEADER;
  FPasswordHeaderName := TMVCJWTDefaults.PASSWORD_HEADER;
  FHMACAlgorithm := AHMACAlgorithm;
  FUseHttpOnly := False;
  FTokenHttpOnlyExpires := Now;
end;

constructor TMVCJWTAuthenticationMiddleware.Create(AAuthenticationHandler: IMVCAuthenticationHandler; AConfigClaims: TJWTClaimsSetup; AUseHttpOnly: Boolean; ALogoffURLSegment: string;
  ASecret, ALoginURLSegment: string; AClaimsToCheck: TJWTCheckableClaims; ALeewaySeconds: Cardinal; AHMACAlgorithm: String);
begin
  Create(AAuthenticationHandler, AConfigClaims, ASecret, ALoginURLSegment, AClaimsToCheck, ALeewaySeconds, AHMACAlgorithm);
  FUseHttpOnly := AUseHttpOnly;
  FLogoffURLSegment := ALogoffURLSegment;
end;

procedure TMVCJWTAuthenticationMiddleware.ExtendExpirationTime(const JWTValue: TJWT);
begin
  JWTValue.Claims.ExpirationTime := Max(JWTValue.Claims.ExpirationTime, Now) +
    (JWTValue.LeewaySeconds + JWTValue.LiveValidityWindowInSeconds) * OneSecond;
  if FUseHttpOnly then
  begin
    FTokenHttpOnlyExpires := JWTValue.Claims.ExpirationTime;
  end;
end;

procedure TMVCJWTAuthenticationMiddleware.InternalRender(AJSONOb: TJDOJsonObject;
  AMediaType, AContentCharset: string; AContext: TWebContext; AInstanceOwner: Boolean);
var
  lEncoding: TEncoding;
  lContentType, lJValue: string;
  lCookie: TCookie;
begin
  lJValue := AJSONOb.ToJSON;

  if FUseHttpOnly then
  begin
    lCookie := AContext.Response.Cookies.Add;
    lCookie.Expires := FTokenHttpOnlyExpires;
    lCookie.Path := '/';
    lCookie.Name := 'token';
    lCookie.Value := AJSONOb.S['token'];
    lCookie.HttpOnly := True;
    // Cookie.Secure := True;
    // Cookie.SameSite := 'none';
  end;

  lContentType := BuildContentType(AMediaType, AContentCharset);
  AContext.Response.RawWebResponse.ContentType := lContentType;

  lEncoding := TEncoding.GetEncoding(AContentCharset);
  try
    AContext.Response.SetContentStream(TBytesStream.Create(TEncoding.Convert(TEncoding.Default, lEncoding,
      TEncoding.Default.GetBytes(lJValue))), lContentType);
  finally
    lEncoding.Free;
  end;

  if AInstanceOwner then
    FreeAndNil(AJSONOb)
end;

procedure TMVCJWTAuthenticationMiddleware.SendLogoffRender(AContext: TWebContext);
const
  ReturnMessage = '{ "message": "Successful logout" }';
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

function TMVCJWTAuthenticationMiddleware.NeedsToBeExtended(const JWTValue: TJWT): Boolean;
var
  lWillExpireIn: Int64;
begin
  lWillExpireIn := SecondsBetween(Now, JWTValue.Claims.ExpirationTime);
  Result := lWillExpireIn <= JWTValue.LiveValidityWindowInSeconds;
end;

procedure TMVCJWTAuthenticationMiddleware.OnAfterControllerAction(AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      const AHandled: Boolean);
begin
  // Implement as needed
end;

procedure TMVCJWTAuthenticationMiddleware.OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
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
  AuthAccessToken: string;
  AuthToken: string;
  ErrorMsg: string;
  CookieToken: string;
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
      // retrieve the token from the "authentication Bearer" header
      AuthHeader := AContext.Request.Headers[FAuthorizationHeaderName];
      if AuthHeader.IsEmpty then
        // retrieve the token from the "access_token" query param
        AuthHeader := AContext.Request.Params[FAuthorizationAccessToken];

      if not AuthHeader.IsEmpty then
      begin
        { load and verify token even for an action that doesn't require it }
        JWTValue := TJWT.Create(FSecret, FLeewaySeconds);
        try
          JWTValue.RegClaimsToChecks := Self.FClaimsToChecks;

          // retrieve the token from the "authentication Bearer" header
          AuthToken := '';
          if AuthHeader.Substring(0, TMVCJWTDefaults.AUTH_SCHEMA.Length).ToLower = 'bearer' then
          begin
            AuthToken := AuthHeader.Remove(0, TMVCJWTDefaults.AUTH_SCHEMA.Length).Trim;
            AuthToken := Trim(TNetEncoding.URL.Decode(AuthToken));
          end;

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
    // retrieve the token from the "authentication Bearer" header
    AuthHeader := AContext.Request.Headers[FAuthorizationHeaderName];
    if (not AuthHeader.IsEmpty) then
    begin
      AuthToken := '';
      if AuthHeader.Substring(0, TMVCJWTDefaults.AUTH_SCHEMA.Length).ToLower = 'bearer' then
      begin
        AuthToken := AuthHeader.Remove(0, TMVCJWTDefaults.AUTH_SCHEMA.Length).Trim;
        AuthToken := Trim(TNetEncoding.URL.Decode(AuthToken));
      end;
    end
    else
    begin
      // retrieve the token from the "access_token" query param
      AuthAccessToken := AContext.Request.Params[FAuthorizationAccessToken];
      if (not AuthAccessToken.IsEmpty) then
      begin
        AuthToken := AuthAccessToken.Trim;
        AuthToken := Trim(TNetEncoding.URL.Decode(AuthToken));
      end
      else
      begin
        if FUseHttpOnly then
        begin
          CookieToken := AContext.Request.Cookie('token');
          if (not CookieToken.IsEmpty) then
          begin
            AuthToken := CookieToken.Trim;
            AuthToken := Trim(TNetEncoding.URL.Decode(AuthToken));
          end;
        end;
      end;
    end;

    if AuthToken.IsEmpty then
      raise EMVCJWTException.Create(HTTP_STATUS.Unauthorized, 'Authorization Required');
    if not JWTValue.LoadToken(AuthToken, ErrorMsg) then
      raise EMVCJWTException.Create(HTTP_STATUS.Unauthorized, ErrorMsg);

    if JWTValue.CustomClaims['username'].IsEmpty then
      raise EMVCJWTException.Create(HTTP_STATUS.Unauthorized, 'Invalid Token, Authorization Required');

    AContext.LoggedUser.UserName := JWTValue.CustomClaims['username'];
    AContext.LoggedUser.Roles.AddRange(JWTValue.CustomClaims['roles'].Split([',']));
    AContext.LoggedUser.LoggedSince := JWTValue.Claims.IssuedAt;
    AContext.LoggedUser.CustomData := JWTValue.CustomClaims.AsCustomData;

    if Assigned(FAuthenticationHandler) then
    begin
      FAuthenticationHandler.OnAuthorization(AContext, AContext.LoggedUser.Roles, AControllerQualifiedClassName,
        AActionName, IsAuthorized);
      if not IsAuthorized then
        raise EMVCJWTException.Create(HTTP_STATUS.Forbidden, 'Authorization Forbidden');
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
  if SameText(AContext.Request.PathInfo, FLoginURLSegment) then
  begin
    LBasicAuthHeader := AContext.Request.Headers[FAuthorizationHeaderName];
    if LBasicAuthHeader.IsEmpty then
    begin
      // read from headers
      LUsername := TNetEncoding.URL.Decode(AContext.Request.Headers[FUserNameHeaderName]);
      LPassword := TNetEncoding.URL.Decode(AContext.Request.Headers[FPasswordHeaderName]);

      // read from content
      if LUsername.IsEmpty and not SameText(AContext.Request.ContentMediaType, TMVCMediaType.APPLICATION_JSON) then
      begin
        LUsername := AContext.Request.ContentParam(FUserNameHeaderName);
        LPassword := AContext.Request.ContentParam(FPasswordHeaderName);
      end;

      // read from json content
      if LUsername.IsEmpty then
      begin
        lJObj := StrToJSONObject(AContext.Request.Body, False);
        try
          if Assigned(lJObj) then
          begin
            LUsername := lJObj.S[FUserNameHeaderName];
            LPassword := lJObj.S[FPasswordHeaderName];
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
        raise EMVCJWTException.Create(HTTP_STATUS.Unauthorized, 'Invalid authorization type');

      LBasicAuthHeader := LBasicAuthHeader.Remove(0, 'basic'.Length).Trim;
      LBasicAuthParts := TBase64Encoding.Base64.Decode(LBasicAuthHeader).Split([':']);

      if Length(LBasicAuthParts) <> 2 then
        raise EMVCJWTException.Create(HTTP_STATUS.Unauthorized, 'Invalid authorization type');

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
            raise EMVCJWTException.Create(HTTP_STATUS.Unauthorized, 'Unauthorized');
        end;

        LJWTValue := TJWT.Create(FSecret, FLeewaySeconds);
        try
          // let's user config claims and custom claims
          if not Assigned(FSetupJWTClaims) then
            raise EMVCJWTException.Create('SetupJWTClaims not set');
          LJWTValue.Data := AContext.Request;
          FSetupJWTClaims(LJWTValue);

          if FUseHttpOnly then
          begin
            FTokenHttpOnlyExpires := LJWTValue.Claims.ExpirationTime;
          end;

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


{ TMVCJWTCookieAuthenticationMiddleware }

constructor TMVCJWTCookieAuthenticationMiddleware.Create(
  AAuthenticationHandler: IMVCAuthenticationHandler;
  AConfigClaims: TJWTClaimsSetup;
  ASecret: string;
  ALoginURLSegment: string;
  ALogoutURLSegment: string;
  AClaimsToCheck: TJWTCheckableClaims;
  ALeewaySeconds: Cardinal;
  AHMACAlgorithm: String);
begin
  inherited Create;
  FAuthenticationHandler := AAuthenticationHandler;
  FSetupJWTClaims := AConfigClaims;
  FSecret := ASecret;
  FLoginURLSegment := ALoginURLSegment;
  FLogoutURLSegment := ALogoutURLSegment;
  FClaimsToChecks := AClaimsToCheck;
  FLeewaySeconds := ALeewaySeconds;
  FHMACAlgorithm := AHMACAlgorithm;
  // Secure defaults
  FCookieName := 'jwt_token';
  FCookieSecure := True;
  FCookieSameSite := ssStrict;
  FCookiePath := '/';
  FCookieDomain := '';
  FTokenExpires := 0;
end;

function TMVCJWTCookieAuthenticationMiddleware.SetCookieName(
  const AName: string): TMVCJWTCookieAuthenticationMiddleware;
begin
  FCookieName := AName;
  Result := Self;
end;

function TMVCJWTCookieAuthenticationMiddleware.SetCookieSecure(
  ASecure: Boolean): TMVCJWTCookieAuthenticationMiddleware;
begin
  // SameSite=None requires Secure=True per browser requirements
  if (not ASecure) and (FCookieSameSite = ssNone) then
    raise EMVCException.Create('Cannot set Secure=False when SameSite=None. ' +
      'Browsers require Secure=True for SameSite=None cookies.');
  FCookieSecure := ASecure;
  Result := Self;
end;

function TMVCJWTCookieAuthenticationMiddleware.SetCookieSameSite(
  ASameSite: TMVCJWTCookieSameSite): TMVCJWTCookieAuthenticationMiddleware;
begin
  // SameSite=None requires Secure=True per browser requirements
  if (ASameSite = ssNone) and (not FCookieSecure) then
    raise EMVCException.Create('SameSite=None requires Secure=True. ' +
      'Call SetCookieSecure(True) before setting SameSite=None.');
  FCookieSameSite := ASameSite;
  Result := Self;
end;

function TMVCJWTCookieAuthenticationMiddleware.SetCookiePath(
  const APath: string): TMVCJWTCookieAuthenticationMiddleware;
begin
  FCookiePath := APath;
  Result := Self;
end;

function TMVCJWTCookieAuthenticationMiddleware.SetCookieDomain(
  const ADomain: string): TMVCJWTCookieAuthenticationMiddleware;
begin
  FCookieDomain := ADomain;
  Result := Self;
end;

procedure TMVCJWTCookieAuthenticationMiddleware.SetCookie(
  AContext: TWebContext; const AToken: string; AExpires: TDateTime);
var
  lCookie: TCookie;
  lSameSite: string;
begin
  lCookie := AContext.Response.Cookies.Add;
  lCookie.Name := FCookieName;
  lCookie.Value := AToken;
  lCookie.Path := FCookiePath;
  lCookie.Expires := AExpires;
  lCookie.HttpOnly := True;  // Always True for XSS protection
  lCookie.Secure := FCookieSecure;
  if not FCookieDomain.IsEmpty then
    lCookie.Domain := FCookieDomain;

  // Set SameSite attribute
  case FCookieSameSite of
    ssStrict: lSameSite := 'Strict';
    ssLax: lSameSite := 'Lax';
    ssNone: lSameSite := 'None';
  end;
  {$IF CompilerVersion >= 34.0}  // Delphi 10.4 Sydney and later
  lCookie.SameSite := lSameSite;
  {$ENDIF}
end;

procedure TMVCJWTCookieAuthenticationMiddleware.InvalidateCookie(AContext: TWebContext);
begin
  // Set cookie with empty value and past expiration date
  SetCookie(AContext, '', EncodeDate(1970, 1, 1));
end;

function TMVCJWTCookieAuthenticationMiddleware.GetTokenFromCookie(
  AContext: TWebContext): string;
begin
  Result := Trim(TNetEncoding.URL.Decode(AContext.Request.Cookie(FCookieName)));
end;

function TMVCJWTCookieAuthenticationMiddleware.NeedsToBeExtended(
  const JWTValue: TJWT): Boolean;
var
  lWillExpireIn: Int64;
begin
  lWillExpireIn := SecondsBetween(Now, JWTValue.Claims.ExpirationTime);
  Result := lWillExpireIn <= JWTValue.LiveValidityWindowInSeconds;
end;

procedure TMVCJWTCookieAuthenticationMiddleware.ExtendExpirationTime(
  const JWTValue: TJWT);
begin
  JWTValue.Claims.ExpirationTime := Max(JWTValue.Claims.ExpirationTime, Now) +
    (JWTValue.LeewaySeconds + JWTValue.LiveValidityWindowInSeconds) * OneSecond;
  FTokenExpires := JWTValue.Claims.ExpirationTime;
end;

procedure TMVCJWTCookieAuthenticationMiddleware.RenderLoginResponse(
  AContext: TWebContext; const AToken: string);
var
  lJsonObject: TJDOJsonObject;
  lEncoding: TEncoding;
  lContentType: string;
begin
  // Set the secure cookie
  SetCookie(AContext, AToken, FTokenExpires);

  // Render JSON response
  lJsonObject := TJDOJsonObject.Create;
  try
    lJsonObject.S['token'] := AToken;
    lContentType := BuildContentType(TMVCMediaType.APPLICATION_JSON, TMVCConstants.DEFAULT_CONTENT_CHARSET);
    AContext.Response.RawWebResponse.ContentType := lContentType;

    lEncoding := TEncoding.GetEncoding(TMVCConstants.DEFAULT_CONTENT_CHARSET);
    try
      AContext.Response.SetContentStream(
        TBytesStream.Create(
          TEncoding.Convert(TEncoding.Default, lEncoding, TEncoding.Default.GetBytes(lJsonObject.ToJSON))
        ),
        lContentType
      );
    finally
      lEncoding.Free;
    end;
  finally
    lJsonObject.Free;
  end;
end;

procedure TMVCJWTCookieAuthenticationMiddleware.RenderLogoutResponse(AContext: TWebContext);
const
  ReturnMessage = '{ "message": "Successful logout" }';
  ContentType = 'application/json; charset=UTF-8';
var
  lEncoding: TEncoding;
begin
  // Invalidate the cookie
  InvalidateCookie(AContext);

  // Render JSON response
  AContext.Response.RawWebResponse.ContentType := ContentType;
  lEncoding := TEncoding.UTF8;
  AContext.Response.SetContentStream(
    TBytesStream.Create(lEncoding.GetBytes(ReturnMessage)),
    ContentType
  );
end;

procedure TMVCJWTCookieAuthenticationMiddleware.OnBeforeRouting(
  AContext: TWebContext; var AHandled: Boolean);
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
  lJObj: TJsonObject;
begin
  // Handle login
  if SameText(AContext.Request.PathInfo, FLoginURLSegment) then
  begin
    LBasicAuthHeader := AContext.Request.Headers[TMVCJWTDefaults.AUTHORIZATION_HEADER];
    if LBasicAuthHeader.IsEmpty then
    begin
      // read from headers
      LUsername := TNetEncoding.URL.Decode(AContext.Request.Headers[TMVCJWTDefaults.USERNAME_HEADER]);
      LPassword := TNetEncoding.URL.Decode(AContext.Request.Headers[TMVCJWTDefaults.PASSWORD_HEADER]);

      // read from content
      if LUsername.IsEmpty and not SameText(AContext.Request.ContentMediaType, TMVCMediaType.APPLICATION_JSON) then
      begin
        LUsername := AContext.Request.ContentParam(TMVCJWTDefaults.USERNAME_HEADER);
        LPassword := AContext.Request.ContentParam(TMVCJWTDefaults.PASSWORD_HEADER);
      end;

      // read from json content
      if LUsername.IsEmpty then
      begin
        lJObj := StrToJSONObject(AContext.Request.Body, False);
        try
          if Assigned(lJObj) then
          begin
            LUsername := lJObj.S[TMVCJWTDefaults.USERNAME_HEADER];
            LPassword := lJObj.S[TMVCJWTDefaults.PASSWORD_HEADER];
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
        raise EMVCJWTException.Create(HTTP_STATUS.Unauthorized, 'Invalid authorization type');

      LBasicAuthHeader := LBasicAuthHeader.Remove(0, 'basic'.Length).Trim;
      LBasicAuthParts := TBase64Encoding.Base64.Decode(LBasicAuthHeader).Split([':']);

      if Length(LBasicAuthParts) <> 2 then
        raise EMVCJWTException.Create(HTTP_STATUS.Unauthorized, 'Invalid authorization type');

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
            raise EMVCJWTException.Create(HTTP_STATUS.Unauthorized, 'Unauthorized');
        end;

        LJWTValue := TJWT.Create(FSecret, FLeewaySeconds);
        try
          // let's user config claims and custom claims
          if not Assigned(FSetupJWTClaims) then
            raise EMVCJWTException.Create('SetupJWTClaims not set');
          LJWTValue.Data := AContext.Request;
          FSetupJWTClaims(LJWTValue);

          FTokenExpires := LJWTValue.Claims.ExpirationTime;

          // these claims are mandatory and managed by the middleware
          if not LJWTValue.CustomClaims['username'].IsEmpty then
            raise EMVCJWTException.Create(
              'Custom claim "username" is reserved and cannot be modified in the JWT setup');

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

          RenderLoginResponse(AContext, LJWTValue.GetToken);
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
  // Handle logout
  else if SameText(AContext.Request.PathInfo, FLogoutURLSegment) then
  begin
    RenderLogoutResponse(AContext);
    AHandled := True;
  end;
end;

procedure TMVCJWTCookieAuthenticationMiddleware.OnBeforeControllerAction(
  AContext: TWebContext;
  const AControllerQualifiedClassName, AActionName: string;
  var AHandled: Boolean);
var
  AuthRequired: Boolean;
  IsAuthorized: Boolean;
  JWTValue: TJWT;
  AuthToken: string;
  ErrorMsg: string;
begin
  // check if the resource is protected
  if Assigned(FAuthenticationHandler) then
  begin
    FAuthenticationHandler.OnRequest(AContext, AControllerQualifiedClassName, AActionName, AuthRequired);
    if not AuthRequired then
    begin
      AHandled := False;
      // Load token for non-protected resources to support LoggedUser.IsValid
      AuthToken := GetTokenFromCookie(AContext);
      if not AuthToken.IsEmpty then
      begin
        JWTValue := TJWT.Create(FSecret, FLeewaySeconds);
        try
          JWTValue.RegClaimsToChecks := Self.FClaimsToChecks;
          if JWTValue.LoadToken(AuthToken, ErrorMsg) then
          begin
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

  // Verify token from cookie
  JWTValue := TJWT.Create(FSecret, FLeewaySeconds);
  try
    JWTValue.RegClaimsToChecks := Self.FClaimsToChecks;
    AuthToken := GetTokenFromCookie(AContext);

    if AuthToken.IsEmpty then
      raise EMVCJWTException.Create(HTTP_STATUS.Unauthorized, 'Authorization Required');
    if not JWTValue.LoadToken(AuthToken, ErrorMsg) then
      raise EMVCJWTException.Create(HTTP_STATUS.Unauthorized, ErrorMsg);

    if JWTValue.CustomClaims['username'].IsEmpty then
      raise EMVCJWTException.Create(HTTP_STATUS.Unauthorized, 'Invalid Token, Authorization Required');

    AContext.LoggedUser.UserName := JWTValue.CustomClaims['username'];
    AContext.LoggedUser.Roles.AddRange(JWTValue.CustomClaims['roles'].Split([',']));
    AContext.LoggedUser.LoggedSince := JWTValue.Claims.IssuedAt;
    AContext.LoggedUser.CustomData := JWTValue.CustomClaims.AsCustomData;

    if Assigned(FAuthenticationHandler) then
    begin
      FAuthenticationHandler.OnAuthorization(AContext, AContext.LoggedUser.Roles, AControllerQualifiedClassName,
        AActionName, IsAuthorized);
      if not IsAuthorized then
        raise EMVCJWTException.Create(HTTP_STATUS.Forbidden, 'Authorization Forbidden');
    end;

    // Handle token refresh
    if JWTValue.LiveValidityWindowInSeconds > 0 then
    begin
      if NeedsToBeExtended(JWTValue) then
      begin
        ExtendExpirationTime(JWTValue);
        // Update the cookie with the new token
        SetCookie(AContext, JWTValue.GetToken, FTokenExpires);
        // Add header to inform client about token refresh
        AContext.Response.SetCustomHeader('X-JWT-Refreshed', 'true');
      end;
    end;

    AHandled := False;
  finally
    JWTValue.Free;
  end;
end;

procedure TMVCJWTCookieAuthenticationMiddleware.OnAfterControllerAction(
  AContext: TWebContext;
  const AControllerQualifiedClassName: string;
  const AActionName: string;
  const AHandled: Boolean);
begin
  // Implement as needed
end;

procedure TMVCJWTCookieAuthenticationMiddleware.OnAfterRouting(
  AContext: TWebContext;
  const AHandled: Boolean);
begin
  // Implement as needed
end;


end.
