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

{$WARN SYMBOL_DEPRECATED OFF} // the classes are deprecated for their users, not inside their own unit

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
  Web.HTTPApp,
  MVCFramework.Filters.Authentication,
  MVCFramework.Filters.Authentication.Internal;

type
  // Moved to MVCFramework.JWT (the filters use it): re-exported for 3.4 code.
  TMVCJWTCookieSameSite = MVCFramework.JWT.TMVCJWTCookieSameSite;

  // Moved to MVCFramework.Filters.Authentication (the filters implement the middleware): re-exported for 3.4 code.
  TMVCJWTDefaults = MVCFramework.Filters.Authentication.TMVCJWTDefaults;

  // Re-export so callers that uses MVCFramework.Middleware.JWT still see
  // the type without needing to add MVCFramework.JWT explicitly.
  TJWTClaimsSetup = MVCFramework.JWT.TJWTClaimsSetup;

const
  ssStrict = MVCFramework.JWT.ssStrict;
  ssLax = MVCFramework.JWT.ssLax;
  ssNone = MVCFramework.JWT.ssNone;

type

  TMVCJWTAuthenticationMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    fCore: TMVCJWTAuthenticationCore;
    FAuthorizationHeaderName: string;
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
      ASecret: string = MVC_JWT_INSECURE_DEFAULT_SECRET;
      ALoginURLSegment: string = '/login';
      AClaimsToCheck: TJWTCheckableClaims = [];
      ALeewaySeconds: Cardinal = 300;
      AHMACAlgorithm: String = HMAC_HS512); overload; virtual;
    /// <summary>
    /// Creates JWT authentication middleware using a custom signer (e.g., TRSAJWTSigner).
    /// Use this for RS256/RS384/RS512 asymmetric JWT authentication.
    /// </summary>
    constructor Create(AAuthenticationHandler: IMVCAuthenticationHandler;
      AConfigClaims: TJWTClaimsSetup;
      ASigner: IJWTSigner;
      ALoginURLSegment: string = '/login';
      AClaimsToCheck: TJWTCheckableClaims = [];
      ALeewaySeconds: Cardinal = 300); overload; virtual;
    constructor Create(AAuthenticationHandler: IMVCAuthenticationHandler;
      AConfigClaims: TJWTClaimsSetup;
      AUseHttpOnly: Boolean;
      ALogoffURLSegment: string = '/logoff';
      ASecret: string = MVC_JWT_INSECURE_DEFAULT_SECRET;
      ALoginURLSegment: string = '/login';
      AClaimsToCheck: TJWTCheckableClaims = [];
      ALeewaySeconds: Cardinal = 300;
      AHMACAlgorithm: String = HMAC_HS512); overload; virtual;
      // deprecated with the class (a member of a deprecated class cannot carry its own directive, E1030);
      // for a token in a cookie use TMVCJWTCookieAuthenticationMiddleware, or now JWTCookieAuthenticationBundle
    /// <summary>
    /// Name of the request parameter the token is also accepted in, on top of the
    /// Authorization header. Defaults to 'access_token' - the 3.4 behaviour, which
    /// SSE endpoints, &lt;img&gt; tags and download links depend on, none of which can
    /// send a header. Set it to an empty string to accept the token from the header
    /// only: a token in a URL ends up in proxy logs, browser history and Referer.
    /// Note this reads Request.Params, which is wider than the query string: a
    /// route parameter of the same name wins over it, and a form field is read too.
    /// </summary>
    property AuthorizationAccessTokenParamName: string read fCore.AccessTokenParamName
      write fCore.AccessTokenParamName;
    property AuthorizationHeaderName: string read FAuthorizationHeaderName;
    property LoginURLSegment: string read fCore.LoginURLSegment; // the Swagger document reads it
    property UserNameHeaderName: string read FUserNameHeaderName;
    property PasswordHeaderName: string read FPasswordHeaderName;
  end deprecated 'Use the JWT filter bundle (MVCFramework.Filters.Authentication): AEngine.Use(JWTAuthenticationBundle(...))';

  // Moved to MVCFramework.Filters.Authentication (the filters implement the middleware): re-exported for 3.4 code.
  TMVCOnAcceptTokenProc = MVCFramework.Filters.Authentication.TMVCOnAcceptTokenProc;
  TMVCOnNewJWTToBlackList = MVCFramework.Filters.Authentication.TMVCOnNewJWTToBlackList;

  TMVCJWTBlackListMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    fCore: TMVCJWTBlackListCore;
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
    /// <summary>
    /// AccessTokenParamName: the request parameter the token is also read from when
    /// the Authorization header carries none, as AuthorizationAccessTokenParamName of
    /// TMVCJWTAuthenticationMiddleware; give the same name, or a token sent there is
    /// never checked against the blacklist. '' (the default): the header only.
    /// </summary>
    constructor Create(
      OnAcceptToken: TMVCOnAcceptTokenProc;
      OnNewJWTToBlackList: TMVCOnNewJWTToBlackList;
      BlackListRequestURLSegment: string = '/logout';
      AccessTokenParamName: string = '');
  end deprecated 'Use the blacklist of the JWT filter bundle (MVCFramework.Filters.Authentication): OnAcceptToken and OnNewJWTToBlackList in the options of AEngine.Use(JWTAuthenticationBundle(...))';

  /// <summary>
  /// Secure JWT authentication middleware using HTTP-only cookies.
  /// This middleware provides secure-by-default settings for cookie-based JWT authentication.
  /// Default settings: Secure=True, SameSite=Strict, HttpOnly=True
  /// </summary>
  TMVCJWTCookieAuthenticationMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    fCore: TMVCJWTCookieAuthenticationCore;
    FHMACAlgorithm: String;
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
    ); overload; virtual;
    /// <summary>
    /// Creates cookie-based JWT authentication middleware using a custom signer (e.g., TRSAJWTSigner).
    /// </summary>
    constructor Create(
      AAuthenticationHandler: IMVCAuthenticationHandler;
      AConfigClaims: TJWTClaimsSetup;
      ASigner: IJWTSigner;
      ALoginURLSegment: string = '/login';
      ALogoutURLSegment: string = '/logout';
      AClaimsToCheck: TJWTCheckableClaims = [];
      ALeewaySeconds: Cardinal = 300
    ); overload; virtual;
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
  end deprecated 'Use the JWT cookie filter bundle (MVCFramework.Filters.Authentication): AEngine.Use(JWTCookieAuthenticationBundle(...))';

  function UseJWTMiddleware(
      aAuthenticationHandler: IMVCAuthenticationHandler;
      aConfigClaims: TJWTClaimsSetup;
      aSecret: string = MVC_JWT_INSECURE_DEFAULT_SECRET;
      aLoginURLSegment: string = '/login';
      aClaimsToCheck: TJWTCheckableClaims = [];
      aLeewaySeconds: Cardinal = 300;
      aHMACAlgorithm: String = HMAC_HS512): IMVCMiddleware; overload;
      deprecated 'Use the JWT filter bundle (MVCFramework.Filters.Authentication): AEngine.Use(JWTAuthenticationBundle(...))';

  /// <summary>
  /// Creates JWT authentication middleware using a custom signer (e.g., TRSAJWTSigner for RS256).
  /// Requires MVCFramework.JWT.RSA unit and TaurusTLS for asymmetric algorithms.
  /// </summary>
  function UseJWTMiddleware(
      aAuthenticationHandler: IMVCAuthenticationHandler;
      aConfigClaims: TJWTClaimsSetup;
      aSigner: IJWTSigner;
      aLoginURLSegment: string = '/login';
      aClaimsToCheck: TJWTCheckableClaims = [];
      aLeewaySeconds: Cardinal = 300): IMVCMiddleware; overload;
      deprecated 'Use the JWT filter bundle (MVCFramework.Filters.Authentication): AEngine.Use(JWTAuthenticationBundle(...))';

  function UseJWTMiddlewareWithHTTPOnlyCookie(
      aAuthenticationHandler: IMVCAuthenticationHandler;
      aConfigClaims: TJWTClaimsSetup;
      aSecret: string = MVC_JWT_INSECURE_DEFAULT_SECRET;
      aLoginURLSegment: string = '/login';
      aLogoutURLSegment: string = '/logoff';
      aClaimsToCheck: TJWTCheckableClaims = [];
      aLeewaySeconds: Cardinal = 300;
      aHMACAlgorithm: String = HMAC_HS512): IMVCMiddleware;
      deprecated 'Use the JWT cookie filter bundle (MVCFramework.Filters.Authentication): AEngine.Use(JWTCookieAuthenticationBundle(...))';

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
  ): TMVCJWTCookieAuthenticationMiddleware; overload;
      deprecated 'Use the JWT cookie filter bundle (MVCFramework.Filters.Authentication): AEngine.Use(JWTCookieAuthenticationBundle(...))';

  /// <summary>
  /// Creates JWT cookie authentication middleware using a custom signer (e.g., TRSAJWTSigner for RS256).
  /// Requires MVCFramework.JWT.RSA unit and TaurusTLS for asymmetric algorithms.
  /// </summary>
  function UseJWTCookieAuthentication(
      AAuthenticationHandler: IMVCAuthenticationHandler;
      AConfigClaims: TJWTClaimsSetup;
      ASigner: IJWTSigner;
      ALoginURLSegment: string = '/login';
      ALogoutURLSegment: string = '/logout';
      AClaimsToCheck: TJWTCheckableClaims = [];
      ALeewaySeconds: Cardinal = 300
  ): TMVCJWTCookieAuthenticationMiddleware; overload;
      deprecated 'Use the JWT cookie filter bundle (MVCFramework.Filters.Authentication): AEngine.Use(JWTCookieAuthenticationBundle(...))';

  function UseJWTBlackListMiddleware(
      OnAcceptToken: TMVCOnAcceptTokenProc;
      OnNewJWTToBlackList: TMVCOnNewJWTToBlackList;
      BlackListRequestURLSegment: string = '/logout'
    ): IMVCMiddleware;
      deprecated 'Use the blacklist of the JWT filter bundle (MVCFramework.Filters.Authentication): OnAcceptToken and OnNewJWTToBlackList in the options of AEngine.Use(JWTAuthenticationBundle(...))';

implementation

uses
  System.NetEncoding;

function UseJWTMiddleware(
      aAuthenticationHandler: IMVCAuthenticationHandler;
      aConfigClaims: TJWTClaimsSetup;
      aSecret: string = MVC_JWT_INSECURE_DEFAULT_SECRET;
      aLoginURLSegment: string = '/login';
      aClaimsToCheck: TJWTCheckableClaims = [];
      aLeewaySeconds: Cardinal = 300;
      aHMACAlgorithm: String = HMAC_HS512): IMVCMiddleware;
begin
  Result := TMVCJWTAuthenticationMiddleware.Create(
    aAuthenticationHandler, aConfigClaims, aSecret, aLoginURLSegment, aClaimsToCheck, aLeewaySeconds, aHMACAlgorithm);
end;

function UseJWTMiddleware(
    aAuthenticationHandler: IMVCAuthenticationHandler;
    aConfigClaims: TJWTClaimsSetup;
    aSigner: IJWTSigner;
    aLoginURLSegment: string;
    aClaimsToCheck: TJWTCheckableClaims;
    aLeewaySeconds: Cardinal): IMVCMiddleware;
begin
  Result := TMVCJWTAuthenticationMiddleware.Create(
    aAuthenticationHandler, aConfigClaims, aSigner, aLoginURLSegment, aClaimsToCheck, aLeewaySeconds);
end;


function UseJWTMiddlewareWithHTTPOnlyCookie(
    aAuthenticationHandler: IMVCAuthenticationHandler;
    aConfigClaims: TJWTClaimsSetup;
    aSecret: string = MVC_JWT_INSECURE_DEFAULT_SECRET;
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

function UseJWTCookieAuthentication(
    AAuthenticationHandler: IMVCAuthenticationHandler;
    AConfigClaims: TJWTClaimsSetup;
    ASigner: IJWTSigner;
    ALoginURLSegment: string;
    ALogoutURLSegment: string;
    AClaimsToCheck: TJWTCheckableClaims;
    ALeewaySeconds: Cardinal
): TMVCJWTCookieAuthenticationMiddleware;
begin
  Result := TMVCJWTCookieAuthenticationMiddleware.Create(
    AAuthenticationHandler,
    AConfigClaims,
    ASigner,
    ALoginURLSegment,
    ALogoutURLSegment,
    AClaimsToCheck,
    ALeewaySeconds
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
  // Fail at startup rather than accepting forged tokens: the shipped demo secret
  // is public, so any client could sign its own token (GHSA-hgv7-ch4w-2f47).
  CheckJWTSecret(ASecret);
  fCore := TMVCJWTAuthenticationCore.Create(AAuthenticationHandler, AConfigClaims, ASecret, nil,
    ALoginURLSegment, AClaimsToCheck, ALeewaySeconds, TMVCJWTDefaults.AUTHORIZATION_ACCESS_TOKEN);
  FAuthorizationHeaderName := TMVCJWTDefaults.AUTHORIZATION_HEADER;
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

constructor TMVCJWTAuthenticationMiddleware.Create(
  AAuthenticationHandler: IMVCAuthenticationHandler;
  AConfigClaims: TJWTClaimsSetup;
  ASigner: IJWTSigner;
  ALoginURLSegment: string;
  AClaimsToCheck: TJWTCheckableClaims;
  ALeewaySeconds: Cardinal);
begin
  inherited Create;
  fCore := TMVCJWTAuthenticationCore.Create(AAuthenticationHandler, AConfigClaims, '', ASigner,
    ALoginURLSegment, AClaimsToCheck, ALeewaySeconds, TMVCJWTDefaults.AUTHORIZATION_ACCESS_TOKEN);
  FHMACAlgorithm := ASigner.GetAlgorithm;
  FAuthorizationHeaderName := TMVCJWTDefaults.AUTHORIZATION_HEADER;
  FUserNameHeaderName := TMVCJWTDefaults.USERNAME_HEADER;
  FPasswordHeaderName := TMVCJWTDefaults.PASSWORD_HEADER;
  FUseHttpOnly := False;
  FTokenHttpOnlyExpires := Now;
end;

procedure TMVCJWTAuthenticationMiddleware.ExtendExpirationTime(const JWTValue: TJWT);
begin
  TMVCJWTAuthenticationCore.ExtendExpirationTime(JWTValue);
  if FUseHttpOnly then
  begin
    FTokenHttpOnlyExpires := JWTValue.Claims.ExpirationTime;
  end;
end;

procedure TMVCJWTAuthenticationMiddleware.InternalRender(AJSONOb: TJDOJsonObject;
  AMediaType, AContentCharset: string; AContext: TWebContext; AInstanceOwner: Boolean);
var
  lJValue: string;
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
    lCookie.Secure := True;
    {$IF CompilerVersion >= 35.0}  // TCookie.SameSite exists since 10.4.2, but CompilerVersion is 34.0 for the whole 10.4 line: gate at 11
    lCookie.SameSite := 'Strict';
    {$ENDIF}
  end;

  TMVCJWTAuthenticationCore.RenderJSON(AContext, lJValue, AMediaType, AContentCharset);

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
  Cookie.HttpOnly := True;
  Cookie.Secure := True;
  {$IF CompilerVersion >= 35.0}  // TCookie.SameSite exists since 10.4.2, but CompilerVersion is 34.0 for the whole 10.4 line: gate at 11
  Cookie.SameSite := 'Strict';
  {$ENDIF}

  Encoding := TEncoding.GetEncoding(AContentEncoding);
  try
    AContext.Response.SetContentStream(TBytesStream.Create(TEncoding.Convert(TEncoding.Default, Encoding, TEncoding.Default.GetBytes(returnMessage))), ContentType);
  finally
    Encoding.Free;
  end;
end;

function TMVCJWTAuthenticationMiddleware.NeedsToBeExtended(const JWTValue: TJWT): Boolean;
begin
  Result := TMVCJWTAuthenticationCore.NeedsToBeExtended(JWTValue);
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
  lToken: string;
  lExpires: TDateTime;
begin
  if not fCore.AuthenticationRequired(AContext, AControllerQualifiedClassName, AActionName) then
  begin
    AHandled := False;
    fCore.LoadOptionalToken(AContext, TMVCJWTAuthenticationCore.RequestToken(AContext, fCore.AccessTokenParamName));
    Exit;
  end;

  lToken := TMVCJWTAuthenticationCore.RequestToken(AContext, fCore.AccessTokenParamName);
  // the HttpOnly cookie, read only when neither the header nor the parameter is in the request
  if FUseHttpOnly and AContext.Request.Headers[FAuthorizationHeaderName].IsEmpty and
    (fCore.AccessTokenParamName.IsEmpty or AContext.Request.Params[fCore.AccessTokenParamName].IsEmpty) then
    lToken := Trim(TNetEncoding.URL.Decode(AContext.Request.Cookie('token').Trim));

  lToken := fCore.Authorize(AContext, lToken, AControllerQualifiedClassName, AActionName, lExpires);
  if not lToken.IsEmpty then
  begin
    if FUseHttpOnly then
      FTokenHttpOnlyExpires := lExpires;
    AContext.Response.SetCustomHeader(FAuthorizationHeaderName, 'Bearer ' + lToken);
  end;

  AHandled := False;
end;

procedure TMVCJWTAuthenticationMiddleware.OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
var
  lToken: string;
  lExpires: TDateTime;
  lJsonObject: TJDOJsonObject;
begin
  if fCore.TryLogin(AContext, lToken, lExpires) then
  begin
    if FUseHttpOnly then
    begin
      FTokenHttpOnlyExpires := lExpires;
    end;
    lJsonObject := TJDOJsonObject.Create;
    try
      lJsonObject.S['token'] := lToken;
      InternalRender(lJsonObject, TMVCMediaType.APPLICATION_JSON, TMVCConstants.DEFAULT_CONTENT_CHARSET,
        AContext, False);
    finally
      lJsonObject.Free;
    end;
    AHandled := True;
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
  BlackListRequestURLSegment: string;
  AccessTokenParamName: string);
begin
  inherited Create;
  fCore := TMVCJWTBlackListCore.Create(OnAcceptToken, OnNewJWTToBlackList, BlackListRequestURLSegment,
    AccessTokenParamName);
  Assert(Assigned(fCore.OnAcceptToken));
  Assert(not fCore.BlackListRequestURLSegment.IsEmpty);
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
begin
  fCore.BeforeRouting(AContext, AHandled);
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
  CheckJWTSecret(ASecret);
  fCore := TMVCJWTCookieAuthenticationCore.Create(AAuthenticationHandler, AConfigClaims, ASecret, nil,
    ALoginURLSegment, ALogoutURLSegment, AClaimsToCheck, ALeewaySeconds);
  FHMACAlgorithm := AHMACAlgorithm;
  FTokenExpires := 0;
end;

constructor TMVCJWTCookieAuthenticationMiddleware.Create(
  AAuthenticationHandler: IMVCAuthenticationHandler;
  AConfigClaims: TJWTClaimsSetup;
  ASigner: IJWTSigner;
  ALoginURLSegment: string;
  ALogoutURLSegment: string;
  AClaimsToCheck: TJWTCheckableClaims;
  ALeewaySeconds: Cardinal);
begin
  inherited Create;
  FHMACAlgorithm := ASigner.GetAlgorithm;
  fCore := TMVCJWTCookieAuthenticationCore.Create(AAuthenticationHandler, AConfigClaims, '', ASigner,
    ALoginURLSegment, ALogoutURLSegment, AClaimsToCheck, ALeewaySeconds);
  FTokenExpires := 0;
end;

function TMVCJWTCookieAuthenticationMiddleware.SetCookieName(
  const AName: string): TMVCJWTCookieAuthenticationMiddleware;
begin
  fCore.CookieName := AName;
  Result := Self;
end;

function TMVCJWTCookieAuthenticationMiddleware.SetCookieSecure(
  ASecure: Boolean): TMVCJWTCookieAuthenticationMiddleware;
begin
  fCore.SetCookieSecure(ASecure);
  Result := Self;
end;

function TMVCJWTCookieAuthenticationMiddleware.SetCookieSameSite(
  ASameSite: TMVCJWTCookieSameSite): TMVCJWTCookieAuthenticationMiddleware;
begin
  fCore.SetCookieSameSite(ASameSite);
  Result := Self;
end;

function TMVCJWTCookieAuthenticationMiddleware.SetCookiePath(
  const APath: string): TMVCJWTCookieAuthenticationMiddleware;
begin
  fCore.CookiePath := APath;
  Result := Self;
end;

function TMVCJWTCookieAuthenticationMiddleware.SetCookieDomain(
  const ADomain: string): TMVCJWTCookieAuthenticationMiddleware;
begin
  fCore.CookieDomain := ADomain;
  Result := Self;
end;

procedure TMVCJWTCookieAuthenticationMiddleware.SetCookie(
  AContext: TWebContext; const AToken: string; AExpires: TDateTime);
begin
  fCore.SetCookie(AContext, AToken, AExpires);
end;

procedure TMVCJWTCookieAuthenticationMiddleware.InvalidateCookie(AContext: TWebContext);
begin
  fCore.InvalidateCookie(AContext);
end;

function TMVCJWTCookieAuthenticationMiddleware.GetTokenFromCookie(
  AContext: TWebContext): string;
begin
  Result := fCore.GetTokenFromCookie(AContext);
end;

function TMVCJWTCookieAuthenticationMiddleware.NeedsToBeExtended(
  const JWTValue: TJWT): Boolean;
begin
  Result := TMVCJWTAuthenticationCore.NeedsToBeExtended(JWTValue);
end;

procedure TMVCJWTCookieAuthenticationMiddleware.ExtendExpirationTime(
  const JWTValue: TJWT);
begin
  TMVCJWTAuthenticationCore.ExtendExpirationTime(JWTValue);
  FTokenExpires := JWTValue.Claims.ExpirationTime;
end;

procedure TMVCJWTCookieAuthenticationMiddleware.RenderLoginResponse(
  AContext: TWebContext; const AToken: string);
begin
  fCore.RenderLoginResponse(AContext, AToken, FTokenExpires);
end;

procedure TMVCJWTCookieAuthenticationMiddleware.RenderLogoutResponse(AContext: TWebContext);
begin
  fCore.RenderLogoutResponse(AContext);
end;

procedure TMVCJWTCookieAuthenticationMiddleware.OnBeforeRouting(
  AContext: TWebContext; var AHandled: Boolean);
var
  lToken: string;
begin
  // Handle login
  if fCore.JWT.TryLogin(AContext, lToken, FTokenExpires) then
  begin
    RenderLoginResponse(AContext, lToken);
    AHandled := True;
  end
  // Handle logout
  else if SameText(AContext.Request.PathInfo, fCore.LogoutURLSegment) then
  begin
    RenderLogoutResponse(AContext);
    AHandled := True;
  end;
end;

procedure TMVCJWTCookieAuthenticationMiddleware.OnBeforeControllerAction(
  AContext: TWebContext;
  const AControllerQualifiedClassName, AActionName: string;
  var AHandled: Boolean);
begin
  fCore.Check(AContext, AControllerQualifiedClassName, AActionName);
  AHandled := False;
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
