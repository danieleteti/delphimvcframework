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
// ***************************************************************************
//
// INTERNAL to the framework: not an API, it changes without notice. The pieces
// MVCFramework.Filters, MVCFramework.Filters.Authentication and MVCFramework.Filters.OIDC share: a classic
// middleware as a filter, and how an authentication filter asks its handler. Also
// the checks that a filter and its deprecated middleware share (CSRF, RateLimit, Trace).

unit MVCFramework.Filters.Internal;

{$I dmvcframework.inc}

interface

uses
  System.SysUtils,
  MVCFramework,
  MVCFramework.Filters; // TMVCRateLimitOptions, CSRFToken: Filters uses this unit only in its implementation

// The handler of an authentication filter. A controller action reaches the
// handler unchanged; a Minimal API route has no controller, and the filter
// attached to its group is what asks for authentication: OnRequest is not
// asked there (authentication is required), the other two methods are.
// nil stays nil: the middleware then requires an identity everywhere.
function FilterAuthHandler(const AHandler: IMVCAuthenticationHandler): IMVCAuthenticationHandler;

// Raises when AHandler is nil.
procedure CheckHandler(const AHandler: IMVCAuthenticationHandler; const AFilterName: string);

// OnBeforeRouting of AMiddleware as an HTTP filter: when the middleware handles
// the request it has written the answer, and the request ends there.
function MiddlewareBeforeRoutingFilter(const AMiddleware: IMVCMiddleware): TMVCHTTPFilter;

// OnBeforeControllerAction of AMiddleware as an endpoint filter, with the
// controller and the action of the context. A middleware that handles the
// request has written the answer: nil, so nothing else is rendered.
function MiddlewareBeforeActionFilter(const AMiddleware: IMVCMiddleware): TMVCEndpointFilter;

// True when A = B. Same length is not secret, the content is: no early exit
// on the first different character.
function SameSecret(const A, B: string): Boolean;

// The check of the CSRF filters and of TMVCCSRFMiddleware, built from their
// parameters (and raising as they do): True when the request passes, False
// when it does not and the 403 is written.
function CSRFCheck(const ASecret: string; const ATokenSource: TFunc<TWebContext, string>;
  const AHeaderName, AFormFieldName: string; const AExemptPaths, AAllowedHosts: TArray<string>): TFunc<TWebContext, Boolean>;

// True if the value is a plain host[:port] (no scheme, path, spaces or
// wildcards; no IPv6 literals): what AAllowedHosts of the CSRF check accepts.
function MVCIsValidHost(const AHost: string): Boolean;

// The check of RateLimitHTTPFilter and of TMVCRateLimitMiddleware on AOptions
// as they are: Storage assigned, ExcludedPaths in lower case, an empty
// APIKeyHeaderName or HeaderPrefix used empty. Stamps the X-RateLimit-* headers;
// True when the request is over the limit, with the 429 written.
function MVCRateLimitExceeded(const AContext: TWebContext;
  const AOptions: TMVCRateLimitOptions): Boolean;

// A body as the Trace filter and TMVCTraceMiddleware log it: ABytes decoded with
// the charset of AContentType; when it names none, UTF-8, or the ANSI code page
// that Render(string) writes such a body in. ABytes can be only the start of a
// body of ABodySize bytes, cut by the size limit: in UTF-8 a character cut in two
// at the end is left out. Never raises: bytes that are not UTF-8 when the charset
// says so, or an unknown charset, give '<N bytes not decodable as charset>'.
function MVCTraceBodyText(const ABytes: TBytes; const ABodySize: Int64;
  const AContentType: string): string;

// The start of ABytes that can give AMaxChars characters (4 bytes at most each):
// a large body is not decoded whole to log its first characters.
function MVCTraceBodyBytes(const ABytes: TBytes; const AMaxChars: Int64): TBytes;

implementation

uses
  System.Generics.Collections,
  System.Math,                      // Max (RateLimit headers)
  System.DateUtils,                 // DateTimeToUnix, SecondsBetween (RateLimit)
  MVCFramework.Commons;

type
  TMVCFilterAuthHandler = class(TInterfacedObject, IMVCAuthenticationHandler)
  private
    fHandler: IMVCAuthenticationHandler;
  public
    constructor Create(const AHandler: IMVCAuthenticationHandler);
    procedure OnRequest(const AContext: TWebContext; const AControllerQualifiedClassName,
      AActionName: string; var AAuthenticationRequired: Boolean);
    procedure OnAuthentication(const AContext: TWebContext; const AUserName, APassword: string;
      AUserRoles: TList<string>; var AIsValid: Boolean;
      const ASessionData: TDictionary<string, string>);
    procedure OnAuthorization(const AContext: TWebContext; AUserRoles: TList<string>;
      const AControllerQualifiedClassName: string; const AActionName: string;
      var AIsAuthorized: Boolean);
  end;

constructor TMVCFilterAuthHandler.Create(const AHandler: IMVCAuthenticationHandler);
begin
  inherited Create;
  fHandler := AHandler;
end;

procedure TMVCFilterAuthHandler.OnRequest(const AContext: TWebContext;
  const AControllerQualifiedClassName, AActionName: string; var AAuthenticationRequired: Boolean);
begin
  if AControllerQualifiedClassName = '' then
    AAuthenticationRequired := True
  else
    fHandler.OnRequest(AContext, AControllerQualifiedClassName, AActionName, AAuthenticationRequired);
end;

procedure TMVCFilterAuthHandler.OnAuthentication(const AContext: TWebContext;
  const AUserName, APassword: string; AUserRoles: TList<string>; var AIsValid: Boolean;
  const ASessionData: TDictionary<string, string>);
begin
  fHandler.OnAuthentication(AContext, AUserName, APassword, AUserRoles, AIsValid, ASessionData);
end;

procedure TMVCFilterAuthHandler.OnAuthorization(const AContext: TWebContext;
  AUserRoles: TList<string>; const AControllerQualifiedClassName, AActionName: string;
  var AIsAuthorized: Boolean);
begin
  fHandler.OnAuthorization(AContext, AUserRoles, AControllerQualifiedClassName, AActionName, AIsAuthorized);
end;

function FilterAuthHandler(const AHandler: IMVCAuthenticationHandler): IMVCAuthenticationHandler;
begin
  if AHandler = nil then
    Result := nil // the middleware then requires an identity everywhere and authorizes it
  else
    Result := TMVCFilterAuthHandler.Create(AHandler);
end;

procedure CheckHandler(const AHandler: IMVCAuthenticationHandler; const AFilterName: string);
begin
  if AHandler = nil then
    raise EMVCException.CreateFmt('%s: the authentication handler is required', [AFilterName]);
end;

function MiddlewareBeforeRoutingFilter(const AMiddleware: IMVCMiddleware): TMVCHTTPFilter;
begin
  Result :=
    procedure(const AContext: TWebContext; const ANext: TMVCHTTPFilterNext)
    var
      lHandled: Boolean;
    begin
      lHandled := False;
      AMiddleware.OnBeforeRouting(AContext, lHandled);
      if not lHandled then
        ANext();
    end;
end;

function MiddlewareBeforeActionFilter(const AMiddleware: IMVCMiddleware): TMVCEndpointFilter;
begin
  Result :=
    function(const AContext: TWebContext; const ANext: TMVCEndpointFilterNext): IMVCResponse
    var
      lHandled: Boolean;
    begin
      lHandled := False;
      AMiddleware.OnBeforeControllerAction(AContext, AContext.ControllerQualifiedClassName,
        AContext.ActionName, lHandled);
      if lHandled then
        Exit(nil);
      Result := ANext();
    end;
end;

// --- shared secrets ------------------------------------------------------

function SameSecret(const A, B: string): Boolean;
var
  I, lDiff: Integer;
begin
  if Length(A) <> Length(B) then
    Exit(False);
  lDiff := 0;
  for I := 1 to Length(A) do
    lDiff := lDiff or (Ord(A[I]) xor Ord(B[I]));
  Result := lDiff = 0;
end;

// --- CSRF -----------------------------------------------------------------

function MVCIsValidHost(const AHost: string): Boolean;
var
  lName, lPort: string;
  lColon, lPortNum: Integer;
  C: Char;
begin
  lName := AHost;
  lPort := '';
  lColon := lName.IndexOf(':');
  if lColon >= 0 then
  begin
    lPort := lName.Substring(lColon + 1);
    lName := lName.Substring(0, lColon);
    if (lPort = '') or (Length(lPort) > 5) or not TryStrToInt(lPort, lPortNum) or
      (lPortNum < 1) or (lPortNum > 65535) then
      Exit(False);
    for C in lPort do
      if not CharInSet(C, ['0'..'9']) then
        Exit(False);
  end;
  Result := lName <> '';
  for C in lName do
    if not (CharInSet(C, ['a'..'z', 'A'..'Z', '0'..'9', '-', '.', '_'])) then
      Exit(False);
end;

function CSRFIsExempt(const APath: string; const AExemptPaths, AExemptPrefixes: TArray<string>): Boolean;
var
  I: Integer;
begin
  // case-sensitive (router paths are), and on a path boundary:
  // '/a/login' exempts '/a/login' and '/a/login/x', not '/a/login-evil'
  for I := 0 to High(AExemptPaths) do
    if (APath = AExemptPaths[I]) or APath.StartsWith(AExemptPrefixes[I], False) then
      Exit(True);
  Result := False;
end;

function CSRFOriginIsAcceptable(AContext: TWebContext; const AAllowedHosts: TArray<string>): Boolean;
var
  lOrigin, lHost: string;
  lPos: Integer;
begin
  lOrigin := AContext.Request.Headers['Origin'];
  if lOrigin.IsEmpty then
    Exit(True);
  lPos := lOrigin.IndexOf('://');
  if lPos >= 0 then
    lOrigin := lOrigin.Substring(lPos + 3);
  Result := SameText(lOrigin, AContext.Request.Headers['Host']);
  if not Result then
    for lHost in AAllowedHosts do
      if SameText(lOrigin, lHost) then
        Exit(True);
end;

procedure CSRFReject(AContext: TWebContext; const AReason: string);
begin
  AContext.Response.StatusCode := HTTP_STATUS.Forbidden;
  AContext.Response.ReasonString := 'Forbidden';
  AContext.Response.ContentType := TMVCMediaType.TEXT_PLAIN;
  AContext.Response.Content := 'CSRF check failed: ' + AReason;
end;

function CSRFCheck(const ASecret: string; const ATokenSource: TFunc<TWebContext, string>;
  const AHeaderName, AFormFieldName: string; const AExemptPaths, AAllowedHosts: TArray<string>): TFunc<TWebContext, Boolean>;
var
  lExempt, lHost: string;
  lExemptPrefixes: TArray<string>;
  I: Integer;
begin
  if Length(ASecret) < 16 then
    raise EMVCException.Create('CSRF middleware: secret must be at least 16 characters');
  if not Assigned(ATokenSource) then
    raise EMVCException.Create('CSRF middleware: token source is required');
  for lExempt in AExemptPaths do
    if (lExempt.Trim = '') or not lExempt.StartsWith('/') or (lExempt = '/') then
      raise EMVCException.CreateFmt(
        'CSRF middleware: invalid exempt path [%s] (must start with ''/'' and cannot be ''/'' alone)', [lExempt]);
  for lHost in AAllowedHosts do
    if not MVCIsValidHost(lHost) then
      raise EMVCException.CreateFmt(
        'CSRF middleware: invalid allowed host [%s] (expected host or host:port)', [lHost]);
  // AExemptPaths with the trailing '/', built once
  SetLength(lExemptPrefixes, Length(AExemptPaths));
  for I := 0 to High(AExemptPaths) do
    if AExemptPaths[I].EndsWith('/') then
      lExemptPrefixes[I] := AExemptPaths[I]
    else
      lExemptPrefixes[I] := AExemptPaths[I] + '/';
  Result :=
    function(AContext: TWebContext): Boolean
    var
      lSessionId, lReceived: string;
    begin
      Result := True;
      if not (AContext.Request.HTTPMethod in [httpPOST, httpPUT, httpPATCH, httpDELETE, httpQUERY]) then
        Exit;
      if CSRFIsExempt(AContext.Request.PathInfo, AExemptPaths, lExemptPrefixes) then
        Exit;

      Result := False; // from here on, anything but a valid token ends the request
      if not CSRFOriginIsAcceptable(AContext, AAllowedHosts) then
      begin
        CSRFReject(AContext, 'origin');
        Exit;
      end;
      // no session, no token: HMAC of '' would be a value anybody can compute
      lSessionId := ATokenSource(AContext);
      if lSessionId.IsEmpty then
      begin
        CSRFReject(AContext, 'no session');
        Exit;
      end;
      lReceived := AContext.Request.Headers[AHeaderName];
      if lReceived.IsEmpty then
        lReceived := AContext.Request.ContentParam(AFormFieldName);
      if lReceived.IsEmpty or not SameSecret(lReceived, CSRFToken(ASecret, lSessionId)) then
      begin
        CSRFReject(AContext, 'token');
        Exit;
      end;
      Result := True;
    end;
end;

// --- RateLimit ------------------------------------------------------------

function RateLimitExtractKey(const AContext: TWebContext; const AOptions: TMVCRateLimitOptions): string;
begin
  case AOptions.KeyType of
    rlkIPAddress:
      Result := AContext.Request.ClientIP;

    rlkUserID:
      begin
        // Try to get user ID from session
        if AContext.LoggedUser.IsValid then
          Result := AContext.LoggedUser.UserName
        else
        begin
          // Fallback to IP if no user is logged in
          Result := AContext.Request.ClientIP;
        end;
      end;

    rlkAPIKey:
      begin
        Result := AContext.Request.Headers[AOptions.APIKeyHeaderName];
        if Result.IsEmpty then
          Result := AContext.Request.ClientIP; // Fallback to IP
      end;

    rlkCustom:
      begin
        if Assigned(AOptions.KeyExtractor) then
          Result := AOptions.KeyExtractor(AContext)
        else
          Result := AContext.Request.ClientIP; // Fallback to IP
      end;
  else
    Result := AContext.Request.ClientIP;
  end;

  // Add key type prefix to avoid collisions
  Result := Format('%d:%s', [Ord(AOptions.KeyType), Result]);
end;

function RateLimitIsPathExcluded(const APath: string; const AExcludedPaths: TArray<string>): Boolean;
var
  lExcludedPath: string;
  lNormalizedPath: string;
begin
  Result := False;
  lNormalizedPath := APath.ToLower;

  for lExcludedPath in AExcludedPaths do
  begin
    if lNormalizedPath.StartsWith(lExcludedPath) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure RateLimitSetHeaders(const AContext: TWebContext; const AHeaderPrefix: string;
  const ALimit, ARemaining: Integer; const AResetTime: TDateTime);
var
  lResetUnixTime: Int64;
begin
  // Standard rate limit headers
  AContext.Response.SetCustomHeader(AHeaderPrefix + 'Limit', ALimit.ToString);
  AContext.Response.SetCustomHeader(AHeaderPrefix + 'Remaining', Max(0, ARemaining).ToString);

  // Reset time as Unix timestamp
  lResetUnixTime := DateTimeToUnix(AResetTime, False);
  AContext.Response.SetCustomHeader(AHeaderPrefix + 'Reset', lResetUnixTime.ToString);
end;

function MVCRateLimitExceeded(const AContext: TWebContext;
  const AOptions: TMVCRateLimitOptions): Boolean;
var
  lKey: string;
  lRemaining: Integer;
  lResetTime: TDateTime;
begin
  if RateLimitIsPathExcluded(AContext.Request.PathInfo, AOptions.ExcludedPaths) then
    Exit(False);

  lKey := RateLimitExtractKey(AContext, AOptions);

  Result := AOptions.Storage.CheckRateLimit(lKey, AOptions.MaxRequests, AOptions.WindowSeconds,
    lRemaining, lResetTime);

  RateLimitSetHeaders(AContext, AOptions.HeaderPrefix, AOptions.MaxRequests, lRemaining, lResetTime);

  if Result then
  begin
    // Call custom callback if set
    if Assigned(AOptions.OnExceeded) then
      AOptions.OnExceeded(AContext, lKey, AOptions.MaxRequests, AOptions.WindowSeconds);

    // Return 429 Too Many Requests
    AContext.Response.StatusCode := HTTP_STATUS.TooManyRequests;
    AContext.Response.ReasonString := 'Too Many Requests';
    AContext.Response.ContentType := TMVCMediaType.APPLICATION_JSON;
    AContext.Response.Content := Format(
      '{"error":"Rate limit exceeded","limit":%d,"window_seconds":%d,"retry_after":%d}',
      [AOptions.MaxRequests, AOptions.WindowSeconds, SecondsBetween(lResetTime, Now)]
    );

    // Add Retry-After header (RFC 6585)
    AContext.Response.SetCustomHeader('Retry-After', SecondsBetween(lResetTime, Now).ToString);
  end;
end;

// --- Trace ------------------------------------------------------------------

function MVCTraceBodyText(const ABytes: TBytes; const ABodySize: Int64;
  const AContentType: string): string;
var
  lMediaType, lCharset: string;
  lEncoding: TEncoding;
  lCount, lMinCount: Integer;
begin
  if Length(ABytes) = 0 then
    Exit('');
  // a character cut by the size limit: up to 3 of its bytes at the end
  lMinCount := Length(ABytes);
  if Length(ABytes) < ABodySize then
    lMinCount := Max(1, lMinCount - 3);
  lCharset := '';
  try
    SplitContentMediaTypeAndCharset(AContentType, lMediaType, lCharset);
    lCharset := lCharset.DeQuotedString('"'); // charset="utf-8" is valid too (RFC 9110)
    if (lCharset = '') or SameText(lCharset, TMVCCharSet.UTF_8) or
      SameText(lCharset, TMVCCharSet.UTF_8_WITHOUT_DASH) then
      lEncoding := TEncoding.UTF8
    else
      lEncoding := TEncoding.GetEncoding(lCharset);
    try
      // GetCharCount answers 0, without raising, for bytes that are not UTF-8
      for lCount := Length(ABytes) downto lMinCount do
        if lEncoding.GetCharCount(ABytes, 0, lCount) > 0 then
          Exit(lEncoding.GetString(ABytes, 0, lCount));
    finally
      if not TEncoding.IsStandardEncoding(lEncoding) then
        lEncoding.Free;
    end;
    if lCharset = '' then
      Exit(TEncoding.Default.GetString(ABytes));
  except
    // an unknown charset, or a decoder that raises: the trace never breaks a response
  end;
  if lCharset = '' then
    lCharset := TMVCCharSet.UTF_8;
  Result := Format('<%d bytes not decodable as %s>', [Length(ABytes), lCharset]);
end;

function MVCTraceBodyBytes(const ABytes: TBytes; const AMaxChars: Int64): TBytes;
begin
  if AMaxChars <= 0 then
    Exit(nil);
  if AMaxChars >= Length(ABytes) then
    Exit(ABytes);
  Result := Copy(ABytes, 0, Integer(Min(Int64(Length(ABytes)), AMaxChars * 4)));
end;

end.
