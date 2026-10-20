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

unit MVCFramework.Middleware.CSRF;

// CSRF protection for cookie-authenticated web apps (synchronizer token,
// stateless): the token is HMAC-SHA256(secret, session id), so nothing is
// stored server-side. The session id comes from the caller (e.g. the JWT in
// the auth cookie): the middleware does not depend on the session type.
//
// Unsafe methods (POST, PUT, PATCH, DELETE, QUERY: it has a body, like POST) must carry the token in the
// header or, failing that, in the form field; otherwise 403 and the action
// is never run. If an Origin header is present its host must match the Host
// header (X-Forwarded-Host is NOT trusted: behind a proxy that rewrites Host,
// make the proxy preserve it, or list the public name(s) in AAllowedHosts).
// AAllowedHosts: optional extra host[:port] values accepted for Origin besides
// the Host header, compared case-insensitively, no wildcards. Entries that are
// empty, contain spaces, a scheme/path, '*' or a bad port make Create raise.
// Default (empty) = only the Host header, as before. Exempt entries must start with '/' and cannot be
// '/' alone (it would turn the check off): Create raises otherwise. Exempt entries are matched against PathInfo
// (without the engine base path), case-sensitively, on a '/' boundary.

{$I dmvcframework.inc}

interface

uses
  System.SysUtils,
  MVCFramework;

type
  TMVCCSRFMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    FSecret: string;
    FTokenSource: TFunc<TWebContext, string>;
    FHeaderName: string;
    FFormFieldName: string;
    FExemptPaths: TArray<string>;
    FAllowedHosts: TArray<string>;
    function IsExempt(const APath: string): Boolean;
    function OriginIsAcceptable(AContext: TWebContext): Boolean;
    procedure Reject(AContext: TWebContext; const AReason: string);
  public
    constructor Create(const ASecret: string; ATokenSource: TFunc<TWebContext, string>;
      const AHeaderName: string = 'X-CSRF-Token'; const AFormFieldName: string = 'csrf_token';
      const AExemptPaths: TArray<string> = [];
      const AAllowedHosts: TArray<string> = []);
    /// <summary>True if the value is a plain host[:port] (no scheme, path, spaces or wildcards; no IPv6 literals).</summary>
    class function IsValidHost(const AHost: string): Boolean; static;
    /// <summary>HMAC-SHA256 (lowercase hex) of the session id, keyed with the secret.</summary>
    class function TokenFor(const ASecret, ASessionId: string): string; static;
    procedure OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
    procedure OnBeforeControllerAction(AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      var AHandled: Boolean);
    procedure OnAfterControllerAction(AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      const AHandled: Boolean);
    procedure OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
  end;

implementation

uses
  System.Classes,
  System.StrUtils,
  MVCFramework.Commons,
  MVCFramework.HMAC;

// Compares every position of the longer string: the time does not depend on
// where the first difference is.
function ConstantTimeEquals(const A, B: string): Boolean;
var
  I, lLen: Integer;
  lDiff: Integer;
begin
  lDiff := Length(A) xor Length(B);
  lLen := Length(A);
  if Length(B) > lLen then
    lLen := Length(B);
  for I := 1 to lLen do
  begin
    if (I <= Length(A)) and (I <= Length(B)) then
      lDiff := lDiff or (Ord(A[I]) xor Ord(B[I]))
    else
      lDiff := lDiff or 1;
  end;
  Result := lDiff = 0;
end;

{ TMVCCSRFMiddleware }

constructor TMVCCSRFMiddleware.Create(const ASecret: string;
  ATokenSource: TFunc<TWebContext, string>; const AHeaderName, AFormFieldName: string;
  const AExemptPaths: TArray<string>; const AAllowedHosts: TArray<string>);
var
  lExempt, lHost: string;
begin
  inherited Create;
  if Length(ASecret) < 16 then
    raise EMVCException.Create('CSRF middleware: secret must be at least 16 characters');
  if not Assigned(ATokenSource) then
    raise EMVCException.Create('CSRF middleware: token source is required');
  for lExempt in AExemptPaths do
    if (lExempt.Trim = '') or not lExempt.StartsWith('/') or (lExempt = '/') then
      raise EMVCException.CreateFmt(
        'CSRF middleware: invalid exempt path [%s] (must start with ''/'' and cannot be ''/'' alone)', [lExempt]);
  for lHost in AAllowedHosts do
    if not IsValidHost(lHost) then
      raise EMVCException.CreateFmt(
        'CSRF middleware: invalid allowed host [%s] (expected host or host:port)', [lHost]);
  FAllowedHosts := AAllowedHosts;
  FSecret := ASecret;
  FTokenSource := ATokenSource;
  FHeaderName := AHeaderName;
  FFormFieldName := AFormFieldName;
  FExemptPaths := AExemptPaths;
end;

class function TMVCCSRFMiddleware.IsValidHost(const AHost: string): Boolean;
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

class function TMVCCSRFMiddleware.TokenFor(const ASecret, ASessionId: string): string;
begin
  Result := BytesToHex(HMAC(HMAC_SHA256, ASessionId, ASecret));
end;

function TMVCCSRFMiddleware.IsExempt(const APath: string): Boolean;
var
  lExempt: string;
begin
  // case-sensitive (router paths are), and on a path boundary:
  // '/a/login' exempts '/a/login' and '/a/login/x', not '/a/login-evil'
  for lExempt in FExemptPaths do
    if (APath = lExempt) or
      APath.StartsWith(IfThen(lExempt.EndsWith('/'), lExempt, lExempt + '/'), False) then
      Exit(True);
  Result := False;
end;

function TMVCCSRFMiddleware.OriginIsAcceptable(AContext: TWebContext): Boolean;
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
    for lHost in FAllowedHosts do
      if SameText(lOrigin, lHost) then
        Exit(True);
end;

procedure TMVCCSRFMiddleware.Reject(AContext: TWebContext; const AReason: string);
begin
  AContext.Response.StatusCode := HTTP_STATUS.Forbidden;
  AContext.Response.ReasonString := 'Forbidden';
  AContext.Response.ContentType := TMVCMediaType.TEXT_PLAIN;
  AContext.Response.Content := 'CSRF check failed: ' + AReason;
end;

procedure TMVCCSRFMiddleware.OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
var
  lSessionId, lReceived: string;
begin
  if not (AContext.Request.HTTPMethod in [httpPOST, httpPUT, httpPATCH, httpDELETE, httpQUERY]) then
    Exit;
  if IsExempt(AContext.Request.PathInfo) then
    Exit;

  AHandled := True; // from here on, anything but a valid token ends the request
  if not OriginIsAcceptable(AContext) then
  begin
    Reject(AContext, 'origin');
    Exit;
  end;
  // no session, no token: HMAC of '' would be a value anybody can compute
  lSessionId := FTokenSource(AContext);
  if lSessionId.IsEmpty then
  begin
    Reject(AContext, 'no session');
    Exit;
  end;
  lReceived := AContext.Request.Headers[FHeaderName];
  if lReceived.IsEmpty then
    lReceived := AContext.Request.ContentParam(FFormFieldName);
  if lReceived.IsEmpty or not ConstantTimeEquals(lReceived, TokenFor(FSecret, lSessionId)) then
  begin
    Reject(AContext, 'token');
    Exit;
  end;
  AHandled := False;
end;

procedure TMVCCSRFMiddleware.OnBeforeControllerAction(AContext: TWebContext;
  const AControllerQualifiedClassName, AActionName: string; var AHandled: Boolean);
begin
  // do nothing
end;

procedure TMVCCSRFMiddleware.OnAfterControllerAction(AContext: TWebContext;
  const AControllerQualifiedClassName, AActionName: string; const AHandled: Boolean);
begin
  // do nothing
end;

procedure TMVCCSRFMiddleware.OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
begin
  // do nothing
end;

end.
