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

{$WARN SYMBOL_DEPRECATED OFF} // the classes are deprecated for their users, not inside their own unit

interface

uses
  System.SysUtils,
  MVCFramework;

type
  TMVCCSRFMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    fCheck: TFunc<TWebContext, Boolean>; // CSRFCheck of MVCFramework.Filters.Internal
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
  end deprecated 'Use the CSRF filters (MVCFramework.Filters): AEngine.Use(CSRFHTTPFilter(...)) checks before routing, as the middleware, AEngine.Use(CSRFEndpointFilter(...)) after; CSRFToken gives the token';

implementation

uses
  MVCFramework.Filters,          // CSRFToken
  MVCFramework.Filters.Internal; // CSRFCheck, MVCIsValidHost

{ TMVCCSRFMiddleware }

constructor TMVCCSRFMiddleware.Create(const ASecret: string;
  ATokenSource: TFunc<TWebContext, string>; const AHeaderName, AFormFieldName: string;
  const AExemptPaths: TArray<string>; const AAllowedHosts: TArray<string>);
begin
  inherited Create;
  fCheck := CSRFCheck(ASecret, ATokenSource, AHeaderName, AFormFieldName, AExemptPaths, AAllowedHosts);
end;

class function TMVCCSRFMiddleware.IsValidHost(const AHost: string): Boolean;
begin
  Result := MVCIsValidHost(AHost);
end;

class function TMVCCSRFMiddleware.TokenFor(const ASecret, ASessionId: string): string;
begin
  Result := CSRFToken(ASecret, ASessionId);
end;

procedure TMVCCSRFMiddleware.OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
begin
  AHandled := True; // as before: a token source (or a form) that raises leaves the request handled
  AHandled := not fCheck(AContext); // a failed check has written the 403
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
