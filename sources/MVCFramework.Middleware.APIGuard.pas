// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
//
// Contributor of this file: Giordano Giaccaglia - KD Software S.r.l.s.
// https://www.kdsoftware.it - info@kdsoftware.it
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

unit MVCFramework.Middleware.APIGuard;

{
  The intrusion guard as a classic middleware. The logic lives in
  MVCFramework.Filters.APIGuard: use the HTTP filter in new code.

    lGuard := TMVCAPIGuard.Create(...);
    AEngine.Use(APIGuardHTTPFilter(lGuard));
}

{$I dmvcframework.inc}

{$WARN SYMBOL_DEPRECATED OFF} // the class is deprecated for its users, not inside its own unit

interface

uses
  MVCFramework,
  MVCFramework.Filters.APIGuard;

const
  // moved to MVCFramework.Filters.APIGuard: the aliases keep the code that uses this unit compiling
  GUARD_RULE_SCAN = MVCFramework.Filters.APIGuard.GUARD_RULE_SCAN;
  GUARD_RULE_BRUTEFORCE = MVCFramework.Filters.APIGuard.GUARD_RULE_BRUTEFORCE;
  GUARD_RULE_FLOOD = MVCFramework.Filters.APIGuard.GUARD_RULE_FLOOD;
  GUARD_RULE_INJECTION = MVCFramework.Filters.APIGuard.GUARD_RULE_INJECTION;
  GUARD_RULE_BAD_AGENT = MVCFramework.Filters.APIGuard.GUARD_RULE_BAD_AGENT;
  GUARD_RULE_MANUAL = MVCFramework.Filters.APIGuard.GUARD_RULE_MANUAL;
  GUARD_AUDIT_BLOCK = MVCFramework.Filters.APIGuard.GUARD_AUDIT_BLOCK;
  GUARD_AUDIT_UNBLOCK = MVCFramework.Filters.APIGuard.GUARD_AUDIT_UNBLOCK;
  GUARD_AUDIT_DENIED = MVCFramework.Filters.APIGuard.GUARD_AUDIT_DENIED;
  gpRequest = MVCFramework.Filters.APIGuard.gpRequest;
  gpResponse = MVCFramework.Filters.APIGuard.gpResponse;

type
  TMVCAPIGuardPhase = MVCFramework.Filters.APIGuard.TMVCAPIGuardPhase;
  TMVCAPIGuardMatcher = MVCFramework.Filters.APIGuard.TMVCAPIGuardMatcher;
  TMVCAPIGuardBlockedIP = MVCFramework.Filters.APIGuard.TMVCAPIGuardBlockedIP;
  TMVCAPIGuardAlert = MVCFramework.Filters.APIGuard.TMVCAPIGuardAlert;
  TMVCAPIGuardAlertProc = MVCFramework.Filters.APIGuard.TMVCAPIGuardAlertProc;
  TMVCAPIGuardAuditEvent = MVCFramework.Filters.APIGuard.TMVCAPIGuardAuditEvent;
  IMVCAPIGuardStorage = MVCFramework.Filters.APIGuard.IMVCAPIGuardStorage;
  TMVCAPIGuardFileStorage = MVCFramework.Filters.APIGuard.TMVCAPIGuardFileStorage;
  IMVCAPIGuardAuditLog = MVCFramework.Filters.APIGuard.IMVCAPIGuardAuditLog;
  TMVCAPIGuardFileAuditLog = MVCFramework.Filters.APIGuard.TMVCAPIGuardFileAuditLog;
  TMVCAPIGuardRule = MVCFramework.Filters.APIGuard.TMVCAPIGuardRule;

  /// <summary>Add it FIRST: a middleware that handles the request before it
  /// would hide the request from the guard.</summary>
  TMVCAPIGuardMiddleware = class(TMVCAPIGuard, IMVCMiddleware)
  public
    { IMVCMiddleware }
    procedure OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
    procedure OnBeforeControllerAction(AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      var AHandled: Boolean);
    procedure OnAfterControllerAction(AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      const AHandled: Boolean);
    procedure OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
  end deprecated 'Use the APIGuard HTTP filter (MVCFramework.Filters.APIGuard): AEngine.Use(APIGuardHTTPFilter(TMVCAPIGuard.Create(...)))';

function APIGuardNormalizeIP(const AIP: string): string;

implementation

function APIGuardNormalizeIP(const AIP: string): string;
begin
  Result := MVCFramework.Filters.APIGuard.APIGuardNormalizeIP(AIP);
end;

{ TMVCAPIGuardMiddleware }

procedure TMVCAPIGuardMiddleware.OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
begin
  AHandled := HandleRequest(AContext);
end;

procedure TMVCAPIGuardMiddleware.OnBeforeControllerAction(AContext: TWebContext;
  const AControllerQualifiedClassName, AActionName: string; var AHandled: Boolean);
begin
  // Nothing to do here
end;

procedure TMVCAPIGuardMiddleware.OnAfterControllerAction(AContext: TWebContext;
  const AControllerQualifiedClassName, AActionName: string; const AHandled: Boolean);
begin
  // Nothing to do here
end;

procedure TMVCAPIGuardMiddleware.OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
begin
  HandleResponse(AContext);
end;

end.
