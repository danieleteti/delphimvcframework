unit HooksU;

// Auth + logging via Endpoint Filters (chain-of-responsibility, .NET-style).
//
// Each filter wraps the call to ANext (the next filter or, at the bottom of
// the stack, the actual handler). A single filter can do pre-handler logic,
// post-handler logic, exception handling and finally-style cleanup, all in
// one place.

{$I dmvcframework.inc}

interface

uses
  System.SysUtils,
  System.SyncObjs,
  System.Generics.Collections,
  MVCFramework,
  MVCFramework.MinimalAPI,
  MVCFramework.Validation,
  MVCFramework.Validators;

type
  TPrincipal = class
  private
    fUser: string;
    fRole: string;
  public
    constructor Create(const AUser, ARole: string); reintroduce;
    property User: string read fUser;
    property Role: string read fRole;
  end;

  // Domain exception mapped by BusinessErrors filter.
  ETokenError = class(Exception);

  // Validatable DTO used by /api/widgets to demonstrate auto-validation.
  // Must descend from TMVCValidatable for the engine to pick it up.
  TWidgetDto = class(TMVCValidatable)
  private
    fName: string;
    fQty: Integer;
  public
    [MVCRequired]
    [MVCMinLength(3)]
    property Name: string read fName write fName;
    [MVCPositive]
    property Qty: Integer read fQty write fQty;
  end;

  // In-memory thread-safe ring buffer of audit lines so the integration
  // test can inspect what happened during request processing.
  TAuditLog = class
  strict private
    fLock: TCriticalSection;
    fItems: TList<string>;
    class var FInstance: TAuditLog;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Append(const ALine: string);
    function Snapshot: TArray<string>;
    procedure Clear;
    class function Instance: TAuditLog;
    class destructor ClassDestroy;
  end;

  // Token "database". Two tokens: one user role, one admin.
  TTokenStore = class
  strict private
    class var FMap: TDictionary<string, TPrincipal>;
  public
    class constructor ClassCreate;
    class destructor ClassDestroy;
    class function TryResolve(const AToken: string;
      out APrincipal: TPrincipal): Boolean;
  end;

// --- Filter builders ------------------------------------------------------

// LoggingFilter: wraps the next call so that REQ/RES/ERR/TIME/DONE all end
// up in the audit log. Replaces the entire 4-hook stack of the previous
// version with a single cohesive function.
function LoggingFilter: TMVCEndpointFilter;

// BearerAuthFilter: requires Authorization: Bearer <token> on incoming
// requests; short-circuits 401 if missing or invalid; otherwise stashes
// the resolved TPrincipal on the context and forwards to the next filter.
function BearerAuthFilter: TMVCEndpointFilter;

// RequireRoleFilter: assumes BearerAuthFilter ran first. Short-circuits
// 403 if the principal's role does not match. Forwards otherwise.
function RequireRoleFilter(const ARequired: string): TMVCEndpointFilter;

// BusinessErrorsFilter: catches ETokenError raised by inner code and
// replaces it with a 401 response. Other exceptions propagate.
function BusinessErrorsFilter: TMVCEndpointFilter;

// Standalone helper: the principal stashed by BearerAuth on the context.
function CurrentPrincipal(const ACtx: TWebContext): TPrincipal;

implementation

uses
  System.Classes,
  System.Diagnostics,
  MVCFramework.Commons;

const
  PRINCIPAL_KEY = '__auth_principal_ptr';

// --------------------------------------------------------------------------

constructor TPrincipal.Create(const AUser, ARole: string);
begin
  inherited Create;
  fUser := AUser;
  fRole := ARole;
end;

// --------------------------------------------------------------------------

constructor TAuditLog.Create;
begin
  inherited Create;
  fLock := TCriticalSection.Create;
  fItems := TList<string>.Create;
end;

destructor TAuditLog.Destroy;
begin
  fItems.Free;
  fLock.Free;
  inherited;
end;

procedure TAuditLog.Append(const ALine: string);
begin
  fLock.Enter;
  try
    fItems.Add(ALine);
    while fItems.Count > 1024 do
      fItems.Delete(0);
  finally
    fLock.Leave;
  end;
end;

function TAuditLog.Snapshot: TArray<string>;
begin
  fLock.Enter;
  try
    Result := fItems.ToArray;
  finally
    fLock.Leave;
  end;
end;

procedure TAuditLog.Clear;
begin
  fLock.Enter;
  try
    fItems.Clear;
  finally
    fLock.Leave;
  end;
end;

class function TAuditLog.Instance: TAuditLog;
begin
  if FInstance = nil then
    FInstance := TAuditLog.Create;
  Result := FInstance;
end;

class destructor TAuditLog.ClassDestroy;
begin
  FInstance.Free;
end;

// --------------------------------------------------------------------------

class constructor TTokenStore.ClassCreate;
begin
  FMap := TDictionary<string, TPrincipal>.Create;
  FMap.Add('alice-token', TPrincipal.Create('alice', 'user'));
  FMap.Add('bob-token',   TPrincipal.Create('bob',   'admin'));
end;

class destructor TTokenStore.ClassDestroy;
var
  P: TPrincipal;
begin
  if FMap <> nil then
  begin
    for P in FMap.Values do
      P.Free;
    FMap.Free;
  end;
end;

class function TTokenStore.TryResolve(const AToken: string;
  out APrincipal: TPrincipal): Boolean;
begin
  Result := FMap.TryGetValue(AToken, APrincipal);
end;

// --- Helpers --------------------------------------------------------------

function CurrentPrincipal(const ACtx: TWebContext): TPrincipal;
var
  S: string;
  P: NativeInt;
begin
  Result := nil;
  if not ACtx.Data.ContainsKey(PRINCIPAL_KEY) then
    Exit;
  S := ACtx.Data[PRINCIPAL_KEY];
  P := StrToInt64Def(S, 0);
  if P <> 0 then
    Result := TPrincipal(Pointer(P));
end;

procedure SetPrincipal(const ACtx: TWebContext; const APrincipal: TPrincipal);
begin
  ACtx.Data[PRINCIPAL_KEY] := IntToStr(NativeInt(Pointer(APrincipal)));
end;

// --- Filter builders ------------------------------------------------------

function LoggingFilter: TMVCEndpointFilter;
begin
  Result := function (const Ctx: TWebContext;
                       const Next: TMVCEndpointFilterNext): IMVCResponse
    var
      sw: TStopwatch;
    begin
      sw := TStopwatch.StartNew;
      TAuditLog.Instance.Append(Format('REQ %s %s',
        [Ctx.Request.HTTPMethodAsString, Ctx.Request.PathInfo]));
      try
        try
          Result := Next();
          if Result <> nil then
            TAuditLog.Instance.Append(Format('RES %d %s',
              [Result.StatusCode, Ctx.Request.PathInfo]));
        except
          on E: Exception do
          begin
            TAuditLog.Instance.Append(Format('ERR %s: %s',
              [E.ClassName, E.Message]));
            raise;
          end;
        end;
      finally
        TAuditLog.Instance.Append(Format('TIME %dms %s',
          [sw.ElapsedMilliseconds, Ctx.Request.PathInfo]));
        TAuditLog.Instance.Append('DONE ' + Ctx.Request.PathInfo);
      end;
    end;
end;

function BearerAuthFilter: TMVCEndpointFilter;
begin
  Result := function (const Ctx: TWebContext;
                       const Next: TMVCEndpointFilterNext): IMVCResponse
    var
      lAuth, lToken: string;
      lPrincipal: TPrincipal;
    begin
      lAuth := Ctx.Request.Headers['Authorization'];
      if not lAuth.StartsWith('Bearer ', True) then
      begin
        TAuditLog.Instance.Append('AUTH missing-bearer');
        Exit(Status(401, 'Bearer token required'));
      end;
      lToken := Trim(Copy(lAuth, 8, MaxInt));
      if not TTokenStore.TryResolve(lToken, lPrincipal) then
      begin
        TAuditLog.Instance.Append('AUTH bad-token=' + lToken);
        Exit(Status(401, 'Invalid token'));
      end;
      SetPrincipal(Ctx, lPrincipal);
      TAuditLog.Instance.Append(Format('AUTH ok user=%s role=%s',
        [lPrincipal.User, lPrincipal.Role]));
      Result := Next();   // proceed to handler / inner filter
    end;
end;

function RequireRoleFilter(const ARequired: string): TMVCEndpointFilter;
begin
  Result := function (const Ctx: TWebContext;
                       const Next: TMVCEndpointFilterNext): IMVCResponse
    var
      P: TPrincipal;
    begin
      P := CurrentPrincipal(Ctx);
      if P = nil then
        Exit(Status(401, 'Authentication required'));
      if not SameText(P.Role, ARequired) then
      begin
        TAuditLog.Instance.Append(Format('AUTHZ deny user=%s need-role=%s have=%s',
          [P.User, ARequired, P.Role]));
        Exit(Status(403, Format('Role "%s" required', [ARequired])));
      end;
      Result := Next();
    end;
end;

function BusinessErrorsFilter: TMVCEndpointFilter;
begin
  Result := function (const Ctx: TWebContext;
                       const Next: TMVCEndpointFilterNext): IMVCResponse
    begin
      try
        Result := Next();
      except
        on E: ETokenError do
          Result := Status(401, E.Message);
        // Other exceptions propagate up to the next outer filter or the
        // middleware's default ProblemDetails envelope.
      end;
    end;
end;

end.
