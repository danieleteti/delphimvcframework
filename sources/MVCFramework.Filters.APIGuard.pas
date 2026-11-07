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

unit MVCFramework.Filters.APIGuard;

{
  Intrusion guard: watches requests and responses per client IP and blocks the
  IP when a rule trips (path scanning, brute force, flooding, injection probes,
  known attack tools, or any custom rule). The blacklist can be persisted
  (text file here, database in MVCFramework.Filters.APIGuard.FireDAC), and
  so can the history of the block events (AuditLog, same two backends).
  A JSON status endpoint, reachable only from the monitor IPs, reports
  requests/minute, blocked IPs, per-rule counters and the latest alerts.

  An HTTP filter: register it FIRST, so it is the outermost and sees every
  request and every final status (the classic middleware run inside it).

    lGuard: IMVCAPIGuard;
    ...
    lGuard := TMVCAPIGuard.Create(
      TMVCAPIGuardFileStorage.Create('blacklist.txt'));
    lGuard.BruteForce.Configure(5, 300, 3600).OnPaths(['/auth']);
    lGuard.AddMonitorIP('10.0.0.5');
    AEngine.Use(APIGuardHTTPFilter(lGuard));
}

{$I dmvcframework.inc}

interface

uses
  System.Classes,
  System.SysUtils,
  System.DateUtils,
  System.Generics.Collections,
  System.SyncObjs,
  MVCFramework,
  MVCFramework.Commons;

const
  GUARD_RULE_SCAN = 'scan';
  GUARD_RULE_BRUTEFORCE = 'bruteforce';
  GUARD_RULE_FLOOD = 'flood';
  GUARD_RULE_INJECTION = 'injection';
  GUARD_RULE_BAD_AGENT = 'bad_agent';
  GUARD_RULE_MANUAL = 'manual';

  /// <summary>Kinds of audit events</summary>
  GUARD_AUDIT_BLOCK = 'block';
  GUARD_AUDIT_UNBLOCK = 'unblock';
  /// <summary>A request refused with 403 because its IP is blocked</summary>
  GUARD_AUDIT_DENIED = 'denied';

type
  /// <summary>When a rule looks at the request: before routing (request
  /// only) or after it (the response status is known).</summary>
  TMVCAPIGuardPhase = (gpRequest, gpResponse);

  /// <summary>True when the request is one occurrence of what the rule counts.</summary>
  TMVCAPIGuardMatcher = reference to function(const AContext: TWebContext): Boolean;

  TMVCAPIGuardBlockedIP = record
    IP: string;
    Rule: string;
    Reason: string;
    BlockedAt: TDateTime;
    /// <summary>0 = permanent</summary>
    ExpiresAt: TDateTime;
    function IsExpired(const ANow: TDateTime): Boolean;
  end;

  TMVCAPIGuardAlert = record
    Timestamp: TDateTime;
    IP: string;
    Rule: string;
    Path: string;
    Message: string;
  end;

  TMVCAPIGuardAlertProc = reference to procedure(const AAlert: TMVCAPIGuardAlert);

  TMVCAPIGuardAuditEvent = record
    Timestamp: TDateTime;
    /// <summary>GUARD_AUDIT_BLOCK, GUARD_AUDIT_UNBLOCK or GUARD_AUDIT_DENIED</summary>
    Event: string;
    IP: string;
    /// <summary>The rule of the block the event is about</summary>
    Rule: string;
    Path: string;
    Message: string;
  end;

  /// <summary>Where the blacklist survives a restart. Called, on the guard's
  /// writer thread, after changes with the whole list: a blacklist is small
  /// and changes rarely.</summary>
  IMVCAPIGuardStorage = interface
    ['{3F0B8E62-5C1D-4C4B-9A57-61E0C2D7A4B1}']
    function Load: TArray<TMVCAPIGuardBlockedIP>;
    procedure Store(const AEntries: TArray<TMVCAPIGuardBlockedIP>);
  end;

  /// <summary>One line per IP, tab separated:
  /// ip, rule, blocked_at, expires_at (empty = permanent), reason.
  /// A line with the IP alone is a permanent manual block, so the file can
  /// be edited by hand; lines starting with # are ignored.</summary>
  TMVCAPIGuardFileStorage = class(TInterfacedObject, IMVCAPIGuardStorage)
  private
    fFileName: string;
  public
    constructor Create(const AFileName: string);
    function Load: TArray<TMVCAPIGuardBlockedIP>;
    procedure Store(const AEntries: TArray<TMVCAPIGuardBlockedIP>);
  end;

  /// <summary>History of blocks, unblocks and refused requests. Appended,
  /// never rewritten, not cleared by Reset. Both methods are called on the
  /// guard's writer thread, never on a request thread.</summary>
  IMVCAPIGuardAuditLog = interface
    ['{5D7A0C93-2E48-4B61-8F1C-A3B94E6D2075}']
    /// <summary>The events queued since the last call, oldest first.</summary>
    procedure Append(const AEvents: TArray<TMVCAPIGuardAuditEvent>);
    /// <summary>Deletes the events older than ABefore (see AuditRetentionDays).</summary>
    procedure Purge(const ABefore: TDateTime);
  end;

  /// <summary>One line per event, tab separated, appended:
  /// timestamp, event, ip, rule, path, message.</summary>
  TMVCAPIGuardFileAuditLog = class(TInterfacedObject, IMVCAPIGuardAuditLog)
  private
    fFileName: string;
  public
    constructor Create(const AFileName: string);
    procedure Append(const AEvents: TArray<TMVCAPIGuardAuditEvent>);
    procedure Purge(const ABefore: TDateTime);
  end;

  TMVCAPIGuardRule = class
  private
    fName: string;
    fEnabled: Boolean;
    fPhase: TMVCAPIGuardPhase;
    fThreshold: Integer;
    fWindowSeconds: Integer;
    fBlockSeconds: Integer;
    fStatusCodes: TArray<Integer>;
    fPaths: TArray<string>;
    fMatcher: TMVCAPIGuardMatcher;
    fDetections: Int64;
    fBlocks: Int64;
    function Matches(const AContext: TWebContext): Boolean;
  public
    constructor Create(const AName: string; const APhase: TMVCAPIGuardPhase;
      const AThreshold, AWindowSeconds, ABlockSeconds: Integer;
      const AMatcher: TMVCAPIGuardMatcher = nil);
    /// <summary>ABlockSeconds = 0 blocks permanently.</summary>
    function Configure(const AThreshold, AWindowSeconds, ABlockSeconds: Integer): TMVCAPIGuardRule;
    /// <summary>Counts only requests under these paths (prefix, on segment
    /// boundary, case insensitive). Empty = every path.</summary>
    function OnPaths(const APaths: array of string): TMVCAPIGuardRule;
    /// <summary>Response rules without a matcher count these statuses. Empty = every status.</summary>
    function OnStatusCodes(const ACodes: array of Integer): TMVCAPIGuardRule;
    property Name: string read fName;
    property Enabled: Boolean read fEnabled write fEnabled;
    property Phase: TMVCAPIGuardPhase read fPhase;
    /// <summary>Occurrences within WindowSeconds that block the IP.</summary>
    property Threshold: Integer read fThreshold write fThreshold;
    property WindowSeconds: Integer read fWindowSeconds write fWindowSeconds;
    property BlockSeconds: Integer read fBlockSeconds write fBlockSeconds;
    property Matcher: TMVCAPIGuardMatcher read fMatcher write fMatcher;
  end;

  /// <summary>The guard through an interface. Keep it in an IMVCAPIGuard variable: it is
  /// reference counted, so a guard whose configuration raises is released, with its
  /// writer thread. APIGuardHTTPFilter keeps it alive for as long as the engine has
  /// the filter. See TMVCAPIGuard for what each member does.</summary>
  IMVCAPIGuard = interface
    ['{8C2E5B14-7D3A-4F96-B0E1-52A9D4C7F318}']
    function GetScan: TMVCAPIGuardRule;
    function GetBruteForce: TMVCAPIGuardRule;
    function GetFlood: TMVCAPIGuardRule;
    function GetInjection: TMVCAPIGuardRule;
    function GetBadAgent: TMVCAPIGuardRule;
    function AddRule(const AName: string; const APhase: TMVCAPIGuardPhase;
      const AThreshold, AWindowSeconds, ABlockSeconds: Integer;
      const AMatcher: TMVCAPIGuardMatcher = nil): TMVCAPIGuardRule;
    procedure AddWhitelistIP(const AIP: string);
    procedure AddMonitorIP(const AIP: string);
    procedure AddMonitorOnlyPath(const APath: string);
    procedure AddSignature(const ASignature: string);
    procedure ClearSignatures;
    procedure AddBadUserAgent(const AAgent: string);
    procedure ClearBadUserAgents;
    procedure BlockIP(const AIP, AReason: string; const ABlockSeconds: Integer = 0);
    procedure UnblockIP(const AIP: string);
    function IsBlocked(const AIP: string): Boolean;
    function BlockedIPs: TArray<TMVCAPIGuardBlockedIP>;
    procedure Reset;
    function GetUseClientIp: Boolean;
    procedure SetUseClientIp(const AValue: Boolean);
    function GetMaxAlerts: Integer;
    procedure SetMaxAlerts(const AValue: Integer);
    function GetOnAlert: TMVCAPIGuardAlertProc;
    procedure SetOnAlert(const AValue: TMVCAPIGuardAlertProc);
    function GetAuditLog: IMVCAPIGuardAuditLog;
    procedure SetAuditLog(const AValue: IMVCAPIGuardAuditLog);
    function GetAuditRetentionDays: Integer;
    procedure SetAuditRetentionDays(const AValue: Integer);
    function HandleRequest(const AContext: TWebContext): Boolean;
    procedure HandleResponse(const AContext: TWebContext);
    property Scan: TMVCAPIGuardRule read GetScan;
    property BruteForce: TMVCAPIGuardRule read GetBruteForce;
    property Flood: TMVCAPIGuardRule read GetFlood;
    property Injection: TMVCAPIGuardRule read GetInjection;
    property BadAgent: TMVCAPIGuardRule read GetBadAgent;
    property UseClientIp: Boolean read GetUseClientIp write SetUseClientIp;
    property MaxAlerts: Integer read GetMaxAlerts write SetMaxAlerts;
    property OnAlert: TMVCAPIGuardAlertProc read GetOnAlert write SetOnAlert;
    property AuditLog: IMVCAPIGuardAuditLog read GetAuditLog write SetAuditLog;
    property AuditRetentionDays: Integer read GetAuditRetentionDays write SetAuditRetentionDays;
  end;

  /// <summary>The guard: rules, blacklist, counters, status endpoint. The filter
  /// returned by APIGuardHTTPFilter keeps it alive; keep the object reference
  /// to tune it or to block and unblock at run time.</summary>
  TMVCAPIGuard = class(TInterfacedObject, IMVCAPIGuard)
  private type
    TCounter = record
      WindowStart: TDateTime;
      Count: Integer;
    end;
  private
    fLock: TCriticalSection;
    fStorage: IMVCAPIGuardStorage;
    fStatusPath: string;
    fRules: TObjectList<TMVCAPIGuardRule>;
    fScan, fBruteForce, fFlood, fInjection, fBadAgent: TMVCAPIGuardRule; // owned by fRules
    fBlocked: TDictionary<string, TMVCAPIGuardBlockedIP>;
    fCounters: TDictionary<string, TCounter>;
    fWhitelist: TList<string>;
    fMonitorIPs: TList<string>;
    fMonitorPaths: TArray<string>;
    fSignatures: TList<string>;
    fBadUserAgents: TList<string>;
    fAlerts: TList<TMVCAPIGuardAlert>;
    fMaxAlerts: Integer;
    fOnAlert: TMVCAPIGuardAlertProc;
    fAuditLog: IMVCAPIGuardAuditLog;
    fAuditRetentionDays: Integer;
    // writer thread: storage and audit log are written off the request threads
    fWriter: TThread;
    fWake: TEvent;
    fStopping: Boolean;
    fStoreDirty: Boolean;
    fPendingEvents: TList<TMVCAPIGuardAuditEvent>;
    fDroppedEvents: Int64;
    fLastPurge: TDateTime;
    fUseClientIp: Boolean;
    fStartedAt: TDateTime;
    fLastSweep: TDateTime;
    fRequestsTotal: Int64;
    fBlockedRequestsTotal: Int64;
    fSecondBuckets: array [0 .. 59] of Integer;
    fSecondStamps: array [0 .. 59] of Int64;
    function SourceIP(const AContext: TWebContext): string;
    function IsMonitorIP(const AContext: TWebContext): Boolean;
    function PathMatches(const APath: string; const APrefixes: TArray<string>): Boolean;
    function MatchesSignature(const AContext: TWebContext): Boolean;
    function MatchesBadUserAgent(const AContext: TWebContext): Boolean;
    procedure CountRequest;
    function RequestsLastMinute: Integer;
    procedure Sweep(const ANow: TDateTime);
    function IsBlockedLocked(const AIP: string; const ANow: TDateTime): Boolean;
    function ActiveBlocksLocked(const ANow: TDateTime): TArray<TMVCAPIGuardBlockedIP>;
    procedure PersistLocked;
    procedure QueueEventLocked(const AEvent, AIP, ARule, APath, AMessage: string;
      const ATimestamp: TDateTime);
    procedure FlushPending;
    procedure WriterLoop;
    procedure AddAlertLocked(const AIP, ARule, APath, AMessage: string);
    procedure BlockLocked(const AIP, ARule, AReason: string; const ABlockSeconds: Integer);
    function EvaluateRules(const AContext: TWebContext; const APhase: TMVCAPIGuardPhase;
      const AIP: string): Boolean;
    procedure SendForbidden(const AContext: TWebContext);
    procedure SendNotFound(const AContext: TWebContext);
    procedure SendStatus(const AContext: TWebContext);
    function GetScan: TMVCAPIGuardRule;
    function GetBruteForce: TMVCAPIGuardRule;
    function GetFlood: TMVCAPIGuardRule;
    function GetInjection: TMVCAPIGuardRule;
    function GetBadAgent: TMVCAPIGuardRule;
    function GetUseClientIp: Boolean;
    procedure SetUseClientIp(const AValue: Boolean);
    function GetMaxAlerts: Integer;
    procedure SetMaxAlerts(const AValue: Integer);
    function GetOnAlert: TMVCAPIGuardAlertProc;
    procedure SetOnAlert(const AValue: TMVCAPIGuardAlertProc);
    function GetAuditLog: IMVCAPIGuardAuditLog;
    procedure SetAuditLog(const AValue: IMVCAPIGuardAuditLog);
    function GetAuditRetentionDays: Integer;
    procedure SetAuditRetentionDays(const AValue: Integer);
  public
    /// <param name="AStorage">nil = the blacklist lives in memory only</param>
    /// <param name="AStatusPath">'' disables the status endpoint</param>
    constructor Create(const AStorage: IMVCAPIGuardStorage = nil;
      const AStatusPath: string = '/api-guard/status');
    destructor Destroy; override;
    /// <summary>A custom rule; the guard owns it. A response rule with a nil
    /// matcher counts the statuses set with OnStatusCodes.</summary>
    function AddRule(const AName: string; const APhase: TMVCAPIGuardPhase;
      const AThreshold, AWindowSeconds, ABlockSeconds: Integer;
      const AMatcher: TMVCAPIGuardMatcher = nil): TMVCAPIGuardRule;
    /// <summary>Never blocked, never counted.</summary>
    procedure AddWhitelistIP(const AIP: string);
    /// <summary>May read the status endpoint. Default: loopback only.
    /// Checked against the socket peer; a request carrying X-Forwarded-For,
    /// X-Real-IP or Forwarded is refused, since its peer is a proxy. With
    /// MVCTrustProxyForwardedHeaders it is checked against ClientIp instead:
    /// add the IPs of the machines that reach the dashboard through the proxy.</summary>
    procedure AddMonitorIP(const AIP: string);
    /// <summary>Reserves a path (prefix, on segment boundary) to the monitor
    /// IPs, e.g. a dashboard served by the static files filter registered after
    /// the guard. Anyone else gets a 404.</summary>
    procedure AddMonitorOnlyPath(const APath: string);
    /// <summary>Lower case substring looked for in path and query (rule "injection").</summary>
    procedure AddSignature(const ASignature: string);
    procedure ClearSignatures;
    /// <summary>Lower case substring looked for in User-Agent (rule "bad_agent").</summary>
    procedure AddBadUserAgent(const AAgent: string);
    procedure ClearBadUserAgents;
    procedure BlockIP(const AIP, AReason: string; const ABlockSeconds: Integer = 0);
    procedure UnblockIP(const AIP: string);
    function IsBlocked(const AIP: string): Boolean;
    function BlockedIPs: TArray<TMVCAPIGuardBlockedIP>;
    /// <summary>Forgets counters, blocks, alerts and statistics, and persists the empty list.</summary>
    procedure Reset;
    /// <summary>The built-in rules, to tune them: Configure(threshold, window, block), OnPaths,
    /// OnStatusCodes, Enabled. A rule of your own: keep what AddRule returns.</summary>
    property Scan: TMVCAPIGuardRule read GetScan;
    property BruteForce: TMVCAPIGuardRule read GetBruteForce;
    property Flood: TMVCAPIGuardRule read GetFlood;
    property Injection: TMVCAPIGuardRule read GetInjection;
    property BadAgent: TMVCAPIGuardRule read GetBadAgent;
    /// <summary>Key the counters on ClientIp instead of the socket peer. ClientIp
    /// reads X-Forwarded-For only when MVCTrustProxyForwardedHeaders is True:
    /// set both, and only behind a reverse proxy that sets those headers,
    /// otherwise a client picks the IP that gets blocked.</summary>
    property UseClientIp: Boolean read GetUseClientIp write SetUseClientIp;
    property MaxAlerts: Integer read GetMaxAlerts write SetMaxAlerts;
    /// <summary>Called, under the guard's lock, for every block. Keep it short.</summary>
    property OnAlert: TMVCAPIGuardAlertProc read GetOnAlert write SetOnAlert;
    /// <summary>nil = no history. Set it before the server starts. Written by
    /// the writer thread; a failed write is logged and the events are lost.</summary>
    property AuditLog: IMVCAPIGuardAuditLog read GetAuditLog write SetAuditLog;
    /// <summary>Events older than this are purged, at most once an hour.
    /// 0 (default) = kept forever.</summary>
    property AuditRetentionDays: Integer read GetAuditRetentionDays write SetAuditRetentionDays;
    /// <summary>Before routing. True: the guard has answered (403, 404 or the
    /// status), the request goes no further. Called by the filter.</summary>
    function HandleRequest(const AContext: TWebContext): Boolean;
    /// <summary>Once the response is complete: the response rules. Called by the filter.</summary>
    procedure HandleResponse(const AContext: TWebContext);
  end;

/// <summary>The HTTP filter of AGuard. Register it first:
/// AEngine.Use(APIGuardHTTPFilter(lGuard)).</summary>
function APIGuardHTTPFilter(const AGuard: IMVCAPIGuard): TMVCHTTPFilter;

/// <summary>One spelling per address, so that lists and blacklist keys compare
/// equal whatever form the host reports: IPv6 canonical (RFC 5952: lower case,
/// no leading zeros, longest zero run as ::), IPv4-mapped IPv6 as plain IPv4,
/// no brackets, no zone. Anything that does not parse is returned trimmed.</summary>
function APIGuardNormalizeIP(const AIP: string): string;

implementation

uses
  System.NetEncoding,
  JsonDataObjects,
  MVCFramework.Logger,
  MVCFramework.Serializer.Commons;

const
  SKIP_FLAG = '__apiguard_skip';
  LOG_TAG = 'apiguard';
  // a blocked IP that keeps hammering queues one "denied" per request: bounded
  MAX_PENDING_EVENTS = 10000;
  WRITER_IDLE_MS = 3600 * 1000;

function APIGuardNormalizeIP(const AIP: string): string;
var
  lIP, lLeft, lRight, lV4: string;
  lParts, lV4Parts: TArray<string>;
  lGroups: array [0 .. 7] of Integer;
  lCount, lMissing, I, lValue, lRunStart, lRunLen, lBestStart, lBestLen: Integer;

  function ParseGroups(const AText: string): Boolean;
  var
    lGroup: string;
  begin
    if AText = '' then
      Exit(True);
    for lGroup in AText.Split([':']) do
    begin
      lValue := StrToIntDef('$' + lGroup, -1);
      if (lGroup = '') or (Length(lGroup) > 4) or (lValue < 0) or (lCount > 7) then
        Exit(False);
      lGroups[lCount] := lValue;
      Inc(lCount);
    end;
    Result := True;
  end;

begin
  lIP := AIP.Trim.ToLower;
  Result := lIP;
  if lIP.StartsWith('[') and lIP.EndsWith(']') then
    lIP := lIP.Substring(1, lIP.Length - 2);
  if lIP.Contains('%') then
    lIP := lIP.Substring(0, lIP.IndexOf('%'));
  if not lIP.Contains(':') then
    Exit(lIP);

  // a trailing dotted IPv4 (::ffff:1.2.3.4) becomes the last two groups
  if lIP.Contains('.') then
  begin
    lV4 := lIP.Substring(lIP.LastIndexOf(':') + 1);
    lV4Parts := lV4.Split(['.']);
    if Length(lV4Parts) <> 4 then
      Exit;
    for I := 0 to 3 do
      if (StrToIntDef(lV4Parts[I], -1) < 0) or (StrToIntDef(lV4Parts[I], -1) > 255) then
        Exit;
    lIP := lIP.Substring(0, lIP.LastIndexOf(':') + 1) +
      IntToHex(StrToInt(lV4Parts[0]) * 256 + StrToInt(lV4Parts[1]), 1) + ':' +
      IntToHex(StrToInt(lV4Parts[2]) * 256 + StrToInt(lV4Parts[3]), 1);
  end;

  lCount := 0;
  lParts := lIP.Split(['::'], TStringSplitOptions.None);
  if Length(lParts) > 2 then
    Exit;
  lLeft := lParts[0];
  lRight := '';
  if Length(lParts) = 2 then
    lRight := lParts[1];
  if not ParseGroups(lLeft) then
    Exit;
  I := lCount;
  if not ParseGroups(lRight) then
    Exit;
  if Length(lParts) = 2 then
  begin
    lMissing := 8 - lCount;
    if lMissing < 1 then
      Exit;
    // shift the right part to the end, zero fill the gap
    Move(lGroups[I], lGroups[I + lMissing], (lCount - I) * SizeOf(Integer));
    FillChar(lGroups[I], lMissing * SizeOf(Integer), 0);
  end
  else if lCount <> 8 then
    Exit;

  if (lGroups[0] = 0) and (lGroups[1] = 0) and (lGroups[2] = 0) and (lGroups[3] = 0)
    and (lGroups[4] = 0) and (lGroups[5] = $FFFF) then
    Exit(Format('%d.%d.%d.%d', [lGroups[6] shr 8, lGroups[6] and $FF, lGroups[7] shr 8, lGroups[7] and $FF]));

  lBestStart := -1;
  lBestLen := 1; // a single zero group is not compressed
  lRunStart := -1;
  lRunLen := 0;
  for I := 0 to 7 do
    if lGroups[I] = 0 then
    begin
      if lRunStart < 0 then
        lRunStart := I;
      Inc(lRunLen);
      if lRunLen > lBestLen then
      begin
        lBestStart := lRunStart;
        lBestLen := lRunLen;
      end;
    end
    else
    begin
      lRunStart := -1;
      lRunLen := 0;
    end;

  Result := '';
  I := 0;
  while I < 8 do
  begin
    if I = lBestStart then
    begin
      Result := Result + '::';
      Inc(I, lBestLen);
      Continue;
    end;
    if (Result <> '') and not Result.EndsWith(':') then
      Result := Result + ':';
    Result := Result + IntToHex(lGroups[I], 1).ToLower;
    Inc(I);
  end;
end;

{ TMVCAPIGuardBlockedIP }

function TMVCAPIGuardBlockedIP.IsExpired(const ANow: TDateTime): Boolean;
begin
  Result := (ExpiresAt <> 0) and (ExpiresAt <= ANow);
end;

{ TMVCAPIGuardFileStorage }

constructor TMVCAPIGuardFileStorage.Create(const AFileName: string);
begin
  inherited Create;
  fFileName := AFileName;
end;

function TMVCAPIGuardFileStorage.Load: TArray<TMVCAPIGuardBlockedIP>;
var
  lLines: TStringList;
  lLine: string;
  lParts: TArray<string>;
  lEntry: TMVCAPIGuardBlockedIP;
  lList: TList<TMVCAPIGuardBlockedIP>;
begin
  Result := nil;
  if not FileExists(fFileName) then
    Exit;
  lList := TList<TMVCAPIGuardBlockedIP>.Create;
  try
    lLines := TStringList.Create;
    try
      lLines.LoadFromFile(fFileName, TEncoding.UTF8);
      for lLine in lLines do
      begin
        if lLine.Trim.IsEmpty or lLine.StartsWith('#') then
          Continue;
        lParts := lLine.Split([#9]);
        lEntry := Default(TMVCAPIGuardBlockedIP);
        lEntry.IP := lParts[0].Trim;
        lEntry.Rule := GUARD_RULE_MANUAL;
        lEntry.BlockedAt := Now;
        if Length(lParts) > 1 then
          lEntry.Rule := lParts[1];
        if Length(lParts) > 4 then
          lEntry.Reason := lParts[4];
        try
          if (Length(lParts) > 2) and (lParts[2] <> '') then
            lEntry.BlockedAt := ISOTimeStampToDateTime(lParts[2]);
          if (Length(lParts) > 3) and (lParts[3] <> '') then
            lEntry.ExpiresAt := ISOTimeStampToDateTime(lParts[3]);
        except
          // a hand-edited line with a bad date must not stop the server: keep it permanent
          lEntry.ExpiresAt := 0;
        end;
        lList.Add(lEntry);
      end;
    finally
      lLines.Free;
    end;
    Result := lList.ToArray;
  finally
    lList.Free;
  end;
end;

procedure TMVCAPIGuardFileStorage.Store(const AEntries: TArray<TMVCAPIGuardBlockedIP>);
var
  lLines: TStringList;
  lEntry: TMVCAPIGuardBlockedIP;
  lExpires: string;
begin
  lLines := TStringList.Create;
  try
    lLines.Add('# ip' + #9 + 'rule' + #9 + 'blocked_at' + #9 + 'expires_at (empty = permanent)' + #9 + 'reason');
    for lEntry in AEntries do
    begin
      lExpires := '';
      if lEntry.ExpiresAt <> 0 then
        lExpires := DateTimeToISOTimeStamp(lEntry.ExpiresAt);
      lLines.Add(string.Join(#9, [lEntry.IP, lEntry.Rule,
        DateTimeToISOTimeStamp(lEntry.BlockedAt), lExpires,
        lEntry.Reason.Replace(#9, ' ').Replace(#13, ' ').Replace(#10, ' ')]));
    end;
    lLines.SaveToFile(fFileName, TEncoding.UTF8);
  finally
    lLines.Free;
  end;
end;

{ TMVCAPIGuardFileAuditLog }

constructor TMVCAPIGuardFileAuditLog.Create(const AFileName: string);
begin
  inherited Create;
  fFileName := AFileName;
end;

procedure TMVCAPIGuardFileAuditLog.Append(const AEvents: TArray<TMVCAPIGuardAuditEvent>);

  function Clean(const AValue: string): string;
  begin
    // path and message carry attacker text: no forged fields or lines
    Result := AValue.Replace(#9, ' ').Replace(#13, ' ').Replace(#10, ' ');
  end;

var
  lWriter: TStreamWriter;
  lNew: Boolean;
  lEvent: TMVCAPIGuardAuditEvent;
begin
  lNew := not FileExists(fFileName);
  lWriter := TStreamWriter.Create(fFileName, True, TEncoding.UTF8);
  try
    if lNew then
      lWriter.WriteLine('# timestamp' + #9 + 'event' + #9 + 'ip' + #9 + 'rule' + #9 + 'path' + #9 + 'message');
    for lEvent in AEvents do
      lWriter.WriteLine(string.Join(#9, [DateTimeToISOTimeStamp(lEvent.Timestamp), Clean(lEvent.Event),
        Clean(lEvent.IP), Clean(lEvent.Rule), Clean(lEvent.Path), Clean(lEvent.Message)]));
  finally
    lWriter.Free;
  end;
end;

procedure TMVCAPIGuardFileAuditLog.Purge(const ABefore: TDateTime);
var
  lLines, lKept: TStringList;
  lLine: string;
  lOld: Boolean;
begin
  if not FileExists(fFileName) then
    Exit;
  lLines := TStringList.Create;
  lKept := TStringList.Create;
  try
    lLines.LoadFromFile(fFileName, TEncoding.UTF8);
    for lLine in lLines do
    begin
      lOld := False;
      if not lLine.StartsWith('#') then
      try
        lOld := ISOTimeStampToDateTime(lLine.Split([#9])[0]) < ABefore;
      except
        // a line without a readable date is kept: never delete what cannot be dated
      end;
      if not lOld then
        lKept.Add(lLine);
    end;
    if lKept.Count < lLines.Count then
      lKept.SaveToFile(fFileName, TEncoding.UTF8);
  finally
    lKept.Free;
    lLines.Free;
  end;
end;

{ TMVCAPIGuardRule }

constructor TMVCAPIGuardRule.Create(const AName: string; const APhase: TMVCAPIGuardPhase;
  const AThreshold, AWindowSeconds, ABlockSeconds: Integer; const AMatcher: TMVCAPIGuardMatcher);
begin
  inherited Create;
  fName := AName;
  fPhase := APhase;
  fEnabled := True;
  fMatcher := AMatcher;
  Configure(AThreshold, AWindowSeconds, ABlockSeconds);
end;

function TMVCAPIGuardRule.Configure(const AThreshold, AWindowSeconds,
  ABlockSeconds: Integer): TMVCAPIGuardRule;
begin
  if (AThreshold < 1) or (AWindowSeconds < 1) or (ABlockSeconds < 0) then
    raise EMVCException.CreateFmt('APIGuard rule "%s": threshold and window must be >= 1, block >= 0', [fName]);
  fThreshold := AThreshold;
  fWindowSeconds := AWindowSeconds;
  fBlockSeconds := ABlockSeconds;
  Result := Self;
end;

function TMVCAPIGuardRule.OnPaths(const APaths: array of string): TMVCAPIGuardRule;
var
  I: Integer;
begin
  SetLength(fPaths, Length(APaths));
  for I := 0 to High(APaths) do
    fPaths[I] := APaths[I].ToLower.TrimRight(['/']);
  Result := Self;
end;

function TMVCAPIGuardRule.OnStatusCodes(const ACodes: array of Integer): TMVCAPIGuardRule;
var
  I: Integer;
begin
  SetLength(fStatusCodes, Length(ACodes));
  for I := 0 to High(ACodes) do
    fStatusCodes[I] := ACodes[I];
  Result := Self;
end;

function TMVCAPIGuardRule.Matches(const AContext: TWebContext): Boolean;
var
  lCode: Integer;
begin
  if Assigned(fMatcher) then
    Exit(fMatcher(AContext));
  if (fPhase = gpRequest) or (Length(fStatusCodes) = 0) then
    Exit(True);
  for lCode in fStatusCodes do
    if lCode = AContext.Response.StatusCode then
      Exit(True);
  Result := False;
end;

{ TMVCAPIGuard }

constructor TMVCAPIGuard.Create(const AStorage: IMVCAPIGuardStorage;
  const AStatusPath: string);
var
  lEntry, lNormalized: TMVCAPIGuardBlockedIP;
  lNow: TDateTime;
begin
  inherited Create;
  fLock := TCriticalSection.Create;
  fStorage := AStorage;
  fStatusPath := AStatusPath.ToLower;
  fRules := TObjectList<TMVCAPIGuardRule>.Create(True);
  fBlocked := TDictionary<string, TMVCAPIGuardBlockedIP>.Create;
  fCounters := TDictionary<string, TCounter>.Create;
  fWhitelist := TList<string>.Create;
  fMonitorIPs := TList<string>.Create;
  fSignatures := TList<string>.Create;
  fBadUserAgents := TList<string>.Create;
  fAlerts := TList<TMVCAPIGuardAlert>.Create;
  fPendingEvents := TList<TMVCAPIGuardAuditEvent>.Create;
  fWake := TEvent.Create(nil, False, False, '');
  fMaxAlerts := 100;
  fStartedAt := Now;
  fLastSweep := fStartedAt;

  fMonitorIPs.AddRange(['127.0.0.1', '::1']);

  // probes for traversal, SQLi, XSS, Log4Shell, and the files scanners look for
  fSignatures.AddRange(['../', '..\', '/etc/passwd', 'win.ini', '<script', 'javascript:',
    'union select', 'union all select', ' or 1=1', '''or''1''=''1', 'sleep(', 'benchmark(',
    'xp_cmdshell', '${jndi:', '/.env', '/.git/', '/wp-admin', '/wp-login', '/phpmyadmin',
    'cmd.exe', '/bin/sh']);
  fBadUserAgents.AddRange(['sqlmap', 'nikto', 'nmap', 'masscan', 'zgrab', 'nuclei',
    'dirbuster', 'gobuster', 'wpscan', 'acunetix', 'nessus', 'hydra', 'w3af', 'fimap']);

  fScan := TMVCAPIGuardRule.Create(GUARD_RULE_SCAN, gpResponse, 20, 60, 3600).OnStatusCodes([404, 405]);
  fBruteForce := TMVCAPIGuardRule.Create(GUARD_RULE_BRUTEFORCE, gpResponse, 10, 300, 3600).OnStatusCodes([401, 403]);
  fFlood := TMVCAPIGuardRule.Create(GUARD_RULE_FLOOD, gpRequest, 600, 60, 600);
  fInjection := TMVCAPIGuardRule.Create(GUARD_RULE_INJECTION, gpRequest, 3, 300, 86400, MatchesSignature);
  fBadAgent := TMVCAPIGuardRule.Create(GUARD_RULE_BAD_AGENT, gpRequest, 1, 60, 86400, MatchesBadUserAgent);
  fRules.AddRange([fScan, fBruteForce, fFlood, fInjection, fBadAgent]);

  if Assigned(fStorage) then
  begin
    lNow := Now;
    for lEntry in fStorage.Load do
      if (lEntry.IP.Trim <> '') and not lEntry.IsExpired(lNow) then
      begin
        lNormalized := lEntry;
        lNormalized.IP := APIGuardNormalizeIP(lEntry.IP);
        fBlocked.AddOrSetValue(lNormalized.IP, lNormalized);
      end;
  end;

  // last: if Load raised, there is no thread to stop
  fWriter := TThread.CreateAnonymousThread(WriterLoop);
  fWriter.FreeOnTerminate := False;
  fWriter.Start;
end;

destructor TMVCAPIGuard.Destroy;
begin
  if Assigned(fWriter) then
  begin
    // the writer flushes what is still queued before it ends
    fStopping := True;
    fWake.SetEvent;
    fWriter.WaitFor;
    fWriter.Free;
  end;
  fWake.Free;
  fPendingEvents.Free;
  fAlerts.Free;
  fBadUserAgents.Free;
  fSignatures.Free;
  fMonitorIPs.Free;
  fWhitelist.Free;
  fCounters.Free;
  fBlocked.Free;
  fRules.Free;
  fLock.Free;
  inherited;
end;

function TMVCAPIGuard.AddRule(const AName: string; const APhase: TMVCAPIGuardPhase;
  const AThreshold, AWindowSeconds, ABlockSeconds: Integer;
  const AMatcher: TMVCAPIGuardMatcher): TMVCAPIGuardRule;
var
  lRule: TMVCAPIGuardRule;
begin
  for lRule in fRules do
    if SameText(lRule.Name, AName) then
      raise EMVCException.CreateFmt('APIGuard: rule "%s" already exists', [AName]);
  Result := TMVCAPIGuardRule.Create(AName, APhase, AThreshold, AWindowSeconds, ABlockSeconds, AMatcher);
  fRules.Add(Result);
end;

procedure TMVCAPIGuard.AddWhitelistIP(const AIP: string);
begin
  fWhitelist.Add(APIGuardNormalizeIP(AIP));
end;

procedure TMVCAPIGuard.AddMonitorIP(const AIP: string);
begin
  fMonitorIPs.Add(APIGuardNormalizeIP(AIP));
end;

procedure TMVCAPIGuard.AddMonitorOnlyPath(const APath: string);
begin
  fMonitorPaths := fMonitorPaths + [APath.ToLower.TrimRight(['/'])];
end;

procedure TMVCAPIGuard.AddSignature(const ASignature: string);
begin
  fSignatures.Add(ASignature.ToLower);
end;

procedure TMVCAPIGuard.ClearSignatures;
begin
  fSignatures.Clear;
end;

procedure TMVCAPIGuard.AddBadUserAgent(const AAgent: string);
begin
  fBadUserAgents.Add(AAgent.ToLower);
end;

procedure TMVCAPIGuard.ClearBadUserAgents;
begin
  fBadUserAgents.Clear;
end;

function TMVCAPIGuard.SourceIP(const AContext: TWebContext): string;
begin
  if fUseClientIp then
    Exit(APIGuardNormalizeIP(AContext.Request.ClientIp));
  Result := AContext.Request.PeerIp;
  // an adapter that cannot see the socket: the forwarded IP is all there is
  if Result = '' then
    Result := AContext.Request.ClientIp;
  // Indy reports IPv6 expanded (0:0:0:0:0:0:0:1): one spelling per address
  Result := APIGuardNormalizeIP(Result);
end;

function TMVCAPIGuard.IsMonitorIP(const AContext: TWebContext): Boolean;
var
  lPeer: string;
begin
  lPeer := APIGuardNormalizeIP(AContext.Request.PeerIp);
  if lPeer = '' then
    Exit(False);
  // trusted proxy: ClientIp takes the hop the proxy wrote, not the client's
  if MVCTrustProxyForwardedHeaders then
    Exit(fMonitorIPs.Contains(APIGuardNormalizeIP(AContext.Request.ClientIp)));
  // a proxy on the same host makes every client look like loopback: a forwarded
  // request is not a monitor unless the proxy is trusted (see above)
  if (AContext.Request.Headers['X-Forwarded-For'] <> '') or
    (AContext.Request.Headers['X-Real-IP'] <> '') or
    (AContext.Request.Headers['Forwarded'] <> '') then
    Exit(False);
  Result := fMonitorIPs.Contains(lPeer);
end;

function TMVCAPIGuard.PathMatches(const APath: string;
  const APrefixes: TArray<string>): Boolean;
var
  lPath, lPrefix: string;
begin
  if Length(APrefixes) = 0 then
    Exit(True);
  lPath := APath.ToLower;
  for lPrefix in APrefixes do
    if (lPath = lPrefix) or lPath.StartsWith(lPrefix + '/') then
      Exit(True);
  Result := False;
end;

function TMVCAPIGuard.MatchesSignature(const AContext: TWebContext): Boolean;
var
  lRaw, lDecoded, lSignature: string;
begin
  lRaw := (AContext.Request.RawPathInfo + '?' + AContext.Request.QueryString).ToLower;
  try
    // twice: %252e%252e%252f is a classic way past a single decode
    lDecoded := TNetEncoding.URL.Decode(TNetEncoding.URL.Decode(lRaw)).ToLower;
  except
    lDecoded := lRaw;
  end;
  lDecoded := lDecoded.Replace('+', ' ');
  for lSignature in fSignatures do
    if lRaw.Contains(lSignature) or lDecoded.Contains(lSignature) then
      Exit(True);
  Result := False;
end;

function TMVCAPIGuard.MatchesBadUserAgent(const AContext: TWebContext): Boolean;
var
  lAgent, lBad: string;
begin
  lAgent := AContext.Request.Headers['User-Agent'].ToLower;
  if lAgent = '' then
    Exit(False);
  for lBad in fBadUserAgents do
    if lAgent.Contains(lBad) then
      Exit(True);
  Result := False;
end;

procedure TMVCAPIGuard.CountRequest;
var
  lSecond: Int64;
  lIdx: Integer;
begin
  lSecond := DateTimeToUnix(Now);
  lIdx := lSecond mod 60;
  if fSecondStamps[lIdx] <> lSecond then
  begin
    fSecondStamps[lIdx] := lSecond;
    fSecondBuckets[lIdx] := 0;
  end;
  Inc(fSecondBuckets[lIdx]);
  Inc(fRequestsTotal);
end;

function TMVCAPIGuard.RequestsLastMinute: Integer;
var
  lSecond: Int64;
  I: Integer;
begin
  lSecond := DateTimeToUnix(Now);
  Result := 0;
  for I := 0 to 59 do
    if fSecondStamps[I] > lSecond - 60 then
      Inc(Result, fSecondBuckets[I]);
end;

procedure TMVCAPIGuard.Sweep(const ANow: TDateTime);
var
  lKeys: TArray<string>;
  lKey: string;
  lChanged: Boolean;
  lMaxWindow: Integer;
  lRule: TMVCAPIGuardRule;
begin
  if SecondsBetween(ANow, fLastSweep) < 60 then
    Exit;
  fLastSweep := ANow;
  lMaxWindow := 60;
  for lRule in fRules do
    if lRule.WindowSeconds > lMaxWindow then
      lMaxWindow := lRule.WindowSeconds;
  lKeys := fCounters.Keys.ToArray;
  for lKey in lKeys do
    if IncSecond(fCounters[lKey].WindowStart, lMaxWindow) < ANow then
      fCounters.Remove(lKey);
  lChanged := False;
  lKeys := fBlocked.Keys.ToArray;
  for lKey in lKeys do
    if fBlocked[lKey].IsExpired(ANow) then
    begin
      QueueEventLocked(GUARD_AUDIT_UNBLOCK, lKey, fBlocked[lKey].Rule, '', 'block expired',
        fBlocked[lKey].ExpiresAt);
      fBlocked.Remove(lKey);
      lChanged := True;
    end;
  if lChanged then
    PersistLocked;
end;

function TMVCAPIGuard.IsBlockedLocked(const AIP: string; const ANow: TDateTime): Boolean;
var
  lEntry: TMVCAPIGuardBlockedIP;
begin
  // an expired entry stays in fBlocked until the next Sweep: it no longer counts
  Result := fBlocked.TryGetValue(AIP, lEntry) and not lEntry.IsExpired(ANow);
end;

function TMVCAPIGuard.ActiveBlocksLocked(const ANow: TDateTime): TArray<TMVCAPIGuardBlockedIP>;
var
  lEntry: TMVCAPIGuardBlockedIP;
begin
  Result := nil;
  for lEntry in fBlocked.Values do
    if not lEntry.IsExpired(ANow) then
      Result := Result + [lEntry];
end;

procedure TMVCAPIGuard.PersistLocked;
begin
  if not Assigned(fStorage) then
    Exit;
  // the writer stores the list as it is then: close changes become one Store
  fStoreDirty := True;
  fWake.SetEvent;
end;

procedure TMVCAPIGuard.QueueEventLocked(const AEvent, AIP, ARule, APath,
  AMessage: string; const ATimestamp: TDateTime);
var
  lEvent: TMVCAPIGuardAuditEvent;
begin
  if not Assigned(fAuditLog) then
    Exit;
  if fPendingEvents.Count >= MAX_PENDING_EVENTS then
  begin
    Inc(fDroppedEvents);
    Exit;
  end;
  lEvent.Timestamp := ATimestamp;
  lEvent.Event := AEvent;
  lEvent.IP := AIP;
  lEvent.Rule := ARule;
  lEvent.Path := APath;
  lEvent.Message := AMessage;
  fPendingEvents.Add(lEvent);
  fWake.SetEvent;
end;

procedure TMVCAPIGuard.FlushPending;
var
  lStore: Boolean;
  lSnapshot: TArray<TMVCAPIGuardBlockedIP>;
  lEvents: TArray<TMVCAPIGuardAuditEvent>;
  lDropped: Int64;
  lAuditLog: IMVCAPIGuardAuditLog;
begin
  // copy under the lock, write outside it: requests never wait for a file or a database
  fLock.Enter;
  try
    lStore := fStoreDirty;
    fStoreDirty := False;
    if lStore then
      lSnapshot := fBlocked.Values.ToArray;
    lEvents := fPendingEvents.ToArray;
    fPendingEvents.Clear;
    lDropped := fDroppedEvents;
    fDroppedEvents := 0;
    lAuditLog := fAuditLog;
  finally
    fLock.Leave;
  end;

  if lStore then
  try
    fStorage.Store(lSnapshot);
  except
    // the in-memory blacklist still works: losing the file must not take the API down
    on E: Exception do
      Log.Error('APIGuard: cannot persist the blacklist: [%s] %s', [E.ClassName, E.Message], LOG_TAG);
  end;

  if lDropped > 0 then
    Log.Error('APIGuard: %d audit events dropped, the audit log cannot keep up', [lDropped], LOG_TAG);
  if not Assigned(lAuditLog) then
    Exit;
  if Length(lEvents) > 0 then
  try
    lAuditLog.Append(lEvents);
  except
    on E: Exception do
      Log.Error('APIGuard: cannot write %d audit events: [%s] %s',
        [Length(lEvents), E.ClassName, E.Message], LOG_TAG);
  end;

  if (fAuditRetentionDays > 0) and (MinutesBetween(Now, fLastPurge) >= 60) then
  begin
    fLastPurge := Now;
    try
      lAuditLog.Purge(IncDay(fLastPurge, -fAuditRetentionDays));
    except
      on E: Exception do
        Log.Error('APIGuard: cannot purge the audit log: [%s] %s', [E.ClassName, E.Message], LOG_TAG);
    end;
  end;
end;

procedure TMVCAPIGuard.WriterLoop;
begin
  // wakes on every change, and hourly for the audit retention
  repeat
    fWake.WaitFor(WRITER_IDLE_MS);
    FlushPending;
  until fStopping;
  FlushPending; // what was queued while the last flush was writing
end;

procedure TMVCAPIGuard.AddAlertLocked(const AIP, ARule, APath, AMessage: string);
var
  lAlert: TMVCAPIGuardAlert;
begin
  lAlert.Timestamp := Now;
  lAlert.IP := AIP;
  lAlert.Rule := ARule;
  lAlert.Path := APath;
  lAlert.Message := AMessage;
  fAlerts.Add(lAlert);
  while fAlerts.Count > fMaxAlerts do
    fAlerts.Delete(0);
  Log.Warn('APIGuard: %s [ip %s] [rule %s] [path %s]', [AMessage, AIP, ARule, APath], LOG_TAG);
  QueueEventLocked(GUARD_AUDIT_BLOCK, AIP, ARule, APath, AMessage, lAlert.Timestamp);
  if Assigned(fOnAlert) then
  try
    fOnAlert(lAlert);
  except
    on E: Exception do
      Log.Error('APIGuard: OnAlert raised [%s] %s', [E.ClassName, E.Message], LOG_TAG);
  end;
end;

procedure TMVCAPIGuard.BlockLocked(const AIP, ARule, AReason: string;
  const ABlockSeconds: Integer);
var
  lEntry: TMVCAPIGuardBlockedIP;
begin
  lEntry.IP := AIP;
  lEntry.Rule := ARule;
  lEntry.Reason := AReason;
  lEntry.BlockedAt := Now;
  lEntry.ExpiresAt := 0;
  if ABlockSeconds > 0 then
    lEntry.ExpiresAt := IncSecond(lEntry.BlockedAt, ABlockSeconds);
  fBlocked.AddOrSetValue(AIP, lEntry);
  PersistLocked;
end;

function TMVCAPIGuard.EvaluateRules(const AContext: TWebContext;
  const APhase: TMVCAPIGuardPhase; const AIP: string): Boolean;
var
  lRule: TMVCAPIGuardRule;
  lKey, lPath: string;
  lCounter: TCounter;
  lNow: TDateTime;
begin
  Result := False;
  lPath := AContext.Request.PathInfo;
  for lRule in fRules do
  begin
    if (not lRule.Enabled) or (lRule.Phase <> APhase) or (not PathMatches(lPath, lRule.fPaths)) then
      Continue;
    // matcher outside the lock: custom matchers may be slow
    if not lRule.Matches(AContext) then
      Continue;
    lNow := Now;
    lKey := lRule.Name + '|' + AIP;
    fLock.Enter;
    try
      Inc(lRule.fDetections);
      // shortcut: fixed window, a burst across two windows can reach 2x threshold; use timestamps if that matters
      if (not fCounters.TryGetValue(lKey, lCounter))
        or (IncSecond(lCounter.WindowStart, lRule.WindowSeconds) <= lNow) then
      begin
        lCounter.WindowStart := lNow;
        lCounter.Count := 0;
      end;
      Inc(lCounter.Count);
      fCounters.AddOrSetValue(lKey, lCounter);
      if (lCounter.Count >= lRule.Threshold) and not IsBlockedLocked(AIP, lNow) then
      begin
        Inc(lRule.fBlocks);
        fCounters.Remove(lKey);
        BlockLocked(AIP, lRule.Name,
          Format('%d occurrences in %ds', [lCounter.Count, lRule.WindowSeconds]), lRule.BlockSeconds);
        AddAlertLocked(AIP, lRule.Name, lPath,
          Format('IP blocked: %d occurrences in %ds', [lCounter.Count, lRule.WindowSeconds]));
        Result := True;
      end;
    finally
      fLock.Leave;
    end;
    if Result then
      Exit;
  end;
end;

procedure TMVCAPIGuard.SendForbidden(const AContext: TWebContext);
begin
  // no rule name, no expiry: nothing that helps tune the attack
  AContext.Response.StatusCode := HTTP_STATUS.Forbidden;
  AContext.Response.ReasonString := 'Forbidden';
  AContext.Response.ContentType := TMVCMediaType.APPLICATION_JSON;
  AContext.Response.Content := '{"error":"Forbidden"}';
end;

procedure TMVCAPIGuard.SendNotFound(const AContext: TWebContext);
begin
  AContext.Response.StatusCode := HTTP_STATUS.NotFound;
  AContext.Response.ReasonString := 'Not Found';
  AContext.Response.ContentType := TMVCMediaType.APPLICATION_JSON;
  AContext.Response.Content := '{"error":"Not Found"}';
end;

procedure TMVCAPIGuard.SendStatus(const AContext: TWebContext);
var
  lJSON, lItem: TJsonObject;
  lBlockedIPs, lRules, lAlerts: TJsonArray;
  lRule: TMVCAPIGuardRule;
  lEntry: TMVCAPIGuardBlockedIP;
  lActive: TArray<TMVCAPIGuardBlockedIP>;
  I: Integer;
begin
  lJSON := TJsonObject.Create;
  try
    fLock.Enter;
    try
      lActive := ActiveBlocksLocked(Now);
      lJSON.S['started_at'] := DateTimeToISOTimeStamp(fStartedAt);
      lJSON.L['uptime_seconds'] := SecondsBetween(Now, fStartedAt);
      lJSON.I['requests_last_minute'] := RequestsLastMinute;
      lJSON.L['requests_total'] := fRequestsTotal;
      lJSON.L['blocked_requests_total'] := fBlockedRequestsTotal;
      lJSON.I['blocked_ips_count'] := Length(lActive);
      lBlockedIPs := lJSON.A['blocked_ips'];
      lRules := lJSON.A['rules'];
      lAlerts := lJSON.A['alerts'];
      for lEntry in lActive do
      begin
        lItem := lBlockedIPs.AddObject;
        lItem.S['ip'] := lEntry.IP;
        lItem.S['rule'] := lEntry.Rule;
        lItem.S['reason'] := lEntry.Reason;
        lItem.S['blocked_at'] := DateTimeToISOTimeStamp(lEntry.BlockedAt);
        if lEntry.ExpiresAt = 0 then
          lItem.Values['expires_at'] := nil
        else
          lItem.S['expires_at'] := DateTimeToISOTimeStamp(lEntry.ExpiresAt);
      end;
      for lRule in fRules do
      begin
        lItem := lRules.AddObject;
        lItem.S['name'] := lRule.Name;
        lItem.B['enabled'] := lRule.Enabled;
        if lRule.Phase = gpRequest then
          lItem.S['phase'] := 'request'
        else
          lItem.S['phase'] := 'response';
        lItem.I['threshold'] := lRule.Threshold;
        lItem.I['window_seconds'] := lRule.WindowSeconds;
        lItem.I['block_seconds'] := lRule.BlockSeconds;
        lItem.L['detections'] := lRule.fDetections;
        lItem.L['blocks'] := lRule.fBlocks;
      end;
      for I := fAlerts.Count - 1 downto 0 do
      begin
        lItem := lAlerts.AddObject;
        lItem.S['timestamp'] := DateTimeToISOTimeStamp(fAlerts[I].Timestamp);
        lItem.S['ip'] := fAlerts[I].IP;
        lItem.S['rule'] := fAlerts[I].Rule;
        lItem.S['path'] := fAlerts[I].Path;
        lItem.S['message'] := fAlerts[I].Message;
      end;
    finally
      fLock.Leave;
    end;
    AContext.Response.StatusCode := HTTP_STATUS.OK;
    AContext.Response.ContentType := TMVCMediaType.APPLICATION_JSON;
    AContext.Response.SetCustomHeader('Cache-Control', 'no-store');
    AContext.Response.Content := lJSON.ToJSON;
  finally
    lJSON.Free;
  end;
end;

procedure TMVCAPIGuard.BlockIP(const AIP, AReason: string; const ABlockSeconds: Integer);
begin
  fLock.Enter;
  try
    BlockLocked(APIGuardNormalizeIP(AIP), GUARD_RULE_MANUAL, AReason, ABlockSeconds);
    AddAlertLocked(APIGuardNormalizeIP(AIP), GUARD_RULE_MANUAL, '', 'IP blocked manually: ' + AReason);
  finally
    fLock.Leave;
  end;
end;

procedure TMVCAPIGuard.UnblockIP(const AIP: string);
var
  lIP: string;
  lEntry: TMVCAPIGuardBlockedIP;
begin
  lIP := APIGuardNormalizeIP(AIP);
  fLock.Enter;
  try
    if fBlocked.TryGetValue(lIP, lEntry) then
    begin
      fBlocked.Remove(lIP);
      QueueEventLocked(GUARD_AUDIT_UNBLOCK, lIP, lEntry.Rule, '', 'unblocked manually', Now);
      PersistLocked;
    end;
  finally
    fLock.Leave;
  end;
end;

function TMVCAPIGuard.IsBlocked(const AIP: string): Boolean;
begin
  fLock.Enter;
  try
    Result := IsBlockedLocked(APIGuardNormalizeIP(AIP), Now);
  finally
    fLock.Leave;
  end;
end;

function TMVCAPIGuard.BlockedIPs: TArray<TMVCAPIGuardBlockedIP>;
begin
  fLock.Enter;
  try
    Result := ActiveBlocksLocked(Now);
  finally
    fLock.Leave;
  end;
end;

procedure TMVCAPIGuard.Reset;
var
  lRule: TMVCAPIGuardRule;
  lEntry: TMVCAPIGuardBlockedIP;
  I: Integer;
begin
  fLock.Enter;
  try
    for lEntry in fBlocked.Values do
      QueueEventLocked(GUARD_AUDIT_UNBLOCK, lEntry.IP, lEntry.Rule, '', 'reset', Now);
    fCounters.Clear;
    fBlocked.Clear;
    fAlerts.Clear;
    fRequestsTotal := 0;
    fBlockedRequestsTotal := 0;
    for I := 0 to 59 do
    begin
      fSecondBuckets[I] := 0;
      fSecondStamps[I] := 0;
    end;
    for lRule in fRules do
    begin
      lRule.fDetections := 0;
      lRule.fBlocks := 0;
    end;
    PersistLocked;
  finally
    fLock.Leave;
  end;
end;

function APIGuardHTTPFilter(const AGuard: IMVCAPIGuard): TMVCHTTPFilter;
var
  lGuard: IMVCAPIGuard;
begin
  if not Assigned(AGuard) then
    raise EMVCConfigException.Create('APIGuardHTTPFilter: the guard is nil');
  lGuard := AGuard; // the closure owns the guard: released with the engine's filters
  Result :=
    procedure(const AContext: TWebContext; const ANext: TMVCHTTPFilterNext)
    begin
      if lGuard.HandleRequest(AContext) then
        Exit;
      ANext();
      lGuard.HandleResponse(AContext);
    end;
end;

function TMVCAPIGuard.GetScan: TMVCAPIGuardRule;
begin
  Result := fScan;
end;

function TMVCAPIGuard.GetBruteForce: TMVCAPIGuardRule;
begin
  Result := fBruteForce;
end;

function TMVCAPIGuard.GetFlood: TMVCAPIGuardRule;
begin
  Result := fFlood;
end;

function TMVCAPIGuard.GetInjection: TMVCAPIGuardRule;
begin
  Result := fInjection;
end;

function TMVCAPIGuard.GetBadAgent: TMVCAPIGuardRule;
begin
  Result := fBadAgent;
end;

function TMVCAPIGuard.GetUseClientIp: Boolean;
begin
  Result := fUseClientIp;
end;

procedure TMVCAPIGuard.SetUseClientIp(const AValue: Boolean);
begin
  fUseClientIp := AValue;
end;

function TMVCAPIGuard.GetMaxAlerts: Integer;
begin
  Result := fMaxAlerts;
end;

procedure TMVCAPIGuard.SetMaxAlerts(const AValue: Integer);
begin
  fMaxAlerts := AValue;
end;

function TMVCAPIGuard.GetOnAlert: TMVCAPIGuardAlertProc;
begin
  Result := fOnAlert;
end;

procedure TMVCAPIGuard.SetOnAlert(const AValue: TMVCAPIGuardAlertProc);
begin
  fOnAlert := AValue;
end;

function TMVCAPIGuard.GetAuditLog: IMVCAPIGuardAuditLog;
begin
  Result := fAuditLog;
end;

procedure TMVCAPIGuard.SetAuditLog(const AValue: IMVCAPIGuardAuditLog);
begin
  fAuditLog := AValue;
end;

function TMVCAPIGuard.GetAuditRetentionDays: Integer;
begin
  Result := fAuditRetentionDays;
end;

procedure TMVCAPIGuard.SetAuditRetentionDays(const AValue: Integer);
begin
  fAuditRetentionDays := AValue;
end;

function TMVCAPIGuard.HandleRequest(const AContext: TWebContext): Boolean;
var
  lIP: string;
  lEntry: TMVCAPIGuardBlockedIP;
  lBlocked, lStatus, lMonitorPath, lMonitorPass, lDenied: Boolean;
  lNow: TDateTime;
begin
  Result := False;
  lIP := SourceIP(AContext);
  lNow := Now;
  // the status endpoint answers its monitors even while their IP is blocked;
  // anyone else goes on as for any other path (usually a 404 that "scan" counts)
  lStatus := (fStatusPath <> '') and SameText(AContext.Request.PathInfo.TrimRight(['/']), fStatusPath)
    and IsMonitorIP(AContext);
  // monitor-only paths go on for monitors (e.g. to the static files), 404 for anyone else
  lMonitorPath := (Length(fMonitorPaths) > 0) and PathMatches(AContext.Request.PathInfo, fMonitorPaths);
  lMonitorPass := lMonitorPath and IsMonitorIP(AContext);

  fLock.Enter;
  try
    // the dashboard's own polling is not traffic: it would inflate the counters it shows
    if not (lStatus or lMonitorPass) then
      CountRequest;
    Sweep(lNow);
    lBlocked := IsBlockedLocked(lIP, lNow);
  finally
    fLock.Leave;
  end;

  if lStatus then
  begin
    AContext.Data[SKIP_FLAG] := '1';
    SendStatus(AContext);
    Result := True;
    Exit;
  end;

  if lMonitorPass then
  begin
    AContext.Data[SKIP_FLAG] := '1';
    Exit;
  end;
  lDenied := lMonitorPath;

  if fWhitelist.Contains(lIP) then
  begin
    AContext.Data[SKIP_FLAG] := '1';
    if lDenied then
    begin
      SendNotFound(AContext);
      Result := True;
    end;
    Exit;
  end;

  if not lBlocked then
    lBlocked := EvaluateRules(AContext, gpRequest, lIP);

  if lBlocked then
  begin
    fLock.Enter;
    try
      Inc(fBlockedRequestsTotal);
      // re-read: EvaluateRules may have just added the entry
      if fBlocked.TryGetValue(lIP, lEntry) then
        QueueEventLocked(GUARD_AUDIT_DENIED, lIP, lEntry.Rule, AContext.Request.PathInfo,
          'request refused', Now);
    finally
      fLock.Leave;
    end;
    AContext.Data[SKIP_FLAG] := '1';
    SendForbidden(AContext);
    Result := True;
  end
  else if lDenied then
  begin
    SendNotFound(AContext); // no skip flag: "scan" counts it like any other 404
    Result := True;
  end;
end;

procedure TMVCAPIGuard.HandleResponse(const AContext: TWebContext);
begin
  if AContext.Data.ContainsKey(SKIP_FLAG) then
    Exit;
  // a block here takes effect from the next request: this response is already built
  EvaluateRules(AContext, gpResponse, SourceIP(AContext));
end;

end.
