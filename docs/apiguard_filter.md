# APIGuard HTTP Filter

Contributed by Giordano Giaccaglia, [KD Software S.r.l.s.](https://www.kdsoftware.it) ([info@kdsoftware.it](mailto:info@kdsoftware.it)).

`APIGuardHTTPFilter` (`MVCFramework.Filters.APIGuard`) is an intrusion guard for DelphiMVCFramework
services, as an HTTP filter. It watches every
request and every response per client IP, and when an IP behaves like an attacker — path scanning,
password brute force, flooding, injection probes, known attack tools, or anything you describe with a
custom rule — it **blocks the IP**. A blocked IP gets `403` until the block expires, or forever.

The blacklist can be kept in memory, in a local text file or in a database table, and so can the
history of blocks, unblocks and refused requests (the audit log). Both are written by a background
thread, so a request never waits for a file or a database. A JSON status
endpoint and an optional HTML dashboard, reachable only from localhost (or from IPs you list),
report requests per minute, blocked IPs, per-rule counters and the latest alerts.

The guard is additive and opt-in: nothing changes until you register it on the engine. It works the
same in front of controllers and Minimal API routes.

- Sample: [`samples/filters_apiguard`](../samples/filters_apiguard) (Minimal API server, dashboard, admin routes, attack simulator)
- Tests: `unittests/general/TestClient/APIGuardFilterTestsU.pas` (`TAPIGuardFilterTests`)

---

## Contents

1. [Units](#units)
2. [Quick start](#quick-start)
3. [How a request is processed](#how-a-request-is-processed)
4. [Rules](#rules)
5. [Signatures and user agents](#signatures-and-user-agents)
6. [Which IP is blocked](#which-ip-is-blocked)
7. [Whitelist](#whitelist)
8. [Blacklist storage](#blacklist-storage)
9. [Status endpoint](#status-endpoint)
10. [Dashboard and monitor-only paths](#dashboard-and-monitor-only-paths)
11. [Alerts](#alerts)
    - [Audit log](#audit-log)
12. [Managing blocks from code](#managing-blocks-from-code)
13. [Responses sent by the guard](#responses-sent-by-the-guard)
14. [Thread safety and configuration timing](#thread-safety-and-configuration-timing)
15. [Recipes](#recipes)
16. [Limitations](#limitations)
17. [API reference](#api-reference)

---

## Units

| Unit | Contents | Extra dependency |
|------|----------|------------------|
| `MVCFramework.Filters.APIGuard` | `TMVCAPIGuard`, `APIGuardHTTPFilter`, rules, file storage and file audit log, `APIGuardNormalizeIP` | none |
| `MVCFramework.Filters.APIGuard.FireDAC` | `TMVCAPIGuardFireDACStorage` and `TMVCAPIGuardFireDACAuditLog`, the blacklist and the audit log in tables | FireDAC + the driver unit of your database |

Both units are in the `dmvcframeworkRT` runtime package and compile on every supported Delphi
(10.2 Tokyo and later). The guard is host-independent: Indy Direct, HTTP.sys and WebBroker
(ISAPI / Apache) run the same code.

---

## Quick start

```delphi
uses
  MVCFramework.Filters.APIGuard;

procedure ConfigureEngine(AEngine: TMVCEngine);
var
  lGuard: TMVCAPIGuard;
begin
  lGuard := TMVCAPIGuard.Create(
    TMVCAPIGuardFileStorage.Create(TPath.Combine(AppPath, 'blacklist.txt')));
  lGuard.Rule(GUARD_RULE_BRUTEFORCE).OnPaths(['/auth/login']);

  AEngine.Use(APIGuardHTTPFilter(lGuard));   // FIRST filter
  // ... the other filters, then the controllers or the Minimal API routes
  AEngine.AddController(TMyController);
end;
```

That is a working setup: five built-in rules with production defaults, the blacklist persisted next
to the executable, and `GET /api-guard/status` available from localhost.

**Register the guard as the first filter.** HTTP filters wrap one another in the order they are
registered, the first being the outermost, and all of them wrap the classic middleware whatever the
order. Registered first, the guard sees every request before anything can answer it (static files,
CORS preflight, a login, a rate limit), and every final status after: a `401` written by an
authentication bundle is counted like one written by an action.

**Lifetime.** `TMVCAPIGuard` is reference counted. `APIGuardHTTPFilter` keeps the guard alive for as
long as the engine keeps the filter, and the guard goes when the engine is freed. Do not free
`lGuard`. Keeping the object reference to configure it, or to call `BlockIP` later (for example from
a Minimal API route or a controller), is fine. `APIGuardHTTPFilter(nil)` raises `EMVCConfigException`.

Register a guard once: two filters over the same guard would count every request twice.

---

## How a request is processed

Before `ANext()`, that is before routing and before the classic middleware:

1. The request is counted in the statistics. About once a minute, expired counters and expired
   blocks are removed here.
2. **Status endpoint.** If the path is the status path and the caller is a monitor IP, the guard
   answers with the JSON status and does not call `ANext()`. This happens even while that IP is
   blocked.
3. **Monitor-only paths.** For a monitor IP, the request goes on (`ANext()`) to the next filter,
   for example the static files serving the dashboard. For anyone else, it is marked as denied.
4. **Whitelist.** A whitelisted IP is neither counted nor blocked. It still gets 404 on a
   monitor-only path.
5. **Blocked IP.** An IP that is already blocked gets `403`, and `ANext()` is not called.
6. **Request rules** (`gpRequest`) are evaluated. If one trips, the IP is blocked and this request
   already gets `403`.
7. A denied monitor-only path gets `404`.

After `ANext()` returns, the response is complete, whoever wrote it (an action, a Minimal API
handler, another filter, the router's 404):

8. **Response rules** (`gpResponse`) are evaluated, for example on the 401 of a failed login. If one
   trips, the IP is blocked **from the next request on**, because this response is already built.

Requests the guard answered itself (`403`, status endpoint, whitelisted) are not evaluated again
after `ANext()`. The `404` for a denied monitor-only path is evaluated, so `scan` counts it.

---

## Rules

### Built-in rules

| Constant | Name | Phase | Counts | Threshold | Window | Block |
|----------|------|-------|--------|-----------|--------|-------|
| `GUARD_RULE_SCAN` | `scan` | response | 404 and 405 responses | 20 | 60 s | 1 h |
| `GUARD_RULE_BRUTEFORCE` | `bruteforce` | response | 401 and 403 responses | 10 | 300 s | 1 h |
| `GUARD_RULE_FLOOD` | `flood` | request | every request | 600 | 60 s | 10 min |
| `GUARD_RULE_INJECTION` | `injection` | request | requests matching a [signature](#signatures-and-user-agents) | 3 | 300 s | 24 h |
| `GUARD_RULE_BAD_AGENT` | `bad_agent` | request | User-Agent of a known attack tool | 1 | 60 s | 24 h |

`GUARD_RULE_MANUAL` (`manual`) is not a rule. It is the name stored for blocks made with `BlockIP`,
and for hand-written lines in the blacklist file.

### How counting works

- Every rule keeps one counter **per rule and per IP**.
- A counter counts the occurrences in a **fixed window** of `WindowSeconds`, which starts at the first
  occurrence. When the window is over, the next occurrence starts a new one.
- When the counter reaches `Threshold`, the IP is blocked for `BlockSeconds` and the counter is
  reset. **`BlockSeconds = 0` blocks permanently.**
- Rules are evaluated in order: the built-in ones, then the custom ones in the order they were
  added. The first rule that trips blocks the IP, and the rest are not evaluated for that request.
- `detections` (in the status) counts every match. `blocks` counts the blocks the rule caused.

### Tuning a rule

`Rule(AName)` returns the rule (the name is case-insensitive) and raises `EMVCException` if no rule
has that name. All the setters return the rule, so they can be chained:

```delphi
// threshold, window (s), block (s); 0 = permanent
lGuard.Rule(GUARD_RULE_SCAN).Configure(10, 60, 1800);

// count only under these paths: prefix match on a segment boundary, case-insensitive.
// '/auth' matches /auth and /auth/login, not /authors
lGuard.Rule(GUARD_RULE_BRUTEFORCE).Configure(5, 300, 3600).OnPaths(['/auth/login', '/api/token']);

// a response rule without a matcher counts these statuses (empty = every status)
lGuard.Rule(GUARD_RULE_SCAN).OnStatusCodes([404]);

// switch a rule off
lGuard.Rule(GUARD_RULE_FLOOD).Enabled := False;

// properties can also be set one by one
lGuard.Rule(GUARD_RULE_INJECTION).BlockSeconds := 0;   // permanent
```

`Configure` validates its arguments: threshold and window must be at least 1, and the block time at
least 0. Otherwise it raises `EMVCException`. Setting the properties directly is not validated.

### Custom rules

```delphi
function AddRule(const AName: string; const APhase: TMVCAPIGuardPhase;
  const AThreshold, AWindowSeconds, ABlockSeconds: Integer;
  const AMatcher: TMVCAPIGuardMatcher = nil): TMVCAPIGuardRule;

TMVCAPIGuardMatcher = reference to function(const AContext: TWebContext): Boolean;
```

The matcher returns `True` when the request is **one occurrence** of what the rule counts. The
guard owns the rule. A name that is already in use raises `EMVCException`.

- `gpRequest`: the matcher runs before routing and can read only the request.
- `gpResponse`: the matcher runs after routing and can also read `AContext.Response.StatusCode`.
  A response rule with no matcher counts the statuses given with `OnStatusCodes`.
- A request rule with no matcher counts every request (that is how `flood` works).

```delphi
// honeypot: nobody legitimate asks for these files, one hit is enough
lGuard.AddRule('honeypot', gpRequest, 1, 60, 86400,
  function(const AContext: TWebContext): Boolean
  var
    lPath: string;
  begin
    lPath := AContext.Request.PathInfo.ToLower;
    Result := (lPath = '/backup.zip') or (lPath = '/admin.php');
  end);

// too many validation failures: someone fuzzing the API
lGuard.AddRule('fuzzing', gpResponse, 30, 60, 3600).OnStatusCodes([400, 422]);

// server errors caused by one client: probing for a crash
lGuard.AddRule('server_errors', gpResponse, 10, 60, 1800).OnStatusCodes([500]).OnPaths(['/api']);

// a method your API never serves
lGuard.AddRule('trace_method', gpRequest, 3, 60, 3600,
  function(const AContext: TWebContext): Boolean
  begin
    Result := AContext.Request.HTTPMethod = httpTRACE;
  end);
```

The matcher runs on the request thread and **outside** the guard's lock, so it may be slow without
stalling other requests. It must be thread-safe, and it must not raise: an exception in a matcher
becomes the request's error.

---

## Signatures and user agents

The `injection` rule looks for **lowercase substrings** in the path plus the query string. It checks
the raw form, and the form URL-decoded twice (with `+` read as a space). Decoding twice catches
`%252e%252e%252f` and the like.

Default signatures:

```
../   ..\   /etc/passwd   win.ini   <script   javascript:
union select   union all select   " or 1=1"   'or'1'='1   sleep(   benchmark(   xp_cmdshell
${jndi:   /.env   /.git/   /wp-admin   /wp-login   /phpmyadmin   cmd.exe   /bin/sh
```

The `bad_agent` rule looks for lowercase substrings in `User-Agent`:

```
sqlmap nikto nmap masscan zgrab nuclei dirbuster gobuster wpscan acunetix nessus hydra w3af fimap
```

```delphi
lGuard.AddSignature('/actuator/');        // stored lowercase
lGuard.AddBadUserAgent('python-requests');
lGuard.ClearSignatures;                   // then add only your own
lGuard.ClearBadUserAgents;
```

Signatures are a tripwire, not a WAF: they catch automated probes, not a determined attacker who
encodes around them. Keep validating input in the actions (see the `dmvcframework-security`
guidance). Check that your legitimate traffic does not contain a signature: an API that accepts file
paths or SQL text in a query parameter will trip `injection`. In that case, narrow the rule with
`OnPaths` or remove the signature.

---

## Which IP is blocked

By default the guard uses the **socket peer** (`Request.PeerIp`), not `X-Forwarded-For`. Those
headers are written by the client: trusting them would let an attacker dodge every block by changing
the header, or get somebody else's IP blocked.

| Deployment | Setting |
|------------|---------|
| Clients connect directly | default |
| Behind a reverse proxy (nginx, IIS ARR, a load balancer) that sets `X-Forwarded-For` | `lGuard.UseClientIp := True` **and** `MVCTrustProxyForwardedHeaders := True` |

With `UseClientIp := True` the guard uses `Request.ClientIp`. `ClientIp` honours the forwarded
headers only when the framework-wide switch `MVCTrustProxyForwardedHeaders` (unit
`MVCFramework.Commons`, default `False`) is on. It then takes the hop added by your proxy, counting
`MVCTrustedProxyCount` hops (default 1) from the right, and ignores what the client wrote before. Set
both switches, or the guard keeps seeing the proxy's IP:

```delphi
MVCTrustProxyForwardedHeaders := True;   // once, at startup
MVCTrustedProxyCount := 1;               // 2 behind a CDN plus a reverse proxy
lGuard.UseClientIp := True;
```

Without a proxy in front, leave both off. Otherwise a client could pose as any IP.

> **Behind a proxy, all clients share the proxy's address until you do this.** One attacker then
> blocks everyone. If the proxy runs on the same host, every proxied client also arrives as
> `127.0.0.1`, which is a **monitor IP**: do not publish the status endpoint and the monitor-only
> paths through the proxy (see the nginx recipe).

If a host adapter cannot see the socket (`PeerIp` is empty), the guard falls back to `ClientIp` for
blocking. For the **monitor check it never does**: there, an empty `PeerIp` means "not a monitor".

### IP normalization

The same address can be spelled in many ways. Indy, for example, reports `::1` as
`0:0:0:0:0:0:0:1`. Every IP the guard sees or is given passes through `APIGuardNormalizeIP`:

| Input | Normalized |
|-------|------------|
| `0:0:0:0:0:0:0:1`, `::1` | `::1` |
| `2001:0db8:0000:0000:0000:ff00:0042:8329` | `2001:db8::ff00:42:8329` |
| `[FE80:0:0:0:0:0:0:1%12]` | `fe80::1` |
| `::ffff:127.0.0.1`, `0:0:0:0:0:ffff:7f00:1` | `127.0.0.1` |
| `10.1.2.3` | `10.1.2.3` |

That covers: lower case, no leading zeros, the longest zero run as `::` (RFC 5952), IPv4-mapped
addresses as plain IPv4, and no brackets or zone. Input that does not parse is returned trimmed. The
monitor list, the whitelist, `BlockIP`, `UnblockIP`, `IsBlocked` and the entries loaded from storage
are all normalized, so you can write addresses in any form.

---

## Whitelist

```delphi
lGuard.AddWhitelistIP('10.0.0.6');   // e.g. your uptime probe, an internal batch job
```

A whitelisted IP is never counted and never blocked. It does **not** get monitor rights: monitor-only
paths still answer it with 404, and the status endpoint answers only monitor IPs.

Monitor IPs are not whitelisted either. Localhost can be blocked like any other IP, but the status
endpoint and the monitor-only paths keep answering it. If your own tests run from localhost, either
whitelist `127.0.0.1` or call `Reset` between tests.

---

## Blacklist storage

```delphi
IMVCAPIGuardStorage = interface
  function Load: TArray<TMVCAPIGuardBlockedIP>;
  procedure Store(const AEntries: TArray<TMVCAPIGuardBlockedIP>);
end;
```

- `Load` is called once, in the constructor. Entries that have already expired are dropped.
- `Store` receives the **whole list** after it changes: after a block, an unblock, the removal of
  expired blocks, and `Reset`. A blacklist is small and changes rarely, so rewriting it is simpler
  and safer than keeping a journal.
- `Store` runs on the guard's **writer thread**, never on a request thread and outside the guard's
  lock: a request that blocks an IP only marks the list as changed. Changes close together become one
  `Store` of the latest list. The writer is stopped when the guard is destroyed (when the engine is
  freed and releases the filter), and it writes what is still pending before it ends.
- If `Store` raises, the guard logs the error and keeps working with the list in memory: losing the
  file must not take the API down. The next change rewrites the whole list. If `Load` raises in the
  constructor, the exception propagates and the server does not start.

### Memory only

```delphi
lGuard := TMVCAPIGuard.Create;   // the blacklist is lost on restart
```

### Text file — `TMVCAPIGuardFileStorage`

```delphi
lGuard := TMVCAPIGuard.Create(
  TMVCAPIGuardFileStorage.Create(TPath.Combine(AppPath, 'blacklist.txt')));
```

UTF-8. One line per IP, with tab-separated fields:

```
# ip	rule	blocked_at	expires_at (empty = permanent)	reason
203.0.113.7	scan	2026-10-29T18:00:07.853+01:00	2026-10-29T19:00:07.853+01:00	20 occurrences in 60s
198.51.100.23	manual	2026-10-29T18:10:00.000+01:00		abuse report #1234
192.0.2.99
```

- Lines starting with `#` and empty lines are ignored.
- **A line with only an IP is a permanent manual block.** You can ban an address by adding a line
  and restarting.
- A date that does not parse does not stop the server: that entry becomes permanent.
- Tabs and line breaks inside the reason are replaced with spaces when the file is written.
- The file is read only at startup. Edits made while the server runs are overwritten the next time
  the list changes. To block at runtime, use `BlockIP`.

### Database — `TMVCAPIGuardFireDACStorage`

```delphi
uses
  FireDAC.Phys.PG,                             // the driver unit of your database
  MVCFramework.Filters.APIGuard.FireDAC;

lGuard := TMVCAPIGuard.Create(
  TMVCAPIGuardFireDACStorage.Create('MyConnDef', 'dmvc_ip_blacklist'));
```

The first argument is the name of a FireDAC connection definition, which must exist before the guard
is created, because `Load` runs in the constructor. The table name is optional (default
`dmvc_ip_blacklist`) and may contain only letters, digits, `_` and `.` (for a schema prefix);
anything else raises `EMVCException`, because the name is put into the SQL. The table must exist:

```sql
CREATE TABLE dmvc_ip_blacklist (
  ip         VARCHAR(64)  NOT NULL PRIMARY KEY,
  rule_name  VARCHAR(64)  NOT NULL,
  reason     VARCHAR(255),
  blocked_at TIMESTAMP    NOT NULL,
  expires_at TIMESTAMP              -- NULL = permanent
);
```

Adapt the types to your database (e.g. `DATETIME2` on SQL Server). Each `Load` and `Store` opens its
own connection from the definition. `Store` runs `DELETE` plus one `INSERT` per entry inside a
transaction, and rolls it back on error. The reason is truncated to 255 characters.

To ban an IP permanently from outside the application, insert a row with `expires_at` NULL and
restart the server.

> **One storage per running instance.** Every `Store` replaces the whole table with that process's
> list. Two instances sharing one table overwrite each other's blocks. See
> [Limitations](#limitations).

### Your own storage

Implement `IMVCAPIGuardStorage`, for example a JSON file or a key in a configuration service:

```delphi
type
  TMyStorage = class(TInterfacedObject, IMVCAPIGuardStorage)
  public
    function Load: TArray<TMVCAPIGuardBlockedIP>;
    procedure Store(const AEntries: TArray<TMVCAPIGuardBlockedIP>);
  end;
```

`TMVCAPIGuardBlockedIP` fields: `IP`, `Rule`, `Reason`, `BlockedAt`, `ExpiresAt` (local
`TDateTime`, `0` = permanent). `IsExpired(ANow)` tells whether an entry is over.

---

## Status endpoint

```delphi
TMVCAPIGuard.Create(AStorage, '/api-guard/status');   // the default path
TMVCAPIGuard.Create(AStorage, '/internal/guard');     // another path
TMVCAPIGuard.Create(AStorage, '');                    // no endpoint
```

The path is matched exactly (case-insensitive, trailing slash ignored), on any HTTP method. It answers
only the **monitor IPs**, which are `127.0.0.1` and `::1` by default:

```delphi
lGuard.AddMonitorIP('10.0.0.5');     // your monitoring host
```

The monitor check uses the socket peer only, never forwarded headers. Behind a reverse proxy, the
peer is the proxy: either do not expose the endpoint through the proxy (recommended), or read it from
the server itself. Anyone else goes through the normal flow and usually gets the application's 404,
which `scan` counts. The endpoint keeps answering monitors while their own IP is blocked.

Response: `200`, `application/json`, `Cache-Control: no-store`.

```json
{
  "started_at": "2026-10-29T18:15:56.120+01:00",
  "uptime_seconds": 3600,
  "requests_last_minute": 137,
  "requests_total": 48210,
  "blocked_requests_total": 21,
  "blocked_ips_count": 1,
  "blocked_ips": [
    {
      "ip": "127.0.0.3",
      "rule": "bruteforce",
      "reason": "5 occurrences in 300s",
      "blocked_at": "2026-10-29T18:15:56.293+01:00",
      "expires_at": "2026-10-29T18:16:56.293+01:00"
    }
  ],
  "rules": [
    {
      "name": "bruteforce", "enabled": true, "phase": "response",
      "threshold": 5, "window_seconds": 300, "block_seconds": 60,
      "detections": 5, "blocks": 1
    }
  ],
  "alerts": [
    {
      "timestamp": "2026-10-29T18:15:56.294+01:00",
      "ip": "127.0.0.3", "rule": "bruteforce", "path": "/api/login",
      "message": "IP blocked: 5 occurrences in 300s"
    }
  ]
}
```

| Field | Meaning |
|-------|---------|
| `requests_last_minute` | requests in the last 60 seconds (one-second buckets), all IPs. The monitors' calls to the status endpoint and to the monitor-only paths are not counted, so a dashboard does not inflate what it shows |
| `requests_total` | requests since start or since `Reset`, with the same exclusions |
| `blocked_requests_total` | requests answered `403` because the IP was blocked or got blocked by that request |
| `blocked_ips` | the blocks active now; `blocked_ips_count` is their number. `expires_at` is `null` for permanent blocks. An expired block is left out at once, although the guard removes it from memory and storage at the next cleanup (within a minute) |
| `rules[].detections` | matches since start or `Reset` |
| `rules[].blocks` | blocks the rule caused |
| `alerts` | the latest blocks, newest first, at most `MaxAlerts` (default 100) |

Timestamps are ISO 8601 in local time with the offset, as everywhere in the framework.

```bash
curl http://localhost:8080/api-guard/status
```

---

## Dashboard and monitor-only paths

`AddMonitorOnlyPath` reserves a path to the monitor IPs (prefix on a segment boundary,
case-insensitive). The guard lets those requests through (`ANext()`) only for monitors, and answers
anyone else `404 {"error":"Not Found"}`, including whitelisted IPs. That makes it possible to serve a
page with the static files filter, which has no IP filter of its own:

```delphi
lGuard.AddMonitorOnlyPath('/api-guard/ui');
AEngine.Use(APIGuardHTTPFilter(lGuard));           // before the static files
AEngine.Use(StaticFilesHTTPFilter('/api-guard/ui', TPath.Combine(AppPath, 'www')));
```

The sample ships such a page, `samples/filters_apiguard/bin/www/index.html`:

- one self-contained file, no CDN;
- tiles with the counters, a requests-per-minute chart, and the blocked IPs, rules and alerts tables;
- refresh every 2, 5 or 15 seconds, or paused; the next read starts only when the previous one has
  ended, an answer that arrives late never overwrites a newer one, and a failed read keeps the last
  good data on screen, marked as stale;
- a hint on every value (hover the dotted labels and column headers) that tells how it is computed;
- light and dark themes.

Copy it into your project's `www` folder, and change `STATUS_URL` in its script if you moved the
endpoint. Every value from the server goes into the page as text, never as HTML: alert paths are
chosen by the attacker.

You can reserve any other path the same way, for example the admin routes of
[Managing blocks from code](#managing-blocks-from-code): `lGuard.AddMonitorOnlyPath('/admin')`.

---

## Alerts

An alert is raised every time an IP is blocked, by a rule or by `BlockIP`. Each alert:

1. is kept in memory for the status endpoint (the last `MaxAlerts`, default 100);
2. is written to the log as a warning with the tag `apiguard`
   (`APIGuard: IP blocked: 5 occurrences in 300s [ip ...] [rule bruteforce] [path /api/login]`);
3. is queued for the `AuditLog`, if assigned, as a `block` event;
4. is passed to `OnAlert`, if assigned.

```delphi
lGuard.MaxAlerts := 500;
lGuard.OnAlert :=
  procedure(const AAlert: TMVCAPIGuardAlert)
  begin
    // AAlert.Timestamp, IP, Rule, Path, Message
    MyAlertQueue.Enqueue(AAlert);   // hand it over, do not send it from here
  end;
```

`OnAlert` is called **under the guard's lock**, on the request thread that caused the block. Keep it
short: push the alert to a queue and send the e-mail, Slack message or SIEM event from another
thread. An exception raised in `OnAlert` is logged and swallowed.

Detections that do not reach a threshold raise no alert. They show up only in `rules[].detections`.

### Audit log

The alerts in memory are only the latest ones and are lost on restart. To keep a history, assign an
audit log. Unlike the blacklist it is **appended to, never rewritten**. Three kinds of event are
recorded:

| Event | When | `Rule` | `Path` | `Message` |
|-------|------|--------|--------|-----------|
| `block` | an IP is blocked by a rule or by `BlockIP` (the same as an alert) | the rule that blocked it | the request that tripped the rule | as in the alert |
| `unblock` | `UnblockIP`, a block that expires, `Reset` | the rule of the block | empty | `unblocked manually`, `block expired`, `reset` |
| `denied` | every request refused with `403` because its IP is blocked | the rule of the block | the refused request | `request refused` |

An expiry is recorded with the expiry time, when the guard removes the entry (within a minute).

```delphi
IMVCAPIGuardAuditLog = interface
  procedure Append(const AEvents: TArray<TMVCAPIGuardAuditEvent>);
  procedure Purge(const ABefore: TDateTime);
end;
```

Requests only queue the events; the writer thread passes them to `Append` in batches, oldest first.
The queue holds at most 10 000 events: beyond that (an audit log that cannot keep up with a blocked
IP that keeps hammering) further events are dropped, and the number dropped is logged as an error.
If `Append` raises, the error is logged and that batch is lost; the blocks are not affected.

Text file, `TMVCAPIGuardFileAuditLog` (UTF-8, a header line when the file is created, then one line
per event with tab-separated fields; tabs and line breaks inside the fields, which may come from the
request path, are replaced with spaces):

```delphi
lGuard.AuditLog := TMVCAPIGuardFileAuditLog.Create(TPath.Combine(AppPath, 'apiguard_audit.txt'));
```

```
# timestamp	event	ip	rule	path	message
2026-10-29T18:00:07.853+01:00	block	203.0.113.7	scan	/wp-login.php	IP blocked: 20 occurrences in 60s
2026-10-29T18:00:08.102+01:00	denied	203.0.113.7	scan	/xmlrpc.php	request refused
2026-10-29T18:10:00.000+01:00	block	198.51.100.23	manual		IP blocked manually: abuse report #1234
2026-10-29T19:00:07.853+01:00	unblock	203.0.113.7	scan		block expired
```

Database, `TMVCAPIGuardFireDACAuditLog` (same rules for the connection definition and the table
name as `TMVCAPIGuardFireDACStorage`; each batch is inserted in one transaction, fields are truncated
to the column sizes):

```delphi
lGuard.AuditLog := TMVCAPIGuardFireDACAuditLog.Create('MyConnDef', 'dmvc_apiguard_audit');
```

```sql
CREATE TABLE dmvc_apiguard_audit (
  event_at   TIMESTAMP    NOT NULL,
  event_type VARCHAR(16)  NOT NULL,   -- block, unblock, denied
  ip         VARCHAR(64)  NOT NULL,
  rule_name  VARCHAR(64)  NOT NULL,
  path       VARCHAR(255),
  message    VARCHAR(255)
);
CREATE INDEX ix_dmvc_apiguard_audit_event_at ON dmvc_apiguard_audit (event_at);
```

The index serves the retention purge. Add an identity key if your database wants one; the guard only
inserts and purges. Several instances can share the table.

#### Retention

By default nothing is ever deleted. To keep only the last days:

```delphi
lGuard.AuditRetentionDays := 90;   // 0 (default) = keep forever
```

The writer thread calls `Purge` with `Now - AuditRetentionDays` on its first write and then at most
once an hour. The file version rewrites the file without the older lines (lines whose date cannot be
read are kept); the FireDAC version runs `DELETE ... WHERE event_at < :before`. A failed purge is
logged and tried again an hour later.

---

## Managing blocks from code

```delphi
lGuard.BlockIP('203.0.113.7', 'abuse report #1234');          // permanent
lGuard.BlockIP('203.0.113.8', 'cooling off', 3600);           // one hour
lGuard.UnblockIP('203.0.113.7');
if lGuard.IsBlocked('203.0.113.8') then ...
for lEntry in lGuard.BlockedIPs do ...                        // snapshot copy
lGuard.Reset;   // forgets counters, blocks, alerts and statistics, and stores the empty list
```

These methods take the guard's lock and are safe to call while the server runs, for example from an
admin route. Protect that route: put it under a monitor-only path, behind authentication, or both.
The sample does it with two Minimal API routes that capture the guard:

```delphi
procedure ConfigureRoutes(const ARoot: TMVCRouteGroup<TObject>; const AGuard: TMVCAPIGuard);
var
  lAdmin: TMVCRouteGroup<TObject>;
begin
  lAdmin := ARoot.Prefix('/admin/blocked');
  // POST /admin/blocked/203.0.113.7
  lAdmin.MapPost<string>('/($ip)',
    function(AIP: string): IMVCResponse
    begin
      AGuard.BlockIP(AIP, 'blocked from /admin', 3600);
      Result := Ok('Blocked ' + AIP);
    end);
  // DELETE /admin/blocked/203.0.113.7
  lAdmin.MapDelete<string>('/($ip)',
    function(AIP: string): IMVCResponse
    begin
      AGuard.UnblockIP(AIP);
      Result := Ok('Unblocked ' + AIP);
    end);
end;

// in ConfigureEngine
lGuard.AddMonitorOnlyPath('/admin');
AEngine.Use(APIGuardHTTPFilter(lGuard));
ConfigureRoutes(AEngine.Root, lGuard);
```

The routes capture the object reference, not an interface: the filter keeps the guard alive, and
the routes and the filter go together when the engine is freed. A controller does the same through a
reference kept when the engine was configured.

---

## Responses sent by the guard

| When | Status | Body |
|------|--------|------|
| The IP is blocked, or this request blocked it | `403 Forbidden` | `{"error":"Forbidden"}` |
| A monitor-only path called by a non-monitor IP | `404 Not Found` | `{"error":"Not Found"}` |
| The status endpoint, for a monitor IP | `200 OK` | the status JSON |

The `403` deliberately says nothing about the rule or the expiry, so as not to help an attacker tune
the attack. There is no `Retry-After` header.

A block caused by a **response rule** (scan, brute force) applies from the next request on: the
request that reached the threshold still gets its normal response, e.g. the 5th failed login still
gets `401`.

---

## Thread safety and configuration timing

All the guard's state (counters, blacklist, alerts, statistics) is protected by a single critical
section. The filter has no state of its own: it calls `HandleRequest` before `ANext()` and
`HandleResponse` after it. The request path holds it only for short dictionary operations; custom matchers run outside
it.

**Configuration is not synchronized.** Call `Rule(...)`, `Configure`, `OnPaths`, `OnStatusCodes`,
`Enabled`, `AddRule`, `AddSignature`, `AddBadUserAgent`, the `Clear*` methods, `AddWhitelistIP`,
`AddMonitorIP`, `AddMonitorOnlyPath`, `UseClientIp`, `MaxAlerts`, `OnAlert`, `AuditLog` and
`AuditRetentionDays` while configuring the
engine, **before the server starts listening**. Changing them while requests are served is a data
race.

`BlockIP`, `UnblockIP`, `IsBlocked`, `BlockedIPs` and `Reset` are safe at any time.

The blacklist storage and the audit log are written by one writer thread owned by the guard, outside
the lock. Your `IMVCAPIGuardStorage` and `IMVCAPIGuardAuditLog` implementations are therefore called
from that thread only (`Load` excepted: it runs in the constructor), one call at a time, and need no
locking of their own.

---

## Recipes

### Protect only the login

```delphi
lGuard.Rule(GUARD_RULE_SCAN).Enabled := False;
lGuard.Rule(GUARD_RULE_FLOOD).Enabled := False;
lGuard.Rule(GUARD_RULE_BRUTEFORCE).Configure(5, 900, 1800).OnPaths(['/auth/login']);
```

### A public API with real 404s

Clients that probe for resources that may not exist (`GET /orders/123`) produce honest 404s. Count
only unknown paths, not missing resources, by limiting `scan` to the paths no client should ever
request:

```delphi
lGuard.Rule(GUARD_RULE_SCAN).Enabled := False;
lGuard.AddRule('scan_outside_api', gpResponse, 15, 60, 3600,
  function(const AContext: TWebContext): Boolean
  begin
    Result := (AContext.Response.StatusCode = HTTP_STATUS.NotFound)
      and not AContext.Request.PathInfo.StartsWith('/api/');
  end);
```

### Permanent ban for attack tools and injection probes

```delphi
lGuard.Rule(GUARD_RULE_BAD_AGENT).BlockSeconds := 0;
lGuard.Rule(GUARD_RULE_INJECTION).Configure(1, 60, 0);
```

### Behind nginx

```nginx
location / {
    proxy_pass http://127.0.0.1:8080;
    proxy_set_header X-Forwarded-For $remote_addr;   # overwrite, do not append the client's value
}
location /api-guard/ { return 404; }                 # do not publish the monitor paths
```

```delphi
MVCTrustProxyForwardedHeaders := True;
lGuard.UseClientIp := True;
```

The monitor check always uses the socket peer, which here is nginx (`127.0.0.1`). That is why the
`location /api-guard/` block matters: without it, anyone on the internet would be a monitor.

### Tests that run from localhost

```delphi
[Setup]
procedure TMyTests.Setup;
begin
  fGuard.Reset;    // or: fGuard.AddWhitelistIP('127.0.0.1') once in SetupFixture
end;
```

### Simulating attacks

`samples/filters_apiguard/test_apiguard.py` (Python 3, standard library only) runs every scenario
against the sample. Each one comes from its own loopback address, `127.0.0.2` to `127.0.0.7`, and the
script binds the source address because Windows otherwise connects from `127.0.0.1`:

```
python test_apiguard.py all
python test_apiguard.py bruteforce --port 8080
```

---

## Limitations

- **Fixed windows.** A burst that straddles two windows can reach almost twice the threshold before
  it is blocked. This is fine for abuse detection; if you need exact limits, use
  `RateLimitHTTPFilter` with a sliding window or token bucket storage (`TMVCRateLimitOptions`)
  alongside the guard, registered after it.
- **One process, one state.** Counters, statistics and alerts live in the process memory. Several
  instances behind a load balancer each count only their own share of an attacker's traffic, and a
  shared storage is not kept in sync: each instance overwrites the others. Give every instance its
  own file or table, or put the guard where all traffic passes (a single gateway instance).
- **No CIDR ranges.** Monitor IPs, the whitelist and blocks are single addresses. To exclude a whole
  network from counting, use a custom rule that is never matched for it, or a reverse proxy rule.
- **The blacklist is loaded at startup.** Changes made to the file or the table while the server runs
  are not picked up, and they are overwritten at the next change. Use `BlockIP` and `UnblockIP` at
  runtime.
- **Rewrite on every change.** `Store` writes the whole list. It runs off the request path, but a
  copy of the list is taken under the lock: fine up to a few thousand entries. A mass attack from
  many thousands of IPs makes every write longer.
- **Pending writes are lost on a crash.** The writer flushes when the guard is destroyed; a process
  that is killed loses what was still queued (usually milliseconds of events).
- **Signatures are substrings.** They catch automated probes, not a targeted attack, and they can
  match legitimate input. They do not replace validation and parameterized queries.
- **Clock-based.** Expiries use the local clock (`Now`). Moving the clock moves the expiries.
- **An exception out of `ANext()` skips the response rules.** Actions and handlers never raise out
  of it (the engine turns their exceptions into responses), but a filter registered after the guard
  can: the engine then answers `500`, and that request is not counted by the response rules.

---

## API reference

### `APIGuardHTTPFilter`

| Member | Description |
|--------|-------------|
| `APIGuardHTTPFilter(AGuard: TMVCAPIGuard): TMVCHTTPFilter` | the filter of `AGuard`, which it keeps alive; register it first with `AEngine.Use`. Raises `EMVCConfigException` on `nil` |

### `TMVCAPIGuard`

| Member | Description |
|--------|-------------|
| `Create(AStorage: IMVCAPIGuardStorage = nil; AStatusPath = '/api-guard/status')` | `nil` storage = memory only; `''` path = no endpoint. Loads the blacklist |
| `Rule(AName): TMVCAPIGuardRule` | a built-in or custom rule; raises if unknown |
| `AddRule(AName, APhase, AThreshold, AWindowSeconds, ABlockSeconds, AMatcher = nil)` | adds a custom rule, owned by the guard; raises on a duplicate name |
| `AddSignature(s)` / `ClearSignatures` | substrings for `injection` |
| `AddBadUserAgent(s)` / `ClearBadUserAgents` | substrings for `bad_agent` |
| `AddWhitelistIP(ip)` | never counted, never blocked |
| `AddMonitorIP(ip)` | may read the status endpoint and monitor-only paths (default: `127.0.0.1`, `::1`) |
| `AddMonitorOnlyPath(path)` | reserves a path prefix to the monitor IPs; 404 for the others |
| `BlockIP(ip, reason, seconds = 0)` | blocks an IP (`0` = permanent), raises an alert, persists |
| `UnblockIP(ip)` | removes a block, persists |
| `IsBlocked(ip): Boolean` | currently blocked and not expired |
| `BlockedIPs: TArray<TMVCAPIGuardBlockedIP>` | copy of the active blocks (expired ones left out) |
| `Reset` | clears counters, blacklist, alerts, statistics; persists the empty list |
| `UseClientIp: Boolean` | use `ClientIp` instead of the socket peer; needs `MVCTrustProxyForwardedHeaders := True` to read forwarded headers. Default `False` |
| `MaxAlerts: Integer` | alerts kept for the status. Default 100 |
| `OnAlert: TMVCAPIGuardAlertProc` | called for every block, under the lock |
| `AuditLog: IMVCAPIGuardAuditLog` | history of blocks, unblocks and 403s, written by the writer thread; `nil` (default) = none |
| `AuditRetentionDays: Integer` | events older than this are purged hourly; `0` (default) = kept forever |
| `HandleRequest(AContext): Boolean` / `HandleResponse(AContext)` | the two halves the filter calls; not for application code |

### `TMVCAPIGuardRule`

| Member | Description |
|--------|-------------|
| `Configure(AThreshold, AWindowSeconds, ABlockSeconds): TMVCAPIGuardRule` | validated setter; block `0` = permanent |
| `OnPaths([...]): TMVCAPIGuardRule` | count only under these path prefixes; empty = everywhere |
| `OnStatusCodes([...]): TMVCAPIGuardRule` | statuses counted by a response rule without a matcher |
| `Name`, `Phase` | read-only |
| `Enabled`, `Threshold`, `WindowSeconds`, `BlockSeconds`, `Matcher` | read/write |

### Types and constants

| Name | Description |
|------|-------------|
| `TMVCAPIGuardPhase` | `gpRequest` (before `ANext()`), `gpResponse` (after it) |
| `TMVCAPIGuardMatcher` | `reference to function(const AContext: TWebContext): Boolean` |
| `TMVCAPIGuardBlockedIP` | `IP`, `Rule`, `Reason`, `BlockedAt`, `ExpiresAt` (0 = permanent), `IsExpired(ANow)` |
| `TMVCAPIGuardAlert` | `Timestamp`, `IP`, `Rule`, `Path`, `Message` |
| `TMVCAPIGuardAlertProc` | `reference to procedure(const AAlert: TMVCAPIGuardAlert)` |
| `IMVCAPIGuardStorage` | `Load`, `Store` — see [Blacklist storage](#blacklist-storage) |
| `TMVCAPIGuardFileStorage` | `Create(AFileName)` |
| `TMVCAPIGuardFireDACStorage` | `Create(AConnectionDefName, ATableName = 'dmvc_ip_blacklist')` |
| `TMVCAPIGuardAuditEvent` | `Timestamp`, `Event`, `IP`, `Rule`, `Path`, `Message` |
| `IMVCAPIGuardAuditLog` | `Append`, `Purge` — see [Audit log](#audit-log) |
| `TMVCAPIGuardFileAuditLog` | `Create(AFileName)` |
| `TMVCAPIGuardFireDACAuditLog` | `Create(AConnectionDefName, ATableName = 'dmvc_apiguard_audit')` |
| `APIGuardNormalizeIP(ip): string` | the canonical spelling of an address |
| `GUARD_RULE_SCAN`, `GUARD_RULE_BRUTEFORCE`, `GUARD_RULE_FLOOD`, `GUARD_RULE_INJECTION`, `GUARD_RULE_BAD_AGENT` | built-in rule names |
| `GUARD_RULE_MANUAL` | rule name of blocks made with `BlockIP` or written by hand |
| `GUARD_AUDIT_BLOCK`, `GUARD_AUDIT_UNBLOCK`, `GUARD_AUDIT_DENIED` | audit event kinds |
