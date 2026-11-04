# APIGuard Middleware Sample

Contributed by Giordano Giaccaglia, [KD Software S.r.l.s.](https://www.kdsoftware.it) ([info@kdsoftware.it](mailto:info@kdsoftware.it)).

`TMVCAPIGuardMiddleware` (`MVCFramework.Middleware.APIGuard`) watches every request and response per
client IP and **blocks the IP** when a rule trips. Blocked IPs get `403 {"error":"Forbidden"}` until the
block expires (or forever, with a block time of 0).

## Rules

| Rule | Counts | Built-in default | This sample |
|------|--------|------------------|-------------|
| `scan` | 404/405 responses | 20 in 60s, block 1h | 10 in 60s |
| `bruteforce` | 401/403 responses | 10 in 300s, block 1h | 5 in 300s, only on `/api/login` |
| `flood` | every request | 600 in 60s, block 10min | 100 in 10s |
| `injection` | attack signatures in path/query (traversal, SQLi, XSS, `${jndi:`, `/.env`, `/wp-admin`...), also double URL-decoded | 3 in 300s, block 24h | 1 in 60s, plus `/actuator/` |
| `bad_agent` | User-Agent of attack tools (sqlmap, nikto, nmap, nuclei...) | 1 in 60s, block 24h | 1 in 60s |
| `honeypot` | *custom rule of this sample*: `/backup.zip`, `/admin.php`, `/config.json` | - | 1 in 60s |

Every block in the sample lasts 60 seconds, so the demo can be replayed.

Each rule is tuned on its own:

```delphi
lGuard.Rule(GUARD_RULE_BRUTEFORCE).Configure(5, 300, 3600).OnPaths(['/api/login']);
lGuard.Rule(GUARD_RULE_SCAN).Enabled := False;
lGuard.AddRule('honeypot', gpRequest, 1, 60, 3600,
  function(const AContext: TWebContext): Boolean
  begin
    Result := AContext.Request.PathInfo = '/backup.zip';
  end);
```

`gpRequest` rules look at the request before routing; `gpResponse` rules run after it and can test
`AContext.Response.StatusCode` (or, without a matcher, the codes given to `OnStatusCodes`).

## Blacklist storage

- **Text file** - `TMVCAPIGuardFileStorage.Create('blacklist.txt')`, used here (`bin/blacklist.txt`).
  Tab separated, one IP per line; a line with only an IP is a permanent block, so it can be edited by hand.
- **Database** - `TMVCAPIGuardFireDACStorage.Create('MyConnDef')` in `MVCFramework.Middleware.APIGuard.FireDAC`.
  The `CREATE TABLE` is in the unit header.
- **Memory only** - `TMVCAPIGuardMiddleware.Create` with no storage.

## Monitoring endpoint

`GET /api-guard/status` answers only the monitor IPs (loopback by default, more with `AddMonitorIP`),
even while their own IP is blocked. Anyone else gets a plain 404. It reports:

- `requests_last_minute`, `requests_total`, `blocked_requests_total`
- `blocked_ips`: ip, rule, reason, blocked_at, expires_at (`null` = permanent)
- `rules`: configuration plus `detections` and `blocks` per rule
- `alerts`: the latest blocks, newest first (`MaxAlerts`, default 100)

Every block also goes to the log and to `OnAlert`, which this sample prints on the console.

## Dashboard

`http://localhost:8080/api-guard/ui/` is a static page (`bin/www/index.html`, served by
`TMVCStaticFilesMiddleware`) that polls the status endpoint every few seconds: tiles with the counters,
a requests/minute chart, blocked IPs, rules and alerts. It is reserved to the monitor IPs like the
endpoint itself:

```delphi
lGuard.AddMonitorOnlyPath('/api-guard/ui');            // 404 for anyone else
AEngine.AddMiddleware(lGuard);                          // first
AEngine.AddMiddleware(TMVCStaticFilesMiddleware.Create('/api-guard/ui', TPath.Combine(AppPath, 'www')));
```

## Which IP

The guard keys on the **socket peer** (`PeerIp`). Behind a reverse proxy that sets
`X-Forwarded-For`, set `UseClientIp := True` and `MVCTrustProxyForwardedHeaders := True`; without such a
proxy that header is written by the client, who could then dodge blocks or get somebody else blocked.
The monitor check always uses the socket peer.

Full documentation: [`docs/apiguard_middleware.md`](../../docs/apiguard_middleware.md).

## Run it

1. Build and run `APIGuardSample` (Indy Direct, port 8080).
2. In another console: `python test_apiguard.py all` (Python 3, standard library only)
   or one of `scan`, `bruteforce`, `flood`, `injection`, `agent`, `honeypot`, `status`; `--port` for another port.

The script sends each attack from its own loopback address (`127.0.0.2` ... `127.0.0.7`) so that one block
does not stop the next scenario, then prints the status endpoint read from `127.0.0.1`.

Add the guard as the **first** middleware: one that handles a request before it would hide the request
from the guard.
