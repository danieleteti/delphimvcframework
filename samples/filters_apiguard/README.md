# APIGuard HTTP Filter Sample

Contributed by Giordano Giaccaglia, [KD Software S.r.l.s.](https://www.kdsoftware.it) ([info@kdsoftware.it](mailto:info@kdsoftware.it)).

`APIGuardHTTPFilter` (`MVCFramework.Filters.APIGuard`) watches every request and response per client IP
and **blocks the IP** when a rule trips. Blocked IPs get `403 {"error":"Forbidden"}` until the block
expires (or forever, with a block time of 0).

This sample puts the guard in front of a **Minimal API** (no controller class) and of the static files of
its dashboard, both registered as filters:

```delphi
lGuard := CreateGuard;                                       // TMVCAPIGuard, rules tuned
lEngine.Use(APIGuardHTTPFilter(lGuard));                     // FIRST: the outermost filter
lEngine.Use(StaticFilesHTTPFilter('/api-guard/ui', TPath.Combine(AppPath, 'www')));
ConfigureRoutes(lEngine.Root, lGuard);                       // /api/hello, /api/login, /admin/blocked/($ip)
```

The filter keeps the guard alive and the engine releases it: do not free `lGuard`. The `/admin` routes use
the same object to block and unblock at run time.

## Rules

| Rule | Counts | Built-in default | This sample |
|------|--------|------------------|-------------|
| `scan` | 404/405 responses | 20 in 60s, block 1h | 10 in 60s |
| `bruteforce` | 401/403 responses | 10 in 300s, block 1h | 5 in 300s, only on `/api/login` |
| `flood` | every request | 600 in 60s, block 10min | 100 in 10s |
| `injection` | attack signatures in path/query, also double URL-decoded | 3 in 300s, block 24h | 1 in 60s, plus `/actuator/` |
| `bad_agent` | User-Agent of attack tools (sqlmap, nikto, nmap, nuclei...) | 1 in 60s, block 24h | 1 in 60s |
| `honeypot` | *custom rule of this sample*: `/backup.zip`, `/admin.php`, `/config.json` | - | 1 in 60s |

Every block in the sample lasts 60 seconds, so the demo can be replayed.

## Endpoints

| Path | Who | What |
|------|-----|------|
| `GET /api/hello` | anyone | normal traffic |
| `POST /api/login` | anyone | `{"username":"admin","password":"secret"}`; a wrong password is a 401 that `bruteforce` counts |
| `GET /api-guard/status` | monitor IPs (localhost) | JSON status: counters, blocked IPs, rules, alerts |
| `GET /api-guard/ui/` | monitor IPs | the dashboard (`bin/www/index.html`), 404 for anyone else |
| `POST /admin/blocked/{ip}` | monitor IPs | blocks an IP for 60 s |
| `DELETE /admin/blocked/{ip}` | monitor IPs | unblocks an IP |

`/admin` and `/api-guard/ui` are reserved with `AddMonitorOnlyPath`: anyone else gets a 404, which `scan`
counts like any other.

## Storage

The blacklist is kept in `bin/blacklist.txt` (`TMVCAPIGuardFileStorage`) and the history of blocks,
unblocks and refused requests in `bin/apiguard_audit.txt` (`TMVCAPIGuardFileAuditLog`). For a database,
`TMVCAPIGuardFireDACStorage` and `TMVCAPIGuardFireDACAuditLog` in `MVCFramework.Filters.APIGuard.FireDAC`
(the `CREATE TABLE`s are in the unit header).

## Run it

1. Build and run `APIGuardFilterSample` (Indy Direct, port 8080).
2. In another console: `python test_apiguard.py all` (Python 3, standard library only)
   or one of `scan`, `bruteforce`, `flood`, `injection`, `agent`, `honeypot`, `status`; `--port` for another port.
3. Open `http://localhost:8080/api-guard/ui/`, then unblock one of the attackers:
   `curl -X DELETE http://localhost:8080/admin/blocked/127.0.0.3`.

The script sends each attack from its own loopback address (`127.0.0.2` ... `127.0.0.7`) so that one block
does not stop the next scenario, then prints the status endpoint read from `127.0.0.1`.

Full documentation: [`docs/apiguard_filter.md`](../../docs/apiguard_filter.md).
