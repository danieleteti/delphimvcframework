# DMVCFramework — Configuration with dotEnv

**All configuration lives in `.env`, never in the source.** Ports, connection strings, secrets, view paths,
log destinations: the wizard reads them all through `dotEnv`. Hard-coding any of them is a defect — and for
a secret, a security one (see the `dmvcframework-security` skill).

---

## 1. Setup — the `Boot` pattern

The global accessor is a **function**, `dotEnv`, exported from `MVCFramework.Commons`. It returns a lazily
created singleton. You configure it once, at startup, with `dotEnvConfigure`:

```delphi
// BootConfigU.pas — this is what the wizard generates
procedure ConfigDotEnv;
begin
  dotEnvConfigure(
    function: IMVCDotEnv
    begin
      Result := NewDotEnv
                 .UseStrategy(TMVCDotEnvPriority.FileThenEnv)
                 .UseProfile('test')   // loads .env.test if present
                 .UseProfile('prod')   // loads .env.prod if present
                 .Build(AppPath);      // the executable's folder
    end);
end;

procedure Boot;
begin
  ConfigDotEnv;    // FIRST — before the logger, the profiler and the server
  ConfigLogger;
  ConfigProfiler;
end;
```

**Order is not negotiable.** The first call to `dotEnv` freezes the singleton; a `dotEnvConfigure` after that
raises `EMVCDotEnv('dotEnv already initialized')`. `Boot` runs before anything reads configuration:

```delphi
// Project.dpr
begin
  Boot;
  RunServer(dotEnv.Env('dmvc.server.port', 8080));
end.
```

`TMVCEngine` copies the singleton into `TMVCConfig.dotEnv`, so the whole framework sees the same instance.

> The wizard deliberately omits `.UseLogger(...)` here: dotEnv's fallback logger calls `LogI`, which would
> create a default logger *before* `ConfigLogger` installs the real one.

---

## 2. Reading values

```delphi
function Env(const Name: string): string;
function Env(const Name: string; const DefaultValue: String): string;
function Env(const Name: string; const DefaultValue: Integer): Integer;
function Env(const Name: string; const DefaultValue: Boolean): Boolean;
function Env(const Name: string; const DefaultValue: Double): Double;
procedure RequireKeys(const Keys: TArray<String>);
procedure Rebuild;
function SaveToFile(const FileName: String): IMVCDotEnv;
function ToArray: TArray<String>;
```

- A **missing key returns `''`** — no exception. The default overloads substitute the default only when the
  resolved value is empty.
- A **malformed value raises**: a non-numeric `Integer`, a non-boolean `Boolean` and a bad `Double` all raise
  `EMVCDotEnv`. They do **not** fall back to the default.
- Booleans accept `yes/1/true` and `no/0/false`, case-insensitively.
- Doubles parse with a **dot** decimal separator (`en-US`), whatever the machine locale.
- `ToArray` / `SaveToFile` dump the **file dictionary only** — never the OS environment.

**Fail fast at boot** rather than starting a server with half a configuration:

```delphi
dotEnv.RequireKeys(['dmvc.server.port', 'firedac.connection_definition_name', 'JWT_SECRET']);
// raises EMVCDotEnv('Required keys not found: ...') listing every one that is missing or empty
```

---

## 3. Precedence — `TMVCDotEnvPriority`

```delphi
TMVCDotEnvPriority = (FileThenEnv, EnvThenFile, OnlyFile, OnlyEnv);
```

| Strategy | Lookup order |
|----------|--------------|
| `FileThenEnv` | `.env` files first; fall back to the OS environment. **What the wizard uses.** |
| `EnvThenFile` | OS environment first; fall back to the files. The library default if you do not choose. |
| `OnlyFile` | Files only; the OS environment is never consulted. |
| `OnlyEnv` | OS environment only. |

`EnvThenFile` is the one to pick when a container/CI must be able to override a committed default.
`FileThenEnv` is the one to pick when the file is the source of truth.

---

## 4. Profiles

`Build` loads, in this order — **later files overwrite earlier keys**:

1. `<path>\.env` — unless you called `SkipDefaultEnv`
2. `<path>\.env.<profile>` for each `UseProfile(...)`, **in registration order**

So `.UseProfile('test').UseProfile('prod')` means prod wins over test, and both win over `.env`.
A profile file that does not exist is **silently skipped** — a typo in a profile name is invisible unless you
attach `.UseLogger(...)`.

`Build(ADir)` resolves relative to the **executable's folder**, not the working directory:
`Build()` → `<exedir>\.env`; `Build('env1')` → `<exedir>\env1\.env*`. Deploy `.env` next to the exe.

Pick the profile at runtime with the delegate overload:

```delphi
.UseProfile(function: string begin Result := GetEnvironmentVariable('APP_PROFILE') end)
```

---

## 5. `.env` syntax

```bash
# comments start with # or ;
dmvc.server.port=8080                  # keys may contain dots
name="a quoted value"                  # single or double quotes; may span multiple lines
password="XYZ${USERNAME}!"             # ${KEY} expands from another key OR the OS environment
home=${__home__}                       # built-ins: __os__, __home__, __dmvc.version__
```

**`$[...]` evaluates an expression**, and can reference keys defined *earlier in the file*:

```bash
PORT=8000
DATABASE_PORT=$[PORT + 1000]                        # 9000
DEBUG_LEVEL=$[IF PORT > 8000 THEN 3 ELSE 1]
FULL_NAME=$[APP_NAME + "_v" + ToString(VERSION)]
FULL_API_URL=${HOST}:$[API_PORT]/api/v$[VERSION]
```

Operators: `+ - * /`, `MOD`, `DIV`, comparisons, `LIKE`, `AND`/`OR`/`NOT`, `IF … THEN … ELSE …`.
Functions: `sqrt, logn, log, round, abs, floor, ceil, power, contains, ToString, ToInteger, ToFloat, Min,
Max, Sort, Length, Upper, Lower, Trim, Left, Right, Substr, IndexOf, Replace, Now, Today, Year, Month, Day,
FormatDate, ParseDate, DateAdd, DateDiff`.

---

## 6. Keys the wizard projects use

The framework hard-codes none of these — the generated `.dpr` maps them onto `TMVCConfigKey`. These are the
conventional names; keep them.

| Area | Keys |
|------|------|
| Server | `dmvc.server.port`, `dmvc.default.content_type`, `dmvc.default.content_charset`, `dmvc.allow_unhandled_actions`, `dmvc.expose_server_signature`, `dmvc.max_request_size`, `dmvc.indy.keep_alive`, `dmvc.indy.listen_queue`, `dmvc.handler.max_connections` |
| Views | `dmvc.view_path`, `dmvc.default.view_file_extension`, `dmvc.view_cache` |
| Data | `dmvc.max_entities_record_count`, `firedac.connection_definition_name`, `firedac.connection_definitions_filename` |
| HTTPS | `https.enabled`, `https.cert.privkey`, `https.cert.cacert`, `https.cert.password` |
| Profiler | `dmvc.profiler.enabled`, `dmvc.profiler.warning_threshold`, `dmvc.profiler.logs_only_over_threshold` |
| Logging | `logger.config.file`, `logger.file.{folder,basename,max_kb,max_backups}`, `logger.html.*`, `logger.syslog.*` |
| Sqids | `dmvc.sqids.alphabet`, `dmvc.sqids.min_length` |

Application keys are yours to name (`JWT_SECRET`, `JWT_ISSUER`, `DB_*`, `BASE_URL`, …).

---

## 7. Gotchas

- **Keys are case-sensitive.** `Env('MODE')` will not find `mode=dev`. (Names *inside* `$[...]` expressions
  are case-insensitive — a different dictionary.)
- **A missing file is silent.** Only `RequireKeys` or a `UseLogger` will tell you.
- **Never commit secrets.** `.gitignore` the `.env`, commit a `.env.sample` with empty values, and let the
  deployment supply the real ones (or use `EnvThenFile` so the environment overrides).
- **Never fall back to a default secret.** `dotEnv.Env('JWT_SECRET', 'change-me')` ships a signing key that
  everyone who reads your repository already has. Put it in `RequireKeys` and let the server refuse to start.
- `Build` twice on the same builder raises; `Env` before `Build` raises. The builder is single-use.
