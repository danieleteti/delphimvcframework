# CLAUDE.md

## Overview

DelphiMVCFramework - RESTful framework for Delphi (Object Pascal). REST services, JSON-RPC APIs, web apps. MVC architecture, ORM (MVCActiveRecord), auth (JWT, Basic), middleware, WebSocket, serialization.

**Language**: Object Pascal / Delphi
**Version**: `sources/dmvcframeworkbuildconsts.inc` (DMVCFRAMEWORK_VERSION) — currently `3.5.0-silicon-rc5`
**License**: Apache License 2.0

## Build Commands

```bash
# Tests — the matrix matters: the same suite runs against every host
python -m invoke tests                 # Win32+Win64, classic (WebBroker) server
python -m invoke tests32               # Win32 only
python -m invoke tests64               # Win64 only
python -m invoke tests-indydirect      # same suite, Indy Direct host
python -m invoke tests-httpsys         # same suite, HTTP.sys host
python -m invoke tests-isapi           # hosted by IIS Express ISAPI (Win64)
python -m invoke tests-apache          # hosted by Apache 2.4 module (Win64)
python -m invoke tests-all             # classic + Indy Direct + HTTP.sys
python -m invoke tests-all-hosts       # the full host matrix
python -m invoke --list                # everything else

build_tests.bat                        # Win32 unit tests (Delphi 13)

# Manual MSBuild
"C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat" && msbuild project.dproj /p:Config=DEBUG /p:Platform=Win32

# Packages & Samples
python -m invoke build-core
python -m invoke build-samples [--filter=name]

# Release & Utils
python -m invoke release [--skip-tests] [--skip-build]
python -m invoke generate-nullables
python -m invoke clean
```

**`invoke tests` returns exit code 0 even on BUILD FAIL** — read the output, do not trust the exit status.

## Project Structure

```
sources/              - Framework source (*.pas, *.inc)
packages/             - Packages by Delphi version (d100..d130)
lib/                  - Third-party (loggerpro, swagdoc, dmustache)
samples/              - 40+ sample applications
unittests/            - Tests (TestServer + TestClient)
tools/                - entitygenerator, certificatesgenerator
ideexpert/            - IDE wizard for project creation
  templates/          - TemplatePro .tpro templates
  tests/              - Wizard test suite
```

## Architecture

### The layering that matters

**Everything above the host is host-independent.** A controller, an action, an entity, a middleware does not
know what is listening on the socket. Only the `.dpr` differs between hosts. Keep host-specific code out of
`ConfigureEngine`.

### Server hosts (3.5)

Three backends, one controller stack, selected in the `.dpr` via `TMVCServerFactory` (`MVCFramework.Server.Factory`) returning `IMVCServer` (`.Server.Intf`):

| Host | Constructor | When |
|------|-------------|------|
| **Indy Direct** | `TMVCServerFactory.CreateIndyDirect(lEngine)` | **Default for new projects.** No WebModule, no WebBroker. |
| HTTP.sys | `TMVCServerFactory.CreateHttpSys(lEngine)` | Kernel-mode HTTP. Needs admin or a `netsh http add urlacl`. |
| WebBroker | `TMVCServerFactory.CreateWebBroker(AConfigAction, AEngineConfig)` or `TMVCEngine.CreateForWebBroker(AWebModule, AConfigProc)` | Deploying inside ISAPI / Apache / an existing WebBroker app. Supported indefinitely — **never mark WebBroker APIs deprecated.** |

`IMVCServer`: `Listen`, `Stop`, `IsRunning`, `RunAndWait`. HTTPS is configured **on `IMVCServer`** (certificate properties) — never set up TLS on the Indy component from the caller.
`MVCFramework.Signal` exports `WaitForTerminationSignal`; `EnterInShutdownState` / `IsShuttingDown` live in `MVCFramework`.

**`TMVCListener` is deprecated as of 3.5** (a thin wrapper over `IMVCServer`) and will be removed in 4.0. Do not write new code against it.

### Two routing styles, same engine

- **Controller-based**: attribute-routed methods on a `TMVCController`. Actions are **functions** returning data or `IMVCResponse` — never `procedure` + `Render(...)`.
- **Minimal API** (`MVCFramework.MinimalAPI`): lambda routes on a route group. `lEngine.Root` / `.Prefix(...)` → `TMVCRouteGroup<T>` (**a record — `Use`/`Prefix`/`AsWeb` return a new group; discarding the result is a silent no-op**), then `MapGet`/`MapPost`/`MapMethods`. Handlers are `function(...): IMVCResponse`, max 4 typed args, bound **by argument type**. Register classic middleware **before** the first `MapXxx` — the minimal dispatcher is installed lazily on the first `Map` and short-circuits matching routes.

### Key files

| Area | Files | Key Classes |
|------|-------|-------------|
| Core | `MVCFramework.pas`, `MVCFramework.Router.pas`, `MVCFramework.Commons.pas` | `TMVCEngine`, `TMVCController` |
| Hosting | `MVCFramework.Server.{Intf,Factory,Indy,HttpSys,WebBroker}.pas`, `.Signal.pas` | `IMVCServer`, `TMVCServerFactory` |
| Minimal API | `MVCFramework.MinimalAPI.pas` | `TMVCRouteGroup<T>`, `TMVCRouteHandle`, `TMVCFormFile` |
| Filters | `MVCFramework.Filters.pas` (+ `.Filters.Redis.pas`) | `TMVCEndpointFilter` (per-group), `TMVCHTTPFilter` (engine-wide) |
| ORM | `MVCFramework.ActiveRecord.pas`, `MVCFramework.ActiveRecordController.pas`, `MVCFramework.Repository.pas` | `TMVCActiveRecord`, `IMVCRepository<T>` |
| Middleware | `Middleware.{JWT,CORS,Compression,StaticFiles,RateLimit,ETag,Swagger,Analytics,Trace,ActiveRecord}.pas` | `IMVCMiddleware` |
| Serialization | `Serializer.JsonDataObjects.pas` | JSON with naming conventions |
| SSE | `MVCFramework.SSE.pas`, `MVCFramework.SSEController.pas` | `TMVCSSEController`, `SSEBroker` |
| WebSocket | `WebSocket.{Server,Client}.pas` | `TMVCWebSocketServer` |
| Auth | `MVCFramework.{JWT,JWT.RSA,HMAC}.pas` | `TJWT` |
| Config | `MVCFramework.DotEnv.pas` | `NewDotEnv`, `dotEnv`, `dotEnvConfigure` |
| Other | `RESTClient.pas`, `Cache.pas`, `RQL.Parser.pas`, `Nullables.pas` | |

**Filters vs middleware**: `MVCFramework.Filters` is the modern surface — `TMVCEndpointFilter` attaches to a route group and fires only on a match; `TMVCHTTPFilter` is engine-wide and wraps routing itself. 18/19 classic middleware helpers have a filter equivalent; only OIDC is still middleware-only.

**Ownership**: the framework **frees the object returned by a functional action** (`MVCFramework.pas`, the `tkClass` branch of the dispatcher). `ToFree<T>` is only for intermediates you allocate and do **not** return — `Result := ToFree(x)` is a double free. `ViewData` owns nothing.

Middleware executes in the order added via `AddMiddleware()`. Controllers are stateless (created per request).

## Delphi Versions

`d100`=10 Seattle, `d101`=10.1 Berlin, `d102`=10.2 Tokyo, `d103`=10.3 Rio, `d104`=10.4 Sydney, `d110`=11.x Alexandria, `d120`=12 Athens, `d130`=13 Florence.

**Oldest supported: 10.2 Tokyo** (`d102`). The `d100` and `d101` package folders still exist but are below the supported baseline — do not assume the source compiles there.

Each version folder: `dmvcframeworkRT.dpk` (runtime), `dmvcframeworkDT.dpk` (design-time + IDE expert), `loggerproRT.dpk`, `SwagDoc.dpk`.

**The framework source MUST compile on the oldest supported Delphi — currently 10.2 Tokyo.**

Concretely, in `sources/`:
- **No inline variable declarations** (`var x := ...`, `var x: T;` inside a `begin..end`) and **no `for var`** — both require 10.3 Rio. Declare in the routine's `var` section.
- No `NameOf`, no inline `if` expressions (13 Florence).
- `class var` inside a class declaration is fine — it is not an inline declaration.

Generated code, samples and the wizard scaffold may target newer versions (they assume 11 Alexandria).

Check `sources/` before committing:

```bash
grep -rnE "^\s+var [a-zA-Z_]+ :=|for var " sources/*.pas
```

## Backward compatibility

**A 3.4.x server upgraded to 3.5 must notice no API change.** New features are additive and opt-in. If a feature is wired into the wizard scaffold, it must not change the generated output for existing project types. To retire behaviour, change the feature rather than removing the API.

## Testing

TestServer (background HTTP server) + TestClient (requests + assertions), DUnitX. Win32/Win64.
- `unittests/general/TestServer/bin/` - server executable
- `unittests/general/TestClient/bin32/` and `bin64/` - test executables

The same suite is run against every host (see the test matrix above) — a change to the engine or the router must be green on all of them, not just the classic one.

## Dependencies

Indy (HTTP/TCP/WebSocket), FireDAC (DB for ActiveRecord), JsonDataObjects (JSON), LoggerPro (logging), SwagDoc (OpenAPI).

**JSON is always JsonDataObjects.** Never `System.JSON`. Parse a request body with `StrToJSONObject(Context.Request.Body)`.

## File Naming

- `MVCFramework.*.pas` - framework units
- `MVCFramework.Middleware.*.pas` - middleware
- `MVCFramework.Serializer.*.pas` - serializers
- `MVCFramework.ActiveRecord*.pas` - ORM

## Key Attributes

`MVCPath`, `MVCHTTPMethod`, `MVCProduces`/`MVCConsumes` (routing), `MVCTable`/`MVCTableField`/`MVCPrimaryKey` (ORM), `MVCNameAs`/`MVCNameCase` (serialization, default `ncLowerCase`), `MVCSerialize`/`MVCDoNotSerialize`/`MVCDoNotDeserialize`.

`[MVCRequiresRole]` is **not** in `MVCFramework` — it lives in `MVCFramework.Middleware.Authentication.RoleBasedAuthHandler` and is enforced only when that handler is installed. Without it, it compiles and does nothing.

---

## IDE Expert - Project Presets

8 presets in `ideexpert/DMVC.Expert.Presets.pas` (`TDMVCProjectPreset`), in enum order:

| Preset | Shape |
|--------|-------|
| **RESTful API** | REST controller + CRUD + CORS/compression |
| **Minimal API RESTful** | Lambda routes, no controller class, per-group endpoint filters |
| **Web Application** | TemplatePro/HTMX + static files + session |
| **Minimal API WebApp** | Lambda routes via `.AsWeb`, `RenderView`/`ViewData`, MemorySession + RequireLogin filters |
| **JSON-RPC Service** | JSON-RPC 2.0 endpoint + MVCDoc |
| **Real-Time Application** | WebSocket server + session + static files |
| **Full-Stack Application** | REST + TemplatePro + WebSocket + HTMX + JWT + ActiveRecord + ETag + RateLimit + Analytics + HTTPS |
| **Custom Project** | Full wizard, every option visible |

Each preset pre-populates the same wizard form with different defaults; the user accepts them (OK) or customizes. The two Minimal API presets additionally run the form in minimal-API mode (`TfrmDMVCNewProject.SetMinimalAPIMode`).

**Default server backend in every preset: Indy Direct** (not WebBroker).

Tutorial projects showing the Minimal API scaffolds live in `samples/wizard_showcase/{rest,web}/`.

## IDE Expert - Template System (TemplatePro)

Code gen uses TemplatePro `.tpro` templates (migration complete, no legacy files).

**Key files:**
- `DMVC.Expert.ProjectGenerator.pas` - Main generator
- `DMVC.Expert.CodeGen.TemplateEngine.pas` - Template engine wrapper
- `DMVC.Expert.Presets.pas` - Preset configs per project type
- `DMVC.Expert.Templates.rc` / `.res` - Embedded resources

**Template loading (dual-mode):**
1. External files: `C:\Users\Public\Documents\delphimvcframework_wizard_templates\` (priority)
2. Embedded resources in BPL (fallback)

Templates with `_` prefix (e.g. `_license_header.tpro`) always use embedded version.

**Config Keys -> Template Variables:** JSON keys with dots become underscores (`webmodule.middleware.session.memory` -> `webmodule_middleware_session_memory`).

**TemplatePro syntax:**
- Variabili: `{{:var}}`, `{{:obj.prop}}` — HTML-escaped by default; the `$` suffix (`{{:x$}}`) emits raw HTML
- Loop index: `{{:item.@@index}}` (a pseudo-property of the loop variable, 1-based)
- Condizionali: `{{if cond}}...{{elseif}}...{{else}}...{{endif}}`, negation `{{if !x}}`
- Loop: `{{for item in list}}...{{endfor}}`
- Include: `{{include "file.tpro"}}` (view-root relative), inheritance: `{{extends}}` (file-relative) + `{{block}}`
- Filtri: `eq`, `ne`, `gt`, `ge`, `lt`, `le`, `contains`, plus DMVC's own `json`, `urlencode`, `count`, `fromquery`
- Ref: https://www.danieleteti.it/templatepro/

Generated views use **Bootstrap 5.3** (CDN, in `baselayout`) + a slim `style.css`; dark mode via `data-bs-theme`.

**Generated code is code-first**: short markers and one-liners only where the *why* is not obvious. Do not drown it in explanatory comments. The showcase tutorials in `samples/wizard_showcase/` are the exception — those are didactic.

**Build commands:**
```bash
# Build wizard BPL (requires the IDE to be closed)
C:\DEV\dmvcframework\ideexpert\build_wizard2.bat

# Test templates
cd C:\DEV\dmvcframework\ideexpert\tests && build_and_run.bat
```

**`.res` files are produced by the IDE build from `.rc`** — never run `brcc32` by hand.
