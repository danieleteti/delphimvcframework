# CLAUDE.md

## Overview

DelphiMVCFramework - RESTful framework for Delphi (Object Pascal). REST services, JSON-RPC APIs, web apps. MVC architecture, ORM (MVCActiveRecord), auth (JWT, Basic), middleware, WebSocket, serialization.

**Language**: Object Pascal / Delphi
**Version**: `sources/dmvcframeworkbuildconsts.inc` (DMVCFRAMEWORK_VERSION)
**License**: Apache License 2.0

## Build Commands

```bash
# Tests
build_tests.bat                          # Win32 unit tests (Delphi 13)
python -m invoke tests32                 # Build+run Win32
python -m invoke tests64                 # Build+run Win64
python -m invoke tests                   # Both

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

## Architecture (Key Files)

| Area | Files | Key Classes |
|------|-------|-------------|
| Core | `MVCFramework.pas`, `MVCFramework.Router.pas`, `MVCFramework.Commons.pas` | `TMVCEngine`, `TMVCController` |
| ORM | `MVCFramework.ActiveRecord.pas`, `MVCFramework.ActiveRecordController.pas` | `TMVCActiveRecord` |
| Middleware | `Middleware.{JWT,CORS,Compression,StaticFiles,RateLimit,ETag,Swagger,Analytics,ActiveRecord}.pas` | `IMVCMiddleware` |
| Serialization | `Serializer.JsonDataObjects.pas` | JSON with naming conventions |
| WebSocket | `WebSocket.{Server,Client,pas}` | `TMVCWebSocketServer` |
| Auth | `MVCFramework.{JWT,JWT.RSA,HMAC}.pas` | `TJWT` |
| Other | `RESTClient.pas`, `DotEnv.pas`, `Cache.pas`, `RQL.Parser.pas`, `Nullables.pas` | |

Middleware executes in order added via `AddMiddleware()`. Controllers stateless (created per request).

## Delphi Versions

`d100`=10 Seattle, `d101`=10.1 Berlin, `d102`=10.2 Tokyo, `d103`=10.3 Rio, `d104`=10.4 Sydney, `d110`=11.x Alexandria, `d113`=11.3, `d120`=12 Athens, `d130`=13 Florence.

Each version folder: `dmvcframeworkRT.dpk` (runtime), `dmvcframeworkDT.dpk` (design-time + IDE expert), `loggerproRT.dpk`, `SwagDoc.dpk`.

The whole codebase *MUST* be compatible with Delphi version 10 Seattle or better. Do not use in-line declarations and other syntax added after Delphi 10 Seattle.

## Testing

TestServer (background HTTP server) + TestClient (requests + assertions). Win32/Win64.
- `unittests/general/TestServer/bin/` - server executable
- `unittests/general/TestClient/bin32/` and `bin64/` - test executables

## Dependencies

Indy (HTTP/TCP/WebSocket), FireDAC (DB for ActiveRecord), JsonDataObjects (JSON), LoggerPro (logging), SwagDoc (OpenAPI).

## File Naming

- `MVCFramework.*.pas` - framework units
- `MVCFramework.Middleware.*.pas` - middleware
- `MVCFramework.Serializer.*.pas` - serializers
- `MVCFramework.ActiveRecord*.pas` - ORM

## Key Attributes

`MVCPath`, `MVCHTTPMethod`, `MVCProduces`/`MVCConsumes` (routing), `MVCTable`/`MVCTableField`/`MVCPrimaryKey` (ORM), `MVCNameAs`/`MVCNameCase` (serialization), `MVCSerialize`/`MVCDoNotSerialize`.

---

## IDE Expert - Project Types

5 project types in Object Repository (`File > New > Other > DelphiMVCFramework`):

| Type | Controller | Entity | SSV | WebSocket | JSON-RPC | Middleware |
|------|-----------|--------|-----|-----------|----------|------------|
| **REST API** | CRUD | Yes | No | No | No | CORS, Compression, JWT, ActiveRecord |
| **Full Stack MVC** | Index+CRUD | Yes | Yes (3 engines) | No | No | StaticFiles, Compression, Session(Memory), ActiveRecord |
| **WebSocket Server** | Optional | No | No | Yes | No | Analytics, Trace |
| **JSON-RPC** | No | Optional | No | No | Yes | Compression, JWT |
| **Microservice** | Custom only | Optional | No | No | Optional | CORS, JWT, RateLimit |

**Wizard**: Configurable with smart defaults per type. Each type = preset populating same wizard form. User accept defaults (OK) or customize.

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

**Templates (18):** `_license_header`, `program.dpr`, `program_service.dpr`, `service.pas`, `service.dfm`, `controller.pas`, `entity.pas`, `webmodule.pas`, `webmodule.dfm`, `jsonrpc.pas`, `services.pas`, `authentication.pas`, `helpers_{mustache,templatepro,webstencils}.pas`, `websocketserver.pas`, `project.dproj`, `project.rc`, `views/index_complete_view`.

**Config Keys -> Template Variables:** JSON keys with dots become underscores (`webmodule.middleware.session.memory` -> `webmodule_middleware_session_memory`).

**TemplatePro syntax:**
- Variabili: `{{:var}}`, `{{:obj.prop}}`
- Condizionali: `{{if cond}}...{{elseif}}...{{else}}...{{endif}}`
- Loop: `{{for item in list}}...{{endfor}}`
- Include: `{{include "file.tpro"}}`
- Filtri: `eq`, `ne`, `gt`, `ge`, `lt`, `le`, `contains`
- Ref: https://www.danieleteti.it/templatepro/

**Build commands:**
```bash
# Build wizard BPL (richiede IDE chiuso)
C:\DEV\dmvcframework\ideexpert\build_wizard2.bat

# Test templates
cd C:\DEV\dmvcframework\ideexpert\tests && build_and_run.bat
```
