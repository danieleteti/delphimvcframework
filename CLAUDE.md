# CLAUDE.md

## Overview

DelphiMVCFramework is a RESTful framework for Delphi (Object Pascal) supporting REST services, JSON-RPC APIs, and web applications. MVC architecture with ORM (MVCActiveRecord), authentication (JWT, Basic Auth), middleware system, WebSocket support, and serialization.

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
| Core | `MVCFramework.pas`, `Router.pas`, `Commons.pas` | `TMVCEngine`, `TMVCController` |
| ORM | `ActiveRecord.pas`, `ActiveRecordController.pas` | `TMVCActiveRecord` |
| Middleware | `Middleware.{JWT,CORS,Compression,StaticFiles,RateLimit,ETag,Swagger,Analytics,ActiveRecord}.pas` | `IMVCMiddleware` |
| Serialization | `Serializer.JsonDataObjects.pas` | JSON with naming conventions |
| WebSocket | `WebSocket.{Server,Client,pas}` | `TMVCWebSocketServer` |
| Auth | `JWT.pas`, `HMAC.pas` | `TJWT` |
| Other | `RESTClient.pas`, `DotEnv.pas`, `Cache.pas`, `RQL.Parser.pas`, `Nullables.pas` | |

Middleware executes in order added via `AddMiddleware()`. Controllers are stateless (created per request).

## Delphi Versions

`d100`=10 Seattle, `d101`=10.1 Berlin, `d102`=10.2 Tokyo, `d103`=10.3 Rio, `d104`=10.4 Sydney, `d110`=11.x Alexandria, `d113`=11.3, `d120`=12 Athens, `d130`=13 Florence.

Each version folder: `dmvcframeworkRT.dpk` (runtime), `dmvcframeworkDT.dpk` (design-time + IDE expert), `loggerproRT.dpk`, `SwagDoc.dpk`.

## Testing

TestServer (background HTTP server) + TestClient (requests + assertions). Both Win32/Win64.
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

**Wizard approach**: Configurable wizard with smart defaults per type (Approccio 2). Each type creates a preset that populates the same wizard form. User can accept defaults (OK) or customize.

## IDE Expert - Template System (TemplatePro)

Code generation uses TemplatePro `.tpro` templates (migrazione completata, nessun file legacy).

**Key files:**
- `DMVC.Expert.ProjectGenerator.pas` - Main generator
- `DMVC.Expert.CodeGen.TemplateEngine.pas` - Template engine wrapper
- `DMVC.Expert.Presets.pas` - Preset configurations per project type
- `DMVC.Expert.Templates.rc` / `.res` - Embedded resources

**Template loading (dual-mode):**
1. File esterni: `C:\Users\Public\Documents\delphimvcframework_wizard_templates\` (priorita')
2. Risorse embedded nel BPL (fallback)

Template con prefisso `_` (es. `_license_header.tpro`) usano sempre la versione embedded.

**Templates (18):** `_license_header`, `program.dpr`, `program_service.dpr`, `service.pas`, `service.dfm`, `controller.pas`, `entity.pas`, `webmodule.pas`, `webmodule.dfm`, `jsonrpc.pas`, `services.pas`, `authentication.pas`, `helpers_{mustache,templatepro,webstencils}.pas`, `websocketserver.pas`, `project.dproj`, `project.rc`, `views/index_complete_view`.

**Config Keys -> Template Variables:** chiavi JSON con punti diventano underscore (`webmodule.middleware.session.memory` -> `webmodule_middleware_session_memory`).

**TemplatePro syntax:**
- Variabili: `{{:var}}`, `{{:obj.prop}}`
- Condizionali: `{{if cond}}...{{elseif}}...{{else}}...{{endif}}`
- Loop: `{{for item in list}}...{{endfor}}`
- Include: `{{include "file.tpro"}}`
- Filtri confronto: `eq`, `ne`, `gt`, `ge`, `lt`, `le`, `contains`
- Ref: https://www.danieleteti.it/templatepro/

**Comandi operativi:**
```bash
# Build wizard BPL (richiede IDE chiuso)
C:\DEV\dmvcframework\ideexpert\build_wizard2.bat

# Test templates
cd C:\DEV\dmvcframework\ideexpert\tests && build_and_run.bat
```
