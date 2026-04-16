# Changelog

All notable changes to DelphiMVCFramework will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [3.5.0-silicon] - Unreleased

### ⚠ BREAKING CHANGES

Two serialisation defaults have been flipped. Both affect the JSON
output of `OKResponse(TObject)` and are one-line overrides to restore
the pre-3.5 behaviour if needed.

**1. TGUID default format: braces + dashes → dashes only (RFC 4122)**

Before:
```json
{ "id": "{550E8400-E29B-41D4-A716-446655440000}" }
```
After:
```json
{ "id": "550e8400-e29b-41d4-a716-446655440000" }
```

The new default matches what JavaScript, Java, Python, .NET and
database clients expect. Delphi callers that parse the JSON response
with a strict braced-GUID regex will need to either update the parser
or restore the old default at program startup:
```pascal
uses MVCFramework.Serializer.Commons;
...
MVCGuidSerializationTypeDefault := gstBraces;
```
Alternatively, decorate individual fields with
`[MVCGuidSerialization(gstBraces)]`.

**2. `TDate` / `TDateTime` / `TTime` with value 0 serialises as the
1899-12-30 epoch, not as JSON `null`**

Before: a zero `TDateTime` emitted `null` on the wire. The framework
treated zero as a "not set" sentinel because `NullableDateTime` did
not yet exist.

After: zero is a valid calendar instant and is serialised as such:
```json
{ "when": "1899-12-30T00:00:00.000+00:00" }
```

If your API relied on the null-for-zero behaviour, migrate the
affected fields to `NullableTDateTime` (which preserves
`HasValue=False` → `null` correctly), or keep the zero check on the
client side. There is no runtime flag to restore the old behaviour -
the sentinel was lossy and breaks round-trips, so a real nullable
type is the only correct alternative going forward.

### Added

- **Pluggable HTTP server backends** behind a new `IMVCServer` interface
  (`MVCFramework.Server.Intf`). Three concrete backends ship out of the
  box:
  - `TMVCWebBrokerServer` - the classic WebBroker + `TIdHTTPWebBrokerBridge`
    pipeline (now one option among several, not the only path).
  - `TMVCIndyServer` - a direct `TIdHTTPServer` backend that skips
    WebBroker entirely. Smaller request pipeline, faster dispatch for
    small payloads.
  - `TMVCHttpSysServer` - Windows kernel-mode `http.sys` backend with
    async dispatch to the default task pool. Request body is streamed
    straight into a pre-sized `TBytes` when `Content-Length` is known,
    avoiding the legacy `TMemoryStream` + final `SetLength/Move`
    round trip.

  Speaking class constructors on `TMVCEngine` select the backend that
  the engine targets: `TMVCEngine.CreateForWebBroker(AWebModule)`,
  `TMVCEngine.CreateForIndyDirect(AConfigAction)`,
  `TMVCEngine.CreateForHttpSys(AConfigAction)`. The pre-3.5 one-arg
  `TMVCEngine.Create(AWebModule)` constructor is preserved but
  deprecated; existing code compiles unchanged with a deprecated
  warning until you migrate at your convenience.

  Built-in HTTPS: each backend accepts a `HTTPSConfigurator` (wired up
  by adding `uses MVCFramework.Server.HTTPS.TaurusTLS`) so TLS setup is
  inside the server, not in the caller.

- **Streaming JSON serializer fast path** (`MVCFramework.Serializer.Streaming`)
  for `OKResponse(TObject)` / `OKResponse(TObjectList<T>)`. Writes JSON
  directly to the response stream via `System.JSON.Writers.TJsonTextWriter`
  with a per-class cached emission plan - no intermediate `TJDOJsonObject`
  tree, no UTF-16 string. Benchmarked gain on HTTP.sys at c=100 (median
  of 3 x 30 s): `pods/small` +18.6% (2641 -> 3132 rps),
  `pods/large` +74.6% (251 -> 438 rps). Requires Delphi 10.3 Rio+; on
  older compilers the new path is a stub and the legacy serializer is
  used unchanged.

  **Full feature parity with the legacy serializer** - byte-identical
  output across all supported shapes, verified via a 50-scenario parity
  harness (`performancetest/parity/ParityCheck.exe`):

  - Primitive properties: `Integer`, `Int64`, `Single`, `Double`,
    `Extended`, `Currency`, `String`, `Boolean`, `TDate`, `TTime`,
    `TDateTime`, `TGUID`, non-`Boolean` enums.
  - All `NullableXxx` record types - `NullableString`, `NullableInt8`..`Int64`,
    `NullableUInt8`..`UInt64`, `NullableByte`, `NullableSingle`/`Double`/`Extended`/`Float32`/`Float64`,
    `NullableCurrency`, `NullableBoolean`, `NullableTDate`/`TTime`/`TDateTime`,
    `NullableTGUID`, `NullableAnsiString`, `NullableInteger`. Honours
    `MVCSerializeNulls`.
  - Nested `TObject` properties (recursively validated at plan-build
    time with placeholder-based cycle detection).
  - `TObjectList<T>` / `TList<T>` properties (polymorphic items: plan
    is resolved per runtime `ClassType`).
  - `TArray<T>` properties - primitives, `TObject` subclasses, `Nullable*`
    records.
  - `TStream` / `TMemoryStream` / `TStringStream` properties - base64
    encoded, matching `TMVCStreamSerializerJsonDataObject` output.
  - `TDataSet` properties - delegates to the legacy
    `SerializeDataSet(TMVCNameCase.ncUseDefault)` so `ApplyNameCase`,
    ignored fields, nested datasets, blob base64, `ftGuid` / `ftFMTBcd`
    handling and field attributes all come across untouched.
  - Attributes honoured: `MVCNameAs`, `MVCNameCase` (class-level and
    `MVCNameCaseDefault` global), `MVCDoNotSerialize`.
  - Classes marked `[MVCSerialize(stFields)]` and properties whose type
    has a custom `IMVCTypeSerializer` registered (that the streaming
    path does not recognise natively) transparently fall back to the
    legacy serializer - output is byte-identical in both cases.

  If a runtime class or polymorphic list item turns out to be
  unsupported mid-emission, the streaming path raises an internal
  fallback exception, rewinds the output stream to the pre-write
  mark, discards the thread-local writer state and returns `False`
  so the caller engages the legacy path with no partial bytes on
  the wire.
- **HTTP.sys async dispatch** - the kernel-mode backend now offloads
  every request body read + pipeline execution to the default task pool
  instead of serialising everything on the listener thread. Upload 1 MB
  went from 95 to 534 rps on the bench machine.
- **HTTP.sys zero-copy body read** - when `Content-Length` is known up
  front the request body is written straight into a pre-sized `TBytes`,
  eliminating the previous `TMemoryStream` + final `SetLength/Move`
  round trip.
- **Per-engine route table** (`TMVCRouteTable`) computed once at
  `AddController` time and indexed first by HTTP method then by path,
  replacing the per-request RTTI scan that used to run on every
  dispatch. Static routes hit a string dictionary in O(1); parametric
  routes are a short per-method list using the already-cached regex
  from `gMVCGlobalActionParamsCache`.
- **Render fast path for `OKResponse(TJsonBaseObject)`** - the JsonObject
  is written to the response stream via `TJsonBaseObject.SaveToStream`,
  skipping the UTF-16 Delphi string that `ToJSON(True)` would allocate
  and the subsequent UTF-8 re-encode inside `Render(AContent: string)`.

### Changed

- Default `TGUID` serialisation format is now dashes-only (RFC 4122)
  instead of `{braces}`. See **BREAKING CHANGES** above for migration.
- `TDate` / `TDateTime` / `TTime` zero no longer serialises as JSON
  `null`. See **BREAKING CHANGES** above for migration.

### Fixed

- **`Single` properties no longer leak the imprecise Extended tail on
  the wire.** Both the legacy and the streaming serializers now
  round-trip `Single` values through their lossless 7-digit decimal
  form before emitting, so `Single(1e-10)` serialises as `1E-10`
  instead of `1.00000001335143E-10`. `Double` / `Extended` properties
  still use 15 significant digits as before.
- **Server-abstraction test fixtures leaked their `TMVCEngine`**
  (~2.5 MB per test run). `[SetUp]` / `[TearDown]` methods were
  declared under `protected` visibility, and DUnitX only invokes
  fixture lifecycle methods that live in `public`. Tests passed but
  FastMM4 reported an "Unexpected Memory Leak" block at process
  shutdown. Visibility fixed; `tests32` / `tests64` now report a clean
  shutdown. Users of the framework were never affected - the leak
  only showed up during CI test runs, never at runtime.

### Performance

All numbers below are median of 3 x 30 s runs at c=100 on a loopback
HTTP.sys bench (i9-13980HX, Win 11, Release Win64). See
`performancetest/results/BASELINE_AFTER.md` for the full matrix and
the cross-backend comparison.

| Scenario        | Before | After  | Delta  |
|-----------------|-------:|-------:|-------:|
| health          | 2354   | 3380   | +44%   |
| json/small      | 2099   | 2858   | +36%   |
| json/large      |  735   |  889   | +21%   |
| heavy chain     | 1874   | 3131   | +67%   |
| upload 1 MB     |   95   |  892   | +839%  |
| pods/small (\*) |  new   | 3132   | +18.6% over legacy |
| pods/large (\*) |  new   |  438   | +74.6% over legacy |

`(*)` new benchmark scenarios introduced in 3.5.x exercising the
streaming serializer.

## [3.4.3-aluminium] - Current Stable

> 👉 A deep analysis of what's new in DelphiMVCFramework-3.4.3-aluminium is available on [Daniele Teti Blog](https://www.danieleteti.it/post/released-dmvcframework-3-4-3-aluminium/)

### Added
- **WebSocket support** (RFC 6455) - Full bidirectional real-time communication
  - Server implementation with groups, periodic messages, and broadcasting
  - Client implementation with auto-reconnect support
- **Repository pattern** support for cleaner data access abstractions
- **Rate limiting middleware** with in-memory and Redis-backed implementations
- **Server-Sent Events (SSE)** improvements with `TMVCSSEController`
- Enhanced build system with centralized dependency management

### Improved
- Updated TemplatePro to version 0.9.0
- Better package structure for all Delphi versions
- Performance optimizations and bug fixes

## [3.4.2-magnesium] - Previous Stable

> 👉 A deep analysis of what's new in DelphiMVCFramework-3.4.2-magnesium is available on [Daniele Teti Blog](https://www.danieleteti.it/post/delphimvcframework-3-4-2-magnesium/)

### Added
- Support for Delphi 13 Florence
- Enhanced TLS 1.3 support with automatic cipher suite negotiation
- Improved security headers handling
- Better SNI (Server Name Indication) support
- Enhanced certificate validation mechanisms
- ⚡ **Comprehensive Validation System** with 50+ validators
  - Property-level validators: `MVCRequired`, `MVCEmail`, `MVCRange`, `MVCPattern`, etc.
  - Format validators: `MVCCreditCard`, `MVCIBAN`, `MVCIPv4`, `MVCSemVer`, etc.
  - Tax ID validators: `MVCITCodiceFiscale`, `MVCITPartitaIVA`, `MVCUSSSN`, `MVCBRCPF`, etc.
  - Cross-field validator: `MVCCompareField` for password confirmation, etc.
  - Object-level validation: `OnValidate(const AErrors: PMVCValidationErrors)` method for complex cross-field rules
  - Automatic validation on `[MVCFromBody]` parameter injection
  - Recursive validation for nested objects and collections
  - Zero allocation for valid objects (lazy dictionary creation)
  - HTTP 422 response with JSON error details on validation failure

## [3.4.1-sodium]

> 👉 A deep analysis of what's new in DelphiMVCFramework-3.4.1-sodium is available on [Daniele Teti Blog](https://www.danieleteti.it/post/delphimvcframework-3-4-1-sodium/)

### Added
- Performance improvements in JSON serialization
- Enhanced middleware pipeline
- Better error handling mechanisms

## [3.4.0-neon] - Major Release

> 👉 Deeper analysis of what's new in DelphiMVCFramework-3.4.0-neon is available on [Daniele Teti Blog](http://www.danieleteti.it/post/delphimvcframework-3-4-0-neon/)

### Added
- ⚡ Support for dotEnv configuration files
- ⚡ MSHeap memory manager support for Win32 and Win64
- ⚡ HTMX server-side support through unit `samples\htmx\MVCFramework.HTMX.pas`
- ⚡ "Load Style" methods for `TMVCActiveRecord` entities
- ⚡ Functional Actions support - functions can now be used as actions
- ⚡ Enhanced `TMVCResponse` type for functional actions
- ⚡ SQL and RQL Named Queries support for TMVCActiveRecord
- ⚡ Better error messages for serialization failures

### Fixed
- 🐞 Issue [#664](https://github.com/danieleteti/delphimvcframework/issues/664)
- 🐞 Issue [#667](https://github.com/danieleteti/delphimvcframework/issues/667)
- 🐞 Issue [#680](https://github.com/danieleteti/delphimvcframework/issues/680)
- 🐞 Issue [#682](https://github.com/danieleteti/delphimvcframework/issues/682)
- 🐞 Wrong comparison in checks for ro/RW/PK fields in `TMVCActiveRecord`

### Changed
- Property `HTTPErrorCode` renamed to `HTTPStatusCode` in `EMVCException`
- Removed `statuscode`, `reasonstring` and default value fields from exception JSON rendering

## [3.3.0-fluorine] - Feature Release

### Added
- ⚡ Support for Delphi 11.3 Alexandria
- ⚡ Records support in Swagger param and response attributes
- ⚡ Prometheus middleware sample
- ⚡ Enhanced profiler with threshold logging

### Fixed
- 🐞 Issue [#648](https://github.com/danieleteti/delphimvcframework/issues/648)
- 🐞 Issue [#652](https://github.com/danieleteti/delphimvcframework/issues/652)

### Improved
- Better compatibility with Delphi 10.2 Tokyo and older versions
- Improved wizard with commented code examples

## [3.2.3-radium] - Stability Release

### Added
- ⚡ Standard HTTP reason strings in error responses
- ⚡ `HTTP_STATUS.ReasonStringFor(HTTPStatusCode)` method
- ⚡ SHA1 instead of MD5 for mid-air-collision handling
- ⚡ Global `MVCSerializeNulls` configuration variable
- ⚡ Built-in profiler for Delphi 10.4+ with timing measurements
- ⚡ `ActionQualifiedName` context property
- ⚡ Object pool and interface object pool support
- ⚡ Async JSON-RPC call support
- ⚡ Enhanced `TMVCActiveRecordMiddleware` with multiple connection support

### Fixed
- Multiple rendering problems in Swagger interface
- Issues [#594](https://github.com/danieleteti/delphimvcframework/issues/594), [#595](https://github.com/danieleteti/delphimvcframework/issues/595), [#590](https://github.com/danieleteti/delphimvcframework/issues/590)
- Issue [#490](https://github.com/danieleteti/delphimvcframework/issues/490)
- Issues [#583](https://github.com/danieleteti/delphimvcframework/issues/583), [#585](https://github.com/danieleteti/delphimvcframework/issues/585)

### Improved
- Better nullable types with `Equal` method and `TryHasValue`
- Enhanced error handling and unit test coverage
- Better profiler integration with nested method calls

## [3.2.2-nitrogen] - Major Feature Release

### Added
- ⚡ Support for Delphi 11.x Alexandria
- ⚡ New `TMVCRESTClient` implementation based on Net components
- ⚡ `MVCJSONRPCAllowGET` attribute for JSON-RPC over HTTP GET
- ⚡ eLua server-side view support
- ⚡ `TMVCLRUCache` efficient LRU cache implementation
- ⚡ `TMVCRedirectMiddleware` for HTTP redirections
- ⚡ XML field type support in PostgreSQL for `TMVCActiveRecord`
- ⚡ `OnContextCreate` and `OnContextDestroyed` events for `TMVCEngine`
- ⚡ `NullableTGUID` support
- ⚡ `MVCFromBody`, `MVCFromQueryString`, `MVCFromHeader`, `MVCFromCookie` attributes
- ⚡ Automated mid-air collision avoidance with ETag support
- ⚡ `TMVCJWTBlackListMiddleware` for JWT token blacklisting
- ⚡ Enhanced static files middleware with file filtering
- ⚡ Default filtering and partitioning for `TMVCActiveRecord`
- ⚡ Pascal `set` serialization/deserialization
- ⚡ GUID primary key support

### Fixed
- Issues [#484](https://github.com/danieleteti/delphimvcframework/issues/484), [#472](https://github.com/danieleteti/delphimvcframework/issues/472), [#470](https://github.com/danieleteti/delphimvcframework/issues/470)
- Issues [#453](https://github.com/danieleteti/delphimvcframework/issues/453), [#455](https://github.com/danieleteti/delphimvcframework/issues/455)
- Various other stability and performance issues

### Breaking Changes
- Removed deprecated constructor for `TMVCJWTAuthenticationMiddleware`
- Changed signature of method `IMVCMiddleware.OnAfterControllerAction`

### Improved
- Dramatically improved JSON-to-DataSet operations performance
- Better connection handling for multiple database connections
- Enhanced RQL support with better SQLGenerator architecture
- Improved error handling for JSON-RPC APIs

## [3.2.1-carbon] - Book Reference Release

This version is referenced by the "DelphiMVCFramework - The Official Guide" book.

### Added
- Enhanced Swagger documentation with docExpansion parameter
- New `Context: TWebContext` parameter in JSON-RPC Hooks
- Boolean serialization improvements for JSON-RPC
- React demo sample
- Serialization support for `TList<T>` of simple types
- `MetadataAsJSONObject` method for dataset field definitions
- Field options: `foReadOnly` and `foWriteOnly` in `MVCTableField`
- Object deserialization from arbitrary JSON nodes
- Primary key type handling improvements
- `TMVCStaticFilesMiddleware` SPA application support
- `Context.HostingFrameworkType` property
- `ncSnakeCase` naming case support
- Mustache partials support
- Dynamic properties access in `TMVCActiveRecord`

### Fixed
- Issues [#421](https://github.com/danieleteti/delphimvcframework/issues/421), [#424](https://github.com/danieleteti/delphimvcframework/issues/424), [#436](https://github.com/danieleteti/delphimvcframework/issues/436)
- Issues [#438](https://github.com/danieleteti/delphimvcframework/issues/438), [#432](https://github.com/danieleteti/delphimvcframework/issues/432), [#435](https://github.com/danieleteti/delphimvcframework/issues/435)
- Issues [#434](https://github.com/danieleteti/delphimvcframework/issues/434), [#221](https://github.com/danieleteti/delphimvcframework/issues/221), [#430](https://github.com/danieleteti/delphimvcframework/issues/430)

### Breaking Changes
- `TMVCStaticFileMiddleware` cannot be registered to "/" anymore
- `DocumentRoot` of `TMVCStaticFileMiddleware` must be a valid folder

## [3.2.0-boron] - Major Architecture Release

### Added
- Support for Delphi 10.4 Sydney
- Nullable support in MVCActiveRecord
- Non-autogenerated primary keys in MVCActiveRecord
- Complete nullable types support in default serializer
- `ncCamelCase` and `ncPascalCase` attribute formatters
- Swagger support
- `MVCDoNotDeserialize` attribute
- SQLGenerator and RQL compiler for PostgreSQL, SQLite, MSSQLServer
- `MVCNameAs` attribute with `Fixed` parameter
- Interfaces serialization support
- Spring4D collections and nullable types support
- `OnRouterLog` event for custom request logging
- System controllers loading configuration
- Enhanced HATEOAS support in renders
- `TMVCActiveRecord.Count` method
- `contains` and `out` operators in RQL
- `TMVCAnalyticsMiddleware` for API analytics
- `TMVCActiveRecord.DeleteAll` and `DeleteRQL` methods
- `TMVCActiveRecord.Store` method for automatic Insert/Update
- Microsoft SQL Server and SQLite support
- JSON verbatim pass for `TJSONObject` properties
- `StrDict` function for dictionary rendering
- Custom exception handling
- SSL server support for `TMVCListener`
- `in` operator in RQL parser
- `X-HTTP-Method-Override` header support
- Support for `TArray<String/Integer/Double>` serialization
- JWT standard compliance improvements
- 180+ unit tests
- `StrToJSONObject` safe parsing function
- Custom `TDataSet` serialization callbacks
- Shortcut render methods (201Created, 202Accepted, 204NoContent)
- Generic iterables serialization without `MVCListOf`
- ObjectPool and IntfObjectPool
- Experimental Android server support
- Children objects lifecycle management in `TMVCActiveRecord`

### Fixed
- Issues [#38](https://github.com/danieleteti/delphimvcframework/issues/38), [#140](https://github.com/danieleteti/delphimvcframework/issues/140), [#161](https://github.com/danieleteti/delphimvcframework/issues/161)
- Issues [#184](https://github.com/danieleteti/delphimvcframework/issues/184), [#278](https://github.com/danieleteti/delphimvcframework/issues/278), [#164](https://github.com/danieleteti/delphimvcframework/issues/164)
- Many other stability and performance issues
- Serious security bug in static file serving

### Breaking Changes
- `MVCPrimaryKey` attribute removed and merged with `MVCTableField`
- Middleware `OnAfterControllerAction` invocation order changed
- `TMVCEngine` no longer serves static files
- `TMVCEngine.Config` property is now read-only
- Various configuration keys removed

### Improved
- Dataset serialization speed (up to 2 orders of magnitude improvement)
- Better packages organization
- Enhanced JSON-RPC improvements
- ObjectDict function for flexible rendering

## [3.1.0-lithium] - ActiveRecord Release

### Added
- `TMVCActiveRecord` framework
- `TMVCActiveRecordController` with automatic RESTful interface
- EntityProcessor for complex cases
- JSON-RPC executor HTTP headers configuration
- `TDataSetHolder`
- `TMVCResponse` for generic responses
- `gzip` compression support
- Spring4d nullable types support
- `TMVCJSONRPCPublisher` for plain Delphi objects exposure
- Interface-based JSON-RPC client layer

### Fixed
- 404 and 500 status codes content-type
- Speed improvements
- `MAX_REQUEST_SIZE` request limiting

### Breaking Changes
- JSON-RPC client layer is now interface-based
- `TCompressionMiddleware` renamed to `TMVCCompressionMiddleware`

## [3.0.0-hydrogen] - Major Version Release

### Added
- First release of version 3.0.0 architecture
- Complete framework rewrite
- New routing system
- Enhanced middleware support

## Legacy Versions (2.x)

### [2.1.3-lithium]
- Fixed issue #64
- Added regression tests

### [2.1.2-helium]
- Fixed `TJSONBool` compatibility for older Delphi versions
- Added JSONBOOL conditional define

### [2.1.1-hydrogen]
- Updated IDE Expert with version display
- Fixed mapper null values handling
- Added Boolean values support in dataset serialization
- Added unit tests for Mapper and nullability
- Added `DMVCFRAMEWORK_VERSION` constant

---

## Migration Guides

For detailed migration information between major versions, see:
- [Migration from 2.x to 3.x](MIGRATION_2_TO_3.md)
- [Migration from 3.1.x to 3.2.x](MIGRATION_31_TO_32.md)
- [Migration from 3.2.x to 3.3.x](MIGRATION_32_TO_33.md)

## Contributing

We welcome contributions! Please see our [Contributing Guidelines](CONTRIBUTING.md) for details on how to contribute to the changelog and release process.