# Changelog

All notable changes to DelphiMVCFramework will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [3.4.2-magnesium] - Current Stable

> 👉 A deep analysis of what's new in DelphiMVCFramework-3.4.2-magnesium is available on [Daniele Teti Blog](https://www.danieleteti.it/post/delphimvcframework-3-4-2-magnesium/)

### Added
- Support for Delphi 13 Athens
- Enhanced TLS 1.3 support with automatic cipher suite negotiation
- Improved security headers handling
- Better SNI (Server Name Indication) support
- Enhanced certificate validation mechanisms

## [3.4.1-sodium] - Previous Stable

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