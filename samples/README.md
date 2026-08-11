# DelphiMVCFramework Samples

Each folder contains a self-contained sample project. Build with Delphi 10.1+ and run.

> Folders `_`, `bin`, `commons` and `data` are **not** samples - they contain shared assets, compiled outputs, shared business objects and database files used by other samples.

## Getting Started

| Sample | Description |
|--------|-------------|
| `basicdemo_server` | Minimal HTTP server with a hello-world controller - **start here** |
| `basicdemo_vclclient` | VCL GUI client that talks to the basic demo server |
| `console_sample` | Console application showcasing the Console module (spinners, menus, tables, box drawing) |
| `servercontainer` | Standalone server container setup |

## REST API & CRUD

| Sample | Description |
|--------|-------------|
| `simple_api_using_datasets` | Minimal REST API backed by TDataSet |
| `simple_api_using_mvcactiverecord` | Minimal REST API backed by ActiveRecord |
| `simple_api_using_mvcactiverecord_with_injection` | Same, with dependency injection |
| `simple_api_using_mvcactiverecord_with_version` | Same, with URL-based API versioning |
| `simple_api_using_repository_with_injection` | REST API using the Repository pattern |
| `activerecord_restful_crud` | Full CRUD through `TMVCActiveRecordController` |
| `activerecord_showcase` | ActiveRecord features: queries, RQL, transactions, nullables |
| `articles_crud_server` | Articles CRUD server (pairs with clients below) |
| `articles_crud_vcl_client` | VCL client for articles CRUD |
| `articles_crud_vcl_client_api_binder` | VCL client with automatic API binding |
| `articles_crud_vcl_client_meta` | VCL client using metadata-driven approach |
| `master_details` | Master-detail record relationships |
| `avoid_mid_air_collisions_sample` | Optimistic locking - prevents concurrent update conflicts |
| `repository_showcase` | Repository pattern for data access |

## Authentication & Authorization

| Sample | Description |
|--------|-------------|
| `jsonwebtoken` | JWT authentication and authorization |
| `jsonwebtoken_livevaliditywindow` | JWT with sliding validity window |
| `jsonwebtoken_roleauth` | JWT with role-based authorization |
| `jsonwebtokenplain` | Plain JWT without framework middleware |
| `middleware_jwt_httponly_cookie` | JWT stored in HTTP-only cookies (XSS-safe) |
| `middleware_jwtblacklist` | JWT with token revocation/blacklist |
| `middleware_basicauthentication` | HTTP Basic Authentication |
| `middleware_oidc` | OpenID Connect (Keycloak, Entra ID, Auth0, Google...) |
| `middleware_oidc_docker` | OIDC with Docker Compose, PostgreSQL, user provisioning |
| `middleware_oidc_jwks` | OIDC with cryptographic ID-token signature verification via JWKS |
| `jsonwebtoken_refreshtoken` | Access token + refresh token rotation |
| `jwt_asymmetric_auth` | Asymmetric JWT (RS256 via TaurusTLS): private key signs, public key verifies. Includes a test suite over all 16 supported algorithms |
| `custom_auth` | Custom authentication (client + server) |
| `custom_role_auth` | Custom role-based authorization (client + server) |
| `hmac` | HMAC request signing |

## Minimal API

| Sample | Description |
|--------|-------------|
| `minimal_api` | Lambda routes with no controller class - every parameter-binding mode, bound by declared type |
| `minimal_api_auth` | The full `TMVCRouteGroup` hook chain: request logging, timing, auth |
| `minimal_api_webapp` | Server-rendered web app (TemplatePro + HTMX) built entirely from lambdas |
| `wizard_showcase` | Two self-documenting tutorials, REST and web - formerly wizard presets |

## Middleware

| Sample | Description |
|--------|-------------|
| `middleware` | Middleware pipeline basics |
| `middleware_activerecord` | ActiveRecord connection management middleware |
| `middleware_analytics` | Request analytics and tracking |
| `middleware_compression` | Response compression (gzip, deflate) |
| `middleware_cors` | Cross-Origin Resource Sharing |
| `middleware_etag` | ETag-based cache validation |
| `middleware_range_media` | HTTP Range requests for audio/video streaming (RFC 7233) |
| `middleware_ratelimit_memory` | Rate limiting (in-memory) |
| `middleware_ratelimit_redis` | Rate limiting (Redis-backed) |
| `middleware_staticfiles` | Static file serving |
| `middleware_trace` | Request tracing and debugging |

## Real-Time: WebSocket

| Sample | Description |
|--------|-------------|
| `websocket_chat` | Real-time chat with broadcasting |
| `websocket_groups` | Group-based messaging with rooms and commands |
| `websocket_primer` | Echo server with periodic heartbeat and VCL client |
| `websocket_client_sample` | VCL WebSocket client |
| `websocket_javascript_client_sample` | JavaScript client - stock ticker with live updates |

## Real-Time: Server-Sent Events

| Sample | Description |
|--------|-------------|
| `sse` | Basic SSE implementation |
| `sse2` | Advanced SSE with separate sender and viewer apps |
| `sse_chat` | Real-time chat using SSE (client + server) |
| `sse_indy_servers` | SSE for Indy-based servers |

## Streaming & Incremental Responses

| Sample | Description |
|--------|-------------|
| `http_streaming` | SSE, JSON Lines and CSV writers driving a browser UI |
| `streamed_array_writer` | Every incremental streaming mechanism in one project: nine endpoints over a 200k-row table |

## Server-Side Views & Templating

| Sample | Description |
|--------|-------------|
| `serversideviews_mustache` | Server-side rendering with Mustache |
| `serversideviews_templatepro` | Server-side rendering with TemplatePro |
| `templatepro_json` | TemplatePro with JSON data binding |

## HTMX Integration

| Sample | Description |
|--------|-------------|
| `htmx` | HTMX frontend integration basics |
| `htmx_mustache` | HTMX + Mustache templating |
| `htmx_templatepro` | HTMX + TemplatePro templating |
| `htmx_website_with_templatepro` | Full website with HTMX and TemplatePro |
| `htmx_website_with_webstencils` | Full website with HTMX and WebStencils |
| `instant_search_with_htmx_and_templatepro` | Live instant-search with HTMX |

## JSON-RPC

| Sample | Description |
|--------|-------------|
| `jsonrpc` | JSON-RPC 2.0 server with async and sync clients |

## Serialization & Rendering

| Sample | Description |
|--------|-------------|
| `jsondataobjects_serializer` | JSONDataObjects-based serialization |
| `jsonwriterrenders` | Custom JSON renderers |
| `renders` | Various rendering options (JSON, HTML, streams) |
| `render_binary_contents` | Rendering binary content (files, images) |

## API Documentation (Swagger / OpenAPI)

| Sample | Description |
|--------|-------------|
| `swagger_primer` | Swagger integration basics |
| `swagger_doc` | OpenAPI documentation generation |
| `swagger_doc_extended` | Extended Swagger features |
| `swagger_api_versioning_primer` | API versioning with Swagger |
| `swagger_ui` | Swagger UI web frontend |

## REST Client

| Sample | Description |
|--------|-------------|
| `RESTClient` | Making HTTP requests with `TMVCRESTClient` |

## Logging

| Sample | Description |
|--------|-------------|
| `Logger` | Basic logging with LoggerPro |
| `LoggerGUI` | Logging with GUI output |
| `custom_logger` | Custom logger implementation |
| `disable_default_logger` | Disabling the default logger |
| `log_filter` | Filtering log output by level/tag |
| `logger_with_callback` | Logger with callback functions |

## Sessions & Caching

| Sample | Description |
|--------|-------------|
| `sessions` | In-memory session management |
| `session_file_based` | File-based session storage |
| `outputcachewithredis` | Output caching with Redis |

## SSL / TLS

| Sample | Description |
|--------|-------------|
| `tls13` | HTTPS on Indy Direct + TaurusTLS, pinned to TLS 1.3 |
| `ssl_server` | SSL/TLS server (WebBroker + Indy OpenSSL, TLS 1.2 max) |
| `ssl_client` | SSL/TLS client |

## Validation

| Sample | Description |
|--------|-------------|
| `validation_showcase` | Input validation and error reporting (client + server) |
| `validation_vs_storage_demo` | The two validation layers - DTO and storage - on a single POST endpoint |

## Deployment

| Sample | Description |
|--------|-------------|
| `server_types` | The same controller and `ConfigureEngine` on every host - Indy Direct, HTTP.sys, WebBroker standalone, WebBroker via `IMVCServer`, ISAPI, Apache. Only the `.dpr` differs. No `.dproj`: open a `.dpr` in the IDE to generate one |
| `apache_module` | DMVCFramework as Apache module (mod_dmvc) |
| `isapi` | ISAPI extension for IIS |
| `server_in_dll` | Server packaged as a DLL |
| `windows_service` | Server as a Windows Service |

## Configuration

| Sample | Description |
|--------|-------------|
| `dotenv_simple` | Loading `.env` files |
| `dotenv_showcase` | Advanced `.env` management |

## Advanced Patterns

| Sample | Description |
|--------|-------------|
| `action_filters` | Before/after action filters |
| `controllers_register` | Dynamic controller registration at runtime |
| `services_injection` | Dependency injection and service container |
| `functional_actions_showcase` | Function-based actions (no controller class needed) |
| `strongly_typed_actions` | Actions with strongly typed parameters |
| `routing` | URL routing patterns and parameter extraction |
| `custom_exception_handling` | Custom exception handling and error responses |
| `custom_exception_handling_using_controller` | Exception handling through a dedicated controller |
| `webcontextevents` | Web context lifecycle events |
| `mvcasync` | Asynchronous task execution |
| `soap_rest` | SOAP and REST coexisting in the same server |
| `profiling` | Performance profiling |
| `profiling_showcase` | Detailed profiling metrics |
| `concurrency_speed_test` | Concurrent request performance benchmarks |
| `staticfiles_test` | Verification harness for the HTTP filters: StaticFiles, Compression, ETag, IPBlock, RateLimit, CORS, BasicAuth, sessions. Exits non-zero on failure |
| `error_page_test` | Verification harness for `UseExceptionHandler`: HTML error pages for browsers, JSON left untouched for APIs |

## Data Structures & Utilities

| Sample | Description |
|--------|-------------|
| `console_colors_basics` | The essential foreground colors and simple composition |
| `console_colors_composition` | Combining `Fore`, `Back` and `Style` |
| `console_colors_badges` | Badge patterns for test results, HTTP status and log levels |
| `console_colors` | The full ANSI palette, foreground and background |
| `console_themes_demo` | Console themes |
| `bloom_filter` | Bloom filter data structure |
| `objectpool` | Object pool pattern |
| `sqids_showcase` | Short unique ID generation (Sqids) |
| `nullable_types_showcase` | Nullable types showcase |
| `nullables` | Nullable value handling and tests |
| `higher_order_functions` | Higher-order functions (map, filter, reduce) |
| `datasets` | TDataSet utilities |
| `datapump` | Data migration utility |
| `ado` | ADO database integration |
| `rql2sql` | Interactive RQL-to-SQL converter (Firebird, PostgreSQL, MySQL, MSSQL, SQLite, Oracle) |
| `file_upload` | File upload handling |
| `data_url_upload` | Browser-to-server file upload via JSON-embedded data URLs (RFC 2397). Shows `IMVCTypeSerializer` end-to-end with a `TDataURL` custom type. |

## Full Demo Applications

| Sample | Description |
|--------|-------------|
| `wine_cellar_sample` | Complete app with VCL client, mobile client and REST server |

## External Projects

For Prometheus metrics integration, see [dmvc-prometheus-metrics](https://github.com/marcobreveglieri/dmvc-prometheus-metrics).
