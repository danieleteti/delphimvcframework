// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ***************************************************************************
//
// Filter helpers for the minimal-API surface.
//
// Two kinds of filter live here:
//
//   * EndpointFilter  — per-route-group, wraps the handler, returns
//                       IMVCResponse. Registered via .Use(...) on a group.
//   * HTTPFilter      — engine-wide, wraps the entire dispatch including
//                       routing. Registered via lEngine.UseHTTPFilter(...).
//
// The base filter types (TMVCEndpointFilter, TMVCHTTPFilter, *Next) live in
// MVCFramework.MinimalAPI — this unit only provides the off-the-shelf helpers
// that build on top of them.
//
// Helpers exposed here:
//
//   EndpointFilter (per-group):
//     CORS, JWT, BasicAuth, ActiveRecord,
//     MemorySession, FileSession, DatabaseSession
//
//   HTTPFilter (engine-wide):
//     StaticFiles, Compression, ETag, IPBlock, RateLimit, RequestLog,
//     CORSFilter, SecurityHeaders, Shutdown, RangeMedia,
//     BearerAuthFilter, AllowedOriginFilter, AllowedHostFilter,
//     Analytics, Trace, Redirect, OpenAPI, Swagger
//
//   Plus top-level helpers: WaitForShutdownOrConsoleReturn.
//
// Redis-backed companion unit (optional, avoids pulling DelphiRedisClient
// into every Filters consumer):
//     MVCFramework.Filters.Redis — RateLimitRedis
//
// ***************************************************************************

unit MVCFramework.Filters;

{$I dmvcframework.inc}

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  LoggerPro,                                   // ILogWriter (Analytics helper)
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.MinimalAPI,                     // TMVCEndpointFilter, TMVCHTTPFilter, *Next
  MVCFramework.JWT,                            // TJWT, TJWTCheckableClaims (JWT helper)
  MVCFramework.Middleware.Authentication,      // IMVCAuthenticationHandler (JWT + BasicAuth)
  MVCFramework.Middleware.StaticFiles,         // TMVCStaticFileRulesProc, TMVCStaticFileMediaTypesCustomizer
  MVCFramework.OpenAPI3,                       // TMVCOpenAPIInfo (OpenAPI helper)
  MVCFramework.Swagger.Commons,                // TMVCSwaggerInfo, JWT_DEFAULT_DESCRIPTION (Swagger helper)
  Swag.Common.Types;                           // TMVCTransferProtocolSchemes (Swagger helper)

// -------------------------------------------------------------------------
// EndpointFilter helpers (per route group, attached via group.Use).
// -------------------------------------------------------------------------

// Returns a filter that wires an in-memory session factory onto every request
// passing through the group it is attached to (and every nested sub-group, by
// the standard filter-inheritance rules).
//
//   lEngine.Root.AsWeb
//     .Use(MemorySession(10))         // 10-minute idle timeout, HttpOnly on
//     .MapGet('/', HomeHandler);
//
// The factory is owned by the filter via an interface-managed holder, so it is
// freed when the filter is dropped (typically engine shutdown). The session
// itself is read/written via the usual TWebContext.Session API.
function MemorySession(const ATimeoutInMinutes: Integer = 0;
  const AHttpOnly: Boolean = True;
  const ASecure: Boolean = False): TMVCEndpointFilter;

// CORS endpoint filter: stamps the Access-Control-* headers on the responses of
// the group's routes. Like every endpoint filter it runs only on a matched route,
// so it answers a browser preflight (OPTIONS) only on a route registered for
// OPTIONS too; an OPTIONS on a GET/POST-only route is a 404 from the router
// before this filter runs. To answer preflights for the whole API use
// CORSFilter, the HTTP filter, which runs before routing.
// Defaults match the typical permissive setup used by the classic
// TMVCCORSMiddleware.
function CORS(
  const AAllowedOriginURLs: string = '*';
  const AAllowsCredentials: Boolean = True;
  const AExposeHeaders: string = '';
  const AAllowsHeaders: string = 'X-Requested-With, Content-Type, Accept, Origin, Authorization';
  const AAllowsMethods: string = 'GET, POST, PUT, DELETE, OPTIONS';
  const AAccessControlMaxAge: Integer = 86400): TMVCEndpointFilter;

// JWT bearer-auth filter. Reads the Authorization header, verifies the
// token against ASecret with AHMACAlgorithm, validates the standard claims
// listed in AClaimsToCheck, and populates Context.LoggedUser on success.
// On failure short-circuits with a 401 (Status response, {"message": ...}).
// Special-cases the configured login URL: a POST to ALoginURLSegment is
// forwarded to AAuthenticationHandler.OnAuthentication for credential
// verification, then a freshly-minted token is returned in the
// Authorization response header.
function JWT(
  const AAuthenticationHandler: IMVCAuthenticationHandler;
  const AClaimsSetup: TJWTClaimsSetup;
  const ASecret: string = MVC_JWT_INSECURE_DEFAULT_SECRET;
  const ALoginURLSegment: string = '/login';
  const AClaimsToCheck: TJWTCheckableClaims = [];
  const ALeewaySeconds: Cardinal = 300;
  const AHMACAlgorithm: string = 'HS512'): TMVCEndpointFilter;

// ActiveRecord lifecycle filter. Acquires a FireDAC connection from the
// FDManager pool keyed by ADefaultConnectionDefName before the handler
// runs and releases it inside a `finally` so the connection is returned
// to the pool even when the handler raises. The FireDAC connection
// definitions must already be loaded (typically in BootConfigU at startup).
function ActiveRecord(
  const ADefaultConnectionDefName: string): TMVCEndpointFilter;

// -------------------------------------------------------------------------
// HTTPFilter helpers (engine-wide, attached via lEngine.UseHTTPFilter).
// -------------------------------------------------------------------------

// HTTPFilter that serves static files from ARootFolder under APrefix.
// Requests matching the prefix and pointing to an existing file short-
// circuit with the file contents; everything else falls through to Next()
// (route matching + handlers).
// Path traversal is rejected (any '..' or absolute paths return 403).
// MIME type is inferred from the file extension; unknown types fall back
// to application/octet-stream.
//
//   lEngine.UseHTTPFilter(StaticFiles('/static', 'www'));
function StaticFiles(const APrefix: string; const ARootFolder: string;
  const ADefaultDocument: string = 'index.html'): TMVCHTTPFilter; overload;

// HTTPFilter that compresses the response body post-Next when:
//   - response has a ContentStream larger than ACompressionThreshold bytes;
//   - client advertised a supported encoding via Accept-Encoding (gzip or
//     deflate; gzip wins if both are listed).
// Sets Content-Encoding accordingly. No-op for responses below threshold,
// responses without a ContentStream, or requests without Accept-Encoding.
//
//   lEngine.UseHTTPFilter(Compression(1024));
function Compression(const ACompressionThreshold: Integer = 1024): TMVCHTTPFilter;

// HTTPFilter that adds RFC 7232 ETag validation post-Next:
//   - SHA1-hashes the response ContentStream and stamps a strong ETag
//     header on the response (quoted hex digest);
//   - if the request carries If-None-Match matching the computed ETag,
//     short-circuits the body and returns 304 Not Modified.
// No-op for responses without a ContentStream or with a non-2xx status.
// Register BEFORE Compression so ETag wraps it on the outside — that way
// the post-Next of ETag runs AFTER Compression has rewritten the stream,
// and the hash reflects the bytes actually sent on the wire (different
// ETag per encoding, per RFC).
//
//   lEngine.UseHTTPFilter(ETag).UseHTTPFilter(Compression);
function ETag: TMVCHTTPFilter;

// HTTPFilter that short-circuits requests from any client IP in the
// blocklist with HTTP 403. Comparison is exact-string against
// Ctx.Request.ClientIp and Ctx.Request.PeerIp; no CIDR matching. Returns the filter as-is so
// callers can chain registration.
//
//   lEngine.UseHTTPFilter(IPBlock(['10.0.0.5', '192.168.1.42']));
function IPBlock(const ABlockedIPs: TArray<string>): TMVCHTTPFilter;

// HTTPFilter that enforces a sliding-window rate limit per client IP.
// More than AMaxRequests within AWindowSeconds triggers HTTP 429 with a
// Retry-After header. State is held in a process-wide thread-safe map.
//
//   lEngine.UseHTTPFilter(RateLimit(100, 60));   // 100 req / minute / IP
function RateLimit(const AMaxRequests: Integer = 60;
  const AWindowSeconds: Integer = 60): TMVCHTTPFilter;

// HTTPFilter that emits a one-line request log via MVCFramework.Logger
// after the inner pipeline completes. Format:
//   [HTTP] <ip> <method> <path> -> <status> (<duration_ms>ms)
// Times include any HTTPFilter wrapped inside it; place this filter
// outermost (first registered) to capture full request latency.
//
//   lEngine.UseHTTPFilter(RequestLog);
function RequestLog: TMVCHTTPFilter;

// HTTPFilter version of CORS. Short-circuits OPTIONS preflight requests
// with the full CORS header set + 200 OK; on every other request stamps
// the simple CORS headers post-Next so they land on success AND error
// responses. Equivalent to the EndpointFilter CORS() helper but engine-
// wide and runs BEFORE routing — important for preflights that should
// not be subject to per-group filters or route matching.
//
//   lEngine.UseHTTPFilter(CORSFilter('*', False, '', 'Content-Type,Authorization',
//     'GET,POST,PUT,DELETE,OPTIONS', 1728000));
function CORSFilter(
  const AAllowedOriginURLs: string = '*';
  const AAllowsCredentials: Boolean = False;
  const AExposeHeaders: string = '';
  const AAllowsHeaders: string = 'Content-Type,Authorization';
  const AAllowsMethods: string = 'GET,POST,PUT,DELETE,PATCH,OPTIONS';
  const AAccessControlMaxAge: Integer = 1728000): TMVCHTTPFilter;

/// <summary>Returns the value to emit as Access-Control-Allow-Origin given a
/// comma-separated list of configured origins and the request's Origin header:
/// the matched origin, '*' if a wildcard is configured, or '' if there is no
/// match (the caller then emits no ACAO header). Never returns the whole list
/// verbatim, which would be an invalid header.</summary>
function MVCMatchCORSOrigin(const AConfiguredOrigins, ARequestOrigin: string): string;

// HTTPFilter that stamps the baseline response security headers
//   X-XSS-Protection: 0
//   X-Content-Type-Options: nosniff
// on every response. Stamped pre-Next so handlers can still override if a
// stricter (or looser) policy is needed for a specific route. Equivalent
// to the classic TMVCSecurityHeadersMiddleware.
//
//   lEngine.UseHTTPFilter(SecurityHeaders);
function SecurityHeaders: TMVCHTTPFilter;

// -------------------------------------------------------------------------
// Request guards (HTTPFilter): who may talk to a path, and from where.
// All three take an optional APathPrefix, the full routed path (engine path
// prefix included) they guard; '' guards every request. A request is guarded
// when its path, normalized as the router does, is the prefix or below it.
// The path is tested as PathInfo, as RawPathInfo and as the URL-decoded
// RawPathInfo, so a guard and the router cannot disagree on what they see.
// Register the guards before any classic middleware.
// -------------------------------------------------------------------------

type
  // Token validator for BearerAuthFilter. Return True to accept the token and
  // set AUserName (and ARoles) of the identity it stands for; False to refuse.
  TMVCBearerValidator = reference to function(const AToken: string;
    out AUserName: string; var ARoles: TArray<string>): Boolean;

// The path as the router sees it before matching: query dropped, trimmed,
// leading '/', lower case, no trailing '/'. '/' and '' both give ''.
function MVCNormalizePath(const APath: string): string;

// True when the request targets APathPrefix or something below it (see above).
// AExact is True when it targets the prefix itself (not a sub-path) on every
// form tested. APathPrefix '' matches every request (AExact False).
function MVCRequestTargetsPath(const AContext: TWebContext;
  const APathPrefix: string; out AExact: Boolean): Boolean;

// Validator for a fixed set of tokens (name = identity, value = secret). The
// comparison does not stop at the first different character. A token that is
// empty or has leading/trailing whitespace is refused with an exception: a
// client token is trimmed, so it could never match.
function StaticBearerTokens(const ATokens: TArray<TPair<string, string>>): TMVCBearerValidator;

// HTTPFilter: `Authorization: Bearer <token>` checked with AValidator on the
// guarded paths. On success fills Context.LoggedUser (UserName, Roles,
// LoggedSince); otherwise 401 with `WWW-Authenticate: Bearer`.
//
//   lEngine.UseHTTPFilter(BearerAuthFilter(
//     StaticBearerTokens([TPair<string, string>.Create('cli', lToken)]), '/mcp'));
// No verb is exempt: a browser CORS preflight (OPTIONS) carries no Authorization and
// gets 401 on the guarded paths. The prefix must be the full routed path.
function BearerAuthFilter(const AValidator: TMVCBearerValidator;
  const APathPrefix: string = ''): TMVCHTTPFilter;

// HTTPFilter: refuses with 403 a request that carries an Origin not in the
// list (case-insensitive, exact). A request with no Origin header passes: it
// is not a browser cross-origin request. CORSFilter only decides what the
// browser may read; this one stops the request on the server.
function AllowedOriginFilter(const AOrigins: TArray<string>;
  const APathPrefix: string = ''): TMVCHTTPFilter;

// HTTPFilter against DNS rebinding: refuses with 403 a request whose Host
// header (port ignored) is not localhost, 127.0.0.1, [::1] or one of AHosts.
// Meant for services that listen on loopback without authentication.
function AllowedHostFilter(const AHosts: TArray<string>;
  const APathPrefix: string = ''): TMVCHTTPFilter;

// HTTPFilter: limits APath (exact, not its sub-paths) to the verbs in AMethods.
// A request that does not target APath goes on; one that targets it with a
// verb not in AMethods, or that targets a sub-path of it, gets 405 with
// `Allow: <verbs>` (listed in TMVCHTTPMethodType order).
function AllowedMethodsFilter(const AMethods: TMVCHTTPMethods;
  const APath: string): TMVCHTTPFilter;

// HTTPFilter that intercepts a POST request to AURLSegment carrying the
// configured API key query-string param and triggers an orderly engine
// shutdown via EnterInShutdownState. Non-matching requests fall through
// to Next() unmodified. DESTRUCTIVE endpoint — register this AS THE FIRST
// HTTPFilter so it runs ahead of CORS, Compression, RateLimit etc., and
// always pair it with a strong, hard-to-guess API key value.
//
//   lEngine.UseHTTPFilter(Shutdown('apikey', 'a-strong-secret'));
//
// Pair on the host side with WaitForShutdownOrConsoleReturn (below) to
// block the main thread until the shutdown actually fires.
function Shutdown(const AAPIKeyQueryStringParamName: string;
  const AAPIKeyQueryStringParamValue: string;
  const AURLSegment: string = '/api/shutdown'): TMVCHTTPFilter;

// Blocks the calling thread until either an HTTP shutdown request fires
// (see EnterInShutdownState / IsShuttingDown in MVCFramework.Commons) or
// the user presses ENTER in the host console. Windows only — raises on
// other platforms. Use as the foreground main-loop primitive for console
// hosts paired with the Shutdown HTTPFilter.
procedure WaitForShutdownOrConsoleReturn;

// -------------------------------------------------------------------------
// Extended EndpointFilter helpers
// -------------------------------------------------------------------------

type
  // Validator callback for the lambda-friendly BasicAuth overload. Return True
  // to authenticate the caller; populate ARoles with the user's roles (used to
  // seed Context.LoggedUser.Roles). Return False to reject (the filter sends a
  // 401 with WWW-Authenticate Basic).
  TMVCBasicAuthValidator = reference to function(
    const AUserName, APassword: string;
    var ARoles: TArray<string>): Boolean;

// Basic Auth (RFC 7617) EndpointFilter. Reads `Authorization: Basic <b64>`,
// base64-decodes user:password, validates via AValidator, and on success
// populates Context.LoggedUser.UserName / Roles / Realm. On failure replies
// with 401 + `WWW-Authenticate: Basic realm="..."`.
//
//   lProtected := lGroup.Use(BasicAuth(
//     function(const U, P: string; var R: TArray<string>): Boolean
//     begin
//       Result := SameText(U, 'admin') and (P = 'secret');
//       if Result then R := ['admin'];
//     end));
function BasicAuth(
  const AValidator: TMVCBasicAuthValidator;
  const ARealm: string = 'DelphiMVCFramework REALM'): TMVCEndpointFilter; overload;

// Basic Auth EndpointFilter wrapping a full IMVCAuthenticationHandler. Replays
// the classic TMVCBasicAuthenticationMiddleware semantics (OnRequest +
// OnAuthentication + OnAuthorization, with 401 vs 403 split based on whether
// the user was authenticated) — preserves exact parity for callers porting from
// the classic middleware to the minimal-API surface. AControllerQualifiedClassName
// and AActionName passed to the handler are empty strings (minimal-API has no
// controller class / declared action name).
function BasicAuth(
  const AHandler: IMVCAuthenticationHandler;
  const ARealm: string = 'DelphiMVCFramework REALM'): TMVCEndpointFilter; overload;

// Endpoint filter: require an authenticated user (Context.LoggedUser.IsValid).
// Replies 401 (Status response) when no valid user is present. Register AFTER
// the authentication filter (JWT / BasicAuth / custom) that populates LoggedUser.
//
//   lEngine.Prefix('/admin').Use(JWT(...)).Use(Authorize);
function Authorize: TMVCEndpointFilter;

// Endpoint filter: require the authenticated user to hold a role. The array
// overload is ANY-OF (passes when the user holds at least one listed role).
// Replies 401 when unauthenticated, 403 when authenticated without the role.
//
//   lEngine.Prefix('/admin').Use(JWT(...)).Use(RequireRole('admin'));
function RequireRole(const ARole: string): TMVCEndpointFilter; overload;
function RequireRole(const ARoles: TArray<string>): TMVCEndpointFilter; overload;

// File-backed session factory EndpointFilter — sibling of MemorySession with
// the same per-group / nested-inheritance shape. Sessions survive process
// restart (data stored under ASessionFolder).
function FileSession(const ATimeoutInMinutes: Integer = 0;
  const ASessionFolder: string = 'dmvc_sessions';
  const AHttpOnly: Boolean = True;
  const ASecure: Boolean = False): TMVCEndpointFilter;

// Database-backed session factory EndpointFilter — sibling of MemorySession.
// Requires an ActiveRecord connection in scope on the same group (i.e. the
// dmvc_sessions table is read/written via TMVCActiveRecord on the ambient
// connection). Combine with ActiveRecord(...) earlier in the filter chain.
function DatabaseSession(const ATimeoutInMinutes: Integer = 0;
  const AConnectionDefName: string = '';
  const AHttpOnly: Boolean = True;
  const ASecure: Boolean = False): TMVCEndpointFilter;

// -------------------------------------------------------------------------
// Extended HTTPFilter helpers
// -------------------------------------------------------------------------

// Analytics HTTPFilter — post-Next observer. Emits one CSV-style log line per
// request:
//   <ip>;<method>;<path>;<status>;<route-name-or-empty>;<resp-len>;<host>
// When ALogWriter is nil, uses GetAnalyticsDefaultLogger (rotates CSV files
// under AppPath\analytics — same behavior as the classic Analytics middleware).
function Analytics(const ALogWriter: ILogWriter = nil): TMVCHTTPFilter;

// Trace HTTPFilter — full pre/post request/response dump via Log.Debug with
// ALogTag. Body truncated to AMaxBodySize bytes; non-text content types are
// hidden when AHideNonTextBody is True. The Authorization header is redacted
// to '<redacted>' by default; pass ALogAuthorization=True to log it raw (matches
// classic Trace middleware behavior — useful for debugging but leaks bearer
// tokens to your log files).
function Trace(const AMaxBodySize: Int64 = 4096;
  const ALogTag: string = 'trace';
  const ALogAuthorization: Boolean = False;
  const AHideNonTextBody: Boolean = True): TMVCHTTPFilter;

// Redirect HTTPFilter — pattern-based pre-Next short-circuit. Exact case-
// insensitive PathInfo match against APaths; on match returns 301 (permanent)
// or 302 (temporary) with Location: ATargetURL. Optional query string
// preservation (default True) appends `?<qs>` to the target.
//
//   lEngine.UseHTTPFilter(Redirect(['/'], '/web'));
function Redirect(const APaths: TArray<string>;
  const ATargetURL: string;
  const APermanent: Boolean = False;
  const APreserveQuery: Boolean = True): TMVCHTTPFilter; overload;

// Redirect HTTPFilter — per-rule (from, to) overload for more flexible
// rewrites. Same matching semantics as the single-target overload.
//
//   lEngine.UseHTTPFilter(Redirect([
//     TPair<string,string>.Create('/old',  '/new'),
//     TPair<string,string>.Create('/docs', 'https://docs.example.com')]));
function Redirect(const ARules: TArray<TPair<string, string>>;
  const APermanent: Boolean = False;
  const APreserveQuery: Boolean = True): TMVCHTTPFilter; overload;

// RangeMedia HTTPFilter — serves files from ADocumentRoot under AURLPath with
// HTTP Range request support (RFC 7233). Returns 206 Partial Content for
// single-range "bytes=START-END" / "bytes=-N" requests, 416 for invalid
// ranges, and falls back to 200 OK on plain GET/HEAD. Use it instead of (or
// alongside) StaticFiles whenever you serve HTML5 <audio> / <video> — those
// elements need seek support to play correctly.
//
// Always stamps `Accept-Ranges: bytes`, `Content-Encoding: identity` (so
// Compression won't re-encode the body and break Content-Range) and
// `Cache-Control: no-store` (Chromium's media element cache ignores
// no-cache). Only intercepts GET / HEAD; everything else falls through.
// Path-traversal is blocked by full-path normalization against the
// resolved document root.
//
//   lEngine.UseHTTPFilter(RangeMedia('/static/media', 'static/media'));
function RangeMedia(const AURLPath: string;
  const ADocumentRoot: string): TMVCHTTPFilter;

// -------------------------------------------------------------------------
// StaticFiles full-options overload
// -------------------------------------------------------------------------

type
  // Full StaticFiles HTTPFilter options — parity with TMVCStaticFilesMiddleware
  // (sanity check, charset, SPA fallback, MIME customization, request rules,
  // case-insensitive prefix). Build via TMVCStaticFilesOptions.Default and
  // override fields as needed.
  TMVCStaticFilesOptions = record
    Prefix: string;                                       // URL prefix (e.g. '/static')
    RootFolder: string;                                   // Filesystem root. If relative + not existing, resolved against AppPath.
    IndexDocument: string;                                // Default file for directory requests (e.g. 'index.html')
    Charset: string;                                      // Charset applied to text/* + json MIME types (e.g. 'UTF-8')
    SPAWebAppSupport: Boolean;                            // When True, falls back to index.html walking up to the nearest existing folder
    CaseSensitivePrefix: Boolean;                         // When False (default), prefix matching is case-insensitive (Windows-friendly)
    MediaTypesCustomizer: TMVCStaticFileMediaTypesCustomizer;  // Optional callback to add/override MIME types
    Rules: TMVCStaticFileRulesProc;                       // Optional per-request allow/deny + path rewrite
    class function Default: TMVCStaticFilesOptions; static;
  end;

// StaticFiles HTTPFilter — full-parity overload. Honors directory listing
// redirect (301 + trailing slash), SPA fallback, MIME customizer, request rules,
// sanity check (raises if root missing), case-insensitive prefix matching, and
// delegates actual file send to TMVCStaticContents.SendFile so If-Modified-Since
// + Last-Modified are honored.
function StaticFiles(const AOptions: TMVCStaticFilesOptions): TMVCHTTPFilter; overload;

// -------------------------------------------------------------------------
// API documentation helpers
// -------------------------------------------------------------------------

// OpenAPI HTTPFilter — exposes a freshly-built OpenAPI 3.x document at AURL
// (default '/openapi.json'). Auto-discovers minimal-API routes via the engine's
// TMVCMinimalAPIMiddleware AND classic controllers. Wraps the existing
// TMVCOpenAPI3Middleware.
//
//   var lInfo: TMVCOpenAPIInfo;
//   lInfo.Title := 'My API';
//   lInfo.Version := '1.0';
//   lEngine.UseHTTPFilter(OpenAPI(lEngine, lInfo));
function OpenAPI(AEngine: TMVCEngine; const AInfo: TMVCOpenAPIInfo;
  const AURL: string = '/openapi.json'): TMVCHTTPFilter;

// Swagger HTTPFilter — exposes the Swagger JSON document at AURL: Swagger 2.0
// by default, OpenAPI 3 with ASpecVersion = ssvOpenAPI3 (same URL, same
// attributes). Full parity wrapper around TMVCSwaggerMiddleware: auto-discovers
// JWT middleware, supports MVCSwagAuthentication / MVCSwagJSONSchemaField /
// MVCSWAGDefaultModel / MVCRequiresAuthentication / TMVCActiveRecordController
// CRUD doc generation — features the newer OpenAPI 3 helper does not yet cover.
// Pull in MVCFramework.Filters to call this; SwagDoc is transitively required.
function Swagger(AEngine: TMVCEngine;
  const AInfo: TMVCSwaggerInfo;
  const AURL: string = '/swagger.json';
  const AJWTDescription: string = JWT_DEFAULT_DESCRIPTION;
  const AEnableBasicAuthentication: Boolean = False;
  const AHost: string = '';
  const ABasePath: string = '';
  const APathFilter: string = '';
  const ATransferProtocolSchemes: TMVCTransferProtocolSchemes = [psHTTP, psHTTPS];
  const AEnableBearerAuthentication: Boolean = False;
  const ASpecVersion: TMVCSwaggerSpecVersion = ssvSwagger2): TMVCHTTPFilter;

implementation


uses
{$IF Defined(MSWINDOWS)}
  WinAPI.Windows,                   // GetStdHandle, PeekConsoleInput (WaitForShutdownOrConsoleReturn)
{$ENDIF}
  System.Diagnostics,
  System.StrUtils,
  System.SyncObjs,
  System.TypInfo,
  System.Math,                      // Min (Trace HTTPFilter)
  System.IOUtils,                   // TPath, TFile, TDirectory (StaticFiles HTTPFilter)
  System.ZLib,                      // TZCompressionStream (Compression HTTPFilter)
  System.Hash,                      // THashSHA1 (ETag HTTPFilter)
  System.DateUtils,                 // IncSecond (RateLimit HTTPFilter)
  System.NetEncoding,               // TNetEncoding.Base64 (BasicAuth helper)
  MVCFramework.Logger,              // Log.Debug, LogI, LogW (RequestLog/Trace/Shutdown)
  MVCFramework.Middleware.RateLimit,// TMVCInMemoryRateLimitStorage (RateLimit HTTPFilter)
  MVCFramework.Serializer.Commons,  // TMVCSerializerHelper.DecodeString (BasicAuth)
  MVCFramework.Session,
  MVCFramework.Session.Database,    // TMVCWebSessionDatabaseFactory (DatabaseSession)
  MVCFramework.ActiveRecord,        // ActiveRecordConnectionsRegistry (ActiveRecord filter)
  MVCFramework.Middleware.Analytics,    // GetAnalyticsDefaultLogger (Analytics helper)
  MVCFramework.Middleware.OpenAPI3,     // TMVCOpenAPI3Middleware (OpenAPI helper)
  MVCFramework.Middleware.Swagger,      // TMVCSwaggerMiddleware (Swagger helper)
  FireDAC.Comp.Client;              // FDManager (ActiveRecord filter)

const
  { Largest slice served in a single 206 response. }
  MAX_RANGE_SLICE = 8 * 1024 * 1024;

type
  // Holds the lifetime of a session factory for the MemorySession filter.
  // The filter closure captures an ISessionFactoryHolder reference; when the
  // closure is released (filter dropped at engine shutdown), the interface's
  // refcount drops to zero and the destructor frees the underlying factory.
  ISessionFactoryHolder = interface
    ['{8A0E3CD2-9F7B-4F9E-9E61-2D5B3D0B4B41}']
    function Factory: TMVCWebSessionFactory;
  end;

  TSessionFactoryHolder = class(TInterfacedObject, ISessionFactoryHolder)
  strict private
    fFactory: TMVCWebSessionFactory;
  public
    constructor Create(AFactory: TMVCWebSessionFactory);
    destructor Destroy; override;
    function Factory: TMVCWebSessionFactory;
  end;

constructor TSessionFactoryHolder.Create(AFactory: TMVCWebSessionFactory);
begin
  inherited Create;
  fFactory := AFactory;
end;

destructor TSessionFactoryHolder.Destroy;
begin
  fFactory.Free;
  inherited;
end;

function TSessionFactoryHolder.Factory: TMVCWebSessionFactory;
begin
  Result := fFactory;
end;

function MemorySession(const ATimeoutInMinutes: Integer;
  const AHttpOnly: Boolean; const ASecure: Boolean): TMVCEndpointFilter;
var
  lHolder: ISessionFactoryHolder;
begin
  // Allocate the factory and wrap it in an interface-managed holder. The
  // closure below captures `lHolder` (an interface), so the holder's lifetime
  // follows the closure's: when the filter is dropped (engine shutdown) the
  // closure is freed, the captured interface refcount drops to zero, and the
  // holder's destructor frees the factory.
  lHolder := TSessionFactoryHolder.Create(
    TMVCWebSessionMemoryFactory.Create(AHttpOnly, ATimeoutInMinutes, ASecure));
  Result :=
    function (const AContext: TWebContext;
              const ANext: TMVCEndpointFilterNext): IMVCResponse
    begin
      AContext.SetSessionFactory(lHolder.Factory);
      Result := ANext();
    end;
end;

{ -------------------------------------------------------------------------- }
{ Native filter implementations: Compression / CORS / JWT / ActiveRecord     }
{ -------------------------------------------------------------------------- }

// --- ActiveRecord ---------------------------------------------------------

var
  // Single-shot guard for FDConnectionDefs.ini load. Equivalent to the
  // gCONNECTION_DEF_FILE_LOADED flag in the classic middleware: the file
  // is parsed once per process, not on every filter construction.
  gMinimalARConnDefFileLoaded: Integer = 0;

function ActiveRecord(
  const ADefaultConnectionDefName: string): TMVCEndpointFilter;
const
  cConnectionDefFileName = 'FDConnectionDefs.ini';
var
  lInitLock: TObject;
  lInitialized: Boolean;
begin
  lInitLock := TObject.Create;
  lInitialized := False;
  Result :=
    function (const AContext: TWebContext;
              const ANext: TMVCEndpointFilterNext): IMVCResponse
    begin
      // Lazy connection-definitions load (idempotent across filter instances).
      if not lInitialized then
      begin
        TMonitor.Enter(lInitLock);
        try
          if not lInitialized then
          begin
            if TInterlocked.CompareExchange(gMinimalARConnDefFileLoaded, 1, 0) = 0 then
            begin
              FDManager.ConnectionDefFileAutoLoad := False;
              FDManager.ConnectionDefFileName := cConnectionDefFileName;
              if not FDManager.ConnectionDefFileLoaded then
                FDManager.LoadConnectionDefFile;
              if (ADefaultConnectionDefName <> '')
                and (not FDManager.IsConnectionDef(ADefaultConnectionDefName)) then
              begin
                // the full path goes to the log, the client gets the file name
                LogE('Connection definitions file: ' + FDManager.ActualConnectionDefFileName);
                raise EMVCConfigException.CreateFmt(
                  'ConnectionDefName "%s" not found in config file "%s" - or config file not present',
                  [ADefaultConnectionDefName, ExtractFileName(FDManager.ActualConnectionDefFileName)]);
              end;
            end;
            lInitialized := True;
          end;
        finally
          TMonitor.Exit(lInitLock);
        end;
      end;

      // Acquire connection BEFORE Next(), release inside try..finally so the
      // connection is returned to the pool even if the handler raises. The
      // classic IMVCMiddleware OnBeforeRouting/OnAfterRouting hooks can't
      // express this — an exception in the handler skips OnAfterRouting and
      // leaks the connection.
      if ADefaultConnectionDefName <> '' then
        ActiveRecordConnectionsRegistry.AddDefaultConnection(ADefaultConnectionDefName);
      try
        Result := ANext();
      finally
        if ADefaultConnectionDefName <> '' then
          ActiveRecordConnectionsRegistry.RemoveDefaultConnection(False);
      end;
    end;
end;

// --- StaticFiles HTTPFilter ----------------------------------------------

function DetectStaticMimeType(const AFile: string): string;
var
  lExt: string;
begin
  lExt := ExtractFileExt(AFile).ToLower;
  if (lExt = '.html') or (lExt = '.htm') then Exit('text/html; charset=utf-8');
  if lExt = '.css'  then Exit('text/css; charset=utf-8');
  if lExt = '.js'   then Exit('application/javascript; charset=utf-8');
  if lExt = '.json' then Exit('application/json; charset=utf-8');
  if lExt = '.svg'  then Exit('image/svg+xml');
  if lExt = '.png'  then Exit('image/png');
  if (lExt = '.jpg') or (lExt = '.jpeg') then Exit('image/jpeg');
  if lExt = '.gif'  then Exit('image/gif');
  if lExt = '.webp' then Exit('image/webp');
  if lExt = '.ico'  then Exit('image/x-icon');
  if lExt = '.woff' then Exit('font/woff');
  if lExt = '.woff2' then Exit('font/woff2');
  if lExt = '.ttf'  then Exit('font/ttf');
  if lExt = '.pdf'  then Exit('application/pdf');
  if lExt = '.txt'  then Exit('text/plain; charset=utf-8');
  if lExt = '.xml'  then Exit('application/xml');
  Result := 'application/octet-stream';
end;

function StaticFiles(const APrefix: string; const ARootFolder: string;
  const ADefaultDocument: string): TMVCHTTPFilter;
begin
  Result :=
    procedure (const AContext: TWebContext;
               const ANext: TMVCHTTPFilterNext)
    var
      lPath, lSuffix, lFile, lAbsRoot, lAbsFile, lMime: string;
      lStream: TFileStream;
    begin
      lPath := AContext.Request.PathInfo;

      // Only act on requests inside the prefix. Exact match or sub-path.
      if (lPath <> APrefix) and (not lPath.StartsWith(APrefix + '/')) then
      begin
        ANext();
        Exit;
      end;

      lSuffix := Copy(lPath, Length(APrefix) + 1, MaxInt).TrimLeft(['/']);

      // Default document for directory-style requests (e.g. /static -> /static/index.html).
      if (lSuffix = '') and (ADefaultDocument <> '') then
        lSuffix := ADefaultDocument;

      // Reject path traversal up front. The canonicalization check below is
      // the real defense, but rejecting obvious patterns lets us return 403
      // without touching the filesystem.
      if lSuffix.Contains('..') or (Pos(':', lSuffix) > 0) then
      begin
        AContext.Response.StatusCode := 403;
        Exit;
      end;

      lFile := TPath.Combine(ARootFolder, lSuffix.Replace('/', PathDelim));

      // Canonicalize and ensure the file stays inside the root folder.
      // GetFullPath collapses '..' segments; if the result escapes the root,
      // the request is rejected.
      lAbsRoot := IncludeTrailingPathDelimiter(TPath.GetFullPath(ARootFolder));
      lAbsFile := TPath.GetFullPath(lFile);
      if not lAbsFile.StartsWith(lAbsRoot, True) then
      begin
        AContext.Response.StatusCode := 403;
        Exit;
      end;

      if not TFile.Exists(lAbsFile) then
      begin
        // File doesn't exist under our prefix. Let routing try to match —
        // there may be a lambda handler for the same path. If nothing
        // matches the request will end as a 404 anyway.
        ANext();
        Exit;
      end;

      lMime := DetectStaticMimeType(lAbsFile);
      lStream := TFileStream.Create(lAbsFile, fmOpenRead or fmShareDenyWrite);
      AContext.Response.StatusCode := 200;
      AContext.Response.SetContentStream(lStream, lMime);
      // No ANext() — short-circuit: routing / handlers don't run for this request.
    end;
end;

// --- Compression ----------------------------------------------------------

// Picks the first supported encoding from a comma-separated Accept-Encoding
// header. Returns ctGZIP, ctDeflate, or ctNone. gzip wins if both are listed
// because every modern client supports it and the framing is unambiguous.
function PickCompressionEncoding(const AAcceptEncoding: string): TMVCCompressionType;
var
  lLower: string;
  lTokens: TArray<string>;
  lToken: string;
begin
  Result := TMVCCompressionType.ctNone;
  if AAcceptEncoding = '' then
    Exit;
  lLower := AAcceptEncoding.Trim.ToLower;
  lTokens := lLower.Split([',']);
  // First pass: prefer gzip (most universally interoperable).
  for lToken in lTokens do
    if lToken.Trim = 'gzip' then
      Exit(TMVCCompressionType.ctGZIP);
  for lToken in lTokens do
    if lToken.Trim = 'deflate' then
      Exit(TMVCCompressionType.ctDeflate);
end;

function Compression(const ACompressionThreshold: Integer): TMVCHTTPFilter;
begin
  Result :=
    procedure (const AContext: TWebContext;
               const ANext: TMVCHTTPFilterNext)
    var
      lContentStream: TStream;
      lMemStream: TMemoryStream;
      lZStream: TZCompressionStream;
      lEncoding: TMVCCompressionType;
    begin
      // Run the inner pipeline first. Any ContentStream we want to compress
      // is only populated after routing + handler + render have completed.
      ANext();

      // ISAPI: the host process handles compression at a different layer.
      if IsLibrary then
        Exit;

      // Respect an encoding already chosen downstream. RangeMedia stamps
      // 'Content-Encoding: identity' on partial responses precisely so we do not
      // re-encode and corrupt Content-Range. Re-compressing would also emit
      // contradictory encoding headers. Skip if any Content-Encoding is already
      // set, via either the typed property or a custom header.
      if (AContext.Response.ContentEncoding <> '') or
         (AContext.Response.GetCustomHeader('Content-Encoding') <> '') then
        Exit;

      lContentStream := AContext.Response.ContentStream;
      if (lContentStream = nil) or (lContentStream.Size <= ACompressionThreshold) then
        Exit;

      lEncoding := PickCompressionEncoding(AContext.Request.Headers['Accept-Encoding']);
      if lEncoding = TMVCCompressionType.ctNone then
        Exit;

      // Compress into a fresh memory stream then swap it in. Cannot mutate
      // the original stream in place because TFileStream (e.g. from
      // StaticFiles) is read-only.
      lMemStream := TMemoryStream.Create;
      try
        lZStream := TZCompressionStream.Create(lMemStream,
          TZCompressionLevel.zcMax,
          MVC_COMPRESSION_ZLIB_WINDOW_BITS[lEncoding]);
        try
          lContentStream.Position := 0;
          lZStream.CopyFrom(lContentStream, 0);
        finally
          lZStream.Free;
        end;
      except
        lMemStream.Free;
        raise;
      end;
      lMemStream.Position := 0;
      AContext.Response.InternalSetContentStream(lMemStream, True);
      AContext.Response.ContentEncoding := MVC_COMPRESSION_TYPE_AS_STRING[lEncoding];
    end;
end;

// --- ETag -----------------------------------------------------------------

function ETag: TMVCHTTPFilter;
begin
  Result :=
    procedure (const AContext: TWebContext;
               const ANext: TMVCHTTPFilterNext)
    var
      lContentStream: TStream;
      lDigest, lQuotedETag, lIfNoneMatch: string;
      lEmpty: TMemoryStream;
    begin
      ANext();

      // Only validate 2xx responses; redirects/errors don't carry a stable
      // representation worth caching.
      if (AContext.Response.StatusCode < 200) or (AContext.Response.StatusCode >= 300) then
        Exit;

      lContentStream := AContext.Response.ContentStream;
      if (lContentStream = nil) or (lContentStream.Size = 0) then
        Exit;

      lContentStream.Position := 0;
      lDigest := THashSHA1.GetHashString(lContentStream);
      lQuotedETag := '"' + lDigest + '"';
      AContext.Response.SetCustomHeader('ETag', lQuotedETag);

      lIfNoneMatch := AContext.Request.Headers['If-None-Match'];
      if lIfNoneMatch = '' then
        Exit;

      // Compare allowing the optional weak prefix "W/" that some clients
      // send back even for strong ETags. Compare both quoted forms.
      if SameText(lIfNoneMatch, lQuotedETag) or SameText(lIfNoneMatch, 'W/' + lQuotedETag) then
      begin
        // 304 Not Modified: empty body, keep the ETag header on the way out.
        lEmpty := TMemoryStream.Create;
        AContext.Response.InternalSetContentStream(lEmpty, True);
        AContext.Response.StatusCode := 304;
      end;
    end;
end;

// --- SecurityHeaders -----------------------------------------------------

function SecurityHeaders: TMVCHTTPFilter;
begin
  Result :=
    procedure (const AContext: TWebContext;
               const ANext: TMVCHTTPFilterNext)
    begin
      // '0' per OWASP: the legacy XSS auditor was itself exploitable
      AContext.Response.SetCustomHeader('X-XSS-Protection', '0');
      AContext.Response.SetCustomHeader('X-Content-Type-Options', 'nosniff');
      ANext();
    end;
end;

// --- Request guards -------------------------------------------------------

function MVCNormalizePath(const APath: string): string;
var
  lPos: Integer;
begin
  Result := APath;
  lPos := Result.IndexOf('?');
  if lPos >= 0 then
    Result := Result.Substring(0, lPos);
  Result := Result.Trim;
  if Result = '' then
    Exit;
  if not Result.StartsWith('/') then
    Result := '/' + Result;
  Result := Result.ToLower.TrimRight(['/']);
end;

// A malformed %-sequence must not make a guard raise on a path that has
// nothing to do with it.
function SafeURLDecodePath(const APath: string): string;
begin
  try
    Result := TNetEncoding.URL.Decode(APath);
  except
    Result := '';
  end;
end;

// The match itself, with the prefix already normalised (ASlashPrefix = prefix + '/'):
// the guards prepare both once, when the filter is built, not on every request.
// A form of the path is tested only when it can differ from one already tested
// (PathInfo equal to RawPathInfo, no '%' to decode).
function PathMatchesNormalized(const AContext: TWebContext;
  const ANormPrefix, ASlashPrefix: string; out AExact: Boolean): Boolean;
var
  lRaw, lPathInfo: string;
  lAnyMatch, lAllExact: Boolean;

  procedure Test(const ARawCandidate: string);
  var
    lCandidate: string;
  begin
    lCandidate := MVCNormalizePath(ARawCandidate);
    if (lCandidate = ANormPrefix) or lCandidate.StartsWith(ASlashPrefix) then
    begin
      lAnyMatch := True;
      if lCandidate <> ANormPrefix then
        lAllExact := False;
    end;
  end;

begin
  AExact := False;
  if ANormPrefix = '' then
    Exit(True);
  lAnyMatch := False;
  lAllExact := True;
  lRaw := AContext.Request.RawPathInfo;
  lPathInfo := AContext.Request.PathInfo;
  Test(lRaw);
  if lPathInfo <> lRaw then
    Test(lPathInfo);
  if lRaw.IndexOf('%') >= 0 then
    Test(SafeURLDecodePath(lRaw));
  Result := lAnyMatch;
  AExact := lAnyMatch and lAllExact;
end;

function MVCRequestTargetsPath(const AContext: TWebContext;
  const APathPrefix: string; out AExact: Boolean): Boolean;
var
  lPrefix: string;
begin
  lPrefix := MVCNormalizePath(APathPrefix);
  Result := PathMatchesNormalized(AContext, lPrefix, lPrefix + '/', AExact);
end;

// Same length is not secret, the content is: no early exit on the first
// different character.
function SameSecret(const A, B: string): Boolean;
var
  I, lDiff: Integer;
begin
  if Length(A) <> Length(B) then
    Exit(False);
  lDiff := 0;
  for I := 1 to Length(A) do
    lDiff := lDiff or (Ord(A[I]) xor Ord(B[I]));
  Result := lDiff = 0;
end;

function StaticBearerTokens(const ATokens: TArray<TPair<string, string>>): TMVCBearerValidator;
var
  lTokens: TArray<TPair<string, string>>;
  lToken: TPair<string, string>;
begin
  lTokens := Copy(ATokens);
  for lToken in lTokens do
  begin
    if lToken.Key.Trim.IsEmpty then
      raise EMVCException.Create('A bearer token needs a name: it becomes the logged user');
    if lToken.Value.Trim.IsEmpty then
      raise EMVCException.CreateFmt('The bearer token "%s" is empty', [lToken.Key]);
    if lToken.Value <> lToken.Value.Trim then
      raise EMVCException.CreateFmt('The bearer token "%s" has leading or trailing ' +
        'whitespace; a client token is trimmed, so it could never match', [lToken.Key]);
  end;
  Result :=
    function (const AToken: string; out AUserName: string;
              var ARoles: TArray<string>): Boolean
    var
      lItem: TPair<string, string>;
    begin
      Result := False;
      AUserName := '';
      // every token is compared: the time does not tell which one matched
      for lItem in lTokens do
        if SameSecret(AToken, lItem.Value) and not Result then
        begin
          AUserName := lItem.Key;
          Result := True;
        end;
    end;
end;

function BearerAuthFilter(const AValidator: TMVCBearerValidator;
  const APathPrefix: string): TMVCHTTPFilter;
var
  lPrefix, lSlashPrefix: string;
begin
  lPrefix := MVCNormalizePath(APathPrefix);
  lSlashPrefix := lPrefix + '/';
  Result :=
    procedure (const AContext: TWebContext;
               const ANext: TMVCHTTPFilterNext)
    var
      lExact: Boolean;
      lAuth, lToken, lUserName: string;
      lRoles: TArray<string>;
    begin
      if not PathMatchesNormalized(AContext, lPrefix, lSlashPrefix, lExact) then
      begin
        ANext();
        Exit;
      end;
      lAuth := AContext.Request.Headers['Authorization'];
      lToken := '';
      if lAuth.StartsWith('Bearer ', True) then
        lToken := lAuth.Substring(7).Trim;
      lUserName := '';
      lRoles := nil;
      // an accepted token must name a user: LoggedUser without a name is "nobody"
      if lToken.IsEmpty or not AValidator(lToken, lUserName, lRoles) or lUserName.IsEmpty then
      begin
        AContext.Response.SetCustomHeader('WWW-Authenticate', 'Bearer');
        AContext.Response.StatusCode := 401;
        Exit; // no Next() - short-circuit
      end;
      AContext.LoggedUser.UserName := lUserName;
      AContext.LoggedUser.LoggedSince := Now;
      AContext.LoggedUser.Roles.Clear;
      AContext.LoggedUser.Roles.AddRange(lRoles);
      ANext();
    end;
end;

function InListIgnoreCase(const AValue: string; const AList: TArray<string>): Boolean;
var
  lItem: string;
begin
  for lItem in AList do
    if SameText(AValue, lItem) then
      Exit(True);
  Result := False;
end;

function TrimmedNonEmpty(const AValues: TArray<string>): TArray<string>;
var
  lValue: string;
begin
  Result := nil;
  for lValue in AValues do
    if not lValue.Trim.IsEmpty then
      Result := Result + [lValue.Trim];
end;

function AllowedOriginFilter(const AOrigins: TArray<string>;
  const APathPrefix: string): TMVCHTTPFilter;
var
  lOrigins: TArray<string>;
  lPrefix, lSlashPrefix: string;
begin
  lOrigins := TrimmedNonEmpty(AOrigins);
  lPrefix := MVCNormalizePath(APathPrefix);
  lSlashPrefix := lPrefix + '/';
  Result :=
    procedure (const AContext: TWebContext;
               const ANext: TMVCHTTPFilterNext)
    var
      lExact: Boolean;
      lOrigin: string;
    begin
      if PathMatchesNormalized(AContext, lPrefix, lSlashPrefix, lExact) then
      begin
        lOrigin := AContext.Request.Headers['Origin'];
        if (lOrigin <> '') and not InListIgnoreCase(lOrigin, lOrigins) then
        begin
          AContext.Response.StatusCode := 403;
          Exit; // no Next() - short-circuit
        end;
      end;
      ANext();
    end;
end;

// ':' followed by digits only
function IsPortSuffix(const ASuffix: string): Boolean;
var
  I: Integer;
begin
  Result := (Length(ASuffix) > 1) and (ASuffix[1] = ':');
  if Result then
    for I := 2 to Length(ASuffix) do
      if (ASuffix[I] < '0') or (ASuffix[I] > '9') then
        Exit(False);
end;

// The host of a Host header without its port. Only host[:port] and [ipv6][:port]
// are Hosts; anything else (`localhost:80@evil`, `[::1]evil`, a bare IPv6 literal)
// gives '', which is in no list.
function HostWithoutPort(const AHost: string): string;
var
  lPos: Integer;
  lRest: string;
begin
  Result := AHost.Trim;
  if Result.StartsWith('[') then // [::1]:8080
  begin
    lPos := Result.IndexOf(']');
    if lPos < 0 then
      Exit('');
    lRest := Result.Substring(lPos + 1);
    if (lRest <> '') and not IsPortSuffix(lRest) then
      Exit('');
    Exit(Result.Substring(0, lPos + 1));
  end;
  lPos := Result.IndexOf(':');
  if lPos < 0 then
    Exit;
  if (lPos <> Result.LastIndexOf(':')) or not IsPortSuffix(Result.Substring(lPos)) then
    Exit('');
  Result := Result.Substring(0, lPos);
end;

function AllowedHostFilter(const AHosts: TArray<string>;
  const APathPrefix: string): TMVCHTTPFilter;
var
  lHosts: TArray<string>;
  lPrefix, lSlashPrefix: string;
begin
  lHosts := TrimmedNonEmpty(AHosts) + ['localhost', '127.0.0.1', '[::1]'];
  lPrefix := MVCNormalizePath(APathPrefix);
  lSlashPrefix := lPrefix + '/';
  Result :=
    procedure (const AContext: TWebContext;
               const ANext: TMVCHTTPFilterNext)
    var
      lExact: Boolean;
    begin
      if PathMatchesNormalized(AContext, lPrefix, lSlashPrefix, lExact) and
         not InListIgnoreCase(HostWithoutPort(AContext.Request.Headers['Host']), lHosts) then
      begin
        AContext.Response.StatusCode := 403;
        Exit; // no Next() - short-circuit
      end;
      ANext();
    end;
end;

function AllowedMethodsFilter(const AMethods: TMVCHTTPMethods;
  const APath: string): TMVCHTTPFilter;
var
  lPrefix, lSlashPrefix, lAllow: string;
  lMethod: TMVCHTTPMethodType;
begin
  lPrefix := MVCNormalizePath(APath);
  if lPrefix = '' then
    raise EMVCException.Create('AllowedMethodsFilter needs a path: '''' would limit the whole application');
  lSlashPrefix := lPrefix + '/';
  lAllow := '';
  for lMethod := Low(TMVCHTTPMethodType) to High(TMVCHTTPMethodType) do
    if lMethod in AMethods then
    begin
      if lAllow <> '' then
        lAllow := lAllow + ', ';
      lAllow := lAllow + GetEnumName(TypeInfo(TMVCHTTPMethodType), Ord(lMethod)).Substring(4);
    end;
  Result :=
    procedure (const AContext: TWebContext;
               const ANext: TMVCHTTPFilterNext)
    var
      lExact: Boolean;
    begin
      if not PathMatchesNormalized(AContext, lPrefix, lSlashPrefix, lExact) then
      begin
        ANext();
        Exit;
      end;
      if not lExact or not (AContext.Request.HTTPMethod in AMethods) then
      begin
        AContext.Response.SetCustomHeader('Allow', lAllow);
        AContext.Response.StatusCode := HTTP_STATUS.MethodNotAllowed;
        Exit; // no Next() - short-circuit
      end;
      ANext();
    end;
end;

// --- Shutdown ------------------------------------------------------------

function Shutdown(const AAPIKeyQueryStringParamName: string;
  const AAPIKeyQueryStringParamValue: string;
  const AURLSegment: string): TMVCHTTPFilter;
var
  lURLSegment: string;
  lParamName: string;
  lParamValue: string;
begin
  // Snapshot args — closure owns its own copies.
  lURLSegment := AURLSegment;
  lParamName := AAPIKeyQueryStringParamName;
  lParamValue := AAPIKeyQueryStringParamValue;
  Result :=
    procedure (const AContext: TWebContext;
               const ANext: TMVCHTTPFilterNext)
    var
      lPathInfo: string;
    begin
      lPathInfo := AContext.Request.PathInfo;
      // Case-insensitive prefix match — POST only.
      if lPathInfo.StartsWith(lURLSegment, True)
        and (AContext.Request.HTTPMethod = httpPOST) then
      begin
        if AContext.Request.QueryStringParam(lParamName) = lParamValue then
        begin
          LogW('Shutdown HTTPFilter intercepted a shutdown POST request at ' + lURLSegment);
          AContext.Response.Content := 'Shutting down';
          AContext.Response.StatusCode := 200;
          EnterInShutdownState;
          Exit; // short-circuit — no Next()
        end;
      end;
      ANext();
    end;
end;

{$IF Defined(MSWINDOWS)}
function ShutdownFilterIsEnterPressed: Boolean;
var
  lNumberOfEvents, lNumberOfEventsRead: DWORD;
  lBuffer: TInputRecord;
  lStdHandle: THandle;
begin
  Result := False;
  lStdHandle := GetStdHandle(STD_INPUT_HANDLE);
  lNumberOfEvents := 0;
  GetNumberOfConsoleInputEvents(lStdHandle, lNumberOfEvents);
  if lNumberOfEvents = 0 then
    Exit;
  PeekConsoleInput(lStdHandle, lBuffer, 1, lNumberOfEventsRead);
  if lNumberOfEventsRead = 0 then
    Exit;
  if lBuffer.EventType = KEY_EVENT then
  begin
    if lBuffer.Event.KeyEvent.bKeyDown
      and (lBuffer.Event.KeyEvent.wVirtualKeyCode = VK_RETURN) then
      Result := True;
  end;
  FlushConsoleInputBuffer(lStdHandle);
end;

procedure WaitForShutdownOrConsoleReturn;
begin
  while not (IsShuttingDown or ShutdownFilterIsEnterPressed) do
    Sleep(500);
  LogI('Shutting down...');
  WriteLn('Shutting down...');
end;
{$ELSE}
procedure WaitForShutdownOrConsoleReturn;
begin
  raise Exception.Create('WaitForShutdownOrConsoleReturn is available only on MSWindows');
end;
{$ENDIF}

// --- IPBlock --------------------------------------------------------------

function IPBlock(const ABlockedIPs: TArray<string>): TMVCHTTPFilter;
var
  lBlocked: TArray<string>;
begin
  // Copy the array into a local variable so the closure captures a snapshot,
  // not the caller's mutable storage. Cheap and small (IP lists are tiny).
  lBlocked := Copy(ABlockedIPs);
  Result :=
    procedure (const AContext: TWebContext;
               const ANext: TMVCHTTPFilterNext)
    var
      lIP, lPeer, lBan: string;
    begin
      // the socket peer as well: ClientIp follows headers the client writes
      lIP := AContext.Request.ClientIp;
      lPeer := AContext.Request.PeerIp;
      for lBan in lBlocked do
        if SameText(lIP, lBan) or SameText(lPeer, lBan) then
        begin
          AContext.Response.StatusCode := 403;
          Exit; // no Next() — short-circuit
        end;
      ANext();
    end;
end;

// --- RateLimit ------------------------------------------------------------

function RateLimit(const AMaxRequests: Integer;
  const AWindowSeconds: Integer): TMVCHTTPFilter;
var
  lStorage: IMVCRateLimitStorage;
begin
  // Delegate to the shared in-memory storage, which windows the counter AND
  // evicts expired keys under its own lock. The previous inline dictionary
  // never removed entries, so requests carrying a spoofable per-request key
  // (e.g. a forged X-Forwarded-For behind no proxy) grew memory without bound.
  // The interface is captured by the closure and released when the filter is
  // dropped at engine shutdown.
  lStorage := TMVCInMemoryRateLimitStorage.Create(rlsFixedWindow);

  Result :=
    procedure (const AContext: TWebContext;
               const ANext: TMVCHTTPFilterNext)
    var
      lRemaining: Integer;
      lResetTime: TDateTime;
    begin
      if lStorage.CheckRateLimit(AContext.Request.ClientIp, AMaxRequests,
        AWindowSeconds, lRemaining, lResetTime) then
      begin
        AContext.Response.SetCustomHeader('Retry-After', IntToStr(AWindowSeconds));
        AContext.Response.StatusCode := 429;
        Exit; // short-circuit, no Next()
      end;
      ANext();
    end;
end;

// --- RequestLog -----------------------------------------------------------

function RequestLog: TMVCHTTPFilter;
begin
  Result :=
    procedure (const AContext: TWebContext;
               const ANext: TMVCHTTPFilterNext)
    var
      lStart: TStopwatch;
    begin
      lStart := TStopwatch.StartNew;
      try
        ANext();
      finally
        lStart.Stop;
        LogI(Format('[HTTP] %s %s %s -> %d (%dms)', [
          AContext.Request.ClientIp,
          GetEnumName(TypeInfo(TMVCHTTPMethodType), Ord(AContext.Request.HTTPMethod)),
          AContext.Request.PathInfo,
          AContext.Response.StatusCode,
          lStart.ElapsedMilliseconds]));
      end;
    end;
end;

// --- CORS (HTTPFilter version) --------------------------------------------

// Forward — defined later under "CORS (EndpointFilter)" because the
// original classic CORS() helper also calls it. Single source of truth.
function SplitCORSOrigins(const AConfiguredOrigins: string): TArray<string>; forward;
procedure StampSimpleCORSHeaders(const AContext: TWebContext;
  const AAllowedOrigins: TArray<string>;
  const AAllowsCredentials: Boolean;
  const AExposeHeaders: string); forward;

function CORSFilter(
  const AAllowedOriginURLs: string;
  const AAllowsCredentials: Boolean;
  const AExposeHeaders: string;
  const AAllowsHeaders: string;
  const AAllowsMethods: string;
  const AAccessControlMaxAge: Integer): TMVCHTTPFilter;
var
  lOrigins: TArray<string>;
begin
  lOrigins := SplitCORSOrigins(AAllowedOriginURLs);
  Result :=
    procedure (const AContext: TWebContext;
               const ANext: TMVCHTTPFilterNext)
    begin
      // Preflight: short-circuit OPTIONS with the full CORS header set +
      // 200 OK and empty body. Routing/handlers never run — important for
      // OPTIONS to a path with no matching route (would otherwise 404).
      if AContext.Request.HTTPMethod = httpOPTIONS then
      begin
        StampSimpleCORSHeaders(AContext,
          lOrigins, AAllowsCredentials, AExposeHeaders);
        AContext.Response.SetCustomHeader('Access-Control-Allow-Methods', AAllowsMethods);
        AContext.Response.SetCustomHeader('Access-Control-Allow-Headers', AAllowsHeaders);
        AContext.Response.SetCustomHeader('Access-Control-Max-Age', IntToStr(AAccessControlMaxAge));
        AContext.Response.StatusCode := 200;
        Exit; // no Next() — short-circuit
      end;

      // Non-preflight: forward, then stamp the simple headers in a
      // try..finally so they land on success AND on exception-derived
      // 500 responses (browser still needs CORS headers to surface the
      // error to the page).
      try
        ANext();
      finally
        StampSimpleCORSHeaders(AContext,
          lOrigins, AAllowsCredentials, AExposeHeaders);
      end;
    end;
end;

// --- CORS (EndpointFilter) ------------------------------------------------

// Stamps the "simple" CORS response headers (origin, credentials, expose).
// Extracted to a module-level proc because anonymous methods cannot capture
// nested procedures (E2555). The closure inside CORS() calls it twice — for
// the preflight branch and the post-handler branch.
function SplitCORSOrigins(const AConfiguredOrigins: string): TArray<string>;
var
  i: Integer;
begin
  Result := AConfiguredOrigins.Split([',']);
  for i := 0 to High(Result) do
    Result[i] := Result[i].Trim;
end;

// AAllowedOrigins already split and trimmed (SplitCORSOrigins): the filters
// do it once at registration, not on every request.
function MatchCORSOrigin(const AAllowedOrigins: TArray<string>;
  const ARequestOrigin: string): string;
var
  lAllowed: string;
begin
  Result := '';
  if ARequestOrigin = '' then
    Exit;
  for lAllowed in AAllowedOrigins do
  begin
    if lAllowed = '*' then
      Exit('*');
    if SameText(ARequestOrigin, lAllowed) then
      Exit(lAllowed);
  end;
end;

function MVCMatchCORSOrigin(const AConfiguredOrigins, ARequestOrigin: string): string;
begin
  Result := MatchCORSOrigin(SplitCORSOrigins(AConfiguredOrigins), ARequestOrigin);
end;

procedure StampSimpleCORSHeaders(const AContext: TWebContext;
  const AAllowedOrigins: TArray<string>;
  const AAllowsCredentials: Boolean;
  const AExposeHeaders: string);
var
  lAllowOrigin: string;
begin
  // Reflect the request Origin against the configured list instead of emitting
  // the whole (possibly multi-value) configured string verbatim, which is an
  // invalid header and, with '*' + credentials, the forbidden CORS combo.
  lAllowOrigin := MatchCORSOrigin(AAllowedOrigins, AContext.Request.Headers['Origin']);
  if lAllowOrigin <> '' then
  begin
    AContext.Response.SetCustomHeader('Access-Control-Allow-Origin', lAllowOrigin);
    // The ACAO value depends on the request Origin, so caches must vary on it.
    // Appended: the handler may already vary on something of its own.
    if (lAllowOrigin <> '*') and
      (Pos('Origin', AContext.Response.GetCustomHeader('Vary')) = 0) then
      AContext.Response.SetCustomHeader('Vary',
        Trim((AContext.Response.GetCustomHeader('Vary') + ', Origin').TrimLeft([',', ' '])));
  end;
  if MVCCORSAllowsCredentials(AAllowsCredentials, lAllowOrigin) then
    AContext.Response.SetCustomHeader('Access-Control-Allow-Credentials', 'true');
  if AExposeHeaders <> '' then
    AContext.Response.SetCustomHeader('Access-Control-Expose-Headers', AExposeHeaders);
end;

function CORS(
  const AAllowedOriginURLs: string;
  const AAllowsCredentials: Boolean;
  const AExposeHeaders: string;
  const AAllowsHeaders: string;
  const AAllowsMethods: string;
  const AAccessControlMaxAge: Integer): TMVCEndpointFilter;
var
  lOrigins: TArray<string>;
begin
  lOrigins := SplitCORSOrigins(AAllowedOriginURLs);
  Result :=
    function (const AContext: TWebContext;
              const ANext: TMVCEndpointFilterNext): IMVCResponse
    var
      lResp: TMVCResponse;
    begin
      // Preflight: short-circuit OPTIONS with the full CORS header set,
      // 200 OK, empty body. The handler never runs.
      if AContext.Request.HTTPMethod = httpOPTIONS then
      begin
        StampSimpleCORSHeaders(AContext,
          lOrigins, AAllowsCredentials, AExposeHeaders);
        AContext.Response.SetCustomHeader('Access-Control-Allow-Methods', AAllowsMethods);
        AContext.Response.SetCustomHeader('Access-Control-Allow-Headers', AAllowsHeaders);
        AContext.Response.SetCustomHeader('Access-Control-Max-Age', IntToStr(AAccessControlMaxAge));
        lResp := TMVCResponse.Create;
        lResp.StatusCode := http_status.OK;
        Exit(lResp);
      end;

      // Non-preflight: forward, then stamp the simple CORS headers on the
      // response. Use a try..finally so headers are stamped even if the
      // handler raises (the framework's exception handler still writes a
      // response — we want the browser to see CORS headers on the 500).
      try
        Result := ANext();
      finally
        StampSimpleCORSHeaders(AContext,
          lOrigins, AAllowsCredentials, AExposeHeaders);
      end;
    end;
end;

// --- JWT ------------------------------------------------------------------

// Module-level helper extracted from JWT() so the closure can call it
// (anonymous methods cannot capture nested procedures — E2555).
function BuildJWTLoginResponse(const AContext: TWebContext;
  const AUserName: string;
  const ARolesList: TList<string>;
  const ASessionData: TDictionary<string, string>;
  const AClaimsSetup: TJWTClaimsSetup;
  const ASecret, AHMACAlgorithm: string;
  const ALeewaySeconds: Cardinal): IMVCResponse;
var
  lJWT: TJWT;
  lToken: string;
  lResp: TMVCResponse;
  lKey: string;
begin
  lJWT := TJWT.Create(ASecret, ALeewaySeconds);
  try
    lJWT.HMACAlgorithm := AHMACAlgorithm;
    // Apply caller-provided baseline claims (issuer, audience, expiry).
    if Assigned(AClaimsSetup) then
      AClaimsSetup(lJWT);
    // LoggedUser.IsValid needs a login time: "iat" unless the setup gave one
    if lJWT.Claims.IssuedAt <= 0 then // an absent claim reads as 0001-01-01, not 0
      lJWT.Claims.IssuedAt := Now;
    // Custom claims: roles + session data set by the auth handler.
    // All roles go into ONE comma-joined 'roles' claim (the framework's standard
    // shape, read back via Split(',')). The previous per-iteration assignment
    // overwrote the claim each loop and kept only the last role.
    // 'username' + 'roles': the claims TMVCJWTAuthenticationMiddleware writes and
    // reads, so a token from either side fills LoggedUser on the other
    lJWT.CustomClaims.Items['username'] := AUserName;
    if ARolesList.Count > 0 then
      lJWT.CustomClaims.Items['roles'] := string.Join(',', ARolesList.ToArray);
    if Assigned(ASessionData) then
      for lKey in ASessionData.Keys do
        lJWT.CustomClaims.Items[lKey] := ASessionData[lKey];
    lToken := lJWT.GetToken;
  finally
    lJWT.Free;
  end;
  lResp := TMVCResponse.Create;
  lResp.StatusCode := http_status.OK;
  AContext.Response.SetCustomHeader('Authorization', 'bearer ' + lToken);
  Result := lResp;
end;

function JWT(
  const AAuthenticationHandler: IMVCAuthenticationHandler;
  const AClaimsSetup: TJWTClaimsSetup;
  const ASecret: string;
  const ALoginURLSegment: string;
  const AClaimsToCheck: TJWTCheckableClaims;
  const ALeewaySeconds: Cardinal;
  const AHMACAlgorithm: string): TMVCEndpointFilter;
const
  cAuthHeader      = 'Authorization';
  cBearerPrefix    = 'bearer ';
  cUserNameHeader  = 'jwtusername';
  cPasswordHeader  = 'jwtpassword';
begin
  // Fail while building the filter, not on the first forged request
  // (GHSA-hgv7-ch4w-2f47).
  CheckJWTSecret(ASecret);
  Result :=
    function (const AContext: TWebContext;
              const ANext: TMVCEndpointFilterNext): IMVCResponse
    var
      lAuthHeader, lToken, lError: string;
      lJWT: TJWT;
      lAccepted: Boolean;
      lRoles: TList<string>;
      lSession: TSessionData;
      lUserName, lPassword: string;
    begin
      // Login endpoint: special-case credential exchange -> token.
      // POST /login with jwtusername + jwtpassword headers returns a token.
      if SameText(AContext.Request.PathInfo, ALoginURLSegment)
        and (AContext.Request.HTTPMethod = httpPOST) then
      begin
        lUserName := AContext.Request.Headers[cUserNameHeader];
        lPassword := AContext.Request.Headers[cPasswordHeader];
        if (lUserName = '') or (lPassword = '') then
          Exit(Status(http_status.Unauthorized, 'Missing credentials'));
        lRoles := TList<string>.Create;
        lSession := TSessionData.Create;
        try
          lAccepted := False;
          AAuthenticationHandler.OnAuthentication(AContext,
            lUserName, lPassword, lRoles, lAccepted, lSession);
          if not lAccepted then
            Exit(Status(http_status.Unauthorized, 'Invalid credentials'));
          Result := BuildJWTLoginResponse(AContext, lUserName, lRoles, lSession,
            AClaimsSetup, ASecret, AHMACAlgorithm, ALeewaySeconds);
        finally
          lRoles.Free;
          lSession.Free;
        end;
        Exit;
      end;

      // All other endpoints: require a valid bearer token.
      lAuthHeader := AContext.Request.Headers[cAuthHeader];
      if not lAuthHeader.ToLower.StartsWith(cBearerPrefix) then
        Exit(Status(http_status.Unauthorized, 'Missing or malformed bearer token'));
      lToken := Copy(lAuthHeader, Length(cBearerPrefix) + 1, MaxInt).Trim;

      lJWT := TJWT.Create(ASecret, ALeewaySeconds);
      try
        lJWT.HMACAlgorithm := AHMACAlgorithm;
        lJWT.RegClaimsToChecks := AClaimsToCheck;
        if not lJWT.LoadToken(lToken, lError) then
          Exit(Status(http_status.Unauthorized, 'Invalid token: ' + lError));
        // Make claims available to the handler via the framework's
        // LoggedUser slot — same surface controller-based apps use.
        // Filled as TMVCJWTAuthenticationMiddleware does, so Authorize / RequireRole
        // work behind JWT(...). "sub" stays a fallback for tokens issued elsewhere.
        AContext.LoggedUser.Clear;
        AContext.LoggedUser.UserName := lJWT.CustomClaims['username'];
        if AContext.LoggedUser.UserName = '' then
          AContext.LoggedUser.UserName := lJWT.Claims.Subject;
        if lJWT.CustomClaims['roles'] <> '' then
          AContext.LoggedUser.Roles.AddRange(lJWT.CustomClaims['roles'].Split([',']));
        if lJWT.Claims.IssuedAt > 0 then
          AContext.LoggedUser.LoggedSince := lJWT.Claims.IssuedAt
        else // a valid token with no "iat" (it reads as 0001-01-01): IsValid needs a time
          AContext.LoggedUser.LoggedSince := Now;
        AContext.LoggedUser.CustomData := lJWT.CustomClaims.AsCustomData;
      finally
        lJWT.Free;
      end;

      Result := ANext();
    end;
end;

// --- BasicAuth (EndpointFilter) ------------------------------------------

function Build401WithWWWAuthenticate(const AContext: TWebContext;
  const ARealm: string): IMVCResponse;
begin
  // Stamp WWW-Authenticate directly on the live response — TMVCResponse.Create
  // initializes fHeaders to nil and the engine's render path does not
  // lazy-instantiate it, so writing via .Headers AVs.
  AContext.Response.SetCustomHeader('WWW-Authenticate',
    'Basic realm=' + QuotedStr(ARealm));
  Result := Status(http_status.Unauthorized);
end;

function Build403Forbidden: IMVCResponse;
begin
  Result := Status(http_status.Forbidden);
end;

function ParseBasicAuthHeader(const AHeader: string;
  out AUserName, APassword: string): Boolean;
var
  lToken, lDecoded: string;
  lParts: TArray<string>;
begin
  Result := False;
  if AHeader.IsEmpty or (not AHeader.StartsWith('Basic ', True)) then
    Exit;
  lToken := AHeader.Remove(0, 'Basic '.Length).Trim;
  // Use the framework's decoder for consistency with the classic middleware.
  lDecoded := TMVCSerializerHelper.DecodeString(lToken);
  lParts := lDecoded.Split([':']);
  if Length(lParts) <> 2 then
    Exit;
  AUserName := lParts[0];
  APassword := lParts[1];
  Result := True;
end;

function BasicAuth(const AValidator: TMVCBasicAuthValidator;
  const ARealm: string): TMVCEndpointFilter;
begin
  Result :=
    function (const AContext: TWebContext;
              const ANext: TMVCEndpointFilterNext): IMVCResponse
    var
      lUserName, lPassword: string;
      lRoles: TArray<string>;
    begin
      if not ParseBasicAuthHeader(AContext.Request.Headers['Authorization'],
        lUserName, lPassword) then
        Exit(Build401WithWWWAuthenticate(AContext, ARealm));

      lRoles := nil;
      if not AValidator(lUserName, lPassword, lRoles) then
        Exit(Build401WithWWWAuthenticate(AContext, ARealm));

      AContext.LoggedUser.UserName := lUserName;
      AContext.LoggedUser.LoggedSince := Now;
      AContext.LoggedUser.Realm := ARealm;
      AContext.LoggedUser.Roles.Clear;
      AContext.LoggedUser.Roles.AddRange(lRoles);
      Result := ANext();
    end;
end;

function BasicAuth(const AHandler: IMVCAuthenticationHandler;
  const ARealm: string): TMVCEndpointFilter;
begin
  Result :=
    function (const AContext: TWebContext;
              const ANext: TMVCEndpointFilterNext): IMVCResponse
    var
      lAuthRequired, lIsValid, lIsAuthorized: Boolean;
      lUserName, lPassword: string;
      lRoles: TList<string>;
      lSession: TSessionData;
      lPair: TPair<string, string>;
    begin
      // Match classic TMVCBasicAuthenticationMiddleware sequence exactly so
      // callers porting an IMVCAuthenticationHandler get identical semantics.
      // Empty strings stand in for ControllerQualifiedClassName / ActionName —
      // minimal-API has no controller class. Handlers that gate on those will
      // see them as empty and should treat that as "every action requires auth"
      // (this filter is per-group; presence on the group implies "auth required").
      { Deny by default: the parameter is `var`, not `out`, so a handler that
        leaves a branch unassigned would have this read stack garbage - and
        False here skips authentication altogether. }
      lAuthRequired := True;
      AHandler.OnRequest(AContext, '', '', lAuthRequired);
      if not lAuthRequired then
        Exit(ANext());

      // Pull cached creds from session if available — mirrors classic line 209.
      // Guard: minimal-API may have no session filter in the chain; touching the
      // session then raises EMVCConfigException. Without a session we just skip
      // the cache — creds are re-read from the Authorization header each request.
      if AContext.HasSessionSupport then
        AContext.LoggedUser.LoadFromSession(AContext.Session);
      lIsValid := AContext.LoggedUser.IsValid;
      if not lIsValid then
      begin
        if not ParseBasicAuthHeader(AContext.Request.Headers['Authorization'],
          lUserName, lPassword) then
          Exit(Build401WithWWWAuthenticate(AContext, ARealm));

        lRoles := TList<string>.Create;
        try
          lSession := TSessionData.Create;
          try
            AHandler.OnAuthentication(AContext, lUserName, lPassword,
              lRoles, lIsValid, lSession);
            if lIsValid then
            begin
              AContext.LoggedUser.Roles.AddRange(lRoles);
              AContext.LoggedUser.UserName := lUserName;
              AContext.LoggedUser.LoggedSince := Now;
              AContext.LoggedUser.Realm := ARealm;
              // Only persist to the session when one is configured (see guard above).
              if AContext.HasSessionSupport then
              begin
                AContext.SessionRegenerateId; // against session fixation, as the classic middleware
                AContext.LoggedUser.SaveToSession(AContext.Session);
                for lPair in lSession do
                  AContext.Session[lPair.Key] := lPair.Value;
              end;
            end;
          finally
            lSession.Free;
          end;
        finally
          lRoles.Free;
        end;
      end;

      lIsAuthorized := False;
      if lIsValid then
        AHandler.OnAuthorization(AContext, AContext.LoggedUser.Roles,
          '', '', lIsAuthorized);

      if lIsAuthorized then
        Exit(ANext());

      // Mirror classic split (line 261-266): 403 if user is identified but lacks
      // authorization, 401 (challenge again) if not even identified.
      if lIsValid then
        Result := Build403Forbidden
      else
        Result := Build401WithWWWAuthenticate(AContext, ARealm);
    end;
end;

// --- Authorize / RequireRole (EndpointFilter) ----------------------------

function Authorize: TMVCEndpointFilter;
begin
  Result :=
    function (const Ctx: TWebContext;
              const Next: TMVCEndpointFilterNext): IMVCResponse
    begin
      if not Ctx.LoggedUser.IsValid then
        Exit(Status(http_status.Unauthorized, 'Authentication required'));
      Result := Next();
    end;
end;

function RequireRole(const ARoles: TArray<string>): TMVCEndpointFilter;
begin
  Result :=
    function (const Ctx: TWebContext;
              const Next: TMVCEndpointFilterNext): IMVCResponse
    var
      lRole: string;
      lHas: Boolean;
    begin
      if not Ctx.LoggedUser.IsValid then
        Exit(Status(http_status.Unauthorized, 'Authentication required'));
      lHas := False;
      for lRole in ARoles do
        if Ctx.LoggedUser.Roles.Contains(lRole) then
        begin
          lHas := True;
          Break;
        end;
      if not lHas then
        Exit(Status(http_status.Forbidden, 'Insufficient privileges'));
      Result := Next();
    end;
end;

function RequireRole(const ARole: string): TMVCEndpointFilter;
begin
  Result := RequireRole(TArray<string>.Create(ARole));
end;

// --- FileSession (EndpointFilter) ----------------------------------------

function FileSession(const ATimeoutInMinutes: Integer;
  const ASessionFolder: string;
  const AHttpOnly: Boolean; const ASecure: Boolean): TMVCEndpointFilter;
var
  lHolder: ISessionFactoryHolder;
begin
  lHolder := TSessionFactoryHolder.Create(
    TMVCWebSessionFileFactory.Create(AHttpOnly, ATimeoutInMinutes, ASessionFolder, ASecure));
  Result :=
    function (const AContext: TWebContext;
              const ANext: TMVCEndpointFilterNext): IMVCResponse
    begin
      AContext.SetSessionFactory(lHolder.Factory);
      Result := ANext();
    end;
end;

// --- DatabaseSession (EndpointFilter) ------------------------------------

function DatabaseSession(const ATimeoutInMinutes: Integer;
  const AConnectionDefName: string;
  const AHttpOnly: Boolean; const ASecure: Boolean): TMVCEndpointFilter;
var
  lHolder: ISessionFactoryHolder;
begin
  // AConnectionDefName is currently inert in TMVCWebSessionDatabaseFactory
  // (it relies on the ambient ActiveRecord default connection) — kept on the
  // signature for forward compatibility and to document the dependency on
  // ActiveRecord(...) earlier in the filter chain.
  lHolder := TSessionFactoryHolder.Create(
    TMVCWebSessionDatabaseFactory.Create(AHttpOnly, ATimeoutInMinutes, AConnectionDefName, ASecure));
  Result :=
    function (const AContext: TWebContext;
              const ANext: TMVCEndpointFilterNext): IMVCResponse
    begin
      AContext.SetSessionFactory(lHolder.Factory);
      Result := ANext();
    end;
end;

// --- Analytics (HTTPFilter) ----------------------------------------------

const
  ANALYTICS_TAG = 'analytics';

var
  // Lazy default logger cache — analytics CSV appender shared across every
  // Analytics() filter built without an explicit ALogWriter. Initialised on
  // first use, kept until process exit.
  gAnalyticsLogger: ILogWriter = nil;
  gAnalyticsLoggerLock: TObject = nil;

function GetSharedAnalyticsLogger: ILogWriter;
begin
  if gAnalyticsLogger = nil then
  begin
    TMonitor.Enter(gAnalyticsLoggerLock);
    try
      if gAnalyticsLogger = nil then
        // Delegate to the classic middleware's well-tested CSV file appender
        // (LoggerProSimpleFileAppender writing under AppPath + 'analytics').
        gAnalyticsLogger := GetAnalyticsDefaultLogger;
    finally
      TMonitor.Exit(gAnalyticsLoggerLock);
    end;
  end;
  Result := gAnalyticsLogger;
end;

function Analytics(const ALogWriter: ILogWriter): TMVCHTTPFilter;
const
  // Map HTTP status class to log level: 1xx/2xx/3xx -> Info, 4xx -> Warning,
  // 5xx -> Error. Mirrors LOG_LEVEL[] in the classic Analytics middleware.
  LOG_LEVEL: array [1..5] of TLogType = (
    TLogType.Info, TLogType.Info, TLogType.Info,
    TLogType.Warning, TLogType.Error);
var
  lConfigured: ILogWriter;
begin
  lConfigured := ALogWriter; // captured by closure
  Result :=
    procedure (const AContext: TWebContext;
               const ANext: TMVCHTTPFilterNext)
    var
      lReq: TMVCWebRequest;
      lResp: TMVCWebResponse;
      lEffective: ILogWriter;
      lAction: string;
    begin
      try
        ANext();
      finally
        lEffective := lConfigured;
        if lEffective = nil then
          lEffective := GetSharedAnalyticsLogger;
        lReq := AContext.Request;
        lResp := AContext.Response;
        // Column layout matches the classic Analytics middleware:
        //   ip ; method ; path ; status ; <action> ; resp-len ; host
        // The <action> column carries the FQ controller+action in classic; in
        // HTTPFilter mode there is no controller class, so we substitute the
        // PathInfo as the most useful action identifier (callers parsing the
        // CSV can still rely on column count, and the value is meaningful).
        lAction := lReq.PathInfo;
        lEffective.Log(LOG_LEVEL[Max(1, Min(5, lResp.StatusCode div 100))],
          lReq.ClientIp + ';' +
          lReq.Method + ';' +
          lReq.PathInfo + ';' +
          lResp.StatusCode.ToString + ';' +
          lAction + ';' +
          lResp.ContentLength.ToString + ';' +
          lReq.Host,
          ANALYTICS_TAG);
      end;
    end;
end;

// --- Trace (HTTPFilter) --------------------------------------------------

function TraceIsTextContentType(const AContentType: string): Boolean;
var
  lLower: string;
begin
  lLower := AContentType.ToLower;
  Result :=
    lLower.StartsWith(TMVCMediaType.APPLICATION_JSON, True) or
    lLower.StartsWith(TMVCMediaType.APPLICATION_XML, True) or
    lLower.StartsWith(TMVCMediaType.APPLICATION_FORM_URLENCODED, True) or
    lLower.StartsWith('text/');
end;

function Trace(const AMaxBodySize: Int64;
  const ALogTag: string;
  const ALogAuthorization: Boolean;
  const AHideNonTextBody: Boolean): TMVCHTTPFilter;
begin
  Result :=
    procedure (const AContext: TWebContext;
               const ANext: TMVCHTTPFilterNext)
    var
      lSW: TStopwatch;
      lReq: TMVCWebRequest;
      lAuth, lBody: string;
      lDump: TStringStream;
    begin
      // Pre-Next: REQUEST -------------------------------------------------
      AContext.Request.ReadTotalContent;
      lReq := AContext.Request;
      if ALogAuthorization then
        lAuth := lReq.Authorization
      else if lReq.Authorization = '' then
        lAuth := ''
      else
        lAuth := '<redacted>';
      Log.Debug(Format('[TRACE][REQ][%s][IP:%s][URL:%s][QS:%s][LEN:%d][ACCEPT:%s][UA:%s][AUTH:%s]', [
        lReq.HTTPMethodAsString, lReq.ClientIp, lReq.PathInfo,
        lReq.QueryFieldsDelimitedText, lReq.ContentLength,
        lReq.Accept, lReq.UserAgent, lAuth]), ALogTag);

      if lReq.HTTPMethod in [httpPOST, httpPUT, httpPATCH] then
      begin
        if (not AHideNonTextBody) or
           TraceIsTextContentType(lReq.Headers['content-type']) then
          lBody := TEncoding.UTF8.GetString(lReq.RawContent)
                     .Substring(0, AMaxBodySize)
        else
          lBody := '<hidden non text content>';
        Log.Debug('[TRACE][REQ][BODY] ' + lBody, ALogTag);
      end;

      // Inner pipeline ----------------------------------------------------
      lSW := TStopwatch.StartNew;
      try
        ANext();
      finally
        lSW.Stop;

        // Post-Next: RESPONSE ---------------------------------------------
        Log.Debug(Format('[TRACE][RES][STATUS] %d %s (%dms)', [
          AContext.Response.StatusCode,
          AContext.Response.ReasonString,
          lSW.ElapsedMilliseconds]), ALogTag);
        Log.Debug('[TRACE][RES][HEADERS] ' + string.Join(' | ',
          AContext.Response.CustomHeaders.ToStringArray), ALogTag);
        Log.Debug('[TRACE][RES][CT] ' + AContext.Response.ContentType, ALogTag);

        if (not AHideNonTextBody) or
           TraceIsTextContentType(AContext.Response.ContentType) then
        begin
          lDump := TStringStream.Create;
          try
            if Assigned(AContext.Response.ContentStream) then
            begin
              AContext.Response.ContentStream.Position := 0;
              lDump.CopyFrom(AContext.Response.ContentStream,
                Min(AContext.Response.ContentStream.Size, AMaxBodySize));
              AContext.Response.ContentStream.Position := 0;
            end
            else
              lDump.WriteString(AContext.Response.Content.Substring(0, AMaxBodySize));
            Log.Debug('[TRACE][RES][BODY] ' + lDump.DataString, ALogTag);
          finally
            lDump.Free;
          end;
        end
        else
          Log.Debug('[TRACE][RES][BODY] <hidden non text content>', ALogTag);
      end;
    end;
end;

// --- Redirect (HTTPFilter) -----------------------------------------------

function BuildRedirectResponse(const AContext: TWebContext;
  const ATarget: string;
  const APermanent, APreserveQuery: Boolean): Boolean;
var
  lTarget, lQS: string;
begin
  lTarget := ATarget;
  if APreserveQuery then
  begin
    lQS := AContext.Request.QueryString;
    if lQS <> '' then
      lTarget := lTarget + '?' + lQS;
  end;
  AContext.Response.CustomHeaders.Values['Location'] := lTarget;
  if APermanent then
    AContext.Response.StatusCode := http_status.MovedPermanently
  else
    AContext.Response.StatusCode := http_status.Found;
  LogI(Format('Redirect [%s] -> [%s]', [AContext.Request.PathInfo, lTarget]));
  Result := True;
end;

function Redirect(const APaths: TArray<string>;
  const ATargetURL: string;
  const APermanent: Boolean;
  const APreserveQuery: Boolean): TMVCHTTPFilter;
var
  lPaths: TArray<string>;
begin
  lPaths := Copy(APaths);
  Result :=
    procedure (const AContext: TWebContext;
               const ANext: TMVCHTTPFilterNext)
    var
      lPath, lMatch: string;
    begin
      lPath := AContext.Request.PathInfo;
      for lMatch in lPaths do
        if SameText(lPath, lMatch) then
        begin
          BuildRedirectResponse(AContext, ATargetURL, APermanent, APreserveQuery);
          Exit; // short-circuit
        end;
      ANext();
    end;
end;

function Redirect(const ARules: TArray<TPair<string, string>>;
  const APermanent: Boolean;
  const APreserveQuery: Boolean): TMVCHTTPFilter;
var
  lRules: TArray<TPair<string, string>>;
begin
  lRules := Copy(ARules);
  Result :=
    procedure (const AContext: TWebContext;
               const ANext: TMVCHTTPFilterNext)
    var
      lPath: string;
      lRule: TPair<string, string>;
    begin
      lPath := AContext.Request.PathInfo;
      for lRule in lRules do
        if SameText(lPath, lRule.Key) then
        begin
          BuildRedirectResponse(AContext, lRule.Value, APermanent, APreserveQuery);
          Exit;
        end;
      ANext();
    end;
end;

// --- RangeMedia ----------------------------------------------------------

type
  // Per-filter state for RangeMedia. Wrapped in an interface so the closure
  // owns it via a reference-counted ref, freeing the dictionary at engine
  // shutdown.
  IRangeMediaState = interface
    ['{2E5A7B8C-4A13-4E62-9F2A-7C8D1E4B6F02}']
    function URLPath: string;
    function DocumentRoot: string;
    function MediaTypes: TDictionary<string, string>;
  end;

  TRangeMediaState = class(TInterfacedObject, IRangeMediaState)
  strict private
    fURLPath: string;
    fDocumentRoot: string;
    fMediaTypes: TDictionary<string, string>;
    procedure InitMediaTypes;
  public
    constructor Create(const AURLPath, ADocumentRoot: string);
    destructor Destroy; override;
    function URLPath: string;
    function DocumentRoot: string;
    function MediaTypes: TDictionary<string, string>;
  end;

constructor TRangeMediaState.Create(const AURLPath, ADocumentRoot: string);
begin
  inherited Create;
  fURLPath := AURLPath.TrimRight(['/']);
  fDocumentRoot := TPath.Combine(AppPath, ADocumentRoot);
  // Trailing separator makes the StartsWith check unambiguous —
  // '/app/media/' must not match the sibling '/app/media_evil/'.
  if not fDocumentRoot.EndsWith(TPath.DirectorySeparatorChar) then
    fDocumentRoot := fDocumentRoot + TPath.DirectorySeparatorChar;
  fMediaTypes := TDictionary<string, string>.Create;
  InitMediaTypes;
end;

destructor TRangeMediaState.Destroy;
begin
  fMediaTypes.Free;
  inherited;
end;

procedure TRangeMediaState.InitMediaTypes;
begin
  // Audio
  fMediaTypes.Add('.m4a',  'audio/mp4');
  fMediaTypes.Add('.mp3',  'audio/mpeg');
  fMediaTypes.Add('.ogg',  'audio/ogg');
  fMediaTypes.Add('.opus', 'audio/opus');
  fMediaTypes.Add('.wav',  'audio/wav');
  fMediaTypes.Add('.flac', 'audio/flac');
  fMediaTypes.Add('.aac',  'audio/aac');
  fMediaTypes.Add('.weba', 'audio/webm');
  // Video
  fMediaTypes.Add('.mp4',  'video/mp4');
  fMediaTypes.Add('.webm', 'video/webm');
  fMediaTypes.Add('.ogv',  'video/ogg');
  fMediaTypes.Add('.mkv',  'video/x-matroska');
  fMediaTypes.Add('.avi',  'video/x-msvideo');
  fMediaTypes.Add('.mov',  'video/quicktime');
end;

function TRangeMediaState.URLPath: string;
begin
  Result := fURLPath;
end;

function TRangeMediaState.DocumentRoot: string;
begin
  Result := fDocumentRoot;
end;

function TRangeMediaState.MediaTypes: TDictionary<string, string>;
begin
  Result := fMediaTypes;
end;

function RangeMediaResolveFilePath(const AState: IRangeMediaState;
  const APathInfo: string; out AFileName: string): Boolean;
var
  lRelativePath, lFullPath: string;
begin
  Result := False;
  AFileName := '';
  lRelativePath := APathInfo.Substring(AState.URLPath.Length).TrimLeft(['/']);
  if lRelativePath.IsEmpty then
    Exit;
  lRelativePath := lRelativePath.Replace('/', TPath.DirectorySeparatorChar);
  lFullPath := TPath.GetFullPath(TPath.Combine(AState.DocumentRoot, lRelativePath));
  // Directory-traversal defense — DocumentRoot has trailing PathDelim.
  if not lFullPath.StartsWith(AState.DocumentRoot, True) then
    Exit;
  if not TFile.Exists(lFullPath) then
    Exit;
  AFileName := lFullPath;
  Result := True;
end;

function RangeMediaGetMediaType(const AState: IRangeMediaState;
  const AFileName: string): string;
var
  lExt: string;
begin
  lExt := LowerCase(TPath.GetExtension(AFileName));
  if not AState.MediaTypes.TryGetValue(lExt, Result) then
    Result := 'application/octet-stream';
end;

function RangeMediaParseHeader(const ARangeHeader: string;
  const AFileSize: Int64; out ARangeStart, ARangeEnd: Int64): Boolean;
var
  lBytesSpec, lStartStr, lEndStr: string;
  lDashPos: Integer;
begin
  Result := False;
  ARangeStart := 0;
  ARangeEnd := AFileSize - 1;
  if not ARangeHeader.StartsWith('bytes=', True) then
    Exit;
  lBytesSpec := ARangeHeader.Substring(6).Trim;
  if lBytesSpec.Contains(',') then
    Exit; // multi-range not supported
  lDashPos := lBytesSpec.IndexOf('-');
  if lDashPos < 0 then
    Exit;
  lStartStr := lBytesSpec.Substring(0, lDashPos).Trim;
  lEndStr := lBytesSpec.Substring(lDashPos + 1).Trim;
  if lStartStr.IsEmpty and lEndStr.IsEmpty then
    Exit;
  if lStartStr.IsEmpty then
  begin
    // "bytes=-N" suffix range — last N bytes.
    if not TryStrToInt64(lEndStr, ARangeEnd) then
      Exit;
    ARangeStart := Max(0, AFileSize - ARangeEnd);
    ARangeEnd := AFileSize - 1;
  end
  else
  begin
    if not TryStrToInt64(lStartStr, ARangeStart) then
      Exit;
    if not lEndStr.IsEmpty then
    begin
      if not TryStrToInt64(lEndStr, ARangeEnd) then
        Exit;
    end
    else
      ARangeEnd := AFileSize - 1;
  end;
  ARangeEnd := Min(ARangeEnd, AFileSize - 1);
  Result := (ARangeStart >= 0) and (ARangeStart <= ARangeEnd) and
    (ARangeEnd < AFileSize);
end;

function RangeMedia(const AURLPath: string;
  const ADocumentRoot: string): TMVCHTTPFilter;
var
  lState: IRangeMediaState;
begin
  lState := TRangeMediaState.Create(AURLPath, ADocumentRoot);
  Result :=
    procedure (const AContext: TWebContext;
               const ANext: TMVCHTTPFilterNext)
    var
      lPathInfo, lFileName, lRangeHeader, lContentType: string;
      lFileStream: TFileStream;
      lPartialStream: TMemoryStream;
      lFileSize, lRangeStart, lRangeEnd, lContentLength: Int64;
    begin
      lPathInfo := AContext.Request.PathInfo;

      { On a segment boundary, like the middleware twin: a bare prefix match
        makes /media swallow /mediation/report, and this filter never calls
        ANext once it has decided the request is its own. }
      if (not SameText(lPathInfo, lState.URLPath)) and
        (not lPathInfo.StartsWith(lState.URLPath + '/', True)) then
      begin
        ANext();
        Exit;
      end;

      if not (AContext.Request.HTTPMethod in [httpGET, httpHEAD]) then
      begin
        ANext();
        Exit;
      end;

      // Bare '/media' with no filename — leave it to the router.
      if lPathInfo.Substring(lState.URLPath.Length).TrimLeft(['/']).IsEmpty then
      begin
        ANext();
        Exit;
      end;

      // We own this request from here on — never call Next().
      if not RangeMediaResolveFilePath(lState, lPathInfo, lFileName) then
      begin
        AContext.Response.StatusCode := HTTP_STATUS.NotFound;
        Exit;
      end;

      lContentType := RangeMediaGetMediaType(lState, lFileName);
      lFileStream := TFileStream.Create(lFileName, fmOpenRead or fmShareDenyNone);
      try
        lFileSize := lFileStream.Size;
        lRangeHeader := AContext.Request.Headers['Range'];

        AContext.Response.SetCustomHeader('Accept-Ranges',    'bytes');
        // Prevent Compression (if wrapped outside) from re-encoding the
        // body — would break Content-Range semantics.
        AContext.Response.SetCustomHeader('Content-Encoding', 'identity');
        // Chromium media cache ignores no-cache for <video>/<audio>.
        AContext.Response.SetCustomHeader('Cache-Control',    'no-store');

        if lRangeHeader.IsEmpty then
        begin
          AContext.Response.StatusCode := HTTP_STATUS.OK;
          lFileStream.Position := 0;
          AContext.Response.SetContentStream(lFileStream, lContentType);
          lFileStream := nil; // ownership transferred — guard finally
          Exit;
        end;

        if not RangeMediaParseHeader(lRangeHeader, lFileSize,
          lRangeStart, lRangeEnd) then
        begin
          AContext.Response.StatusCode := HTTP_STATUS.RequestedRangeNotSatisfiable;
          AContext.Response.SetCustomHeader('Content-Range',
            'bytes */' + IntToStr(lFileSize));
          Exit;
        end;

        // 206 Partial Content — slice into a fresh memory stream and hand
        // it off; the response sink owns it from here.
        { Same cap as the RangeMedia middleware: see MAX_RANGE_SLICE there.
          "Range: bytes=0-" is a whole-file allocation without it. }
        if (lRangeEnd - lRangeStart + 1) > MAX_RANGE_SLICE then
          lRangeEnd := lRangeStart + MAX_RANGE_SLICE - 1;
        lContentLength := lRangeEnd - lRangeStart + 1;
        lPartialStream := TMemoryStream.Create;
        try
          lPartialStream.SetSize(lContentLength);
          lFileStream.Position := lRangeStart;
          lPartialStream.CopyFrom(lFileStream, lContentLength);
          lPartialStream.Position := 0;
        except
          lPartialStream.Free;
          raise;
        end;

        AContext.Response.StatusCode := HTTP_STATUS.PartialContent;
        AContext.Response.SetCustomHeader('Content-Range',
          Format('bytes %d-%d/%d', [lRangeStart, lRangeEnd, lFileSize]));
        AContext.Response.SetContentStream(lPartialStream, lContentType);
      finally
        lFileStream.Free;
      end;
    end;
end;

// --- StaticFiles (options-record overload) -------------------------------

class function TMVCStaticFilesOptions.Default: TMVCStaticFilesOptions;
begin
  Result.Prefix := '/static';
  Result.RootFolder := '.\www';
  Result.IndexDocument := 'index.html';
  Result.Charset := '';
  Result.SPAWebAppSupport := True;
  Result.CaseSensitivePrefix := False;
  Result.MediaTypesCustomizer := nil;
  Result.Rules := nil;
end;

procedure AddDefaultStaticMediaTypes(const AMap: TMVCStringDictionary);
begin
  // Mirrors TMVCStaticFilesMiddleware.AddMediaTypes line 134-155 — same 18
  // entries the classic ships with so callers porting from it see no MIME
  // regressions.
  AMap.Add('.html', TMVCMediaType.TEXT_HTML + ';charset=' + TMVCCharSet.UTF_8);
  AMap.Add('.htm',  TMVCMediaType.TEXT_HTML + ';charset=' + TMVCCharSet.UTF_8);
  AMap.Add('.txt',  TMVCMediaType.TEXT_PLAIN + ';charset=' + TMVCCharSet.US_ASCII);
  AMap.Add('.text', TMVCMediaType.TEXT_PLAIN + ';charset=' + TMVCCharSet.US_ASCII);
  AMap.Add('.csv',  TMVCMediaType.TEXT_CSV + ';charset=' + TMVCCharSet.UTF_8);
  AMap.Add('.css',  TMVCMediaType.TEXT_CSS + ';charset=' + TMVCCharSet.UTF_8);
  AMap.Add('.js',   TMVCMediaType.TEXT_JAVASCRIPT + ';charset=' + TMVCCharSet.UTF_8);
  AMap.Add('.json', TMVCMediaType.APPLICATION_JSON + ';charset=' + TMVCCharSet.UTF_8);
  AMap.Add('.jpg',  TMVCMediaType.IMAGE_JPEG);
  AMap.Add('.jpeg', TMVCMediaType.IMAGE_JPEG);
  AMap.Add('.jpe',  TMVCMediaType.IMAGE_JPEG);
  AMap.Add('.png',  TMVCMediaType.IMAGE_PNG);
  AMap.Add('.ico',  TMVCMediaType.IMAGE_X_ICON);
  AMap.Add('.appcache', TMVCMediaType.TEXT_CACHEMANIFEST);
  AMap.Add('.svg',  TMVCMediaType.IMAGE_SVG_XML);
  AMap.Add('.svgz', TMVCMediaType.IMAGE_SVG_XML + ';charset=' + TMVCCharSet.UTF_8);
  AMap.Add('.xml',  TMVCMediaType.TEXT_XML + ';charset=' + TMVCCharSet.UTF_8);
  AMap.Add('.pdf',  TMVCMediaType.APPLICATION_PDF);
  AMap.Add('.gif',  TMVCMediaType.IMAGE_GIF);
  AMap.Add('.webp', 'image/webp');
  AMap.Add('.woff', 'font/woff');
  AMap.Add('.woff2','font/woff2');
  AMap.Add('.ttf',  'font/ttf');
end;

type
  // Per-filter state (built once, captured by the closure). Wraps the MIME
  // table so its lifetime follows the closure's via an interface holder.
  IStaticFilesState = interface
    ['{1A1F8A4C-7B26-4F71-A1F2-9B9A4C7E2F30}']
    function Options: TMVCStaticFilesOptions;
    function MediaTypes: TMVCStringDictionary;
    function ResolvedRoot: string;
    function NormalizedPrefix: string;     // always ends with '/'
    procedure RunSanityCheckOnce;
  end;

  TStaticFilesState = class(TInterfacedObject, IStaticFilesState)
  strict private
    fOptions: TMVCStaticFilesOptions;
    fMediaTypes: TMVCStringDictionary;
    fResolvedRoot: string;
    fNormalizedPrefix: string;
    fSanityCheckDone: Integer;
  public
    constructor Create(const AOptions: TMVCStaticFilesOptions);
    destructor Destroy; override;
    function Options: TMVCStaticFilesOptions;
    function MediaTypes: TMVCStringDictionary;
    function ResolvedRoot: string;
    function NormalizedPrefix: string;
    procedure RunSanityCheckOnce;
  end;

constructor TStaticFilesState.Create(const AOptions: TMVCStaticFilesOptions);
begin
  inherited Create;
  fOptions := AOptions;
  fSanityCheckDone := 0;

  fNormalizedPrefix := AOptions.Prefix.Trim;
  if not fNormalizedPrefix.EndsWith('/') then
    fNormalizedPrefix := fNormalizedPrefix + '/';

  // Resolve root: absolute existing path wins as-is, otherwise combine with
  // AppPath. Same fallback as TMVCStaticFilesMiddleware.Create line 171-178.
  if TDirectory.Exists(AOptions.RootFolder) then
    fResolvedRoot := TPath.GetFullPath(AOptions.RootFolder)
  else
    fResolvedRoot := TPath.GetFullPath(TPath.Combine(AppPath, AOptions.RootFolder));

  fMediaTypes := TMVCStringDictionary.Create;
  AddDefaultStaticMediaTypes(fMediaTypes);
  if Assigned(AOptions.MediaTypesCustomizer) then
    AOptions.MediaTypesCustomizer(fMediaTypes);
end;

destructor TStaticFilesState.Destroy;
begin
  fMediaTypes.Free;
  inherited;
end;

function TStaticFilesState.Options: TMVCStaticFilesOptions;
begin
  Result := fOptions;
end;

function TStaticFilesState.MediaTypes: TMVCStringDictionary;
begin
  Result := fMediaTypes;
end;

function TStaticFilesState.ResolvedRoot: string;
begin
  Result := fResolvedRoot;
end;

function TStaticFilesState.NormalizedPrefix: string;
begin
  Result := fNormalizedPrefix;
end;

procedure TStaticFilesState.RunSanityCheckOnce;
begin
  if TInterlocked.CompareExchange(fSanityCheckDone, 1, 0) = 0 then
  begin
    if not fOptions.Prefix.StartsWith('/') then
      raise EMVCException.Create(
        'StaticFiles Prefix must begin with "/" and cannot be empty');
    if not TDirectory.Exists(fResolvedRoot) then
      raise EMVCException.CreateFmt(
        'StaticFiles RootFolder [%s] is not a valid directory', [fResolvedRoot]);
  end;
end;

function StaticFilesSendFileWithMime(const AContext: TWebContext;
  const AFileName: string;
  const AMediaTypes: TMVCStringDictionary;
  const ACharset: string): Boolean;
var
  lContentType: string;
begin
  Result := False;
  if not TFile.Exists(AFileName) then
    Exit;
  if AMediaTypes.TryGetValue(LowerCase(ExtractFileExt(AFileName)), lContentType) then
    lContentType := BuildContentType(lContentType, ACharset)
  else
    lContentType := BuildContentType(TMVCMediaType.APPLICATION_OCTETSTREAM, '');
  // Delegate to TMVCStaticContents.SendFile so If-Modified-Since /
  // Last-Modified are honored (same as the classic middleware).
  TMVCStaticContents.SendFile(AFileName, lContentType, AContext);
  Result := True;
end;

function StaticFiles(const AOptions: TMVCStaticFilesOptions): TMVCHTTPFilter;
var
  lState: IStaticFilesState;
begin
  lState := TStaticFilesState.Create(AOptions);
  Result :=
    procedure (const AContext: TWebContext;
               const ANext: TMVCHTTPFilterNext)
    var
      lPathInfo, lFullPathInfo, lRelative, lFileName: string;
      lAllow: Boolean;
      lIsStaticFile: Boolean;
      lIsDirectoryTraversalAttack: Boolean;
      lOpts: TMVCStaticFilesOptions;
    begin
      lPathInfo := AContext.Request.PathInfo;
      lOpts := lState.Options;

      // Prefix match. Case-insensitive by default (Windows-friendly); strict
      // when CaseSensitivePrefix=True. The trailing-/ tweak mirrors classic
      // line 254-267: a folder request missing the trailing slash should still
      // match the prefix so we can issue the 301 below.
      if not lPathInfo.StartsWith(lState.NormalizedPrefix, not lOpts.CaseSensitivePrefix) then
      begin
        if (not lPathInfo.EndsWith('/'))
          and (lPathInfo + '/').StartsWith(lState.NormalizedPrefix, not lOpts.CaseSensitivePrefix) then
          lPathInfo := lPathInfo + '/'
        else
        begin
          ANext();
          Exit;
        end;
      end;

      // Rules callback — allow the caller to allow/deny/rewrite the request
      // before we touch the filesystem (parity with classic line 270-279).
      if Assigned(lOpts.Rules) then
      begin
        lAllow := True;
        lOpts.Rules(AContext, lPathInfo, lAllow);
        if not lAllow then
          Exit; // handled by rules, short-circuit
      end;

      // Sanity check — lazy on first matching request so the engine boots even
      // if the root folder doesn't exist yet (classic behavior, line 295-298).
      lState.RunSanityCheckOnce;

      // Compute filesystem path under the resolved root.
      lRelative := lPathInfo;
      if lRelative.StartsWith(lState.NormalizedPrefix,
        not lOpts.CaseSensitivePrefix) then
        lRelative := lRelative.Remove(0, lState.NormalizedPrefix.Length);
      lRelative := lRelative.Replace('/', PathDelim, [rfReplaceAll]);
      if lRelative.StartsWith(PathDelim) then
        lRelative := lRelative.Remove(0, 1);
      { A %00 survives URL decoding as a real #0 and TPath.Combine raises on it
        before any check below runs. Same guard as the middleware twin. }
      if not TPath.HasValidPathChars(lRelative, False) then
      begin
        AContext.Response.StatusCode := HTTP_STATUS.NotFound;
        Exit;
      end;
      lFullPathInfo := TPath.Combine(lState.ResolvedRoot, lRelative);

      // Direct file hit + path-traversal defense via TMVCStaticContents.
      // The flag is read OUTSIDE the if on purpose: IsStaticFile returns False
      // whenever it raises the flag, so a nested check never runs, and the
      // directory and SPA branches below use lFullPathInfo, which carries no
      // containment check of its own.
      lIsStaticFile := TMVCStaticContents.IsStaticFile(lState.ResolvedRoot,
        lRelative, lFileName, lIsDirectoryTraversalAttack);
      if lIsDirectoryTraversalAttack then
      begin
        AContext.Response.StatusCode := http_status.NotFound;
        Exit;
      end;
      if lIsStaticFile then
      begin
        if StaticFilesSendFileWithMime(AContext, lFileName,
          lState.MediaTypes, lOpts.Charset) then
          Exit;
      end;

      // Directory request handling: 301 to /<path>/ if trailing slash missing,
      // then serve <dir>/<IndexDocument>.
      if TDirectory.Exists(lFullPathInfo) then
      begin
        if not AContext.Request.PathInfo.EndsWith('/') then
        begin
          AContext.Response.StatusCode := http_status.MovedPermanently;
          AContext.Response.CustomHeaders.Values['Location'] :=
            AContext.Request.PathInfo + '/';
          Exit;
        end;
        if lOpts.IndexDocument <> '' then
          if StaticFilesSendFileWithMime(AContext,
            TPath.Combine(lFullPathInfo, lOpts.IndexDocument),
            lState.MediaTypes, lOpts.Charset) then
            Exit;
      end;

      // SPA fallback: walk up to nearest existing directory and serve its
      // index.html. Lets a single bundle serve every client-routed URL.
      if lOpts.SPAWebAppSupport and (lOpts.IndexDocument <> '') then
      begin
        while (lFullPathInfo <> '') and (not TDirectory.Exists(lFullPathInfo)) do
          lFullPathInfo := TDirectory.GetParent(lFullPathInfo);
        if lFullPathInfo <> '' then
        begin
          lFileName := TPath.GetFullPath(
            TPath.Combine(lFullPathInfo, lOpts.IndexDocument));
          if StaticFilesSendFileWithMime(AContext, lFileName,
            lState.MediaTypes, lOpts.Charset) then
            Exit;
        end;
      end;

      // No match — fall through to routing / handlers (will likely 404).
      ANext();
    end;
end;

// --- OpenAPI / Swagger (HTTPFilter wrappers) -----------------------------

function OpenAPI(AEngine: TMVCEngine; const AInfo: TMVCOpenAPIInfo;
  const AURL: string): TMVCHTTPFilter;
var
  lMW: IMVCMiddleware;
begin
  // Build the middleware once; closure captures the interface so its lifetime
  // follows the filter (engine shutdown).
  lMW := TMVCOpenAPI3Middleware.Create(AEngine, AInfo, AURL);
  Result :=
    procedure (const AContext: TWebContext;
               const ANext: TMVCHTTPFilterNext)
    var
      lHandled: Boolean;
    begin
      lHandled := False;
      lMW.OnBeforeRouting(AContext, lHandled);
      if lHandled then
        Exit; // doc served, short-circuit
      ANext();
    end;
end;

function Swagger(AEngine: TMVCEngine;
  const AInfo: TMVCSwaggerInfo;
  const AURL: string;
  const AJWTDescription: string;
  const AEnableBasicAuthentication: Boolean;
  const AHost: string;
  const ABasePath: string;
  const APathFilter: string;
  const ATransferProtocolSchemes: TMVCTransferProtocolSchemes;
  const AEnableBearerAuthentication: Boolean;
  const ASpecVersion: TMVCSwaggerSpecVersion): TMVCHTTPFilter;
var
  lMW: IMVCMiddleware;
begin
  lMW := TMVCSwaggerMiddleware.Create(AEngine, AInfo, AURL, AJWTDescription,
    AEnableBasicAuthentication, AHost, ABasePath, APathFilter,
    ATransferProtocolSchemes, AEnableBearerAuthentication, ASpecVersion);
  Result :=
    procedure (const AContext: TWebContext;
               const ANext: TMVCHTTPFilterNext)
    var
      lHandled: Boolean;
    begin
      lHandled := False;
      lMW.OnBeforeRouting(AContext, lHandled);
      if lHandled then
        Exit;
      ANext();
    end;
end;

initialization

gAnalyticsLoggerLock := TObject.Create;

finalization

gAnalyticsLoggerLock.Free;

end.
