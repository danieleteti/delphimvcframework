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
// Ready-made filters, for controllers and Minimal API routes.
//
// Two kinds of filter live here:
//
//   * EndpointFilter  — after routing, wraps the action or the handler, returns
//                       IMVCResponse. Registered engine-wide via
//                       lEngine.Use(...), or via .Use(...) on a group.
//   * HTTPFilter      — engine-wide, wraps the entire dispatch including
//                       routing. Registered via lEngine.Use(...).
//
// The base filter types (TMVCEndpointFilter, TMVCHTTPFilter, *Next) are declared
// in MVCFramework; this unit only provides the off-the-shelf helpers.
//
// Helpers exposed here:
//
//   EndpointFilter (per-group, or engine-wide with lEngine.Use):
//     CORSEndpointFilter, BasicAuthEndpointFilter, CSRFEndpointFilter,
//     AuthorizeEndpointFilter, RequireRoleEndpointFilter,
//     MemorySessionEndpointFilter, FileSessionEndpointFilter, SessionEndpointFilter
//
//   HTTPFilter (engine-wide):
//     StaticFilesHTTPFilter, CompressionHTTPFilter, ETagHTTPFilter, IPBlockHTTPFilter,
//     RateLimitHTTPFilter, RequestLogHTTPFilter, CORSHTTPFilter,
//     SecurityHeadersHTTPFilter, ShutdownHTTPFilter, RangeMediaHTTPFilter,
//     BearerAuthHTTPFilter, AllowedOriginHTTPFilter, AllowedHostHTTPFilter, AllowedMethodsHTTPFilter,
//     AnalyticsHTTPFilter, TraceHTTPFilter, RedirectHTTPFilter,
//     MemorySessionHTTPFilter, FileSessionHTTPFilter, SessionHTTPFilter,
//     CSRFHTTPFilter
//
//   Bundle (lEngine.Use): FileServerBundle — StaticFiles + the Range of RangeMedia + SPA.
//
//   Plus top-level helpers: WaitForShutdownOrConsoleReturn.
//
// Authentication: MVCFramework.Filters.Authentication — JWTAuthenticationBundle,
//     JWTCookieAuthenticationBundle, BasicAuthenticationBundle, CustomAuthenticationBundle
//     (lEngine.Use(...), or .Use(...) on a group). See "Authentication" below.
//
// Companion units, one per optional feature (a consumer of MVCFramework.Filters
// does not pull in FireDAC, SwagDoc or a Redis client):
//     MVCFramework.Filters.ActiveRecord — ActiveRecordEndpointFilter, ActiveRecordHTTPFilter,
//                                         DatabaseSessionEndpointFilter, DatabaseSessionHTTPFilter
//     MVCFramework.Filters.Swagger      — SwaggerHTTPFilter
//     MVCFramework.Filters.OpenAPI      — OpenAPIHTTPFilter
//     MVCFramework.Filters.OIDC         — OIDCAuthenticationBundle
//     MVCFramework.Filters.Redis        — RateLimitRedisHTTPFilter
//
// ***************************************************************************

unit MVCFramework.Filters;

{$I dmvcframework.inc}

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  System.SyncObjs,                             // TCriticalSection (TMVCInMemoryRateLimitStorage)
  LoggerPro,                                   // ILogWriter (Analytics helper)
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.JWT,                            // TMVCJWTCookieSameSite (TMVCAuthCookieOptions)
  MVCFramework.Session;                        // TMVCWebSessionFactory (Session / SessionHTTPFilter)

type
  // Flags of the session cookie, for the session filters (HTTP and endpoint).
  // The default is [scfHttpOnly].
  //
  //   FileSessionHTTPFilter(10, 'sessions', [scfHttpOnly, scfSecure])
  TMVCSessionCookieFlag = (scfHttpOnly, scfSecure);
  TMVCSessionCookieFlags = set of TMVCSessionCookieFlag;

  // Status of the Redirect HTTP filter: 302 (temporary) or 301 (permanent).
  TMVCRedirectKind = (rdkTemporary, rdkPermanent);
  // What the Redirect HTTP filter does with the query string of the request.
  TMVCRedirectQuery = (
    rdqPreserve, // appended to the target: /old?a=1 -> /new?a=1
    rdqDrop      // left out: /old?a=1 -> /new
  );

  TMVCTraceFlag = (
    tfLogAuthorization, // the request Authorization is logged raw instead of 'Bearer ***' (leaks tokens to the log)
    tfLogNonTextBody,   // the body of a non-text content type is logged too
    tfLogBodySecrets    // passwords, tokens and secrets in a body or query string are logged raw instead of '***'
  );
  TMVCTraceFlags = set of TMVCTraceFlag;

// HttpOnly, plus Secure when ASecure: the flags for a Secure setting that is
// read at run time (e.g. from .env).
//
//   MemorySessionHTTPFilter(30, SessionCookieFlags(dotEnv.Env('SESSION_COOKIE_SECURE', True)))
function SessionCookieFlags(const ASecure: Boolean): TMVCSessionCookieFlags;

// Session filters for any session factory: every request passing through gets
// AFactory, which the filter owns (freed when the filter is dropped, typically
// at engine shutdown). SessionEndpointFilter is the endpoint filter,
// SessionHTTPFilter the engine-wide one. MemorySessionEndpointFilter,
// FileSessionEndpointFilter and the database ones of
// MVCFramework.Filters.ActiveRecord are built on them.
function SessionEndpointFilter(const AFactory: TMVCWebSessionFactory): TMVCEndpointFilter;
function SessionHTTPFilter(const AFactory: TMVCWebSessionFactory): TMVCHTTPFilter;

// -------------------------------------------------------------------------
// EndpointFilter helpers (group.Use, or engine-wide with lEngine.Use).
// -------------------------------------------------------------------------

// Returns a filter that wires an in-memory session factory onto every request
// passing through the group it is attached to (and every nested sub-group, by
// the standard filter-inheritance rules).
//
//   lEngine.Root.AsWeb
//     .Use(MemorySessionEndpointFilter(10))         // 10-minute idle timeout, HttpOnly on
//     .MapGet('/', HomeHandler);
//
// The factory is owned by the filter via an interface-managed holder, so it is
// freed when the filter is dropped (typically engine shutdown). The session
// itself is read/written via the usual TWebContext.Session API.
function MemorySessionEndpointFilter(const ATimeoutInMinutes: Integer = 0;
  const AFlags: TMVCSessionCookieFlags = [scfHttpOnly]): TMVCEndpointFilter;

// CORS endpoint filter: stamps the Access-Control-* headers on the responses of
// the group's routes. Like every endpoint filter it runs only on a matched route,
// so it answers a browser preflight (OPTIONS) only on a route registered for
// OPTIONS too; an OPTIONS on a GET/POST-only route is a 404 from the router
// before this filter runs. To answer preflights for the whole API use
// CORSHTTPFilter, the HTTP filter, which runs before routing.
// Defaults match the typical permissive setup used by the classic
// TMVCCORSMiddleware.
function CORSEndpointFilter(
  const AAllowedOriginURLs: string = '*';
  const AAllowsCredentials: Boolean = True;
  const AExposeHeaders: string = '';
  const AAllowsHeaders: string = 'X-Requested-With, Content-Type, Accept, Origin, Authorization';
  const AAllowsMethods: string = 'GET, POST, PUT, DELETE, OPTIONS';
  const AAccessControlMaxAge: Integer = 86400): TMVCEndpointFilter;

// -------------------------------------------------------------------------
// HTTPFilter helpers (engine-wide, attached via lEngine.Use).
// -------------------------------------------------------------------------

type
  // What StaticFiles does with a request under its prefix that is not a file.
  TMVCStaticFilesMode = (
    sfmFiles,  // the request goes on to routing (a 404 when nothing matches)
    sfmSPA     // single-page application: a 404 of the router becomes the nearest index.html and the client-side router takes over
  );

// HTTPFilter that serves static files from ARootFolder under APrefix. It is the
// StaticFilesHTTPFilter(TMVCStaticFilesOptions) filter with default options, so both forms
// behave the same way:
//   - an existing file is served with its MIME type, Last-Modified and 304 on
//     If-Modified-Since;
//   - a folder is served through its ADefaultDocument (a folder requested
//     without the trailing '/' is redirected with a 301);
//   - the prefix is matched case-insensitively, with or without a trailing '/';
//   - a path that escapes the root folder answers 404;
//   - anything else goes on to routing (sfmFiles) or is answered with the
//     nearest index.html (sfmSPA).
// With sfmSPA the router goes first: a GET/HEAD for which the application has
// no route, and which would be a 404, is answered with the index.html of the
// nearest folder instead. The routes of the application (an API next to the
// SPA, at '/' too) are never hidden by it. Needs AllowUnhandledAction = false
// (the default), and the router logs "ROUTE NOT FOUND" for each client route.
// A relative ARootFolder is looked for in the current folder, then in AppPath.
//
//   lEngine.Use(StaticFilesHTTPFilter('/static', 'www'));
//   lEngine.Use(StaticFilesHTTPFilter('/app', 'www/app', sfmSPA));
function StaticFilesHTTPFilter(const APrefix: string; const ARootFolder: string;
  const AMode: TMVCStaticFilesMode = sfmFiles;
  const ADefaultDocument: string = 'index.html'): TMVCHTTPFilter; overload;

// HTTPFilter that compresses the response body post-Next when:
//   - response has a ContentStream larger than ACompressionThreshold bytes;
//   - client advertised a supported encoding via Accept-Encoding (gzip or
//     deflate; the first one the client lists wins).
// Sets Content-Encoding accordingly. No-op for responses below threshold,
// responses without a ContentStream, or requests without Accept-Encoding.
//
//   lEngine.Use(CompressionHTTPFilter(1024));
function CompressionHTTPFilter(const ACompressionThreshold: Integer = 1024): TMVCHTTPFilter;

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
//   lEngine.Use(ETagHTTPFilter()).Use(CompressionHTTPFilter());
function ETagHTTPFilter: TMVCHTTPFilter;

// HTTPFilter that short-circuits requests from any client IP in the
// blocklist with HTTP 403. Comparison is exact-string against
// Ctx.Request.ClientIp and Ctx.Request.PeerIp; no CIDR matching. Returns the filter as-is so
// callers can chain registration.
//
//   lEngine.Use(IPBlockHTTPFilter(['10.0.0.5', '192.168.1.42']));
function IPBlockHTTPFilter(const ABlockedIPs: TArray<string>): TMVCHTTPFilter;

// HTTPFilter that enforces a sliding-window rate limit per client IP.
// More than AMaxRequests within AWindowSeconds triggers HTTP 429 with a
// Retry-After header. State is held in a process-wide thread-safe map.
//
//   lEngine.Use(RateLimitHTTPFilter(100, 60));   // 100 req / minute / IP
function RateLimitHTTPFilter(const AMaxRequests: Integer = 60;
  const AWindowSeconds: Integer = 60): TMVCHTTPFilter; overload;

// HTTPFilter that emits a one-line request log via MVCFramework.Logger
// after the inner pipeline completes. Format:
//   [HTTP] <ip> <method> <path> -> <status> (<duration_ms>ms)
// Times include any HTTPFilter wrapped inside it; place this filter
// outermost (first registered) to capture full request latency.
//
//   lEngine.Use(RequestLogHTTPFilter());
function RequestLogHTTPFilter: TMVCHTTPFilter;

// HTTPFilter version of CORS. Short-circuits OPTIONS preflight requests
// with the full CORS header set + 200 OK; on every other request stamps
// the simple CORS headers post-Next so they land on success AND error
// responses. Equivalent to the EndpointFilter CORSEndpointFilter() helper but engine-
// wide and runs BEFORE routing — important for preflights that should
// not be subject to per-group filters or route matching.
//
//   lEngine.Use(CORSHTTPFilter('*', False, '', 'Content-Type,Authorization',
//     'GET,POST,PUT,DELETE,OPTIONS', 1728000));
function CORSHTTPFilter(
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
//   lEngine.Use(SecurityHeadersHTTPFilter());
function SecurityHeadersHTTPFilter: TMVCHTTPFilter;

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
  // Token validator for BearerAuthHTTPFilter. Return True to accept the token and
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
//   lEngine.Use(BearerAuthHTTPFilter(
//     StaticBearerTokens([TPair<string, string>.Create('cli', lToken)]), '/mcp'));
// No verb is exempt: a browser CORS preflight (OPTIONS) carries no Authorization and
// gets 401 on the guarded paths. The prefix must be the full routed path.
function BearerAuthHTTPFilter(const AValidator: TMVCBearerValidator;
  const APathPrefix: string = ''): TMVCHTTPFilter;

// HTTPFilter: refuses with 403 a request that carries an Origin not in the
// list (case-insensitive, exact). A request with no Origin header passes: it
// is not a browser cross-origin request. CORS only decides what the
// browser may read; this one stops the request on the server.
function AllowedOriginHTTPFilter(const AOrigins: TArray<string>;
  const APathPrefix: string = ''): TMVCHTTPFilter;

// HTTPFilter against DNS rebinding: refuses with 403 a request whose Host
// header (port ignored) is not localhost, 127.0.0.1, [::1] or one of AHosts.
// Meant for services that listen on loopback without authentication.
function AllowedHostHTTPFilter(const AHosts: TArray<string>;
  const APathPrefix: string = ''): TMVCHTTPFilter;

// HTTPFilter: limits APath (exact, not its sub-paths) to the verbs in AMethods.
// A request that does not target APath goes on; one that targets it with a
// verb not in AMethods, or that targets a sub-path of it, gets 405 with
// `Allow: <verbs>` (listed in TMVCHTTPMethodType order).
function AllowedMethodsHTTPFilter(const AMethods: TMVCHTTPMethods;
  const APath: string): TMVCHTTPFilter;

// HTTPFilter that intercepts a POST request to AURLSegment carrying the
// configured API key query-string param and triggers an orderly engine
// shutdown via EnterInShutdownState. Non-matching requests fall through
// to Next() unmodified. DESTRUCTIVE endpoint — register this AS THE FIRST
// HTTPFilter so it runs ahead of CORS, Compression, RateLimit etc., and
// always pair it with a strong, hard-to-guess API key value.
//
//   lEngine.Use(ShutdownHTTPFilter('apikey', 'a-strong-secret'));
//
// Pair on the host side with WaitForShutdownOrConsoleReturn (below) to
// block the main thread until the shutdown actually fires.
function ShutdownHTTPFilter(const AAPIKeyQueryStringParamName: string;
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
//   lProtected := lGroup.Use(BasicAuthEndpointFilter(
//     function(const U, P: string; var R: TArray<string>): Boolean
//     begin
//       Result := SameText(U, 'admin') and (P = 'secret');
//       if Result then R := ['admin'];
//     end, 'Admin area')); // with the realm: without it the call is ambiguous with the handler overload
function BasicAuthEndpointFilter(
  const AValidator: TMVCBasicAuthValidator;
  const ARealm: string = 'DelphiMVCFramework REALM'): TMVCEndpointFilter; overload;

// Basic Auth EndpointFilter wrapping a full IMVCAuthenticationHandler: the
// sequence of TMVCBasicAuthenticationMiddleware (OnRequest, OnAuthentication,
// OnAuthorization; 401 when not identified, 403 when not authorized), answered
// with Status responses, and a session used when there is one. The handler is
// asked as described in "Authentication" below, and is required.
function BasicAuthEndpointFilter(
  const AHandler: IMVCAuthenticationHandler;
  const ARealm: string = 'DelphiMVCFramework REALM'): TMVCEndpointFilter; overload;

// Endpoint filter: require an authenticated user (Context.LoggedUser.IsValid).
// Replies 401 (Status response) when no valid user is present. Register AFTER
// the authentication filter (JWT / BasicAuth / custom) that populates LoggedUser.
//
//   lEngine.Prefix('/admin').Use(JWTAuthenticationBundle(...)).Use(AuthorizeEndpointFilter());
function AuthorizeEndpointFilter: TMVCEndpointFilter;

// Endpoint filter: require the authenticated user to hold a role. The array
// overload is ANY-OF (passes when the user holds at least one listed role).
// Replies 401 when unauthenticated, 403 when authenticated without the role.
//
//   lEngine.Prefix('/admin').Use(JWTAuthenticationBundle(...)).Use(RequireRoleEndpointFilter('admin'));
function RequireRoleEndpointFilter(const ARole: string): TMVCEndpointFilter; overload;
function RequireRoleEndpointFilter(const ARoles: TArray<string>): TMVCEndpointFilter; overload;

// -------------------------------------------------------------------------
// Authentication: the classic authentication middleware as filter bundles,
// in MVCFramework.Filters.Authentication (JWTAuthenticationBundle,
// JWTCookieAuthenticationBundle, BasicAuthenticationBundle,
// CustomAuthenticationBundle) and MVCFramework.Filters.OIDC
// (OIDCAuthenticationBundle), plus BasicAuthEndpointFilter here.
//
// Each part runs the code of its middleware, so it checks and answers the same
// way. Where the middleware has a part that needs no route (login, logout, the
// OIDC callback) that part is an HTTP filter, on the engine also when the
// bundle goes on a group; the check of the request is an endpoint filter:
// engine-wide for controllers and Minimal API routes (AEngine.Use), or on a
// route group (AGroup.Use). Register the session filter, when one is needed,
// before them.
//
// The handler (IMVCAuthenticationHandler) is asked as by the middleware:
//   - for a controller action, OnRequest and OnAuthorization get the qualified
//     class name of the controller and the name of the action;
//   - a Minimal API route has no controller: the filter attached to its group
//     (or engine-wide) is what asks for authentication, so OnRequest is not
//     asked there and authentication is always required; OnAuthorization gets
//     an empty class name and "VERB pattern" as the action.
// The endpoint filters run after OnBeforeControllerAction of every classic
// middleware, whatever the order of registration: a classic hook that reads
// Context.LoggedUser there sees no user yet. Make it an endpoint filter
// registered after the authentication one.
// The HTTP filters (the logins) run before every classic middleware: a rate
// limit, an IP block, CORS or a CSRF check that must cover the login has to be
// an HTTP filter registered before it (RateLimitHTTPFilter, IPBlockHTTPFilter, CORSHTTPFilter, CSRFHTTPFilter).
// The JWT logins answer any verb: without the CORS filter before them, the
// browser preflight (OPTIONS) of a cross-origin login gets their 401.
// -------------------------------------------------------------------------

type
  // The cookie of JWTCookieAuthenticationBundle (MVCFramework.Filters.Authentication) and of
  // OIDCAuthenticationBundle (MVCFramework.Filters.OIDC). The cookie is always HttpOnly. SameSite=None
  // needs Secure: a bundle built with SameSite=None and Secure=False raises.
  TMVCAuthCookieOptions = record
    Name: string;                       // 'jwt_token'
    Secure: Boolean;                    // True: sent only over HTTPS. False only for local development over HTTP
    SameSite: TMVCJWTCookieSameSite;    // ssStrict
    Path: string;                       // '/'
    Domain: string;                     // '': the host of the request
    class function Default: TMVCAuthCookieOptions; static;
  end;

// Endpoint filter: CSRF protection (synchronizer token, stateless: the
// token is HMAC-SHA256(ASecret, session id) and ATokenSource gives the session
// id of the request). POST, PUT, PATCH, DELETE and QUERY must carry the token
// in AHeaderName or in the form field AFormFieldName, and an Origin, when
// present, must be the Host or one of AAllowedHosts: otherwise 403 (text) and
// the action does not run. AExemptPaths are matched against PathInfo, case-
// sensitively, on a '/' boundary; with an endpoint filter a group without it
// is simpler. A secret shorter than 16 characters, or an invalid exempt path or
// host, raises. As an endpoint filter it checks only requests that have a
// route: one without gets the 404 of the router.
//
//   AEngine.Use(CSRFEndpointFilter(lSecret,
//     function(AContext: TWebContext): string
//     begin
//       Result := AContext.Request.Cookie('jwt_token');
//     end));
function CSRFEndpointFilter(const ASecret: string;
  const ATokenSource: TFunc<TWebContext, string>;
  const AHeaderName: string = 'X-CSRF-Token';
  const AFormFieldName: string = 'csrf_token';
  const AExemptPaths: TArray<string> = [];
  const AAllowedHosts: TArray<string> = []): TMVCEndpointFilter;

// HTTP filter: the same CSRF check before routing, for every request, as
// TMVCCSRFMiddleware does. Registered first, it also checks the unsafe
// requests answered by the HTTP filters after it, which an endpoint filter
// never sees. Safe verbs are not checked: the
// logouts of JWTCookieAuthenticationBundle, of the blacklist of JWTAuthenticationBundle and of
// OIDCAuthenticationBundle answer GET too. A login has no session yet, so no token: exempt it, and
// refuse a cross-site login with AllowedOriginHTTPFilter(origins, '/login') (an
// exempt path is not checked for Origin either). CustomAuthenticationBundle logs
// out (DELETE) at its login URL: exempting that login exempts its logout.
function CSRFHTTPFilter(const ASecret: string;
  const ATokenSource: TFunc<TWebContext, string>;
  const AHeaderName: string = 'X-CSRF-Token';
  const AFormFieldName: string = 'csrf_token';
  const AExemptPaths: TArray<string> = [];
  const AAllowedHosts: TArray<string> = []): TMVCHTTPFilter;

// The CSRF token of a session id (lowercase hex HMAC-SHA256), to put in the
// pages and forms the CSRF filters will check. The session id must not change
// while those pages are open: not the raw JWT cookie when JWTCookieAuthenticationBundle renews it
// (a live validity window), rather a claim that stays (e.g. "jti").
function CSRFToken(const ASecret, ASessionId: string): string;

// File-backed session factory EndpointFilter — sibling of MemorySessionEndpointFilter with
// the same per-group / nested-inheritance shape. Sessions survive process
// restart (data stored under ASessionFolder).
function FileSessionEndpointFilter(const ATimeoutInMinutes: Integer = 0;
  const ASessionFolder: string = 'dmvc_sessions';
  const AFlags: TMVCSessionCookieFlags = [scfHttpOnly]): TMVCEndpointFilter;

// -------------------------------------------------------------------------
// Extended HTTPFilter helpers
// -------------------------------------------------------------------------

// Analytics HTTPFilter — post-Next observer. Emits one CSV-style log line per
// request:
//   <ip>;<method>;<path>;<status>;<route-name-or-empty>;<resp-len>;<host>
// When ALogWriter is nil, uses GetAnalyticsDefaultLogger (rotates CSV files
// under AppPath\analytics — same behavior as the classic Analytics middleware).
function AnalyticsHTTPFilter(const ALogWriter: ILogWriter = nil): TMVCHTTPFilter;

// The CSV logger of the Analytics filter and middleware (files rotated under
// AppPath\analytics), built on first use and shared by every caller.
function GetAnalyticsDefaultLogger: ILogWriter;

// Trace HTTPFilter — full pre/post request/response dump via Log.Debug with
// ALogTag, as TMVCTraceMiddleware does. Every line carries a correlation id
// ([rid:...]): the one the request brings in the ACorrelationHeaderName header
// (or X-Correlation-ID), otherwise a generated one; it is echoed in the
// response header of the same name. The body is decoded with the charset of its
// Content-Type (UTF-8 when it names none, or the ANSI code page Render(string)
// writes such a body in) and cut to AMaxBodySize characters (to AMaxBodySize bytes
// when the response is a stream, leaving out a UTF-8 character cut in two); bytes
// that are not UTF-8 when the charset says so are logged as
// '<N bytes not decodable as utf-8>'.
// Non-text content types are hidden unless AFlags has tfLogNonTextBody. The
// request Authorization is masked ('Bearer ***'), and so are Authorization,
// Cookie and Set-Cookie among the response headers; tfLogAuthorization logs the
// request Authorization raw (useful for debugging, leaks bearer tokens to your
// log files). In a body and in the query string, the value of a JSON member, form
// field or XML element whose name has password, passwd, passphrase, pwd, secret,
// token, jwt, api key, private key, credential, authorization, cookie or session
// in it is logged as ***; tfLogBodySecrets logs it raw.
//
//   lEngine.Use(TraceHTTPFilter(4096, 'trace', [tfLogAuthorization]));
function TraceHTTPFilter(const AMaxBodySize: Int64 = 1024;
  const ALogTag: string = 'trace';
  const AFlags: TMVCTraceFlags = [];
  const ACorrelationHeaderName: string = 'X-Request-ID'): TMVCHTTPFilter;

// Redirect HTTPFilter — pattern-based pre-Next short-circuit. Exact case-
// insensitive PathInfo match against APaths; on match returns 301 (permanent)
// or 302 (temporary) with Location: ATargetURL. With rdqPreserve (default) the
// query string is appended to the target as `?<qs>`; rdqDrop leaves it out.
//
//   lEngine.Use(RedirectHTTPFilter(['/'], '/web'));
//   lEngine.Use(RedirectHTTPFilter(['/old'], '/new', rdkPermanent, rdqDrop));
function RedirectHTTPFilter(const APaths: TArray<string>;
  const ATargetURL: string;
  const AKind: TMVCRedirectKind = rdkTemporary;
  const AQuery: TMVCRedirectQuery = rdqPreserve): TMVCHTTPFilter; overload;

// Redirect HTTPFilter — per-rule (from, to) overload for more flexible
// rewrites. Same matching semantics as the single-target overload.
//
//   lEngine.Use(RedirectHTTPFilter([
//     TPair<string,string>.Create('/old',  '/new'),
//     TPair<string,string>.Create('/docs', 'https://docs.example.com')]));
function RedirectHTTPFilter(const ARules: TArray<TPair<string, string>>;
  const AKind: TMVCRedirectKind = rdkTemporary;
  const AQuery: TMVCRedirectQuery = rdqPreserve): TMVCHTTPFilter; overload;

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
//   lEngine.Use(RangeMediaHTTPFilter('/static/media', 'static/media'));
function RangeMediaHTTPFilter(const AURLPath: string;
  const ADocumentRoot: string): TMVCHTTPFilter;

// -------------------------------------------------------------------------
// StaticFiles full-options overload
// -------------------------------------------------------------------------

type
  // Per-request rule of StaticFiles, before the file system is touched: it can rewrite PathInfo,
  // and setting Handled (True on entry) to False ends the request with what the rule answered.
  TMVCStaticFileRulesProc = reference to procedure(const Context: TWebContext; var PathInfo: String; var Handled: Boolean);
  // Adds or overrides the MIME types of StaticFiles (file extension -> media type).
  TMVCStaticFileMediaTypesCustomizer = reference to procedure(const MediaTypes: TMVCStringDictionary);

  // Full StaticFiles HTTPFilter options — parity with TMVCStaticFilesMiddleware
  // (sanity check, charset, SPA fallback, MIME customization, request rules,
  // case-insensitive prefix). Build via TMVCStaticFilesOptions.Default and
  // override fields as needed.
  TMVCStaticFilesFlag = (
    sffCaseSensitivePrefix  // the prefix is matched case-sensitively
  );
  TMVCStaticFilesFlags = set of TMVCStaticFilesFlag;

  TMVCStaticFilesOptions = record
    Prefix: string;                                       // URL prefix (e.g. '/static')
    RootFolder: string;                                   // Filesystem root. If relative + not existing, resolved against AppPath.
    IndexDocument: string;                                // Default file for directory requests (e.g. 'index.html')
    Charset: string;                                      // Charset applied to text/* + json MIME types (e.g. 'UTF-8')
    Mode: TMVCStaticFilesMode;                            // sfmFiles (default, as in the simple form); sfmSPA: a GET/HEAD the router has no route for gets the index.html of the nearest existing folder
    Flags: TMVCStaticFilesFlags;                          // [] (default): the prefix is matched case-insensitively (Windows-friendly)
    MediaTypesCustomizer: TMVCStaticFileMediaTypesCustomizer;  // Optional callback to add/override MIME types
    Rules: TMVCStaticFileRulesProc;                       // Optional per-request allow/deny + path rewrite
    class function Default: TMVCStaticFilesOptions; static;
  end;

// StaticFiles HTTPFilter — the full implementation, the simple form calls it.
// Honors directory redirect (301 + trailing slash), SPA fallback, MIME
// customizer, request rules, sanity check (raises on the first request under
// the prefix if the root folder is missing), case-insensitive prefix matching,
// and delegates the file send to TMVCStaticContents.SendFile so
// If-Modified-Since + Last-Modified are honored.
function StaticFilesHTTPFilter(const AOptions: TMVCStaticFilesOptions): TMVCHTTPFilter; overload;

type
  // What FileServerBundle does with a request under its prefix that is not a file.
  TMVCFileServerMode = (
    fsmFiles,  // the request goes on to routing (a 404 when nothing matches)
    fsmSPA     // a GET/HEAD the router has no route for gets the nearest index.html
  );

  TMVCFileServerFlag = (
    fsfRanges,              // a Range request for a file gets 206 Partial Content, as RangeMediaHTTPFilter answers
    fsfCaseSensitivePrefix  // the prefix is matched case-sensitively
  );
  TMVCFileServerFlags = set of TMVCFileServerFlag;

  // Options of FileServerBundle. Build via TMVCFileServerOptions.Default and
  // override the fields you need.
  TMVCFileServerOptions = record
    URLPrefix: string;                                    // default '/static'
    DocumentRoot: string;                                 // default '.\www'; a relative one is looked for in the current folder, then in AppPath
    Mode: TMVCFileServerMode;                             // default fsmFiles
    DefaultDocument: string;                              // file served for a folder, default 'index.html'
    Charset: string;                                      // charset of the text MIME types, default '' (none added)
    Flags: TMVCFileServerFlags;                           // default [fsfRanges]
    MediaTypesCustomizer: TMVCStaticFileMediaTypesCustomizer;  // adds or overrides MIME types
    Rules: TMVCStaticFileRulesProc;                       // per-request allow/deny + path rewrite
    class function Default: TMVCFileServerOptions; static;
  end;

// A folder served under a URL prefix, in one Use: the files of
// StaticFilesHTTPFilter (MIME type, Last-Modified and 304, default document,
// the 301 of a folder, path traversal refused, Rules and MediaTypesCustomizer)
// plus, with fsfRanges, the 206 Partial Content of RangeMediaHTTPFilter for a
// request that carries a Range header (and the audio/video MIME types of
// RangeMedia). A request without Range gets the whole file, with Last-Modified.
// With fsmSPA a GET/HEAD the router has no route for (and would answer 404)
// gets the nearest index.html: an API on the same prefix keeps its routes
// (AllowUnhandledAction must be false, the default).
// An HTTP part only, which runs before routing: register it like any other Use.
//
//   AEngine.Use(FileServerBundle('/', TPath.Combine(AppPath, 'www')));
//   AEngine.Use(FileServerBundle('/', TPath.Combine(AppPath, 'www'), fsmSPA));
function FileServerBundle(const AURLPrefix, ADocumentRoot: string;
  const AMode: TMVCFileServerMode = fsmFiles): IMVCFilterBundle; overload;
function FileServerBundle(const AOptions: TMVCFileServerOptions): IMVCFilterBundle; overload;

// -------------------------------------------------------------------------
// Sessions, ActiveRecord and RateLimit with options as HTTPFilters. Engine-
// wide, so controllers use them too (the endpoint filters of the same name
// attach to Minimal API route groups only).
// -------------------------------------------------------------------------

// Session factory HTTPFilters: every request passing through gets the session
// factory, the session itself is read/written via TWebContext.Session.
//
//   lEngine.Use(MemorySessionHTTPFilter(10));      // 10-minute idle timeout
//   lEngine.Use(FileSessionHTTPFilter(10, 'sessions'));
function MemorySessionHTTPFilter(const ATimeoutInMinutes: Integer = 0;
  const AFlags: TMVCSessionCookieFlags = [scfHttpOnly]): TMVCHTTPFilter;
function FileSessionHTTPFilter(const ATimeoutInMinutes: Integer = 0;
  const ASessionFolder: string = 'dmvc_sessions';
  const AFlags: TMVCSessionCookieFlags = [scfHttpOnly]): TMVCHTTPFilter;
type
  // Strategy of TMVCInMemoryRateLimitStorage.
  TMVCRateLimitStrategy = (
    rlsFixedWindow,   // a counter that resets at fixed intervals
    rlsSlidingWindow, // a counter that considers the request timestamps
    rlsTokenBucket    // allows bursts, tokens refill over time
  );

  // What a request is counted by.
  TMVCRateLimitKeyType = (
    rlkIPAddress, // the client IP
    rlkUserID,    // the logged user (session or JWT), the client IP when nobody is logged in
    rlkAPIKey,    // the API key header, the client IP when it is missing
    rlkCustom     // the KeyExtractor callback, the client IP when it is not set
  );

  TMVCRateLimitKeyExtractor = reference to function(const AContext: TWebContext): string;

  // Called when a request is over the limit, before the 429 is written.
  TMVCRateLimitExceededProc = reference to procedure(
    const AContext: TWebContext;
    const AKey: string;
    const ALimit: Integer;
    const AWindowSeconds: Integer
  );

  // Rate limit storage: implement it for Redis, Memcached, etc.
  IMVCRateLimitStorage = interface
    ['{8E5F9A7C-2D4B-4B8E-9F3A-1C6D8E9A7B5C}']
    // Increments the counter of AKey: True when the limit is exceeded.
    function CheckRateLimit(const AKey: string; const ALimit: Integer;
      const AWindowSeconds: Integer; out ARemaining: Integer;
      out AResetTime: TDateTime): Boolean;
    procedure ResetKey(const AKey: string);
    procedure Clear;
  end;

  // In-memory rate limit storage, thread-safe.
  TMVCInMemoryRateLimitStorage = class(TInterfacedObject, IMVCRateLimitStorage)
  private
    type
      TRateLimitEntry = record
        Count: Integer;
        WindowStart: TDateTime;
        ResetTime: TDateTime;
      end;
  private
    fStorage: TDictionary<string, TRateLimitEntry>;
    fLastCleanup: TDateTime;
    fCeilingReported: Boolean;
    fLock: TCriticalSection;
    fStrategy: TMVCRateLimitStrategy;
    procedure CleanupExpiredEntries;
  public
    constructor Create(const AStrategy: TMVCRateLimitStrategy = rlsFixedWindow);
    destructor Destroy; override;
    function CheckRateLimit(const AKey: string; const ALimit: Integer;
      const AWindowSeconds: Integer; out ARemaining: Integer;
      out AResetTime: TDateTime): Boolean;
    procedure ResetKey(const AKey: string);
    procedure Clear;
    // Number of distinct keys currently held. Not on IMVCRateLimitStorage on
    // purpose: widening the interface would break third-party implementations.
    function KeyCount: Integer;
  end;

  // RateLimit options. Empty strings keep the defaults of the classic
  // TMVCRateLimitMiddleware ('X-API-Key', 'X-RateLimit-').
  TMVCRateLimitOptions = record
    MaxRequests: Integer;                       // requests allowed per window
    WindowSeconds: Integer;
    KeyType: TMVCRateLimitKeyType;              // client IP, user id, API key or custom
    KeyExtractor: TMVCRateLimitKeyExtractor;    // only for rlkCustom
    APIKeyHeaderName: string;                   // only for rlkAPIKey
    HeaderPrefix: string;                       // prefix of the X-RateLimit-* response headers
    ExcludedPaths: TArray<string>;              // path prefixes that are not limited (e.g. '/health')
    OnExceeded: TMVCRateLimitExceededProc;      // called when a request is refused
    Storage: IMVCRateLimitStorage;              // nil: in-memory
    class function Default: TMVCRateLimitOptions; static;
  end;

// RateLimit HTTPFilter with the same options and the same answer as the
// classic TMVCRateLimitMiddleware: 429 + Retry-After + JSON body, and the
// X-RateLimit-* headers on every response.
//
//   lOptions := TMVCRateLimitOptions.Default;
//   lOptions.MaxRequests := 10;
//   lOptions.ExcludedPaths := ['/health'];
//   lEngine.Use(RateLimitHTTPFilter(lOptions));
function RateLimitHTTPFilter(const AOptions: TMVCRateLimitOptions): TMVCHTTPFilter; overload;

implementation


uses
{$IF Defined(MSWINDOWS)}
  WinAPI.Windows,                   // GetStdHandle, PeekConsoleInput (WaitForShutdownOrConsoleReturn)
{$ENDIF}
  System.Diagnostics,
  System.TypInfo,
  System.Math,                      // Min (Trace HTTPFilter)
  System.IOUtils,                   // TPath, TFile, TDirectory (StaticFiles HTTPFilter)
  System.ZLib,                      // TZCompressionStream (Compression HTTPFilter)
  System.Hash,                      // THashSHA1 (ETag HTTPFilter)
  System.DateUtils,                 // IncSecond (RateLimit HTTPFilter)
  System.NetEncoding,               // TNetEncoding.URL (path decoding)
  MVCFramework.Logger,              // Log.Debug, LogI, LogW (RequestLog/Trace/Shutdown)
  MVCFramework.Serializer.Commons,  // TMVCSerializerHelper.DecodeString (BasicAuth)
  MVCFramework.HMAC,                // HMAC (CSRF token)
  LoggerPro.FileAppender,           // TLoggerProSimpleFileAppender (Analytics default logger)
  LoggerPro.Builder,                // LoggerProBuilder (Analytics default logger)
  MVCFramework.Filters.Internal;    // FilterAuthHandler, CheckHandler, SameSecret, CSRFCheck, MVCRateLimitExceeded, MVCTrace*

const
  { Largest slice served in a single 206 response. }
  MAX_RANGE_SLICE = 8 * 1024 * 1024;

type
  // Holds the lifetime of a session factory for the MemorySessionEndpointFilter filter.
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

function SessionCookieFlags(const ASecure: Boolean): TMVCSessionCookieFlags;
begin
  Result := [scfHttpOnly];
  if ASecure then
    Include(Result, scfSecure);
end;

function SessionEndpointFilter(const AFactory: TMVCWebSessionFactory): TMVCEndpointFilter;
var
  lHolder: ISessionFactoryHolder;
begin
  // Wrap the factory in an interface-managed holder. The closure below
  // captures `lHolder` (an interface), so the holder's lifetime follows the
  // closure's: when the filter is dropped (engine shutdown) the closure is
  // freed, the captured interface refcount drops to zero, and the holder's
  // destructor frees the factory.
  lHolder := TSessionFactoryHolder.Create(AFactory);
  Result :=
    function (const AContext: TWebContext;
              const ANext: TMVCEndpointFilterNext): IMVCResponse
    begin
      AContext.SetSessionFactory(lHolder.Factory);
      Result := ANext();
    end;
end;

function MemorySessionEndpointFilter(const ATimeoutInMinutes: Integer;
  const AFlags: TMVCSessionCookieFlags): TMVCEndpointFilter;
begin
  Result := SessionEndpointFilter(
    TMVCWebSessionMemoryFactory.Create(scfHttpOnly in AFlags, ATimeoutInMinutes, scfSecure in AFlags));
end;

{ -------------------------------------------------------------------------- }
{ Filter implementations                                                     }
{ -------------------------------------------------------------------------- }

// --- StaticFiles HTTPFilter ----------------------------------------------

function StaticFilesHTTPFilter(const APrefix: string; const ARootFolder: string;
  const AMode: TMVCStaticFilesMode; const ADefaultDocument: string): TMVCHTTPFilter;
var
  lOptions: TMVCStaticFilesOptions;
begin
  lOptions := TMVCStaticFilesOptions.Default;
  lOptions.Prefix := APrefix;
  lOptions.RootFolder := ARootFolder;
  lOptions.IndexDocument := ADefaultDocument;
  lOptions.Mode := AMode;
  Result := StaticFilesHTTPFilter(lOptions);
end;

// --- Compression ----------------------------------------------------------

// Picks the first supported encoding of a comma-separated Accept-Encoding
// header, in the order the client lists them (as the classic middleware does).
// Returns ctGZIP, ctDeflate, or ctNone.
function PickCompressionEncoding(const AAcceptEncoding: string): TMVCCompressionType;
var
  lToken: string;
begin
  Result := TMVCCompressionType.ctNone;
  if AAcceptEncoding = '' then
    Exit;
  for lToken in AAcceptEncoding.ToLower.Split([',']) do
  begin
    if lToken.Trim = 'gzip' then
      Exit(TMVCCompressionType.ctGZIP);
    if lToken.Trim = 'deflate' then
      Exit(TMVCCompressionType.ctDeflate);
  end;
end;

function CompressionHTTPFilter(const ACompressionThreshold: Integer): TMVCHTTPFilter;
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

function ETagHTTPFilter: TMVCHTTPFilter;
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

function SecurityHeadersHTTPFilter: TMVCHTTPFilter;
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

function BearerAuthHTTPFilter(const AValidator: TMVCBearerValidator;
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

function AllowedOriginHTTPFilter(const AOrigins: TArray<string>;
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

function AllowedHostHTTPFilter(const AHosts: TArray<string>;
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

function AllowedMethodsHTTPFilter(const AMethods: TMVCHTTPMethods;
  const APath: string): TMVCHTTPFilter;
var
  lPrefix, lSlashPrefix, lAllow: string;
  lMethod: TMVCHTTPMethodType;
begin
  lPrefix := MVCNormalizePath(APath);
  if lPrefix = '' then
    raise EMVCException.Create('AllowedMethodsHTTPFilter needs a path: '''' would limit the whole application');
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

function ShutdownHTTPFilter(const AAPIKeyQueryStringParamName: string;
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

function IPBlockHTTPFilter(const ABlockedIPs: TArray<string>): TMVCHTTPFilter;
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

const
  { Hard ceiling on distinct keys held by the in-memory store. }
  MAX_RATELIMIT_KEYS = 100000;

constructor TMVCInMemoryRateLimitStorage.Create(const AStrategy: TMVCRateLimitStrategy);
begin
  inherited Create;
  fStorage := TDictionary<string, TRateLimitEntry>.Create;
  fLock := TCriticalSection.Create;
  fStrategy := AStrategy;
end;

destructor TMVCInMemoryRateLimitStorage.Destroy;
begin
  fStorage.Free;
  fLock.Free;
  inherited;
end;

function TMVCInMemoryRateLimitStorage.KeyCount: Integer;
begin
  fLock.Acquire;
  try
    Result := fStorage.Count;
  finally
    fLock.Release;
  end;
end;

procedure TMVCInMemoryRateLimitStorage.CleanupExpiredEntries;
var
  lKey: string;
  lEntry: TRateLimitEntry;
  lKeysToRemove: TArray<string>;
  lNow: TDateTime;
begin
  // This is called within a lock, so no need for additional synchronization
  lNow := Now;
  { Throttled: this is a full scan of the store under the one lock, and it used
    to run on every single request. With a large store that serialises every
    request thread behind an O(N) walk. Nothing expires faster than a window, so
    once a second is often enough. }
  if (fLastCleanup > 0) and (SecondsBetween(lNow, fLastCleanup) < 1) then
    Exit;
  fLastCleanup := lNow;
  SetLength(lKeysToRemove, 0);

  for lKey in fStorage.Keys do
  begin
    if fStorage.TryGetValue(lKey, lEntry) then
    begin
      if lNow > lEntry.ResetTime then
      begin
        SetLength(lKeysToRemove, Length(lKeysToRemove) + 1);
        lKeysToRemove[High(lKeysToRemove)] := lKey;
      end;
    end;
  end;

  for lKey in lKeysToRemove do
    fStorage.Remove(lKey);
end;

function TMVCInMemoryRateLimitStorage.CheckRateLimit(const AKey: string;
  const ALimit: Integer; const AWindowSeconds: Integer; out ARemaining: Integer;
  out AResetTime: TDateTime): Boolean;
var
  lEntry: TRateLimitEntry;
  lNow: TDateTime;
  lNewEntry: TRateLimitEntry;
begin
  fLock.Acquire;
  try
    CleanupExpiredEntries;

    lNow := Now;

    { CleanupExpiredEntries only drops windows that have already closed, so a
      caller rotating its key - X-API-Key for rlkAPIKey, the callback value for
      rlkCustom, and X-Forwarded-For for rlkIPAddress wherever forwarded headers
      are trusted - escapes its own limit and grows this dictionary for a whole
      window. Past the ceiling a key that is not already known is refused rather
      than admitted. Clearing the store instead, which is the obvious move,
      would hand the flooder a reset of everybody else's counter: the mitigation
      would be a better bypass than the bug. }
    if (fStorage.Count >= MAX_RATELIMIT_KEYS) and (not fStorage.ContainsKey(AKey)) then
    begin
      if not fCeilingReported then
      begin
        fCeilingReported := True;
        LogW('Rate limit store reached ' + IntToStr(MAX_RATELIMIT_KEYS) +
          ' keys: new keys are being refused. This is what key rotation looks ' +
          'like when the key is client-controlled - validate it before the ' +
          'limiter runs.');
      end;
      { True means "limit exceeded" to every caller of this interface, so the
        refusal has to be True. Exit(False) here would have ADMITTED the key and
        never stored it, which makes it take this branch again on the next
        request: past the ceiling the limiter would have been off for every key
        it did not already know. }
      ARemaining := 0;
      AResetTime := IncSecond(lNow, AWindowSeconds);
      Exit(True);
    end;

    if not fStorage.TryGetValue(AKey, lEntry) then
    begin
      // First request from this key
      lNewEntry.Count := 1;
      lNewEntry.WindowStart := lNow;
      lNewEntry.ResetTime := IncSecond(lNow, AWindowSeconds);
      fStorage.AddOrSetValue(AKey, lNewEntry);

      ARemaining := ALimit - 1;
      AResetTime := lNewEntry.ResetTime;
      Result := False;
      Exit;
    end;

    // Check if window has expired
    if lNow >= lEntry.ResetTime then
    begin
      // Start new window
      lNewEntry.Count := 1;
      lNewEntry.WindowStart := lNow;
      lNewEntry.ResetTime := IncSecond(lNow, AWindowSeconds);
      fStorage.AddOrSetValue(AKey, lNewEntry);

      ARemaining := ALimit - 1;
      AResetTime := lNewEntry.ResetTime;
      Result := False;
      Exit;
    end;

    // Within current window
    if lEntry.Count >= ALimit then
    begin
      // Rate limit exceeded
      ARemaining := 0;
      AResetTime := lEntry.ResetTime;
      Result := True;
      Exit;
    end;

    // Increment counter
    lEntry.Count := lEntry.Count + 1;
    fStorage.AddOrSetValue(AKey, lEntry);

    ARemaining := ALimit - lEntry.Count;
    AResetTime := lEntry.ResetTime;
    Result := False;
  finally
    fLock.Release;
  end;
end;

procedure TMVCInMemoryRateLimitStorage.ResetKey(const AKey: string);
begin
  fLock.Acquire;
  try
    fStorage.Remove(AKey);
  finally
    fLock.Release;
  end;
end;

procedure TMVCInMemoryRateLimitStorage.Clear;
begin
  fLock.Acquire;
  try
    fStorage.Clear;
  finally
    fLock.Release;
  end;
end;

function RateLimitHTTPFilter(const AMaxRequests: Integer;
  const AWindowSeconds: Integer): TMVCHTTPFilter; overload;
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

function RequestLogHTTPFilter: TMVCHTTPFilter;
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
// original CORSEndpointFilter() helper also calls it. Single source of truth.
function SplitCORSOrigins(const AConfiguredOrigins: string): TArray<string>; forward;
procedure StampSimpleCORSHeaders(const AContext: TWebContext;
  const AAllowedOrigins: TArray<string>;
  const AAllowsCredentials: Boolean;
  const AExposeHeaders: string); forward;

function CORSHTTPFilter(
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
// nested procedures (E2555). The closure inside CORSHTTPFilter() calls it twice — for
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

function CORSEndpointFilter(
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

// --- Authentication (the bundles are in MVCFramework.Filters.Authentication) ---

class function TMVCAuthCookieOptions.Default: TMVCAuthCookieOptions;
begin
  Result.Name := 'jwt_token';
  Result.Secure := True;
  Result.SameSite := ssStrict;
  Result.Path := '/';
  Result.Domain := '';
end;

// --- CSRF -----------------------------------------------------------------

function CSRFToken(const ASecret, ASessionId: string): string;
begin
  Result := BytesToHex(HMAC(HMAC_SHA256, ASessionId, ASecret));
end;

function CSRFEndpointFilter(const ASecret: string; const ATokenSource: TFunc<TWebContext, string>;
  const AHeaderName, AFormFieldName: string; const AExemptPaths, AAllowedHosts: TArray<string>): TMVCEndpointFilter;
var
  lCheck: TFunc<TWebContext, Boolean>;
begin
  lCheck := CSRFCheck(ASecret, ATokenSource, AHeaderName, AFormFieldName, AExemptPaths, AAllowedHosts);
  // the same check as before routing: here it runs once the route is known
  Result :=
    function(const AContext: TWebContext; const ANext: TMVCEndpointFilterNext): IMVCResponse
    begin
      if not lCheck(AContext) then
        Exit(nil); // the 403 is written
      Result := ANext();
    end;
end;

function CSRFHTTPFilter(const ASecret: string; const ATokenSource: TFunc<TWebContext, string>;
  const AHeaderName, AFormFieldName: string; const AExemptPaths, AAllowedHosts: TArray<string>): TMVCHTTPFilter;
var
  lCheck: TFunc<TWebContext, Boolean>;
begin
  lCheck := CSRFCheck(ASecret, ATokenSource, AHeaderName, AFormFieldName, AExemptPaths, AAllowedHosts);
  Result :=
    procedure(const AContext: TWebContext; const ANext: TMVCHTTPFilterNext)
    begin
      if lCheck(AContext) then
        ANext(); // otherwise the 403 is written
    end;
end;

// --- BasicAuth (EndpointFilter) ------------------------------------------

function Build401WithWWWAuthenticate(const AContext: TWebContext;
  const ARealm: string; const AStopSession: Boolean = False): IMVCResponse;
begin
  AContext.LoggedUser.Clear;
  // the handler overload keeps the identity in the session, as TMVCBasicAuthenticationMiddleware:
  // a refused one leaves no session behind. The validator overload never uses the session
  if AStopSession and AContext.HasSessionSupport then
    AContext.SessionStop(False);
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

function BasicAuthEndpointFilter(const AValidator: TMVCBasicAuthValidator;
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

function BasicAuthEndpointFilter(const AHandler: IMVCAuthenticationHandler;
  const ARealm: string): TMVCEndpointFilter;
var
  lHandler: IMVCAuthenticationHandler;
begin
  CheckHandler(AHandler, 'BasicAuth');
  lHandler := FilterAuthHandler(AHandler);
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
      // the sequence of TMVCBasicAuthenticationMiddleware; lHandler does not ask OnRequest on a Minimal API route
      { Deny by default: the parameter is `var`, not `out`, so a handler that
        leaves a branch unassigned would have this read stack garbage - and
        False here skips authentication altogether. }
      lAuthRequired := True;
      lHandler.OnRequest(AContext, AContext.ControllerQualifiedClassName, AContext.ActionName,
        lAuthRequired);
      if not lAuthRequired then
        Exit(ANext());

      // Pull cached creds from session if available, as the classic middleware.
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
          Exit(Build401WithWWWAuthenticate(AContext, ARealm, True));

        lRoles := TList<string>.Create;
        try
          lSession := TSessionData.Create;
          try
            lHandler.OnAuthentication(AContext, lUserName, lPassword,
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
        lHandler.OnAuthorization(AContext, AContext.LoggedUser.Roles,
          AContext.ControllerQualifiedClassName, AContext.ActionName, lIsAuthorized);

      if lIsAuthorized then
        Exit(ANext());

      // As the classic middleware: 403 if user is identified but lacks
      // authorization, 401 (challenge again) if not even identified.
      if lIsValid then
        Result := Build403Forbidden
      else
        Result := Build401WithWWWAuthenticate(AContext, ARealm, True);
    end;
end;

// --- Authorize / RequireRole (EndpointFilter) ----------------------------

function AuthorizeEndpointFilter: TMVCEndpointFilter;
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

function RequireRoleEndpointFilter(const ARoles: TArray<string>): TMVCEndpointFilter;
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

function RequireRoleEndpointFilter(const ARole: string): TMVCEndpointFilter;
begin
  Result := RequireRoleEndpointFilter(TArray<string>.Create(ARole));
end;

// --- FileSession (EndpointFilter) ----------------------------------------

function FileSessionEndpointFilter(const ATimeoutInMinutes: Integer;
  const ASessionFolder: string;
  const AFlags: TMVCSessionCookieFlags): TMVCEndpointFilter;
begin
  Result := SessionEndpointFilter(
    TMVCWebSessionFileFactory.Create(scfHttpOnly in AFlags, ATimeoutInMinutes, ASessionFolder, scfSecure in AFlags));
end;

// --- Analytics (HTTPFilter) ----------------------------------------------

const
  ANALYTICS_TAG = 'analytics';

var
  // Lazy default logger cache — analytics CSV appender shared across every
  // AnalyticsHTTPFilter() filter built without an explicit ALogWriter (and the
  // classic middleware). Initialised on first use, kept until process exit.
  gAnalyticsLogger: ILogWriter = nil;
  gAnalyticsLoggerLock: TObject = nil;

function GetAnalyticsDefaultLogger: ILogWriter;
var
  lLog: ILogAppender;
begin
  if gAnalyticsLogger = nil then
  begin
    TMonitor.Enter(gAnalyticsLoggerLock);
    try
      if gAnalyticsLogger = nil then // double check locking (https://en.wikipedia.org/wiki/Double-checked_locking)
      begin
        lLog := TLoggerProSimpleFileAppender.Create(10, 5000, AppPath + 'analytics',
          TPath.ChangeExtension(TLoggerProSimpleFileAppender.DEFAULT_FILENAME_FORMAT, 'csv'));
        TLoggerProAppenderBase(lLog).OnLogRow := procedure(const LogItem: TLogItem; out LogRow: string)
          begin
            LogRow := Format('%s;%s;%s', [
              FormatDateTime('yyyy-mm-dd hh:nn:ss', LogItem.TimeStamp),
              LogItem.LogTypeAsString,
              LogItem.LogMessage]);
          end;
        gAnalyticsLogger := LoggerProBuilder.WriteToAppender(lLog).Build;
      end;
    finally
      TMonitor.Exit(gAnalyticsLoggerLock);
    end;
  end;
  Result := gAnalyticsLogger;
end;

function AnalyticsHTTPFilter(const ALogWriter: ILogWriter): TMVCHTTPFilter;
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
          lEffective := GetAnalyticsDefaultLogger;
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

var
  // process-wide counter: the correlation id of a request that does not bring one
  gTraceRequestCounter: Int64 = 0;

function TraceQueryString(const AQueryString: string; const AFlags: TMVCTraceFlags): string;
begin
  if tfLogBodySecrets in AFlags then
    Result := AQueryString
  else
    Result := MVCTraceMaskSecrets(AQueryString);
end;

function TraceClamp(const AValue: string; const AMaxLength: Int64): string;
begin
  Result := Copy(AValue, 1, Integer(Min(AMaxLength, MaxInt)));
end;

function TraceHTTPFilter(const AMaxBodySize: Int64;
  const ALogTag: string;
  const AFlags: TMVCTraceFlags;
  const ACorrelationHeaderName: string): TMVCHTTPFilter;
begin
  Result :=
    procedure (const AContext: TWebContext;
               const ANext: TMVCHTTPFilterNext)
    var
      lSW: TStopwatch;
      lReq: TMVCWebRequest;
      lAuth, lBody, lId, lRid: string;
      lBytes: TBytes;
    begin
      // Correlation id: honor the one of the caller (proxy, gateway, tracing),
      // otherwise a short process-unique one, and echo it to the caller.
      lId := AContext.Request.Headers[ACorrelationHeaderName];
      if lId = '' then
        lId := AContext.Request.Headers['X-Correlation-ID'];
      if lId = '' then
        lId := IntToHex(TInterlocked.Increment(gTraceRequestCounter), 6);
      AContext.Response.SetCustomHeader(ACorrelationHeaderName, lId);
      lRid := '[rid:' + lId + '] ';

      // Pre-Next: REQUEST -------------------------------------------------
      AContext.Request.ReadTotalContent;
      lReq := AContext.Request;
      if tfLogAuthorization in AFlags then
        lAuth := lReq.Authorization
      else
        lAuth := MVCRedactSecret(lReq.Authorization);
      Log.Debug(Format('%s[TRACE][REQ][%s][IP:%s][URL:%s][QS:%s][LEN:%d][ACCEPT:%s][UA:%s][AUTH:%s]', [
        lRid, lReq.HTTPMethodAsString, lReq.ClientIp, lReq.PathInfo,
        TraceQueryString(lReq.QueryFieldsDelimitedText, AFlags), lReq.ContentLength,
        lReq.Accept, lReq.UserAgent, lAuth]), ALogTag);

      if lReq.HTTPMethod in [httpPOST, httpPUT, httpPATCH] then
      begin
        if (tfLogNonTextBody in AFlags) or
           TraceIsTextContentType(lReq.Headers['content-type']) then
        begin
          lBytes := lReq.RawContent;
          lBody := TraceClamp(MVCTraceBodyText(MVCTraceBodyBytes(lBytes, AMaxBodySize), Length(lBytes),
            lReq.Headers['content-type']), AMaxBodySize);
          if not (tfLogBodySecrets in AFlags) then
            lBody := MVCTraceMaskSecrets(lBody);
        end
        else
          lBody := '<hidden non text content>';
        Log.Debug(lRid + '[TRACE][REQ][BODY] ' + lBody, ALogTag);
      end;

      // Inner pipeline ----------------------------------------------------
      lSW := TStopwatch.StartNew;
      try
        ANext();
      finally
        lSW.Stop;

        // Post-Next: RESPONSE ---------------------------------------------
        Log.Debug(Format('%s[TRACE][RES][STATUS] %d %s (%dms)', [
          lRid,
          AContext.Response.StatusCode,
          AContext.Response.ReasonString,
          lSW.ElapsedMilliseconds]), ALogTag);
        Log.Debug(lRid + '[TRACE][RES][HEADERS] ' +
          MVCRedactedHeaders(AContext.Response.CustomHeaders), ALogTag);
        Log.Debug(lRid + '[TRACE][RES][CT] ' + AContext.Response.ContentType, ALogTag);

        if (tfLogNonTextBody in AFlags) or
           TraceIsTextContentType(AContext.Response.ContentType) then
        begin
          if Assigned(AContext.Response.ContentStream) then
          begin
            lBytes := nil; // still the request's: SetLength would copy it first
            SetLength(lBytes, Max(Int64(0), Min(AContext.Response.ContentStream.Size, AMaxBodySize)));
            AContext.Response.ContentStream.Position := 0;
            AContext.Response.ContentStream.ReadBuffer(lBytes, Length(lBytes));
            AContext.Response.ContentStream.Position := 0;
            lBody := MVCTraceBodyText(lBytes, AContext.Response.ContentStream.Size,
              AContext.Response.ContentType);
          end
          else
            lBody := TraceClamp(AContext.Response.Content, AMaxBodySize);
          if not (tfLogBodySecrets in AFlags) then
            lBody := MVCTraceMaskSecrets(lBody);
          Log.Debug(lRid + '[TRACE][RES][BODY] ' + lBody, ALogTag);
        end
        else
          Log.Debug(lRid + '[TRACE][RES][BODY] <hidden non text content>', ALogTag);
      end;
    end;
end;

// --- Redirect (HTTPFilter) -----------------------------------------------

function BuildRedirectResponse(const AContext: TWebContext;
  const ATarget: string;
  const AKind: TMVCRedirectKind; const AQuery: TMVCRedirectQuery): Boolean;
var
  lTarget, lQS: string;
begin
  lTarget := ATarget;
  if AQuery = rdqPreserve then
  begin
    lQS := AContext.Request.QueryString;
    if lQS <> '' then
      lTarget := lTarget + '?' + lQS;
  end;
  AContext.Response.CustomHeaders.Values['Location'] := lTarget;
  if AKind = rdkPermanent then
    AContext.Response.StatusCode := http_status.MovedPermanently
  else
    AContext.Response.StatusCode := http_status.Found;
  LogI(Format('Redirect [%s] -> [%s]', [AContext.Request.PathInfo, lTarget]));
  Result := True;
end;

function RedirectHTTPFilter(const APaths: TArray<string>;
  const ATargetURL: string;
  const AKind: TMVCRedirectKind;
  const AQuery: TMVCRedirectQuery): TMVCHTTPFilter;
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
          BuildRedirectResponse(AContext, ATargetURL, AKind, AQuery);
          Exit; // short-circuit
        end;
      ANext();
    end;
end;

function RedirectHTTPFilter(const ARules: TArray<TPair<string, string>>;
  const AKind: TMVCRedirectKind;
  const AQuery: TMVCRedirectQuery): TMVCHTTPFilter;
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
          BuildRedirectResponse(AContext, lRule.Value, AKind, AQuery);
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
    function MediaTypes: TMVCStringDictionary;
  end;

  TRangeMediaState = class(TInterfacedObject, IRangeMediaState)
  strict private
    fURLPath: string;
    fDocumentRoot: string;
    fMediaTypes: TMVCStringDictionary;
  public
    constructor Create(const AURLPath, ADocumentRoot: string);
    destructor Destroy; override;
    function URLPath: string;
    function DocumentRoot: string;
    function MediaTypes: TMVCStringDictionary;
  end;

// the audio and video types of RangeMedia, also in the table of FileServerBundle
procedure AddRangeMediaTypes(const AMap: TMVCStringDictionary);
begin
  // Audio
  AMap.Add('.m4a',  'audio/mp4');
  AMap.Add('.mp3',  'audio/mpeg');
  AMap.Add('.ogg',  'audio/ogg');
  AMap.Add('.opus', 'audio/opus');
  AMap.Add('.wav',  'audio/wav');
  AMap.Add('.flac', 'audio/flac');
  AMap.Add('.aac',  'audio/aac');
  AMap.Add('.weba', 'audio/webm');
  // Video
  AMap.Add('.mp4',  'video/mp4');
  AMap.Add('.webm', 'video/webm');
  AMap.Add('.ogv',  'video/ogg');
  AMap.Add('.mkv',  'video/x-matroska');
  AMap.Add('.avi',  'video/x-msvideo');
  AMap.Add('.mov',  'video/quicktime');
end;

constructor TRangeMediaState.Create(const AURLPath, ADocumentRoot: string);
begin
  inherited Create;
  fURLPath := AURLPath.TrimRight(['/']);
  { GetFullPath: 'www/media' joined to AppPath keeps a mix of '/' and '\', while
    the full path of a file is all backslashes. The StartsWith check below would
    then fail for every file, and the folder that is right there would answer 404. }
  fDocumentRoot := TPath.GetFullPath(TPath.Combine(AppPath, ADocumentRoot));
  // Trailing separator makes the StartsWith check unambiguous —
  // '/app/media/' must not match the sibling '/app/media_evil/'.
  if not fDocumentRoot.EndsWith(TPath.DirectorySeparatorChar) then
    fDocumentRoot := fDocumentRoot + TPath.DirectorySeparatorChar;
  fMediaTypes := TMVCStringDictionary.Create;
  AddRangeMediaTypes(fMediaTypes);
end;

destructor TRangeMediaState.Destroy;
begin
  fMediaTypes.Free;
  inherited;
end;

function TRangeMediaState.URLPath: string;
begin
  Result := fURLPath;
end;

function TRangeMediaState.DocumentRoot: string;
begin
  Result := fDocumentRoot;
end;

function TRangeMediaState.MediaTypes: TMVCStringDictionary;
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

type
  // The bytes [AStart, AStart + ACount) of a file, read in place: a 206 is sent
  // from the file, without a copy, and a HEAD reads nothing. Owns the file.
  // Size can only shrink the window (the ETag middleware empties a 304 with Size := 0).
  TFileRangeStream = class(TStream)
  strict private
    fFile: TFileStream;
    fStart, fCount, fPosition: Int64;
  protected
    function GetSize: Int64; override;
    procedure SetSize(const NewSize: Int64); override;
  public
    constructor Create(const AFile: TFileStream; const AStart, ACount: Int64);
    destructor Destroy; override;
    function Read(var ABuffer; ACount: Longint): Longint; override;
    function Write(const ABuffer; ACount: Longint): Longint; override;
    function Seek(const AOffset: Int64; AOrigin: TSeekOrigin): Int64; override;
  end;

constructor TFileRangeStream.Create(const AFile: TFileStream; const AStart, ACount: Int64);
begin
  inherited Create;
  fFile := AFile;
  fStart := AStart;
  fCount := ACount;
  fPosition := 0;
  fFile.Position := AStart; // then only Seek moves it: the reads are sequential
end;

destructor TFileRangeStream.Destroy;
begin
  fFile.Free;
  inherited;
end;

function TFileRangeStream.GetSize: Int64;
begin
  Result := fCount;
end;

procedure TFileRangeStream.SetSize(const NewSize: Int64);
begin
  if NewSize < 0 then
    fCount := 0
  else if NewSize < fCount then
    fCount := NewSize;
end;

function TFileRangeStream.Read(var ABuffer; ACount: Longint): Longint;
begin
  if (ACount <= 0) or (fPosition >= fCount) then
    Exit(0);
  if ACount > fCount - fPosition then
    ACount := Longint(fCount - fPosition); // less than ACount: it fits
  Result := fFile.Read(ABuffer, ACount);
  Inc(fPosition, Result);
end;

function TFileRangeStream.Write(const ABuffer; ACount: Longint): Longint;
begin
  Result := 0; // read-only: WriteBuffer raises EWriteError
end;

function TFileRangeStream.Seek(const AOffset: Int64; AOrigin: TSeekOrigin): Int64;
begin
  if (AOrigin = soCurrent) and (AOffset = 0) then
    Exit(fPosition); // a read of Position: the file stays where it is
  case AOrigin of
    soBeginning:
      fPosition := AOffset;
    soCurrent:
      fPosition := fPosition + AOffset;
    soEnd:
      fPosition := fCount + AOffset;
  end;
  if fPosition < 0 then
    fPosition := 0;
  fFile.Position := fStart + fPosition;
  Result := fPosition;
end;

// The answer of RangeMedia for a file already resolved: 200 without a Range
// header, 206 with a single range, 416 with one it cannot satisfy. Shared with
// FileServerBundle, whose path checks and MIME table are those of StaticFiles.
procedure RangeMediaSendFile(const AContext: TWebContext; const AFileName, AContentType, ARangeHeader: string);
var
  lFileStream: TFileStream;
  lFileSize, lRangeStart, lRangeEnd: Int64;
begin
  lFileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    lFileSize := lFileStream.Size;

    AContext.Response.SetCustomHeader('Accept-Ranges',    'bytes');
    // Prevent Compression (if wrapped outside) from re-encoding the
    // body — would break Content-Range semantics.
    AContext.Response.SetCustomHeader('Content-Encoding', 'identity');
    // Chromium media cache ignores no-cache for <video>/<audio>.
    AContext.Response.SetCustomHeader('Cache-Control',    'no-store');

    if ARangeHeader.IsEmpty then
    begin
      AContext.Response.StatusCode := HTTP_STATUS.OK;
      lFileStream.Position := 0;
      AContext.Response.SetContentStream(lFileStream, AContentType);
      lFileStream := nil; // ownership transferred — guard finally
      Exit;
    end;

    if not RangeMediaParseHeader(ARangeHeader, lFileSize,
      lRangeStart, lRangeEnd) then
    begin
      AContext.Response.StatusCode := HTTP_STATUS.RequestedRangeNotSatisfiable;
      AContext.Response.SetCustomHeader('Content-Range',
        'bytes */' + IntToStr(lFileSize));
      Exit;
    end;

    // 206 Partial Content: the slice of the file, owned by the response from here.
    { Same cap as the RangeMedia middleware: see MAX_RANGE_SLICE there (a host
      that buffers the body, HTTP.sys, would hold the whole file for "bytes=0-"). }
    if (lRangeEnd - lRangeStart + 1) > MAX_RANGE_SLICE then
      lRangeEnd := lRangeStart + MAX_RANGE_SLICE - 1;
    AContext.Response.StatusCode := HTTP_STATUS.PartialContent;
    AContext.Response.SetCustomHeader('Content-Range',
      Format('bytes %d-%d/%d', [lRangeStart, lRangeEnd, lFileSize]));
    AContext.Response.SetContentStream(TFileRangeStream.Create(lFileStream, lRangeStart,
      lRangeEnd - lRangeStart + 1), AContentType);
    lFileStream := nil; // owned by the range stream
  finally
    lFileStream.Free;
  end;
end;

function RangeMediaHTTPFilter(const AURLPath: string;
  const ADocumentRoot: string): TMVCHTTPFilter;
var
  lState: IRangeMediaState;
begin
  lState := TRangeMediaState.Create(AURLPath, ADocumentRoot);
  Result :=
    procedure (const AContext: TWebContext;
               const ANext: TMVCHTTPFilterNext)
    var
      lPathInfo, lFileName: string;
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

      RangeMediaSendFile(AContext, lFileName, RangeMediaGetMediaType(lState, lFileName),
        AContext.Request.Headers['Range']);
    end;
end;

// --- StaticFiles (options-record overload) -------------------------------

class function TMVCStaticFilesOptions.Default: TMVCStaticFilesOptions;
begin
  Result.Prefix := '/static';
  Result.RootFolder := '.\www';
  Result.IndexDocument := 'index.html';
  Result.Charset := '';
  Result.Mode := sfmFiles; // as StaticFilesHTTPFilter(prefix, folder): files only
  Result.Flags := [];
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
    // StaticFilesHTTPFilter: []; FileServerBundle: its flags (fsfRanges: a Range
    // header gets the answer of RangeMedia)
    function Flags: TMVCFileServerFlags;
    procedure RunSanityCheckOnce;
  end;

  TStaticFilesState = class(TInterfacedObject, IStaticFilesState)
  strict private
    fOptions: TMVCStaticFilesOptions;
    fMediaTypes: TMVCStringDictionary;
    fResolvedRoot: string;
    fNormalizedPrefix: string;
    fFlags: TMVCFileServerFlags;
    fSanityCheckDone: Integer;
  public
    constructor Create(const AOptions: TMVCStaticFilesOptions; const AFlags: TMVCFileServerFlags);
    destructor Destroy; override;
    function Options: TMVCStaticFilesOptions;
    function MediaTypes: TMVCStringDictionary;
    function ResolvedRoot: string;
    function NormalizedPrefix: string;
    function Flags: TMVCFileServerFlags;
    procedure RunSanityCheckOnce;
  end;

constructor TStaticFilesState.Create(const AOptions: TMVCStaticFilesOptions; const AFlags: TMVCFileServerFlags);
begin
  inherited Create;
  fOptions := AOptions;
  fFlags := AFlags;
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
  if fsfRanges in fFlags then
    AddRangeMediaTypes(fMediaTypes);
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

function TStaticFilesState.Flags: TMVCFileServerFlags;
begin
  Result := fFlags;
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
  const AFileName: string; const AState: IStaticFilesState; const ACharset: string): Boolean;
var
  lContentType, lRangeHeader: string;
begin
  Result := False;
  if not TFile.Exists(AFileName) then
    Exit;
  if AState.MediaTypes.TryGetValue(LowerCase(ExtractFileExt(AFileName)), lContentType) then
    lContentType := BuildContentType(lContentType, ACharset)
  else
    lContentType := BuildContentType(TMVCMediaType.APPLICATION_OCTETSTREAM, '');
  lRangeHeader := '';
  if (fsfRanges in AState.Flags) and (AContext.Request.HTTPMethod in [httpGET, httpHEAD]) then
    lRangeHeader := AContext.Request.Headers['Range'];
  if lRangeHeader <> '' then
    RangeMediaSendFile(AContext, AFileName, lContentType, lRangeHeader)
  else
    // Delegate to TMVCStaticContents.SendFile so If-Modified-Since /
    // Last-Modified are honored (same as the classic middleware).
    TMVCStaticContents.SendFile(AFileName, lContentType, AContext);
  Result := True;
end;

// the filter of StaticFilesHTTPFilter and of FileServerBundle, on its state
function StaticFilesFilter(const AState: IStaticFilesState): TMVCHTTPFilter;
var
  lState: IStaticFilesState;
begin
  lState := AState;
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
      // with sffCaseSensitivePrefix. The trailing-/ tweak mirrors classic
      // line 254-267: a folder request missing the trailing slash should still
      // match the prefix so we can issue the 301 below.
      if not lPathInfo.StartsWith(lState.NormalizedPrefix, not (sffCaseSensitivePrefix in lOpts.Flags)) then
      begin
        if (not lPathInfo.EndsWith('/'))
          and (lPathInfo + '/').StartsWith(lState.NormalizedPrefix, not (sffCaseSensitivePrefix in lOpts.Flags)) then
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
        not (sffCaseSensitivePrefix in lOpts.Flags)) then
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
        if StaticFilesSendFileWithMime(AContext, lFileName, lState, lOpts.Charset) then
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
            TPath.Combine(lFullPathInfo, lOpts.IndexDocument), lState, lOpts.Charset) then
            Exit;
      end;

      // SPA fallback: a single bundle answers every client-routed URL with the
      // index.html of the nearest existing folder. The router goes first: the
      // routes of the application (an API next to the SPA) are never hidden by
      // index.html, only a GET/HEAD the router has no route for (no ActionName)
      // and answered with a 404 becomes the page.
      if (lOpts.Mode = sfmSPA) and (lOpts.IndexDocument <> '')
        and (AContext.Request.HTTPMethod in [httpGET, httpHEAD]) then
      begin
        ANext();
        if (AContext.ActionName = '') and (AContext.Response.StatusCode = HTTP_STATUS.NotFound) then
        begin
          while (lFullPathInfo <> '') and (not TDirectory.Exists(lFullPathInfo)) do
            lFullPathInfo := TDirectory.GetParent(lFullPathInfo);
          if lFullPathInfo <> '' then
          begin
            lFileName := TPath.GetFullPath(
              TPath.Combine(lFullPathInfo, lOpts.IndexDocument));
            if TFile.Exists(lFileName) then
            begin
              AContext.Response.StatusCode := HTTP_STATUS.OK;
              AContext.Response.ReasonString := 'OK';
              AContext.Response.Content := '';
              StaticFilesSendFileWithMime(AContext, lFileName, lState, lOpts.Charset);
            end;
          end;
        end;
        Exit;
      end;

      // No match — fall through to routing / handlers (will likely 404).
      ANext();
    end;
end;

function StaticFilesHTTPFilter(const AOptions: TMVCStaticFilesOptions): TMVCHTTPFilter;
var
  lState: IStaticFilesState;
begin
  lState := TStaticFilesState.Create(AOptions, []);
  Result := StaticFilesFilter(lState);
end;

// --- FileServerBundle ----------------------------------------------------

class function TMVCFileServerOptions.Default: TMVCFileServerOptions;
begin
  // the defaults of TMVCStaticFilesOptions, plus the ranges
  Result.URLPrefix := '/static';
  Result.DocumentRoot := '.\www';
  Result.Mode := fsmFiles;
  Result.DefaultDocument := 'index.html';
  Result.Charset := '';
  Result.Flags := [fsfRanges];
  Result.MediaTypesCustomizer := nil;
  Result.Rules := nil;
end;

function FileServerBundle(const AURLPrefix, ADocumentRoot: string;
  const AMode: TMVCFileServerMode): IMVCFilterBundle;
var
  lOptions: TMVCFileServerOptions;
begin
  lOptions := TMVCFileServerOptions.Default;
  lOptions.URLPrefix := AURLPrefix;
  lOptions.DocumentRoot := ADocumentRoot;
  lOptions.Mode := AMode;
  Result := FileServerBundle(lOptions);
end;

function FileServerBundle(const AOptions: TMVCFileServerOptions): IMVCFilterBundle;
var
  lOptions: TMVCStaticFilesOptions;
  lState: IStaticFilesState;
begin
  // one filter: the 206 of RangeMedia is how it sends a file asked with Range, so
  // the root, the path checks, Rules and the MIME table of StaticFiles hold for both
  lOptions.Prefix := AOptions.URLPrefix;
  lOptions.RootFolder := AOptions.DocumentRoot;
  lOptions.IndexDocument := AOptions.DefaultDocument;
  lOptions.Charset := AOptions.Charset;
  if AOptions.Mode = fsmSPA then
    lOptions.Mode := sfmSPA
  else
    lOptions.Mode := sfmFiles;
  if fsfCaseSensitivePrefix in AOptions.Flags then
    lOptions.Flags := [sffCaseSensitivePrefix]
  else
    lOptions.Flags := [];
  lOptions.MediaTypesCustomizer := AOptions.MediaTypesCustomizer;
  lOptions.Rules := AOptions.Rules;
  lState := TStaticFilesState.Create(lOptions, AOptions.Flags);
  Result := TMVCFilterBundle.Create([StaticFilesFilter(lState)], []);
end;

// --- Sessions, RateLimit with options (HTTPFilters) -----------------------

function SessionHTTPFilter(const AFactory: TMVCWebSessionFactory): TMVCHTTPFilter;
var
  lHolder: ISessionFactoryHolder;
begin
  lHolder := TSessionFactoryHolder.Create(AFactory);
  Result :=
    procedure (const AContext: TWebContext;
               const ANext: TMVCHTTPFilterNext)
    begin
      AContext.SetSessionFactory(lHolder.Factory);
      ANext();
    end;
end;

function MemorySessionHTTPFilter(const ATimeoutInMinutes: Integer;
  const AFlags: TMVCSessionCookieFlags): TMVCHTTPFilter;
begin
  Result := SessionHTTPFilter(TMVCWebSessionMemoryFactory.Create(scfHttpOnly in AFlags, ATimeoutInMinutes, scfSecure in AFlags));
end;

function FileSessionHTTPFilter(const ATimeoutInMinutes: Integer;
  const ASessionFolder: string;
  const AFlags: TMVCSessionCookieFlags): TMVCHTTPFilter;
begin
  Result := SessionHTTPFilter(TMVCWebSessionFileFactory.Create(scfHttpOnly in AFlags, ATimeoutInMinutes, ASessionFolder, scfSecure in AFlags));
end;

class function TMVCRateLimitOptions.Default: TMVCRateLimitOptions;
begin
  // every field: a record result may be the caller's variable, with what it held
  Result.MaxRequests := 60;
  Result.WindowSeconds := 60;
  Result.KeyType := rlkIPAddress;
  Result.KeyExtractor := nil;
  Result.APIKeyHeaderName := '';
  Result.HeaderPrefix := '';
  Result.ExcludedPaths := nil;
  Result.OnExceeded := nil;
  Result.Storage := nil;
end;

function RateLimitHTTPFilter(const AOptions: TMVCRateLimitOptions): TMVCHTTPFilter;
var
  lOptions: TMVCRateLimitOptions;
  I: Integer;
begin
  lOptions := AOptions;
  if not Assigned(lOptions.Storage) then
    lOptions.Storage := TMVCInMemoryRateLimitStorage.Create(rlsFixedWindow);
  if lOptions.APIKeyHeaderName = '' then
    lOptions.APIKeyHeaderName := 'X-API-Key';
  if lOptions.HeaderPrefix = '' then
    lOptions.HeaderPrefix := 'X-RateLimit-';
  // a copy in lower case: the array of the caller is not touched
  lOptions.ExcludedPaths := nil;
  SetLength(lOptions.ExcludedPaths, Length(AOptions.ExcludedPaths));
  for I := 0 to High(AOptions.ExcludedPaths) do
    lOptions.ExcludedPaths[I] := AOptions.ExcludedPaths[I].ToLower;
  Result :=
    procedure(const AContext: TWebContext; const ANext: TMVCHTTPFilterNext)
    begin
      if not MVCRateLimitExceeded(AContext, lOptions) then
        ANext(); // otherwise the 429 is written
    end;
end;

initialization

gAnalyticsLoggerLock := TObject.Create;

finalization

gAnalyticsLoggerLock.Free;

end.
