# Changelog

All notable changes to DelphiMVCFramework will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [3.5.0-silicon] - Unreleased

### ⚠ BREAKING CHANGES

Several defaults have been flipped. The first two affect the JSON
output of `OKResponse(TObject)` and are one-line overrides to restore
the pre-3.5 behaviour if needed; 4 to 7 are security defaults.

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

**3. `TMVCListener` is now an Indy Direct server - no WebBroker dependency**

`TMVCListener` / `TMVCListenerProperties` (`MVCFramework.Server`) previously
required a `TWebModuleClass` and ran on `TIdHTTPWebBrokerBridge`. They now host
a `TMVCEngine` on the direct Indy backend (`TMVCIndyServer`) with no WebBroker
layer. The configuration API changed accordingly:

- Removed: `SetWebModuleClass` / `WebModuleClass`.
- Removed: `SetSSLOptions` and the SSL listener properties. TLS belongs to the
  server backend, not to `TMVCListener`.
- Added: `SetConfigAction(TProc<TMVCConfig>)` - sets engine config keys, applied
  while the engine is being created.
- Added: `SetEngineConfig(TProc<TMVCEngine>)` - wires controllers and
  middleware, applied after the engine is created.

Migration: move the body of the old WebModule's `WebModuleCreate` (the
`AddController` / `AddMiddleware` calls) into a `SetEngineConfig` proc, and any
`TMVCConfig` key assignments into `SetConfigAction`.

Before:
```pascal
TMVCListener.Create(TMVCListenerProperties.New
  .SetName('App').SetPort(8080)
  .SetWebModuleClass(TMyWebModule));
```
After:
```pascal
TMVCListener.Create(TMVCListenerProperties.New
  .SetName('App').SetPort(8080)
  .SetEngineConfig(
    procedure(AEngine: TMVCEngine)
    begin
      AEngine.AddController(TMyController);
      AEngine.AddMiddleware(UseMemorySessionMiddleware(0));
    end));
```

**4. `X-HTTP-Method-Override` no longer changes the routed verb**

A `POST` carrying `X-HTTP-Method-Override: DELETE` (or `X-HTTP-Method`, or
`X-METHOD-OVERRIDE`) now routes as `POST`. Earlier 3.x changelogs list the header
as supported, and on **Indy Direct** and on WebBroker hosted by
`TIdHTTPWebBrokerBridge` it did work - Indy rewrote the verb itself before the
framework saw the request. It never worked under ISAPI, Apache or HTTP.sys, so the
same application routed differently depending on its host, and a verb a client can
rewrite with a header is a way past any proxy rule that allows only `POST`.

Migration: a client that tunnels verbs must send the real one. An application that
genuinely needs tunnelling can read the header itself - `Context.Request.Headers`
still returns it - and dispatch explicitly.

**5. `IMVCRESTClient` refuses a server certificate the platform rejected**

Without a validation proc installed, the client used to answer "accept" for every
certificate that had already failed platform validation: self-signed, expired,
wrong host name, unknown CA - man-in-the-middle included. It now keeps the RTL's
refusal. `SetValidateServerCertificateProc` still decides per certificate; for a
lab or a test against a self-signed host, the process-wide switch is:

```pascal
MVCRESTClientAcceptInvalidCertificates := True;
```

**6. `TMVCSSEClient` refuses a rejected certificate by default**

`TMVCSSEClient.Create(AURL, AIgnoreCertificateErrors)` now defaults the second
argument to `False`, for the same reason. There is no process-wide switch for the
SSE client: pass `True` at the call site for a host you control and whose
certificate you cannot fix.

**7. The system controllers are off by default**

`TMVCConfigKey.LoadSystemControllers` now defaults to `'false'` (it was `'true'`), and the
wizard writes `dmvc.load_system_controllers=false` in `.env` and reads it with `'false'` as the
default. `TMVCSystemController` answers `/system/describeserver.info` (every controller and
action), `/system/describeplatform.info` and `/system/serverconfig.info` (the whole
`TMVCConfig`): localhost only, but the map and the configuration of the application, so they
are opt-in. To get them back:

```pascal
Config[TMVCConfigKey.LoadSystemControllers] := 'true';
```

### Added

- **`TMVCCSRFMiddleware`: CSRF protection for cookie-authenticated web apps.**
  Stateless synchronizer token: the token is the HMAC-SHA256 of a session id
  that the caller supplies (`TMVCCSRFMiddleware.TokenFor(Secret, SessionId)`),
  so it does not depend on the session type (the JWT of the auth cookie works).
  POST, PUT, PATCH, DELETE and QUERY (it has a body, like POST) must carry it in the `X-CSRF-Token` header or in
  the `csrf_token` form field, otherwise the answer is 403 and the action is
  not run; the comparison is constant-time, a request without a session is
  refused, paths listed as exempt skip the check, and an `Origin` header that
  does not match `Host` is refused (`Host` header only: `X-Forwarded-Host` is not trusted). Exempt
  entries are matched case-sensitively against `PathInfo` (without the engine
  base path), on a `/` boundary. Optional `AAllowedHosts` (`host[:port]`, case-insensitive, no wildcards;
  invalid entries raise) accepts extra `Origin` hosts for apps behind a proxy that rewrites `Host`. The secret must be at least 16 characters; exempt entries must start
  with `/` and cannot be `/` alone (it would disable the check): `Create` raises.
  New unit `MVCFramework.Middleware.CSRF`;
  additive.

- **`StreamItems` and `StreamEach`: stream a lazy sequence of objects.** An
  action returns a `TMVCStreamedResponse` built from a source that produces
  objects one at a time, so nobody has to hand the framework the whole list:
  `StreamItems<T>(function(out AItem: T): Boolean ...)` is called for the next
  item only after the previous one is on the socket (it also takes an
  `IEnumerable<T>`), and `StreamEach<T>(procedure(const AYield: TMVCYield<T>) ...)`
  is the same written as a plain loop, like `yield` in a Python generator. An
  `AOnDone` callback runs at the end, also when the client disconnects or an
  item fails, to close cursors and files. The format follows the `Accept`
  header: JSON array, `application/x-ndjson` or `text/event-stream`. Indy
  Direct and HTTP.sys; WebBroker answers 501 as for `StreamDataSet`. Additive:
  nothing changes for existing actions. A streamed response that fails before
  its first byte is now answered with the normal error response instead of an
  empty one. Showcase in `samples/streamed_array_writer`; `samples/streamed_array_client` reads the stream line by
  line and prints the peak memory of the client (about 11 MB as JSON Lines against about 64 MB as an array, for
  200,000 rows).

- **`StreamDataSet` answers in JSON Lines on request.** A client sending
  `Accept: application/x-ndjson` receives one record per line
  (`application/x-ndjson`, no brackets, no commas) instead of the JSON array,
  with the same flat server memory. Without that header nothing changes: the
  action stays `Result := StreamDataSet(qry)` and the response is the same
  array as before. New constant `TMVCMediaType.APPLICATION_NDJSON`.

- **Request guards as HTTP filters.** `MVCFramework.Filters` gets three
  engine-wide filters that decide who may talk to a path and from where, each
  with an optional `APathPrefix` (the full routed path they guard, `''` for
  every request):
  - `BearerAuthHTTPFilter(validator, prefix)`: `Authorization: Bearer <token>` is
    checked by the validator (`function(const AToken: string; out AUserName:
    string; var ARoles: TArray<string>): Boolean`); on success it fills
    `Context.LoggedUser`, otherwise 401 with `WWW-Authenticate: Bearer`.
    `StaticBearerTokens` is the validator for a fixed set of tokens (constant-
    time comparison; an empty or space-padded token is refused at startup).
  - `AllowedOriginHTTPFilter(origins, prefix)`: 403 when a request carries an
    `Origin` that is not in the list. `CORSHTTPFilter` only decides what the browser
    may read, this one stops the request on the server.
  - `AllowedHostHTTPFilter(hosts, prefix)`: 403 when the `Host` header (port
    ignored) is not `localhost`, `127.0.0.1`, `[::1]` or a declared host: DNS
    rebinding on a service that listens on loopback.

  The path is normalised as the router does (case, trailing `/`, query) and
  tested in its raw and URL-decoded forms, so `/MCP/` or `%2f` do not slip past
  a guard; `MVCNormalizePath` and `MVCRequestTargetsPath` are public for custom
  guards. The `MCPGuard` filter of mcp-server-delphi is gone: the wizard's MCP
  projects use these filters plus `MCPEndpoint` (unit `MVCFramework.MCP.Endpoint`).

- **Sessions, ActiveRecord and the rate limit with options as HTTP filters.**
  `MVCFramework.Filters` gets the engine-wide counterparts of three classic
  middleware, so a controller-based application can use them (the endpoint
  filters `MemorySessionEndpointFilter`, `FileSessionEndpointFilter`, `DatabaseSessionEndpointFilter` and `ActiveRecordEndpointFilter`
  attach to Minimal API route groups only):
  - `MemorySessionHTTPFilter(timeout, flags)`,
    `FileSessionHTTPFilter(timeout, folder, flags)` and
    `DatabaseSessionHTTPFilter(timeout, flags)` put the session factory on every
    request; `DatabaseSessionHTTPFilter` needs `ActiveRecordHTTPFilter` registered before
    it. `flags` is a set, `TMVCSessionCookieFlags` (`[scfHttpOnly]` by default),
    so the call site says what it turns on:
    `FileSessionHTTPFilter(10, 'sessions', [scfHttpOnly, scfSecure])`. For a Secure
    setting read at run time, `SessionCookieFlags(dotEnv.Env('SESSION_COOKIE_SECURE', True))`
    builds the set. The endpoint filters `MemorySessionEndpointFilter`, `FileSessionEndpointFilter` and
    `DatabaseSessionEndpointFilter` take the same `flags`, and `RedirectHTTPFilter` takes a
    `TMVCRedirectKind` (`rdkTemporary`, the default, or `rdkPermanent`) instead
    of a Boolean: `RedirectHTTPFilter(['/old'], '/new', rdkPermanent)`.
  - `ActiveRecordHTTPFilter(connectionDefName, connectionDefFileName)` (and an
    overload with additional connections) acquires the connection(s) before the
    request is dispatched and releases them in a `finally`: the connection goes
    back to the pool when the action raises too. It does not wrap the classic
    middleware, and the additional connections work for every filter instance
    (the middleware set them up for the first instance of the process only).
  - `RateLimitHTTPFilter(TMVCRateLimitOptions)`: the same options and the same answer as
    `TMVCRateLimitMiddleware` (key by IP, user, API key or custom, excluded path
    prefixes, `X-RateLimit-*` header prefix, a callback when a request is
    refused; 429 + `Retry-After` + JSON body). `RateLimitHTTPFilter(max, window)` is
    unchanged.

- **`MVCFramework.Filters` depends on the core only; the optional filters moved to
  companion units.** A consumer of `MVCFramework.Filters` no longer pulls in
  FireDAC, `TMVCActiveRecord`, SwagDoc or the OpenAPI emitter, the same way
  `MVCFramework.Filters.Redis` keeps the Redis client out. Add the unit of the
  filter you use:

  | Unit | Filters |
  |------|---------|
  | `MVCFramework.Filters.ActiveRecord` | `ActiveRecordEndpointFilter`, `ActiveRecordHTTPFilter`, `DatabaseSessionEndpointFilter`, `DatabaseSessionHTTPFilter` |
  | `MVCFramework.Filters.Swagger` | `SwaggerHTTPFilter` |
  | `MVCFramework.Filters.OpenAPI` | `OpenAPIHTTPFilter` |
  | `MVCFramework.Filters.Redis` | `RateLimitRedisHTTPFilter` |

  The signatures do not change. `MVCFramework.Filters` gets `SessionEndpointFilter(factory)`
  and `SessionHTTPFilter(factory)`, the endpoint and the engine-wide filter for any
  `TMVCWebSessionFactory` (the filter owns it); the memory, file and database
  session filters are built on them. The generated projects add the units they
  need.

- **`StaticFilesHTTPFilter`: one implementation, and single-page applications in the simple
  form.** `StaticFilesHTTPFilter(prefix, folder)` used to be a second, smaller
  implementation next to `StaticFilesHTTPFilter(TMVCStaticFilesOptions)`, with its own MIME
  table. It now builds the options and calls the full filter, so the two forms
  behave the same way: `Last-Modified` and `304` on `If-Modified-Since`, `index.html`
  in sub folders, a folder requested without the trailing `/` answers `301`, the
  prefix is case-insensitive and accepts a trailing `/` (`'/static/'`), a path out
  of the root answers `404` (the simple form answered `403`), and a missing root
  folder raises on the first request under the prefix instead of falling through
  to routing. The third parameter is now a mode, not the default document:
  `StaticFilesHTTPFilter('/app', 'www/app', sfmSPA)` serves the nearest `index.html` for
  every URL that is not a file, so the client-side router takes over;
  `sfmFiles` (the default) lets the request go on to routing. The default
  document moved to the fourth parameter. `TMVCStaticFilesOptions.Default`
  now has `SPAWebAppSupport = False`, as the simple form (it was `True`). With
  `sfmSPA` the router goes first: only a GET/HEAD for which the application has
  no route, and which would be a `404`, is answered with `index.html`. The routes
  of the application are never hidden by it, so the SPA can sit at `/` next to
  an API. It needs `AllowUnhandledAction = false` (the default), and the router
  still logs `ROUTE NOT FOUND` for each client-side route.

- **Engine-wide endpoint filters: post-routing filters for controllers.** An
  endpoint filter registered with `AEngine.Use(...)` runs for every
  route the router finds, controller actions and Minimal API routes. The endpoint
  filters that existed for Minimal API route groups only (`BasicAuthEndpointFilter`,
  `AuthorizeEndpointFilter`, `RequireRoleEndpointFilter`, `MemorySessionEndpointFilter`, `FileSessionEndpointFilter`,
  `DatabaseSessionEndpointFilter`, `ActiveRecordEndpointFilter`, `CORSEndpointFilter`, and the JWT check, now
  part of `JWTAuthenticationBundle`) work for controllers this way. `TWebContext` gets
  `ControllerQualifiedClassName` and `ActionName` (for a route: an empty class name
  and the verb + pattern). The first registered is the outermost; `ANext()` runs the
  action and returns `nil` for a controller (its answer is already written), and a
  filter that does not call it answers the request. `TMVCEndpointFilter` and
  `TMVCEndpointFilterNext` are declared in `MVCFramework`; the names stay valid from
  `MVCFramework.MinimalAPI`. Without endpoint filters nothing changes on the path
  of a request.

- **Filter bundles and a single `Use`.** `IMVCFilterBundle` (`MVCFramework`) is a
  set of filters that go together: `HTTPFilters` and `EndpointFilters`, each list in
  the order the bundle runs it. `TMVCFilterBundle.Create(AHTTPFilters, AEndpointFilters)`
  builds one without implementing the interface. One method registers every kind:
  - `TMVCEngine.Use` takes a `TMVCHTTPFilter`, a `TMVCEndpointFilter` or an
    `IMVCFilterBundle`, where the call is, and returns the engine;
  - `TMVCRouteGroup<T>.Use` takes an endpoint filter or a bundle: the endpoint part
    goes on the group, the HTTP part on the engine of the group, because it runs
    before routing. An HTTP filter on a group does not compile.

  The HTTP part of a bundle is registered once, however many groups use the bundle.
  The same bundle on the engine and on a group (in either order), twice on the
  engine, or on a group and on a group nested in it, raises `EMVCConfigException` at
  the second `Use`: its check would run twice on the same routes. On two groups side
  by side it is allowed. A bundle with an HTTP part only (`FileServerBundle`) on a
  group raises too: its filters run before routing for every request, the group
  would protect nothing of it. An empty bundle registers nothing; a nil one raises.
  The HTTP part is on the engine as soon as the group's `Use` returns: discarding the
  result (`AGroup.Use(B);`) keeps the login of `B` and loses its check. The name of
  the argument says its kind: it ends in `HTTPFilter`, `EndpointFilter` or `Bundle`.

  ```pascal
  AEngine.Use(CompressionHTTPFilter(1024));
  AEngine.Use(JWTAuthenticationBundle(TAuthentication.Create, lClaimsSetup, dotEnv.Env('JWT_SECRET')));
  AEngine.Use(TMVCFilterBundle.Create([RequestLogHTTPFilter()], [AuthorizeEndpointFilter()]));
  lAdmin := AGroup.Use(RequireRoleEndpointFilter('admin'));
  ```

- **Authentication bundles** (`MVCFramework.Filters.Authentication`, new unit). Each
  mechanism is one function that builds all its parts with the same handler, secret
  and URLs, registered with one `Use` on the engine or on a route group:

  | Function | HTTP part (on the engine, in order) | Endpoint part |
  |---|---|---|
  | `JWTAuthenticationBundle` | [blacklist] → login → [refresh token] | the JWT check (`Authorization: Bearer`) |
  | `JWTCookieAuthenticationBundle` | [allowed origins of the login] → login + logout | the JWT check (cookie) |
  | `BasicAuthenticationBundle(handler, realm)` | | `BasicAuthEndpointFilter(handler, realm)` |
  | `CustomAuthenticationBundle(handler, loginURL)` | login + logout | the check of the session |
  | `OIDCAuthenticationBundle(TMVCOIDCOptions)` (`MVCFramework.Filters.OIDC`) | login + callback + logout | the check |

  - The JWT bundles have a short form, `JWTAuthenticationBundle(handler, claimsSetup,
    secret, loginURL = '/login')` and `JWTCookieAuthenticationBundle(handler,
    claimsSetup, secret, cookie, loginURL, logoutURL)`, and a form with an options
    record, `TMVCJWTAuthenticationOptions` / `TMVCJWTCookieAuthenticationOptions`
    (start from `.Default`): `Secret` (with `HMACAlgorithm`, HS512) or `Signer`
    (RS256 & co.), `LoginURL`, `ClaimsToCheck` (`[ExpirationTime]`) and
    `LeewaySeconds` (300). The bearer bundle adds `AccessTokenParamName` (`''`: the
    header only), the blacklist (`OnAcceptToken` + `OnNewJWTToBlackList` at
    `BlackListURL`, `/logout`) and the refresh token; the cookie bundle adds
    `Cookie: TMVCAuthCookieOptions`, `LogoutURL` and `LoginAllowedOrigins` (empty: the
    `Origin` of the login is not checked, otherwise `AllowedOriginHTTPFilter` on the
    login). An optional part is on when what it needs is assigned (both blacklist
    callbacks, the refresh store, a non-empty list of origins): there is no Boolean
    switch.
  - **Refresh token.** With `RefreshTokenStore` assigned, the login of
    `JWTAuthenticationBundle` answers `{"access_token": ..., "refresh_token": ...,
    "token_type": "bearer", "expires_in": ...}`, the body of the refresh URL, and the
    bundle adds `RefreshTokenHTTPFilter` at `RefreshToken.RefreshURLSegment`
    (`/auth/refresh`); the tokens are signed with the secret or the signer of the bundle. Both access
    tokens carry `username`, `roles`, the issuer and the lifetime of `RefreshToken`:
    `ClaimsSetup` must be nil (the bundle raises otherwise), and the session data of
    the handler is not in the tokens. `TMVCRefreshTokenConfig` gets `Signer` (nil:
    `Secret` + `HMACAlgorithm`, as before). `RefreshTokenHTTPFilter` stays public for
    a login written in a controller.
  - **Logout with the refresh token.** With the blacklist and `RefreshTokenStore`, a
    request to `BlackListURL` whose JSON body is `{"refresh_token": ...}` also revokes
    that refresh token and its family; without that body only the access token is
    logged out. The blacklist reads the token as the check does, from the
    `AccessTokenParamName` parameter too when the header is missing
    (`TMVCJWTBlackListMiddleware.Create` has an optional `AccessTokenParamName`,
    `''` by default: the header only, as before). The store must be one for the whole
    process: under WebBroker, ISAPI and Apache every web module has its own engine.
  - **No login part.** `LoginURL = ''` leaves the login out of `JWTAuthenticationBundle`
    (as `AccessTokenParamName = ''` leaves the parameter out): the bundle has the check, plus
    the blacklist and the refresh URL when they are set, for the tokens of a login of your
    own signed with the same secret or signer, and `ClaimsSetup` must be nil. That login
    must be outside the check: with the bundle on the engine a Minimal API route is
    always checked, so it is a controller action the handler's `OnRequest` makes
    public, or the bundle goes on the route group it protects. The cookie bundle needs
    its login: an empty `LoginURL` raises.
  - The cookie bundle has no blacklist: `TMVCJWTBlackListMiddleware` reads the
    `Authorization` header, or the `AccessTokenParamName` parameter, never the cookie.
  - A bundle raises when it is built with a nil handler, a nil claims setup (with its
    login, without the refresh token), an empty
    secret, the secret shipped with the framework, `Secret` and `Signer` together, or
    one blacklist callback without the other.
  - `BasicAuthEndpointFilter`, `AuthorizeEndpointFilter`, `RequireRoleEndpointFilter`,
    `CSRFHTTPFilter` / `CSRFEndpointFilter` and `RefreshTokenHTTPFilter` stay public:
    each one is complete on its own.

  ```pascal
  uses MVCFramework.Filters.Authentication;

  AEngine.Use(JWTAuthenticationBundle(TAuthentication.Create, lClaimsSetup, dotEnv.Env('JWT_SECRET')));

  // login + refresh token
  lOptions := TMVCJWTAuthenticationOptions.Default;
  lOptions.Handler := TAuthentication.Create;
  lOptions.Secret := dotEnv.Env('JWT_SECRET');
  lOptions.RefreshTokenStore := gRefreshStore; // one for the process, created in initialization
  AEngine.Use(JWTAuthenticationBundle(lOptions));
  ```

- **`FileServerBundle`: a folder served with one `Use`** (`MVCFramework.Filters`).
  `FileServerBundle(prefix, folder, mode)` serves the files as `StaticFilesHTTPFilter`
  does (MIME type, `Last-Modified` and 304, default document, the 301 of a folder,
  path traversal refused) and answers a request with a `Range` header with
  `206 Partial Content`, as `RangeMediaHTTPFilter` does (416 past the end of the
  file). The mode is a `TMVCFileServerMode`: `fsmFiles` (the default) lets a request
  that is not a file go on to routing; with `fsmSPA` a GET/HEAD the router has no
  route for gets the nearest `index.html`, as `StaticFilesHTTPFilter(..., sfmSPA)`,
  so an API keeps its routes on the same prefix. `FileServerBundle(TMVCFileServerOptions)`
  takes the rest: `DefaultDocument`, `Charset`, `MediaTypesCustomizer`, `Rules` and
  `Flags`, a set of `TMVCFileServerFlag` (`[fsfRanges]` by default;
  `fsfCaseSensitivePrefix`). The Range answer is part of the file server: root,
  prefix, `Rules`, traversal checks and MIME table (the static files' types plus
  the audio/video types of `RangeMediaHTTPFilter`) are the same with and without
  `Range`. The bundle has an HTTP part only. `StaticFilesHTTPFilter` and
  `RangeMediaHTTPFilter` stay.

  ```pascal
  AEngine.Use(FileServerBundle('/', TPath.Combine(AppPath, 'www')));          // files + Range
  AEngine.Use(FileServerBundle('/', TPath.Combine(AppPath, 'www'), fsmSPA));  // + the SPA fallback
  ```

- **Breaking (rc8 → rc9): `Use` replaces `UseHTTPFilter` and `UseEndpointFilter`.**
  `AEngine.UseHTTPFilter(f)` and `AEngine.UseEndpointFilter(f)` are `AEngine.Use(f)`;
  the overload follows the type of the argument. There are no aliases: the methods
  were born in the 3.5 RC cycle. A filter function without arguments needs its
  brackets inside `Use`, or the compiler cannot choose the overload (E2250):
  `AEngine.Use(ETagHTTPFilter())`, `lGroup.Use(AuthorizeEndpointFilter())`.

- **Breaking (rc8 → rc9): the authentication filters are inside their bundles.** The
  login and check filters of each mechanism are no longer public: one half alone, or
  the two halves with different parameters, was a misconfiguration the compiler
  could not see. Each pair is one `Use`:

  | Before | Now |
  |---|---|
  | `JWT(handler, claimsSetup, secret, loginURL, ...)` (rc8), `JWTLoginHTTPFilter` + `JWTEndpointFilter` | `JWTAuthenticationBundle(handler, claimsSetup, secret, loginURL)`, or `JWTAuthenticationBundle(TMVCJWTAuthenticationOptions)` |
  | `JWTCookieLoginHTTPFilter` + `JWTCookieEndpointFilter` | `JWTCookieAuthenticationBundle(...)`; the origin check of the login is `LoginAllowedOrigins` |
  | `JWTBlackListHTTPFilter` | `OnAcceptToken` + `OnNewJWTToBlackList` (+ `BlackListURL`) of `TMVCJWTAuthenticationOptions` |
  | `CustomAuthLoginHTTPFilter` + `CustomAuthEndpointFilter` | `CustomAuthenticationBundle(handler, loginURL)` |
  | `OIDCLoginHTTPFilter` + `OIDCEndpointFilter` | `OIDCAuthenticationBundle(TMVCOIDCOptions)` |

  On a Minimal API group, `AGroup.Use(JWTAuthenticationBundle(...))` puts the check
  on the group and the login on the engine. rc8's `JWT(...)` answered the login only
  when a route matched and returned the token in the `Authorization` response header;
  the login of the bundle is an HTTP filter and answers `{"token": ...}`.

- **The wizard generates filters.** A new controller or web project registers
  `RedirectHTTPFilter`, `FileServerBundle`, `AnalyticsHTTPFilter`, `TraceHTTPFilter`, `CORSHTTPFilter`,
  `ETagHTTPFilter`, `CompressionHTTPFilter`, `RateLimitHTTPFilter`, `SwaggerHTTPFilter`, `ActiveRecordHTTPFilter` and the session
  filters with `AEngine.Use`, in one chain. JWT is generated as a bundle:
  `JWTAuthenticationBundle` (bearer) or `JWTCookieAuthenticationBundle`
  (server-side views), with the secret or the RS256 signer; the Minimal API
  project gets its login at `/api/v1/admin/login` from the bundle on the admin
  group in `Routes.RootU` (`lAdmin := AGroup.Use(JWTAuthenticationBundle(...,
  '/api/v1/admin/login'))`, the login goes on the engine; rc8 had an endpoint filter
  that answered only if a route matched), and the token comes back as `{"token": ...}` (rc8 put it in the
  `Authorization` response header). The generated check no longer accepts
  `?access_token=`. The cookie flavour refuses a `POST /login` from an origin
  that is not in `LOGIN_ALLOWED_ORIGINS` (`.env`, the localhost origins of the
  project by default), so a page of another site cannot log the browser in to
  its own account (`LoginAllowedOrigins` of the bundle options); the server does
  not start when the key is empty, because the bundle checks no origin with an
  empty list. Static files and server-side views get `FileServerBundle('/static', ...)`,
  which also answers `Range` requests; the Swagger UI stays on `StaticFilesHTTPFilter`.
  The Minimal API WebApp helpers are `RequireLoginEndpointFilter` and
  `PageDefaultsEndpointFilter`. RS256 is ignored for Minimal API projects (their templates
  sign with `JWT_SECRET`). With JWT (bearer) and Swagger the document gets the
  bearer scheme; the `/login` operation is documented only by the classic JWT
  middleware. Minimal API projects already used filters. The project menu
  detects an OpenAPI project from `MVCFramework.Swagger.Commons` instead of
  `MVCFramework.Middleware.Swagger`.

- **Filters for every classic authentication middleware.** Each one runs the
  code of its middleware, so it checks and answers the same way. The parts that
  need no route (login, logout, the OIDC callback) are HTTP filters; the check
  of the request is an endpoint filter, engine-wide (`AEngine.Use`, for
  controllers and Minimal API routes) or on a route group (`AGroup.Use`). An
  authentication mechanism is a bundle (see "Authentication bundles"), so one
  `Use` puts both parts in place:

  | Middleware | Use |
  |------------|-----|
  | `TMVCJWTAuthenticationMiddleware` | `JWTAuthenticationBundle` |
  | `TMVCJWTCookieAuthenticationMiddleware` | `JWTCookieAuthenticationBundle` |
  | `TMVCJWTBlackListMiddleware` | `OnAcceptToken` + `OnNewJWTToBlackList` of `TMVCJWTAuthenticationOptions` |
  | `TMVCJWTRefreshTokenMiddleware` | `RefreshTokenStore` of `TMVCJWTAuthenticationOptions` (with a login of your own, `LoginURL := ''` and the login outside the check) |
  | `TMVCBasicAuthenticationMiddleware` | `BasicAuthenticationBundle(handler)` or `BasicAuthEndpointFilter(handler)` |
  | `TMVCCustomAuthenticationMiddleware`, `TMVCRoleBasedAuthMiddleware` | `CustomAuthenticationBundle` |
  | `TMVCOIDCAuthenticationMiddleware` | `OIDCAuthenticationBundle` (`MVCFramework.Filters.OIDC`, options in `TMVCOIDCOptions`) |
  | `TMVCCSRFMiddleware` | `CSRFHTTPFilter` or `CSRFEndpointFilter` |

  - For a controller action the handler's `OnRequest` and `OnAuthorization`
    get the controller and the action, as with the middleware. A Minimal API
    route has no controller: the bundle on its group is what asks for
    authentication, so `OnRequest` is not asked (authentication is always
    required) and `OnAuthorization` gets `''` and `'VERB pattern'`.
  - The JWT bundles take a secret (with the HMAC algorithm, which is honoured)
    or an `IJWTSigner` (RS256 & co.) and check the expiration by default;
    `JWTAuthenticationBundle` reads the token from the header only (`AccessTokenParamName` adds a
    request parameter), `JWTCookieAuthenticationBundle` from the cookie only. `TMVCAuthCookieOptions` is the cookie (HttpOnly, Secure and
    SameSite=Strict by default).
  - The check of a bundle runs after `OnBeforeControllerAction` of every classic
    middleware, whatever the order of registration: a classic hook that reads
    `Context.LoggedUser` there sees no user yet (it was after the middleware
    in the chain). Make it an endpoint filter registered after the bundle.
  - The HTTP part of a bundle (login, logout) runs before every classic middleware: a rate limit, an IP
    block, CORS or a CSRF check that must cover the login is an HTTP filter
    registered before the bundle (a classic CORS middleware does not answer the
    preflight of a cross-origin login any more). The blacklist of `JWTAuthenticationBundle`, as the
    check, reads the token of the `Authorization` header, or of the `AccessTokenParamName` parameter
    when the header is missing; never the cookie. `CSRFHTTPFilter` checks before routing (it covers the
    requests answered by later HTTP filters), `CSRFEndpointFilter` after routing;
    `CSRFToken` computes the token. `CustomAuthenticationBundle` needs a session HTTP filter registered before it.
  - **Breaking (rc8 → rc9):** `BasicAuthEndpointFilter(handler)` no longer asks `OnRequest` on a
    Minimal API route.

- **Breaking (rc8 → rc9): enums instead of Boolean switches.** A `True`/`False` in the
  middle of a call says nothing; the filters and the Minimal API born in the RC cycle take an
  enum or a set instead. No aliases (3.4.3 has none of them); the compiler finds every call.
  - `TraceHTTPFilter(AMaxBodySize, ALogTag, AFlags: TMVCTraceFlags = [], ACorrelationHeaderName)`:
    `tfLogAuthorization` (was `ALogAuthorization = True`), `tfLogNonTextBody` (was
    `AHideNonTextBody = False`). `[]` keeps the defaults.
  - `RedirectHTTPFilter(..., AKind, AQuery: TMVCRedirectQuery = rdqPreserve)`: `rdqDrop` was
    `APreserveQuery = False`.
  - `SwaggerHTTPFilter`: `AAuthentications: TMVCSwaggerAuthentications = []` (`sauBasic`,
    `sauBearer`) in fifth position replaces `AEnableBasicAuthentication` (fifth) and
    `AEnableBearerAuthentication` (tenth): one parameter less. `TMVCSwaggerMiddleware` keeps its
    3.4 Booleans.
  - `TMVCStaticFilesOptions`: `Mode: TMVCStaticFilesMode` (`sfmFiles`, `sfmSPA`) replaces
    `SPAWebAppSupport`, `Flags: TMVCStaticFilesFlags` (`sffCaseSensitivePrefix`) replaces
    `CaseSensitivePrefix`.
  - `TMVCOIDCOptions`: `Flags: TMVCOIDCFlags`, by default `[ofFetchUserInfo, ofPKCE]`, replaces
    `FetchUserInfo` and `UsePKCE` (to turn PKCE off: `lOptions.Flags := lOptions.Flags - [ofPKCE]`).
  - Minimal API: `WithDeprecated` has no parameter; `WithOpenAPI(const AVisibility:
    TMVCOpenAPIVisibility = oavVisible)`, `WithOpenAPI(oavHidden)` was `WithOpenAPI(False)`;
    `Prefix<T>(APath, AData, AOwnership: TMVCGroupDataOwnership = gdoEngine)`, `gdoCaller` was
    `AOwns = False`.
  - They stay Boolean: `AUseCommonHeadersAndFooters` and `AAllowsCredentials` (the same
    parameters of the 3.4 controllers and CORS middleware), `SessionCookieFlags(ASecure)` (a
    value read from `.env`), the named field `TMVCAuthCookieOptions.Secure`.

- **`OpenAPIHTTPFilter(AEngine, AInfo, ASources, AURL)`** (`MVCFramework.Filters.OpenAPI`): the
  filter with document sources of your own (`IMVCOpenAPISource`) after the built-in ones, what
  `TMVCOpenAPI3Middleware.AddSource` gave the middleware. `OpenAPIDocumentJSON(AEngine, AInfo,
  ASources)` returns the document as a string.

- **Breaking (rc8 → rc9): a filter's name ends in `HTTPFilter` or `EndpointFilter`.** The
  name says the kind, like the types `TMVCHTTPFilter` / `TMVCEndpointFilter`; the function that
  builds a bundle ends in `Bundle`, and `Use` registers all three. There are no aliases: the names were born in the
  3.5 RC cycle (3.4.3 has none of them), so a project written on an rc renames its calls; the
  compiler finds each one. In rc8 `CORSFilter` was the HTTP filter and `CORS` the endpoint one.
  `RefreshTokenHTTPFilter`, `SessionCookieFlags`, `CSRFToken`, `StaticBearerTokens`, the
  `TMVC*Options` records and the Minimal API `Redirect(...)` response keep their names.

  | HTTP filter | Now |
  |---|---|
  | `ActiveRecordFilter` | `ActiveRecordHTTPFilter` |
  | `AllowedHostFilter` | `AllowedHostHTTPFilter` |
  | `AllowedMethodsFilter` | `AllowedMethodsHTTPFilter` |
  | `AllowedOriginFilter` | `AllowedOriginHTTPFilter` |
  | `Analytics` | `AnalyticsHTTPFilter` |
  | `BearerAuthFilter` | `BearerAuthHTTPFilter` |
  | `CORS` | `CORSHTTPFilter` |
  | `CORSFilter` (rc8) | `CORSHTTPFilter` |
  | `CSRFFilter` | `CSRFHTTPFilter` |
  | `Compression` | `CompressionHTTPFilter` |
  | `CustomAuthLoginFilter` | `CustomAuthenticationBundle` |
  | `DatabaseSessionFilter` | `DatabaseSessionHTTPFilter` |
  | `ETag` | `ETagHTTPFilter` |
  | `FileSessionFilter` | `FileSessionHTTPFilter` |
  | `IPBlock` | `IPBlockHTTPFilter` |
  | `JWTBlackListFilter` | `JWTAuthenticationBundle` (`OnAcceptToken` + `OnNewJWTToBlackList`) |
  | `JWTCookieLoginFilter` | `JWTCookieAuthenticationBundle` |
  | `JWTLoginFilter` | `JWTAuthenticationBundle` |
  | `MemorySessionFilter` | `MemorySessionHTTPFilter` |
  | `OIDCLoginFilter` | `OIDCAuthenticationBundle` |
  | `OpenAPI` | `OpenAPIHTTPFilter` |
  | `RangeMedia` | `RangeMediaHTTPFilter` |
  | `RateLimit` | `RateLimitHTTPFilter` |
  | `RateLimitRedis` | `RateLimitRedisHTTPFilter` |
  | `Redirect` | `RedirectHTTPFilter` |
  | `RequestLog` | `RequestLogHTTPFilter` |
  | `SecurityHeaders` | `SecurityHeadersHTTPFilter` |
  | `SessionFilter` | `SessionHTTPFilter` |
  | `Shutdown` | `ShutdownHTTPFilter` |
  | `StaticFiles` | `StaticFilesHTTPFilter` (or `FileServerBundle`) |
  | `Swagger` | `SwaggerHTTPFilter` |
  | `Trace` | `TraceHTTPFilter` |

  | Endpoint filter | Now |
  |---|---|
  | `ActiveRecord` | `ActiveRecordEndpointFilter` |
  | `Authorize` | `AuthorizeEndpointFilter` |
  | `BasicAuth` | `BasicAuthEndpointFilter` |
  | `CSRF` | `CSRFEndpointFilter` |
  | `CustomAuth` | `CustomAuthenticationBundle` |
  | `DatabaseSession` | `DatabaseSessionEndpointFilter` |
  | `EndpointCORS` | `CORSEndpointFilter` |
  | `CORS` (rc8) | `CORSEndpointFilter` |
  | `FileSession` | `FileSessionEndpointFilter` |
  | `JWT` | `JWTAuthenticationBundle` |
  | `JWTCookie` | `JWTCookieAuthenticationBundle` |
  | `MemorySession` | `MemorySessionEndpointFilter` |
  | `OIDC` | `OIDCAuthenticationBundle` |
  | `RequireRole` | `RequireRoleEndpointFilter` |
  | `Session` | `SessionEndpointFilter` |

- **The wizard's look.** Segoe UI, a header rule in the brand amber, a "Step n of
  m" indicator, and the project summary in its own box on the last page, where
  it used to be cut off under the buttons.

- **MCP servers from the IDE wizard.** A ninth preset, "MCP Server", and four
  options on the Options page of every preset generate a project that exposes
  tools, resources and prompts over the Model Context Protocol. The protocol
  code is the external library
  [mcp-server-delphi](https://github.com/danieleteti/mcp-server-delphi)
  (0.9.0 or later), which must be in the IDE library path: nothing of it is
  in the framework, and the wizard warns in the Messages view when it does not
  find it or finds an older one.
  - "MCP server on /mcp": `Tools.SampleU.pas` with a sample tool, the guard
    filters (`BearerAuthHTTPFilter`, `AllowedOriginHTTPFilter` and the library's
    `MCPEndpoint`), the session controller and the published endpoint in
    `EngineConfigU`. The endpoint asks for a bearer token, generated for the
    project in `.env` (`mcp.auth.token`); the HTTP server does not start when
    it is empty. A project with the JWT bearer middleware uses that for
    `/mcp` and gets no static token. Available where the project has an
    `EngineConfigU`: the console programs (Indy Direct, HTTP.sys, WebBroker)
    and the Indy Direct and HTTP.sys Windows services.
  - "stdio transport": the same executable started with `--transport stdio`
    talks MCP on stdin/stdout, which is how Claude Code and Claude Desktop
    launch a local server; `.mcp.json` in the project folder points at it, and
    `opencode.json` does the same for OpenCode. In
    this mode no HTTP server runs, the console appender is left out (JSON
    logging profile: `loggerpro.stdio.json`) and the working directory is the
    executable's folder. Console programs only.
  - `Resources.SampleU.pas` and `Prompts.SampleU.pas`, next to `Tools.SampleU.pas`, in
    every MCP project (no box for them).
  - MCP is a project type of its own: the box is selectable only in "MCP Server" (fixed on) and in
    "Custom Project" (/mcp over HTTP, off by default). The stdio transport exists only in "MCP Server":
    it takes over the console, which a server with an API uses for its own output.
  - "REST controllers as MCP tools (prototyping)": one `RegisterFromEngine`
    line that turns every REST action except the `DELETE` ones into a tool
    calling the server over HTTP. Controller projects over plain HTTP only.
  - The "MCP Server" preset has no controller and offers two application
    types only: console and Windows service (no ISAPI, no Apache). Started with no arguments it
    talks MCP on stdin/stdout; `--transport http` starts the HTTP server,
    which listens on `localhost` (`mcp.http.host` in `.env`), also when the
    project is generated as a Windows service. In the other presets the
    default stays the HTTP server.
  - Project Manager menu (Delphi 12+): "New HTTP Filter..." and "New Endpoint
    Filter..." add `HTTPFilters.<Name>U` with `<Name>HTTPFilter` (a
    `TMVCHTTPFilter` that puts the request time in `Server-Timing`) or
    `EndpointFilters.<Name>U` with `<Name>EndpointFilter` (a
    `TMVCEndpointFilter` that logs the routed action and shows, commented out,
    how to answer instead of it), registered with `AEngine.Use(...)` at the end
    of `ConfigureEngine` (of `WebModuleCreate` in
    ISAPI/Apache/WebBroker projects). The endpoint filter is offered only in
    projects with controllers or Minimal API routes.
  - Project Manager menu (Delphi 12+): "New MCP Tool Provider..." adds
    `<Name>ToolsU.pas`, a provider that registers itself. In an "MCP Server"
    project (no controller) the menu has only "New Service...", "New HTTP
    Filter..." and "New MCP Tool Provider...".
  With the options off, the generated projects are what they were.
- **Wizard: one naming and formatting style in the generated code.** Locals
  are `lName`, parameters `AName`, fields `fName`, as in the framework
  sources; the blank lines and trailing spaces the template conditionals
  left behind are gone. Names only: the generated programs behave as before.
- **`time` and `datetime` route parameter kinds**, in controller routes and
  Minimal API routes alike: `($at:time)` accepts `hh:nn` or `hh:nn:ss`,
  `($since:datetime)` an ISO 8601 timestamp (`2026-10-05T14:30:15`, with an
  optional fraction and `Z` or offset; send `+` as `%2B`). A value of another
  shape does not match the route. They bind `TTime` / `TDateTime` parameters
  and are documented as `format: time` / `date-time` by the native OpenAPI
  emitter. `ISOTimeToTime` also reads `hh:nn`.
- **`MVCSwagResponses(..., AModelClass, AIsArray, ADataEnvelope)`.** With
  `ADataEnvelope = True` the documented response is the model wrapped in
  `{"data": ...}`, the body `OkResponse(Body)` / `CreatedResponse(..., Body)`
  render, built from the class: the model definition is registered and the
  name follows `SchemaObjectDefinitionNameSuffixForResponse`, with no JSON
  schema written by hand. The wizard and the Project Manager menu use it.
- **New projects come with the AI coding agent files and skills.** Option
  "AI coding agent files and skills" on the Options page of the IDE wizard,
  checked by default in every preset: `AGENTS.md` (the project facts, the
  skills list, where the framework and RTL sources are), `CLAUDE.md` and
  `GEMINI.md` importing it, `update_ai_skills.bat`, and the
  [delphi-ai-skills](https://github.com/danieleteti/delphi-ai-skills) that fit
  the project (Delphi, code review, DMVCFramework with ActiveRecord, security,
  testing; Minimal API, web app, UI, HTMX, JSON-RPC when the project uses
  them) downloaded into `.claude\skills` from the branch of the framework line
  (`dmvc-3.5`). The download has a cancellable progress dialog; a failure is a
  warning, and the `.bat` repeats the install and refreshes the skills later.
  Untick the option to generate a project without them.
- **OpenAPI 3 from the Swagger middleware.** `TMVCSwaggerMiddleware` and the
  `SwaggerHTTPFilter(...)` HTTP filter take a new optional last parameter,
  `ASpecVersion: TMVCSwaggerSpecVersion = ssvSwagger2`
  (`MVCFramework.Swagger.Commons`). With `ssvOpenAPI3` the same URL serves an
  OpenAPI 3 document (`"openapi": "3.2.1"`, built by SwagDoc) with every
  feature of the middleware: JWT and basic security, `MVCSwagAuthentication`,
  `MVCRequiresAuthentication`, `MVCSwagJSONSchemaField`, `MVCSWAGDefaultModel`
  and the `TMVCActiveRecordController` CRUD paths. The JWT scheme is
  `http`/`bearer`/`JWT`, so Swagger UI's Authorize box takes the raw token.
  Unless the application passes `AHost`, the document's server is relative
  (`"servers": [{"url": "<basePath>"}]`): "Try it out" calls the origin that
  served the document. Swagger 2.0 stays the default (for the two fixes
  that touch it see **Fixed**). The OpenAPI 3 support in SwagDoc is by
  Marcelo Jaloto (PR #916): thank you, Marcelo.
- **The IDE wizard generates API documentation.** New option "API
  documentation (OpenAPI 3)", on by default for RESTful, Full-Stack, Custom
  and Minimal API RESTful projects. Controller projects register the Swagger
  middleware with OpenAPI 3; Minimal API projects register the `OpenAPIHTTPFilter(...)`
  filter. Both publish the document at `/openapi.json` and serve Swagger UI
  at `/swagger`, both published only when `.env` sets
  `dmvc.openapi.enabled=true`. The generated `.env` sets it; without the key
  (e.g. in production) no documentation is published. The wizard downloads the official Swagger UI release (5.33.0,
  SHA-256 verified) into `bin\www\swagger` while it creates the project,
  showing a progress dialog that can be cancelled and gives up after 30
  seconds. Without network access the project is still created and the
  folder contains a README with the download steps. When the documentation
  is published, the console shows the Swagger UI URL at startup.
- **The IDE wizard writes a JWT signing key into the generated `.env`.**
  Projects with JWT (HMAC) get their own `JWT_SECRET`, 384 random bits from
  the system CSPRNG, drawn at every generation, so a new project starts
  without editing `.env` first. The generated `.gitignore` already excludes
  `.env`.
- **"DMVCFramework" menu in the Project Manager (Delphi 12 and 13).** A right
  click on a DMVC project adds, from the same templates as the wizard:
  - **New REST Controller**: `Controllers.<Name>U` under `/api/<segment>`,
    optionally with CRUD actions; request bodies are bound to a model class
    (`TOrder` for `Orders`, editable) with `[MVCFromBody]` and validated;
  - **New Web Controller and View**: a TemplatePro page controller under
    `/web/<segment>` and its view `<segment>/index.html`;
  - **New Minimal API Route Group**: `Routes.<Name>U` with
    `Map<Name>Routes`, mounted on `/api/<segment>`; bodies bound by type
    (`MapPost<TOrder>`, `MapPut<Integer, TOrder>`);
  - **New TemplatePro View**: a page that extends `baselayout.html`, or an
    HTMX fragment.
  The new unit is added to the project and wired in through the editor
  buffer: `AddController(...)` next to the existing ones (before the
  `// Controllers - END` marker when present), or the `Map<Name>Routes` call
  at the end of `ConfigureRoutes`, plus the `uses` entry. When no place is
  found, the unit is still created and a message shows the line to add. The
  When the project publishes an OpenAPI document, the new controller carries
  the `MVCSwag*` attributes (model, summaries, responses) and the new route
  group `.WithTags` / `.WithSummary` / `.Produces<T>`, so the model and the
  operations appear in Swagger UI (Swagger middleware for controllers,
  `MVCFramework.OpenAPI3` for route groups, looked for in the `.dpr` and in
  every unit). The
  menu shows only what the project can take: the controllers in controller
  projects, the route group in Minimal API projects (the `.dpr` calls
  `ConfigureRoutes`), the views where a views folder exists. The older Delphi
  versions do not get the menu.

- **Web Application projects from the IDE wizard start from a fuller UI.**
  The TemplatePro views have a page header, a live server panel and a
  "Start here" list on the home page, drawn icons, themed selection and focus.
  The scaffold stays the minimum to add to; the home page points to the new
  sample `samples/webapp_htmx_forms`.
- **New sample `samples/webapp_htmx_forms`.** A **People** section
  (`/web/people`) that shows how to render a table and edit its rows: the
  controller filters and sorts a list of objects from the query string,
  `people/table.html` loops over it, and with HTMX only the table is replaced
  (fragment when `IsHTMX` and not boosted and not a history restore, plus
  `Vary: HX-Request`). The new/edit forms use every macro of the TemplatePro
  forms library, validate with the DMVCFramework validators and
  `TMVCValidationEngine`, re-render with 422 on errors and follow
  Post/Redirect/Get.
- **The `QUERY` HTTP method (RFC 10008).** `QUERY` is safe and idempotent
  like `GET`, but it carries a request body: the query travels in the
  payload instead of the URL, so it is not URL-length limited and does not
  end up in access logs, proxy caches or browser history. It was the one
  combination `TMVCHTTPMethodType` could not express.

  ```pascal
  [MVCPath('/search')]
  [MVCHTTPMethod([httpQUERY])]
  function Search: IMVCResponse;
  ```

  It works the same way in the Minimal API through `MapQuery`, which has the
  same five arities as `MapPatch`. A class argument binds the request body:

  ```pascal
  lEngine.Root.MapQuery<TSearchFilter>('/search',
    function(AFilter: TSearchFilter): IMVCResponse
    begin
      Result := Ok(DoSearch(AFilter));
    end);
  ```

  `MapMethods([httpQUERY], '/search', ...)` is equivalent - verbs first, then
  the path.

  `IMVCRESTClient` gains `Query` in the same three overloads as `Patch`.
  `MVCConsumes` applies to a `QUERY` route exactly as it does to `POST`.

  Caveats worth knowing before you deploy one:

  - **Cross-origin:** the default `Access-Control-Allow-Methods` of the CORS
    filter and middleware does **not** include `QUERY`, and is deliberately
    left unchanged. Pass the whole list explicitly (it is the fifth argument)
    if a browser on another origin must call the endpoint:

    ```pascal
    CORSHTTPFilter('*', False, '', 'Content-Type,Authorization',
      'GET,POST,PUT,DELETE,PATCH,OPTIONS,QUERY')
    ```
  - **CSRF:** treat `QUERY` like `POST`, not like `GET`. It is safe by the
    specification, but it carries a body and a same-origin `QUERY` is not
    preflighted, so a `QUERY`-only endpoint needs the same token check a
    `POST` one does.
  - **Documentation:** `QUERY` does not appear in the generated Swagger or
    OpenAPI output. The `query` path-item slot only exists in OpenAPI 3.2;
    the Swagger middleware (Swagger 2.0, or OpenAPI 3 through SwagDoc) and
    the native emitter (3.1) do not map it. Both skip
    the verb rather than writing an invalid document. This lands with the
    OpenAPI 3.2 emitter in 4.0.
  - **Hosts:** verified end to end on Indy Direct, HTTP.sys, WebBroker,
    Apache 2.4 and IIS Express ISAPI. None of them filters the verb before
    it reaches the framework.

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

- **JWT refresh token** (`MVCFramework.JWT.RefreshToken`, additive and
  opt-in). An OAuth2-style refresh token layer on top of the JWT
  authentication: short-lived access tokens plus a
  long-lived, revocable refresh token with rotation and reuse-detection.
  The refresh token is an opaque random string (not a JWT); the store
  keeps only its SHA-256 hash. Presenting an already-rotated token
  revokes the whole family (replay defence). Ships with a pluggable
  store interface, an `TMVCActiveRecord`-backed implementation
  (`MVCFramework.JWT.RefreshToken.ActiveRecord`), an HTTP filter
  (`RefreshTokenHTTPFilter`, `MVCFramework.JWT.RefreshToken.Filters`) and a sample
  (`samples/jsonwebtoken_refreshtoken`). See `docs/jwt-refresh-token.md`.

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

- **The filters no longer use the deprecated middleware.** The logic of JWT, JWT cookie,
  blacklist, custom authentication, CSRF, rate limit, OIDC, Swagger and OpenAPI lives in the
  filter units; each middleware is a wrapper over it, with its 3.4 API and behaviour, and will go
  away in 4.0 without the filters noticing. No filter unit turns off the deprecation warning.
  Types moved with an alias (and constants for the enum values) left where they were:
  `TMVCJWTCookieSameSite` to `MVCFramework.JWT`; `TMVCJWTDefaults`, `TMVCOnAcceptTokenProc`,
  `TMVCOnNewJWTToBlackList` to `MVCFramework.Filters.Authentication`; the rate-limit types
  (`IMVCRateLimitStorage`, `TMVCInMemoryRateLimitStorage`, `TMVCRateLimitKeyType`...) and the
  static-files callback types to `MVCFramework.Filters`; `TMVCOIDCUserAuthenticatedProc`,
  `TMVCOIDCAuthRequiredProc` to `MVCFramework.Filters.OIDC`. Their RTTI qualified names change
  accordingly. What the middleware call is public in the filter units: `CSRFCheck`,
  `MVCIsValidHost`, `MVCRateLimitExceeded`, `GetAnalyticsDefaultLogger` (`MVCFramework.Filters`),
  `TMVCJWTAuthenticationCore` & co (`MVCFramework.Filters.Authentication`), `TMVCSwaggerDocument`
  (`MVCFramework.Filters.Swagger`), `OpenAPIDocumentJSON`. `TMVCJWTAuthenticationMiddleware`
  gets a read-only `LoginURLSegment`, which the Swagger document reads.

- **`TraceHTTPFilter` as `TMVCTraceMiddleware`.** Every line carries a
  correlation id (`[rid:...]`: the `X-Request-ID` or `X-Correlation-ID` of the
  request, otherwise a generated one) that is echoed in the response header, the
  request `Authorization` is masked with `MVCRedactSecret` (`Bearer ***`), the
  response headers are logged through the new `MVCRedactedHeaders` (Authorization,
  Proxy-Authorization, Cookie and Set-Cookie are masked: the filter used to log
  them as they were), and the default body limit is `1024` as in the middleware
  (it was `4096`). New last parameter `ACorrelationHeaderName`
  (`'X-Request-ID'`). `MVCFramework.Commons` gets `MVCRedactedHeaders`, used by
  both.
- `CompressionHTTPFilter` picks the first supported encoding in the order of the
  `Accept-Encoding` header, as `TMVCCompressionMiddleware` does (the filter
  preferred `gzip` whenever it was listed).
- **Native OpenAPI emitter (`OpenAPIHTTPFilter(...)` filter, `TMVCOpenAPI3Middleware`):**
  - schema property names are the JSON names the serializer writes
    (`MVCNameAs`, `MVCNameCase`, `MVCNameCaseDefault`); members marked
    `MVCDoNotSerialize` are left out;
  - `Produces<T>` describes the `{"data": T}` envelope that `Ok(object)`
    renders;
  - nullable fields use the OpenAPI 3.1 form (`"type": [..., "null"]`)
    instead of the 3.0 keyword `nullable`;
  - on controllers, a path parameter with the `sqids` converter is a string,
    and action parameters match path placeholders ignoring case (`id` and
    `($ID)` are one path parameter).
- **`TMVCHTTPMethodType` has a ninth member, `httpQUERY`.** It is appended
  at the end of the enumeration, so no existing ordinal moved and no
  persisted value changed meaning. Two consequences:
  - Third-party code with a `case` over `TMVCHTTPMethodType` and **no `else`
    branch** now falls through silently when handed `httpQUERY`. Add the
    branch, or an `else`.
  - An action declaring `[MVCPath]` and **no** `[MVCHTTPMethod]` answers
    every verb. That set now has nine members instead of eight, so such an
    action also answers `QUERY`. It already answered `DELETE`, `PUT` and
    `TRACE`; `QUERY` opens no surface that was not already open. Declare
    `[MVCHTTPMethod([...])]` if you want the route narrowed.

- **`foRefresh` on SQL Server returns the row as it is after the triggers.**
  SQL Server has no BEFORE triggers, and `OUTPUT` reports the row before the
  AFTER triggers run, so a column written by a trigger came back stale. After
  an Insert or Update the row is now selected again by key, in the same batch.
  Same round trip as before, plus one primary-key lookup.
- **SQL Server: an auto-generated integer key that is not an IDENTITY now
  raises on Insert** (`SCOPE_IDENTITY() is NULL ...`) instead of silently
  leaving the in-memory key at 0. The row is inserted before the error. GUID
  and string keys filled by a `DEFAULT` are read back through
  `OUTPUT ... INTO` a table variable and work.
- **RQL `limit(n,0)` / `MaxRecordCount = 0` on SQL Server returns an empty
  list** instead of raising `SQL Server rejects "FETCH NEXT 0 ROWS"`, as
  `LIMIT 0` does on the other engines.

- Default `TGUID` serialisation format is now dashes-only (RFC 4122)
  instead of `{braces}`. See **BREAKING CHANGES** above for migration.
- `TDate` / `TDateTime` / `TTime` zero no longer serialises as JSON
  `null`. See **BREAKING CHANGES** above for migration.
- **Swagger: date fields use the standard formats.** In the generated
  `swagger.json`, `TDate` fields are described as `"format": "date"` and
  `TDateTime` fields as `"format": "date-time"` and `TTime` fields as
  `"format": "time"`, instead of the literal patterns `"yyyy-MM-dd"`,
  `"yyyy-MM-ddTHH:mm:ss"` and `"HH:mm:ss"` (Nullable types included). These
  are the formats
  defined by the specification, so Swagger UI and client generators now
  recognise them: a client regenerated from the document gets date types
  where it used to get plain strings. The basic security scheme now also
  carries its `description`. (SwagDoc update, PR #916 by Marcelo Jaloto.)
- **Swagger documentation attributes renamed to `MVCSwag*`.** The
  field-documentation attributes live in `MVCFramework.Swagger.Commons` as
  `MVCSwagFormat`, `MVCSwagMaxLength`, `MVCSwagMinimum`, `MVCSwagMaximum`
  and `MVCSwagPattern`, alongside the other `MVCSwag*` attributes.

### Security

A security review of the 3.5 tree produced the following fixes. Several of them
change a default; each one says so.

**Denial of service through unbounded allocation**

- The bundled JSON parser had no nesting limit, and neither does upstream. About
  34 KB of valid but deeply nested JSON terminated a Win64 process outright, with
  no catchable exception — well under the 5 MiB `max_request_size`. A depth limit
  is now enforced, `JsonMaxNestingDepth` (default 1000). The check cannot be switched
  off: clearing the value restores the default. An app that needs deeper documents
  raises it.
- **HTTP.sys** sized the body buffer from the `Content-Length` header *before*
  the engine checked `max_request_size`, so a request of a hundred bytes carrying
  no body at all committed whatever the header claimed. The limit is now applied
  before allocating, and the no-`Content-Length` path is capped as well.
- **Indy** never applied `max_request_size` to a chunked body: the chunked path
  leaves `ContentLength` at `-1`, so the guard compared `-1` against the limit.
  The request body is now bounded as it is read.
- Exceeding `max_request_size` now answers **413 on every host**. It used to be
  413 under WebBroker, 500 under Indy, and on HTTP.sys the connection was simply
  dropped.
- `Range: bytes=0-` — what a browser sends first for a `<video>` — buffered the
  whole file in memory. A single 206 response is now capped at 8 MiB; RFC 7233
  allows returning fewer bytes than requested and the client asks for the rest.
- The in-memory rate-limit store had no ceiling. With a client-controlled key
  (`rlkAPIKey`, `rlkCustom`, and `rlkIPAddress` wherever forwarded headers are
  trusted) a caller could both escape its own limit and grow the store by
  rotating the value. The store is now capped at 100 000 keys: past the cap a key
  the store has never seen is **refused with 429**, keys already being counted
  are unaffected, and a warning says what happened. Validate a client-supplied
  key before the limiter sees it - the cap is a backstop, not the fix.

**Path handling**

- The static-files middleware and filter computed a directory-traversal verdict
  and then read it inside a branch that could not be reached, so the check never
  ran. Both now act on it. The check itself was correct all along.
- A route parameter whose **decoded** value carries a `.` or `..` segment no
  longer matches: `%2E%2E` inside one segment used to reach the action as `..`.
  A literal dot segment in the path is refused rather than resolved as well.
  Deliberately not normalised: normalising would have given every host the most
  permissive behaviour. Note what this does **not** cover: HTTP.sys is handed a
  path the kernel has already decoded and collapsed, so `/public/../admin` reaches
  the application as `/admin` before the router looks. A reverse proxy that denies
  `/admin` does not see `/admin` either. That is a deployment rule to write in
  front of the server, not something the framework can undo.
- On Windows, a static-file request containing `:`, `*`, `?` or ending in a dot
  or a space is refused. None of these is a traversal — the file really is under
  the document root — but Win32 normalises them away, so `web.config::$DATA`
  reached the file system while defeating any deny rule written on the name.
- `TMVCFormFile.FileName` is copied verbatim out of the `Content-Disposition`
  header, and the documented idiom was `SaveToFile(TPath.Combine(UPLOAD_DIR,
  Doc.FileName))`. Two names defeat it: `..\..\x` is carried through, and a
  **rooted** name such as `C:\inetpub\wwwroot\shell.aspx` makes `TPath.Combine`
  return it verbatim and drop `UPLOAD_DIR` entirely. **New:** the overload
  `SaveToFile(ADirectory, AClientFileName)` keeps the directory in the caller's
  hands and reduces the name to a leaf - use it. The one-argument
  `SaveToFile(APath)` now refuses a path that ends with the unsanitised client
  name; a path built from `SafeFileName`, and a relative directory like
  `.\uploads`, are unaffected.

**Session**

- The session id is validated where it enters the engine and again where the file
  store turns it into a file name. It arrives from a cookie or a query-string
  parameter and was used verbatim: `TPath.Combine` returns its second argument
  when that one is rooted, so an id could create, truncate or delete a file
  anywhere the process could write. Ids that this engine cannot have issued are
  now dropped, and the caller starts a fresh session. **Behind a load balancer**
  that makes sessions sticky by prefixing the cookie value (HAProxy
  `cookie ... prefix` produces `srv1~DT...`) every id is dropped and no login
  survives the next request: use a separate stickiness cookie instead.
- **Default changed:** the session cookie is now `HttpOnly` by default, in all
  six entry points (`UseMemorySessionMiddleware` and friends, and the
  `MemorySessionEndpointFilter`/`FileSessionEndpointFilter`/`DatabaseSessionEndpointFilter` filters). Pass `False`
  explicitly (the classic helpers) or leave `scfHttpOnly` out of the flags (the
  filters) to restore the old behaviour.
- The session cookie now carries `SameSite=Lax` - **on Delphi 11 and later
  only**. `TCookie.SameSite` arrived in 10.4.2 but `CompilerVersion` cannot tell
  10.4.2 from 10.4, so builds on 10.2-10.4 emit no SameSite attribute at all.
- **New:** `Secure` on the session factory, and a matching last argument on the
  classic helpers (`UseMemorySessionMiddleware` and friends) and the `scfSecure`
  flag on the `MemorySessionEndpointFilter`/`FileSessionEndpointFilter`/`DatabaseSessionEndpointFilter` filters.
  **Default off**,
  and deliberately: a `Secure` cookie is not stored or sent at all over plain
  HTTP, so defaulting it to `True` would silently break every development setup —
  and any existing HTTP deployment — on upgrade, with no error to explain why the
  login stopped sticking. Turn it on wherever the application is actually served
  over TLS:

  ```pascal
  AEngine.AddMiddleware(UseMemorySessionMiddleware(30, True, True));
  //                                                       ^      ^
  //                                                 HttpOnly   Secure
  ```

  Generated projects get it turned on, driven by `SESSION_COOKIE_SECURE` in the
  `.env`, the same shape as `JWT_COOKIE_SECURE`. The file-session scaffold also
  stopped passing `HttpOnly = False` explicitly, which had been undoing the
  framework default on the one store that writes the id to disk.

**Information disclosure**

- Outside `DEBUG`, only the framework's own exceptions put their message in the
  response. Everything else answers `Internal server error`. A FireDAC
  `EFDDBEngineException` message carries the whole SQL statement with real table
  and column names; an IO error carries a server-side absolute path. The detail
  is still logged.
- `TMVCSystemController`'s localhost-only ACL now looks at the **peer address**
  instead of `ClientIp`. `ClientIp` honours `X-Forwarded-For` and `X-Real-IP`
  when `MVCTrustProxyForwardedHeaders` is on — which is exactly the deployment
  where the ACL stopped protecting anything. `TMVCWebRequest.PeerIp` is new and
  ignores those headers entirely; `ClientIp` remains the right answer for logging
  and rate limiting.
- `TMVCActiveRecordController` built without an authorization function exposes
  every registered entity to anonymous callers for read and write. That default
  is unchanged, so nothing breaks, but it now says so in the log, once per
  process.

**Host-dependent behaviour, which is a class of bug on its own**

An application should not become vulnerable because of which server it is
hosted on. These divergences were fixed by making the hosts agree:

- `QueryParams` and `QueryStringParam` disagreed on a repeated key —
  `?role=user&role=admin` gave `user` to one accessor and `admin` to the other.
  Both are first-wins now, which is also what most proxies do. Under WebBroker a
  repeated key additionally raised, answering 500.
- Cookie names and values are percent-encoded on Indy and HTTP.sys, as WebBroker
  already did. Raw concatenation allowed a user-controlled cookie value to inject
  cookie attributes on those two hosts, while the identical code was safe under
  WebBroker. **On upgrade**, incoming cookies are decoded too: a raw value holding
  `+` is read back with a space, and a bare `%` in any cookie on the domain -
  including ones DMVC never wrote - fails the request. DMVC's own session and JWT
  cookies are unaffected. Browser JavaScript reading a DMVC cookie needs
  `decodeURIComponent`.
- Indy decoded the non-standard `%uXXXX` form and silently dropped malformed
  escapes: `%u003Cscript%u003E` reached the action as `<script>` with no `<` in
  the bytes a WAF inspected, and `ad%zzmin` arrived as `admin`. Indy now uses the
  same decoder as the other two hosts. **On upgrade**, that decoder raises on a
  malformed escape where the old one dropped it: `?q=100%` answers 500 on Indy
  instead of reaching the action with a mangled value.
- A request repeating `Authorization`, `Content-Type` or `Content-Length` is
  refused with 400 rather than resolved, because the hosts resolved it
  differently and whatever sits in front may have read the other value.
  Detection needs the raw header list, which only Indy keeps; HTTP.sys is handed
  the known headers already merged by the kernel and WebBroker exposes them one
  at a time.

**Deployment note: routing is case-insensitive**

`GET /ADMIN/Users` reaches the action declared as `/admin/users`, on every host.
This is long-standing and consistent, but most reverse-proxy ACLs — nginx
`location`, Apache `<Location>`, IIS rewrite rules — are case-**sensitive** by
default. A rule that denies `/admin` is bypassed in one request unless it is
written case-insensitively. Check the rules in front of a DMVCFramework server.

**CORS**

- `Access-Control-Allow-Credentials` is no longer emitted when the allowed origin
  is `*` or empty. A browser rejects that combination anyway, so the header only
  ever advertised what no client could use - until someone replaced the wildcard
  with a real origin, at which point the dead header started meaning something.
  The middleware and the endpoint filter now share the decision
  (`MVCCORSAllowsCredentials`), and a response carrying a configured origin gets
  `Vary: Origin` so a shared cache cannot serve one origin's response to another.

**Two switches that were missing, both defaulting to the 3.4 behaviour**

- `TMVCJWTAuthenticationMiddleware.AuthorizationAccessTokenParamName`. The token
  is still accepted in `access_token` outside the `Authorization` header - SSE
  endpoints, `<img>` tags and download links cannot send one - but the parameter
  name was a private field, so the behaviour could not be turned off at all.
  Assign an empty string to accept the header only. Note it reads `Request.Params`,
  which also covers a route parameter and a form field.
- `TRQLCompiler.AllowUnmappedRQLFields` (default `True`). An RQL name the field
  mapping does not know is passed to the SQL as-is: the mapping is keyed on the
  Delphi field name and its alias, never on the database column, so filtering a
  partially mapped table or a view works today and code depends on it. `False`
  refuses those names with `ERQLException`. A one-shot `LogW` reports the first
  time the open valve lets one through. The default will flip in 4.0.

**RQL `limit()`**

- `limit(0,-1)` walked straight past `max_entities_record_count`: SQLite reads a
  negative LIMIT as "no limit". The count is now clamped to the configured cap.

**Wizard**

- Generated projects now default `JWT_COOKIE_SECURE` to `true`. The generated
  `.env` says to set it to `false` only for local development over HTTP.
- The generated authentication handler is **fail-closed**: it reads `ADMIN_USER`
  and `ADMIN_PASSWORD` from `.env`, both empty by default, and refuses everyone
  until they are set. It used to accept any request where user equalled password,
  handing an `admin`-role token to `admin`/`admin`.
- `JWT_SECRET` is generated empty and has no fallback in the generated code: a
  missing key must fail at startup, not sign tokens with a public string.
- `dmvc.expose_x_powered_by` now defaults to `false` in every template.
- Minimal API projects get an engine configuration callback. They were built on
  `TMVCEngine.Create` with no arguments, so every `dmvc.*` key the generated
  `.env` promised was ignored.
- Minimal API + JWT produced a project that looked authenticated and was not: the
  JWT units were emitted only for the non-minimal flavor. The admin route group
  now carries `JWTAuthenticationBundle(...)`, whose check verifies inside the route
  pipeline - the classic middleware verifies in `OnBeforeControllerAction`, which
  a minimal-API route never reaches.
- The generated `.env` emits the HTTPS block based on the chosen **protocol**
  rather than only for the `https.console` project type. An Indy Direct or
  HTTP.sys project on HTTPS was generated without the certificate keys its own
  `.dpr` reads, and a minimal-API web app read a `SESSION_COOKIE_SECURE` the
  `.env` never carried.
- WebBroker, ISAPI, Apache and Windows-service projects with TemplatePro views
  now render errors through `TMVCEngine.UseExceptionHandler` instead of a
  hand-rolled handler that put `E.Message` on the page in release builds too.

**Entity generator**

- An auto-generated primary key is emitted with `[MVCDoNotDeserialize]`, so an id
  in the request body can no longer pick which row an update overwrites. A
  natural key stays deserializable. **Regenerated entities change**: if you feed
  deserialized lists to `TMVCActiveRecord.Merge`, which matches rows by primary
  key, remove the attribute.

**WebSocket server: limits against unauthenticated peers** (PR #915)

A peer could pin a server thread forever by opening the socket and never
finishing the upgrade handshake, or make the server try to allocate up to 2^63
bytes with one 14-byte frame header. `TMVCWebSocketServer` has four new
properties, **on by default** (0 disables each one):

| Property | Default |
|---|---|
| `MaxPayloadLength` | 16 MB, checked before the payload buffer is allocated |
| `HandshakeTimeoutMs` | 5000 |
| `MaxHandshakeHeaders` | 64 |
| `FrameReadTimeoutMs` | 30000, only while a frame that has started is being read |

Idle connected clients are not affected. A connection that fails the handshake
or breaks the protocol is now closed; before, Indy re-entered the handshake on
the same socket.

**Response splitting, blocklist bypass, internal messages**

- A reason phrase carrying CR/LF (`ResponseStatus(200, 'OK ' + SomeInput)`,
  `Response.ReasonString := ...`) added a header to the response: header names,
  header values and redirects were already stripped, the status line was not.
  It now is, on every host.
- `IPBlockHTTPFilter` compared the list against `ClientIp` only. With
  `MVCTrustProxyForwardedHeaders` on, a blocked address that reached the server
  directly sent `X-Forwarded-For` with any other address and passed. The filter
  now refuses the request when either `ClientIp` or `PeerIp` is in the list.
- With `MVCTrustProxyForwardedHeaders` on, `ClientIp` (and the WebSocket
  `RealClientIP`) took the **first** address of `X-Forwarded-For`. A proxy
  appends to that header, so the first address is whatever the client sent: the
  IP rate limit, `IPBlockHTTPFilter` and the request log followed it. `ClientIp` now takes
  the address written by the proxy, the last one. **Behind more than one proxy**
  (a CDN in front of nginx, for example) say how many there are, or `ClientIp`
  is the address of the outer proxy:
  ```pascal
  MVCTrustProxyForwardedHeaders := True;
  MVCTrustedProxyCount := 2; // default 1
  ```
  The proxy must set `X-Forwarded-For` itself: where it sets only `X-Real-IP`, a
  client that sends `X-Forwarded-For` is still believed.
- Three places wrapped an internal message in an `EMVCException`, whose message
  is sent to the client as it is. The detail now goes to the log:
  - `PBKDF2` answered with the Indy/OpenSSL error; the message is now
    `Cannot initialize the hash function`.
  - A TemplatePro or WebStencils error answered with the engine's message and
    exception class. A **release** build now answers `View [name] error`; a
    DEBUG build keeps the detail on the page.
  - `ConnectionDefName "x" not found in config file "..."` carried the absolute
    path of the FireDAC definitions file; it now carries the file name.

### Deprecated

- **The classic authentication middleware.** `TMVCJWTAuthenticationMiddleware`,
  `TMVCJWTCookieAuthenticationMiddleware`, `TMVCJWTBlackListMiddleware`,
  `TMVCBasicAuthenticationMiddleware`, `TMVCCustomAuthenticationMiddleware`,
  `TMVCRoleBasedAuthMiddleware`, `TMVCOIDCAuthenticationMiddleware`,
  `TMVCCSRFMiddleware`, `TMVCJWTRefreshTokenMiddleware` (its warning names the
  `RefreshTokenStore` of `JWTAuthenticationBundle`; with a login of your own,
  `LoginURL` empty and the login outside the check) and
  `UseJWTMiddleware`, `UseJWTCookieAuthentication`, `UseJWTBlackListMiddleware`,
  `UseOIDCAuthentication`: each warning names what to use, the bundle of the
  mechanism, `AEngine.Use(XxxAuthenticationBundle(...))`, or the CSRF
  filters (see "Authentication bundles" and "Filters for every classic
  authentication middleware"). They keep working as before.
  `samples/jsonwebtoken_refreshtoken` and `docs/jwt-refresh-token.md` use
  `JWTAuthenticationBundle` with the refresh token. `TRoleBasedAuthHandler` is not deprecated: it is
  a handler, and works with `CustomAuthenticationBundle`.

- **Classic middleware that have an HTTP filter.** The classes below are
  deprecated and each warning names the filter to use (`MVCFramework.Filters`,
  registered with `AEngine.Use(...)`). They keep working as before;
  existing code compiles with one `W1000` per use.

  | Deprecated | Use |
  |---|---|
  | `TMVCRedirectMiddleware` | `RedirectHTTPFilter` |
  | `TMVCStaticFilesMiddleware` | `StaticFilesHTTPFilter` |
  | `TMVCAnalyticsMiddleware` | `AnalyticsHTTPFilter` |
  | `TMVCTraceMiddleware` | `TraceHTTPFilter` |
  | `TMVCCompressionMiddleware` | `CompressionHTTPFilter` |
  | `TMVCETagMiddleware` | `ETagHTTPFilter` |
  | `TMVCCORSMiddleware` | `CORSHTTPFilter` |
  | `TMVCSecurityHeadersMiddleware` | `SecurityHeadersHTTPFilter` |
  | `TMVCShutdownMiddleware` | `ShutdownHTTPFilter` |
  | `TMVCRangeMediaMiddleware` | `RangeMediaHTTPFilter` |
  | `TMVCSwaggerMiddleware` | `SwaggerHTTPFilter` (`MVCFramework.Filters.Swagger`) |
  | `TMVCOpenAPI3Middleware` | `OpenAPIHTTPFilter` (`MVCFramework.Filters.OpenAPI`) |
  | `TMVCRateLimitMiddleware` | `RateLimitHTTPFilter` (`TMVCRateLimitOptions` for the options) |
  | `TMVCActiveRecordMiddleware` | `ActiveRecordHTTPFilter` (`MVCFramework.Filters.ActiveRecord`) |
  | `UseMemorySessionMiddleware` | `MemorySessionHTTPFilter` |
  | `UseFileSessionMiddleware` | `FileSessionHTTPFilter` |
  | `UseDatabaseSessionMiddleware` | `DatabaseSessionHTTPFilter` (`MVCFramework.Filters.ActiveRecord`) |

  Defaults differ in a few places: `CORSHTTPFilter` has credentials off and the
  headers `Content-Type,Authorization` (the classic middleware: credentials on
  and `jwtusername`, `jwtpassword`, `authentication`, `authorization` too),
  `ETagHTTPFilter` writes a quoted ETag, `RedirectHTTPFilter` keeps the query string, `TraceHTTPFilter`
  redacts the `Authorization` header. Pass the parameters you rely on.

- **`TMVCListener` / `TMVCListenerProperties` / `TMVCListenersContext`**
  (`MVCFramework.Server`) are deprecated and **will be removed in 4.0**.
  After the Indy Direct conversion they are a thin wrapper over the
  `IMVCServer` abstraction and expose strictly less (Indy-only, no HTTPS,
  only `MaxConnections`). Build servers through `TMVCServerFactory` /
  `IMVCServer` (`MVCFramework.Server.Factory`) instead, which also gives you
  the HTTP.sys / WebBroker backends and built-in HTTPS. Existing code keeps
  compiling with a deprecation warning until you migrate.
- **`MVCFormat`, `MVCMinimum`, `MVCMaximum`** (the Swagger documentation
  attributes declared in `MVCFramework`) are deprecated aliases of
  `MVCSwagFormat`, `MVCSwagMinimum` and `MVCSwagMaximum`
  (`MVCFramework.Swagger.Commons`) and **will be removed in 4.0**. Existing
  code keeps compiling with a deprecation warning.

### Fixed

- **OpenAPI 3.1 (`OpenAPIHTTPFilter`): a response that is not JSON.** A controller action
  returning a `TStream` was documented as `application/json` with the schema of `TStream`
  (`Size`, `Position`); `[MVCProduces]` was not read. Now a stream is
  `{"type": "string", "format": "binary"}` under the media type of `[MVCProduces]`
  (`application/octet-stream` without it), any other return type goes under that media type
  (`application/json` without it), and a success `MVCSwagResponses` without a model, on an
  action that produces a media type that is not JSON, gets the binary schema under it.
- **OpenAPI 3.1 (`OpenAPIHTTPFilter`): `MVCSwagResponses` with `DataEnvelope` or a record.**
  The envelope (`{"data": <model>}`, `{"data": [<model>]}`) and the record model were ignored;
  the Swagger middleware already honoured them.
- **The ETag middleware answered a `304` to a range with the range in its body**
  (HTTP.sys; Indy drops the body of a `304` by itself): it empties the body with
  `Size := 0`, which the stream of the `206` ignored. That stream now shrinks.
- **The logout of `JWTAuthenticationBundle` with the refresh token answered 500** when
  `refresh_token` in the body was an object or an array, after the access token was
  already blacklisted. Only a string is a refresh token; anything else is the logout of
  the access token alone.
- **The access tokens of the refresh token had no `nbf` and no `jti`**
  (`TMVCRefreshTokenCore`): a check with `NotBefore` in its claims, as the wizard
  writes it, refused every token of the login and of the refresh; and two logins of a
  user in the same second got the same token, so the logout of one ended the other.
- **A `HEAD` got the body of the `GET`** after its headers, which a client on a
  kept-alive connection reads as the start of the next response: on HTTP.sys for every
  answer (the kernel sends the entity it is given), and on the streamed answers of every
  host that streams (an action without `MVCHTTPMethod` accepts `HEAD`):
  `StreamItems`/`StreamEach`/`StreamDataSet` on Indy Direct and HTTP.sys, the writers of
  `MVCFramework.SSE.Writer` (SSE, JSON Lines, JSON array, CSV) on Indy Direct and on
  WebBroker under the Indy bridge. They get the headers only, and a source stops at its
  first `Connected` check (`False` once the headers are out): `StreamDataSet` pulls no
  record, `StreamItems`/`StreamEach` stop after the first item. Indy,
  Apache and IIS already dropped the body of the other answers to a `HEAD`;
  `TMVCSSEController` answers `GET` only. On a `HEAD` no host sends the `Content-Length`
  of the `GET`.
- **Indy Direct: a compressed response leaked the stream it replaced.**
  `CompressionHTTPFilter` (and the middleware) and `ETagHTTPFilter` (its 304 on a file or a
  stream) swap the body of the response; on Indy Direct the old stream was not
  freed, once per request. It is freed now, as HTTP.sys and WebBroker already
  did.
- **JWT and JWT cookie middleware: a handler that set `IsValid` only to accept
  could log in anybody.** The flag passed to `OnAuthentication` was not
  initialised, so a refused login read whatever was on the stack; it starts
  False now (the custom and Basic logins already did).
- **JWT middleware: a token without `iat` left `Context.LoggedUser.IsValid`
  False** although the token was accepted (the login time read as 0001-01-01).
  It is now the time of the request.
- **JWT middleware: on a public action the token in `access_token` was ignored**
  (it was looked for with a `Bearer ` prefix); it is read as on a private action.
- **`TRoleBasedAuthHandler` on a target RTTI cannot find by name** (a Minimal API
  route, a controller declared in an implementation section) raised an access
  violation. A Minimal API route needs authentication and is authorized; a
  controller it cannot read is refused (its `MVCRequiresRole` cannot be checked).
- **`THMACJWTSigner` accepted an empty or the published demo key**: it raises,
  as `TJWT.Create(secret)` does.
- **`Context.LoggedUser.CustomData` assigned twice leaked the first object**
  (two JWT filters on one request).
- **An HTTP filter that raised before `ANext()`** (a refused login) skipped
  `OnAfterRouting` of the classic middleware and the release of the session.

- **HTTP filters and controllers.** The code of an HTTP filter after `ANext()` ran
  before the action of a controller (the chain was run by the Minimal API
  middleware, inside `OnBeforeRouting`), so a filter that works on the answer
  (`ETagHTTPFilter`, `CompressionHTTPFilter`, `AnalyticsHTTPFilter`, `TraceHTTPFilter`, the session and ActiveRecord
  filters) saw an empty response and released its resources too early. The
  engine now runs the HTTP filters around the whole request: classic middleware
  hooks, routing, the action and the end of the request. `Use` is a
  method of `TMVCEngine`, so a project that does not use the Minimal API does
  not need `MVCFramework.MinimalAPI` in its `uses`; the filters wrap the classic
  middleware whatever the order of registration. `TMVCHTTPFilter` and
  `TMVCHTTPFilterNext` are declared in `MVCFramework`; the names stay valid from
  `MVCFramework.MinimalAPI`.
- **`BasicAuthEndpointFilter`** passes the controller and the action of the
  context to the handler (`OnRequest`, `OnAuthorization`); it used to pass empty
  strings.
- **`RangeMediaHTTPFilter` with a folder written with `/`.** The filter did not
  normalize the folder (`TMVCRangeMediaMiddleware` does): `RangeMediaHTTPFilter('/media',
  'www/media')` on Windows left the root with mixed separators, the check that the
  file stays inside the root failed for every file and the folder answered `404`.
- `ActiveRecordHTTPFilter` with additional connections: the middleware it replaces
  worked out the number of additional connections inside the one-time load of the
  connection definitions, so only the first instance of the process added them.

- **Request body: an integer out of the range of its target is a 400, on every integer type.** The range check
  covered only `jdtLong`/`jdtULong`; a number the parser classifies as `jdtInt` went straight in, so `70000`
  into a `NullableUInt16` became `4464`, `-1` became `65535`, `-1` was accepted into `UInt64`/`NullableUInt64`,
  and `2147483648` into a plain `Integer` became `-2147483648`. Now every integer target (`Integer`, `Cardinal`,
  `Word`, `Byte`, `ShortInt`, `SmallInt`, `Int64`, `UInt64`, subranges, and the supported `Nullable*` integers)
  is checked against its RTTI bounds, and the refusal, like "Cannot deserialize integer value" and the other
  value errors, carries status 400 instead of 500: `Integer value out of range for "altitude"`.
- **`UInt64`, `NullableUInt64` and `Cardinal` above the signed range were serialized negative**
  (`18446744073709551615` came out as `-1`), and `NullableUInt32` above `2^31` too. Fixed in the
  JsonDataObjects serializer and in the streaming serializer used by `OKResponse(obj)`.
- **`OKResponse(obj)` wrote `TTime` and `TDate` as a full timestamp** (`"1899-12-30T20:00:00.000+01:00"`) while
  returning the object directly gave `"20:00:00"`. The streaming serializer now writes `TTime` as `hh:nn:ss`
  and `TDate` as `YYYY-MM-DD`, as the other path does.
- **Swagger: `MVCSwagFormat`, `MVCSwagMaxLength`, `MVCSwagMinimum`, `MVCSwagMaximum`, `MVCSwagPattern` were
  never read.** They now appear in the schema of the field (`format`, `maxLength`, `minimum`, `maximum`,
  `pattern`) in both the Swagger 2.0 and the OpenAPI 3 document. SwagDoc: a number field writes its `format`.
- **Swagger: an action with only `[MVCHTTPMethod([httpQUERY])]` made `/swagger.json` and `/openapi.json`
  answer 500** (access violation). The path was created before the verb loop skipped QUERY, and the path sort
  comparer was not a total order, so QuickSort read past the list. The path is now created on the first
  operation the document can express and the comparer is consistent. In the OpenAPI 3 document (3.2.1) a QUERY
  action is a `query` operation; in Swagger 2.0, which has no such key, it is left out.
- **Swagger 2.0: a body parameter carried `type`/`items` next to `schema`** (`MVCSwagParam(plBody, ..., ptArray)`
  and every ActiveRecord controller body), which `openapi-spec-validator` rejects. A body parameter is now
  described by its schema alone. The documents of the test suite validate clean (2.0, 3.2.1, 3.1.0).
- **The JWT check (rc8's `JWT(...)` endpoint filter, now part of `JWTAuthenticationBundle`): `AuthorizeEndpointFilter` and `RequireRoleEndpointFilter` always answered 401.** The filter read the user
  name from `sub`, which its own login never writes, did not copy the roles, and a token with no `iat` left
  `LoggedUser.LoggedSince` invalid. The login now writes `username`, `roles` and `iat` as
  `TMVCJWTAuthenticationMiddleware` does, and the filter fills `LoggedUser` (user name, roles, custom claims)
  from them; `sub` stays a fallback for tokens issued elsewhere.
- **`CORSEndpointFilter(...)` endpoint filter documentation.** It said it handles preflight; an endpoint filter runs only on a
  matched route, so an `OPTIONS` on a GET/POST-only route is a 404 from the router. The comment now says so and
  points to `CORSHTTPFilter`, the HTTP filter that runs before routing.
- **Minimal API: `WithName` on a `MapMethods` whose first verb is QUERY was lost** (QUERY has no OpenAPI 3.1
  key, so the name never reached the document). The name now lands on the first verb that is not QUERY.
- **JSON-RPC: an invalid request whose id can be read answers with that id.** `"jsonrpc":"1.0","id":12` answered
  `"id":0`, because the version was checked before the id was read. Log message grammar: "has not been found".
- **ActiveRecord controller: an RQL error caused by the request is a 400** (bad syntax, or an unmapped field
  with `AllowUnmappedRQLFields = False`), not a 500.
- **TemplatePro: compile errors and a damaged view cache now go through the view error handler.** They were
  raised outside it, so they were not logged as "View [x] error" and, in a release build, a raw
  `ETProCompilerException` with the server path reached the client.
- **WebBroker: `ContentParam` gives the last value of a repeated form field**, as Indy Direct and HTTP.sys do
  (the forms-library checkbox posts "false" then "true").
- **WebBroker (Indy bridge): cookie values are decoded**, as on the other hosts: `ciao+Bruce%3B+100%25` reads
  `ciao Bruce; 100%`. A stray `%` no host wrote is handed over as it came on every host, instead of a 500 on
  Indy Direct and HTTP.sys.
- **One reason phrase per status code on every host, the RFC 9110 one.** The response adapters set the phrase
  from the framework table whenever the status is set, so the host's own table (Indy, WebBroker, HTTP.sys) no
  longer decides. The table now uses the RFC 9110 names: 413 `Content Too Large`, 414 `URI Too Long`,
  416 `Range Not Satisfiable`, 422 `Unprocessable Content`, 506 `Variant Also Negotiates`. HTTP.sys no longer
  answers `413 OK` and its 413 has the body the other hosts give.
- **Unknown verbs (`PROPFIND`, `MOVE`, `BREW`) answer 501 on every host** (RFC 9110 9.1); they were 500 on
  Indy Direct and WebBroker.
- **A malformed percent-escape in the query string (`?q=100%`) is a 400 on every host**, not a 500.
  `[MVCFromQueryString]` no longer decodes the value a second time (`%2541` reached the action as `A`).
- **A bad value in a path parameter no longer echoes the converter's exception** (`[CLASS: EConvertError]
  [MSG: ...]`) to the client: the message is `Invalid Integer value for param [ID]`, the detail is logged.
- **HTTP.sys and WebBroker (Indy bridge): `RawPathInfo` is the path as sent** (`/a%41b`), as on Indy Direct; it
  was the decoded one.
- **Indy Direct and WebBroker: `Stop` then `Listen` on another port kept the old port** when both hosts meant
  every interface (Indy keeps the bindings of the first activation).
- **Console log with stdout redirected: per-request lines were empty `[INFO]` lines.** The plain renderer of
  `TMVCColorConsoleRenderer` now writes the request context (`200 | 3ms | 127.0.0.1 | GET "/people"`) and the
  context of other log items.
- **`ProblemDetails` comment** said `application/json`; it is `application/problem+json`.
- **The status line never carries request text.** An error raised before a controller is chosen used the
  exception message as reason phrase (`HTTP/1.1 501 Unknown HTTP method [PROPFIND]`); it is now the RFC 9110
  phrase (`501 Not Implemented`), the message stays in the body and the log. The basic/custom authentication
  middleware no longer puts the server name in the 403 status line.
- **HTTP.sys: `Listen` on a server that is already listening is a no-op**, as on Indy Direct and WebBroker (it
  raised `HTTP.sys error in HttpAddUrl`). Stop, then Listen, to change port or host.
- **HTTP.sys: one `Server` header.** The framework's `Server` header goes into the known header slot, so HTTP.sys
  does not add a second one. An error response rendered for the client's `Accept` now carries the charset
  (`application/json; charset=utf-8`) on every host; Indy was adding it on its own, HTTP.sys and WebBroker were not.
- **A string or a fraction for an integer field is a 400 with a message that says so.** `{"heading":"north"}` into
  a plain `Integer`/`Int64` raised `EConvertError` (500); `{"heading":12.5}` said "Integer value out of range". Both
  now answer 400 `Integer value expected for "..."`.
- **An empty response has no body on every host.** An action that only sets a status (`202` for an MCP
  notification, `200`) went out on Indy Direct and the WebBroker Indy bridge as `text/html` with
  `<HTML><BODY><B>202 Accepted</B></BODY></HTML>`, Indy's default content. Now `Content-Length: 0` and no body.
- **Swagger / OpenAPI 3: a property with `[MVCRequired]` is in the schema's `required` list**, in both documents.

- **HTTP.sys: a request with two or more custom headers was never answered.**
  The headers HTTP.sys does not know (`X-Foo`, `jwtusername`, ...) were read through an
  `array[0..0]` indexed in a loop; with range checking on (any Debug build of a
  service) the second one raised `ERangeError` in the worker thread and the client
  waited for a response that never came. They are now walked by pointer, and any
  other exception while the request is read is answered with 500. Seen as a
  `/login` that never replied when the credentials travelled in two headers.
- **Integers at the edge of their range are deserialized correctly (issues #918, #919).** A JSON number can reach
  the deserializer as `jdtInt`, `jdtLong`, `jdtULong` or, at the edges of the Int64 range, `jdtFloat`. Only some of
  those were handled for `NullableInt32`, `NullableInt64` and plain `Int64`, so `2147483647` into a `NullableInt32`
  raised "Cannot deserialize long integer value" and `-9223372036854775808` raised "Cannot deserialize
  floating-point value". Every classification is now accepted and the value is checked against the range of the
  target type (`NullableInt16/32/64`, `NullableUInt16/32/64`, `Int64`, `UInt64`); a value out of range raises
  `EMVCDeserializationException`. Limit: `-9223372036854775809` cannot be told apart from `-9223372036854775808`
  once the parser has turned it into a `Double`, so it is accepted as `Low(Int64)`.

- **`JsonDataObjects` (bundled copy): a number below `-(2^63 - 1)` or equal to `2^63` was read with the wrong value
  in 32-bit builds.** The `CPUX86` branches that convert a `UInt64` to a `Double` used the 32-bit field of the
  value, so `-9223372036854775808` came out as about `-2^64`. Win64 was not affected. Fixed in the bundled copy
  (five places); the same fix is proposed upstream, [ahausladen/JsonDataObjects#94](https://github.com/ahausladen/JsonDataObjects/pull/94).

- **HTTP.sys: a streamed response that fails before its first byte answers with the error instead of hanging.**
  The chunk writer marked the reply as already handled when it was created, so the error response built for a
  failure before the headers was never sent and the client waited for its timeout. The mark is now set when the
  headers go out. Indy Direct was not affected.

- **Swagger: a class used only as an array body gets an object definition (issue #924).** `[MVCSwagParam]` with
  `ptArray` produced a definition of the model class that was itself an array ("array of array"), and which of
  the two came out depended on which route registered the class first. The definition is now always an object;
  only the parameter schema is an array.

- **Swagger/OpenAPI: `[MVCSwagParam]` with `ptArray` describes its elements (issue #377).** A query/header/path
  parameter declared as `ptArray` was emitted as `"type": "array"` with the allowed values in a top-level `enum`
  and no `items`, which is not a valid array parameter. The parameter now has `"items": {"type": "string"}` and
  the values of `AEnumValues` move into `items.enum`. The same applies to the OpenAPI 3 document, where the
  `items` were missing altogether. Body parameters (`AJsonSchema`, model class) are unchanged.

- **JWT: a malformed token is refused with 401, not 500.** A token whose header or payload segment was not valid
  base64/JSON (for example `Authorization: Bearer not.a.token`) raised a JSON parser exception out of
  `TJWT.IsValidToken`, and the client got a 500 with the parser's message. It is now an invalid token like any
  other (`TJWT.LoadToken` returns `False`, the middleware and the filter answer 401).

- **JWT: a Bearer token sent on a session that Basic authentication opened no longer fails with 500.** The user
  of the request was loaded from the session, and the JWT middleware/filter then filled it in over the session's
  identity (`TMVCUser.LoggedSince already set`, roles added twice). The JWT code now starts from an empty user, so
  the token is the identity of that request. A request without a token keeps its session identity.

- **Swagger/OpenAPI: `[MVCSwagExample]` puts an example value on a DTO field (issue #927).** Swagger UI builds the
  sample request body from the type alone, so an optional integer id showed up as `0` (a valid id in many
  databases), and `Nullable` does not help because the UI drops `null` from the type. The new attribute takes the
  value as JSON text - `[MVCSwagExample('null')]`, `[MVCSwagExample('42')]`, `[MVCSwagExample('"abc"')]` - and the
  field is written with `"examples": [value]` in the OpenAPI 3 document and `"example": value` in the Swagger 2.0
  one. It works with or without `[MVCSwagJSONSchemaField]` on the same property. Additive: a field without the
  attribute is emitted exactly as before. Text that is not valid JSON raises an error when the document is built.

- **Swagger/OpenAPI: `[MVCSwagExampleFromFields]` gives a DTO an example made of its field examples (issue #929).**
  In a PATCH, where an absent field means "leave unchanged", the sample body must hold only the fields being
  changed. An example per field cannot say "absent", and Swagger UI samples every property, so "Try it out" also
  overwrote the fields the user did not mean to touch. On a class or record used as a body or response model, the
  new attribute writes on its definition an example object made only of the fields that have a `[MVCSwagExample]`:
  `"examples": [{...}]` in the OpenAPI 3
  document, `"example": {...}` in the Swagger 2.0 one. Swagger UI shows that object as the sample body and puts it
  in "Try it out". The names and the field filters are those of the schema (a `fsReadOnly` id is not in a request
  body), a nested object or array is in the example only through the example on its own property, and every field
  is still documented in `properties`. In a request body a required field without an example raises an error when
  the document is built; a response model is not checked. Not applied to a class nested inline in another one.
  Additive: a class without the attribute is emitted as before. Like `[MVCSwagExample]`, it is read by the
  Swagger document (`SwaggerHTTPFilter`, `TMVCSwaggerMiddleware`), not by the native OpenAPI 3.1 emitter
  (`OpenAPIHTTPFilter`).

- **An exception raised in an HTTP filter is answered as the dispatcher answers it (issue #933).** In DEBUG builds
  the message of an exception raised in an HTTP filter was prefixed with its class name:
  `raise EMVCException.Create(429, 'Too many attempts')` answered `"message":"[EMVCException] Too many attempts"`,
  while the same exception raised in a controller action or in a classic middleware answered
  `"message":"Too many attempts"`. A filter exception now goes through the same code as the dispatcher's: same
  status, message, class name (DEBUG only), log line and custom exception handler call (`SetExceptionHandler`, with
  `Controller = nil` as before). As a result, an `ESqidsException` raised in a filter answers 400 there too, and an
  `EMVCSessionExpiredException` stops the session when there is one (also in the dispatcher: without a session
  factory it no longer turns into a configuration error).

- **Swagger: `JWTAuthenticationBundle` is documented as the JWT middleware is (issue #932).** `SwaggerHTTPFilter`
  and `TMVCSwaggerMiddleware` looked for the deprecated `TMVCJWTAuthenticationMiddleware` only: after the move to
  `AEngine.Use(JWTAuthenticationBundle(...))` the document lost the login path and the `bearer` security
  definition. Now a JWT bundle, engine-wide or on a Minimal API route group, gives the same login operation on its
  `LoginURL` (headers `jwtusername`/`jwtpassword`), the `basic` definition that operation refers to, and `bearer`.
  With `LoginURL = ''`, or with a `RefreshTokenStore` (its login answers the token pair, not the documented
  `{"token": ...}`), the document has `bearer` only. Two bundles on the same login URL give one path. The document
  of a server with the JWT middleware does not change. `JWTCookieAuthenticationBundle` is not documented, as the
  cookie middleware was not. New: `TMVCEngine.FilterBundles`, the bundles registered with `Use`.

- **The Trace filter and `TMVCTraceMiddleware` garbled a body with non-ASCII characters (issue #931).** A response
  written as a stream was decoded with the Windows ANSI code page, so `{"message":"Çıkış yapıldı"}` was logged as
  `{"message":"Ã‡Ä±kÄ±ÅŸ yapÄ±ldÄ±"}`. Request and response bodies are now decoded with the charset of their
  `Content-Type` (quoted or not); when it names none, as UTF-8, or in the ANSI code page that `Render(string)`
  writes such a body in. A UTF-8 character cut in two by the size limit is left out, and bytes that are not UTF-8
  when the charset says so are logged as `<N bytes not decodable as utf-8>`: before, a request body that was not
  valid UTF-8 made the trace raise `EEncodingError` and the request answer 500. Only the first characters of a
  request body are decoded, not the whole body. On Indy Direct the charset of a response is kept apart from its
  `ContentType`, so a response is read as UTF-8 or ANSI there.

- **Wizard: the MCP options say they need mcp-server-delphi, and whether it is there.** As soon as one of the MCP
  options is on, a notice under them says that the generated code uses mcp-server-delphi
  and, checked against the IDE library path, whether it was found (grey) or the project will not compile until it
  is installed (red). The same warning in the Messages window after the generation is unchanged.

- **JSON-RPC: an error raised before routing answers with the id of the
  request.** An exception in `OnBeforeRoutingHook`, or a call to a reserved
  method name, was answered with `"id": 0` whatever the request id was, so
  the client could not match the error to its request.
- **Wizard, JWT: the login of a generated project failed.** The generated
  authentication handler stored a session value under the key `username`,
  which the JWT middleware reserves for its own claim, so `/login` answered
  `JWT Error: "username" is a reserved key name` to valid credentials. The
  handler no longer writes that key.
- **`IMVCServer.Listen` / `RunAndWait` honour the host on Indy Direct and
  WebBroker.** `Listen(8080, 'localhost')` listened on every interface: the
  second argument was stored and never used (HTTP.sys did use it). It now
  binds that address only: an IPv4 address, an IPv6 address (with or without
  brackets) or a host name; `'localhost'` is `127.0.0.1` and `::1`. The
  default (`'0.0.0.0'`), `''`, `'*'` and `'+'` still mean every interface, so
  code that passes no host is unaffected. `Listen` on a server that is
  already listening does nothing: call `Stop` first.
- **Wizard: a console program that fails at startup ends with exit code 1.**
  The generated `.dpr` logged the exception and ended with 0.
- **`ProblemDetails(...)` is now a real RFC 7807 body.** It came out wrapped
  in `{"data": {...}}` with `Content-Type: application/json`; the problem
  object is now the whole body, sent as `application/problem+json`. This is
  what the Minimal API renders for an unhandled exception raised by a route
  handler or an endpoint filter (engine-wide HTTP filters go through the
  engine's error response, as classic middleware does).
- **Minimal API validation errors carry the per-field messages.** The 422
  body only listed the field names in `detail`; it now adds an `errors`
  object (property name to message), the same information a controller
  returns in `items`.
- **Minimal API: internal exception text no longer reaches the client.** The
  ProblemDetails `detail` carried `E.Message` of any exception, so in a
  release build a FireDAC error sent its SQL statement to the caller. It now
  goes through `MVCClientSafeExceptionMessage`, like the controller pipeline:
  the framework's own exceptions keep their message, anything else becomes
  "Internal server error" outside DEBUG.
- **Minimal API exceptions are logged.** Only the router line (`status=500`)
  reached the log; now the class, the message and the request are logged as
  the controller pipeline does (warning with the field errors for a 422).
- **A body of the wrong JSON kind answers 400, not 500.** `[MVCFromBody]` on
  an object receiving a JSON array (or a list receiving an object), and the
  same for a Minimal API class argument, raised an `EMVCSerializationException`
  with the default 500: the client's mistake reported as a server error. It is
  now 400, like a body that does not parse. A list with a registered type
  serializer receiving an object raised `EInvalidCast` (500) and leaked the
  parsed document; it is a 400 too.
- **Returning the bound body in the response no longer frees it twice.**
  `Result := OkResponse(Person)` with `[MVCFromBody] Person` freed the object
  once as body parameter and once with the response (an access violation
  after the reply was sent). In the Minimal API, `Result := Ok(Item)` with a
  bound class argument freed it *before* rendering. The response now takes
  the object over and frees it once, after rendering.
- **`StrToJSONObject` / `StrToJSONArray` leaked the parsed document** when
  the text was valid JSON of the other kind (an array where an object was
  expected, and the reverse): the function answered nil (or raised) and the
  document was never freed.
- **No exception class name in release error bodies, and `GetErrorPageHandler`
  follows the same rule.** An `EMVCException` that reached the engine without a
  controller (e.g. raised by an HTTP filter) sent its class name even in
  release. `GetErrorPageHandler` put the class name and the raw message in the
  body and in the redirect URL (`?class=...&error=...`): outside DEBUG the class
  is now empty and the message is the client-safe one.
- **Serializer: "Cannot serialize field" no longer carries the inner
  exception.** The class and message of the getter's exception went into an
  `EMVCSerializationException` message, which is sent to the client. They are
  logged server-side; the client gets the field name.
- **`TMVCRESTClient` returned an empty `Content` for `+json` / `+xml` media
  types** (`application/problem+json`, `application/vnd.api+json`, ...): they
  were treated as binary. They are read as text now.
- **`TMVCRESTClient.Post(Resource, Body, 'application/x-www-form-urlencoded')`
  sent an empty body.** The body was always rebuilt from the
  `AddBodyFieldURLEncoded` fields, even when there were none; a raw body is
  now kept.

- **Expressions (TemplatePro `@(...)`, `.env` `$[...]`)**, from the
  ExprEvaluator library: a `;` inside a string or a comment no longer splits
  the expression (a trailing `;` is allowed); `not(x)`, `and(x)` and the other
  keywords may be followed by `(`; numbers are converted and concatenated with
  a dot whatever the locale; a variable in a branch skipped by short-circuit is
  not asked to the external resolver; `sqrt`, `log`, `logn`, `power` and `^`
  raise instead of returning NaN or Infinity.
- **TemplatePro: more dataset field types, unsigned values.** Fields of type
  `ftShortint`, `ftByte`, `ftLongWord`, `ftExtended`, `ftGuid`, `ftFixedChar`,
  `ftFixedWideChar` and (Delphi 13+) `ftLargeUint` can be used in views, and
  unsigned values above the signed maximum no longer render as negative
  numbers. Contributed by Patrick Premartin.
- **`OKResponse(StrDict(...))` returned `{"data":{}}`.** The streaming fast
  path of `OKResponse(TObject)` walked the properties of any class, so a
  class with its own type serializer (`TMVCStringDictionary`, user-registered
  ones) came out as an empty object. Such classes now go through their
  serializer again, as a root object and as list items.
- **`[MVCMaxLength(n)]` and `[MVCPattern(...)]` now always validate.** In a
  unit that used `MVCFramework`, Delphi resolved them to the Swagger
  documentation attributes `MVCFramework.MVCMaxLengthAttribute` /
  `MVCPatternAttribute` instead of the validators in
  `MVCFramework.Validators`, silently and whatever the order of the uses
  clause, so the validation never ran. Those two documentation attributes
  have been removed (use `MVCSwagMaxLength` / `MVCSwagPattern` for Swagger
  documentation): the names now always mean the validators. Models that
  declared them in such a unit start validating after the upgrade.
- **Swagger document `host` on Indy Direct and HTTP.sys:** it contained the
  port twice (`"localhost:8080:8080"`), which made the document invalid, and
  behind a proxy or a port mapping it mixed two ports (`"localhost:9090:8080"`).
  The port is now added only when the Host header has none. WebBroker output
  is unchanged; so is any application that passes `AHost`.
- **Swagger middleware, paths with a parameter converter** (`($ID:sqids)`):
  the converter stayed in the documented path literally. The path is now
  `{ID}` with a string path parameter.
- **SQL Server: Insert and Update failed on a table with enabled triggers**
  whenever something had to be read back (the generated key, a `foRefresh`
  field): SQL Server rejects `OUTPUT inserted.col` without `INTO` there.
  Diagnosis: Flavio Basile.
- **SQL Server: optimistic locking was not detected behind a trigger without
  `SET NOCOUNT ON`.** The driver reported the trigger's row count, so a stale
  `foVersion` Update, or an Update/Delete of a missing row, looked successful.
  Framework statements now read `@@ROWCOUNT` right after the statement
  (`TMVCSQLGenerator.GetRowsAffectedSQL`, empty on every other engine).
- **`Delete`, `DeleteRQL`, `DeleteAll`, `HardDeleteRQL` and `RestoreRQL`
  failed on any engine for a class with `foRefresh` fields** (FireDAC -308):
  the refresh read-back now runs only for the entity's own Insert and Update.
- **A `TGUID` auto-generated primary key stayed empty after Insert**, on
  every engine.
- **SQL Server Insert did not quote table and column names**: a column with a
  space in its name broke the statement.
- **Delphi 13.2: `MVCFramework.ActiveRecord.pas` did not compile** (E2010, #917),
  and `MVCFramework.JWT.RSA.pas` did not compile against TaurusTLS after its
  PR #278.

- **HTTP.sys dispatched `SEARCH` - and a dozen other verbs - as `GET`.**
  The HTTP.sys request adapter mapped the kernel's `HTTP_VERB` enumeration
  to `TMVCHTTPMethodType` with a `case` that covered seven verbs and
  answered `httpGET` for everything else, while its guard let the whole
  `OPTIONS..SEARCH` range through. `SEARCH`, `CONNECT`, `TRACK`, `MOVE`,
  `COPY`, `PROPFIND`, `PROPPATCH`, `MKCOL`, `LOCK` and `UNLOCK` therefore
  reached the router as `GET` requests, silently: a `GET`-only action could
  be invoked with any of them. The adapter now parses the verb string it
  had already computed, which is what the Indy and WebBroker adapters have
  always done. Only the HTTP.sys host was affected.

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
- **Wizard, Minimal API preset: the admin routes are fail-closed.** `Routes.RootU`
  used to guard `/api/v1/admin/*` with a placeholder filter that let in any
  `Authorization: Bearer <anything>`. Without JWT the project now gets a token of
  its own in `.env` (`api.admin.token`, 384 random bits, like `mcp.auth.token`),
  checked by the standard `BearerAuthHTTPFilter(StaticBearerTokens(...), ADMIN_PATH)`
  in `EngineConfigU`; the server does not start when the token is empty, and the
  group adds `AuthorizeEndpointFilter()` so a lost filter still answers 401. With JWT the group
  gets `JWTAuthenticationBundle(...)`: the check on the group, the login on the engine.
- **Wizard, Minimal API WebApp preset: the admin login is fail-closed.** `POST
  /web/login` accepted `admin`/`admin`, written in `Routes.RootU`. It now reads
  `ADMIN_USER` and `ADMIN_PASSWORD` from `.env` (generated empty) and refuses
  everyone while they are empty, as the generated JWT handler does.
- **Wizard, generated `.dproj`.** The Debug configuration now defines `DEBUG`
  (`<DCC_Define>DEBUG;$(DCC_Define)</DCC_Define>`; it defined nothing, so a Debug
  build behaved as Release: no exception class names in error responses), Release
  defines `RELEASE`, the unit scope names are set (`DCC_Namespace`: an HTTPS
  project failed under command-line msbuild on TaurusTLS's `uses Classes`), the
  unit search path is `$(DMVC)`, and the `DCCReference` list is read from the
  generated `.dpr`, so it no longer misses `AuthenticationU`, `ServiceU`,
  `WebModuleU` or the MCP sample providers.
- **Wizard, Project Manager items.** "New REST Controller" proposes `TPerson` for
  `People` (irregular plurals and `-sses`/`-xes`/`-shes` handled); "New Web
  Controller" creates `Controllers.<Name>WebU` / `T<Name>WebController`, so it no
  longer collides with a REST controller of the same name; a blank line always
  separates a `uses` clause from the routine after it; `ConfigureEngine` and
  `WebModuleCreate` declare their locals in one `var` section.

### Performance

- `RangeMediaHTTPFilter` and `FileServerBundle` send a `206` from the file: on Indy Direct
  and WebBroker the slice (up to 8 MB) is read as it is sent, no longer copied into memory
  first. HTTP.sys still copies any response stream into memory before it sends it (as before,
  not changed here).
- The Minimal API dispatcher splits and decodes the path once per request, not once per
  route of the verb.

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

- `CORSEndpointFilter(...)` and `CORSHTTPFilter(...)` split and trim the allowed-origins list
  once when the filter is created, not on every request.

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