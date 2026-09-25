# Changelog

All notable changes to DelphiMVCFramework will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [3.5.0-silicon] - Unreleased

### ⚠ BREAKING CHANGES

Several defaults have been flipped. The first two affect the JSON
output of `OKResponse(TObject)` and are one-line overrides to restore
the pre-3.5 behaviour if needed; the last three are security defaults.

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

### Added

- **OpenAPI 3 from the Swagger middleware.** `TMVCSwaggerMiddleware` and the
  `Swagger(...)` HTTP filter take a new optional last parameter,
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
  that touch it see **Fixed**).
- **The IDE wizard generates API documentation.** New option "API
  documentation (OpenAPI 3)", on by default for RESTful, Full-Stack, Custom
  and Minimal API RESTful projects. Controller projects register the Swagger
  middleware with OpenAPI 3; Minimal API projects register the `OpenAPI(...)`
  filter. Both publish the document at `/openapi.json` and serve Swagger UI
  at `/swagger`, both switched by `dmvc.openapi.enabled` in `.env` (default
  `true`; set it to `false` to publish no documentation, e.g. in
  production). The wizard downloads the official Swagger UI release (5.33.0,
  SHA-256 verified) into `bin\www\swagger` while it creates the project,
  showing a progress dialog that can be cancelled and gives up after 30
  seconds. Without network access the project is still created and the
  folder contains a README with the download steps.
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
    CORS('*', False, '', 'Content-Type,Authorization',
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
  opt-in). An OAuth2-style refresh token layer on top of the existing
  `TMVCJWTAuthenticationMiddleware`: short-lived access tokens plus a
  long-lived, revocable refresh token with rotation and reuse-detection.
  The refresh token is an opaque random string (not a JWT); the store
  keeps only its SHA-256 hash. Presenting an already-rotated token
  revokes the whole family (replay defence). Ships with a pluggable
  store interface, an `TMVCActiveRecord`-backed implementation
  (`MVCFramework.JWT.RefreshToken.ActiveRecord`), endpoint filters
  (`MVCFramework.JWT.RefreshToken.Filters`) and a sample
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

- **Native OpenAPI emitter (`OpenAPI(...)` filter, `TMVCOpenAPI3Middleware`):**
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
  `MemorySession`/`FileSession`/`DatabaseSession` filters). Pass `False`
  explicitly to restore the old behaviour.
- The session cookie now carries `SameSite=Lax` - **on Delphi 11 and later
  only**. `TCookie.SameSite` arrived in 10.4.2 but `CompilerVersion` cannot tell
  10.4.2 from 10.4, so builds on 10.2-10.4 emit no SameSite attribute at all.
- **New:** `Secure` on the session factory, and a matching last argument on all
  six entry points (`UseMemorySessionMiddleware` and friends, and the
  `MemorySession`/`FileSession`/`DatabaseSession` filters). **Default `False`**,
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
  now carries the `JWT(...)` endpoint filter, which verifies inside the route
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

### Deprecated

- **`TMVCListener` / `TMVCListenerProperties` / `TMVCListenersContext`**
  (`MVCFramework.Server`) are deprecated and **will be removed in 4.0**.
  After the Indy Direct conversion they are a thin wrapper over the
  `IMVCServer` abstraction and expose strictly less (Indy-only, no HTTPS,
  only `MaxConnections`). Build servers through `TMVCServerFactory` /
  `IMVCServer` (`MVCFramework.Server.Factory`) instead, which also gives you
  the HTTP.sys / WebBroker backends and built-in HTTPS. Existing code keeps
  compiling with a deprecation warning until you migrate.

### Fixed

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