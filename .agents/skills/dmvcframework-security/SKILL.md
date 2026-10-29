---
name: dmvcframework-security
description: Use when writing or reviewing any DelphiMVCFramework server-side code that touches untrusted input — request bodies, query strings, headers, cookies, uploads, redirects, SQL/RQL, HTML output, JWT, CORS, secrets. Covers access control, mass assignment, SQL injection, XSS in TemplatePro, CSRF, path traversal, SSRF, file uploads, security headers, JWT hardening and secret management, with the DMVCFramework API that actually implements each control. Read before shipping an endpoint that a stranger can call.
---

# DMVCFramework — Secure coding reference

**This is a reference for the other DMVCFramework skills.** Whenever you write an endpoint that accepts
input from a client — a controller action, a minimal-API handler, a web page, a filter — the controls below
apply. They are not optional extras.

Adapted from the OWASP-style [VibeSec](https://github.com/BehiSecc/VibeSec-Skill) secure-coding guide
(Apache-2.0), rewritten against the real DMVCFramework API: every class, attribute and filter named here was
verified in `sources/`.

**The rule that catches most of it:** *never trust anything that arrived over the wire* — not the body, not
the query string, not a header, not a cookie, not a filename, not a URL, not a JWT you have not verified.

---

## When in doubt about an API — verify it, never guess

Never invent an identifier or answer from memory. If you need a signature this skill does not cover, ask the
user for the path to their **DelphiMVCFramework checkout** and read `sources/` (and the matching `samples/`
project); failing that, read the official repository —
[sources](https://github.com/danieleteti/delphimvcframework/tree/master/sources) ·
[samples](https://github.com/danieleteti/delphimvcframework/tree/master/samples).
If you still cannot verify it, **say so**. Where a skill and a sample disagree, **the sample wins**.

---

## 1. Access control — the #1 cause of real breaches

Authentication says *who you are*. Authorization says *what you may touch*. Do not confuse them: a valid
JWT is not permission to read row 42.

**The IDOR trap.** This is the single most common serious bug in a CRUD API:

```delphi
// BROKEN — any authenticated user can read anyone's order
function TOrdersController.GetOrder(const ID: Integer): IMVCResponse;
begin
  Result := OKResponse(TMVCActiveRecord.GetByPK<TOrder>(ID));
end;
```

The ID comes from the URL. Nothing ties it to the caller. Scope **every** lookup to the authenticated
principal, in the query — not with an `if` after the fact:

```delphi
function TOrdersController.GetOrder(const ID: Integer): IMVCResponse;
var
  lOrder: TOrder;
begin
  // the ownership check IS the query
  lOrder := TMVCActiveRecord.GetOneByWhere<TOrder>(
    'id = ? and customer_id = ?', [ID, CurrentUserID], False);
  if lOrder = nil then
    Exit(NotFoundResponse);      // same answer as "not yours" — do not leak existence
  Result := OKResponse(lOrder);
end;
```

- Deny by default. A new action is unreachable until you decide who may call it.
- Enforce on **every** verb. A GET that is properly scoped and a DELETE that is not is still a breach.
- Return **404, not 403**, for a resource that exists but is not the caller's — 403 confirms it exists.
- Never trust a role, tenant or user id that arrives in the **body or a query param**. Read it from the
  verified token / session only.

**Where the identity comes from:**

| Style | Read the principal from |
|-------|------------------------|
| JWT (controller) | `Context.LoggedUser.UserName` / `.Roles` / `.CustomData` — populated by the check of `JWTAuthenticationBundle` / `JWTCookieAuthenticationBundle` (or the deprecated `TMVCJWTAuthenticationMiddleware` of a 3.4 project) after signature validation |
| Minimal API | the `AuthorizeEndpointFilter` / `RequireRoleEndpointFilter('admin')` endpoint filters, plus `Ctx.LoggedUser` |
| Web app session | `Context.Session['user']` — only after the session filter/middleware has run |

Role checks belong on the route group or the action, not scattered through the body:

```delphi
// minimal API — the whole group is gated
var lAdmin := lApi.Prefix('/admin').Use(AuthorizeEndpointFilter()).Use(RequireRoleEndpointFilter('admin'));
```

---

## 2. Mass assignment — the framework will happily overwrite `is_admin`

`[MVCFromBody]` deserializes the JSON straight into your object. If that object is your ActiveRecord entity,
the client controls **every writable property**, including the ones you never meant to expose:

```delphi
// BROKEN — POST {"email":"x@y.z","role":"admin","id":1}
function TUsersController.CreateUser(const [MVCFromBody] User: TUser): IMVCResponse;
begin
  User.Insert;                       // role = admin. Congratulations.
  Result := CreatedResponse('/users/' + User.ID.Value.ToString);   // ID is a NullableInt64
end;
```

Two fixes, in order of preference:

**a) Bind to a DTO that only has the fields the client may set.** This is the recommended two-layer pattern
and it is self-documenting:

```delphi
type
  [MVCNameCase(ncCamelCase)]
  TCreateUserDTO = class
  private
    fEmail: string;
    fFullName: string;
  public
    [MVCRequired] [MVCEmail]
    property Email: string read fEmail write fEmail;
    [MVCRequired]
    property FullName: string read fFullName write fFullName;
    // no Role. no ID. no PasswordHash.
  end;
```

**b) Block the field on the entity** with `[MVCDoNotDeserialize]` (honoured by the JSON serializer):

```delphi
[MVCTableField('role')]
[MVCDoNotDeserialize]              // server-assigned; ignored if it arrives in the body
property Role: string read fRole write fRole;
```

`[MVCDoNotSerialize]` hides a field from the **response** (use it for `password_hash`); `[MVCDoNotDeserialize]`
protects it from the **request**. They are different attributes — you often want both.

ActiveRecord's `foAutoGenerated` / `foReadOnly` / `foDoNotInsert` field options keep a column out of the
INSERT, which is useful — but they do **not** stop the value from being deserialized into the object. Do not
rely on them as a mass-assignment guard.

### Validate the input DTO — and know what is not validated

Put validator attributes (`MVCFramework.Validators`) on the DTO's **properties**: `[MVCFromBody]` runs them
before the action and answers 422. Bound by length (`MVCMaxLength`) every string you store; restrict enums
and sort keys with `MVCIn` (case-insensitive). What is **not** validated for you: `[MVCFromQueryString]`,
`[MVCFromContentField]` and route parameters of a controller, HTML form posts, and a `[MVCFromBody]`
`TObjectList<T>` (wrap it in a DTO property). Validate those yourself with `TMVCValidationEngine.Validate`.
Details: the `dmvcframework` skill, `reference/validation.md`.

---

## 3. SQL injection

`TMVCActiveRecord` is parameterized everywhere. The only way to get injected is to build the string yourself.

```delphi
// BROKEN
lUsers := TMVCActiveRecord.Where<TUser>('email = ''' + lEmail + '''', []);

// CORRECT — placeholders, values in the array
lUsers := TMVCActiveRecord.Where<TUser>('email = ?', [lEmail]);
lUsers := TMVCActiveRecord.Where<TUser>('created_at > ? and active = ?', [lFrom, True]);
```

The same rule applies to `GetOneByWhere`, `GetFirstByWhere`, `Select`, `SelectOne`, `SelectRQL` and every repository method:
**the SQL string is a constant; user data goes in the params array.**

**RQL from the client** (`?rql=...`) is a real feature and a real surface. RQL is parsed, not concatenated,
so it will not smuggle SQL — but it lets the caller filter on **any mapped column** and page as deep as they
like. Treat it as a query API: only expose it on resources where every column is safe to filter on, always
combine it with the caller's ownership scope, and keep `MaxRecordCount` bounded.

Column and table names can never be parameterized. If a sort field comes from the client, **allow-list it** —
never interpolate it:

```delphi
if not MatchStr(lSortBy, ['name', 'created_at', 'total']) then
  raise EMVCException.Create(HTTP_STATUS.BadRequest, 'Invalid sort field');
```

---

## 4. XSS — and the one TemplatePro character that disables the defence

TemplatePro **HTML-escapes `{{:value}}` by default**. The `$` suffix turns escaping off:

```html
{{:comment.Body}}      {{# safe — escaped #}}
{{:comment.Body$}}     {{# RAW — an XSS hole if Body came from a user #}}
```

A block does the same for everything inside it — `{{autoescape false}}` … `{{endautoescape}}` disables
escaping exactly like `$` on every value in between. Search for both when reviewing a template.

Use `$` (or `autoescape false`) only for HTML **you** generated. If you must render user-submitted rich text,
sanitize it server-side with an allow-list of tags before it ever reaches `ViewData` — an escape-blacklist
will be bypassed.

**Escaping is HTML-entity encoding, nothing more.** TemplatePro's `HTMLEncode` turns `& < > " '` into
entities. That is enough for element text and for a **quoted** attribute value. It does not protect:

- a URL attribute: `<a href="{{:link}}">` with `link = javascript:alert(1)` contains none of those
  characters and runs. Allow-list the scheme (`https:`, or a relative path) on the server;
- an **unquoted** attribute (`<div class={{:cls}}>`): a space ends the value and starts a new attribute.
  Always quote;
- a JavaScript context — inside `<script>`, an `on*=` handler, or `hx-on`. Do not put user data there; pass
  it in a `data-*` attribute and read it from script.

For JSON APIs, set the content type correctly (`application/json`, which DMVC does) and never build HTML by
string concatenation in Delphi — render a template (see the `dmvcframework-webapp` skill).

Add a Content-Security-Policy (see §8): it is the backstop for the XSS you missed.

---

## 5. CSRF

**Cookie-based auth is vulnerable; `Authorization: Bearer` is not** (the browser does not attach a header
automatically).

The cookie of `JWTCookieAuthenticationBundle` is already correct by default —
`TMVCAuthCookieOptions.Default` is `Secure=True`, `SameSite=Strict`, and the cookie is always `HttpOnly`:

```delphi
lOptions := TMVCJWTCookieAuthenticationOptions.Default;    // Cookie = TMVCAuthCookieOptions.Default
lOptions.Cookie.Secure := dotEnv.Env('JWT_COOKIE_SECURE', True);   // HTTPS only — keep True in production
```

(A 3.4 project has `UseJWTCookieAuthentication(...).SetCookieSecure(True)`: deprecated in 3.5, same defaults.)

**`SameSite` is only written on Delphi 11 or later.** The framework sets the attribute under
`{$IF CompilerVersion >= 35.0}`: on 10.2 Tokyo – 10.4 Sydney the cookie carries no `SameSite` at all, and
the CSRF protection described here does not exist — the browser applies its own default (`Lax` in Chromium,
nothing in some others). On those versions either authenticate with `Authorization: Bearer` or add the
synchronizer token below.

**`SameSite` does not stop login CSRF.** A cross-site page that auto-submits a form to `/login` with the
attacker's credentials is a top-level navigation: the browser stores the attacker's cookie and the victim
works inside the attacker's account. Refuse a cross-site login before it reaches the login:
`lOptions.LoginAllowedOrigins := ['https://app.example.com']` makes the bundle answer 403 to a login whose `Origin`
is not in the list (a request without `Origin` is not a cross-site browser request and passes; the default, empty,
checks nothing — a wizard project reads `LOGIN_ALLOWED_ORIGINS` from `.env` and refuses to start when it is empty). With a login of your own, an `AllowedOriginHTTPFilter(['https://app.example.com'], '/login')`
HTTP filter registered **before** it does the same.

**Do not weaken `SameSite` to `None`** without a hard requirement; `Strict` is what makes the cookie flow
CSRF-safe for authenticated requests. If you ever must relax it (or you are on Delphi 10.x), add the
synchronizer token DMVCFramework ships — stateless: the token is the HMAC-SHA256 of a session id that your
callback reads from the request.

```delphi
// before routing, registered first: POST, PUT, PATCH, DELETE and QUERY need X-CSRF-Token or the
// csrf_token form field, otherwise 403. The login has no session yet: exempt it (Origin guarded above)
AEngine.Use(CSRFHTTPFilter(dotEnv.Env('CSRF_SECRET'),          // at least 16 characters, or it raises
  function(AContext: TWebContext): string
  begin
    Result := AContext.Request.Cookie('jwt_token');                 // the session id of the request
  end, 'X-CSRF-Token', 'csrf_token', ['/login']));
```

Put `CSRFToken(Secret, SessionId)` in the page (a hidden field, or `hx-headers`). The session id must not
change while the page is open: the raw JWT cookie does when `JWTCookieAuthenticationBundle` renews it (a token with a live
validity window) — then use a claim that stays, e.g. `jti`. `CSRFEndpointFilter(...)` is the same check as an endpoint
filter (routed requests only). A 3.4 project may have `TMVCCSRFMiddleware`: deprecated, same check.

And: **GET must never change state.** A GET that deletes something is CSRF-able no matter what you set.

---

## 6. Path traversal and file uploads

**Never build a filesystem path out of client input.** `../../` is the classic, but URL-encoding, UNC paths
and absolute paths all count.

```delphi
// BROKEN — GET /download?file=../../.env
lPath := TPath.Combine(lRoot, Context.Request.QueryStringParam('file'));
```

Use an indirect reference (an id → path lookup you control). If you truly must accept a name, canonicalize
and then verify containment:

```delphi
lFull := TPath.GetFullPath(TPath.Combine(lRoot, lName));
if not lFull.StartsWith(TPath.GetFullPath(lRoot) + PathDelim, True) then
  raise EMVCException.Create(HTTP_STATUS.BadRequest, 'Invalid path');
```

**Uploads** (`TMVCFormFile`, minimal API — see `dmvcframework-minimal-api`):

- `TMVCFormFile.FileName` **is attacker-controlled** (`..\..\x.aspx`, `C:\inetpub\x.aspx`, `NUL`,
  `report.pdf:stream`). Do not build a path out of it. Either generate your own name (a GUID) and keep the
  client's only as metadata, or use the overload that sanitizes for you:

  ```delphi
  lFile.SaveToFile(lUploadDir, lFile.FileName);   // the client picks the name, never the directory
  ```

  `SaveToFile(ADirectory, AClientFileName)` reduces the name to a bare leaf (`SanitizeLeafName`) and raises
  on one that would write somewhere else (a colon, a device name, a trailing dot or space). `SafeFileName`
  returns the same leaf for your own use. The one-argument `SaveToFile(APath)` refuses a path that ends with
  an unsanitized client name, but cannot repair one you built differently — prefer the two-argument form.
  A sanitized leaf can still **overwrite** another upload with the same name: when that matters, use a GUID.
- Do not trust `ContentType` either — it is a client-supplied header. Check the actual bytes (magic number)
  when the file type matters.
- Enforce a **size limit** and an **extension allow-list** (never a deny-list).
- Store uploads **outside** the static-files root, or the upload becomes a way to serve executable content.
- `ContentStream` is request-owned — read it, do not free it.

`FileServerBundle` and `StaticFilesHTTPFilter` (and the deprecated `TMVCStaticFilesMiddleware`) do check for directory
traversal on the paths they serve — a path out of the root answers 404 — but they protect *their* document root,
not any path you construct yourself.

---

## 7. SSRF and open redirect

Both come from the same mistake: taking a URL from the client and acting on it.

**SSRF** — the server fetching a client-supplied URL reaches things the client cannot: `localhost`,
`169.254.169.254` (cloud metadata), your internal network. If a feature needs to fetch a URL, allow-list the
**hosts**, resolve the DNS name and reject private/loopback/link-local addresses, disable redirect-following
(or re-validate each hop), and set a timeout.

**Open redirect** — `Redirect(lUrl)` where `lUrl` came from `?returnUrl=`:

```delphi
// BROKEN — /login?returnUrl=https://evil.example/phish
Result := RedirectResponse(Context.Request.QueryStringParam('returnUrl'));
```

Best: an **allow-list** of known destinations, or an id the server maps to a path. If you must accept a
path, accept only a local one — and `StartsWith('//')` alone is not enough: browsers treat `\` as `/`, so
`/\evil.com` is protocol-relative too, and they strip tabs and newlines before parsing (`/<TAB>/evil.com`).

```delphi
function IsLocalRedirect(const AUrl: string): Boolean;
var
  C: Char;
begin
  // one leading '/', not '//'; no backslash and no control character anywhere
  Result := AUrl.StartsWith('/') and not AUrl.StartsWith('//');
  if Result then
    for C in AUrl do
      if (C < ' ') or (C = #127) or (C = '\') then
        Exit(False);
end;

lNext := Context.Request.QueryStringParam('returnUrl');
if not IsLocalRedirect(lNext) then
  lNext := '/';
Result := RedirectResponse(lNext);
```

---

## 8. Security headers

`SecurityHeadersHTTPFilter` (HTTP filter, `MVCFramework.Filters`) and the deprecated `TMVCSecurityHeadersMiddleware` set **only two**
headers: `X-Content-Type-Options: nosniff` and `X-XSS-Protection: 0` — the value OWASP recommends: the old
browser XSS filter could itself be abused, and CSP is the real control.

**It does not set CSP, HSTS or X-Frame-Options.** For a browser-facing app, add them — CSP is the one that
actually stops XSS:

```delphi
lEngine.Use(
  procedure(const AContext: TWebContext; const ANext: TMVCHTTPFilterNext)
  begin
    AContext.Response.SetCustomHeader('Content-Security-Policy',
      'default-src ''self''; script-src ''self''; object-src ''none''; frame-ancestors ''none''');
    AContext.Response.SetCustomHeader('Strict-Transport-Security',
      'max-age=31536000; includeSubDomains');
    AContext.Response.SetCustomHeader('X-Frame-Options', 'DENY');
    AContext.Response.SetCustomHeader('Referrer-Policy', 'no-referrer');
    ANext();
  end);
```

Note the wizard's `baselayout.html` loads Bootstrap and htmx from a CDN — a strict `script-src 'self'` will
block them. Either self-host those assets or add the CDN origin to the policy deliberately.

**CORS:** with origin `*` (the default of `CORSHTTPFilter`) credentials are never sent. The filter has
credentials **off** by default (`AAllowsCredentials = False`); turn them on only for a cookie-authenticated
frontend, and list only origins you control:

```delphi
// comma-separated list of exact origins; the request's Origin is matched against it
AEngine.Use(CORSHTTPFilter('https://app.example.com,https://admin.example.com'));
```

A 3.4 project may use `TMVCCORSMiddleware` (deprecated): there, naming an origin turns credentials **on** by
default — pass `False` when the frontend authenticates with a Bearer header.

Never build that list from the request's own `Origin` header — that turns it back into "every site".

---

## 9. JWT

- **The algorithm is already pinned.** `TJWT` rejects a token whose `alg` differs from the configured signer's
  (so `none`, or HS256 against an RS256 verifier, fails). Do not undo this with a custom signer that accepts
  several algorithms.
- The secret is a **secret**: ≥256 bits of randomness, from the environment, never in the repo, and rotatable.
  A JWT signed with `'change-me'` is a signature by anyone who reads your GitHub.
- **Pass the claims to check.** `JWTAuthenticationBundle` and `JWTCookieAuthenticationBundle` check the expiration by
  default (`ClaimsToCheck` of their options records is `[TJWTCheckableClaim.ExpirationTime]`; `[]` checks none: the
  token never expires). The classic middleware of a 3.4 project — every JWT middleware constructor and
  `UseJWTCookieAuthentication` — default to `[]`: there an expired token is accepted forever. When the claims setup
  sets `NotBefore` and `IssuedAt`, check them too:

  ```delphi
  lOptions.ClaimsToCheck := [TJWTCheckableClaim.ExpirationTime, TJWTCheckableClaim.NotBefore, TJWTCheckableClaim.IssuedAt];
  ```
- Keep expiry short. A JWT cannot be revoked — that is the trade you accepted. If you need revocation, keep a
  server-side session or a token deny-list (`OnAcceptToken` + `OnNewJWTToBlackList` of
  `TMVCJWTAuthenticationOptions`), or short access tokens plus revocable refresh tokens (`RefreshTokenStore` of the
  same options — one global store for the process; with both, a logout whose JSON body carries `refresh_token`
  revokes it too). With a login of your own, `LoginURL := ''` leaves the bundle with the check only (or use
  `RefreshTokenHTTPFilter` of `MVCFramework.JWT.RefreshToken.Filters`), and the login must be **outside the check**:
  a controller action made public by the handler's `OnRequest`, or a Minimal API route outside the group that
  carries the bundle — an engine-wide check covers every Minimal API route (see `dmvcframework`, *Filters*).
- Do not put secrets or PII in the payload: it is **base64, not encryption**. Anyone can read it.
- Validate the signature **before** trusting any claim, including the user id and the roles. The JWT bundles
  do this; your code must not read claims from an unvalidated token.

---

## 10. Secrets and configuration

- **No secret in source.** Not a JWT key, not a DB password, not an API key. `git` remembers forever.
- Use `dotEnv` — the wizard already wires it: `dotEnv.Env('JWT_SECRET', '')`. Keep `.env` out of git
  (`.gitignore`) and ship a `.env.sample` with empty values. See `dmvcframework`, `reference/dotenv.md`.
- Fail loudly on a missing secret in production (`dotEnv.RequireKeys([...])`) instead of falling back to a
  default like `'change-me'` — a default secret is worse than a crash.
- Do not log credentials, tokens, or full request bodies of auth endpoints. `TraceHTTPFilter` (and the
  deprecated `TMVCTraceMiddleware`) logs request bodies: do not enable it in production on routes that carry
  passwords.
- Serve over HTTPS (`IMVCServer` has the certificate properties — see `dmvcframework`,
  `reference/servers.md`). Cookies with `Secure=True` simply will not travel over plain HTTP.

---

## 11. Errors, rate limiting, and the rest

- **Do not leak internals in errors.** A stack trace, a SQL statement or a file path in a 500 is
  reconnaissance. Log the detail server-side; return a generic message. `UseExceptionHandler` renders a clean
  page for browsers and leaves the JSON error body for APIs.
- **JSON-RPC:** install an exception handler on `PublishObject` — see `dmvcframework-jsonrpc`.
- **Rate-limit** what can be brute-forced (login, password reset, token endpoints):
  `lEngine.Use(RateLimitHTTPFilter(60, 60))`, or `RateLimitRedisHTTPFilter` when you run more than one instance.
  Register it **before** the authentication bundle: its login answers before every filter registered after it.
- **Hash passwords** with bcrypt/scrypt/Argon2 and a per-user salt. Never MD5/SHA-1, never plain, never a
  reversible cipher. Compare in constant time.
- **Timing-safe comparison** for tokens and API keys — a `=` on strings leaks length and prefix.
- Do not disable TLS certificate validation in the REST client "to make it work".

---

## 12. Checklist before an endpoint ships

- [ ] Is it authenticated? Is it **authorized** — scoped to the caller, in the query?
- [ ] Does the body bind to a DTO, or is every sensitive entity field `[MVCDoNotDeserialize]`?
- [ ] Is every SQL string a constant, with values in the params array?
- [ ] Does any template output use `$` or `{{autoescape false}}` on data that came from a user?
- [ ] Is any filesystem path or outbound URL built from client input?
- [ ] Are uploads size-limited, extension-checked, renamed, and stored outside the web root?
- [ ] Are errors generic to the client and detailed only in the log?
- [ ] Is the endpoint rate-limited if it can be brute-forced?
- [ ] Are the secrets it uses coming from `.env`, not from the source?
