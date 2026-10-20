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
| JWT (controller) | `Context.LoggedUser.UserName` / `.Roles` / `.CustomData` — populated by `TMVCJWTAuthenticationMiddleware` after signature validation |
| Minimal API | the `Authorize` / `RequireRole('admin')` endpoint filters, plus `Ctx.LoggedUser` |
| Web app session | `Context.Session['user']` — only after the session filter/middleware has run |

Role checks belong on the route group or the action, not scattered through the body:

```delphi
// minimal API — the whole group is gated
var lAdmin := lApi.Prefix('/admin').Use(Authorize).Use(RequireRole('admin'));
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
  Result := CreatedResponse('/users/' + User.ID.ToString);
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

Use `$` only for HTML **you** generated. If you must render user-submitted rich text, sanitize it server-side
with an allow-list of tags before it ever reaches `ViewData` — an escape-blacklist will be bypassed.

For JSON APIs, set the content type correctly (`application/json`, which DMVC does) and never build HTML by
string concatenation in Delphi — render a template (see the `dmvcframework-webapp` skill).

Add a Content-Security-Policy (see §8): it is the backstop for the XSS you missed.

---

## 5. CSRF

**Cookie-based auth is vulnerable; `Authorization: Bearer` is not** (the browser does not attach a header
automatically).

DMVCFramework's cookie JWT middleware defaults are already correct — `Secure=True`, `HttpOnly=True`,
`SameSite=Strict`:

```delphi
UseJWTCookieAuthentication(...)
  .SetCookieSecure(True)         // HTTPS only — keep True in production
```

**Do not weaken `SameSite` to `None`** without a hard requirement; `Strict` is what makes the cookie flow
CSRF-safe. If you ever must relax it, add a synchronizer token: DMVCFramework does not ship one, so you
generate a random token, store it in the session, echo it in a hidden form field / `hx-headers`, and compare
on every state-changing request.

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

- `TMVCFormFile.FileName` **is attacker-controlled**. `SaveToFile(APath)` writes wherever you point it —
  it does not sanitize. Generate your own name (a GUID) and keep the client's only as metadata.
- Do not trust `ContentType` either — it is a client-supplied header. Check the actual bytes (magic number)
  when the file type matters.
- Enforce a **size limit** and an **extension allow-list** (never a deny-list).
- Store uploads **outside** the static-files root, or the upload becomes a way to serve executable content.
- `ContentStream` is request-owned — read it, do not free it.

The static-files middleware (`TMVCStaticFilesMiddleware`) does check for directory traversal on the paths it
serves — but it protects *its* document root, not any path you construct yourself.

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

Accept only **relative, single-slash** paths, or an allow-list of known destinations:

```delphi
lNext := Context.Request.QueryStringParam('returnUrl');
if (lNext = '') or (not lNext.StartsWith('/')) or lNext.StartsWith('//') then
  lNext := '/';                       // '//evil.com' is protocol-relative — reject it
Result := RedirectResponse(lNext);
```

---

## 8. Security headers

`SecurityHeaders` (HTTP filter, `MVCFramework.Filters`) sets **only two** headers:
`X-XSS-Protection: 1; mode=block` and `X-Content-Type-Options: nosniff`.

**It does not set CSP, HSTS or X-Frame-Options.** For a browser-facing app, add them — CSP is the one that
actually stops XSS:

```delphi
lEngine.UseHTTPFilter(
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

**CORS is permissive by default:** `TMVCCORSMiddleware.Create` defaults to `Access-Control-Allow-Origin: *`
with credentials allowed. That is fine for a public read-only API and wrong for anything with a session.
Name your origins:

```delphi
AEngine.AddMiddleware(TMVCCORSMiddleware.Create('https://app.example.com'));
```

`*` is not a wildcard for "my frontend" — it is "every site on the internet".

---

## 9. JWT

- **Never** accept the `alg` from the token. Reject `none`; do not let an RS256 verifier be tricked into
  HS256 with the public key as the secret. Pin the algorithm you configured.
- The secret is a **secret**: ≥256 bits of randomness, from the environment, never in the repo, and rotatable.
  A JWT signed with `'change-me'` is a signature by anyone who reads your GitHub.
- Always check `ExpirationTime`, `NotBefore`, `IssuedAt` — that is what the `AClaimsToCheck` set is for:

  ```delphi
  [TJWTCheckableClaim.ExpirationTime, TJWTCheckableClaim.NotBefore, TJWTCheckableClaim.IssuedAt]
  ```
- Keep expiry short. A JWT cannot be revoked — that is the trade you accepted. If you need revocation, keep a
  server-side session or a token deny-list.
- Do not put secrets or PII in the payload: it is **base64, not encryption**. Anyone can read it.
- Validate the signature **before** trusting any claim, including the user id and the roles. The middleware
  does this; your code must not read claims from an unvalidated token.

---

## 10. Secrets and configuration

- **No secret in source.** Not a JWT key, not a DB password, not an API key. `git` remembers forever.
- Use `dotEnv` — the wizard already wires it: `dotEnv.Env('JWT_SECRET', '')`. Keep `.env` out of git
  (`.gitignore`) and ship a `.env.sample` with empty values. See `dmvcframework`, `reference/dotenv.md`.
- Fail loudly on a missing secret in production (`dotEnv.RequireKeys([...])`) instead of falling back to a
  default like `'change-me'` — a default secret is worse than a crash.
- Do not log credentials, tokens, or full request bodies of auth endpoints. `TMVCTraceMiddleware` logs
  request bodies: do not enable it in production on routes that carry passwords.
- Serve over HTTPS (`IMVCServer` has the certificate properties — see `dmvcframework`,
  `reference/servers.md`). Cookies with `Secure=True` simply will not travel over plain HTTP.

---

## 11. Errors, rate limiting, and the rest

- **Do not leak internals in errors.** A stack trace, a SQL statement or a file path in a 500 is
  reconnaissance. Log the detail server-side; return a generic message. `UseExceptionHandler` renders a clean
  page for browsers and leaves the JSON error body for APIs.
- **Rate-limit** what can be brute-forced (login, password reset, token endpoints):
  `lEngine.UseHTTPFilter(RateLimit(60, 60))`, or `RateLimitRedis` when you run more than one instance.
- **Hash passwords** with bcrypt/scrypt/Argon2 and a per-user salt. Never MD5/SHA-1, never plain, never a
  reversible cipher. Compare in constant time.
- **Timing-safe comparison** for tokens and API keys — a `=` on strings leaks length and prefix.
- Do not disable TLS certificate validation in the REST client "to make it work".

---

## 12. Checklist before an endpoint ships

- [ ] Is it authenticated? Is it **authorized** — scoped to the caller, in the query?
- [ ] Does the body bind to a DTO, or is every sensitive entity field `[MVCDoNotDeserialize]`?
- [ ] Is every SQL string a constant, with values in the params array?
- [ ] Does any template output use `$` on data that came from a user?
- [ ] Is any filesystem path or outbound URL built from client input?
- [ ] Are uploads size-limited, extension-checked, renamed, and stored outside the web root?
- [ ] Are errors generic to the client and detailed only in the log?
- [ ] Is the endpoint rate-limited if it can be brute-forced?
- [ ] Are the secrets it uses coming from `.env`, not from the source?
