---
name: dmvcframework
description: Use when writing any server-side feature with DelphiMVCFramework (DMVC) — controllers, routes, REST endpoints, entities, middleware, validation, dependency injection, JWT, SSE, or the server bootstrap. The core skill; the Minimal API, web-app, UI, security and testing skills build on it. Triggers on "DMVCFramework", "DelphiMVCFramework", "create a controller", "add a route", "REST endpoint in Delphi", "TMVCActiveRecord", "ActiveRecord entity", "add middleware", "configure JWT", "MVCRequired", "add validation", "service container", "MVCInject".
---

# DMVCFramework Development Guide

DMVCFramework is a mature Delphi MVC/REST framework.
Official repo: https://github.com/danieleteti/delphimvcframework

---

## When in doubt about an API — verify it, never guess

These skills are verified against a specific DelphiMVCFramework release (see the repository's README). If you
need a signature they do not cover, or you suspect the framework has moved on, **do not invent a name and do
not answer from memory** — a plausible-but-wrong identifier costs the user a compile error and their trust.

Verify, in this order:

1. **Ask the user for the local sources.** They almost certainly have the framework on disk:

   > What is the path to your DelphiMVCFramework checkout? I want to confirm the exact signature in
   > `sources/` rather than guess. If you have the samples too, that helps.

   Then read the unit directly (`sources/MVCFramework*.pas`) and, for usage, the matching sample.

2. **Otherwise, read the official repository** — always current:
   - Sources: https://github.com/danieleteti/delphimvcframework/tree/master/sources
   - Samples: https://github.com/danieleteti/delphimvcframework/tree/master/samples

   Raw file, when you know the unit:
   `https://raw.githubusercontent.com/danieleteti/delphimvcframework/master/sources/<Unit>.pas`

3. **If you cannot verify it, say so.** "I am not certain of this signature — check `MVCFramework.X.pas`" is
   a useful answer. A confidently wrong one is not.

The samples are the best documentation of *usage*: they compile, they run, and they are maintained with the
framework. When a skill and a sample disagree, **the sample wins** — and the skill has a bug worth reporting.

**A signature is half the answer.** `sources/` tells you the arity and the types; a sample tells you the
ownership, who frees what, the order of the calls and the idiom — which is where generated code actually
breaks. Read both before writing a call you have not written before, and prefer the project's own existing
controllers as the third input: they carry the conventions this codebase already follows.

**Record the path instead of asking every session.** Once the user gives you the checkout path and you have
confirmed it exists, offer to write it into the instruction file this agent already reads (`AGENTS.md` in the
project root, or `AGENTS.md` / `GEMINI.md`). Ask first, then keep it in a block of its own:

```markdown
<!-- delphi-local-sources -->
DelphiMVCFramework checkout: C:\DEV\dmvcframework   (sources/ + samples/)
Delphi RTL/VCL source: C:\Program Files (x86)\Embarcadero\Studio\23.0\source   (12 Athens, CompilerVersion 36.0)
<!-- /delphi-local-sources -->
```

Look for that block **before** asking. If a path in it no longer exists, say so and ask again rather than
falling back to memory. The `delphi` skill documents the same block, and the two share it.

---

## Delphi Language Target

**Minimum version:** Delphi 11 Alexandria.

**Use freely — Delphi 11+ syntax:**
```delphi
var x := SomeFunction();                  // inline variable
for var i := 0 to List.Count - 1 do      // for-var with index
for var item in Collection do             // for-var with iterator
```

**Do NOT use — Delphi 13 Florence only:**
```delphi
NameOf(MyField)                           // NameOf operator — not available yet
var x := if Condition then A else B;      // inline if expression — not available yet
```

For the language itself — what compiles on which release, object lifetime and `Free`/interfaces, strings and
encodings, generics, RTTI, threading — load the **`delphi`** skill. It assumes no framework and is the
foundation this one sits on.

---

**REQUIRED REFERENCE — `dmvcframework-security`.** Any endpoint that accepts input from a client (body,
query string, header, cookie, upload, URL) must follow it: access control and IDOR, mass assignment,
SQL injection, XSS, CSRF, path traversal, uploads, security headers, JWT, secrets. Invoke it whenever you
write or review such an endpoint — not only when the user says "security".

---

## STOP — this skill works inside a wizard-generated project

**Never create a DMVCFramework project from scratch. Never write a `.dpr` bootstrap by hand.**
The IDE wizard already produces a correct, compiling, runnable project; your job is to add features to it.

### Step 1 — Detect the project in the current folder

The user is expected to have run the wizard already and to have started the agent **from inside the project
folder**. Look for these files in the working directory (a wizard project has them):

```
*.dpr                 the program: engine creation + server bootstrap
EngineConfigU.pas     controllers + middleware registration   <- where most of your work lands
BootConfigU.pas       dotEnv, LoggerPro, profiler
EntitiesU.pas         entities
ServicesU.pas         DI registrations (if the preset includes DI)
Controllers.*.pas     controllers  (controller-based presets)
RoutesU.pas           lambda routes (Minimal API presets)
```

If they are there: read `*.dpr`, `EngineConfigU.pas` and any existing `Controllers.*.pas` / `RoutesU.pas`
**before writing anything**, and match their conventions.

### Step 2 — If there is no wizard project, stop and say so

Do not scaffold one. Tell the user:

> This skill works on a project generated by the DMVCFramework IDE wizard, and I do not see one in this
> folder. Please create it first:
>
> **Delphi IDE → File → New → Other → Delphi Projects → DelphiMVCFramework → New DMVCFramework Application**
>
> Pick the preset that matches what you are building (RESTful API, Web Application, Minimal API, JSON-RPC,
> Real-Time, Full-Stack). Accept the defaults — the server backend is **Indy Direct**. Compile and run it
> once to confirm it works. Then `cd` into the project folder, start me there, and tell me what to add.

Then wait. Do not generate project files in the meantime.

### Step 2b — Detect the host, and leave it alone

**For a NEW project the default is Indy Direct** — a self-contained executable, no WebModule, no WebBroker.
Never introduce a `TWebModule` into a new standalone server.

**But an existing project may legitimately be WebBroker-hosted** — an ISAPI DLL, an Apache module, or an app
that predates Indy Direct. Read the `.dpr` and tell them apart:

| What you find in the project | Host | What you do |
|------------------------------|------|-------------|
| `TMVCServerFactory.CreateIndyDirect` | Indy Direct | The default. Carry on. |
| `TMVCServerFactory.CreateHttpSys` | HTTP.sys | Carry on. |
| `TWebModule` + `TMVCEngine.CreateForWebBroker` / `TMVCEngine.Create(Self, …)`, or a `.dpr` with `TISAPIApplication` / `Web.ApacheApp` | WebBroker (ISAPI / Apache / WebBroker app) | **Legitimate. Keep it.** |

**Everything in this skill except hosting is identical across all three hosts** — controllers, actions,
routing attributes, `IMVCResponse`, entities, ActiveRecord, validation, DI, middleware, JWT, serialization.
A controller does not know what is listening on the socket.

So on a WebBroker project: add controllers and configure the engine exactly as documented, inside the
existing `ConfigureEngine` / `WebModuleCreate`. **Do not migrate it to Indy Direct, do not rewrite the
`.dpr`, do not warn the user that they "should" switch** — an ISAPI or Apache deployment is a deliberate
requirement, not a mistake. Only touch the host if the user explicitly asks.

Details of all hosts: `reference/servers.md`.

### Step 3 — Work within the wizard structure

The wizard generates a specific layout. Always follow it:

| Task | Where to act |
|------|-------------|
| Add a new controller | Create `Controllers.<Resource>U.pas`; add `uses` + `AEngine.AddController(...)` in `EngineConfigU.pas` |
| Add an entity | Extend `EntitiesU.pas` or create `Entities.<Domain>U.pas` beside it |
| Add middleware | Add `AEngine.AddMiddleware(...)` inside `ConfigureEngine` in `EngineConfigU.pas` |
| Change engine config | Edit the `TMVCEngine.Create` config lambda in the `.dpr` |
| Change the port / settings | Edit `bin/.env` — the wizard reads it via `dotEnv`; do not hard-code |

### Wizard-generated file overview

```
MyProject/
├── MyProject.dpr              # Boot, RegisterServices, then RunServer — Indy Direct, no WebModule
├── BootConfigU.pas            # dotEnv + LoggerPro + profiler setup (do not modify)
├── EngineConfigU.pas          # ConfigureEngine() — add controllers/middleware HERE
├── EntitiesU.pas              # Starter entity — extend as needed
├── ServicesU.pas              # DI registrations (presets with a service container)
├── Controllers.HomeU.pas      # Home controller — keep as reference
├── Controllers.PeopleU.pas    # CRUD example — follow this pattern
└── bin/.env                   # port, connection string, secrets — edit here, not in code
```

The `.dpr` already contains the correct Indy Direct bootstrap. Leave it alone unless the task is
specifically about hosting.

### EngineConfigU.pas — where to add controllers

```delphi
procedure ConfigureEngine(AEngine: TMVCEngine);
begin
  // Controllers
  AEngine.AddController(THomeController);
  AEngine.AddController(TPeopleController);
  AEngine.AddController(TProductsController);  // ← add new controllers here
  // Controllers - END

  // Middleware
  AEngine.AddMiddleware(TMVCCORSMiddleware.Create);  // ← add middleware here
  // Middleware - END
end;
```

---

## Reference files — read the one you need

This SKILL.md is the working core. Detailed material lives in `reference/`; open a file only when the
task needs it.

| File | Read it when the task involves |
|------|-------------------------------|
| `reference/servers.md` | Bootstrapping the engine: Indy Direct (default), HTTP.sys, WebBroker/ISAPI/Apache, `IMVCServer`, FireDAC connection setup |
| `reference/dotenv.md` | Configuration: the `Boot`/`dotEnvConfigure` pattern, profiles, precedence, `.env` syntax and expressions, the keys wizard projects use, secrets |
| `reference/activerecord.md` | Entities and the ORM: `TMVCActiveRecord`, mapping attributes, CRUD, RQL, named queries, hooks, soft delete, audit columns, optimistic locking, master-detail, the connection middleware, and the automatic REST CRUD controller |
| `reference/di-and-repository.md` | The service container (`DefaultMVCServiceContainer`, `[MVCInject]`) and `IMVCRepository<T>` |
| `reference/validation.md` | Validator attributes, when validation fires, `OnValidate`, storage validation, the 422 error body |
| `reference/sse.md` | Server-Sent Events: `TMVCSSEController`, `SSEBroker` |

Related skills: **`dmvcframework-minimal-api`** (lambda routes, no controller class) ·
**`dmvcframework-webapp`** (TemplatePro views + HTMX) · **`dmvcframework-testing`** (DUnitX integration tests).

---

## Core Principles

- **Attribute-driven routing** — routes live on controller methods via attributes, no config files
- **Functional actions** — controller actions are **always `function`**, never `procedure + Render(...)`. Return the data or an `IMVCResponse`; the framework serializes it automatically. **Exception:** SSE/streaming actions that write directly to the socket may use `procedure`.
- **Result ownership** — the framework frees the object you return. `ToFree<T>` is only for *extra* objects allocated in the action that you do NOT return (never `Result := ToFree(...)` → double free)
- **ActiveRecord ORM** — entities inherit `TMVCActiveRecord`, fields mapped with attributes
- **Middleware chain** — cross-cutting concerns (CORS, JWT, auth, compression) added in `ConfigureEngine`
- **Auto-serialization** — returning a `TObject` or `TObjectList<T>` from an action produces JSON automatically

---

## Controller Scaffolding — Functional Actions

**RULE: every controller action is a `function`. Never write `procedure SomeAction; begin Render(...); end;`.**
Return data objects or `IMVCResponse`. The framework detects the return type via RTTI and serializes automatically.
The framework frees the returned object after serializing it. `ToFree<T>(obj)` is only for extra
objects you allocate and do *not* return (see *Memory Management in Functional Actions*) — never wrap the result in it.

**File: `Controllers.MyResource.pas`**

```delphi
unit Controllers.MyResource;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Serializer.Commons,
  System.Generics.Collections,
  Entities.MyResource;

type
  [MVCDoc('CRUD API for MyResource')]
  [MVCPath('/myresources')]
  TMyResourceController = class(TMVCController)
  public
    [MVCDoc('Returns all MyResource records')]
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    function GetAll: TObjectList<TMyResource>;

    [MVCDoc('Returns MyResource by ID')]
    [MVCPath('/($id)')]
    [MVCHTTPMethod([httpGET])]
    function GetByID(id: Integer): TMyResource;

    [MVCDoc('Creates a new MyResource; returns 201 + Location')]
    [MVCPath]
    [MVCHTTPMethod([httpPOST])]
    function Create(const [MVCFromBody] Resource: TMyResource): IMVCResponse;

    [MVCDoc('Fully replaces a MyResource; returns 200 + updated record')]
    [MVCPath('/($id)')]
    [MVCHTTPMethod([httpPUT])]
    function Update(const id: Integer; const [MVCFromBody] Resource: TMyResource): IMVCResponse;

    [MVCDoc('Partially updates a MyResource; returns 204')]
    [MVCPath('/($id)')]
    [MVCHTTPMethod([httpPATCH])]
    function PartialUpdate(const id: Integer; const [MVCFromBody] Resource: TMyResource): IMVCResponse;

    [MVCDoc('Deletes a MyResource; returns 204')]
    [MVCPath('/($id)')]
    [MVCHTTPMethod([httpDELETE])]
    function Delete(id: Integer): IMVCResponse;
  end;

implementation

uses
  MVCFramework.ActiveRecord,
  System.SysUtils;

function TMyResourceController.GetAll: TObjectList<TMyResource>;
begin
  Result := TMVCActiveRecord.All<TMyResource>;      // framework frees it
end;

function TMyResourceController.GetByID(id: Integer): TMyResource;
begin
  Result := TMVCActiveRecord.GetByPk<TMyResource>(id);
end;

function TMyResourceController.Create(const [MVCFromBody] Resource: TMyResource): IMVCResponse;
begin
  Resource.Insert;
  Result := CreatedResponse('/myresources/' + Resource.ID.ToString);
end;

function TMyResourceController.Update(const id: Integer;
  const [MVCFromBody] Resource: TMyResource): IMVCResponse;
var
  lExisting: TMyResource;
begin
  lExisting := TMVCActiveRecord.GetByPk<TMyResource>(id);
  lExisting.Assign(Resource);
  lExisting.Update;
  Result := OKResponse(lExisting);                  // response owns and frees it
end;

function TMyResourceController.PartialUpdate(const id: Integer;
  const [MVCFromBody] Resource: TMyResource): IMVCResponse;
var
  lExisting: TMyResource;
begin
  lExisting := ToFree(TMVCActiveRecord.GetByPk<TMyResource>(id));  // not returned → ToFree
  if Resource.Name <> '' then lExisting.Name := Resource.Name;
  // apply other optional fields...
  lExisting.Update;
  Result := NoContentResponse;
end;

function TMyResourceController.Delete(id: Integer): IMVCResponse;
begin
  begin
    var Ctx := TMVCActiveRecord.UseTransactionContext;   // commits on exit, rolls back on exception
    ToFree(TMVCActiveRecord.GetByPk<TMyResource>(id)).Delete;
  end;
  Result := NoContentResponse;
end;

end.
```

### When `procedure` is allowed

`procedure` (no return value) is acceptable only when the action does not produce a serialized body:
- **Redirects** in web controllers — `Redirect('/people');`
- **SSE** — but do not hand-roll it: inherit `TMVCSSEController` (see `reference/sse.md`).

Everything else **must** be a `function`.

---

## Route Attribute Reference

| Attribute | Purpose | Example |
|-----------|---------|---------|
| `[MVCPath]` | Map to controller base path | `[MVCPath]` → `/myresources` |
| `[MVCPath('/($id)')]` | URL parameter | `/myresources/42` |
| `[MVCPath('/search')]` | Sub-path | `/myresources/search` |
| `[MVCHTTPMethod([httpGET])]` | Allowed HTTP methods | `httpGET, httpPOST, httpPUT, httpDELETE, httpPATCH` |
| `[MVCProduces(TMVCMediaType.APPLICATION_JSON)]` | Response content type | optional — JSON is default |
| `[MVCConsumes(TMVCMediaType.APPLICATION_JSON)]` | Expected request type | optional |
| `[MVCDoc('description')]` | Swagger/OpenAPI doc string | any string |
| `[MVCRequiresAuthentication]` | Requires authenticated user | on method or class |
| `[MVCRequiresRole('admin')]` | Requires a role. **Not in `MVCFramework`** — it is `MVCRequiresRoleAttribute` in `MVCFramework.Middleware.Authentication.RoleBasedAuthHandler`, and it is enforced **only** by that handler. Add the unit and the handler, or it silently does nothing | on method or class |

---

## Parameter Injection Attributes

```delphi
// URL segment: /orders/($id) → mapped to 'id' parameter
function GetOrder(id: Integer): TOrder;

// Query string: /orders?status=open
function Search(const [MVCFromQueryString('status', '')] Status: string): TObjectList<TOrder>;

// Request body (auto-deserialized from JSON)
function Create(const [MVCFromBody] Order: TOrder): IMVCResponse;

// Collection from body
function BulkCreate(const [MVCFromBody] Orders: TObjectList<TOrder>): IMVCResponse;

// HTTP header
function WithHeader(const [MVCFromHeader('X-Tenant-ID')] TenantID: string): IMVCResponse;

// Cookie
function WithCookie(const [MVCFromCookie('session')] SessionID: string): IMVCResponse;
```

---

## IMVCResponse Factory Methods

Use these to return responses with specific HTTP status codes from functional actions.
All methods are inherited from `TMVCRenderer` (available in any controller).

```delphi
// 200 OK
Result := OKResponse(AnObject);           // body = serialized object
Result := OKResponse(ObjectDict()         // body = {"data": [...]}
  .Add('data', AList));
Result := OKResponse('message text');     // body = plain message
Result := OKResponse;                     // 200 with empty body

// 201 Created
Result := CreatedResponse('/resources/' + id.ToString);               // no body
Result := CreatedResponse('/resources/' + id.ToString, AnObject);     // with body
Result := CreatedResponse('/resources/' + id.ToString, 'Created OK'); // with message

// 202 Accepted
Result := AcceptedResponse('/jobs/' + jobId);

// 204 No Content
Result := NoContentResponse;

// 3xx Redirect
Result := RedirectResponse('/new-url');
Result := RedirectResponse('/new-url', {Permanent=}True);

// 304 Not Modified
Result := NotModifiedResponse;

// 400 Bad Request
Result := BadRequestResponse('Validation failed');
Result := BadRequestResponse(ErrorObject);

// 401 Unauthorized
Result := UnauthorizedResponse;
Result := UnauthorizedResponse('Token expired');

// 403 Forbidden
Result := ForbiddenResponse;
Result := ForbiddenResponse('Insufficient permissions');

// 404 Not Found
Result := NotFoundResponse;
Result := NotFoundResponse('Resource not found');

// 409 Conflict
Result := ConflictResponse;

// 422 Unprocessable Content
Result := UnprocessableContentResponse('Field email is invalid');
Result := UnprocessableContentResponse(ErrorObject);

// 500 Internal Server Error
Result := InternalServerErrorResponse;
Result := InternalServerErrorResponse('Unexpected failure');

// Custom status code
Result := StatusResponse(418, 'I am a teapot');

// Builder pattern for full control
Result := MVCResponseBuilder
  .StatusCode(202)
  .Header('X-Job-ID', jobId)
  .Body(AnObject)
  .Build;
```

**Error exceptions** (alternative — framework catches and serializes automatically):

```delphi
raise EMVCException.Create(HTTP_STATUS.NotFound, 'Resource not found');
raise EMVCException.Create(HTTP_STATUS.BadRequest, 'Invalid input');
raise EMVCException.Create(HTTP_STATUS.Conflict, 'Already exists');
raise EMVCException.CreateFmt(HTTP_STATUS.UnprocessableEntity, 'Field %s invalid', ['email']);
```

---

## Memory Management in Functional Actions

**The framework already frees the object you return.** `MVCFramework.pas` renders the result and then
calls `lResponseObject.Free` on it. You do not need — and must not add — any ownership call for it.

`ToFree<T>(obj)` exists for the **other** objects: intermediates you allocate inside the action and do
**not** return. It registers them in an owning free-list emptied after the action, so you can skip `try/finally`.

**Never `Result := ToFree(...)`** — the object would be freed by the free-list *and* as the response
object: **double free**.

```delphi
// CORRECT — just return it. No ToFree.
function GetAll: TObjectList<TMyResource>;
begin
  Result := TMVCActiveRecord.All<TMyResource>;
end;

function GetByID(id: Integer): TMyResource;
begin
  Result := TMVCActiveRecord.GetByPk<TMyResource>(id);
end;

// CORRECT — ToFree for an intermediate that is NOT the result
function GetSingleDataSet: TDataSet;
begin
  var lDM := ToFree<TdmMain>(TdmMain.Create(nil));  // the datamodule is not returned
  lDM.dsPeople.Open;
  Result := lDM.dsPeople;                            // the dataset it owns is
end;

// [MVCFromBody] parameter: framework owns it; safe to return it directly
function Create(const [MVCFromBody] Resource: TMyResource): IMVCResponse;
begin
  Resource.Insert;
  Result := CreatedResponse('/myresources/' + Resource.ID.ToString);
end;
```

**Rules:**
- The **result** is freed by the framework — never `ToFree` it
- `ToFree` only for objects allocated inside the action that you do **not** return
- **Do not** `ToFree` a `[MVCFromBody]` parameter — the framework owns it (and detects the alias if you return it, freeing it once)
- Nothing to free besides the result? Then you do not need `ToFree` at all — most actions don't

---

## Request Context

```delphi
// Available inside any action via the inherited Context property:
Context.Request.HTTPMethod                    // TMVCHTTPMethodType
Context.Request.Body                          // raw body string
Context.Request.BodyAs<TMyClass>              // deserialize body to typed object
Context.Request.QueryStringParam('name')      // query string value
Context.Request.Headers['Authorization']      // request header
Context.Request.Cookie('session')             // cookie value
Context.Request.PathInfo                      // URL path
Context.Request.ClientIP                      // remote IP

Context.Response.StatusCode := 200;
Context.Response.SetCustomHeader('X-Header', 'value');

Context.Session['key'] := 'value';            // session read/write
Context.LoggedUser.UserName                   // authenticated user (requires auth middleware)
Context.Data['mykey'] := 'a string';          // per-request storage; TMVCStringDictionary — STRINGS ONLY, no objects
```

---

## Lifecycle Hooks

Override in your controller:

```delphi
procedure OnBeforeAction(AContext: TWebContext; const AActionName: string;
  var AHandled: Boolean); override;
procedure OnAfterAction(AContext: TWebContext; const AActionName: string); override;
procedure OnException(const AContext: TWebContext; const AException: Exception;
  var AHandled: Boolean); override;
procedure MVCControllerAfterCreate; override;
procedure MVCControllerBeforeDestroy; override;
```

`AHandled := True` in `OnBeforeAction` short-circuits the action (auth guards, pre-flight checks, caching).

ActiveRecord lifecycle (override in entity classes):

```delphi
procedure OnBeforeInsertOrUpdate; override;  // validation, computed fields
procedure OnAfterLoad; override;             // load related records
procedure OnAfterInsertOrUpdate; override;   // cascade saves
procedure OnBeforeDelete; override;          // cascade deletes
procedure OnValidation(const Action: TMVCEntityAction); override; // validation
```

---

## Middleware Configuration

Add inside `ConfigureEngine` (in a WebBroker project, that is still where it goes — it is called from
`WebModuleCreate`). Order matters:

```delphi
// FireDAC connection pool for ActiveRecord (required when using ORM)
FEngine.AddMiddleware(TMVCActiveRecordMiddleware.Create('MyConnDef'));

// CORS — must come BEFORE JWT so OPTIONS preflight bypasses auth
FEngine.AddMiddleware(TMVCCORSMiddleware.Create);

// Gzip/deflate compression (threshold in bytes)
FEngine.AddMiddleware(TMVCCompressionMiddleware.Create(256));

// JWT authentication — AConfigClaims is MANDATORY (2nd argument)
FEngine.AddMiddleware(TMVCJWTAuthenticationMiddleware.Create(
  TMyAuthHandler.Create,
  procedure(const JWT: TJWT)                    // TJWTClaimsSetup
  begin
    JWT.Claims.Issuer := 'MyApp';
    JWT.Claims.ExpirationTime := Now + OneHour;
    JWT.Claims.NotBefore := Now - OneMinute * 5;
    JWT.Claims.IssuedAt := Now;
  end,
  dotEnv.Env('JWT_SECRET', ''),                 // never hard-code the secret
  '/auth/login',                                // endpoint that issues tokens
  [TJWTCheckableClaim.ExpirationTime, TJWTCheckableClaim.NotBefore, TJWTCheckableClaim.IssuedAt],
  300                                           // leeway, seconds
));

// Basic authentication
FEngine.AddMiddleware(TMVCBasicAuthenticationMiddleware.Create(TMyAuthHandler.Create));

// Serve static files
FEngine.AddMiddleware(TMVCStaticFilesMiddleware.Create('/app', '.\www'));

// ETag caching
FEngine.AddMiddleware(TMVCETagMiddleware.Create);
```

---

## JWT Authentication Pattern

**Auth handler:**

```delphi
type
  TMyAuthHandler = class(TInterfacedObject, IMVCAuthenticationHandler)
  public
    procedure OnRequest(const AContext: TWebContext;
      const AControllerQualifiedClassName, AActionName: string;
      var AAuthenticationRequired: Boolean);
    procedure OnAuthentication(const AContext: TWebContext;
      const AUserName, APassword: string;
      AUserRoles: TList<string>; var AIsValid: Boolean;
      const ASessionData: TDictionary<string, string>);
    procedure OnAuthorization(const AContext: TWebContext; AUserRoles: TList<string>;
      const AControllerQualifiedClassName, AActionName: string;
      var AIsAuthorized: Boolean);
  end;

procedure TMyAuthHandler.OnRequest(const AContext: TWebContext;
  const AControllerQualifiedClassName, AActionName: string;
  var AAuthenticationRequired: Boolean);
begin
  AAuthenticationRequired := True; // set False for specific public actions
end;

procedure TMyAuthHandler.OnAuthentication(const AContext: TWebContext;
  const AUserName, APassword: string;
  AUserRoles: TList<string>; var AIsValid: Boolean;
  const ASessionData: TDictionary<string, string>);
begin
  AIsValid := (AUserName = 'admin') and (APassword = 'secret');
  if AIsValid then
    AUserRoles.Add('admin');
end;

procedure TMyAuthHandler.OnAuthorization(const AContext: TWebContext;
  AUserRoles: TList<string>;
  const AControllerQualifiedClassName, AActionName: string;
  var AIsAuthorized: Boolean);
begin
  AIsAuthorized := AUserRoles.Contains('admin');
end;
```

**Attribute-level auth:**

```delphi
[MVCRequiresAuthentication]     // any authenticated user — MVCFramework

// [MVCRequiresRole] is NOT part of MVCFramework. It comes from
// MVCFramework.Middleware.Authentication.RoleBasedAuthHandler and is only enforced
// when that handler is installed. Without it the attribute compiles and does nothing.
[MVCRequiresRole('admin')]
```

---

## HTTP_STATUS Constants

```delphi
HTTP_STATUS.OK                  // 200
HTTP_STATUS.Created             // 201
HTTP_STATUS.Accepted            // 202
HTTP_STATUS.NoContent           // 204
HTTP_STATUS.BadRequest          // 400
HTTP_STATUS.Unauthorized        // 401
HTTP_STATUS.Forbidden           // 403
HTTP_STATUS.NotFound            // 404
HTTP_STATUS.MethodNotAllowed    // 405
HTTP_STATUS.Conflict            // 409
HTTP_STATUS.UnprocessableEntity // 422
HTTP_STATUS.InternalServerError // 500
```

---

## Scaffolding Workflow

When the user asks to scaffold a resource (e.g., "create a Products controller"):

### Step 1 — Entity (if DB-backed)
Generate `Entities.Products.pas` with `TProduct` inheriting `TMVCActiveRecord`.
- Use `[MVCNameCase(ncCamelCase)]`, `[MVCTable('...')]`
- Map all fields with `[MVCTableField(...)]` — always `foPrimaryKey, foAutoGenerated` on the PK

### Step 2 — DTO (if needed for validation / PATCH / complex input)
Generate `TProductIn` as a `TMVCValidatable` subclass with validator attributes and `NullableXxx` fields.

### Step 3 — Controller
Generate `Controllers.Products.pas` with `TProductsController`.
- **All actions are `function`** returning either a data type or `IMVCResponse`
- Include full CRUD: `GetAll`, `GetByID`, `Create`, `Update`, `PartialUpdate`, `Delete`
- Add `[MVCDoc(...)]` on every action
- Wrap mutations in transactions (`UseTransactionContext`); return the object, don't `ToFree` it

### Step 4 — Register the controller
In `EngineConfigU.pas`: add `uses Controllers.Products;` and
`AEngine.AddController(TProductsController);` inside `ConfigureEngine`.

### Step 5 — Verify naming
- Controller class: `T[Resource]Controller` (plural resource name)
- Entity class: `T[Entity]` (singular)
- Unit names: `Controllers.[Resource]`, `Entities.[Entity]`

---

## Common Pitfalls

- **Never `procedure + Render(...)`** — use `function` returning data or `IMVCResponse` factory methods
- **Double free** — the framework frees the returned object. `Result := ToFree(x)` and `OKResponse(ToFree(x))` free it twice. `ToFree` is for objects you do NOT return
- **`[MVCOwned]`** — marks a child list for lifecycle management by the parent entity; do not free manually
- **Nullable fields** — `NullableInt64`, `NullableString`, etc. from `MVCFramework.Nullables`; always check `.HasValue` before `.Value`
- **Transactions** — `TMVCActiveRecordMiddleware` opens a connection per request but does NOT auto-commit; wrap multi-step mutations in `StartTransaction/Commit/Rollback`
- **CORS order** — add `TMVCCORSMiddleware` BEFORE JWT/Basic so `OPTIONS` preflight is served without auth
- **Route conflicts** — two actions on the same path must differ only in `[MVCHTTPMethod]`
- **SSE** — never hand-roll the stream; inherit `TMVCSSEController` and override its hooks (`reference/sse.md`)
- **Validation trigger** — any class with ≥1 validator attribute is auto-validated on `[MVCFromBody]`; `TMVCValidatable` is needed only for `OnValidate`. Suppress with `[MVCFromBody(bvDoNotValidate)]`
- **`OnValidate` error keys are stored verbatim** — nothing converts them to the serialized name. Whatever string you pass to `AErrors.Add` is what the client sees; be consistent (the samples use the Delphi property name, e.g. `'EndDate'`)
- **Soft delete — use `NullableTDateTime`** — the `[MVCSoftDeleted]` filter relies on `IS NULL`; a plain `TDateTime` defaults to `1899-12-30`, which would never be NULL
- **`[MVCChangeTracking]` overhead** — allocates a CRC32 snapshot per tracked field; only apply to entities that are frequently partially updated
- **`UseTransactionContext` scope** — declare with `var Ctx :=` at the top of the block; do NOT assign it to another variable (raises `EMVCActiveRecordTransactionContext`)
- **Partition filter and `DeleteAll`** — `DeleteAll(TPartitionedClass)` respects the partition and only deletes rows matching the partition value

---

## Key Units Reference

| Unit | Purpose |
|------|---------|
| `MVCFramework` | Core: `TMVCController`, `TMVCEngine`, attributes, `IMVCResponse` |
| `MVCFramework.Commons` | `TMVCMediaType`, `TMVCHTTPMethodType`, `HTTP_STATUS` |
| `MVCFramework.ActiveRecord` | `TMVCActiveRecord` and its class-level CRUD; also `ActiveRecordConnectionsRegistry`, `TMVCActiveRecordBackEnd`, `loIgnoreNotExistentFields`, `TMVCActiveRecordList` |
| `MVCFramework.Nullables` | `NullableInt32/64/String/Boolean/TDate/TDateTime/Currency` |
| `MVCFramework.Serializer.Commons` | `[MVCNameCase]`, `[MVCNameAs]`, `[MVCDoNotSerialize]`, etc. |
| `MVCFramework.Validation` | `TMVCValidatable`, `EMVCValidationException`, `EMVCStorageValidationException` |
| `MVCFramework.Validators` | All validator attributes (`MVCRequired`, `MVCEmail`, `MVCRange`, …) |
| `MVCFramework.ValidationEngine` | `TMVCValidationEngine` for manual validation |
| `MVCFramework.Middleware.CORS` | `TMVCCORSMiddleware` |
| `MVCFramework.Middleware.Compression` | `TMVCCompressionMiddleware` |
| `MVCFramework.Middleware.ActiveRecord` | `TMVCActiveRecordMiddleware` |
| `MVCFramework.Middleware.JWT` | `TMVCJWTAuthenticationMiddleware` |
| `MVCFramework.DataSet.Utils` | Dataset → JSON helpers |
| `MVCFramework.Container` | `IMVCServiceContainer`, DI registration |
| `MVCFramework.SQLGenerators` | SQL generators (needed for some SQL customization) |

---

## Sample Reference Projects

All samples are in the official repository at https://github.com/danieleteti/delphimvcframework

| Folder | Use case |
|--------|----------|
| `samples/basicdemo_server/` | Minimal setup, hello world |
| `samples/master_details/` | Full CRUD with master-detail relations |
| `samples/routing/` | All routing patterns and parameter types |
| `samples/jsonwebtokenplain/` | JWT authentication |
| `samples/activerecord_restful_crud/` | ActiveRecord + auto-CRUD with `[MVCEntityActions]` |
| `samples/validation_showcase/` | All validator attributes, cross-field, `OnValidate` |
| `samples/validation_vs_storage_demo/` | DTO vs ActiveRecord two-layer validation pattern |
| `samples/activerecord_showcase/` | Full ActiveRecord feature showcase (all attributes, named queries, soft-delete, change tracking, audit, partitioning) |

---
