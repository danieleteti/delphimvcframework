---
name: dmvcframework-testing
description: Use when writing or fixing tests for a DelphiMVCFramework API — integration tests that drive real HTTP against an in-process server, or unit tests around controllers. Covers the DUnitX + in-process IMVCServer + IMVCRESTClient stack, CRUD/auth/authorization test patterns, database fixtures and the console runner. Triggers on "write tests", "integration test", "test the endpoint", "test the controller", "DUnitX", "TMVCRESTClient", "IMVCRESTClient", "test REST API", "API test".
---

# DMVCFramework Integration Testing Guide

DMVCFramework is available at https://github.com/danieleteti/delphimvcframework

It ships with a full test stack: **DUnitX** as test runner, an **in-process `IMVCServer`**
(Indy Direct) and **`IMVCRESTClient`** as the fluent HTTP client. No external server needed —
tests start and stop the server in `[SetupFixture]`/`[TeardownFixture]`.

> **`TMVCListener` is deprecated as of 3.5** and will be removed in 4.0
> (`sources/MVCFramework.Server.pas`). It is now a thin wrapper over `IMVCServer`. Do not write new
> tests against it: build a `TMVCEngine`, host it with `TMVCServerFactory.CreateIndyDirect`, and you
> need no WebModule at all.

**Verify signatures against real code, not memory** — the DUnitX and DMVCFramework units on disk, plus one
existing test as the call site. When you do not know where those sources are, ask the user for the path and
record it as the `dmvcframework` skill describes (the `<!-- delphi-local-sources -->` block in `CLAUDE.md` /
`AGENTS.md`), so the question is asked once per project and not once per session.

Reference test files in the official repository:
- `unittests/general/TestClient/LiveServerTestU.pas`
- `unittests/general/TestClient/ActiveRecordControllerTestU.pas`
- `unittests/general/RESTClient/MVCFramework.Tests.RESTClient.pas`

---

**REQUIRED REFERENCE — `dmvcframework-security`.** Any endpoint that accepts input from a client (body,
query string, header, cookie, upload, URL) must follow it: access control and IDOR, mass assignment,
SQL injection, XSS, CSRF, path traversal, uploads, security headers, JWT, secrets. Invoke it whenever you
write or review such an endpoint — not only when the user says "security".

---

## When in doubt about an API — verify it, never guess

Never invent an identifier or answer from memory. If you need a signature this skill does not cover, ask the
user for the path to their **DelphiMVCFramework checkout** and read `sources/` (and the matching `samples/`
project); failing that, read the official repository —
[sources](https://github.com/danieleteti/delphimvcframework/tree/master/sources) ·
[samples](https://github.com/danieleteti/delphimvcframework/tree/master/samples).
If you still cannot verify it, **say so**. Where a skill and a sample disagree, **the sample wins**.

---

## 0. STOP — tests go alongside a wizard project

This skill tests an **existing** DMVCFramework application. It does not create one.

The user is expected to have a wizard-generated project and to have started the agent **from inside its
folder** (look for `*.dpr` + `EngineConfigU.pas` + `Controllers.*.pas` or `RoutesU.pas`).

- **Read the controllers/routes under test first** — the test project reuses the very same controller and
  engine-configuration units (filters included), so the tests exercise the real thing, not a copy.
- **If there is no project to test**, stop and tell the user to create one with the IDE wizard
  (**File → New → Other → Delphi Projects → DelphiMVCFramework**), accepting the defaults — the server
  backend is **Indy Direct** — then `cd` into the folder and start you there.

The test project itself is a *new, separate* DUnitX project you may create — that is the one thing you do
scaffold here. It hosts the app's engine in-process; it never redefines the app.

---

## 1. Test Project Structure (recommended)

```
MyProjectTests/
├── MyProjectTests.dpr          # DUnitX runner (console or GUI)
├── MyProjectTests.dproj
├── Tests.Server.pas             # Builds the engine under test (no WebModule)
├── Tests.MyResource.pas         # Integration tests per controller/resource
├── Tests.Auth.pas               # Authentication tests
└── Tests.Base.pas               # Base test class (shared setup)
```

---

## 2. Test server (in-process, no WebModule)

Build the engine directly and host it with the Indy Direct backend. There is no WebModule and no `.dfm`.

**This holds even when the application ships as an ISAPI DLL or an Apache module.** The host is not what you
are testing — the engine, the controllers and the filters are. Call the app's own
`ConfigureEngine(AEngine)` from the test, host it in-process with Indy Direct, and you exercise exactly the
same stack that WebBroker would serve in production. Do not try to spin up ISAPI/Apache from a test.
Do **not** re-register controllers and filters by hand in the test: a hand-picked subset is a different
application (leave out the JWT filters and every authentication test in §6 fails).

`ConfigureEngine` reads `dotEnv` (`JWT_SECRET` has no fallback, by design), so the test executable needs a
`.env` in its own folder with its **own throwaway `JWT_SECRET`** — never a copy of the app's secret — kept out
of version control. `Boot` from the wizard's `BootConfigU` runs **once**, in the test `.dpr` (§10): a second
call raises `EMVCDotEnv 'dotEnv already initialized'`, so it cannot live in a per-fixture setup.
A **Minimal API** project registers its routes outside `ConfigureEngine`: also call
`ConfigureRoutes(AEngine.Root)` (unit `RoutesU`, plus `MVCFramework.MinimalAPI` for `Root`), as the wizard's
`.dpr` does.

**File: `Tests.Server.pas`**

```delphi
unit Tests.Server;

interface

uses
  MVCFramework, MVCFramework.Server.Intf;

function StartTestServer(APort: Integer; out AEngine: TMVCEngine): IMVCServer;

implementation

uses
  System.SysUtils,
  MVCFramework.Commons,
  MVCFramework.Server.Factory,
  EngineConfigU;                 // the app's own unit, added to the test project

function StartTestServer(APort: Integer; out AEngine: TMVCEngine): IMVCServer;
begin
  AEngine := TMVCEngine.Create(
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.DefaultContentType] := TMVCMediaType.APPLICATION_JSON;
    end);
  ConfigureEngine(AEngine);      // the same controllers and filters the app serves

  Result := TMVCServerFactory.CreateIndyDirect(AEngine);
  Result.Listen(APort, '127.0.0.1');  // loopback: the default 0.0.0.0 triggers the Windows firewall prompt
end;

end.
```

`IMVCServer` exposes `Listen(APort, AHost)`, `Stop`, `IsRunning`. Stop it in `[TeardownFixture]` and free the
engine afterwards — the server does not own it.

---

## 3. Base test class

Every fixture shares the same shape: start the engine once, hand each test a fresh client.

```delphi
unit Tests.Base;

interface

uses
  DUnitX.TestFramework,
  MVCFramework,                  // TMVCEngine
  MVCFramework.Server.Intf,      // IMVCServer
  MVCFramework.RESTClient.Intf,  // IMVCRESTClient, IMVCRESTResponse
  MVCFramework.RESTClient;       // TMVCRESTClient

const
  TEST_HOST = '127.0.0.1';       // the address StartTestServer listens on
  TEST_PORT = 9998;              // keep off the dev server's port

type
  TBaseAPITest = class(TObject)
  protected
    FEngine: TMVCEngine;
    FServer: IMVCServer;
    FClient: IMVCRESTClient;
  public
    [SetupFixture]    procedure SetupFixture; virtual;
    [TeardownFixture] procedure TeardownFixture; virtual;
    [Setup]           procedure Setup; virtual;
  end;

implementation

uses
  System.SysUtils,
  Tests.Server;                  // StartTestServer — see section 2

procedure TBaseAPITest.SetupFixture;
begin
  FServer := StartTestServer(TEST_PORT, FEngine);
end;

procedure TBaseAPITest.TeardownFixture;
begin
  if Assigned(FServer) and FServer.IsRunning then
    FServer.Stop;
  FServer := nil;
  FreeAndNil(FEngine);
end;

procedure TBaseAPITest.Setup;
begin
  // a fresh client per test: no auth token or header leaks between tests
  FClient := TMVCRESTClient.New
    .BaseURL(TEST_HOST, TEST_PORT)
    .ReadTimeout(30000);
end;

end.
```

`IMVCRESTClient` is interface-managed — never free it.

---

## 4. IMVCRESTClient — the API you actually use

Fluent configuration, then a verb. The verb returns an `IMVCRESTResponse`.

```delphi
// configuration (chainable, returns IMVCRESTClient)
.BaseURL(aHost, aPort)  /  .BaseURL(aURL)
.ReadTimeout(ms)
.Accept('application/json')
.AddHeader(aName, aValue)
.AddQueryStringParam(aName, aValue)      // string, Integer, Int64, Double, TGUID, TDateTime, TDate, TTime
.SetBasicAuthorization(aUser, aPassword)
.SetBearerAuthorization(aToken)
.AddBody(aJSONString, aContentType)
.AddBody(aObject, aOwnsObject = True)    // serialized for you
```

```delphi
// verbs — each returns IMVCRESTResponse
.Get(aResource)
.Post(aResource, aBody: string)   /   .Post(aResource, aObject, aOwnsBody = True)
.Put(aResource, ...)    .Patch(aResource, ...)    .Delete(aResource)
```

`IMVCRESTResponse`:

| Member | Use |
|--------|-----|
| `Success: Boolean` | true when `StatusCode` is 2xx |
| `StatusCode: Integer` / `StatusText: string` | assert on the exact code, not merely on `Success` |
| `Content: string` | the raw body |
| `ToJSONObject: TJDOJsonObject` / `ToJSONArray: TJDOJsonArray` | parsed body — **you own the result, free it** |
| `BodyFor(aObject)` / `BodyForListOf(aList, aClass)` | deserialize straight into your entity/DTO |
| `HeaderValue(aName)` / `Headers` | assert on `Location`, `ETag`, … |
| `CookieByName(aName)` | cookie-auth tests |
| `ContentType`, `ContentLength`, `SaveContentToFile` | binary / download endpoints |

---

## 5. CRUD tests

Assert on the **status code and the body**, not merely that the call did not blow up.

```delphi
[TestFixture]
TCustomersTests = class(TBaseAPITest)
private
  function CreateCustomer: string;   // each test makes its own row; returns its Location
public
  [Test] procedure Create_Returns201_And_Location;
  [Test] procedure Get_Returns_The_Created_Customer;
  [Test] procedure Get_Unknown_Id_Returns404;
  [Test] procedure Post_Invalid_Email_Returns422;
  [Test] procedure Delete_Returns204;
end;

procedure TCustomersTests.Create_Returns201_And_Location;
var
  lResp: IMVCRESTResponse;
begin
  lResp := FClient.Post('/api/customers',
    '{"firstName":"Daniele","lastName":"Teti","email":"d.teti@example.com"}');

  Assert.AreEqual(HTTP_STATUS.Created, lResp.StatusCode);
  Assert.IsNotEmpty(lResp.HeaderValue('Location'));
end;

function TCustomersTests.CreateCustomer: string;
var
  lResp: IMVCRESTResponse;
begin
  lResp := FClient.Post('/api/customers',
    '{"firstName":"Daniele","lastName":"Teti","email":"d.teti@example.com"}');
  Assert.AreEqual(HTTP_STATUS.Created, lResp.StatusCode);
  Result := lResp.HeaderValue('Location');
end;

procedure TCustomersTests.Get_Returns_The_Created_Customer;
var
  lResp: IMVCRESTResponse;
  lJSON: TJDOJsonObject;
begin
  lResp := FClient.Get(CreateCustomer);   // never rely on a row another test created
  Assert.AreEqual(HTTP_STATUS.OK, lResp.StatusCode);

  lJSON := lResp.ToJSONObject;          // the caller owns it
  try
    Assert.AreEqual('Daniele', lJSON.S['firstName']);
  finally
    lJSON.Free;
  end;
end;

procedure TCustomersTests.Get_Unknown_Id_Returns404;
begin
  Assert.AreEqual(HTTP_STATUS.NotFound, FClient.Get('/api/customers/999999').StatusCode);
end;

procedure TCustomersTests.Post_Invalid_Email_Returns422;
var
  lResp: IMVCRESTResponse;
begin
  lResp := FClient.Post('/api/customers',
    '{"firstName":"Bob","lastName":"X","email":"not-an-email"}');
  // validation failed => 422, and the action never ran
  Assert.AreEqual(HTTP_STATUS.UnprocessableEntity, lResp.StatusCode);
  // keys are Delphi property names. Controller body: "items" holds "Email: <message>";
  // Minimal API body: ProblemDetails, "errors": {"Email": "<message>"} (after 3.5.0-rc7)
  Assert.Contains(lResp.Content, 'Email');
end;

procedure TCustomersTests.Delete_Returns204;
begin
  Assert.AreEqual(HTTP_STATUS.NoContent, FClient.Delete(CreateCustomer).StatusCode);
end;
```

Deserializing into your own types instead of poking at JSON:

```delphi
var
  lCustomer: TCustomer;
begin
  lCustomer := TCustomer.Create;
  try
    FClient.Get(CreateCustomer).BodyFor(lCustomer);
    Assert.AreEqual('Daniele', lCustomer.FirstName);
  finally
    lCustomer.Free;
  end;
end;
```

---

## 6. Authentication tests

**Test the lock, not only the key.** For every protected resource, assert that it is *refused* without a
token — that is the test that catches the `[MVCRequiresAuthentication]` or the filter you forgot to add.

```delphi
procedure TSecureTests.NoToken_Returns401;
begin
  Assert.AreEqual(HTTP_STATUS.Unauthorized, FClient.Get('/api/admin/stats').StatusCode);
end;

// a helper, not a test: every test that needs a token logs in itself, so no test depends on run order
function TSecureTests.Login(const AUser, APassword: string): string;
var
  lResp: IMVCRESTResponse;
  lJSON: TJDOJsonObject;
begin
  // JWTAuthenticationBundle (the JWT middleware, in a 3.4 project) serves the login URL you configured (e.g. '/login');
  // a separate client, so the Basic header does not stay on FClient
  lResp := TMVCRESTClient.New.BaseURL(TEST_HOST, TEST_PORT)
    .SetBasicAuthorization(AUser, APassword)
    .Post('/login');
  Assert.AreEqual(HTTP_STATUS.OK, lResp.StatusCode);
  lJSON := lResp.ToJSONObject;
  try
    Result := lJSON.S['token'];   // 'access_token' when the bundle has a RefreshTokenStore
  finally
    lJSON.Free;
  end;
end;

procedure TSecureTests.Login_Returns_A_Token;
begin
  Assert.IsNotEmpty(Login('user1', 'user1'));
end;

procedure TSecureTests.ValidToken_Returns200;
begin
  Assert.AreEqual(HTTP_STATUS.OK,
    FClient.SetBearerAuthorization(Login('user1', 'user1')).Get('/api/admin/stats').StatusCode);
end;

procedure TSecureTests.TamperedToken_Returns401;
begin
  Assert.AreEqual(HTTP_STATUS.Unauthorized,
    FClient.SetBearerAuthorization(Login('user1', 'user1') + 'x').Get('/api/admin/stats').StatusCode);
end;
```

Worth a test each as well: an **expired** token, and a valid token for a user **without** the required role.

---

## 7. Authorization tests — the ones people skip

A 200 for the owner proves nothing. Assert that user B **cannot** reach user A's resource — this is the IDOR
test, and it is the bug most likely to be in your API right now (see the `dmvcframework-security` skill).

```delphi
procedure TOrdersTests.User_Cannot_Read_Another_Users_Order;
begin
  // order 1 belongs to user1
  FClient.SetBearerAuthorization(TokenFor('user2'));
  // 404, not 403 — a 403 would confirm the order exists
  Assert.AreEqual(HTTP_STATUS.NotFound, FClient.Get('/api/orders/1').StatusCode);
end;

procedure TOrdersTests.NonAdmin_Cannot_Delete;
begin
  FClient.SetBearerAuthorization(TokenFor('user2'));
  Assert.AreEqual(HTTP_STATUS.Forbidden, FClient.Delete('/api/orders/1').StatusCode);
end;
```

Do this for **every verb** of every resource that has an owner. One properly scoped GET and one unscoped
DELETE is still a breach.

---

## 8. Database-backed tests

For tests that need a real database, configure FireDAC in `[SetupFixture]`
**before** starting the server. The wizard's `ConfigureEngine` already registers `ActiveRecordHTTPFilter`
(`TMVCActiveRecordMiddleware` in a 3.4 project) with the connection named by
`firedac.connection_definition_name` in `.env`: set it to `TestConn` in the test `.env` — without it no
connection is bound to the request and every ActiveRecord call fails.

```delphi
[TestFixture]
TMyResourceDBTests = class(TObject)
private
  FEngine: TMVCEngine;
  FServer: IMVCServer;
  FClient: IMVCRESTClient;
  procedure SeedTestData;
  procedure CleanupTestData;
public
  [SetupFixture]
  procedure SetupFixture;
  [TeardownFixture]
  procedure TeardownFixture;
  [Setup]
  procedure Setup;
  [TearDown]
  procedure TearDown;
  [Test]
  procedure Create_And_Retrieve_RoundTrip;
end;

procedure TMyResourceDBTests.SetupFixture;
var
  LParams: TStringList;
begin
  // 1. Register the FireDAC connection BEFORE starting the server
  LParams := TStringList.Create;
  try
    LParams.Add('Database=myapp_test');  // use a separate test DB
    LParams.Add('Server=localhost');
    LParams.Add('User_Name=testuser');
    LParams.Add('Password=testpass');
    LParams.Add('Pooled=True');
    LParams.Add('POOL_MaximumItems=10');
    FDManager.AddConnectionDef('TestConn', 'PG', LParams);
  finally
    LParams.Free;
  end;

  // 2. Start the in-process server (the engine's ActiveRecordHTTPFilter uses 'TestConn')
  FServer := StartTestServer(9998, FEngine);
end;

procedure TMyResourceDBTests.TeardownFixture;
begin
  if Assigned(FServer) and FServer.IsRunning then
    FServer.Stop;
  FServer := nil;
  FreeAndNil(FEngine);
  CleanupTestData;
  FDManager.CloseConnectionDef('TestConn');
  FDManager.DeleteConnectionDef('TestConn');
end;

procedure TMyResourceDBTests.Setup;
begin
  // seed here, not in SetupFixture: TearDown wipes the data after every test
  SeedTestData;
  FClient := TMVCRESTClient.New.BaseURL('localhost', 9998).ReadTimeout(30000);
end;

procedure TMyResourceDBTests.TearDown;
begin
  // Clean up records created during the test
  CleanupTestData;
end;

procedure TMyResourceDBTests.Create_And_Retrieve_RoundTrip;
var
  lCreateResp, lGetResp: IMVCRESTResponse;
  lJSON: TJDOJsonObject;
  lLocation: string;
  lID: Integer;
begin
  // CREATE
  lCreateResp := FClient.Post('/myresources', '{"name":"DB Test","isActive":true}');
  Assert.AreEqual(HTTP_STATUS.Created, lCreateResp.StatusCode);
  lLocation := lCreateResp.HeaderValue('Location');
  Assert.IsFalse(lLocation.IsEmpty);

  // RETRIEVE
  lGetResp := FClient.Get(lLocation);
  Assert.AreEqual(HTTP_STATUS.OK, lGetResp.StatusCode);
  lJSON := lGetResp.ToJSONObject;
  try
    // NOTE the shape: an action returning the entity directly serializes it FLAT.
    // The "data" envelope only exists when the action returns OKResponse(obj) / IMVCResponse.
    Assert.AreEqual('DB Test', lJSON.S['name']);
    Assert.IsTrue(lJSON.B['isActive']);
    lID := lJSON.I['id'];
    Assert.IsTrue(lID > 0);
  finally
    lJSON.Free;
  end;
end;
```

---

## 9. Parameterized Tests

DUnitX supports `[TestCase]` for data-driven tests:

```delphi
[Test]
[TestCase('admin role', 'admin,200')]
[TestCase('viewer role', 'viewer,200')]
[TestCase('no role', 'anonymous,401')]
procedure GetResource_ByRole_ReturnsExpectedStatus(const Role: string; const ExpectedStatus: Integer);

procedure TMyTests.GetResource_ByRole_ReturnsExpectedStatus(const Role: string; const ExpectedStatus: Integer);
var
  lResp: IMVCRESTResponse;
begin
  if Role <> 'anonymous' then
    FClient.SetBearerAuthorization(GetTokenForRole(Role))
  else
    FClient.ClearAuthorization;

  lResp := FClient.Get('/protected');
  Assert.AreEqual(ExpectedStatus, lResp.StatusCode);
end;
```

```delphi
[Test]
[TestCase('missing name', '/myresources,{"isActive":true},422')]   // a failed validator is 422, not 400
[TestCase('invalid endpoint', '/unknown,{},404')]
procedure InvalidRequests_ReturnErrors(const Path, Body: string; const ExpectedStatus: Integer);

procedure TMyTests.InvalidRequests_ReturnErrors(const Path, Body: string; const ExpectedStatus: Integer);
begin
  var lResp := FClient.Post(Path, Body);
  Assert.AreEqual(ExpectedStatus, lResp.StatusCode);
end;
```

---

## 10. DUnitX Console Runner

**File: `MyProjectTests.dpr`**

```delphi
program MyProjectTests;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  DUnitX.TestFramework,
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  Tests.Base in 'Tests.Base.pas',
  Tests.MyResource in 'Tests.MyResource.pas',
  Tests.Auth in 'Tests.Auth.pas',
  Tests.Server in 'Tests.Server.pas',
  BootConfigU;                   // the app's own unit


var
  runner: ITestRunner;
  results: IRunResults;
  logger: ITestLogger;
  nunitLogger: ITestLogger;
begin
  try
    Boot;                        // once per process - see section 2
    TDUnitX.CheckCommandLine;
    runner := TDUnitX.CreateRunner;
    runner.UseRTTI := True;

    logger := TDUnitXConsoleLogger.Create(True);
    runner.AddLogger(logger);

    // Optional: NUnit XML output for CI
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(
      TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);

    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      System.ExitCode := EXIT_ERRORS;
    end;
  end;
end.
```

---

## 11. DUnitX Attributes Reference

| Attribute | Scope | Purpose |
|-----------|-------|---------|
| `[TestFixture]` | Class | Marks class as test suite |
| `[Test]` | Method | Marks method as a test |
| `[Setup]` | Method | Runs before each test |
| `[TearDown]` | Method | Runs after each test |
| `[SetupFixture]` | Method | Runs once before all tests in fixture |
| `[TeardownFixture]` | Method | Runs once after all tests in fixture |
| `[TestCase('label', 'p1,p2')]` | Method | Parameterized test case |
| `[Ignore('reason')]` | Class or Method | Skip test with message |
| `[Category('slow')]` | Method | Group tests for filtering |

---

## 12. Assert Reference

```delphi
Assert.AreEqual(expected, actual);
Assert.AreEqual<Integer>(200, lResp.StatusCode);
Assert.AreEqual<string>('foo', lJSON.S['name']);
Assert.AreNotEqual(unexpected, actual);
Assert.IsTrue(condition);
Assert.IsFalse(condition);
Assert.IsNull(obj);
Assert.IsNotNull(obj);
Assert.IsEmpty(str);
Assert.IsFalse(str.IsEmpty);
Assert.Contains(haystack, needle);            // string contains
Assert.StartsWith(prefix, str);
Assert.AreEqual(3, list.Count);
Assert.WillRaise(procedure begin ... end, EMyException);
Assert.WillNotRaise(procedure begin ... end);
Assert.Pass;    // force pass
Assert.Fail('reason');
Assert.Fail(Format('Expected %d got %d', [expected, actual]));
```

---

## 13. HTTP_STATUS Constants

```delphi
HTTP_STATUS.OK                  // 200
HTTP_STATUS.Created             // 201
HTTP_STATUS.Accepted            // 202
HTTP_STATUS.NoContent           // 204
HTTP_STATUS.MovedPermanently    // 301
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

## 14. Common Pitfalls

- **Port conflicts**: always use a non-standard port (e.g., 9998) so tests don't clash with the running dev server
- **Server startup**: `[SetupFixture]` (once) starts the listener; `[Setup]` (per test) creates a fresh client — never start/stop the server per test, it's expensive
- **DB isolation**: use a **separate test database**; reset state in `[TearDown]` not `[SetupFixture]` so each test is independent
- **`ToJSONObject` ownership**: `lResp.ToJSONObject` returns a new `TJsonObject` — always `Free` it in a `try/finally`
- **Path params syntax**: routes use `($param)`; the client uses `{param}` in the URL, filled by `AddPathParam`: `.AddPathParam('id', 42).Get('/api/customers/{id}')`
- **Object posting**: `FClient.Post(url, obj, ownsObject)` — pass `False` if you still need the object after the call
- **Serialization units**: add `MVCFramework.Serializer.JsonDataObjects` and `JsonDataObjects` to uses when deserializing responses to typed objects
- **`[SetupFixture]` vs `[Setup]`**: if you mistakenly start the HTTP server in `[Setup]`, it restarts for every test — this is very slow and often causes port-in-use errors

---

## 15. Scaffolding Workflow for Tests

When the user asks to add tests for a controller/resource:

### Step 1 — Identify what to test
List the endpoints from the controller: GET list, GET by ID, POST, PUT, PATCH, DELETE.
Include error cases: not found, invalid input, unauthorized, forbidden.

### Step 2 — Create (or reuse) `Tests.Server.pas`
Build the engine, register the controller(s) under test and only the filters actually exercised,
host it with `TMVCServerFactory.CreateIndyDirect`.

### Step 3 — Create `Tests.Base.pas`
With `TBaseAPITest` holding `FEngine` + `FServer` + `FClient`, `SetupFixture`/`TeardownFixture`,
`Setup`/`TearDown`.

### Step 4 — Create `Tests.[Resource].pas`
One `[TestFixture]` class per controller. Name tests:
`[Action]_[Condition]_[ExpectedOutcome]` — e.g. `GetByID_NotFound_Returns404`.

### Step 5 — Create or update `MyProjectTests.dpr`
Add the new unit to the `uses` clause; DUnitX auto-discovers via `TDUnitX.RegisterTestFixture`.

### Step 6 — Register fixture
Add `initialization TDUnitX.RegisterTestFixture(TMyTests);` at the bottom of each test unit.

---

## 16. Key Units Reference

| Unit | Purpose |
|------|---------|
| `DUnitX.TestFramework` | `[TestFixture]`, `[Test]`, `Assert.*` |
| `DUnitX.Loggers.Console` | Console output |
| `DUnitX.Loggers.Xml.NUnit` | NUnit XML for CI |
| `MVCFramework.Server.Intf` | `IMVCServer` (`Listen`, `Stop`, `IsRunning`) |
| `MVCFramework.Server.Factory` | `TMVCServerFactory.CreateIndyDirect / CreateHttpSys / CreateWebBroker` |
| `MVCFramework.RESTClient` | `TMVCRESTClient` |
| `MVCFramework.RESTClient.Intf` | `IMVCRESTClient`, `IMVCRESTResponse` |
| `MVCFramework.Commons` | `HTTP_STATUS`, `TMVCMediaType` |
| `MVCFramework.Serializer.JsonDataObjects` | `TMVCJsonDataObjectsSerializer`, `TJSONUtils.JSONArrayToListOf<T>` |
| `JsonDataObjects` | `TJsonObject`, `TJsonArray` — and their aliases `TJDOJsonObject` / `TJDOJsonArray`, which is what `ToJSONObject` / `ToJSONArray` return |
