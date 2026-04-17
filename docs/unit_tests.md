# Running the DMVCFramework unit tests

DMVCFramework ships with a unit test suite (~872 test cases) that runs the same
TestClient against five different server hosts, so every supported deployment
scenario is exercised end-to-end.

| Host | Task | Result |
|------|------|--------|
| Classic (WebBroker + Indy bridge) | `tests32` / `tests64` / `tests` | 872/872 |
| Indy Direct (`TMVCIndyServer`) | `tests32-indydirect` / `tests64-indydirect` / `tests-indydirect` | 872/872 |
| HTTP.sys (`TMVCHttpSysServer`, Windows kernel mode) | `tests32-httpsys` / `tests64-httpsys` / `tests-httpsys` | 872/872 |
| Apache 2.4 module | `tests64-apache` / `tests-apache` | 858/858 (14 host-incompatible tests skipped) |
| ISAPI (IIS Express) | `tests64-isapi` / `tests-isapi` | 871/871 (1 host-incompatible test skipped) |
| **Everything** | `tests-all-hosts` | all of the above |

The short aggregate `tests-all` covers only Classic + IndyDirect + HTTP.sys
(no external dependencies required). Use `tests-all-hosts` to also run the
Apache and ISAPI integration sets.

## Prerequisites

### Required for every task

- **Delphi** 10 Seattle or newer (Delphi 13 Florence recommended).
  Build tooling uses `msbuild` via `rsvars.bat`, shipped with Delphi.
- **Python 3.10+** and the [`invoke`](https://pyinvoke.org) + `colorama`
  packages:

  ```bash
  pip install invoke colorama
  ```

All tasks below are invoked from the repository root.

### Additional for `tests-apache` / `tests64-apache`

Nothing to install manually. The first run downloads
[Apache Lounge httpd 2.4](https://www.apachelounge.com/download/) (Win64
VS17 build) and caches it under `unittests/apache/Apache24/`. The folder
is gitignored; delete it to force a fresh download.

The task generates a minimal `httpd-test.conf` at run time, deploys the
TestServerApache module into `Apache24/modules/mod_dmvctest.dll`, starts
Apache in foreground, runs the TestClient against `http://localhost:8888`,
then stops Apache.

> Apache 2.4 module mode is **Win64 only**: Apache Lounge ships 64-bit
> binaries and the DMVC module is built with the matching bitness.

### Additional for `tests-isapi` / `tests64-isapi`

- **IIS Express** already installed — it ships with Visual Studio 2019+
  and with RAD Studio / Delphi. The 64-bit binary is expected at
  `C:\Program Files\IIS Express\iisexpress.exe`. If it is missing:
  - Install Visual Studio Community (free) with the ASP.NET workload, or
  - Download the IIS Express standalone installer from
    [Microsoft Download Center](https://www.microsoft.com/en-us/download/details.aspx?id=48264).

The task starts IIS Express from a generated `applicationhost.config`
that wires every request to `TestServerISAPI.dll` via a wildcard ISAPI
handler. The test DLL and its fixtures live under `unittests/iis/`
(gitignored). No registration of DLLs with the OS is required.

> ISAPI mode is **Win64 only**, matching the IIS Express 64-bit worker
> process that loads the module.

## Running individual hosts

From the repository root:

```bash
# Classic (WebBroker + Indy bridge) — fastest, no external deps
python -m invoke tests

# One host at a time
python -m invoke tests-indydirect
python -m invoke tests-httpsys
python -m invoke tests-apache
python -m invoke tests-isapi

# Full in-process matrix (Classic + IndyDirect + HTTP.sys)
python -m invoke tests-all

# Full integration matrix across ALL five hosts
python -m invoke tests-all-hosts
```

Each task:

1. Builds `DMVCFrameworkTests.dproj` (TestClient) and the host-specific
   TestServer project (Win32 + Win64 where supported).
2. Starts the server (or Apache / IIS Express hosting the library).
3. Executes the TestClient binary against the listening server.
4. Shuts the server down.

The TestClient prints a DUnitX summary at the end:

```
Tests Found   : 872
Tests Ignored : 0
Tests Passed  : 872
Tests Failed  : 0
Tests Errored : 0
```

An NUnit-compatible XML report is also produced under `UnitTestReports/`.

## Host-incompatible tests

A handful of test cases exercise behaviors that depend on the application
having full control of the HTTP transport (custom status reason phrases,
response compression, permissive URL parsing). When the application is
hosted by Apache or IIS, those environments rewrite / filter the HTTP
exchange *before* the DMVC module sees the request, so the assertions
cannot hold regardless of framework correctness.

Those tests are tagged at the source level:

- `[Category('NotOnApache')]` — skipped only under `tests-apache`
- `[Category('NotOnIIS')]` — skipped only under `tests-isapi`
- `[Category('NotOnApache,NotOnIIS')]` — skipped under both

The invoke tasks pass `--exclude` to the DUnitX runner accordingly, so a
successful run reports **0 failed, 0 errored** on every host. Tests that
*could* pass on a given host but don't are real regressions.

## Adding a new test

No special treatment is needed — the same test runs against every host
by default. Only add a `[Category('NotOnApache')]` / `[Category('NotOnIIS')]`
tag (with an explanatory comment) when the test asserts a behavior the
front-end web server is known to override.
