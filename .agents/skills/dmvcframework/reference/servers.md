# DMVCFramework — Hosting the engine

**For a new project the default is Indy Direct.** Use it unless the deployment target forces something else.

**Never rewrite the host of an existing project.** Read its `.dpr` and work with whatever host it already
has — a WebBroker project (ISAPI, Apache) is a deliberate deployment choice, not a defect. Everything above
the host is identical anyway.

| Host | Use it when |
|------|-------------|
| **Indy Direct** — `TMVCServerFactory.CreateIndyDirect` | **The default for a new project.** Self-contained console/service executable. No WebModule, no WebBroker. |
| HTTP.sys — `CreateHttpSys` | You need the Windows kernel-mode HTTP stack (port sharing with IIS, kernel TLS). Requires admin rights or a `netsh http add urlacl` reservation. |
| WebBroker — `CreateWebBroker` / `TMVCEngine.CreateForWebBroker` | You deploy **inside** a WebBroker host: ISAPI DLL (IIS), Apache module, or an existing WebBroker app. Fully supported — just not the default for a new standalone server. |

The engine and the controllers are identical in all three — only the host differs. Keep the engine
configuration in one shared `ConfigureEngine(AEngine: TMVCEngine)` and let the `.dpr` pick the host.

---

## Indy Direct — the default

```delphi
uses
  MVCFramework,
  MVCFramework.Server.Intf,      // IMVCServer
  MVCFramework.Server.Factory,   // TMVCServerFactory
  MVCFramework.Signal;           // WaitForTerminationSignal
                                 // (EnterInShutdownState / IsShuttingDown live in MVCFramework)

procedure RunServer(APort: Integer);
var
  lEngine: TMVCEngine;
  lServer: IMVCServer;
begin
  lEngine := TMVCEngine.Create(
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.DefaultContentType] := TMVCMediaType.APPLICATION_JSON;
    end);
  try
    ConfigureEngine(lEngine);   // controllers + middleware — shared with every host
    lServer := TMVCServerFactory.CreateIndyDirect(lEngine);
    lServer.RunAndWait(APort);  // Listen + wait for the termination signal + Stop
  finally
    lEngine.Free;
  end;
end;
```

`RunAndWait` is the short form. Spell it out when something must happen between start and stop:

```delphi
lServer := TMVCServerFactory.CreateIndyDirect(lEngine);
lServer.Listen(8080);
WaitForTerminationSignal;
EnterInShutdownState;
lServer.Stop;
```

`IMVCServer` also exposes `IsRunning`, and HTTPS through its own certificate properties — never configure
TLS on the Indy component by hand.

**For a new standalone server: no WebModule, no `.dfm`, no `Web.HTTPApp`.** Introducing a `TWebModule` there
means you have taken a wrong turn.

This says nothing about an **existing** WebBroker project — see below.

---

## HTTP.sys

```delphi
lServer := TMVCServerFactory.CreateHttpSys(lEngine);
lServer.Listen(8080, 'localhost');
```

Needs admin rights, or a URL reservation made once:

```
netsh http add urlacl url=http://+:8080/ user=DOMAIN\myuser
```

---

## WebBroker — for ISAPI, Apache, and existing WebBroker apps

A WebBroker project is **not a mistake to be corrected**. ISAPI (behind IIS) and Apache modules are real
deployment requirements, and an older app may simply predate Indy Direct.

**If the project you are working in is WebBroker-hosted, keep it.** Add controllers and configure the engine
exactly as you would anywhere else — everything above the host (controllers, actions, routing, `IMVCResponse`,
ActiveRecord, validation, DI, middleware, JWT, serialization) is **identical**. Do not migrate it, and do not
suggest migrating it unless the user asks.

The engine is built **per WebModule**, hence the configuration callback:

```delphi
// class function CreateWebBroker(AConfigAction: TProc<TMVCConfig>;
//                                AEngineConfig: TMVCEngineConfigProc = nil): IMVCServer;
// The FIRST callback sets TMVCConfig values; the engine callback is the SECOND one.
lServer := TMVCServerFactory.CreateWebBroker(
  procedure(Config: TMVCConfig)
  begin
    Config[TMVCConfigKey.DefaultContentType] := TMVCMediaType.APPLICATION_JSON;
  end,
  procedure(AEngine: TMVCEngine)
  begin
    ConfigureEngine(AEngine);
  end);
```

The classic form, when you own the `TWebModule`:

```delphi
procedure TMyWebModule.WebModuleCreate(Sender: TObject);
begin
  FEngine := TMVCEngine.CreateForWebBroker(Self,
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.DefaultContentType] := TMVCMediaType.APPLICATION_JSON;
    end);
  ConfigureEngine(FEngine);
end;
```

`CreateForWebBroker` is the speaking constructor — it makes the host explicit at the call site.

The DMVCFramework repository carries a sample per host under `samples/` — one shared `ConfigureEngine`,
several hosts (Indy Direct, HTTP.sys, WebBroker standalone, WebBroker behind `IMVCServer`, ISAPI, Apache).

---

## Database Connection (FireDAC)

**File: `FDConnectionConfigU.pas`**

```delphi
unit FDConnectionConfigU;

interface

const
  CON_DEF_NAME = 'MyAppConn';

procedure SetupDatabaseConnection(AIsPooled: Boolean = True);

implementation

uses System.Classes, FireDAC.Comp.Client;

procedure SetupDatabaseConnection(AIsPooled: Boolean);
var
  LParams: TStringList;
begin
  LParams := TStringList.Create;
  try
    // PostgreSQL — swap DriverID for MySQL / FB / IB / MSSQL / SQLite
    LParams.Add('DriverID=PG');
    LParams.Add('Database=myapp');
    LParams.Add('Server=localhost');
    LParams.Add('User_Name=myuser');
    LParams.Add('Password=mypassword');
    LParams.Add('CharacterSet=UTF8');
    if AIsPooled then
    begin
      LParams.Add('Pooled=True');
      LParams.Add('POOL_MaximumItems=50');
    end;
    FDManager.AddConnectionDef(CON_DEF_NAME, 'PG', LParams);
  finally
    LParams.Free;
  end;
end;

end.
```

Supported FireDAC DriverIDs: `PG` (PostgreSQL), `MySQL`, `FB` (Firebird), `IB` (Interbase), `MSSQL`, `SQLite`.

Call `SetupDatabaseConnection` once at startup, before the engine registers `TMVCActiveRecordMiddleware`.

---
