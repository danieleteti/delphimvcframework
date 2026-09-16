# Minimal API Showcase Samples

Two self-documenting demos of the **Minimal API** feature surface in
DelphiMVCFramework. Each one is a complete, runnable project that catalogs
every parameter-binding mode the framework supports.

These started as wizard presets and were moved into `samples/` so the wizard
project picker stays focused on production-shaped scaffolds. Open the `.dpr`
in Delphi, F9, point a browser at `http://localhost:8080/`.

| Sample | Output | Demonstrates |
|---|---|---|
| [`rest/`](./rest/) | JSON over HTTP | DI interface, primitive route segment, class body + `TMVCValidatable`, record + `[MVCFromQueryString]`, group filter via `.Use` |
| [`web/`](./web/) | TemplatePro views + HTMX | Same as above for the web idiom: server-rendered pages, `RenderView`/`ViewData`, `MapDelete` via `hx-delete`, record + `[MVCFromContentField]` (multi-value `TArray<string>`), `[MVCFromHeader]`, `[MVCFromCookie]` |

Together they cover every binding mode in `MVCFramework.MinimalAPI`. Read
both `RoutesU.pas` and the companion `ShowcaseModelsU.pas` — both are
heavily commented and each handler names the binding mode it illustrates.
