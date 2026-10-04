# webapp_htmx_forms

A controller-based DMVCFramework web application (TemplatePro + HTMX, Indy Direct host) with a "People" section: a table and the forms to create and edit a row. Data lives in memory and is reset at every restart.

## What it shows

- **Table with search, filter and sort via HTMX.** The search box, the status filter and the column headers request `/web/people` with `hx-get`; the server returns only the table fragment and HTMX swaps it in.
- **Page vs fragment.** The action returns the fragment only when `Context.Request.IsHTMX and not Context.Request.HXIsBoosted and not Context.Request.HXIsHistoryRestoreRequest`; otherwise it renders the full page. It always sets `Vary: HX-Request`, because the same URL has two bodies.
- **New/edit forms** built with every macro of the TemplatePro forms library (`bin/templates/lib/forms_bootstrap5.tpro`): `form`, `input`, `textarea`, `select`, `checkbox`, `submit`, `actions`, `auto`.
- **Server-side validation** with the DMVCFramework validator attributes (`MVCRequired`, `MVCMinLength`, `MVCMaxLength`, `MVCEmail`, `MVCIn`, `MVCPastOrPresent`, `MVCRange`) checked by `TMVCValidationEngine.Validate`.
- **422 re-render.** An invalid post renders the form again with status 422, the typed values and a message next to each field.
- **Post/Redirect/Get.** A valid post redirects (302) to the table, so reloading the page does not submit the form again.

The People pages have no login and no CSRF protection: add both before reusing them for real data.

## Run

1. Open `WebAppHTMXForms.dpr` in RAD Studio and press F9 (or build `WebAppHTMXForms.dproj` with msbuild; the exe goes to `bin\`).
2. Browse to `http://localhost:8080/web/people`. The port is `dmvc.server.port` in `bin\.env`.

## Key files

| File | Content |
|------|---------|
| `Controllers.PeoplePagesU.pas` | Table, new and edit actions; page/fragment rule; 422 and redirect |
| `PeopleSampleU.pas` | Row class with validator attributes, in-memory store, table query, form check |
| `bin/templates/people/index.html` | Full page: toolbar, search box, includes the table |
| `bin/templates/people/table.html` | The table fragment returned to HTMX |
| `bin/templates/people/edit.html` | New/edit form |
| `bin/templates/lib/forms_bootstrap5.tpro` | TemplatePro forms library (Bootstrap 5) |
| `EngineConfigU.pas` | Engine setup: TemplatePro view engine, static files, controllers |
