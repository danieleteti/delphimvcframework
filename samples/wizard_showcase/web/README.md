# Minimal API WebApp Showcase

Server-rendered companion to the REST showcase. Same set of binding-mode
demos, surfaced as TemplatePro views with HTMX for interactivity.

## Run

Open `MinimalAPIWebAppShowcase.dpr` in Delphi, press F9. Browser:
`http://localhost:8080/` shows a Bootstrap-styled landing listing every
endpoint with try-it links.

## URL surface

| Route | Demonstrates |
|---|---|
| `GET /` | arity-1 `TWebContext` + `RenderView`/`ViewData` |
| `GET /people` | **DI**: `IPeopleService` + view + per-row `Delete` button |
| `GET /people/(id:int)` | **primitive** Integer from a route segment |
| `DELETE /people/(id:int)` | **`MapDelete`** via HTMX `hx-delete`; returns empty body, HTMX swaps the `<tr>` out |
| `GET /search` | **record** + `[MVCFromQueryString]` with defaults (`TSearchQuery`) |
| `GET /signup` | form view, no binding |
| `POST /signup` | **record** + `[MVCFromContentField]` including `TArray<string>` (multi-checkbox `interests`) |
| `GET /context` | **record** + `[MVCFromHeader]` + `[MVCFromCookie]` (with cookie round-trip) |
| `GET /docs/(slug:*)` | **wildcard** — a trailing `($slug:*)` segment captures the rest of the path (slashes included) as one `string` |

Session is set up via `MemorySession(30)` filter on the public group;
`/people/*` adds a logging filter via `.Use(LogFilter())`.

## Key files

- `RoutesU.pas` — every handler is annotated with the binding mode it
  illustrates.
- `ShowcaseModelsU.pas` — `TSignupForm`, `TContextInfo`, `TSearchQuery`
  records (each field shows a different attribute source).
- `templates/baselayout.html` + `templates/pages/*.html` — Bootstrap 5.3
  views (dark mode default, `data-bs-theme`).
- `ServicesU.pas` — `IPeopleService` registration the routes resolve.
