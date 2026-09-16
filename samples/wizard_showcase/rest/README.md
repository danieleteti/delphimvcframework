# Minimal API REST Showcase

Self-documenting demo of every parameter-binding mode the
`MVCFramework.MinimalAPI` surface supports, served as a JSON API.

## Run

Open `MinimalAPIShowcase.dpr` in Delphi, press F9. Browser:
`http://localhost:8080/` shows an inline HTML landing with copy-paste
`curl` examples.

## URL surface

| Route | Demonstrates |
|---|---|
| `GET /` | arity-0 + inline `TMVCHTMLResponse` (no view engine) |
| `GET /people` | **DI**: `IPeopleService` resolved from the service container |
| `GET /people/(id:int)` | **primitive** Integer bound from a route segment |
| `POST /people` | **class body JSON** + auto-validation via `TMVCValidatable` (`TPersonInput` carries `[MVCRequired]`, `[MVCMinLength]`, `[MVCEmail]`) |
| `GET /search` | **record** + `[MVCFromQueryString]` with per-field default values (`TSearchQuery`) |
| `POST /upload` | **file upload** — a `TMVCFormFile` argument binds the first multipart file (`FileName` / `Size` / `ContentType`) |
| `GET /tags` | **typed array from query** — repeated `?tag=` keys bind to `TArray<string>` (`TTagSearch`) |

The `/people/*` subtree is mounted via
`Prefix('/people').Use(LogFilter())` — every route under that prefix
inherits a logging endpoint filter. Add more filters (auth, rate-limit,
role checks) the same way.

## Key files

- `RoutesU.pas` — every handler is annotated with a comment naming the
  binding mode it illustrates.
- `ShowcaseModelsU.pas` — `TPersonInput` (validable body class) and
  `TSearchQuery` (query record).
- `ServicesU.pas` — the `IPeopleService` registration the routes resolve.
