# OIDC Middleware Sample

Minimal DMVCFramework application demonstrating OpenID Connect authentication
using `MVCFramework.Middleware.OIDC`.

## What this sample shows

- Configuring the OIDC middleware via `UseOIDCAuthentication`
- Customizing cookie settings with fluent setters (`.SetCookieName`, `.SetCookieSecure`)
- Proper OIDC logout via `.SetBaseURL` (redirects to provider's end-session endpoint)
- Implementing `HandleOIDCUser` to extract claims into session data
- Implementing `HandleAuthRequired` to make specific actions public
- Accessing session data in controllers via `Context.LoggedUser.CustomData`

## Prerequisites

- Delphi 12+
- DMVCFramework (latest)
- An OIDC provider (Keycloak, Entra ID, Auth0, Google, etc.)

## Setup

1. **Register a client** with your OIDC provider:
   - Redirect URI: `<BASE_URL>/auth/callback` (default: `http://localhost:8080/auth/callback`)
   - Scopes: `openid email profile`
   - Grant type: Authorization Code

2. **Copy and edit the environment file:**
   ```
   cp .env.example .env
   ```
   Fill in `OIDC_ISSUER`, `OIDC_CLIENT_ID`, `OIDC_CLIENT_SECRET`, and set
   `JWT_SECRET` to a random string. Set `BASE_URL` if not running on
   `http://localhost:8080` — the OIDC redirect URI is derived from it.

3. **Build and run** the project in Delphi or from the command line.

4. **Open** `http://localhost:8080` in your browser.

## Endpoints

| Path | Auth | Description |
|------|------|-------------|
| `GET /` | Public | Landing page with login button |
| `GET /dashboard` | Protected | Shows authenticated user's OIDC claims |
| `GET /auth/login` | - | Redirects to OIDC provider |
| `GET /auth/callback` | - | Handles OIDC callback (internal) |
| `GET /auth/logout` | - | Ends session at OIDC provider, then redirects to `/` |

## Notes

- The session cookie has `Secure=False` for local HTTP development.
  Set `.SetCookieSecure(True)` when deploying behind HTTPS.
- No database is used. The `HandleOIDCUser` callback stores claims in the
  in-memory session only. For production use, add `TMVCActiveRecordMiddleware`
  before the OIDC middleware and look up/create users in the callback.
