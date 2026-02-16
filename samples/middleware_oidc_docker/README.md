# OIDC Docker Sample

DMVCFramework sample demonstrating OpenID Connect authentication with Docker
Compose deployment, PostgreSQL-backed user management, and automatic
first-user-is-admin promotion.

## What this sample demonstrates

- OIDC authentication via `MVCFramework.Middleware.OIDC`
- Docker Compose deployment (application + PostgreSQL)
- ActiveRecord user entity with automatic provisioning on first OIDC login
- First user to log in becomes admin, subsequent users get viewer role
- Role-based access control (admin-only user list)
- SQL-file database migrations
- TemplatePro server-side rendering with BulmaCSS

## Prerequisites

- Delphi 12+ (for compilation)
- Docker and Docker Compose (for deployment)
- An OIDC provider (Keycloak, Pocket-ID, Entra ID, Auth0, Google, etc.)

## OIDC provider setup

Register a new client with your OIDC provider:

1. Set the redirect URI to `http://localhost:8080/auth/callback`
2. Enable scopes: `openid`, `email`, `profile`
3. Grant type: Authorization Code
4. Note the **Client ID** and **Client Secret**

## Configuration

Copy the environment template and fill in your values:

```bash
cd docker
cp .env.example .env
```

Edit `docker/.env`:

| Variable | Description |
|---|---|
| `DB_PASSWORD` | PostgreSQL password |
| `OIDC_ISSUER` | Your OIDC provider URL (e.g. `https://pocket-id.example.com`) |
| `OIDC_CLIENT_ID` | Client ID from provider registration |
| `OIDC_CLIENT_SECRET` | Client secret from provider registration |
| `OIDC_REDIRECT_URI` | Must match what you registered (`http://localhost:8080/auth/callback`) |
| `JWT_SECRET` | Random string (64+ chars) for signing session cookies |

## Run with Docker Compose

```bash
cd docker
docker compose up -d
```

Open `http://localhost:8080` in your browser. The first user to log in via
OIDC becomes the admin.

## Local development (without Docker)

1. Run a PostgreSQL instance locally (or use the Docker Compose PostgreSQL only)
2. Copy `.env.example` to `.env` in the project root
3. Set `DB_HOST=localhost` and fill in OIDC credentials
4. Compile the project in Delphi (Win32 Debug)
5. Run the executable

## Endpoints

| Path | Auth | Description |
|---|---|---|
| `GET /` | Public | Landing page with login button |
| `GET /dashboard` | Protected | OIDC claims and role information |
| `GET /users` | Admin only | List of all registered users |
| `GET /health` | Public | Docker health check (JSON) |
| `GET /auth/login` | - | Redirects to OIDC provider |
| `GET /auth/callback` | - | Handles OIDC callback (internal) |
| `GET /auth/logout` | - | Clears session, redirects to `/` |

## Project structure

| File | Description |
|---|---|
| `OIDCDockerSample.dpr` | Console application entry point |
| `WebModuleU.pas` | Engine setup, middleware stack, OIDC callbacks |
| `AppConfigU.pas` | Environment variable configuration wrapper |
| `UserEntityU.pas` | ActiveRecord entity for the users table |
| `MigrationServiceU.pas` | SQL-file migration runner |
| `HomeControllerU.pas` | Public landing page |
| `DashboardControllerU.pas` | Protected dashboard showing OIDC claims |
| `UserControllerU.pas` | Admin-only user list |
| `HealthControllerU.pas` | Docker health check endpoint |
| `Migrations/V001_CreateUsers.sql` | Users table schema |
| `docker/docker-compose.yml` | App + PostgreSQL services |
| `docker/Dockerfile` | Linux64 container image |
| `docker/init.sql` | Bootstrap schema_migrations table |
