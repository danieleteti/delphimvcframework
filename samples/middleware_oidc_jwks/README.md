# OIDC + JWKS Signature Verification Sample

DMVCFramework application demonstrating **production-ready** OpenID Connect
authentication with cryptographic ID token signature verification via JWKS
(JSON Web Key Set).

## Two Modes of Operation

DMVCFramework's OIDC middleware supports **two security levels** for ID token
validation. Understanding the difference is essential for choosing the right
mode for your deployment.

### Mode 1: TLS Trust (default, no extra dependencies)

```pascal
uses
  MVCFramework.Middleware.OIDC;  // only this

Engine.AddMiddleware(
  UseOIDCAuthentication(OnUser, OnAuth, Issuer, ClientId, Secret, RedirectUri, JWTSecret)
);
```

**How it works:** The middleware validates ID token *claims* (issuer, audience,
expiration, nonce) but does **not** verify the cryptographic signature. It
trusts the token because it was received directly from the OIDC provider over
TLS — the token never passes through the user's browser.

**When this is acceptable:**
- Internal applications behind a corporate network
- Development and testing environments
- When you control the TLS termination and trust the network path

**Limitations:**
- If TLS is compromised (e.g., corporate proxy MITM, misconfigured infra),
  a forged token could be accepted
- Does not comply with OIDC Core spec Section 3.1.3.7 which recommends
  signature verification
- Not suitable for high-security or regulated environments

### Mode 2: JWKS Signature Verification (production recommended)

```pascal
uses
  MVCFramework.Middleware.OIDC,
  MVCFramework.OIDC.JWKS;       // <-- add this (requires TaurusTLS)

Engine.AddMiddleware(
  UseOIDCAuthentication(OnUser, OnAuth, Issuer, ClientId, Secret, RedirectUri, JWTSecret)
    .SetJWKSProvider(
      TMVCJWKSClient.CreateFromIssuer(Issuer))  // auto-discovers jwks_uri
);
```

**How it works:** In addition to claims validation, the middleware:
1. Fetches the provider's public keys from the `jwks_uri` endpoint
2. Matches the key by `kid` (Key ID) from the token header
3. Verifies the cryptographic signature (RS256, ES256, etc.)
4. Rejects tokens with invalid or missing signatures

**When to use this:**
- **Production deployments** (always recommended)
- Applications handling sensitive data
- Regulated environments (PCI-DSS, HIPAA, SOC2)
- When tokens may traverse untrusted networks
- Multi-tenant systems where token forgery risk is higher

## Architecture: Why Two Separate Units?

```
                              NO TaurusTLS dependency
                    +--------------------------------------------+
                    |                                            |
  Framework        |  MVCFramework.JWT.pas                      |
  Packages         |    IJWTSigner       (signing abstraction)  |
  (dmvcframeworkRT) |    IJWKSProvider    (JWKS abstraction)     |
                    |    THMACJWTSigner   (HS256/384/512)        |
                    |                                            |
                    |  MVCFramework.Middleware.OIDC.pas          |
                    |    TMVCOIDCAuthenticationMiddleware        |
                    |    Accepts IJWKSProvider (optional)        |
                    |                                            |
                    +--------------------------------------------+

                              REQUIRES TaurusTLS (opt-in)
                    +--------------------------------------------+
                    |                                            |
  NOT in packages  |  MVCFramework.JWT.RSA.pas                  |
  (user adds to    |    TRSAJWTSigner    (RS256/384/512)        |
   project uses)   |    TRSAPSSJWTSigner (PS256/384/512)        |
                    |    TECDSAJWTSigner  (ES256/384/512)        |
                    |    TEdDSAJWTSigner  (Ed25519)              |
                    |                                            |
                    |  MVCFramework.OIDC.JWKS.pas (THIS)        |
                    |    TMVCJWKSClient                          |
                    |    JWK -> PEM conversion                   |
                    |    JWKS endpoint fetching + caching        |
                    |                                            |
                    +--------------------------------------------+
```

The key design principle: **TaurusTLS is always opt-in**. Projects that don't
need asymmetric JWT or JWKS verification never have to install TaurusTLS or
ship OpenSSL DLLs. The framework packages (`dmvcframeworkRT`) have zero
TaurusTLS dependencies.

## Supported Key Types

The JWKS client automatically handles keys from any OIDC provider:

| Provider | Typical Algorithm | JWK Key Type |
|----------|------------------|--------------|
| Microsoft Entra ID (Azure AD) | RS256 | RSA |
| Google | RS256 | RSA |
| Auth0 | RS256 | RSA |
| Keycloak | RS256 (default), ES256 | RSA, EC |
| Okta | RS256 | RSA |
| Modern providers | ES256, EdDSA | EC, OKP |

Supported JWK key types:
- **RSA** (`kty: "RSA"`) — RS256, RS384, RS512, PS256, PS384, PS512
- **EC** (`kty: "EC"`) — ES256 (P-256), ES384 (P-384), ES512 (P-521)
- **OKP** (`kty: "OKP"`) — EdDSA (Ed25519)

## Prerequisites

- Delphi 12+
- DMVCFramework (latest)
- An OIDC provider (Keycloak, Microsoft Entra ID, Auth0, Google, etc.)
- **TaurusTLS** source in the Delphi search path
- **OpenSSL DLLs** (1.1.1+ or 3.x) in the application directory:
  - For Win32: `libcrypto-1_1.dll` + `libssl-1_1.dll` (OpenSSL 1.1.x)
    or `libcrypto-3.dll` + `libssl-3.dll` (OpenSSL 3.x)
  - For Win64: `libcrypto-1_1-x64.dll` + `libssl-1_1-x64.dll` (OpenSSL 1.1.x)
    or `libcrypto-3-x64.dll` + `libssl-3-x64.dll` (OpenSSL 3.x)

## Setup

1. **Register a client** with your OIDC provider:
   - Redirect URI: `<BASE_URL>/auth/callback` (default: `http://localhost:8080/auth/callback`)
   - Scopes: `openid email profile`
   - Grant type: Authorization Code

2. **Add TaurusTLS** to your Delphi search path.

3. **Copy OpenSSL DLLs** to the output directory (e.g., `Win32\Debug\`).

4. **Copy and edit the environment file:**
   ```
   cp .env.example .env
   ```
   Fill in `OIDC_ISSUER`, `OIDC_CLIENT_ID`, `OIDC_CLIENT_SECRET`, and set
   `JWT_SECRET` to a random string.

5. **Build and run** the project.

6. **Open** `http://localhost:8080` in your browser.

## Configuration

### Environment Variables

| Variable | Required | Default | Description |
|----------|----------|---------|-------------|
| `DMVC_SERVER_PORT` | No | `8080` | HTTP listen port |
| `OIDC_ISSUER` | **Yes** | - | OIDC provider URL |
| `OIDC_CLIENT_ID` | **Yes** | - | OAuth2 client ID |
| `OIDC_CLIENT_SECRET` | **Yes** | - | OAuth2 client secret |
| `BASE_URL` | No | `http://localhost:8080` | Application base URL |
| `JWT_SECRET` | **Yes** | - | Secret for local session JWT (min 32 chars) |
| `JWT_EXPIRATION_HOURS` | No | `8` | Session duration in hours |
| `OIDC_VERIFY_SIGNATURE` | No | `true` | Enable JWKS signature verification |
| `DMVC_VIEW_PATH` | No | `templates` | Path to HTML templates |

### JWKS Cache

The JWKS client caches public keys for **1 hour** by default (configurable via
the `ACacheTTLSeconds` parameter). When a token arrives with an unknown `kid`
(Key ID), the client automatically refetches the JWKS endpoint to handle
**key rotation** — no restart required.

## Endpoints

| Path | Auth | Description |
|------|------|-------------|
| `GET /` | Public | Landing page with login button |
| `GET /dashboard` | Protected | Shows authenticated user's OIDC claims |
| `GET /auth/login` | - | Redirects to OIDC provider |
| `GET /auth/callback` | - | Handles OIDC callback (internal) |
| `GET /auth/logout` | - | Ends session at provider, redirects to `/` |

## API Reference

### TMVCJWKSClient

```pascal
// Create from OIDC issuer (recommended for OIDC)
// Auto-discovers jwks_uri from .well-known/openid-configuration
Provider := TMVCJWKSClient.CreateFromIssuer(
  'https://login.microsoftonline.com/tenant/v2.0',
  3600  // cache TTL in seconds (default: 1 hour)
);

// Create from explicit JWKS URI (for non-OIDC scenarios)
Provider := TMVCJWKSClient.Create(
  'https://login.microsoftonline.com/common/discovery/v2.0/keys',
  3600
);

// Force refresh (e.g., after a key rotation notification)
Provider.RefreshKeys;

// Check cached key count
WriteLn(Provider.KeyCount);
```

### Integration with OIDC Middleware

```pascal
UseOIDCAuthentication(
  OnUserAuthenticated,
  OnAuthRequired,
  dotEnv.Env('OIDC_ISSUER', ''),
  dotEnv.Env('OIDC_CLIENT_ID', ''),
  dotEnv.Env('OIDC_CLIENT_SECRET', ''),
  dotEnv.Env('BASE_URL', '') + '/auth/callback',
  dotEnv.Env('JWT_SECRET', '')
)
.SetJWKSProvider(                                      // <-- enable verification
  TMVCJWKSClient.CreateFromIssuer(
    dotEnv.Env('OIDC_ISSUER', '')))
.SetCookieSecure(True)                                 // HTTPS in production
.SetBaseURL(dotEnv.Env('BASE_URL', ''));
```

## Security Notes

- The local **session JWT** (signed with `JWT_SECRET`) uses HMAC and does NOT
  require TaurusTLS. It secures the session cookie after OIDC login.
- The **ID token** from the OIDC provider is signed with the provider's private
  key (typically RS256). JWKS verification checks this signature using the
  provider's public key.
- These are two different JWTs with different purposes:
  - ID token: issued by the OIDC provider, verified once during login callback
  - Session JWT: issued by your application, verified on every request
- Set `Secure=True` on cookies when deploying behind HTTPS.
- The JWKS client validates `kid` and `alg` to prevent algorithm confusion attacks.
