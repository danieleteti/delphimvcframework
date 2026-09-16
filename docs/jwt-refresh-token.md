# JWT Access + Refresh Token

DelphiMVCFramework ships JWT **access token** validation (`TMVCJWTAuthenticationMiddleware`)
and a sliding-expiration mechanism (`LiveValidityWindowInSeconds`). It does **not** ship a
refresh token out of the box. This feature adds an OAuth2-style **refresh token** as an
additive, opt-in layer: short-lived access tokens plus a long-lived, **revocable** refresh
token with rotation and reuse-detection.

## When to use which

| Mechanism | What it gives | Limitation |
|---|---|---|
| `LiveValidityWindowInSeconds` | access token auto-extends while the user is active | not revocable; the token *is* the long-lived secret |
| Refresh token (this feature) | short access token + long, revocable refresh token | refresh store is stateful |

They are complementary. Use the sliding window for "keep an active user logged in"; use the
refresh token when you need short access tokens with central revocation.

## Design

- The **access token** is a normal JWT (HS512 by default) carrying `username` and `roles`,
  verified by the existing `TMVCJWTAuthenticationMiddleware`.
- The **refresh token** is an *opaque* random string (not a JWT) — so it can be revoked.
  The store keeps only its **SHA-256 hash**, never the token itself.
- **Rotation**: every refresh consumes the presented token and issues a new one in the same
  *family* (login session).
- **Reuse-detection**: if an already-rotated token is presented again (replay), the whole
  family is revoked. A stolen token therefore burns the session instead of granting access.

## Units

| Unit | Contents | Extra dependency |
|---|---|---|
| `MVCFramework.JWT.RefreshToken` | core, in-memory store, classic middleware, helpers | none |
| `MVCFramework.JWT.RefreshToken.Filters` | engine-wide `RefreshTokenHTTPFilter` | `MVCFramework.MinimalAPI` |
| `MVCFramework.JWT.RefreshToken.ActiveRecord` | durable store + entity | FireDAC / ActiveRecord |

The transport-agnostic logic lives once in `TMVCRefreshTokenCore`; the classic middleware and
the HTTPFilter are thin adapters over it. The refresh endpoint is a standalone, engine-wide
concern, so it maps to a classic middleware or an HTTPFilter — not to a per-group EndpointFilter.

## Store contract

```pascal
IMVCRefreshTokenStore = interface
  function Issue(const AUsername, ARoles: string; const ATTLSeconds: Integer): string;
  function Rotate(const AOldRefreshToken: string; const ATTLSeconds: Integer): TMVCRefreshTokenRotation;
  procedure Revoke(const ARefreshToken: string);
end;
```

`Rotate` returns a status: `rtsOK`, `rtsNotFound`, `rtsExpired`, `rtsReuseDetected`.

Two implementations are provided:

- `TMVCInMemoryRefreshTokenStore` — reference, not durable across restarts.
- `TMVCActiveRecordRefreshTokenStore` — durable, backed by a `refresh_tokens` table. A FireDAC
  connection is acquired from the pool per call, so it also works in `OnBeforeRouting`.

Table DDL (SQLite shown; adapt types per RDBMS):

```sql
CREATE TABLE refresh_tokens (
  id          INTEGER PRIMARY KEY AUTOINCREMENT,
  token_hash  TEXT NOT NULL,
  family_id   TEXT NOT NULL,
  username    TEXT NOT NULL,
  roles       TEXT,
  expires_at  TIMESTAMP NOT NULL,
  used        INTEGER DEFAULT 0
);
```

## Wiring — with controllers

```pascal
MVC
  .AddController(TAuthController)        // owns POST /auth/login (returns both tokens)
  .AddController(TProtectedController)
  .AddMiddleware(TMVCJWTAuthenticationMiddleware.Create(   // validates access token
    TAuthenticationSample.Create,
    procedure(const JWT: TJWT) begin JWT.Claims.Issuer := gRefreshCfg.Issuer; end,
    JWT_SECRET, '/__jwt_autologin_unused',
    [TJWTCheckableClaim.ExpirationTime], 0, HMAC_HS512))
  .AddMiddleware(TMVCJWTRefreshTokenMiddleware.Create(gRefreshCfg, gRefreshStore)); // /auth/refresh
```

The middleware auto-login renders only the access token, so login is done by a controller
action that returns both tokens:

```pascal
function TAuthController.Login: TJSONObject;
var
  lAccess, lRefresh: string;
begin
  // ... validate credentials ...
  gRefreshCore.NewTokenPair(lUser, 'role1,role2', lAccess, lRefresh);
  Result := TJSONObject.Create;
  Result.S['access_token'] := lAccess;
  Result.S['refresh_token'] := lRefresh;
  Result.S['token_type'] := 'bearer';
  Result.I['expires_in'] := gRefreshCfg.AccessTokenTTLSeconds;
end;
```

## Wiring — with filters (minimal API)

```pascal
lEngine.UseHTTPFilter(RefreshTokenHTTPFilter(gRefreshCfg, gRefreshStore)); // /auth/refresh

lEngine.Root.MapPost<TLoginInput>('/auth/login',
  function (Input: TLoginInput): IMVCResponse
  var
    lPair: TTokenPair;
  begin
    // ... validate credentials ...
    gRefreshCore.NewTokenPair(Input.Username, 'role1,role2',
      lPair.access_token, lPair.refresh_token);
    lPair.token_type := 'bearer';
    lPair.expires_in := gRefreshCfg.AccessTokenTTLSeconds;
    Result := Ok(lPair);
  end);

lEngine.Root.Prefix('/api').Use(JWT(TAuthHandler.Create, nil, JWT_SECRET, ''))
  .MapGet('/profile', function: IMVCResponse begin Result := Ok(...); end);
```

## Endpoints

```
POST /auth/login    {"username":"u","password":"u"}  -> {access_token, refresh_token, token_type, expires_in}
GET  /api/profile   Authorization: Bearer <access_token>
POST /auth/refresh  {"refresh_token":"..."}          -> new {access_token, refresh_token, ...}
```

`/auth/refresh` responses: `200` with a new pair; `401` on invalid/expired token or on reuse
(`"Refresh token reuse detected; session revoked"`).

## Client flow

1. `POST /auth/login` → store both tokens.
2. Call APIs with the access token.
3. On `401`, `POST /auth/refresh` with the refresh token → new pair → retry. Replace the
   stored refresh token each time (it rotates).

## Production hardening

- Use the ActiveRecord store, not in-memory.
- `Rotate` runs in a transaction; for strict concurrency add row locking
  (`SELECT ... FOR UPDATE` on Postgres/Firebird). SQLite already serializes writers.
- Keep access TTL short (minutes) and refresh TTL long (days).
- The store hashes tokens (SHA-256); never log raw refresh tokens.

## Sample

`samples/jsonwebtoken_refreshtoken/` — runnable WebBroker server demonstrating login, a
protected route, refresh, rotation, and reuse-detection.
