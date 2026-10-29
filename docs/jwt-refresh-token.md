# JWT Access + Refresh Token

DelphiMVCFramework ships JWT **access token** login and validation (`JWTAuthenticationBundle`)
and a sliding-expiration mechanism (`LiveValidityWindowInSeconds`). The OAuth2-style
**refresh token** is an additive, opt-in layer on the same bundle: short-lived access tokens
plus a long-lived, **revocable** refresh token with rotation and reuse-detection.

## When to use which

| Mechanism | What it gives | Limitation |
|---|---|---|
| `LiveValidityWindowInSeconds` | access token auto-extends while the user is active | not revocable; the token *is* the long-lived secret |
| Refresh token (this feature) | short access token + long, revocable refresh token | refresh store is stateful |

They are complementary. Use the sliding window for "keep an active user logged in"; use the
refresh token when you need short access tokens with central revocation.

## Design

- The **access token** is a normal JWT (HS512 by default) carrying `username` and `roles`,
  verified by the check of `JWTAuthenticationBundle` (or the deprecated `TMVCJWTAuthenticationMiddleware`).
- The **refresh token** is an *opaque* random string (not a JWT) — so it can be revoked.
  The store keeps only its **SHA-256 hash**, never the token itself.
- **Rotation**: every refresh consumes the presented token and issues a new one in the same
  *family* (login session).
- **Reuse-detection**: if an already-rotated token is presented again (replay), the whole
  family is revoked. A stolen token therefore burns the session instead of granting access.

## Units

| Unit | Contents | Extra dependency |
|---|---|---|
| `MVCFramework.Filters.Authentication` | `JWTAuthenticationBundle`: login, refresh URL and check in one `Use` | none |
| `MVCFramework.JWT.RefreshToken` | core, config, in-memory store, classic middleware (deprecated), helpers | none |
| `MVCFramework.JWT.RefreshToken.Filters` | engine-wide `RefreshTokenHTTPFilter` (the refresh URL alone) | none |
| `MVCFramework.JWT.RefreshToken.ActiveRecord` | durable store + entity | FireDAC / ActiveRecord |

The transport-agnostic logic lives once in `TMVCRefreshTokenCore`; the HTTPFilter, the login
of the bundle and the classic middleware (`TMVCJWTRefreshTokenMiddleware`, deprecated in 3.5)
are thin adapters over it. The login and the refresh endpoint need no route, so they are HTTP
filters, in the HTTP part of the bundle; the check of the access token is its endpoint part.

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

## Wiring

The refresh token is an option of `JWTAuthenticationBundle`: assign `RefreshTokenStore` in
`TMVCJWTAuthenticationOptions` and the bundle has the login, the refresh URL and the check.

```pascal
uses
  MVCFramework.Filters.Authentication,
  MVCFramework.JWT.RefreshToken;

var
  // one store for the whole process: under WebBroker, ISAPI and Apache every web module
  // has an engine (and a bundle) of its own, and a token issued by one must be refreshed by the others
  gRefreshStore: IMVCRefreshTokenStore;

procedure TMyWebModule.WebModuleCreate(Sender: TObject);
var
  lOptions: TMVCJWTAuthenticationOptions;
begin
  lOptions := TMVCJWTAuthenticationOptions.Default;
  lOptions.Handler := TAuthenticationSample.Create;      // checks the credentials
  lOptions.Secret := JWT_SECRET;                         // or lOptions.Signer (RS256 & co.)
  lOptions.LoginURL := '/auth/login';
  lOptions.RefreshToken.Issuer := 'my-app';
  lOptions.RefreshToken.AccessTokenTTLSeconds := 15 * 60;
  lOptions.RefreshToken.RefreshTokenTTLSeconds := 7 * 24 * 60 * 60;
  lOptions.RefreshTokenStore := gRefreshStore;           // assigned: refresh token on

  MVC
    .AddController(TProtectedController)
    .Use(JWTAuthenticationBundle(lOptions));  // /auth/login, /auth/refresh, the check
end;

initialization
  gRefreshStore := TMVCInMemoryRefreshTokenStore.Create;
finalization
  gRefreshStore := nil;
```

- **Login** (`LoginURL`): the credentials go to `Handler.OnAuthentication`, as with the
  bundle without refresh token; when they are accepted the answer is the pair
  `{"access_token", "refresh_token", "token_type": "bearer", "expires_in"}`, the same body as
  the refresh URL. Wrong credentials: 401.
- **Refresh** (`RefreshToken.RefreshURLSegment`, `/auth/refresh` by default): `POST` with
  `{"refresh_token": ...}` rotates the token and answers a new pair.
- **Check**: the access token is verified on every action (engine-wide `Use`: controllers
  and Minimal API routes). The handler's `OnRequest` can make a controller action public.
- The bundle gives `RefreshToken` its own `Secret` (or `Signer`) and `HMACAlgorithm`: the
  access tokens of the login and of the refresh are signed the way the check verifies them.
- Both access tokens carry `username` and `roles` (what the store keeps), `RefreshToken.Issuer`,
  `iat`, `nbf`, a `jti` of their own and the lifetime `AccessTokenTTLSeconds`. `ClaimsSetup` must stay nil (the bundle
  raises otherwise), and the session data of the handler is not in the tokens.
- **Logout**: with the blacklist too (`OnAcceptToken` + `OnNewJWTToBlackList`), a request to
  `BlackListURL` (`/logout`) with the access token blacklists it and, when its JSON body is
  `{"refresh_token": ...}`, revokes that refresh token and its family (`IMVCRefreshTokenStore.Revoke`):
  the next refresh with it gets `401`. Without that body only the access token is logged out.
- **A login of your own** (an action that checks the credentials itself): `LoginURL := ''`
  leaves the login out of the bundle, which keeps the refresh URL and the check; the action
  answers the pair with `TMVCRefreshTokenCore.Create(lRefreshConfig, lStore).IssueTokenPair(...)`,
  on the same store and on a `TMVCRefreshTokenConfig` with the bundle's `Secret` (or `Signer`).
  The login must be outside the check: with the bundle on the engine a Minimal API route is
  always checked, so the login is a controller action that the handler's `OnRequest` makes
  public; or the bundle goes on the route group it protects and the login stays outside it.

With the Minimal API, the same bundle goes on the route group it protects; its login and its
refresh URL are HTTP filters, so they go on the engine:

```pascal
lEngine.Root.Prefix('/api').Use(JWTAuthenticationBundle(lOptions))
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
