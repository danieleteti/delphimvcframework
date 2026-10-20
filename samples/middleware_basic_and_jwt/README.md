# HTTP Basic and JWT on the same engine

A server that accepts both `Authorization: Basic ...` and `Authorization: Bearer <token>` on the same routes.

Both `TMVCBasicAuthenticationMiddleware` and `TMVCJWTAuthenticationMiddleware` ask the handler's `OnRequest`
whether a request needs authentication. Give each middleware its own handler ([AuthHandlerU.pas](AuthHandlerU.pas)):

- the Basic handler answers *yes* only for requests that carry `Authorization: Basic ...`;
- the JWT handler answers *yes* for all the others, so a Bearer token is verified and a request with no credentials
  gets a 401.

Each middleware takes the requests meant for it and leaves the rest alone. The action
([SecureControllerU.pas](SecureControllerU.pas)) is the same for both: it reads `Context.LoggedUser`.

## Try it

Start `BasicAndJWT.exe` (port 8099), then:

```
# Basic
curl -u daniele:secret http://localhost:8099/api/secure

# a token from Basic credentials, then the token
curl -X POST -u daniele:secret http://localhost:8099/login
curl -H "Authorization: Bearer <token>" http://localhost:8099/api/secure

# no credentials, wrong password, malformed token: all 401
curl -i http://localhost:8099/api/secure
curl -i -u daniele:wrong http://localhost:8099/api/secure
curl -i -H "Authorization: Bearer not.a.token" http://localhost:8099/api/secure
```

## Notes

- The Basic middleware keeps the login in a session (`UseMemorySessionMiddleware`). A client that keeps the session
  cookie after a Basic request can then send a Bearer token: the token replaces the identity the session carried.
- The credentials and the JWT secret are demo values. Use your own user store, and read the secret from the
  environment (`dotEnv`).
- `MultiAuthTestsU` in `unittests/general/TestClient` runs the same combination as an automated test.
