# JWT HTTP-Only Cookie Authentication Sample

This sample demonstrates how to use the new `TMVCJWTCookieAuthenticationMiddleware` for secure JWT authentication using HTTP-only cookies in DelphiMVCFramework.

## Security Features

The middleware provides **secure defaults**:

| Feature | Default | Description |
|---------|---------|-------------|
| HttpOnly | `True` (always) | Cookie cannot be accessed by JavaScript (XSS protection) |
| Secure | `True` | Cookie only sent over HTTPS |
| SameSite | `Strict` | Cookie not sent with cross-site requests (CSRF protection) |

## Quick Start

```pascal
uses
  MVCFramework.Middleware.JWT;

// Add the middleware with secure defaults
Engine.AddMiddleware(
  UseJWTCookieAuthentication(
    TMyAuthHandler.Create,
    procedure(const JWT: TJWT)
    begin
      JWT.Claims.ExpirationTime := Now + OneHour;
    end,
    'my-secret-key-change-in-production',
    '/login',
    '/logout'
  )
);
```

## Configuration

### For Local Development (HTTP)

During development over HTTP, disable the Secure flag:

```pascal
Engine.AddMiddleware(
  UseJWTCookieAuthentication(...)
    .SetCookieSecure(False)  // Only for HTTP development!
);
```

### Full Configuration Options

```pascal
Engine.AddMiddleware(
  UseJWTCookieAuthentication(
    TMyAuthHandler.Create,
    procedure(const JWT: TJWT)
    begin
      JWT.Claims.ExpirationTime := Now + OneHour;
    end,
    'my-secret-key',
    '/login',
    '/logout',
    [TJWTCheckableClaim.ExpirationTime, TJWTCheckableClaim.NotBefore],
    300  // Leeway seconds
  )
  .SetCookieName('my_jwt')        // Default: 'jwt_token'
  .SetCookieSecure(True)          // Default: True (requires HTTPS)
  .SetCookieSameSite(ssStrict)    // Default: ssStrict (options: ssStrict, ssLax, ssNone)
  .SetCookiePath('/')             // Default: '/'
  .SetCookieDomain('example.com') // Default: '' (current domain)
);
```

## How HTTP-Only Cookies Work

### Classic HTTP Requests (Browser Navigation)

When you click a link or submit a form, the browser **automatically** includes all cookies for that domain:

```html
<!-- Cookie is sent automatically -->
<a href="/admin/profile">My Profile</a>
<form action="/admin/update" method="POST">...</form>
```

No JavaScript involved - the cookie is sent in the `Cookie` HTTP header.

### JavaScript (fetch/XHR)

You **cannot** read the cookie value with `document.cookie` (HttpOnly protection against XSS).

But the browser still sends it automatically if you use `credentials`:

```javascript
// Same-origin request
fetch('/api/data', {
  credentials: 'same-origin'  // Sends cookies for same domain
});

// Cross-origin request
fetch('https://api.example.com/data', {
  credentials: 'include'  // Sends cookies cross-origin
});
```

### HTMX

HTMX uses `XMLHttpRequest` internally, which sends cookies automatically for same-origin requests:

```html
<!-- No configuration needed for same-origin -->
<button hx-get="/admin/profile" hx-target="#result">
  Load Profile
</button>

<div hx-get="/admin/dashboard" hx-trigger="load">
  Loading...
</div>
```

For cross-origin requests, add the credentials configuration:

```html
<button hx-get="https://api.example.com/data"
        hx-request='{"credentials": "include"}'
        hx-target="#result">
  Load Cross-Origin Data
</button>
```

## HTMX Complete Example

```html
<!DOCTYPE html>
<html>
<head>
    <script src="https://unpkg.com/htmx.org@1.9.10"></script>
</head>
<body>
    <!-- Login form -->
    <form hx-post="/login" hx-target="#status">
        <input type="text" name="username" placeholder="Username">
        <input type="password" name="password" placeholder="Password">
        <button type="submit">Login</button>
    </form>

    <!-- Status display -->
    <div id="status"></div>

    <!-- Protected content - cookie sent automatically -->
    <div hx-get="/admin/profile"
         hx-trigger="click from:#loadProfile"
         hx-target="#profile">
    </div>
    <button id="loadProfile">Load Profile</button>
    <div id="profile"></div>

    <!-- Logout -->
    <button hx-get="/logout" hx-target="#status">Logout</button>
</body>
</html>
```

## Verifying Cookies with Browser DevTools

### Chrome/Edge

1. Open DevTools (F12 or Ctrl+Shift+I)
2. Go to **Application** tab
3. In the left sidebar, expand **Storage** > **Cookies**
4. Click on your domain (e.g., `localhost`)
5. Look for `jwt_token` cookie

Verify these attributes:
- **HttpOnly**: Should show a checkmark
- **Secure**: Should show a checkmark (in production)
- **SameSite**: Should show `Strict`

### Firefox

1. Open DevTools (F12)
2. Go to **Storage** tab
3. Expand **Cookies** in the left sidebar
4. Click on your domain
5. Check the cookie attributes in the table

### Network Tab Inspection

1. Open DevTools > **Network** tab
2. Make a request to a protected endpoint
3. Click on the request
4. Check **Request Headers** for `Cookie: jwt_token=...`
5. Check **Response Headers** for `Set-Cookie` after login

## Cross-Origin Configuration

For cross-origin requests (API on different domain), you need:

### Server Configuration (Delphi)

```pascal
// Enable CORS
Engine.AddMiddleware(
  TMVCCORSMiddleware.Create(
    'https://myapp.com',     // Allowed origin
    'GET,POST,PUT,DELETE',   // Allowed methods
    'Content-Type'           // Allowed headers
  )
);

// JWT Cookie with cross-origin settings
Engine.AddMiddleware(
  UseJWTCookieAuthentication(...)
    .SetCookieSecure(True)      // Required for cross-origin
    .SetCookieSameSite(ssNone)  // Allow cross-site requests
    .SetCookieDomain('.example.com')  // Share across subdomains
);
```

**Important**: `SameSite=None` requires `Secure=True`. The middleware validates this and raises an exception if you try to set `SameSite=None` with `Secure=False`.

### Client Configuration (JavaScript)

```javascript
fetch('https://api.example.com/data', {
  method: 'GET',
  credentials: 'include',  // Required for cross-origin cookies
  headers: {
    'Content-Type': 'application/json'
  }
});
```

### HTMX Cross-Origin

```html
<meta name="htmx-config" content='{"withCredentials": true}'>
<!-- or per-request -->
<button hx-get="https://api.example.com/data"
        hx-request='{"credentials": "include"}'>
  Load
</button>
```

## Token Refresh

When a token is refreshed (if `LiveValidityWindowInSeconds` is configured), the middleware:

1. Sets a new cookie with the refreshed token
2. Adds header `X-JWT-Refreshed: true` to the response

You can check for this header in JavaScript:

```javascript
const response = await fetch('/api/data', { credentials: 'same-origin' });
if (response.headers.get('X-JWT-Refreshed') === 'true') {
  console.log('Token was refreshed');
}
```

## Test Users

This sample includes these test credentials:

| Username | Password | Roles |
|----------|----------|-------|
| user1 | user1 | role1 |
| user2 | user2 | role2 |
| user3 | user3 | role1, role2 |
| admin | admin | role1, role2, admin |

## Endpoints

| Endpoint | Method | Auth Required | Description |
|----------|--------|---------------|-------------|
| `/login` | POST | No | Authenticate and receive JWT cookie |
| `/logout` | GET | No | Invalidate JWT cookie |
| `/status` | GET | No | Check authentication status |
| `/public` | GET | No | Public content |
| `/admin/profile` | GET | Yes (any role) | User profile |
| `/admin/role1` | GET | Yes (role1) | Content for role1 users |
| `/admin/role2` | GET | Yes (role2) | Content for role2 users |

## Running the Sample

1. Open `JWTCookieServer.dproj` in Delphi IDE
2. Build and run (F9)
3. Open browser to `http://localhost:8080`
4. Use the web interface to test login/logout and protected endpoints

## Security Considerations

1. **Always use HTTPS in production** - The `Secure` flag should be `True`
2. **Keep `SameSite=Strict`** unless you need cross-site requests
3. **Use a strong secret key** - Change the default in production
4. **Set appropriate expiration times** - Balance security and usability
5. **Consider token blacklisting** - For logout invalidation before expiration

## Migration from Old Middleware

If you're using the old `TMVCJWTAuthenticationMiddleware` with HTTP-only cookies:

```pascal
// Old (deprecated)
Engine.AddMiddleware(
  TMVCJWTAuthenticationMiddleware.Create(
    AuthHandler, ClaimsSetup, True, // HTTP-only flag
    ...
  )
);

// New (recommended)
Engine.AddMiddleware(
  UseJWTCookieAuthentication(
    AuthHandler, ClaimsSetup, Secret,
    '/login', '/logout', Claims, Leeway
  )
  .SetCookieSecure(False)  // Only if needed for HTTP dev
);
```

The new middleware provides:
- Secure defaults (Secure=True, SameSite=Strict)
- Fluent configuration API
- Proper cookie invalidation on logout
- Token refresh header notification
- SameSite validation (None requires Secure)
