# Rate Limit Middleware Sample - In-Memory Storage

This sample demonstrates how to use the **TMVCRateLimitMiddleware** with in-memory storage to protect your API endpoints from abuse and overload.

## 📋 Table of Contents

- [Introduction](#introduction)
- [How Rate Limiting Works](#how-rate-limiting-works)
- [Configuration](#configuration)
- [Endpoints](#endpoints)
- [Build and Run](#build-and-run)
- [Testing](#testing)
- [HTTP Headers](#http-headers)
- [Key Types](#key-types)
- [In-Memory Storage](#in-memory-storage)
- [Practical Examples](#practical-examples)
- [Troubleshooting](#troubleshooting)

---

## 🎯 Introduction

**Rate Limiting** is a fundamental technique for:
- **Protecting APIs** from abuse and DoS attacks
- **Ensuring fair distribution** of resources among users
- **Preventing server overload**
- **Implementing usage quotas** for different service tiers

This sample implements a **Fixed Window** strategy with in-memory storage.

### Features

- ✅ **Fixed Window Rate Limiting**: 10 requests per 60 seconds per IP address
- ✅ **In-Memory Storage**: Zero external dependencies
- ✅ **Thread-Safe**: Uses `TCriticalSection` for concurrent access
- ✅ **Standard Headers**: Returns `X-RateLimit-*` headers per RFC 6585
- ✅ **Excluded Paths**: Health check endpoints bypass rate limiting
- ✅ **Custom Callbacks**: Configurable event handlers

## 🔍 How Rate Limiting Works

### Fixed Window Strategy

The **Fixed Window** strategy is the simplest rate limiting algorithm:

```
Window 1          Window 2          Window 3
[0-60 sec]        [60-120 sec]      [120-180 sec]
│                 │                 │
│ Req1: OK        │ Req1: OK        │
│ Req2: OK        │ Req2: OK        │
│ ...             │ ...             │
│ Req10: OK       │ Req10: OK       │
│ Req11: 429!     │ Req11: 429!     │
│ Req12: 429!     │                 │
└─────────────────└─────────────────└─────────────>
                                              time
```

**How it works**:
1. Each time window (e.g., 60 seconds) has a **request counter**
2. With each request, the counter is **incremented**
3. If the counter **exceeds the limit**, the request is **rejected** with HTTP 429
4. At the end of the window, the counter is **reset**

**Advantages**:
- ✅ Simple to implement and understand
- ✅ Excellent performance (~0.01ms overhead)
- ✅ Minimal memory usage
- ✅ Predictable behavior

**Disadvantages**:
- ⚠️ Possible "burst" at window boundaries
- ⚠️ Doesn't distribute requests uniformly over time

### Practical Example

Configuration: **10 requests per 60 seconds per IP**

```
Time (sec)  | Request   | Status       | Counter
------------|-----------|--------------|----------
00:00       | Req #1    | 200 OK       | 1/10
00:05       | Req #2    | 200 OK       | 2/10
00:10       | Req #3    | 200 OK       | 3/10
...
00:45       | Req #10   | 200 OK       | 10/10
00:50       | Req #11   | 429 BLOCKED  | 10/10
00:55       | Req #12   | 429 BLOCKED  | 10/10
01:00       | Req #1    | 200 OK       | 1/10  ← Reset!
```

### Middleware Flow

```
HTTP Request → Middleware → Extract Key (IP) → Check Storage
                                                      ↓
                                            [Count > Limit?]
                                            /              \
                                          YES              NO
                                           ↓                ↓
                                    Return 429         Increment
                                    (Rate Limited)     Pass to Controller
```

## ⚙️ Configuration

The middleware is configured in `WebModuleU.pas`:

```pascal
procedure TWebModule1.WebModuleCreate(Sender: TObject);
var
  LRateLimitMiddleware: TMVCRateLimitMiddleware;
begin
  FMVCEngine := TMVCEngine.Create(Self, ...);

  // Create rate limit middleware
  // 10 requests per 60 seconds per IP address
  LRateLimitMiddleware := TMVCRateLimitMiddleware.Create(
    10,           // Limit: max 10 requests
    60,           // Window: 60 seconds (1 minute)
    rlkIPAddress  // Key: rate limit by client IP address
  );

  // Exclude endpoints that should not be rate limited
  LRateLimitMiddleware.AddExcludedPath('/health');
  LRateLimitMiddleware.AddExcludedPath('/metrics');

  // Optional: callback when rate limit is exceeded
  LRateLimitMiddleware.SetOnRateLimitExceeded(
    procedure(const AContext: TWebContext; const AKey: string;
      const ALimit: Integer; const AWindowSeconds: Integer)
    begin
      LogW(Format('Rate limit exceeded for key: %s', [AKey]));
    end
  );

  // Add middleware to engine (BEFORE controllers!)
  FMVCEngine
    .AddMiddleware(LRateLimitMiddleware)  // ← Must be before controllers!
    .AddController(TMainController)
    .AddController(THealthController);
end;
```

### Configuration Parameters

| Parameter | Type | Description | Example |
|-----------|------|-------------|---------|
| `ALimit` | Integer | Maximum requests per window | `10` |
| `AWindowSeconds` | Integer | Time window in seconds | `60` |
| `AKeyType` | TRateLimitKeyType | Key extraction strategy | `rlkIPAddress` |

## 🔌 Endpoints

### Rate Limited Endpoints

These endpoints are limited to **10 requests per 60 seconds per IP**:

#### `GET /api/hello`
Simple welcome endpoint.

**Request**:
```bash
curl http://localhost:8080/api/hello
```

**Response (200 OK)**:
```json
{
  "message": "Hello World!",
  "timestamp": "2025-10-23 14:38:21",
  "rate_limit_info": "You have 9 requests remaining in this window"
}
```

#### `GET /api/protected`
Protected endpoint with complex data.

**Response (200 OK)**:
```json
{
  "message": "This is a protected endpoint with rate limiting",
  "timestamp": "2025-10-23 14:38:21",
  "data": ["item1", "item2", "item3"]
}
```

#### `GET /api/user/{username}`
User information endpoint.

**Response (200 OK)**:
```json
{
  "username": "john.doe",
  "full_name": "John Doe",
  "rate_limited": true
}
```

### Excluded Endpoints

These endpoints are **NOT** rate-limited:

#### `GET /health`
Health check for monitoring systems.

**Response (200 OK)**:
```json
{
  "status": "healthy",
  "timestamp": "2025-10-23 14:38:21"
}
```

## 🏗️ Build and Run

### Option 1: Build Script

```cmd
cd C:\DEV\dmvcframework\samples\middleware_ratelimit_memory
build.bat
```

### Option 2: Manual MSBuild

```cmd
call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"
cd C:\DEV\dmvcframework\samples\middleware_ratelimit_memory
msbuild RateLimitSample.dproj /t:Build /p:Config=Debug /p:Platform=Win32
```

### Run the Server

```cmd
cd bin
RateLimitSample.exe
```

**Expected Output**:
```
2025-10-23 14:38:21 [INFO] ** DMVCFramework Rate Limit Middleware Sample **
2025-10-23 14:38:21 [INFO] Starting HTTP Server on port 8080
2025-10-23 14:38:21 [INFO]
2025-10-23 14:38:21 [INFO] Rate Limit Configuration:
2025-10-23 14:38:21 [INFO]   - Limit: 10 requests per 60 seconds (1 minute)
2025-10-23 14:38:21 [INFO]   - Strategy: Fixed Window
2025-10-23 14:38:21 [INFO]   - Key: IP Address
2025-10-23 14:38:21 [INFO]
2025-10-23 14:38:21 [INFO] Press RETURN to stop the server
```

## 🧪 Testing

### Automated Test Script

Run the PowerShell test script:

```powershell
.\test_ratelimit.ps1
```

The script will:
1. Make 15 requests to `/api/hello`
2. Verify first 10 return **200 OK**
3. Verify next 5 return **429 Too Many Requests**
4. Test `/health` endpoint is not rate-limited (15/15 success)

### Manual Testing with curl

#### Single Request
```bash
curl -i http://localhost:8080/api/hello
```

#### Test Rate Limiting (Windows CMD)
```cmd
for /L %i in (1,1,15) do @curl -i http://localhost:8080/api/hello && @echo.
```

#### Test Rate Limiting (PowerShell)
```powershell
1..15 | ForEach-Object {
    $response = Invoke-WebRequest -Uri "http://localhost:8080/api/hello"
    Write-Host "Request $_: Status $($response.StatusCode)"
}
```

#### Test Excluded Endpoint
```bash
# This should never be rate-limited
for /L %i in (1,1,20) do @curl http://localhost:8080/health
```

### Expected Behavior

**Requests 1-10** (Success):
```http
HTTP/1.1 200 OK
X-RateLimit-Limit: 10
X-RateLimit-Remaining: 9
X-RateLimit-Reset: 2025-10-23T14:39:21Z
Content-Type: application/json

{...}
```

**Requests 11-15** (Rate Limited):
```http
HTTP/1.1 429 Too Many Requests
X-RateLimit-Limit: 10
X-RateLimit-Remaining: 0
X-RateLimit-Reset: 2025-10-23T14:39:21Z
Retry-After: 45
Content-Type: application/json

{
  "error": "Rate limit exceeded",
  "message": "Too many requests. Please try again later.",
  "limit": 10,
  "window_seconds": 60,
  "retry_after": 45
}
```

## 📊 HTTP Headers

### Request Headers

The middleware automatically reads:
- `X-Forwarded-For` - For IP addresses behind proxies
- `Authorization` - For rate limiting by API key
- Custom headers - If configured

### Response Headers (Success)

When a request **succeeds** (200 OK):

```http
HTTP/1.1 200 OK
X-RateLimit-Limit: 10
X-RateLimit-Remaining: 7
X-RateLimit-Reset: 2025-10-23T14:39:21Z
Content-Type: application/json
```

- `X-RateLimit-Limit`: Maximum requests allowed in the window
- `X-RateLimit-Remaining`: Requests remaining before limit
- `X-RateLimit-Reset`: ISO 8601 timestamp when the window resets

### Response Headers (Rate Limited)

When the limit is **exceeded** (429):

```http
HTTP/1.1 429 Too Many Requests
X-RateLimit-Limit: 10
X-RateLimit-Remaining: 0
X-RateLimit-Reset: 2025-10-23T14:39:21Z
Retry-After: 45
Content-Type: application/json
```

- `Retry-After`: Seconds to wait before retrying

## 🔑 Key Types

The middleware supports different key extraction strategies:

### 1. Rate Limit by IP Address (Default)

```pascal
TMVCRateLimitMiddleware.Create(10, 60, rlkIPAddress);
```

**Key**: Client IP address (e.g., `192.168.1.100`)

**Use Case**: General API protection, public APIs

**Pros**: Simple, no authentication required
**Cons**: Issues with NAT, proxies, VPNs

### 2. Rate Limit by User ID

```pascal
TMVCRateLimitMiddleware.Create(10, 60, rlkUserID);
```

**Key**: Authenticated username

**Use Case**: APIs with authentication, per-user quotas

**Pros**: Precise, fair per user
**Cons**: Requires authentication

### 3. Rate Limit by API Key

```pascal
TMVCRateLimitMiddleware.Create(100, 3600, rlkAPIKey);
```

**Key**: API key from `Authorization` header

**Use Case**: Public APIs with registration, tiered service

**Pros**: Allows differentiated tiers
**Cons**: API key management overhead

### 4. Rate Limit by Custom Header

```pascal
TMVCRateLimitMiddleware.Create(10, 60, rlkCustomHeader);
```

**Key**: Value from custom header (e.g., `X-Tenant-ID`)

**Use Case**: Multi-tenancy, organizations

## 💾 In-Memory Storage

### Characteristics

**Implementation**: `TMVCInMemoryRateLimitStorage`

**Pros**:
- ✅ **Zero external dependencies** - No database, no Redis
- ✅ **Maximum speed** - In-memory operations (~0.01ms)
- ✅ **Simple** - No additional configuration
- ✅ **Thread-safe** - Uses `TCriticalSection`
- ✅ **Automatic cleanup** - Removes expired keys

**Cons**:
- ❌ **Single-server only** - Not shared across multiple servers
- ❌ **Not persistent** - Resets on application restart
- ❌ **Limited memory** - Grows with number of unique keys

### When to Use

✅ **Ideal for**:
- Single-server deployments
- Internal applications
- Development and testing
- "Soft" limits not critical for business

❌ **Not suitable for**:
- Load balanced multi-server setups
- "Hard" business-critical limits
- Strict audit and compliance requirements

### For Multi-Server Deployments

For deployments with **load balancers** or **clusters**, use the Redis sample:
```
📁 samples/middleware_ratelimit_redis/
```

### Internal Implementation

```pascal
type
  TRateLimitEntry = record
    Count: Integer;
    WindowStart: TDateTime;
  end;

  TMVCInMemoryRateLimitStorage = class
  private
    FData: TDictionary<string, TRateLimitEntry>;
    FLock: TCriticalSection;  // Thread-safety
  public
    function CheckRateLimit(...): Boolean;
  end;
```

**Thread Safety**:
```pascal
FLock.Enter;
try
  // Thread-safe dictionary access
  if FData.TryGetValue(AKey, LEntry) then
    // ...
finally
  FLock.Leave;
end;
```

## 📚 Practical Examples

### Example 1: Different Limits for Different Endpoints

```pascal
// Generous rate limit for reads
LReadMiddleware := TMVCRateLimitMiddleware.Create(100, 60, rlkIPAddress);
LReadMiddleware.AddExcludedPath('/api/write');

// Strict rate limit for writes
LWriteMiddleware := TMVCRateLimitMiddleware.Create(10, 60, rlkIPAddress);
LWriteMiddleware.AddIncludedPath('/api/write');

FMVCEngine
  .AddMiddleware(LReadMiddleware)
  .AddMiddleware(LWriteMiddleware)
  .AddController(TMainController);
```

### Example 2: Rate Limit by Service Tier

```pascal
// Determine limit based on user tier
function GetLimitForUser(const AUserID: string): Integer;
begin
  case GetUserTier(AUserID) of
    utFree: Result := 10;         // 10 req/min
    utPro: Result := 100;         // 100 req/min
    utEnterprise: Result := 1000; // 1000 req/min
  end;
end;
```

### Example 3: Combining IP + User Rate Limits

```pascal
// First check IP (anti-DoS)
LIPMiddleware := TMVCRateLimitMiddleware.Create(1000, 60, rlkIPAddress);

// Then check User (quota)
LUserMiddleware := TMVCRateLimitMiddleware.Create(100, 60, rlkUserID);

FMVCEngine
  .AddMiddleware(LIPMiddleware)   // First: IP protection
  .AddMiddleware(LUserMiddleware) // Second: User quota
  .AddController(TMainController);
```

## 🔧 Troubleshooting

### Problem: Rate limit not working

**Symptom**: All requests pass even after exceeding limit

**Solution**: Verify middleware is added **before** controllers:
```pascal
FMVCEngine
  .AddMiddleware(LRateLimitMiddleware)  // ← Before!
  .AddController(TMainController);      // ← After!
```

### Problem: All requests are blocked

**Symptom**: Even the first request returns 429

**Cause**: Incorrect configuration

**Solution**: Verify parameters are > 0:
```pascal
LRateLimitMiddleware := TMVCRateLimitMiddleware.Create(
  10,   // Must be > 0
  60,   // Must be > 0
  rlkIPAddress
);
```

### Problem: Headers not appearing

**Symptom**: `X-RateLimit-*` headers are missing

**Solution**: Use `curl -i` or check in browser dev tools:
```bash
curl -i http://localhost:8080/api/hello
```

## 📖 References

- [DMVCFramework Documentation](https://github.com/danieleteti/delphimvcframework)
- [RFC 6585 - HTTP 429 Too Many Requests](https://tools.ietf.org/html/rfc6585)
- [Rate Limiting Patterns](https://cloud.google.com/architecture/rate-limiting-strategies-techniques)
- [Redis Storage Sample](../middleware_ratelimit_redis/) - For distributed deployments

## 📄 License

This sample is part of **DMVCFramework** and is released under **Apache License 2.0**.
