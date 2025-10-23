# Rate Limit Middleware Sample - Redis Storage

This sample demonstrates how to use the **TMVCRateLimitMiddleware** with **Redis storage** for **distributed rate limiting** across multiple server instances.

## 📋 Table of Contents

- [Introduction](#introduction)
- [Why Redis?](#why-redis)
- [How It Works](#how-it-works)
- [Prerequisites](#prerequisites)
- [Configuration](#configuration)
- [Build and Run](#build-and-run)
- [Endpoints](#endpoints)
- [Testing](#testing)
- [Redis Monitoring](#redis-monitoring)
- [Multi-Server Deployment](#multi-server-deployment)
- [Redis Keys Format](#redis-keys-format)
- [Production Considerations](#production-considerations)

---

## 🎯 Introduction

This sample implements **distributed rate limiting** using Redis as shared storage. Unlike in-memory storage (which only works on a single server), Redis allows you to:

- ✅ **Share counters** across multiple server instances
- ✅ **Persist limits** even after server restart
- ✅ **Scale horizontally** with load balancers
- ✅ **Atomic operations** with Redis INCR
- ✅ **High availability** with Redis Sentinel/Cluster

### Features

- **Redis Storage**: Uses `TMVCRedisRateLimitStorage` for distributed rate limiting
- **Scalable**: Works seamlessly across multiple server instances
- **Atomic Operations**: Uses Redis INCR for thread-safe counting
- **Fail-Open**: Allows requests if Redis is unavailable (configurable)
- **Standard Headers**: Returns `X-RateLimit-*` headers per RFC 6585
- **Auto-expiry**: Keys automatically expire using Redis TTL

## 🚀 Why Redis?

### Problem: Single-Server Limitation

With in-memory storage, each server has its own counter:

```
Load Balancer
      │
      ├─────────────┬─────────────┐
      │             │             │
  Server A      Server B      Server C
  Counter: 5    Counter: 7    Counter: 3

  ⚠️ Limit: 10 req/min per IP
  ⚠️ But user can make 30 total requests! (10 + 10 + 10)
```

### Solution: Shared Redis Storage

With Redis, all servers share the same counter:

```
           Load Balancer
                 │
      ┌──────────┼──────────┐
      │          │          │
  Server A   Server B   Server C
      │          │          │
      └──────────┼──────────┘
                 │
            ┌────▼────┐
            │  Redis  │
            │Counter:15│ ✅ Limit enforced!
            └─────────┘
```

### Redis vs In-Memory Comparison

| Feature | In-Memory | Redis |
|---------|-----------|-------|
| Multi-server | ❌ No | ✅ Yes |
| Persistent | ❌ No | ✅ Yes |
| Atomicity | ⚠️ TCriticalSection | ✅ Atomic INCR |
| Scalability | ❌ Limited | ✅ Unlimited |
| Fail-over | ❌ No | ✅ Sentinel/Cluster |
| External Dependencies | ✅ None | ⚠️ Redis Required |

## 🔍 How It Works

### Redis INCR Command

The middleware uses Redis's atomic `INCR` command:

```redis
# First request from IP 192.168.1.100
SET ratelimit:0:192.168.1.100 1
EXPIRE ratelimit:0:192.168.1.100 60

# Second request
INCR ratelimit:0:192.168.1.100  → Returns 2

# Third request
INCR ratelimit:0:192.168.1.100  → Returns 3

# ... 8 more requests ...

# 11th request
INCR ratelimit:0:192.168.1.100  → Returns 11 ⚠️ Over limit!
# Server returns HTTP 429

# After 60 seconds
# Key expires automatically
```

### Flow Diagram

```
HTTP Request → Middleware → Extract Key → Redis
                                            ↓
                                      INCR counter
                                            ↓
                                    [counter > limit?]
                                    /              \
                                  YES              NO
                                   ↓                ↓
                          Return 429          Pass to Controller
                          Set Retry-After     Add Rate Limit Headers
```

### Atomicity Guarantee

Redis `INCR` is atomic, ensuring thread-safety across:
- Multiple threads in the same server
- Multiple server instances
- Concurrent requests from different clients

No race conditions possible!

## 🔧 Prerequisites

### 1. Install Redis

**Windows (using Chocolatey)**:
```bash
choco install redis-64
```

**Windows (manual download)**:
https://github.com/microsoftarchive/redis/releases

**Docker**:
```bash
docker run -d -p 6379:6379 --name redis redis:latest
```

**Linux (Ubuntu/Debian)**:
```bash
sudo apt-get update
sudo apt-get install redis-server
sudo systemctl start redis-server
```

**Linux (RHEL/CentOS)**:
```bash
sudo yum install redis
sudo systemctl start redis
```

### 2. Verify Redis is Running

```bash
redis-cli ping
```

Expected response: `PONG`

### 3. DelphiRedisClient Library

This sample requires the **DelphiRedisClient** library:

```bash
git clone --recursive https://github.com/danieleteti/delphiredisclient
```

Add to your Delphi library path:
```
C:\DEV\delphiredisclient\sources
```

## ⚙️ Configuration

The middleware is configured in `WebModuleU.pas`:

```pascal
procedure TWebModule1.WebModuleCreate(Sender: TObject);
var
  LRateLimitMiddleware: TMVCRateLimitMiddleware;
  LRedisStorage: IMVCRateLimitStorage;
begin
  FMVCEngine := TMVCEngine.Create(Self, ...);

  // Create Redis storage
  LRedisStorage := TMVCRedisRateLimitStorage.Create(
    '127.0.0.1',  // Redis host
    6379,         // Redis port
    '',           // Redis password (empty if no auth)
    'ratelimit:'  // Key prefix in Redis
  );

  // Create rate limit middleware with Redis storage
  // 20 requests per 120 seconds (2 minutes) per IP address
  LRateLimitMiddleware := TMVCRateLimitMiddleware.Create(
    20,            // Max requests
    120,           // Window in seconds
    rlkIPAddress,  // Rate limit by IP address
    LRedisStorage  // Use Redis storage
  );

  // Exclude health check from rate limiting
  LRateLimitMiddleware.AddExcludedPath('/health');
  LRateLimitMiddleware.AddExcludedPath('/metrics');

  // Optional: callback when rate limit is exceeded
  LRateLimitMiddleware.SetOnRateLimitExceeded(
    procedure(const AContext: TWebContext; const AKey: string;
      const ALimit: Integer; const AWindowSeconds: Integer)
    begin
      LogW(Format('[Redis] Rate limit exceeded for key: %s', [AKey]));
    end
  );

  // Add middleware to engine
  FMVCEngine
    .AddMiddleware(LRateLimitMiddleware)
    .AddController(TMainController)
    .AddController(THealthController);
end;
```

### Configuration Parameters

**TMVCRedisRateLimitStorage**:

| Parameter | Type | Description | Example |
|-----------|------|-------------|---------|
| `ARedisHost` | string | Redis server hostname | `'127.0.0.1'` |
| `ARedisPort` | Integer | Redis server port | `6379` |
| `ARedisPassword` | string | Redis AUTH password | `''` (empty) |
| `AKeyPrefix` | string | Prefix for Redis keys | `'ratelimit:'` |

## 🏗️ Build and Run

### Option 1: Build Script

```cmd
cd C:\DEV\dmvcframework\samples\middleware_ratelimit_redis
build.bat
```

### Option 2: Manual MSBuild

```cmd
call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"
cd C:\DEV\dmvcframework\samples\middleware_ratelimit_redis
msbuild RateLimitRedisSample.dproj /t:Build /p:Config=Debug /p:Platform=Win32
```

### Run the Server

**Important**: Start Redis first!

```cmd
# Terminal 1: Start Redis
redis-server

# Terminal 2: Start the application
cd bin
RateLimitRedisSample.exe
```

**Expected Output**:
```
2025-10-23 14:43:52 [INFO] ** DMVCFramework Rate Limit Middleware Sample (Redis) **
2025-10-23 14:43:52 [INFO] Starting HTTP Server on port 8080
2025-10-23 14:43:52 [INFO]
2025-10-23 14:43:52 [INFO] Rate Limit Configuration:
2025-10-23 14:43:52 [INFO]   - Storage: Redis (127.0.0.1:6379)
2025-10-23 14:43:52 [INFO]   - Limit: 20 requests per 120 seconds (2 minutes)
2025-10-23 14:43:52 [INFO]   - Strategy: Fixed Window with Redis
2025-10-23 14:43:52 [INFO]   - Key: IP Address
2025-10-23 14:43:52 [INFO]
2025-10-23 14:43:52 [INFO] Monitor Redis:
2025-10-23 14:43:52 [INFO]   redis-cli MONITOR
2025-10-23 14:43:52 [INFO]   redis-cli KEYS "ratelimit:*"
2025-10-23 14:43:52 [INFO]
2025-10-23 14:43:52 [INFO] Press RETURN to stop the server
```

## 🔌 Endpoints

### Rate Limited Endpoints

These endpoints are limited to **20 requests per 120 seconds per IP**:

#### `GET /api/hello`
Simple hello endpoint.

**Request**:
```bash
curl -i http://localhost:8080/api/hello
```

**Response (200 OK)**:
```json
{
  "message": "Hello from Redis Rate Limited API!",
  "timestamp": "2025-10-23 14:43:52",
  "server_id": "ServerA"
}
```

#### `GET /api/data`
Data listing endpoint.

**Response (200 OK)**:
```json
{
  "data": [
    {"id": "1", "name": "Item 1"},
    {"id": "2", "name": "Item 2"},
    ...
  ],
  "count": 10
}
```

#### `POST /api/submit`
Data submission endpoint.

**Request**:
```json
{
  "name": "Test Item",
  "value": 123
}
```

**Response (200 OK)**:
```json
{
  "success": true,
  "id": "generated-id",
  "message": "Data submitted successfully"
}
```

### Excluded Endpoints

#### `GET /health`
Health check (not rate-limited).

**Response (200 OK)**:
```json
{
  "status": "healthy",
  "redis_connected": true,
  "timestamp": "2025-10-23 14:43:52"
}
```

## 🧪 Testing

### Single Request Test

```bash
curl -i http://localhost:8080/api/hello
```

### Rate Limiting Test (Windows CMD)

```cmd
# Make 25 requests (limit is 20)
for /L %i in (1,1,25) do @curl -i http://localhost:8080/api/hello && @echo Request %i completed
```

### Rate Limiting Test (PowerShell)

```powershell
1..25 | ForEach-Object {
    try {
        $response = Invoke-WebRequest -Uri "http://localhost:8080/api/hello"
        Write-Host "Request $_: SUCCESS (200)" -ForegroundColor Green
    }
    catch {
        if ($_.Exception.Response.StatusCode -eq 429) {
            Write-Host "Request $_: RATE LIMITED (429)" -ForegroundColor Red
        }
    }
}
```

### Testing Across Multiple Servers

1. Start Redis:
   ```bash
   redis-server
   ```

2. Start multiple server instances on different ports:
   ```cmd
   # Terminal 1
   set PORT=8080
   RateLimitRedisSample.exe

   # Terminal 2
   set PORT=8081
   RateLimitRedisSample.exe

   # Terminal 3
   set PORT=8082
   RateLimitRedisSample.exe
   ```

3. Make requests to different servers:
   ```bash
   curl http://localhost:8080/api/hello  # Server 1
   curl http://localhost:8081/api/hello  # Server 2
   curl http://localhost:8082/api/hello  # Server 3
   ```

4. All requests count toward the same limit!

## 📊 Redis Monitoring

### Monitor All Redis Commands

```bash
redis-cli MONITOR
```

You'll see output like:
```
"INCR" "ratelimit:0:192.168.1.100"
"TTL" "ratelimit:0:192.168.1.100"
"EXPIRE" "ratelimit:0:192.168.1.100" "120"
```

### List All Rate Limit Keys

```bash
redis-cli KEYS "ratelimit:*"
```

Output:
```
1) "ratelimit:0:192.168.1.100"
2) "ratelimit:0:192.168.1.101"
3) "ratelimit:0:10.0.0.55"
```

### Get Current Count for a Key

```bash
redis-cli GET "ratelimit:0:192.168.1.100"
```

Output: `"15"` (15 requests made)

### Get Remaining TTL

```bash
redis-cli TTL "ratelimit:0:192.168.1.100"
```

Output: `(integer) 87` (87 seconds remaining)

### Reset Rate Limit for an IP

```bash
# Delete specific key
redis-cli DEL "ratelimit:0:192.168.1.100"

# Or pattern delete
redis-cli --scan --pattern "ratelimit:*" | xargs redis-cli DEL
```

### Clear All Rate Limit Keys

```bash
redis-cli EVAL "return redis.call('del', unpack(redis.call('keys', ARGV[1])))" 0 "ratelimit:*"
```

## 🌐 Multi-Server Deployment

Redis enables true distributed rate limiting:

```
                  ┌─────────────┐
                  │Load Balancer│
                  │ (HAProxy)   │
                  └──────┬──────┘
                         │
         ┌───────────────┼───────────────┐
         │               │               │
    ┌────▼───┐      ┌───▼────┐     ┌───▼────┐
    │Server A│      │Server B│     │Server C│
    │ :8080  │      │ :8081  │     │ :8082  │
    └────┬───┘      └───┬────┘     └───┬────┘
         │              │              │
         └──────────────┼──────────────┘
                        │
                   ┌────▼────┐
                   │  Redis  │
                   │ :6379   │
                   └─────────┘

All servers share the same rate limit state!
```

### Example: HAProxy Configuration

```haproxy
backend api_servers
    balance roundrobin
    server server1 127.0.0.1:8080 check
    server server2 127.0.0.1:8081 check
    server server3 127.0.0.1:8082 check
```

### Example: Nginx Configuration

```nginx
upstream api_backend {
    server 127.0.0.1:8080;
    server 127.0.0.1:8081;
    server 127.0.0.1:8082;
}

server {
    listen 80;
    location / {
        proxy_pass http://api_backend;
    }
}
```

## 🔑 Redis Keys Format

Keys are stored with this format:
```
{prefix}:{keytype}:{identifier}
```

### Examples

**By IP Address** (`rlkIPAddress`):
```
ratelimit:0:192.168.1.100
ratelimit:0:10.0.0.55
```

**By User ID** (`rlkUserID`):
```
ratelimit:1:john.doe
ratelimit:1:jane.smith
```

**By API Key** (`rlkAPIKey`):
```
ratelimit:2:abc123xyz789
ratelimit:2:key-prod-001
```

**By Custom Header** (`rlkCustomHeader`):
```
ratelimit:3:tenant-acme-corp
ratelimit:3:org-123456
```

### Key Lifecycle

1. **First Request**: Key created with value `1`, TTL set to window duration
2. **Subsequent Requests**: Key incremented atomically
3. **After Window**: Key expires automatically (Redis TTL)
4. **Next Window**: New key created

## 🏭 Production Considerations

### 1. Redis High Availability

Use **Redis Sentinel** for automatic failover:

```bash
# Start Redis with Sentinel
redis-server --port 6379
redis-sentinel sentinel.conf
```

Or use **Redis Cluster** for sharding:

```bash
redis-cli --cluster create \
  127.0.0.1:7000 127.0.0.1:7001 127.0.0.1:7002 \
  --cluster-replicas 1
```

### 2. Redis Security

**Enable AUTH**:
```bash
# redis.conf
requirepass your_strong_password_here
```

**Use TLS**:
```bash
# redis.conf
tls-port 6380
tls-cert-file /path/to/cert.pem
tls-key-file /path/to/key.pem
```

### 3. Redis Persistence

**RDB (snapshots)**:
```bash
# redis.conf
save 900 1      # Save after 900 sec if 1 key changed
save 300 10     # Save after 300 sec if 10 keys changed
save 60 10000   # Save after 60 sec if 10000 keys changed
```

**AOF (append-only file)**:
```bash
# redis.conf
appendonly yes
appendfsync everysec
```

### 4. Monitoring

Monitor Redis metrics:
- **Memory usage**: `redis-cli INFO memory`
- **Operations/sec**: `redis-cli INFO stats`
- **Connected clients**: `redis-cli INFO clients`
- **Slow commands**: `redis-cli SLOWLOG GET 10`

### 5. Connection Pooling

For production, implement Redis connection pooling to reduce overhead.

### 6. Fail-Over Behavior

The middleware is configured to **fail-open** by default:
- If Redis is unavailable → Requests are allowed
- Prevents Redis outage from taking down your API

To change to **fail-closed** (block on Redis error):
```pascal
// Customize CheckRateLimit to raise exception on Redis error
```

## 🔧 Troubleshooting

### Error: "Could not connect to Redis"

**Check if Redis is running**:
```bash
redis-cli ping
```

Should return: `PONG`

**Check Redis is listening on correct port**:
```bash
netstat -an | findstr 6379  # Windows
netstat -an | grep 6379     # Linux
```

**Check firewall**:
```bash
# Windows
netsh advfirewall firewall add rule name="Redis" dir=in action=allow protocol=TCP localport=6379

# Linux
sudo ufw allow 6379/tcp
```

### Problem: Rate limit not enforced across instances

**Verify all servers use same Redis**:
```bash
# On each server
redis-cli -h your-redis-host ping
```

**Check key prefix is consistent**:
```pascal
// Must be same on all servers
LRedisStorage := TMVCRedisRateLimitStorage.Create(
  'redis-host',
  6379,
  '',
  'ratelimit:'  // ← Same prefix!
);
```

**Verify keys are created**:
```bash
redis-cli KEYS "ratelimit:*"
```

### Problem: Performance issues

**Monitor Redis latency**:
```bash
redis-cli --latency
```

**Check Redis is on same network**:
- Use local Redis or same datacenter
- Latency should be < 1ms

**Enable Redis pipelining** (future enhancement).

## 📖 References

- [DMVCFramework Documentation](https://github.com/danieleteti/delphimvcframework)
- [Redis Documentation](https://redis.io/documentation)
- [DelphiRedisClient](https://github.com/danieleteti/delphiredisclient)
- [RFC 6585 - HTTP 429](https://tools.ietf.org/html/rfc6585)
- [Redis INCR Command](https://redis.io/commands/incr)
- [In-Memory Storage Sample](../middleware_ratelimit_memory/) - For single-server deployments

## 📄 License

This sample is part of **DMVCFramework** and is released under **Apache License 2.0**.
