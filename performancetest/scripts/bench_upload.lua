-- wrk/wrk2 script: 1 MB POST upload
-- Measures body read path and framework I/O overhead.
-- Body is generated once and reused across all requests.

local size = 1024 * 1024
local body = string.rep("A", size)

wrk.method  = "POST"
wrk.path    = "/bench/upload"
wrk.body    = body
wrk.headers["Content-Type"] = "application/octet-stream"
wrk.headers["Accept"]       = "application/json"
