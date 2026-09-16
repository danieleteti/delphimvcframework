-- wrk/wrk2 script: same tiny payload, traversing the full middleware chain
-- (CORS + Analytics + Compression). Compare vs /bench/health to quantify
-- middleware overhead per request.
wrk.method  = "GET"
wrk.path    = "/bench/heavy"
wrk.headers["Accept"] = "application/json"
wrk.headers["Accept-Encoding"] = "gzip, deflate"
