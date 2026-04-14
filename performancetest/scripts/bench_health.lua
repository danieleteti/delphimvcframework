-- wrk/wrk2 script: tiny JSON health check
-- Measures pure framework overhead on the shortest possible path.
wrk.method  = "GET"
wrk.path    = "/bench/health"
wrk.headers["Accept"] = "application/json"
