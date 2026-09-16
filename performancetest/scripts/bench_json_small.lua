-- wrk/wrk2 script: fixed-size small JSON
-- Measures serializer warm-path cost with a representative REST payload.
wrk.method  = "GET"
wrk.path    = "/bench/json/small"
wrk.headers["Accept"] = "application/json"
