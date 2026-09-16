-- wrk/wrk2 script: POD object (TBenchItem) serialized via RTTI
-- Exercises the generic object serializer path (opt#6 fast route),
-- unlike json_small which is a raw TJsonObject (opt#1 fast route).
wrk.method  = "GET"
wrk.path    = "/bench/pods/small"
wrk.headers["Accept"] = "application/json"
