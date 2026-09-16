-- wrk/wrk2 script: 100-record JSON array (~20 KB)
-- Measures serializer under sustained object-graph load.
wrk.method  = "GET"
wrk.path    = "/bench/json/large"
wrk.headers["Accept"] = "application/json"
