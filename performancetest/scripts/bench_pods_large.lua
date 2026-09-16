-- wrk/wrk2 script: TObjectList<TBenchItem> serialized via RTTI
-- Exercises the serializer tree-build + UTF-8 output path under sustained
-- object-graph load. Pairs with json_large to isolate RTTI cost.
wrk.method  = "GET"
wrk.path    = "/bench/pods/large"
wrk.headers["Accept"] = "application/json"
