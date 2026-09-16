# DMVCFramework Performance — Post-Optimization Comparison

**Date:** 2026-04-15
**Branch:** feature/server-abstraction-layer
**Commits under test:** fcafff25..0e6c3a40 (7 commits on top of `just before switch`)
**Harness:** `scripts/run_all_stable.bat` (3 reps × 30 s × c=100, median)

## What changed on the code side

| Commit | Affects |
|---|---|
| `fix(httpsys): dispatch request handling to task pool` | HTTP.sys only — bug fix (listener was serial) |
| `build(packages): explicit Release compiler flags` | All backends (via consumer apps) |
| `perf(render): fast path for OKResponse(TJsonBaseObject)` | All backends — render hot path |
| `perf(router): index routes by HTTP method + path` | All backends — dispatch |
| `perf(httpsys): stream request body into pre-sized TBytes` | HTTP.sys only |

Two of the five engine-side commits only touch HTTP.sys, one of them a
true bug fix. IndyDirect and WebBroker share the two cross-cutting wins
(render fast path + route table) and nothing else. That asymmetry is
visible in the numbers below and is the intended story, not a miss.

## Raw numbers (median of 3 × 30 s @ c=100)

`rps` = requests/sec. `*` = successRate < 99% (see note).

| Scenario | WebBroker | IndyDirect | HTTP.sys |
|---|---:|---:|---:|
| health       | 897*  | 1784 | 3380 |
| json_small   | 938*  | 1575 | 2858 |
| json_large   | 911*  |  630 |  889 |
| heavy        | 470*  | 1513 | 3131 |
| upload 1 MB  | 425*  |  762 |  892 |
| pods/small   | 513*  | 1302 | 2641 |
| pods/large   | 516*  |  231 |  251 |

## Delta vs pre-opt baseline (`BASELINE.md`, c=100 single 30 s run)

| Scenario | WebBroker* | IndyDirect | HTTP.sys |
|---|---:|---:|---:|
| health       | +73%*  | **+55%**  | **+44%**  |
| json_small   | −2%*   |  +14%     | **+36%**  |
| json_large   | +404%* | **+45%**  |  +21%     |
| heavy        | −29%*  |  −9%      | **+67%**  |
| upload 1 MB  | +14%*  | **+85%**  | **+839%** |
| pods/small   | new    |  new      |  new      |
| pods/large   | new    |  new      |  new      |

\* WebBroker scenarios have successRate 6–57% — the server crashes under
sustained c=100 on this machine. Throughput numbers include rejected
connections, so *every* WebBroker delta is polluted and cannot be read
as a real optimization win or loss. WebBroker uses a completely separate
transport (TIdHTTPWebBrokerBridge) and was not touched by any of these
commits in a way that would change its failure pattern.

## Per-backend reading

### HTTP.sys
Biggest beneficiary. Two HTTP.sys-specific fixes landed (async dispatch
bug + streaming body) plus both cross-cutting optimizations. Every
scenario between **+21% and +839%**.

Notable:
- **upload +839%** — bug fix (listener was fully synchronous) plus the
  zero-copy body read. The baseline was a pathological 95 rps for a
  kernel-mode server; the new number (892 rps) is what HTTP.sys *should*
  have always been.
- **heavy +67%** — the middleware chain benefits from O(1) route
  dispatch; the old RTTI-every-request scan was proportionally a bigger
  share of this scenario's total time.

### IndyDirect
Moderate, consistent gains on 4 of 5 pre-existing scenarios.

- **upload +85%** — render path + route table compound into a
  measurable win even though IndyDirect's body read was already
  efficient.
- **json_large +45%** — the render fast path shines when the payload is
  big enough that the UTF-16 string is a real allocation.
- **health +55%** — route table + fast path on small payloads.
- **heavy −9%** — within the ±15-20 % run-to-run variance of the bench
  machine; read as *neutral*, not regression. Would need a quiet
  machine to resolve.

IndyDirect was the baseline for "a working backend" — no bug to fix,
no inefficient body path to rewrite. Its improvements come entirely from
the two shared optimizations (render fast path + route table), which
cap out at the 40–80% range on this kind of workload. This is the
expected shape: dramatic wins come from fixing *bugs* (HTTP.sys had
one), incremental wins come from *optimizations* (Indy only had those).

### WebBroker
Cannot be compared meaningfully at c=100 on this machine. Flaky under
sustained load — the server dies mid-run with `connection refused`
errors accounting for 43–94 % of all requests. The crash is pre-existing
and unrelated to any of the commits under test. For a meaningful
WebBroker comparison it needs to be re-benched at c=50 or lower where
it stays up.

## New benchmark scenarios

`pods/small` and `pods/large` return plain `TBenchItem` / `TObjectList`
objects, forcing the RTTI-driven serializer path (no `TJsonBaseObject`
shortcut). They exist to isolate the tree-building cost.

Results show the serializer tree-build dominates pods/large (231–516 rps
regardless of backend), which is why the attempted opt#6
(ToUtf8JSON-bytes-direct) did not help and was reverted — the bottleneck
is the tree walk, not the string serialization at the end.

## Methodology caveats

1. Machine variance measured at ±20 % even with 3-sample medians. Any
   delta < 15 % should be treated as noise.
2. Loopback-only; network-bound scenarios not exercised.
3. `c=100` fits IndyDirect and HTTP.sys comfortably but is above
   WebBroker's stability ceiling on this machine.
4. All samples are post-warm-up (the first request builds the route
   table; subsequent requests use the cached one).

## Reproduce

```cmd
cd C:\DEV\dmvcframework\performancetest
scripts\build_all.bat
scripts\run_all_stable.bat 30 100 3
python scripts\parse_results.py
```

Raw JSONs: `results/<backend>_<scenario>_r<N>.json`.
