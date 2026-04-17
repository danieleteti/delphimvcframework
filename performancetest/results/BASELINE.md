# DMVCFramework Performance Baseline

**Date:** 2026-04-14
**Commit:** fcafff25 + WIP HTTP.sys async dispatch fix (uncommitted)
**Framework version:** 3.5.0-silicon-beta
**Branch:** feature/server-abstraction-layer

## Preamble — HTTP.sys bug fixed before baselining

The first baseline run showed HTTP.sys running ~2× slower than IndyDirect,
which contradicts the expected kernel-mode IOCP advantage. Root cause found
in `sources/MVCFramework.Server.HttpSys.pas`: the listener thread processed
every request **strictly sequentially** — accept → body read → middleware →
controller → Flush, all on the same thread before accepting the next one.
Every byte of async kernel I/O was being thrown away.

**Fix:** dispatch body read + full pipeline to `System.Threading.TTask.Run`;
allocate a fresh 16KB request buffer each iteration so in-flight tasks do
not alias each other; capture the managed TBytes in the closure so
refcounting keeps the buffer alive until the task finishes.

Regression check: `invoke tests32-httpsys` → **870/870 pass** (2 pre-existing
host-bind errors unrelated to this change).

Delta on HTTP.sys at c=200 before vs after fix:

| Scenario   | Before (rps) | After (rps) | Improvement |
|------------|-------------:|------------:|------------:|
| health     |        1,530 |       2,963 |     **+94%** |
| json_small |        1,520 |       2,559 |     **+68%** |
| json_large |          569 |         849 |     **+49%** |
| heavy      |        1,391 |       1,739 |     **+25%** |
| upload 1MB |          184 |         559 |    **+204%** |

This is a bug fix, not an optimization — so it's not part of the
"optimizations" track. The numbers below are the **post-fix baseline**
against which future optimizations are measured.

## Machine

- CPU:    13th Gen Intel(R) Core(TM) i9-13980HX
- Cores:  24 physical / 32 logical
- RAM:    32 GB
- OS:     Windows 11 Pro 10.0.26200 (Build 26200.8037, 25H2)
- Delphi: 13.x (Studio 37.0) — compiler version 37.0
- Build:  Release, Win64; DCC_Optimize=true, range/overflow/assertions off,
          DebugInformation=0 (verified via `tools/audit_release_flags.py`)

## Parameters

- Duration per scenario: 30 s
- Driver:                oha 1.14.0 PGO Windows (bundled `tools/oha.exe`)
- Loopback:              `http://localhost:9999/`
- Concurrency:
  - **c=100** — all three backends complete successfully (authoritative baseline)
  - **c=200** — WebBroker crashes at this load (connection refused after first
    scenario); IndyDirect and HTTP.sys survive. Used to illustrate peak for
    the two more robust backends.

## Baseline c=100 (stable across all 3 backends)

Format: `rps  /  p50 ms  /  p99 ms`. Higher rps better, lower ms better.

| Scenario     | WebBroker            | IndyDirect           | HTTP.sys             |
|--------------|----------------------|----------------------|----------------------|
| health       | 519 / 199 / 968      | 1,153 / 0.94 / 849   | **2,354** / 4 / 324  |
| json_small   | 959 / 757 / 971      | 1,388 / 0.70 / 833   | **2,099** / 2 / 572  |
| json_large   | 181 / 518 / 1,801    | 435 / 1.84 / 2,094   | **735** / 1.3 / 1,477 |
| heavy        | 659 / 245 / 1,408    | 1,670 / 0.30 / 1,174 | **1,874** / 0.5 / 778 |
| upload 1MB   | 372 / 92 / 521       | **413** / 53 / 1,500 | 95 / 910 / 3,525     |

## Peak c=200 (IndyDirect + HTTP.sys only)

WebBroker omitted because it died with ~99% connection-refused errors at
this concurrency. The crash reproduces reliably — pre-existing
`TIdHTTPWebBrokerBridge` scheduler limit, not related to the HTTP.sys fix.

| Scenario     | IndyDirect             | HTTP.sys             |
|--------------|------------------------|----------------------|
| health       | **3,758** / 0.12 / 883 | 2,963 / 37 / 444     |
| json_small   | 2,558 / 0.13 / 1,288   | **2,559** / 30 / 640 |
| json_large   | 792 / 0.54 / 3,887     | **849** / 53 / 2,536 |
| heavy        | **2,416** / 0.13 / 1,329 | 1,739 / 10 / 1,263 |
| upload 1MB   | **1,086** / 0.91 / 2,288 | 559 / 49 / 2,536   |

## Observations for the optimization work

### 1. HTTP.sys is now competitive with IndyDirect on small-JSON GETs

At c=100, HTTP.sys wins 4 of 5 GET scenarios. At c=200, the two backends are
nearly tied on `json_small` (2558 vs 2559) and HTTP.sys leads on `json_large`
(849 vs 792). Kernel-mode IOCP is earning its keep. Remaining HTTP.sys
weak point: `upload` — worse than IndyDirect at both concurrencies. Likely
the 1-chunk-at-a-time `HttpReceiveRequestEntityBody` loop in `ReadFullBody`;
candidate for async streaming instead of copy-in-full.

### 2. IndyDirect wins on uploads

413 rps at c=100 vs HTTP.sys 95. IndyDirect reads the body stream directly
without kernel→user→framework triple copy. If we want uniform throughput,
HTTP.sys adapter needs streaming body support.

### 3. Tail latency is bad on every backend

p99 in the seconds range across all scenarios. Classic signature of under-
provisioned worker pools and / or coordinated-omission artifacts in the
driver. Worth re-running with `wrk2 -R <rate>` (constant throughput mode)
to get coordinated-omission-free percentiles.

### 4. WebBroker can't sustain c=200

Reproducible with standalone server. Crash manifests as TCP listener dying
after ~30s of load (2800+ "connection refused" errors follow). Predates
this work. Workaround for benchmarking: cap at c=100. Not part of the
optimization scope for 3.5.x.

### 5. json_large is still the serializer killshot

At c=100, `json_large` drops throughput 3–5× vs `json_small` on all three
backends. JSON serialization dominates once payload grows beyond ~1 KB.
Confirms the TODO's priority ordering: **JsonDataObjects writer reuse**
(thread-local builder + writer) is top-1 ROI optimization.

## Per-scenario purpose

| Scenario     | Purpose                                                 |
|--------------|---------------------------------------------------------|
| `health`     | Pure framework overhead — shortest possible path         |
| `json_small` | Serializer warm path, typical REST payload (~500 B)      |
| `json_large` | Serializer under sustained object-graph load (~20 KB)    |
| `heavy`      | Same payload as health but through CORS+Analytics+Compression middleware |
| `upload`     | 1 MB POST body — exercises body read path                |

## Reproduce

```cmd
cd C:\DEV\dmvcframework\performancetest
scripts\build_all.bat
scripts\run_all.bat 30 100
python scripts\parse_results.py
```

For the c=200 peak numbers (IndyDirect + HTTP.sys only), start each server
manually and run scenarios via `run_scenario.bat <backend> <scenario> 30 200`.

Raw JSONs: `results/<backend>_<scenario>.json`.
Aggregate CSV: `results/summary.csv`.

## Next optimization targets (priority order)

1. **Thread-local JsonDataObjects writer reuse** — biggest serializer win
   on `json_large` across all backends.
2. **Route metadata cache at registration time** — eliminates RTTI per
   request; hits all scenarios.
3. **HTTP.sys streaming body read** — close the gap on `upload`.
4. **Controller pool per-thread** — modest win on allocations, coupled
   with #2.
5. **WebBroker c=200 stability** — out of scope for 3.5.x but worth
   filing for future investigation.
