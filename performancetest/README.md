# DMVCFramework Performance Baseline

Benchmark harness for measuring framework overhead across the 3 `IMVCServer`
backends (WebBroker / IndyDirect / HTTP.sys). Purpose: **establish a baseline
before implementing the optimizations listed in `todo_performance_improvements.md`**,
so every change can be measured against stable reference numbers.

## Layout

```
performancetest/
├── bench_server/       One benchmark server, three .dpr variants (one per backend)
├── scripts/            wrk Lua scripts + run_all.bat driver + Python aggregator
├── tools/              Release-config audit script (catch wrong compiler flags)
└── results/            Output logs + CSV summary
```

## Prerequisites

- Delphi 13 (same as `unittests/`) with Win64 target.
- Load driver. **Primary**: [`oha`](https://github.com/hatoo/oha) — Rust,
  native Windows binary, JSON output, actively maintained. The repo ships
  a pre-downloaded `tools/oha.exe` so no install is needed; the scripts
  pick it up automatically. To install system-wide instead:
  `winget install hatoo.oha` or `cargo install oha`.
  **Fallback**: [`wrk`](https://github.com/wg/wrk) via WSL
  (`sudo apt install wrk`). `run_scenario.bat` uses whichever it finds
  first (oha wins).
- Python 3.10+ for `parse_results.py` and `audit_release_flags.py`.

## Build

One command (calls `rsvars.bat` internally):

```cmd
cd performancetest\scripts
build_all.bat
```

Output: `performancetest\bench_server\PerfBenchServer_*.exe`. Edit
`build_all.bat` to point `RSVARS` at a different Delphi install if needed.

Equivalent manual commands:

```cmd
call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"
msbuild performancetest\bench_server\PerfBenchServer_WebBroker.dproj  /p:Config=Release /p:Platform=Win64
msbuild performancetest\bench_server\PerfBenchServer_IndyDirect.dproj /p:Config=Release /p:Platform=Win64
msbuild performancetest\bench_server\PerfBenchServer_HttpSys.dproj    /p:Config=Release /p:Platform=Win64
```

Opening `bench_server\ProjectGroup.groupproj` in the IDE and doing
*Build All Projects* produces the same result.

## Audit Release flags first

Before benchmarking, verify no package has a wrong compiler switch in its
Release config (a single wrong flag can cost 5-15% CPU — trivial to miss
after IDE edits):

```bash
python performancetest/tools/audit_release_flags.py
```

Exit code 0 = clean. Any deviation is printed with project path and
expected vs actual value.

## Run the benchmark

```cmd
cd performancetest\scripts
run_all.bat 30 200 8
```

The arguments are `DURATION_SECONDS CONNECTIONS THREADS`. The script:

1. Starts each backend in turn on port 9999.
2. Polls `/bench/health` until the server is up.
3. Runs all 5 scenarios against it.
4. Kills the backend and moves to the next.
5. Writes per-scenario logs to `../results/` and a timestamped Markdown
   summary at `../results/summary_<stamp>.md`.

To run a single scenario against a manually-started backend:

```cmd
REM start one backend manually in another terminal:
cd performancetest\bench_server\Win64\Release
PerfBenchServer_IndyDirect.exe

REM then drive it:
cd performancetest\scripts
run_scenario.bat indydirect json_small 30 200 8
```

## Aggregate results

```bash
python performancetest/scripts/parse_results.py
```

Produces `results/summary.csv` and a terminal table:

```
backend     scenario      rps  p50 ms  p99 ms
webbroker   health      45123    3.21    7.40
indydirect  health      72891    2.05    4.90
httpsys     health      98117    1.50    3.70
...
```

## Scenarios

| Scenario     | Method | Path                  | Payload          | What it measures                            |
|--------------|--------|-----------------------|------------------|---------------------------------------------|
| `health`     | GET    | `/bench/health`       | ~50 B JSON       | Pure framework overhead, shortest path      |
| `json_small` | GET    | `/bench/json/small`   | ~500 B JSON      | Serializer warm path                        |
| `json_large` | GET    | `/bench/json/large`   | ~20 KB JSON      | Serializer under load                       |
| `upload`     | POST   | `/bench/upload`       | 1 MB body        | Body read I/O path                          |
| `heavy`      | GET    | `/bench/heavy`        | ~60 B JSON       | Same tiny payload, full middleware chain    |

The middleware chain active on every scenario is:
`CORS → Analytics → Compression`. `health` and `heavy` use the same payload
size so the **delta (heavy - health) quantifies middleware overhead per request**.

## Baseline recording

After a clean benchmark run, record the numbers in `results/BASELINE.md`
(see the template below). This is the reference point against which every
optimization in `todo_performance_improvements.md` is measured.

```markdown
# Baseline - <DATE> - commit <hash>

## Machine
CPU:    <model>
Cores:  <n>
RAM:    <n> GB
OS:     Windows 11 <build>
Delphi: 13.x

## Parameters
Duration:    30s
Connections: 200
Threads:     8
Loopback:    yes (127.0.0.1)

## Results (rps / p50 / p99)

| Scenario     | WebBroker       | IndyDirect      | HTTP.sys        |
|--------------|-----------------|-----------------|-----------------|
| health       | R / p50 / p99   | R / p50 / p99   | R / p50 / p99   |
| json_small   | ...             | ...             | ...             |
| json_large   | ...             | ...             | ...             |
| upload       | ...             | ...             | ...             |
| heavy        | ...             | ...             | ...             |

## Notes
<anything non-obvious: background processes, kernel version, etc.>
```

## What NOT to conclude from these numbers

- They're loopback, so network-bound scenarios are not represented.
- No DB, no FireDAC, no disk I/O. Those come in separate scenarios layered
  on top (see `todo_performance_improvements.md` Tier extensions).
- Percentiles reported by `wrk` are from a co-ordinated-omission-affected
  sample. For strict SLO-style measurement re-run with `wrk2 -R <rate>`
  (constant-throughput mode).
