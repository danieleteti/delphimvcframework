"""
parse_results.py - Aggregate benchmark output into a CSV and pretty table.

Primary input:  ../results/*.json  (oha --json output)
Fallback input: ../results/*.log   (wrk --latency text output)

Output:
  ../results/summary.csv
  Terminal table (backend x scenario) with rps, p50, p99

File naming convention:
  <backend>_<scenario>.{json,log}     e.g. indydirect_json_small.json
"""

from __future__ import annotations

import csv
import json
import re
import sys
from pathlib import Path

# oha JSON shape (simplified):
# {
#   "summary": {"requestsPerSec": 12345.6, ...},
#   "latencyPercentiles": {"p50": 0.0031, "p90": ..., "p95": ..., "p99": ...}
# }


def parse_oha(path: Path) -> dict | None:
    try:
        data = json.loads(path.read_text(encoding="utf-8"))
    except (json.JSONDecodeError, OSError):
        return None
    summary = data.get("summary") or {}
    pct = data.get("latencyPercentiles") or {}
    rps = summary.get("requestsPerSec")
    if rps is None:
        return None
    # oha emits latencies in seconds as floats.
    def ms(key: str) -> float | None:
        v = pct.get(key)
        return round(v * 1000.0, 3) if isinstance(v, (int, float)) else None
    return {
        "rps": rps,
        "latency_avg_ms": round(summary.get("average", 0) * 1000.0, 3) if summary.get("average") else None,
        "latency_p50_ms": ms("p50"),
        "latency_p90_ms": ms("p90"),
        "latency_p99_ms": ms("p99"),
    }


RE_WRK_RPS = re.compile(r"Requests/sec:\s+([\d.]+)")
RE_WRK_LAT_AVG = re.compile(r"Latency\s+([\d.]+)(us|ms|s)")
RE_WRK_LAT_P = re.compile(r"\s{5,}(\d+)%\s+([\d.]+)(us|ms|s)")


def _to_ms(v: float, unit: str) -> float:
    if unit == "us":
        return v / 1000.0
    if unit == "s":
        return v * 1000.0
    return v


def parse_wrk(path: Path) -> dict | None:
    text = path.read_text(errors="ignore")
    m_rps = RE_WRK_RPS.search(text)
    if not m_rps:
        return None
    m_avg = RE_WRK_LAT_AVG.search(text)
    percentiles = {}
    for m in RE_WRK_LAT_P.finditer(text):
        percentiles[int(m.group(1))] = _to_ms(float(m.group(2)), m.group(3))
    return {
        "rps": float(m_rps.group(1)),
        "latency_avg_ms": _to_ms(float(m_avg.group(1)), m_avg.group(2)) if m_avg else None,
        "latency_p50_ms": percentiles.get(50),
        "latency_p90_ms": percentiles.get(90),
        "latency_p99_ms": percentiles.get(99),
    }


def split_name(stem: str) -> tuple[str, str] | None:
    parts = stem.split("_", 1)
    return (parts[0], parts[1]) if len(parts) == 2 else None


def main() -> int:
    results_dir = Path(__file__).parent.parent / "results"
    if not results_dir.exists():
        print(f"No results dir at {results_dir}", file=sys.stderr)
        return 1

    rows = []
    for f in sorted(results_dir.iterdir()):
        if f.suffix == ".json":
            metrics = parse_oha(f)
        elif f.suffix == ".log":
            metrics = parse_wrk(f)
        else:
            continue
        if not metrics:
            continue
        name = split_name(f.stem)
        if not name:
            continue
        backend, scenario = name
        rows.append({"backend": backend, "scenario": scenario, **metrics})

    if not rows:
        print("No parseable results found.", file=sys.stderr)
        return 1

    out = results_dir / "summary.csv"
    with out.open("w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=rows[0].keys())
        w.writeheader()
        w.writerows(rows)

    print(f"Wrote {out} ({len(rows)} rows)")
    sw = max(len(r["scenario"]) for r in rows) + 2
    bw = max(len(r["backend"]) for r in rows) + 2
    print(f"\n{'backend':<{bw}}{'scenario':<{sw}}{'rps':>12}{'p50 ms':>10}{'p99 ms':>10}")
    for r in sorted(rows, key=lambda x: (x["scenario"], x["backend"])):
        print(
            f"{r['backend']:<{bw}}{r['scenario']:<{sw}}"
            f"{r['rps']:>12,.0f}"
            f"{(r['latency_p50_ms'] or 0):>10.2f}"
            f"{(r['latency_p99_ms'] or 0):>10.2f}"
        )
    return 0


if __name__ == "__main__":
    sys.exit(main())
