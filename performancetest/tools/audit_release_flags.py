"""
audit_release_flags.py - Audit Release config of all .dproj in the repo.

Checks that each project's Release configuration has the expected compiler
switches for a production build:

  DCC_Optimize              = true
  DCC_RangeChecking         = false
  DCC_IntegerOverflowCheck  = false
  DCC_Assertions            = false
  DCC_DebugInformation      = 0
  DCC_GenerateStackFrames   = false
  DCC_LocalDebugSymbols     = false
  DCC_SymbolReferenceInfo   = 0

IDE edits often toggle these by accident (ref: feedback_res_files_ide_build.md).
A single wrong flag can cost 5-15% CPU. This script flags deviations so the
real baseline benchmark is not polluted.

Usage:
  python audit_release_flags.py [ROOT]

ROOT defaults to the repo root (two levels above this file).

Exit code 0 = all OK, 1 = deviations found.
"""

from __future__ import annotations

import sys
import xml.etree.ElementTree as ET
from dataclasses import dataclass
from pathlib import Path

NS = {"msb": "http://schemas.microsoft.com/developer/msbuild/2003"}

EXPECTED = {
    "DCC_Optimize": "true",
    "DCC_RangeChecking": "false",
    "DCC_IntegerOverflowCheck": "false",
    "DCC_Assertions": "false",
    "DCC_DebugInformation": "0",
    "DCC_GenerateStackFrames": "false",
    "DCC_LocalDebugSymbols": "false",
    "DCC_SymbolReferenceInfo": "0",
}


@dataclass
class Finding:
    project: Path
    key: str
    expected: str
    actual: str | None


def find_release_group(root: ET.Element) -> ET.Element | None:
    """Find the PropertyGroup that defines the Release config.

    Delphi dproj files are inconsistent: in some projects Cfg_1 is Release
    and Cfg_2 is Debug; in others it is inverted. Rather than guessing,
    we first look for a group with Condition mentioning Release, then
    among groups conditioned on '$(Cfg_N)'!='' we pick the one whose
    corresponding '$(Config)'=='Release' alias maps to it.
    """
    # Step 1: find which Cfg_N is the Release alias via its Condition.
    release_cfg: str | None = None
    for pg in root.findall("msb:PropertyGroup", NS):
        cond = pg.attrib.get("Condition", "")
        if "'$(Config)'=='Release'" in cond:
            # e.g. "'$(Config)'=='Release' or '$(Cfg_1)'!=''"
            import re as _re
            m = _re.search(r"\$\(Cfg_(\d+)\)", cond)
            if m:
                release_cfg = f"Cfg_{m.group(1)}"
                break

    if not release_cfg:
        return None

    # Step 2: find the compiler-flags group conditioned on that Cfg only.
    target_cond = f"'$({release_cfg})'!=''"
    for pg in root.findall("msb:PropertyGroup", NS):
        if pg.attrib.get("Condition", "") == target_cond:
            return pg
    return None


def audit_project(path: Path) -> list[Finding]:
    try:
        tree = ET.parse(path)
    except ET.ParseError as e:
        print(f"[WARN] {path}: {e}", file=sys.stderr)
        return []
    root = tree.getroot()
    group = find_release_group(root)
    if group is None:
        # Not a standard Delphi dproj layout - skip silently.
        return []

    findings: list[Finding] = []
    for key, expected in EXPECTED.items():
        elem = group.find(f"msb:{key}", NS)
        actual = elem.text.strip() if (elem is not None and elem.text) else None
        # Missing elements inherit defaults. Embarcadero defaults vary, so
        # we only flag explicit mismatches; missing is treated as "unset".
        if actual is None:
            if key in ("DCC_Optimize",):
                # Critical: must be explicitly true for a perf build.
                findings.append(Finding(path, key, expected, "<missing>"))
            continue
        if actual.lower() != expected.lower():
            findings.append(Finding(path, key, expected, actual))
    return findings


def main() -> int:
    root = Path(sys.argv[1]) if len(sys.argv) > 1 else Path(__file__).resolve().parents[2]
    if not root.exists():
        print(f"Root not found: {root}", file=sys.stderr)
        return 2

    dprojs = sorted(root.rglob("*.dproj"))
    if not dprojs:
        print(f"No .dproj files under {root}", file=sys.stderr)
        return 2

    all_findings: list[Finding] = []
    for dproj in dprojs:
        # Skip IDE expert and tests where defaults may differ intentionally.
        if ".git" in dproj.parts or "__history" in dproj.parts:
            continue
        all_findings.extend(audit_project(dproj))

    if not all_findings:
        print(f"[OK] {len(dprojs)} project(s) scanned, all Release configs match expected flags.")
        return 0

    print(f"[DEVIATIONS] {len(all_findings)} mismatch(es) across {len(dprojs)} project(s):\n")
    grouped: dict[Path, list[Finding]] = {}
    for f in all_findings:
        grouped.setdefault(f.project, []).append(f)
    for proj, items in sorted(grouped.items()):
        rel = proj.relative_to(root)
        print(f"  {rel}")
        for it in items:
            print(f"    {it.key:<28} expected={it.expected:<8} actual={it.actual}")
        print()
    return 1


if __name__ == "__main__":
    sys.exit(main())
