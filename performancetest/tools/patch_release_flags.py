"""
patch_release_flags.py - Inject explicit perf flags into Release config of
every package .dproj in packages/d*/.

Finds the PropertyGroup that contains the line

    <DCC_Define>RELEASE;$(DCC_Define)</DCC_Define>

and ensures that group explicitly sets each of:

    DCC_Optimize              = true
    DCC_RangeChecking         = false
    DCC_IntegerOverflowCheck  = false
    DCC_Assertions            = false
    DCC_GenerateStackFrames   = false

Rationale: when a developer opens the .dpk in the IDE and builds it in
Release, these flags must be guaranteed. Relying on the compiler's implicit
defaults exposes the build to regressions when Embarcadero changes
defaults or when the IDE silently edits the dproj.

The script is idempotent: running it twice is a no-op. It preserves indentation
and line endings.

Usage:
  python patch_release_flags.py [ROOT]

ROOT defaults to the repo root packages/ directory.
"""

from __future__ import annotations

import re
import sys
from pathlib import Path

FLAGS = [
    ("DCC_Optimize", "true"),
    ("DCC_RangeChecking", "false"),
    ("DCC_IntegerOverflowCheck", "false"),
    ("DCC_Assertions", "false"),
    ("DCC_GenerateStackFrames", "false"),
]

RELEASE_MARKER = "<DCC_Define>RELEASE;$(DCC_Define)</DCC_Define>"


def patch_file(path: Path) -> bool:
    """Return True if the file was modified."""
    text = path.read_text(encoding="utf-8")
    if RELEASE_MARKER not in text:
        return False

    # Locate the enclosing PropertyGroup that holds the RELEASE marker.
    # Walk backwards from the marker to the nearest <PropertyGroup ...>
    # and forwards to the matching </PropertyGroup>.
    marker_idx = text.index(RELEASE_MARKER)
    open_idx = text.rfind("<PropertyGroup", 0, marker_idx)
    close_idx = text.find("</PropertyGroup>", marker_idx)
    if open_idx == -1 or close_idx == -1:
        return False

    head = text[:open_idx]
    group = text[open_idx:close_idx]
    tail = text[close_idx:]

    # Detect indentation used inside the group (first child element).
    indent_match = re.search(r"\n([ \t]+)<DCC_", group)
    indent = indent_match.group(1) if indent_match else "        "

    new_group = group
    added = []
    for key, value in FLAGS:
        # Already present (any value)?
        if re.search(rf"<{key}>[^<]*</{key}>", new_group):
            # Normalize to expected value if it differs.
            new_group, n = re.subn(
                rf"<{key}>[^<]*</{key}>",
                f"<{key}>{value}</{key}>",
                new_group,
                count=1,
            )
            if n and f"<{key}>{value}</{key}>" not in group:
                added.append(f"{key} (corrected)")
            continue
        # Insert immediately after the RELEASE define line, preserving indent.
        injection = f"\n{indent}<{key}>{value}</{key}>"
        new_group = new_group.replace(
            RELEASE_MARKER,
            RELEASE_MARKER + injection,
            1,
        )
        added.append(key)

    if new_group == group:
        return False

    path.write_text(head + new_group + tail, encoding="utf-8")
    print(f"  patched {path.name}: {', '.join(added)}")
    return True


def main() -> int:
    if len(sys.argv) > 1:
        root = Path(sys.argv[1])
    else:
        root = Path(__file__).resolve().parents[2] / "packages"

    if not root.exists():
        print(f"Not found: {root}", file=sys.stderr)
        return 2

    dprojs = sorted(root.rglob("*.dproj"))
    if not dprojs:
        print(f"No .dproj under {root}", file=sys.stderr)
        return 2

    changed = 0
    for d in dprojs:
        if patch_file(d):
            changed += 1

    print(f"\n[done] {changed}/{len(dprojs)} dproj modified.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
