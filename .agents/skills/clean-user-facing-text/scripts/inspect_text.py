#!/usr/bin/env python3
"""Inspect text for invisible Unicode / space homoglyphs (Layer A)."""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

# Allow running as script from any cwd
sys.path.insert(0, str(Path(__file__).resolve().parent))

from common import emit_json, read_text_input  # noqa: E402
from text_unicode import human_report, inspect_text  # noqa: E402


def main() -> int:
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("path", nargs="?", default="-", help="Text file path, or - for stdin")
    p.add_argument("--json", action="store_true", help="JSON report")
    p.add_argument(
        "--aggressive",
        action="store_true",
        help="Also flag Latin confusable / fullwidth lookalikes",
    )
    p.add_argument(
        "--strip-emoji-glue",
        action="store_true",
        help="Paranoid: flag all load-bearing invisibles too (emoji glue, script joiners, flag tags, same-script fillers/selectors, orthographic Cf)",
    )
    p.add_argument(
        "--force-text",
        action="store_true",
        help="Scan even when the input looks like a binary container",
    )
    args = p.parse_args()

    text = read_text_input(args.path, allow_binary=args.force_text)
    report = inspect_text(
        text,
        aggressive=args.aggressive,
        strip_emoji_glue=args.strip_emoji_glue,
    )
    if args.json:
        emit_json(report.to_dict())
    else:
        print(human_report(report))
    return 0 if report.suspicious_total == 0 else 1


if __name__ == "__main__":
    raise SystemExit(main())
