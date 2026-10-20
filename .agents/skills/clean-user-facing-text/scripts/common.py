"""Small, dependency-free helpers for the text-only skill."""

from __future__ import annotations

import json
import os
import sys
import tempfile
from pathlib import Path
from typing import Any

MAX_INPUT_BYTES = int(os.environ.get("WATERMARKS_MAX_INPUT_BYTES", str(256 << 20)))
MAX_STDIN_BYTES = int(os.environ.get("WATERMARKS_MAX_STDIN_BYTES", str(64 << 20)))
BINARY_SNIFF_BYTES = 8192

BINARY_MAGIC: tuple[tuple[bytes, str], ...] = (
    (b"PK\x03\x04", "a ZIP container such as DOCX or ODT"),
    (b"%PDF-", "a PDF"),
    (b"\x89PNG\r\n\x1a\n", "a PNG image"),
    (b"\xff\xd8\xff", "a JPEG image"),
    (b"GIF87a", "a GIF image"),
    (b"GIF89a", "a GIF image"),
    (b"RIFF", "a RIFF container"),
    (b"\x1f\x8b", "a gzip archive"),
    (b"7z\xbc\xaf\x27\x1c", "a 7-Zip archive"),
    (b"Rar!\x1a\x07", "a RAR archive"),
    (b"\x7fELF", "an ELF binary"),
)

_ALLOWED_CONTROLS = frozenset({0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x1B})


def eprint(*args: object) -> None:
    print(*args, file=sys.stderr)


def _configure_stdio() -> None:
    for stream, errors in (
        (sys.stdin, "surrogateescape"),
        (sys.stdout, "backslashreplace"),
        (sys.stderr, "backslashreplace"),
    ):
        reconfigure = getattr(stream, "reconfigure", None)
        if reconfigure is not None:
            try:
                reconfigure(encoding="utf-8", errors=errors)
            except (OSError, ValueError):
                pass


_configure_stdio()


def looks_binary(data: bytes) -> str | None:
    if not data:
        return None
    for magic, label in BINARY_MAGIC:
        if data.startswith(magic):
            return label
    head = data[:BINARY_SNIFF_BYTES]
    if b"\x00" in head:
        return "binary data containing NUL bytes"
    controls = sum(1 for value in head if value < 0x20 and value not in _ALLOWED_CONTROLS)
    if controls / len(head) > 0.05:
        return "binary data dense in control bytes"
    return None


def guard_binary(data: bytes, origin: str, *, allow_binary: bool = False) -> None:
    if allow_binary:
        return
    kind = looks_binary(data)
    if kind is None:
        return
    eprint(f"refusing to treat {origin} as text: it looks like {kind}.")
    eprint("This lightweight skill accepts text files only.")
    raise SystemExit(2)


def _read_stdin_capped(*, allow_binary: bool = False) -> str:
    stream = getattr(sys.stdin, "buffer", None)
    if stream is None:
        text = sys.stdin.read()
        data = text.encode("utf-8", errors="surrogateescape")
        if len(data) > MAX_STDIN_BYTES:
            raise SystemExit(f"refusing stdin input larger than {MAX_STDIN_BYTES} bytes")
        guard_binary(data[:BINARY_SNIFF_BYTES], "stdin", allow_binary=allow_binary)
        return text

    chunks: list[bytes] = []
    total = 0
    while True:
        chunk = stream.read(1 << 20)
        if not chunk:
            break
        if not chunks:
            guard_binary(
                chunk[:BINARY_SNIFF_BYTES],
                "stdin",
                allow_binary=allow_binary,
            )
        total += len(chunk)
        if total > MAX_STDIN_BYTES:
            raise SystemExit(f"refusing stdin input larger than {MAX_STDIN_BYTES} bytes")
        chunks.append(chunk)
    return b"".join(chunks).decode("utf-8", errors="surrogateescape")


def read_text_input(path: str | None, *, allow_binary: bool = False) -> str:
    if path is None or path == "-":
        return _read_stdin_capped(allow_binary=allow_binary)
    source = Path(path)
    if source.stat().st_size > MAX_INPUT_BYTES:
        raise SystemExit(f"refusing input larger than {MAX_INPUT_BYTES} bytes: {path}")
    data = source.read_bytes()
    guard_binary(data, str(source), allow_binary=allow_binary)
    return data.decode("utf-8", errors="surrogateescape")


def _default_file_mode() -> int:
    mask = os.umask(0)
    os.umask(mask)
    return 0o666 & ~mask


def safe_write_bytes(path: str | Path, data: bytes) -> None:
    destination = Path(path)
    destination.parent.mkdir(parents=True, exist_ok=True)
    if destination.is_symlink():
        raise OSError(f"refusing to write through symlink: {destination}")

    fd, temporary = tempfile.mkstemp(
        prefix=f".{destination.name}.",
        suffix=".tmp",
        dir=str(destination.parent),
    )
    try:
        if hasattr(os, "fchmod"):
            os.fchmod(fd, _default_file_mode())
        with os.fdopen(fd, "wb") as handle:
            handle.write(data)
            handle.flush()
            os.fsync(handle.fileno())
        os.replace(temporary, destination)
    except BaseException:
        try:
            os.unlink(temporary)
        except OSError:
            pass
        raise


def write_text_output(text: str, path: str | None) -> None:
    if path is None or path == "-":
        sys.stdout.write(text)
        if text and not text.endswith("\n"):
            sys.stdout.write("\n")
        return
    safe_write_bytes(path, text.encode("utf-8", errors="surrogateescape"))


def backup_path(source: Path) -> Path:
    backup = source.with_suffix(source.suffix + ".bak")
    try:
        safe_write_bytes(backup, source.read_bytes())
    except OSError as error:
        eprint(f"cannot create backup {backup}: {error}")
        raise SystemExit(2) from error
    return backup


def emit_json(data: Any) -> None:
    json.dump(data, sys.stdout, indent=2, ensure_ascii=False)
    sys.stdout.write("\n")


def cleaned_path(source: Path, suffix: str = ".cleaned") -> Path:
    return source.with_name(f"{source.stem}{suffix}{source.suffix}")
