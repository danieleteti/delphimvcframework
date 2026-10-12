#!/usr/bin/env python3
"""Have the templates changed what an existing project type generates?

Compares two output folders of TestTemplateGenerator.exe, case by case, on the
cases they share (a case added by one side is listed, not compared). Values
drawn at every run are normalised first.

    git worktree add --detach ..\\..\\..\\base HEAD
    (build and run TestTemplateGenerator.exe --skip-compile there)
    python compare_outputs.py <base>\\ideexpert\\tests\\output output
"""
import os
import re
import sys

SKIP_DIRS = {"Win32", "Win64", ".claude", "swagger", "__history"}
SKIP_FILES = {"compile.bat", "compile_output.txt"}
SKIP_EXT = {".res", ".rc", ".dcu", ".exe", ".rsm", ".map", ".png", ".ico"}
NORMALISE = [
    (re.compile(r"\{[0-9A-Fa-f]{8}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{12}\}"), "{GUID}"),
    (re.compile(r"(JWT_SECRET=)[0-9a-f]+"), r"\1SECRET"),
    (re.compile(r"(mcp\.auth\.token=)[0-9a-f]+"), r"\1TOKEN"),
    (re.compile(r"(dmvc\.sqids\.alphabet=)\w+"), r"\1ALPHABET"),
    (re.compile(r"('dmvc\.sqids\.alphabet', ')\w+"), r"\1ALPHABET"),
]


def files_of(root):
    found = {}
    for folder, dirs, files in os.walk(root):
        dirs[:] = [d for d in dirs if d not in SKIP_DIRS]
        for name in files:
            if name in SKIP_FILES or os.path.splitext(name)[1].lower() in SKIP_EXT:
                continue
            path = os.path.join(folder, name)
            found[os.path.relpath(path, root)] = path
    return found


def text_of(path):
    data = open(path, "rb").read().decode("utf-8", "replace")
    data = data.replace(chr(13) + chr(10), chr(10))  # a checkout may differ in line endings only
    for pattern, replacement in NORMALISE:
        data = pattern.sub(replacement, data)
    return data


def main():
    base, new = sys.argv[1], sys.argv[2]
    base_cases = {d for d in os.listdir(base) if os.path.isdir(os.path.join(base, d)) and not d.startswith("_")}
    new_cases = {d for d in os.listdir(new) if os.path.isdir(os.path.join(new, d)) and not d.startswith("_")}
    differences = 0
    compared = 0
    for case in sorted(base_cases & new_cases):
        a, b = files_of(os.path.join(base, case)), files_of(os.path.join(new, case))
        for rel in sorted(set(a) | set(b)):
            if rel not in a:
                print(f"{case}: new file {rel}")
                differences += 1
            elif rel not in b:
                print(f"{case}: missing file {rel}")
                differences += 1
            else:
                compared += 1
                if text_of(a[rel]) != text_of(b[rel]):
                    print(f"{case}: different {rel}")
                    differences += 1
    print(f"\n{len(base_cases & new_cases)} cases and {compared} files compared, {differences} differences")
    print(f"cases only in the second folder ({len(new_cases - base_cases)}): {', '.join(sorted(new_cases - base_cases))}")
    if base_cases - new_cases:
        print(f"cases only in the first folder: {', '.join(sorted(base_cases - new_cases))}")
        differences += 1
    sys.exit(1 if differences else 0)


if __name__ == "__main__":
    main()
