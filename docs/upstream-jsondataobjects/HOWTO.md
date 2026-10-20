# HOWTO — push the branch and open the PR

Everything below is **ready to run**; nothing here has been executed. The branch exists
locally in `C:\DLib\JsonDataObjects\`, the commit is made, the tests are green.

## Use `C:\DLib\JsonDataObjects`, NOT `C:\DLib\JsonDataObjectsFork`

There are two clones on this machine. Only the first one is usable for the PR:

| Clone | Base commit | Usable for the PR? |
|---|---|---|
| `C:\DLib\JsonDataObjects` | `4fdd3057` (upstream master, 2026) | **yes** — commit `c9e91d7` is `+160/-0` |
| `C:\DLib\JsonDataObjectsFork` | `0ea4b5b` (Sep **2021**) | no — same diff measures `+4494/-1184` |

The `…Fork` clone has the right *file contents* (both `Source/JsonDataObjects.pas` and
`UnitTest/TestJsonDataObjects.pas` are byte-identical to the patched files in the other
clone), but its git history stops at 2021. GitHub computes a PR diff against the merge
base, so a branch pushed from there would show five years of unrelated upstream churn as
if you had written it. Do not commit there; the `…Fork` clone is redundant.

Wiring the fork as a *remote* of the good clone (step 1 below) is all that was ever
needed — a fork is a place to push, not a place to work.

## State of the local repo

```
repo    C:\DLib\JsonDataObjects
origin  https://github.com/ahausladen/JsonDataObjects.git   (upstream, NOT your fork)
branch  fix/parser-nesting-depth-limit
base    4fdd3057  feat: Added "FailOnPrimitive: Boolean = False" parameter to IsValidJSON()
commit  c9e91d7   fix: Limit the parser's nesting depth to bound stack usage
diff    2 files, +160 / -0
tests   OK: 60 tests   (Win32 and Win64, Delphi 13)
```

`origin` is upstream, so **do not `git push origin`** — you have no write access there and
you don't want to try. Your fork `danieleteti/JsonDataObjects` already exists (forked
2021), it just isn't wired up as a remote yet.

Verified on 2026-08-31: `gh auth status` is logged in as `danieleteti`;
`danieleteti/JsonDataObjects` is a fork of `ahausladen/JsonDataObjects` with `master` as
its default branch and **`master` as its only branch** (nothing pushed yet); upstream
`master` on GitHub is still at `4fdd3057`, so `c9e91d7`'s parent is exactly the current
upstream tip and the PR will apply cleanly with no rebase. `PR.md`'s "Base: `master` @
`4fdd3057`" is accurate as written.

## 1. Add your fork as a remote

```bash
cd /d C:\DLib\JsonDataObjects
git remote add fork https://github.com/danieleteti/JsonDataObjects.git
git remote -v
```

The fork's `master` being years behind doesn't matter: the branch is based on current
upstream `master`, and that's what GitHub compares against.

## 2. Push the branch

```bash
git push -u fork fix/parser-nesting-depth-limit
```

The fork currently has only `master`, so this creates the branch. Nothing on the fork gets
overwritten, and its stale 2021 `master` is left exactly as it is — no sync needed.

## 3. Open the PR

Run from the directory holding this file, so `--body-file` resolves:

```bash
cd /d C:\DEV\dmvcframework\docs\upstream-jsondataobjects

gh pr create ^
  --repo ahausladen/JsonDataObjects ^
  --base master ^
  --head danieleteti:fix/parser-nesting-depth-limit ^
  --title "Limit the parser's nesting depth (a 34 KB document can kill the process)" ^
  --body-file PR.md
```

(Under bash/PowerShell replace the `^` line continuations with `\` / backtick, or put it
all on one line.)

`PR.md` opens with a metadata header (repository, base, patch filename). Delete those
first few lines before submitting if you'd rather the PR body start at "## The problem" —
GitHub readers don't need them.

## Applying the patch by hand instead

For anyone reproducing this on their own clone of upstream `master` @ `4fdd3057`:

```bash
git checkout -b fix/parser-nesting-depth-limit 4fdd3057
git apply --check jsondataobjects-max-nesting-depth.patch   # verify first
git apply jsondataobjects-max-nesting-depth.patch
```

On a newer upstream `master`, try `git apply -3` and resolve. The patch is additions only,
so a conflict would only come from upstream editing the same neighbourhoods
(`ParseObjectBody` / `ParseArrayBody` / the `var` block after `TJDOJsonArray`).

## Running the tests

The upstream DUnit project references a `{$R *.RES}` that isn't in the repo, so build your
own console runner **outside** the repo:

```pascal
program jdotest;
{$APPTYPE CONSOLE}
uses
  DUnitTestRunner,
  JsonDataObjects in 'C:\DLib\JsonDataObjects\Source\JsonDataObjects.pas',
  TestJsonDataObjects in 'C:\DLib\JsonDataObjects\UnitTest\TestJsonDataObjects.pas';
begin
  DUnitTestRunner.RunRegisteredTests;
end.
```

From a `.bat` (`rsvars.bat` needs `call`, and does nothing useful from PowerShell):

```bat
call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"
dcc32 -B -Q -DCONSOLE_TESTRUNNER -NSWinapi;System.Win;System;Data;Vcl;Vcl.Imaging;Xml;Web ^
  -U"C:\Program Files (x86)\Embarcadero\Studio\37.0\source\DUnit\src" ^
  -N"%CD%\out32" -E"%CD%\out32" jdotest.dpr
```

`dcc64` with `out64` for Win64. Both report `OK: 60 tests`. The compilers exit 0 even on
error — read the output, not the exit status.

Re-run on 2026-08-31 against `C:\DLib\JsonDataObjectsFork\Source` (byte-identical file):
`OK: 60 tests` on Win32 and on Win64, both compiling with zero warnings. `repro_Depth.dpr`
built against the same source prints `EJsonParserException: JSON nesting level exceeds the
maximum of 512 (1, 515)` and exits 0 at depth 8500 (Win64) and 50000 (Win32) — no crash,
no exit 139.

Give the `.bat` an explicit `cd /d` to its own directory and absolute paths for `-N`/`-E`:
invoked from another working directory, `%CD%` is not the script's folder.

---

# Using it, as an application developer

## The default

Nothing to do. `JsonMaxNestingDepth` starts at `DefaultJsonMaxNestingDepth = 512`, which
is far deeper than any document a real API exchanges, and an order of magnitude below the
depth at which the parser dies (8 500 on Win64).

## Raising the limit

If you genuinely exchange deeply nested documents, set it once at startup:

```pascal
begin
  JsonMaxNestingDepth := 2000;   // global, and not thread-safe: set it before you serve
  ...
```

Like `JsonSerializationConfig`, it is a plain global with no locking. Set it during
initialization, before any worker thread parses anything — not per request.

## What you get when a document is too deep

`EJsonParserException`, the same exception any other malformed document raises, carrying
the usual line/column/position:

```
EJsonParserException: JSON nesting level exceeds the maximum of 512 (1, 515)
```

So existing handling needs no change — code that already catches `EJsonParserException`
around `Parse` catches this too:

```pascal
try
  LObj := TJsonBaseObject.Parse(LBody);
except
  on E: EJsonParserException do
    // 400 Bad Request; the message names the limit that was hit
end;
```

`IsValidJSON` now answers `False` for such a document instead of taking the process down
with it. That's the important part: before this patch its `except` clause could not catch
the failure, because nothing was raised — the stack simply ran out and the process
vanished with exit code 139, no exception, no log line.

## Why zero doesn't switch it off

```pascal
JsonMaxNestingDepth := 0;    // <- still 512, NOT "unlimited"
JsonMaxNestingDepth := -1;   // <- still 512
```

Zero and negatives fall back to `DefaultJsonMaxNestingDepth`. A limit that one stray line
of configuration can clear is a limit an attacker eventually gets — and "0 means no limit"
is exactly the line someone copies from a forum post. If you really want the old unbounded
behaviour you have to say so explicitly:

```pascal
JsonMaxNestingDepth := MaxInt;   // back to pre-patch behaviour, stack overflow included
```

which is deliberately something you cannot type by accident.
