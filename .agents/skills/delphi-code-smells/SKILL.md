---
name: delphi-code-smells
description: Use when reviewing existing Delphi / Object Pascal code for defects, hunting a memory leak or an access violation, or hardening new code against the classic mistakes. Covers compiler warnings and hints that indicate a real bug, warnings-as-errors, leak detection and how to make a leak fail a build, third-party static analysis, and a catalogue of smells each with a way to find it. Triggers on "code smell", "code review", "review this unit", "refactor this unit", "memory leak", "leak", "leaking", "double free", "access violation", "AV", "static analysis", "lint", "compiler warning", "W1035", "$WARN", "warnings as errors", "FastMM", "ReportMemoryLeaksOnShutdown", "audit this code".
---

# Delphi code smells — make the machine find them first

A review pass that starts with an opinion is worth little. A review pass that starts with the compiler and
a leak report is worth a lot, because it is objective and free. Do that half first, every time.

Every warning code, directive and API below was **produced by `dcc32.exe` 37.0 on this machine or copied
from the shipped RTL/DUnitX source**, not recalled. Where something could not be verified it says so.

---

## What this skill owns, and what it does not

| Question | Where it is answered |
|----------|---------------------|
| *Why* does this leak? How does ownership work? `Free`, `try/finally`, interfaces, refcounting, `TObjectList<T>`, `TComponent`, managed records | `delphi`, `reference/memory.md` — **not restated here** |
| What is the right `try/except` shape? What must I never catch? | `delphi`, `reference/exceptions.md` |
| `Synchronize` vs `Queue`, locks, `FreeOnTerminate` | `delphi`, `reference/concurrency.md` |
| Who frees the object a DMVC action returns? What is `ToFree`? | `dmvcframework` SKILL.md — **not restated here** |
| **How do I find the leak? How do I prove it? How do I stop it coming back?** | **here** |

This skill is diagnosis. When a smell needs semantics to explain, it links; it does not re-teach.

---

## Order of attack — an unfamiliar unit

Work down this list. **Stop reporting when you run out of items that cost the user something.**

1. **Compile it with warnings and hints on and read every line of output.** Free, objective, and the single
   highest-yield step. `reference/static-analysis.md`.
2. **Run it with `ReportMemoryLeaksOnShutdown := True`.** If there is a test suite, run that.
   `reference/leaks.md`.
3. **Ownership by hand** — every `Create` in the unit: who frees it, on which paths, including the raising
   one. This is where the compiler cannot help you.
4. **Exception handling** — every `except` without an `on`, every empty handler, every `try/except` that
   should have been `try/finally`.
5. **Concurrency** — anything reachable from a `TThread.Execute` or a `TTask.Run` body.
6. **Everything else.** Naming, `with`, long methods, magic numbers.

**A leak the compiler cannot see outranks a naming convention, always.** If the review output is a wall of
style nits and no lifetime finding, the review was not done. Report defects in the order above, and say for
each one what it costs at runtime — a leak, an AV, a silently wrong result. "This is ugly" is not a finding.

---

## The five-minute pass

```bash
# 1. what the compiler already knows (see reference/static-analysis.md for the codes)
dcc32.exe -H -W MyProject.dpr

# 2. the greps that pay for themselves, in rough order of yield
rg -n -U 'try\b[^;]*?\n\s*\w+\s*:=\s*T\w+\.Create'   # constructor INSIDE the try  -> AV in the finally
rg -n -P '(?s)except\s*(//[^\n]*\n\s*)*end'          # swallowed exception
rg -n '\bwith\b\s+\w+.*\bdo\b'                       # with
rg -n 'FreeAndNil\s*\(\s*[lL]'                       # FreeAndNil on a local
rg -n 'FreeOnTerminate'                              # thread lifetime
rg -n '\w+\s*:=\s*\w+\s*\+\s*'                       # candidate string concat in a loop
```

Then read `reference/smells.md` for what each hit means and how to fix it.

---

## Reference files

| File | Read it when |
|------|--------------|
| `reference/static-analysis.md` | Compiler warnings and hints (verified codes), `$WARN`, turning a warning into an error, the build switches, and an honest note on Pascal Analyzer / SonarDelphi / FixInsight / madExcept / Error Insight |
| `reference/leaks.md` | Finding, reproducing and fencing a leak: the shipped memory manager, `ReportMemoryLeaksOnShutdown`, reading the report, `RegisterExpectedMemoryLeak`, and DUnitX per-test leak detection — the item that makes a leak fail a build |
| `reference/smells.md` | The catalogue. Each entry: the bad code, what it costs at runtime, the fix, and how to detect it |

---

## The defects worth finding, ranked

Detail and detection for each in `reference/smells.md`.

| # | Defect | Costs you |
|---|--------|-----------|
| 1 | Constructor **inside** the `try` | `Destroy` called on uninitialised memory — AV |
| 2 | Object created and never freed on some path | Leak |
| 3 | `Create` in a loop, `Free` after it | Leak, once per iteration |
| 4 | Object held as *both* a class reference and an interface | Double free, or a dangling pointer |
| 5 | Two objects holding interfaces to each other | Leak the shutdown report shows as a matched pair |
| 6 | `except end` | The diagnosis is deleted; the program carries on wrong |
| 7 | `try/except` where `try/finally` was meant | Resource is released only when something raised |
| 8 | `Result` not assigned on every path | Caller reads garbage. **W1035** |
| 9 | Event handler / observer never unsubscribed | AV when the publisher fires into a freed subscriber |
| 10 | A `function` that returns an object nobody knows they own | Leak, or a double free |
| 11 | Worker thread touching the VCL | AV, once a week, on the customer's machine |
| 12 | A thread outliving the object it captured | AV at shutdown |
| 13 | Method silently hiding an ancestor's virtual | Wrong method runs. **W1010** |
| 14 | `with` | A field added later hijacks a name; silent, compiles |
| 15 | Magic literal used as a status code | Wrong branch after any refactor |

---

## Reporting a review

- One line per finding: **location, what goes wrong at runtime, the fix.** Not an essay.
- Lead with anything that leaks, dangles or returns a wrong value. Style last, or not at all.
- If a compiler warning already covers a finding, **cite the code** (`W1035`) — it turns your opinion into
  something the user can put in their build and never hear from you again about.
- Never report a smell you cannot say how to detect. If you cannot name a warning, a tool or a grep for it,
  it is a preference, and the user did not ask for your preferences.
