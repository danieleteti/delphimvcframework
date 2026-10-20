# What a machine can find for you

The Delphi compiler is the most under-used static analyser on the machine. It ships with every install, it
runs in the time the build already takes, and most projects have its output turned into wallpaper.

**Every code in the first table was produced by running `dcc32.exe` (37.0, Delphi 13 Florence) over a file
written to trigger it.** The identifier → code table further down is the docwiki's, verbatim. Nothing here
is recalled.

---

## 1. Turn them on

```bash
dcc32.exe -H -W MyProject.dpr     # -H = output hint messages, -W = output warning messages
```

In a `.dproj`, the two MSBuild properties are `DCC_Hints` and `DCC_Warnings`
(they feed the compiler's `Hints=` / `Warnings=` options in `CodeGear.Delphi.Targets`). In the IDE the same
settings live under *Project Options → Delphi Compiler → Hints and Warnings*; the exact menu path has moved
between releases, so read it off the dialog rather than quoting it.

Per-unit, the master switches are `{$HINTS ON}` and `{$WARNINGS ON}`.

**First move on a legacy codebase:** build it clean once, capture the output, count the warnings. That
number is the review backlog, sorted by the compiler, for free.

---

## 2. The warnings that mean you have a bug

Measured output, one line each, from a file compiled with `dcc32 37.0`:

| Code | What the compiler printed | Why it is a real defect |
|------|---------------------------|-------------------------|
| **W1035** | `Return value of function 'NoRet' might be undefined` | A path leaves the function without assigning `Result`. The caller reads whatever was on the stack. **The most valuable warning in the list.** |
| **W1036** | `Variable 'X' might not have been initialized` | Reading a local before writing it. Same class of bug, one scope down |
| **W1037** | `FOR-Loop variable 'I' may be undefined after loop` | The value of a classic `for` counter after the loop is undefined *by the language*. Code that reads it works until it doesn't |
| **W1010** | `Method 'Doit' hides virtual method of base type 'TBase'` | You meant `override` and wrote a new method. The ancestor keeps calling its own. Silent, and it compiles |
| **W1020** | `Constructing instance of 'TAbs' containing abstract method 'TAbs.Must'` | The call to the abstract method raises `EAbstractError` at runtime |
| **W1048** | `Unsafe typecast of 'TObject' to 'Pointer'` | Off by default. Turn it on (`{$WARN UNSAFE_CAST ON}`) when auditing casts |
| **W1021** / **W1022** | `Comparison always evaluates to False` / `... to True` | A range check that can never fire — usually the wrong type or the wrong constant |
| **W1000** | `Symbol 'Gone' is deprecated: 'use New instead'` | Someone told you what to use instead. Do it before it is removed |
| **W1057** / **W1058** | `Implicit string cast from 'AnsiString' to 'string'` / `... with potential data loss from 'string' to 'AnsiString'` | A silent transcode at every assignment. `W1058` is where non-ASCII text gets destroyed |
| **W1059** / **W1060** | The same two, as *explicit* casts | You wrote the cast, so you meant it — but `W1060` still loses characters |
| **W1072** | `Implicit conversion may lose significant digits from 'Integer' to 'Byte'` | Silent truncation |

And the hints, which are cheaper to fix and often point at a bug in progress:

| Code | What the compiler printed | Reading |
|------|---------------------------|---------|
| **H2077** | `Value assigned to 'A' never used` | A dead store. Frequently the second half of a copy-paste, or a result you forgot to use |
| **H2164** | `Variable 'Unused' is declared but never used in 'DeclaredNeverUsed'` | Leftover. Sometimes the *wrong* variable was used in its place |
| **H2219** | `Private symbol 'FNeverUsed' declared but never used` | A field nobody reads — or a field somebody meant to read |
| **H2135** | `FOR or WHILE loop executes zero times - deleted` | The loop bounds are backwards. The compiler removed your code and told you politely |

`H2077` and `H2164` in a routine you are reviewing for a lifetime bug deserve a second look: a dead store to
an object variable is often the free that went missing.

---

## 3. `{$WARN}` — the exact syntax

From the docwiki *Warning messages (Delphi)*:

```
{$WARN identifier ON | OFF | ERROR | DEFAULT}
```

| Form | Effect |
|------|--------|
| `{$WARN id ON}` | display it |
| `{$WARN id OFF}` | suppress it |
| `{$WARN id ERROR}` | **treat it as an error** — the module does not compile |
| `{$WARN id DEFAULT}` | back to the Project Options setting |

Scope is local: the setting applies from that point in the file, for that compilation unit.

Only the identifiers below can be controlled this way. Table copied from the docwiki:

| Identifier | Code | | Identifier | Code | | Identifier | Code |
|---|---|---|---|---|---|---|---|
| `SYMBOL_DEPRECATED` | W1000 | | `FILE_OPEN` | W1026 | | `IMPLICIT_STRING_CAST` | W1057 |
| `SYMBOL_LIBRARY` | W1001 | | `FILE_OPEN_UNITSRC` | W1027 | | `IMPLICIT_STRING_CAST_LOSS` | W1058 |
| `SYMBOL_PLATFORM` | W1002 | | `BAD_GLOBAL_SYMBOL` | W1028 | | `EXPLICIT_STRING_CAST` | W1059 |
| `SYMBOL_EXPERIMENTAL` | W1003 | | `DUPLICATE_CTOR_DTOR` | W1029 | | `EXPLICIT_STRING_CAST_LOSS` | W1060 |
| `UNIT_LIBRARY` | W1004 | | `INVALID_DIRECTIVE` | W1030 | | `CVT_WCHAR_TO_ACHAR` | W1061 |
| `UNIT_PLATFORM` | W1005 | | `PACKAGE_NO_LINK` | W1031 | | `CVT_NARROWING_STRING_LOST` | W1062 |
| `UNIT_DEPRECATED` | W1006 | | `PACKAGED_THREADVAR` | W1032 | | `CVT_ACHAR_TO_WCHAR` | W1063 |
| `UNIT_EXPERIMENTAL` | W1007 | | `IMPLICIT_IMPORT` | W1033 | | `CVT_WIDENING_STRING_LOST` | W1064 |
| `HRESULT_COMPAT` | W1008 | | `HPPEMIT_IGNORED` | W1034 | | `LOST_EXTENDED_PRECISION` | W1066 |
| `HIDING_MEMBER` | W1009 | | `NO_RETVAL` | W1035 | | `LNKDFM_NOTFOUND` | W1067 |
| `HIDDEN_VIRTUAL` | W1010 | | `USE_BEFORE_DEF` | W1036 | | `IMMUTABLE_STRINGS` | W1068 |
| `GARBAGE` | W1011 | | `FOR_LOOP_VAR_UNDEF` | W1037 | | `MOBILE_DELPHI` | W1069 |
| `BOUNDS_ERROR` | W1012 | | `UNIT_NAME_MISMATCH` | W1038 | | `UNSAFE_VOID_POINTER` | W1070 |
| `ZERO_NIL_COMPAT` | W1013 | | `NO_CFG_FILE_FOUND` | W1039 | | `IMPLICIT_INTEGER_CAST_LOSS` | W1071 |
| `STRING_CONST_TRUNCED` | W1014 | | `IMPLICIT_VARIANTS` | W1040 | | `IMPLICIT_CONVERSION_LOSS` | W1072 |
| `FOR_LOOP_VAR_VARPAR` | W1015 | | `UNICODE_TO_LOCALE` | W1041 | | `COMBINING_SIGNED_UNSIGNED64` | W1073 |
| `TYPED_CONST_VARPAR` | W1016 | | `LOCALE_TO_UNICODE` | W1042 | | `UNKNOWN_CUSTOM_ATTRIBUTE` | W1074 |
| `ASG_TO_TYPED_CONST` | W1017 | | `IMAGEBASE_MULTIPLE` | W1043 | | `OVERLOADING_ARRAY_PROPERTY` | W1075 |
| `CASE_LABEL_RANGE` | W1018 | | `SUSPICIOUS_TYPECAST` | W1044 | | `POINTER_CAST_NARROWER` | W1076 |
| `FOR_VARIABLE` | W1019 | | `PRIVATE_PROPACCESSOR` | W1045 | | `POINTER_CAST_MIGRATION` | W1077 |
| `CONSTRUCTING_ABSTRACT` | W1020 | | `UNSAFE_TYPE` | W1046 | | `POPOPT_WITH_NO_MATCHING` | W1078 |
| `COMPARISON_FALSE` | W1021 | | `UNSAFE_CODE` | W1047 | | `MISSING_POPOPT` | W1079 |
| `COMPARISON_TRUE` | W1022 | | `UNSAFE_CAST` | W1048 | | `INVALID_NORETURN` | W1080 |
| `COMPARING_SIGNED_UNSIGNED` | W1023 | | `OPTION_TRUNCATED` | W1049 | | `XML_WHITESPACE_NOT_ALLOWED` | W1201 |
| `COMBINING_SIGNED_UNSIGNED` | W1024 | | `WIDECHAR_REDUCED` | W1050 | | `XML_UNKNOWN_ENTITY` | W1202 |
| `UNSUPPORTED_CONSTRUCT` | W1025 | | `DUPLICATES_IGNORED` | W1051 | | `XML_INVALID_NAME_START` | W1203 |
| | | | `MESSAGE_DIRECTIVE` | W1054 | | `XML_INVALID_NAME` | W1204 |
| | | | `TYPEINFO_IMPLICITLY_ADDED` | W1055 | | `XML_EXPECTED_CHARACTER` | W1205 |
| | | | `RLINK_WARNING` | W1056 | | `XML_CREF_NO_RESOLVE` | W1206 |
| | | | | | | `XML_NO_PARM` | W1207 |
| | | | | | | `XML_NO_MATCHING_PARM` | W1208 |

**Hints are not in this table and are not `$WARN`-controllable.** They are all-or-nothing via `{$HINTS}` /
`-H`. There is no docwiki page listing hint codes; the four in the previous section came from the compiler.

---

## 4. Warnings as errors

There is **no master "treat warnings as errors" switch** in the `.dproj` — `DCC_Warnings` and `DCC_Hints`
only turn output on and off. You promote warnings one at a time, which is the right granularity anyway.

Two ways, both verified against `dcc32` 37.0:

```delphi
{$WARN NO_RETVAL ERROR}       // in the unit, or in a shared .inc every unit includes
```

```bash
dcc32.exe -W^NO_RETVAL MyProject.dpr    # -W[+|-|^][warn_id]:  + on, - off, ^ error
```

Either way the message changes category and the build stops:

```text
W4.dpr(6) Warning: W1035 Return value of function 'NoRet' might be undefined     # before
W4.dpr(6) Error:   E1035 Return value of function 'NoRet' might be undefined     # after
```

The code keeps its number and changes its letter. Grep your CI log for `Error: E10` and you have a gate.

**Where to start on an existing codebase** — these three are almost never false positives and each one is a
real runtime defect:

```delphi
{$WARN NO_RETVAL ERROR}          // W1035 — function returns garbage
{$WARN USE_BEFORE_DEF ERROR}     // W1036 — local read before written
{$WARN FOR_LOOP_VAR_UNDEF ERROR} // W1037 — loop counter read after the loop
```

Add `HIDDEN_VIRTUAL` (W1010) next; it is the one that produces "my override is never called".

Suppressing a warning is legitimate when you have understood it. Do it **narrowly and with a reason**, the
way the RTL does — turn it off for the lines that need it and back on immediately:

```delphi
{$WARN SYMBOL_DEPRECATED OFF}
  LegacyCall;                    // kept for the 10.4 build; removed when that target is dropped
{$WARN SYMBOL_DEPRECATED DEFAULT}
```

A `{$WARN ... OFF}` at the top of a unit with no comment is itself a smell: someone silenced a warning
rather than reading it.

---

## 5. Range and overflow checking — cheap runtime analysis

Not static, but it belongs in the same conversation because it is the same trade: turn it on in the debug
configuration and a whole class of silent corruption becomes a loud exception you can debug.

| Directive | Catches | Raises |
|-----------|---------|--------|
| `{$RANGECHECKS ON}` / `{$R+}` | array and string index out of bounds, out-of-range ordinal assignment | `ERangeError` |
| `{$OVERFLOWCHECKS ON}` / `{$Q+}` | integer arithmetic that wrapped | `EIntOverflow` |
| `{$ASSERTIONS ON}` / `{$C+}` | your own `Assert` conditions | `EAssertionFailed` |

**Do not assume the Debug configuration has them on** — check the project. Range and overflow checking in
particular are frequently off in both configurations, which means an out-of-bounds write has been silently
corrupting adjacent memory for as long as the code has existed, and the AV it eventually causes is nowhere
near the line at fault. Turning `{$R+}` and `{$Q+}` on in Debug for one run is a five-minute experiment that
regularly ends a multi-day hunt.

Note the corollary: if a bug appears *only* in Release, compare the two configurations' settings before
theorising. Different checking, different optimisation, different `Assert` behaviour — the code was equally
wrong in both, and one of them was hiding it.

---

## 6. Third-party static analysis

**Not verified against a local install.** These are commercial or external products; what follows is what
they are for, not a claim about their current switches, rule identifiers or versions. Check the vendor's
own documentation before quoting a flag to a user, and never invent a rule id.

| Tool | What it adds over the compiler |
|------|-------------------------------|
| **Pascal Analyzer** (Peganza) | Whole-program reports the compiler does not produce: unused units in `uses`, dead code across units, call trees, cyclomatic complexity, inconsistent parameter naming, `with` usage. The strongest "what is actually reachable" answer available for Delphi |
| **SonarDelphi** (community plugin for SonarQube) | Rule-based issues in a CI dashboard with history and a quality gate. Fits a team that already runs SonarQube for other languages |
| **FixInsight** | IDE-integrated defect rules aimed at exactly this skill's subject matter — lifetime and control-flow mistakes the compiler allows |
| **madExcept** | Runtime, not static: exception reports with stack traces, plus leak reporting. Complementary — it tells you *where* the AV happened in a shipped build. DUnitX ships stack-trace adapters for it (`DUnitX.StackTrace.MadExcept5.pas`), so it can also decorate test failures |
| **IDE Error Insight** | Live, in the editor, and free. It uses a different front end from the compiler, so it both misses things and reports things that compile fine. **Never report an Error Insight squiggle as a defect without compiling** |

Older RAD Studio releases carried a "Audits and Metrics" feature; whether it is present in the version the
user has is **not something to assert** — ask, or ignore it.

None of these replaces step 1. A codebase with 400 unread compiler warnings does not need another tool.
