# Finding a leak, proving it, and stopping it coming back

**This file is diagnosis only.** Why a given construct leaks — `Free`, `try/finally`, refcounting, the
mixed object/interface bug, container and `TComponent` ownership — is `delphi`, `reference/memory.md`.
Who frees the object a DMVCFramework action returns is `dmvcframework` SKILL.md. Neither is repeated here.

Everything below was read out of the shipped source on this machine
(`…\Studio\37.0\source\rtl\sys\System.pas`, `…\rtl\sys\getmem.inc`, `…\source\DunitX\`) or produced by
running a program compiled with `dcc32` 37.0.

---

## 1. What you already have

The Windows memory manager that ships in the RTL **is FastMM**. `getmem.inc` says so in its own copyright
string:

```text
'FastMM Embarcadero Edition (c) 2004 - 2011 Pierre le Riche'
```

So leak *detection* needs no download. What the shipped edition does not have is the full version's
FullDebugMode instrumentation — see §5.

The API surface in `System.pas`:

```delphi
var
  ReportMemoryLeaksOnShutdown: Boolean;

function RegisterExpectedMemoryLeak(P: Pointer): Boolean; platform;
function UnregisterExpectedMemoryLeak(P: Pointer): Boolean; platform;
procedure GetMemoryManagerState(var AMemoryManagerState: TMemoryManagerState); platform;
```

---

## 2. `ReportMemoryLeaksOnShutdown`

Set it **first**, before anything allocates — the first statement of the `.dpr`'s main block:

```delphi
begin
  ReportMemoryLeaksOnShutdown := True;
  ...
end.
```

The RTL's own comment on the declaration carries the one caveat that bites people:

> *This setting has no effect if this module is sharing a memory manager owned by another module.*

So: a DLL, a BPL, or anything using `System.Sharemem` / `System.SimpleShareMem` reports through the
**owning** module, not through itself. Set it in the host `.exe`, not in the library, and do not conclude
"no leaks" from a silent DLL.

### What the report actually looks like

Measured, from a console program that leaked one `TStringList`:

```text
Unexpected Memory Leak
An unexpected memory leak has occurred. The unexpected small block leaks are:

13 - 20 bytes: Unknown x 1
69 - 76 bytes: TStringList x 1
```

On a **console** app it goes to stderr; on a GUI app it is a `MessageBoxA` titled `Unexpected Memory Leak`
(`getmem.inc`, `ShowMessage`, branching on `IsConsole`). CI that runs a GUI test host will therefore hang
on a modal dialog instead of failing — one more reason to test in a console runner.

### What it tells you, and what it does not

| It gives you | It does not give you |
|--------------|---------------------|
| The **class name** of leaked small blocks, and a count | Where it was allocated. No stack trace, no unit, no line |
| A size bucket per class | Which of the 40 places that construct a `TStringList` was the one |
| For medium and large blocks: **sizes only** (`The sizes of unexpected leaked medium and large blocks are:`) | Any class name at all for those |
| `Unknown`, `AnsiString`, `UnicodeString` for non-object blocks | Which object owned that string |
| One aggregate at process exit | *When* it leaked, or whether it leaked once or 10 000 times |

It is a **smoke alarm, not a diagnosis**: it tells you the building is on fire and nothing about which room.
Two further blunt edges worth knowing before you trust a silent run:

- **A crash means no report.** The report runs during normal shutdown. If the process dies, you learn nothing.
- **A global never freed is a leak.** An object created in a unit's `initialization` and never destroyed is
  still allocated at exit and appears in the report. That is correct — but a codebase with a dozen of them
  produces a report nobody reads, which is how the real leak stays hidden. See `reference/smells.md`.

### Silencing a leak you have decided to keep

```delphi
RegisterExpectedMemoryLeak(fSingleton);
```

Use it for a deliberately immortal allocation, and only after you have written down why. Every registered
leak is one the report will never tell you about again, including the day it becomes a real one.

---

## 3. Narrowing it down without a stack trace

The shipped manager gives you no allocation site, so you bisect. In order of speed:

1. **Read the class name.** Half the time it names the type and there is only one place that creates it.
2. **`grep` for every `.Create` of that class** and check each against `reference/smells.md` entries 1–3.
   A `Create` with no `try/finally` in the same routine is the answer more often than not.
3. **Bisect by feature.** Run the program doing nothing, then one operation, then two. The report changes
   the moment you touch the offending path.
4. **Measure a delta yourself** around a suspect block, using the same API DUnitX uses:

```delphi
// GetMemoryManagerState, TMemoryManagerState and TSmallBlockTypeState are all in the
// implicit System unit - nothing to add to uses.

function AllocatedBytes: Int64;
var
  lState: TMemoryManagerState;
  lBlock: TSmallBlockTypeState;
begin
  GetMemoryManagerState(lState);
  Result := lState.TotalAllocatedMediumBlockSize + lState.TotalAllocatedLargeBlockSize;
  for lBlock in lState.SmallBlockTypeStates do
    Result := Result + Int64(lBlock.UseableBlockSize * lBlock.AllocatedBlockCount);
end;
```

(Body copied from `DUnitX.MemoryLeakMonitor.FastMM4.pas`, `GetMemoryAllocated`.) Call it either side of the
suspect code and print the difference. Crude, exact, and it works in a release build.

5. **Only then** reach for FullDebugMode (§5). It is the tool that answers "where was this allocated", and
   it costs an external dependency and a slower build.

---

## 4. Leaks in tests — the item that actually stops the regression

A leak that fails a test never reaches production. DUnitX supports this, and the shape of the support is
not what you would guess, so read this section rather than assuming.

### What DUnitX gives you

Verified in `…\Studio\37.0\source\DunitX\`:

```delphi
IMemoryLeakMonitor = interface
['{A374A4D0-9BF6-4E01-8A29-647F92CBF41C}']
  procedure PreSetup;   procedure PostSetUp;
  procedure PreTest;    procedure PostTest;
  procedure PreTearDown; procedure PostTearDown;
  function SetUpMemoryAllocated: Int64;
  function TearDownMemoryAllocated: Int64;
  function TestMemoryAllocated: Int64;
end;

IMemoryLeakMonitor2 = interface(IMemoryLeakMonitor)
['{33559983-D522-4ED5-9B5E-AC9A055FA01A}']
  function GetReport: string;
end;
```

The runner calls the monitor around setup, test and teardown, and if the three deltas do not sum to zero it
records a result of type `TTestResultType.MemoryLeak` (`DUnitX.TestRunner.pas`, `CheckMemoryAllocations`).
That result is **not** `Pass` and **not** `Ignored`, so `IRunResults.AllPassed` becomes `False`
(`DUnitX.RunResults.pas`, `RecordResult`) — which in the standard runner sets the process exit code:

```delphi
if not results.AllPassed then
  System.ExitCode := EXIT_ERRORS;      // EXIT_ERRORS = 1, DUnitX.TestFramework
```

**That is the whole point: a leak becomes a red build.** `IRunResults.MemoryLeakCount` gives you the count
separately if you want to report it.

### The catch: it is off by default, and the default monitor is a no-op

`DUnitX.MemoryLeakMonitor.Default.pas` implements every method as an empty body and returns `0` from all
three `…MemoryAllocated` functions. It is registered automatically when nothing else has been
(`DUnitX.TestFramework.pas`). So out of the box **every test reports zero bytes leaked, always.**

The real monitors — `DUnitX.MemoryLeakMonitor.FastMM4.pas` and `…FastMM5.pas` — register themselves only
under `{$IFDEF USE_FASTMM4_LEAK_MONITOR}`, and `DUnitX.MemoryLeaks.inc` ships with that define commented
out, above this shipped comment:

> *Uncomment to use FastMM4 Memory Leak Tracking. NOTE : Memory leak tracking does not work very well at
> the moment, as it's reporting leaks when logging information during tests (calls to .Status etc).*

Take that warning at face value: expect false positives from tests that log.

### Turning it on

The monitor classes live in the *implementation* section of their units, so you cannot instantiate them
from outside. Register your own in the test project — the implementation is ten lines and is the one from
the shipped FastMM4 monitor:

```delphi
uses
  System.Classes, DUnitX.TestFramework, DUnitX.ServiceLocator;

type
  TLeakMonitor = class(TInterfacedObject, IMemoryLeakMonitor)
  private
    fPreSetup, fPostSetup, fPreTest, fPostTest, fPreTearDown, fPostTearDown: Int64;
    function GetMemoryAllocated: Int64;
  public
    procedure PreSetup;      procedure PostSetUp;
    procedure PreTest;       procedure PostTest;
    procedure PreTearDown;   procedure PostTearDown;
    function SetUpMemoryAllocated: Int64;
    function TearDownMemoryAllocated: Int64;
    function TestMemoryAllocated: Int64;
  end;

// ... PreTest sets fPreTest := GetMemoryAllocated, PostTest sets fPostTest, and
//     TestMemoryAllocated returns fPostTest - fPreTest.  Same for setup and teardown.
//     GetMemoryAllocated is the AllocatedBytes function in section 3.

// in the .dpr, before creating the runner.
// The container class was RENAMED between releases - see the note below:
//   13 Florence: TDUnitXServiceLocator     12 Athens: TDUnitXIoC
TDUnitXServiceLocator.DefaultContainer.RegisterType<IMemoryLeakMonitor>(
  function: IMemoryLeakMonitor
  begin
    Result := TLeakMonitor.Create;
  end);
```

(The `RegisterType<>` call is copied verbatim from the initialization section of
`DUnitX.MemoryLeakMonitor.FastMM4.pas`.)

**Check the container's name against the release you build with** (and if you do not know where that
release's sources are, ask the user for the path and record it as the `delphi` skill describes, in the
`<!-- delphi-local-sources -->` block). The DUnitX shipped with 13 Florence
declares `TDUnitXServiceLocator` in `DUnitX.ServiceLocator.pas`; the one shipped with 12 Athens declares
`TDUnitXIoC` in `DUnitX.IoC.pas`. `DefaultContainer.RegisterType<>` is called identically in both, so only
the class name changes. Both were read from `DUnitX.MemoryLeakMonitor.FastMM4.pas` in the two trees. On any
other release, grep that same file in your own `Studio\<n>.0\source\DunitX\` instead of trusting either
name. If instead your project compiles the DUnitX units from source, the
one-line alternative is to define `USE_FASTMM4_LEAK_MONITOR` and add
`DUnitX.MemoryLeakMonitor.FastMM4` to the `.dpr`'s `uses`.

Implement `IMemoryLeakMonitor2.GetReport` too if you want the failure message to say more than a byte count.

### Exempting a test

```delphi
[TestFixture]
[IgnoreMemoryLeaks]                 // whole fixture exempt
TCacheTests = class
published
  [Test]
  [IgnoreMemoryLeaks(False)]        // ...except this one, which is checked
  procedure Cache_Releases_Entries;
end;
```

`IgnoreMemoryLeaks` is declared in `DUnitX.Attributes.pas` and re-exported by `DUnitX.TestFramework.pas`;
`Create(const AIgnoreMemoryLeaks: Boolean = True)`. Fixture-level and test-level both work, and the
test-level one overrides the fixture. Shipped example: `…\DunitX\Tests\DUnitX.Tests.MemoryLeaks.pas`.

Use it for a genuinely immortal singleton, never to make a red test green.

### Belt and braces

Whatever you do about per-test monitoring, put this in the test `.dpr` as well:

```delphi
ReportMemoryLeaksOnShutdown := True;
```

It costs nothing and catches leaks in fixtures, in `initialization` blocks and in the runner setup that a
per-test delta cannot see. Note it will *not* fail the build on its own — it only prints. Only the DUnitX
monitor changes the exit code.

---

## 5. FastMM's full version — an external download

**The full FastMM is not in the Delphi source tree.** Searching
`…\Studio\37.0\source\` for `FastMM` finds only the C++ RTL allocator, the DUnit/DUnitX *adapter* units,
and the copyright line quoted in §1 — no `FastMM4.pas`. Treat it as a third-party dependency the user has
to obtain and add themselves.

What it is for, and the reason it exists at all: **FullDebugMode records a stack trace at every
allocation**, so the leak report names the call stack that created the object instead of just the class.
It also fills freed blocks with a known pattern and checks it, which turns use-after-free and double-free
from "an AV somewhere later" into an immediate, located failure.

That is the answer to the one question §2's report cannot answer. It costs a large slowdown and, in the
FastMM4 lineage, a support DLL alongside the executable. **Details of its options, defines and file layout
were not verified against a local install — read its own documentation, and do not quote a switch name to
a user from memory.**

---

## Checklist

- [ ] `ReportMemoryLeaksOnShutdown := True` as the first statement of the debug `.dpr`?
- [ ] Is the process a DLL/package sharing a memory manager — in which case that setting does nothing?
- [ ] Does the test project register a real `IMemoryLeakMonitor`, or is it running the no-op default?
- [ ] Does the CI runner check `AllPassed` / the exit code, so a `MemoryLeak` result actually fails?
- [ ] Is any `[IgnoreMemoryLeaks]` or `RegisterExpectedMemoryLeak` in the codebase still justified?
- [ ] Does the test host run as a console app, so the leak report cannot become a modal dialog on CI?
