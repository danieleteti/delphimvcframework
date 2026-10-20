# The catalogue

Every entry has four parts: **the bad code**, **what it costs at runtime**, **the fix**, **how to detect
it**. An entry with no detection method would be an opinion, and opinions do not belong in a review.

*Why* the lifetime ones are wrong is `delphi`, `reference/memory.md`; *why* the exception ones are wrong is
`delphi`, `reference/exceptions.md`; the threading semantics are `delphi`, `reference/concurrency.md`.
This file says how to find them.

Detection greps are written for `rg` (ripgrep). `-U` enables multiline. They are heuristics that produce a
shortlist to read — none of them is a verdict.

---

## Lifetime

### 1. Constructor inside the `try`

```delphi
// BROKEN
var lList: TStringList;
try
  lList := TStringList.Create;
  lList.LoadFromFile(APath);
finally
  lList.Free;          // if Create raised, this calls Destroy on uninitialised memory
end;
```

**Costs** an access violation in the `finally`, which then replaces the real exception — you lose both the
crash site and the original error.

**Fix** acquire, *then* `try`:

```delphi
var lList := TStringList.Create;
try
  lList.LoadFromFile(APath);
finally
  lList.Free;
end;
```

**Detect**

```bash
rg -n -U 'try\s*\n\s*\w+\s*:=\s*T\w+\.Create'
```

Also flagged by **W1036** (`Variable might not have been initialized`) when the variable is a plain local
and the compiler can see the path — but it does *not* fire in every shape, so grep as well.

---

### 2. No `try/finally` at all

```delphi
// BROKEN            (TReportBuilder = class ... procedure Build; ... end;)
procedure TReportBuilder.Build;
begin
  var lStream := TMemoryStream.Create;
  Render(lStream);          // raises on a malformed template
  lStream.SaveToFile(fPath);
  lStream.Free;             // never reached
end;
```

**Costs** a leak on every exception, and on every `Exit` someone adds later.

**Fix** wrap it. Two objects, two nested `try` blocks.

**Detect** — the highest-yield grep in this file. List every construction, then check each routine for a
`finally`:

```bash
rg -n ':=\s*T\w+\.Create'          # every construction
rg -n -U 'T\w+\.Create[^;]*;(?:(?!\bfinally\b|\bend;)[\s\S]){0,400}?\bend;'   # ...with no finally nearby
```

And at runtime: `ReportMemoryLeaksOnShutdown` names the class. See `reference/leaks.md`.

---

### 3. Create in a loop, free after it

```delphi
// BROKEN
var lItem: TOrderLine;
for var i := 0 to lRows.Count - 1 do
begin
  lItem := TOrderLine.Create;
  lItem.Parse(lRows[i]);
  fTotal := fTotal + lItem.Amount;
end;
lItem.Free;                 // frees the last one only
```

**Costs** `Count - 1` leaked objects. This is the leak that shows up as a class with a huge `x N` in the
shutdown report and as a service whose memory grows all day.

**Fix** the `try/finally` goes **inside** the loop body — or, better, put them in an owning
`TObjectList<TOrderLine>` and free that once.

**Detect**

```bash
rg -n -U '(for|while)[^\n]*\bdo\b[\s\S]{0,300}?\.Create[\s\S]{0,300}?\bend;\s*\n\s*\w+\.Free'
```

Read the report for a leak count much larger than 1 — that is almost always this.

---

### 4. `FreeAndNil` used to paper over a lifetime nobody understands

```delphi
// SMELL
finally
  FreeAndNil(lList);        // lList goes out of scope on the next line
end;

// WORSE
procedure TCache.Refresh;
begin
  FreeAndNil(fData);        // is anything else holding fData right now?
  fData := Load;
end;
```

**Costs** nothing directly — that is what makes it a smell rather than a bug. It signals *"this variable
may be read again"*, which is false for the local, and it hides the real question for the field: who else
has a pointer to the object you just destroyed? `FreeAndNil` nils **your** reference, not theirs. The AV
arrives later, somewhere else.

**Fix** on a local, plain `Free`. On a field, keep `FreeAndNil` only when something genuinely tests it for
`nil` later — and then answer the aliasing question explicitly.

**Detect**

```bash
rg -n 'FreeAndNil\s*\(\s*[lL][A-Z]'    # on an l-prefixed local
rg -n -U 'finally[\s\S]{0,80}?FreeAndNil'
```

---

### 5. Mixed object and interface reference to the same instance

```delphi
// BROKEN — freed twice
type TLogger = class(TInterfacedObject, ILogger) end;

var lObj := TLogger.Create;      // object reference, refcount still 0
try
  var lIntf: ILogger := lObj;    // refcount 1
  DoWork(lIntf);
finally
  lObj.Free;                     // ...and lIntf's release already destroyed it
end;
```

**Costs** a double free, or a dangling pointer in the mirror-image case where the interface goes out of
scope first. Both are AVs at an unrelated later line.

**Fix** pick one model and hold it from the moment of creation. Semantics: `delphi`,
`reference/memory.md`.

**Detect** — find classes that implement an interface, then look for `.Free` on a variable of that class
type in the same unit:

```bash
rg -n 'class\s*\(\s*TInterfacedObject\s*,'      # the classes at risk
rg -n '\.Free\b'                                 # ...and any Free in those units
```

Any overlap is a manual read. There is no compiler warning for this.

---

### 6. Two objects holding interfaces to each other

```delphi
// BROKEN — neither is ever released
type
  TParent = class(TInterfacedObject, IParent)
    fChild: IChild;         // Parent keeps Child alive
  end;
  TChild = class(TInterfacedObject, IChild)
    fParent: IParent;       // ...and Child keeps Parent alive
  end;
```

**Costs** a leak that no amount of correct `try/finally` will fix, because nothing is wrong with the
`try/finally`. Both refcounts sit at 1 forever.

**Fix** make the back-reference not count. `[weak]` and `[unsafe]` are real attributes —
`WeakAttribute` and `UnsafeAttribute` are declared in `System.pas` and both are used in the RTL
(`System.JSON.Builders.pas` uses `[weak]`, `System.Generics.Collections.pas` uses `[unsafe]`):

```delphi
  TChild = class(TInterfacedObject, IChild)
    [weak] fParent: IParent;    // does not add a reference; nils itself when the target dies
  end;
```

`[unsafe]` also skips the refcount but does **not** get nilled — it is the faster, sharper one. Prefer
`[weak]` unless you can prove the target outlives the reference.

Verified on Win32 with `dcc32` 37.0: without `[weak]` the pair leaks and neither destructor runs; with
`[weak]` both destructors run at the expected point.

**Detect** — the shutdown report is unusually good at this one. **Two classes leaked in matching counts**
(`TChild x 1, TParent x 1`) is the signature. Then:

```bash
rg -n -U 'class\s*\([^)]*TInterfacedObject[^)]*\)[\s\S]{0,400}?f\w+\s*:\s*I[A-Z]\w+'
```

Interface-typed *fields* are where cycles live. Closure-captured `Self` is the same bug with a different
face — `delphi`, `reference/generics-and-rtti.md`.

---

### 7. A `function` that returns an object, and a caller who does not know they own it

```delphi
// SMELL             (TCustomerRepo = class ... function Find(...): TStringList; ... end;)
function TCustomerRepo.Find(const AName: string): TStringList;
begin
  Result := TStringList.Create;
  ...
end;

// at the call site, three months later:
if Repo.Find('acme').Count > 0 then    // leaked, and nobody can see it here
```

**Costs** a leak per call, and the call site reads as if it were free. The mirror failure is the caller who
*does* free something the callee still owns — a double free.

**Fix**, in order of preference: return an **interface** (refcounting makes the question disappear); or
have the caller pass in the container to fill (`procedure FillInto(ATarget: TStrings)`); or, if it must
return an object, **say so in the name** — `CreateX`, `ExtractX` — the way the RTL does with
`TObjectList<T>.Extract`.

**Detect**

```bash
rg -n -U 'function\s+\w+(\.\w+)?\([^)]*\)\s*:\s*T[A-Z]\w+;[\s\S]{0,200}?Result\s*:=\s*T\w+\.Create'
rg -n '\.\w+\([^)]*\)\.\w+'        # method call chained straight off a call - candidate leaked temporary
```

---

### 8. A global or singleton that never dies

```delphi
// SMELL
initialization
  GCache := TDictionary<string, TObject>.Create;
  // no finalization
```

**Costs** two things. It is itself a leak the shutdown report will print. Worse, a report with a dozen of
these becomes noise nobody reads, which is how the *real* leak survives. And a singleton that owns other
objects hides their lifetimes inside itself, so nothing else in the report is trustworthy either.

**Fix** free it in the same unit's `finalization`. If it genuinely must outlive shutdown, call
`RegisterExpectedMemoryLeak` on it so the report stays clean and the decision is written down.

**Detect**

```bash
rg -n -U 'initialization[\s\S]{0,400}?\.Create[\s\S]*?\bend\.'   # then check for a finalization
rg -n -c 'finalization'
```

---

### 9. An event handler or observer never unsubscribed

```delphi
// BROKEN     (TFeed = class ... OnTick: TNotifyEvent ... end;  TChartPanel = class(TPanel) fFeed: TFeed; end;)
constructor TChartPanel.Create(AOwner: TComponent);
begin
  inherited;
  fFeed := GlobalFeed;
  fFeed.OnTick := HandleTick;     // fFeed outlives this panel
end;
// destructor does not clear it
```

**Costs** an access violation the next time the publisher fires, into a method of a freed object. It
reproduces only when the subscriber dies first, which is why it ships.

**Fix** unsubscribe in the destructor:

```delphi
destructor TChartPanel.Destroy;
begin
  if Assigned(fFeed) and (TMethod(fFeed.OnTick).Data = Self) then
    fFeed.OnTick := nil;
  inherited;
end;
```

For components, `TComponent.FreeNotification` / `RemoveFreeNotification` (`System.Classes`) is the built-in
version of the same idea: the referencing component is told through `Notification(…, opRemove)` when the
target dies.

**Detect**

```bash
rg -n ':=\s*(Self\.)?\w*(Handle|On)\w*;'      # assignments of a method to an event property
rg -n 'On[A-Z]\w*\s*:=\s*\w+;'
```

For each hit, check the owning class's destructor for the matching `:= nil`. Absence is the finding.

---

## Exceptions and control flow

### 10. `except end`

```delphi
// BROKEN
try
  fConnection.Commit;
except
end;
```

**Costs** the diagnosis. The transaction did not commit, nothing was logged, and the program continues as
though it had. Every downstream symptom will be investigated in the wrong place.

**Fix** handle one named class and say why, or log and re-`raise` with a bare `raise`.

**Detect**

```bash
rg -n -U 'except\s*(//[^\n]*\n\s*)*end'
rg -n -U 'except[\s\S]{0,40}?\bend\b'          # near-empty handlers too
```

---

### 11. `try/except` where `try/finally` was meant

```delphi
// BROKEN
var lStream := TFileStream.Create(APath, fmOpenRead);
try
  Load(lStream);
except
  lStream.Free;      // freed only when something raised
  raise;
end;
```

**Costs** a leak on the **success** path — the common path, so the leak is proportional to how well the
program is working.

**Fix** `finally`. If you need both, nest: `finally` outside, `except` inside.

**Detect**

```bash
rg -n -U 'except[\s\S]{0,120}?\.Free'
```

Every hit is either this bug or a deliberate and unusual construction. Read all of them.

---

### 12. An empty `except` in a destructor

```delphi
// BROKEN     (TTransport = class ... procedure Close; ... end;  TSession = class ... fTransport: TTransport; ... end;)
destructor TSession.Destroy;
begin
  try
    fTransport.Close;
  except
  end;
  fTransport.Free;
  inherited;
end;
```

**Costs** worse than a normal swallowed exception: a destructor that hides its own failure will happily
leave half the object's resources unreleased and report success. If the `except` also skips the `Free`, it
leaks silently every time.

**Fix** if `Close` can legitimately fail, catch it, **log it**, and continue to the `Free`. Never let the
handler skip the release, and never let the destructor raise (an exception escaping a `finally` replaces
the one in flight — `delphi`, `reference/exceptions.md`).

**Detect**

```bash
rg -n -U 'destructor[\s\S]{0,400}?except\s*(//[^\n]*\n\s*)*end'
```

---

### 13. `Result` not assigned on every path

```delphi
// BROKEN
function TryLoad(const APath: string): Boolean;
begin
  if not TFile.Exists(APath) then
    Exit;                 // Result never assigned
  ...
  Result := True;
end;
```

**Costs** the caller branches on whatever was in the register. Non-deterministic, build-dependent, and it
will behave differently in Release.

**Fix** `Exit(False)`, or assign `Result` first thing.

**Detect** **W1035**. Promote it to an error and it can never happen again:
`{$WARN NO_RETVAL ERROR}` — see `reference/static-analysis.md`.

---

### 14. A discarded function result

```delphi
// BROKEN — compiles clean, no warning, no hint
TryStrToInt(lText, lValue);
Writeln(lValue);            // lValue was never written
```

Verified with `dcc32` 37.0: a function called as a statement produces **no diagnostic at all**.

**Costs** silently wrong data. The `Try…` family, `Supports`, `ITask.Wait`, and every
`function …: Boolean` that reports success are all discardable this way.

**Fix** branch on it. That is what it is for.

**Detect** — the compiler will not help, so grep for the known offenders at statement position:

```bash
rg -n '^\s*(TryStrTo\w+|TryEncode\w+|Supports)\s*\(' 
rg -n '^\s*\w+\.(Wait|TryEnter|TryGetValue|Extract)\s*\('
```

---

### 15. A magic literal used as a status code

```delphi
// SMELL
if lOrder.State = 3 then
  Ship(lOrder);
```

**Costs** nothing today and a wrong branch after the next schema change. The literal carries no type, so
nothing checks it, and the day someone inserts a state the code silently ships cancelled orders.

**Fix** a scoped enumeration, or at minimum a named constant. `delphi`, `reference/style.md` for the
naming.

**Detect**

```bash
rg -n '(State|Status|Kind|Mode|Result|Code)\w*\s*(=|<>|:=)\s*-?\d+'
rg -n 'case\s+\w+\s+of[\s\S]{0,200}?^\s*\d+\s*:'
```

Ignore `0`, `1` and `-1` used as counts or indexes; the finding is a domain value written as a bare integer.

---

## Structure

### 16. `with`

```delphi
// BROKEN
with lCustomer do
begin
  Name := ANameField;      // whose Name? whose ANameField?
  Save;
end;
```

**Costs** silent capture. Adding a field to `TCustomer` later can hijack an identifier that resolved to the
enclosing scope, and the code still compiles and now writes to the wrong place. Debuggers cannot resolve
the names either.

**Fix** a local variable. Never generate `with` in new code.

**Detect**

```bash
rg -n '\bwith\b[^\n]*\bdo\b'
```

Nested `with`s (`with A, B do`) are the acute form — report those first.

---

### 17. A method that silently hides an ancestor's virtual

```delphi
// BROKEN
TBase = class
  procedure Execute(AIndex: Integer); virtual;
end;
TDerived = class(TBase)
  procedure Execute(AIndex: Integer);   // no "override" — a NEW method
end;
```

**Costs** the ancestor keeps calling its own implementation. "My override is never called" is this, almost
every time.

**Fix** `override` when you meant to replace it, `reintroduce` when the hiding is deliberate, `overload`
when the signature differs on purpose.

**Detect** **W1010** (`Method '…' hides virtual method of base type '…'`). Promote it:
`{$WARN HIDDEN_VIRTUAL ERROR}`. `HIDING_MEMBER` (W1009) covers the non-virtual case.

---

### 18. String concatenation in a loop

```delphi
// BROKEN
for var i := 0 to 50000 do
  lCsv := lCsv + lRows[i] + ',';
```

**Costs** O(n²) — every `+` allocates a new string and copies everything so far. At 50 000 rows this is the
difference between milliseconds and a hang the user reports as a freeze.

**Fix** `TStringBuilder` (`System.SysUtils`), or `TStringList.Add` plus `.Text` when the pieces are lines.
Below a few hundred appends it does not matter; do not "fix" those.

**Detect**

```bash
rg -n -U '(for|while)[^\n]*\bdo\b[\s\S]{0,200}?(\w+)\s*:=\s*\2\s*\+'
```

---

## Concurrency

### 19. A worker thread touching the UI

```delphi
// BROKEN
procedure TImportThread.Execute;
begin
  for var lRow in fRows do
  begin
    Import(lRow);
    ProgressBar1.Position := ...;    // VCL, from a worker thread
  end;
end;
```

**Costs** access violations and painting corruption that reproduce roughly once a week, on somebody else's
machine. A `TTimer` handler is main-thread and is fine; a `TThread` or a `TTask` body is not.

**Fix** `TThread.Queue` for progress, `TThread.Synchronize` when you need the answer back.
`delphi`, `reference/concurrency.md`.

**Detect** — make it fail loudly first (available since Delphi 11 Alexandria, `Vcl.Controls`):

```delphi
TControl.RaiseOnNonMainThreadUsage := True;    // in the debug build
```

then grep the bodies:

```bash
rg -n -U 'procedure\s+\w+\.Execute;[\s\S]{0,800}?(Label|Edit|Memo|Grid|Progress|Form|Application|Screen)\w*\.'
rg -n -U 'TTask\.(Run|Create)[\s\S]{0,400}?(Label|Edit|Memo|Form)\w*\.'
```

---

### 20. A thread that outlives what it references

```delphi
// BROKEN
fThread := TThread.CreateAnonymousThread(
  procedure
  begin
    fResults.Add(Fetch);      // fResults belongs to a form that may already be closed
  end);
fThread.FreeOnTerminate := True;
fThread.Start;
```

**Costs** an access violation at shutdown, or worse, a write into freed memory that corrupts something
else and crashes elsewhere. With `FreeOnTerminate := True` the thread reference is dangling the instant
`Execute` returns, so you cannot even `WaitFor` it to find out.

**Fix** either own the thread (`FreeOnTerminate := False`, `Terminate`, `WaitFor`, `Free` in the owner's
destructor), or capture only values the closure can own. `TThread.RemoveQueuedEvents` in the destructor for
anything already queued.

**Detect**

```bash
rg -n 'FreeOnTerminate\s*:=\s*True'
```

For each hit: is the thread variable used after `Start`? Does the closure capture a field or `Self`? Both
are findings. Then check every destructor of a class that owns a thread for a `Terminate` + `WaitFor` pair:

```bash
rg -n -U 'destructor[\s\S]{0,400}?fThread' 
```

---

## What not to report

- Naming that is consistent within the file, even if it is not the guide's. `delphi`,
  `reference/style.md`: the file you are editing wins.
- Method length, in isolation. A 200-line `case` that is a dispatch table is fine.
- `Free` versus `FreeAndNil` on a field where the choice is defensible.
- Anything you found by pattern-matching and did not read.

A review that ends with "no lifetime or exception defects found; three style notes below" is a good review.
A review that ends with thirty style notes and no lifetime finding is one that never opened the destructors.
