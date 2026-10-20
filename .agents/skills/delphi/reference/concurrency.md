# Delphi — threads, tasks and the UI rule

## The rule that comes first

**The VCL is single-threaded. Only the main thread may touch a control, a form, `Application` or `Screen`.**
There is no exception and no "it usually works" — a background thread writing `Label1.Caption` produces
access violations that reproduce once a week on the customer's machine.

Make the violation loud in your debug build (`Vcl.Controls`, available since **11 Alexandria**):

```delphi
TControl.RaiseOnNonMainThreadUsage := True;
```

It raises when a `TWinControl` handle is created off the main thread.

---

## `TThread` — `System.Classes`

```delphi
constructor Create(CreateSuspended: Boolean); overload;
procedure Start;
procedure Terminate;                            // sets Terminated; it does NOT kill anything
function WaitFor: LongWord;
property Terminated: Boolean read FTerminated;
property FreeOnTerminate: Boolean read FFreeOnTerminate write SetFreeOnTerminate;
property OnTerminate: TNotifyEvent read FOnTerminate write FOnTerminate;
class property CurrentThread: TThread read GetCurrentThread;    // "Current" is the same property
class function CreateAnonymousThread(const ThreadProc: TProc): TThread; static;
class procedure NameThreadForDebugging(const AThreadName: string; AThreadID: TThreadID = TThreadID(-1));
class function GetTickCount64: UInt64; static;
```

```delphi
type
  TImportThread = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TImportThread.Execute;
begin
  NameThreadForDebugging('Import');
  while not Terminated do
  begin
    var lRow := FetchNext;
    if lRow = nil then
      Break;
    Process(lRow);
  end;
end;
```

`Terminate` only sets a flag. **Your `Execute` must poll `Terminated`**, or the thread runs to completion
regardless. A long blocking call inside the loop makes shutdown take as long as that call.

For a one-off job, skip the class:

```delphi
var lThread := TThread.CreateAnonymousThread(
  procedure
  begin
    var lResult := ExpensiveWork;
    TThread.Queue(nil,
      procedure
      begin
        Memo1.Lines.Add(lResult);       // main thread
      end);
  end);
lThread.FreeOnTerminate := True;        // CreateAnonymousThread already sets this
lThread.Start;
```

### `FreeOnTerminate` — and why you cannot then touch the reference

With `FreeOnTerminate = True` the thread object destroys itself the moment `Execute` returns. The variable
holding it is dangling from that instant. **You cannot `WaitFor` it, read its fields, or `Free` it.** If you
need the result or a clean shutdown, keep `FreeOnTerminate = False`, `WaitFor`, then `Free`.

### Getting back to the main thread

```delphi
class procedure Synchronize(const AThread: TThread; AThreadProc: TThreadProcedure); overload; static;
class procedure Queue(const AThread: TThread; AThreadProc: TThreadProcedure); overload; static;
class procedure ForceQueue(const AThread: TThread; const AThreadProc: TThreadProcedure; ADelay: Integer = 0); overload; static;
class procedure RemoveQueuedEvents(const AThread: TThread); overload; static;
```

`TThreadProcedure = reference to procedure`, so you pass an anonymous method. `nil` as the first argument
means "not associated with a particular thread".

| Call | Behaviour |
|------|-----------|
| `Synchronize` | **blocks** the calling thread until the main thread has run the block |
| `Queue` | posts and returns immediately; runs on the main thread later. If called *from* the main thread it executes immediately |
| `ForceQueue` | always posts, even from the main thread; optional delay in ms |

Prefer `Queue` for progress and logging — `Synchronize` in a tight loop serialises your worker against the
UI and is a classic cause of "the background job is slower than doing it inline". Use `Synchronize` when
you need the main thread's answer before continuing.

**A `Queue`d closure can outlive the object it captured.** If the form closes before the block runs, the
block touches a freed form. `RemoveQueuedEvents(AThread)` in the form's destructor, or capture only values.

---

## `System.Threading` — tasks

```delphi
TTaskStatus = (Created, WaitingToRun, Running, Completed, WaitingForChildren, Canceled, Exception);

ITask = interface
  function Start: ITask;
  procedure Cancel;
  function Wait(Timeout: Cardinal = INFINITE): Boolean; overload;
  property Status: TTaskStatus read GetStatus;
end;
```

```delphi
class function TTask.Run(const Func: TProc): ITask; overload; static;     // create + start
class function TTask.Create(const Proc: TProc): ITask; overload; static;  // create, you Start it
class function TTask.WaitForAll(const Tasks: array of ITask): Boolean; overload; static;
class function TTask.WaitForAny(const Tasks: array of ITask): Integer; overload; static;
class function TTask.Future<T>(const Func: TFunc<T>): IFuture<T>; overload; static;
```

```delphi
var lTask := TTask.Run(
  procedure
  begin
    DoWork;
  end);
// ... later
lTask.Wait;

// A value coming back
var lFuture := TTask.Future<Integer>(
  function: Integer
  begin
    Result := CountRows;
  end);
var lCount := lFuture.Value;      // blocks until ready; re-raises the task's exception here
```

Three things about tasks:

- **An exception inside a task is swallowed until you wait.** `Wait`, `WaitForAll` or `IFuture.Value`
  re-raise it (wrapped). A fire-and-forget `TTask.Run` whose body raises loses the error entirely — wrap
  the body in `try/except` and log.
- `Cancel` is **cooperative**: it sets the status. There is no way to stop running code. Poll
  `TTask.CurrentTask.Status` or your own flag.
- Tasks run on `TThreadPool.Default`. A task that blocks (I/O, a lock, `Sleep`) occupies a pool thread; a
  poolful of blocked tasks deadlocks work that is queued behind them. Give blocking work its own
  `TThreadPool`, or a plain `TThread`.

### `TParallel`

```delphi
TParallel.&For(1, 1000,
  procedure(AIndex: Integer)
  begin
    Process(AIndex);
  end);
```

`For` is a reserved word, hence the `&` escape — `TParallel.&For`, not `TParallel.For`. It returns a
`TLoopResult`. The body runs on many threads at once: it must not touch shared mutable state without a
lock, and it must not touch the UI. Only worth it when the per-item work is substantial; the scheduling
overhead swamps a cheap loop body.

---

## Locking

| Primitive | Unit | Use for |
|-----------|------|---------|
| `TMonitor` | `System` (implicit) | The cheapest lock. Locks **any `TObject`** with no extra field |
| `TCriticalSection` | `System.SyncObjs` | An explicit lock object; `Enter`/`Leave`/`TryEnter`/`Acquire`/`Release` |
| `TLightweightMREW` | `System.SyncObjs` | Multi-read/exclusive-write, as a record (no heap object) |
| `TInterlocked` | `System.SyncObjs` | `Increment`, `Decrement`, `Add`, `Exchange`, `CompareExchange` — atomic, no lock |
| `TEvent` / `TSimpleEvent` | `System.SyncObjs` | Signalling between threads |
| `TSemaphore`, `TCountdownEvent` | `System.SyncObjs` | Bounded concurrency, fan-in |
| `TThreadList<T>` | `System.Generics.Collections` | `LockList` / `UnlockList` around a `TList<T>` |

```delphi
// TMonitor — no extra field, no construction
TMonitor.Enter(fCache);
try
  fCache.AddOrSetValue(lKey, lValue);
finally
  TMonitor.Exit(fCache);
end;
```

`TMonitor` also has `TryEnter`, `Wait`, `Pulse`, `PulseAll` — the full condition-variable set — and
`SetSpinCount`. Note `TMonitor.Exit` shadows the `Exit` standard procedure inside that scope; that is why
it is always written qualified.

```delphi
// TCriticalSection — one instance, created once, freed once
fLock := TCriticalSection.Create;
...
fLock.Enter;
try
  ...
finally
  fLock.Leave;
end;
```

Rules that prevent the deadlocks:

- **Always release in a `finally`.** A raise inside a critical section that does not unlock hangs every
  other thread, permanently.
- **Take multiple locks in one global order**, everywhere, or you have written a deadlock.
- **Never call `Synchronize` while holding a lock** the main thread also takes. That is the textbook
  deadlock in a VCL app.
- `TInterlocked.Increment(fCounter)` beats a lock for a counter, by a lot.
- A `TCriticalSection` protects the data you use it for, not "the object". Document which fields it covers.

---

## Reference counting is not thread safe by itself

`_AddRef`/`_Release` on interfaces are atomic, so passing an interface between threads is safe. But **the
variable holding it is not**: two threads assigning to the same `ILogger` field race. Copy an interface
into a local before use, and guard the shared field.

Strings and dynamic arrays are copy-on-write with an atomic refcount — reading the same `string` from many
threads is fine; assigning to a shared `string` variable from many threads is not.

---

## Checklist

- [ ] Does any background code touch a control, `Application`, or `Screen`?
- [ ] Does `Execute` poll `Terminated`?
- [ ] Is a `FreeOnTerminate` thread's reference used after `Start`?
- [ ] Is every `Enter`/`Acquire` matched by a `finally Leave`/`Release`?
- [ ] Can a `Queue`d closure outlive the object it captured?
- [ ] Does any `TTask.Run` body swallow its exception?
- [ ] Does any pooled task block, and can the pool starve because of it?
