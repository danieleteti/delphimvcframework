# Delphi — exceptions

## The hierarchy

`Exception` is declared in **`System.SysUtils`**, not `System`. Everything you raise should descend from it.

```
TObject
└── Exception                        System.SysUtils
    ├── EAbort                       Abort — swallowed silently by the VCL
    ├── EArgumentException
    │   ├── EArgumentOutOfRangeException
    │   ├── EArgumentNilException
    │   └── EInOutArgumentException
    ├── EAssertionFailed             a failed Assert
    ├── EConvertError                StrToInt and friends
    ├── EInvalidCast                 a failed "as"
    ├── EIntfCastError               a failed interface "as"
    ├── EListError                   index out of bounds
    ├── ENotSupportedException / ENotImplemented / EInvalidOpException
    ├── EOperationCancelled
    ├── EInOutError                  file I/O; has a Path field
    │   ├── EFileNotFoundException / EDirectoryNotFoundException
    │   └── EPathNotFoundException / EPathTooLongException
    ├── EOSError                     has ErrorCode; raised by RaiseLastOSError
    ├── EHeapException
    │   ├── EOutOfMemory
    │   └── EInvalidPointer          freeing a bad pointer
    ├── EMonitor / EMonitorLockException
    └── EExternal                    hardware / OS traps
        ├── EAccessViolation
        ├── EStackOverflow
        ├── EExternalException
        └── EIntError → EDivByZero, ERangeError, EIntOverflow
```

Define your own with a meaningful name and let it carry data:

```delphi
type
  EOrderNotFound = class(Exception)
  private
    fOrderID: Integer;
  public
    constructor Create(const AOrderID: Integer);
    property OrderID: Integer read fOrderID;
  end;

constructor EOrderNotFound.Create(const AOrderID: Integer);
begin
  inherited CreateFmt('Order %d not found', [AOrderID]);
  fOrderID := AOrderID;
end;
```

`Exception` constructors worth knowing (all in `System.SysUtils`):
`Create(const Msg: string)`, `CreateFmt(const Msg: string; const Args: array of const)`,
`CreateRes` / `CreateResFmt` (resourcestring), `CreateHelp` / `CreateFmtHelp`.

---

## Block syntax — and the thing that is a syntax error

**A single block cannot have both `except` and `finally`.** Nest them; `finally` outside, `except` inside
(or the reverse, depending on what you mean).

```delphi
lStream := TFileStream.Create(lPath, fmOpenRead);
try
  try
    Load(lStream);
  except
    on E: EReadError do
      raise EImportFailed.CreateFmt('Cannot read %s: %s', [lPath, E.Message]);
  end;
finally
  lStream.Free;
end;
```

`except` forms:

```delphi
except
  on E: EConvertError do  ...     // most specific first
  on E: Exception do      ...     // catch-all with the instance
else
  ...                             // catch-all without an instance; runs for non-Exception raises too
end;
```

- Handlers are tested **in order**, so list the derived class before its ancestor. Putting
  `on E: Exception` first makes every later handler dead code, and the compiler will not tell you.
- Inside an `on E: ... do`, **`E` is owned by the runtime**. Do not free it. It is destroyed when the
  handler exits — which is also why you must not store `E` for later.
- `except` with no `on` catches **everything**, including the `EAccessViolation` that means your code is
  broken and the `EOutOfMemory` that means the process is finished.

### Re-raise vs swallow

```delphi
except
  on E: Exception do
  begin
    Log(E.Message);
    raise;              // bare "raise" — re-raises THE SAME exception, original stack preserved
  end;
end;
```

`raise;` (no operand) is only legal inside an `except` block. `raise E;` re-raises the *same object* but
resets the raise address, so the debugger and any stack-trace provider point at your handler instead of the
real fault. Prefer the bare form.

**Never write `except end`.** It deletes the diagnosis and leaves the program in an undefined state. If you
genuinely want to ignore one specific failure, say which one and say why:

```delphi
try
  TFile.Delete(lTempPath);
except
  on EFileNotFoundException do ;   // already gone; nothing to do
end;
```

### Wrapping — keep the cause

```delphi
except
  on E: EDatabaseError do
    Exception.RaiseOuterException(EImportFailed.Create('Import failed'));
end;
```

`Exception.RaiseOuterException` (a `class procedure` on `Exception`) chains the exception in flight into the
new one's `InnerException`. `ThrowOuterException` is the identical C++-flavoured spelling. Read the chain
back with `InnerException` and `BaseException`; `StackTrace` is populated only if a stack-trace provider is
installed (`GetExceptionStackInfoProc`).

Without `RaiseOuterException`, a plain `raise ENew.Create(...)` inside a handler **loses the original
cause** — copy `E.Message` into the new message if you are not chaining.

### `raise ... at`

```delphi
raise EArgumentException.Create('bad AIndex') at ReturnAddress;
```

Attributes the exception to the **caller** rather than to the line inside your validation helper. That is
what makes an argument-check routine useful in a debugger. `ExceptAddr` and `ExceptObject` (both in
`System`) give you the current raise address / object inside a handler.

### `Abort` and `EAbort`

`Abort` (`System.SysUtils`) raises `EAbort`, which the VCL's default handler swallows without a dialog. Use
it to cancel a user-initiated operation. Never use it for an error.

---

## What NOT to catch

| Do not catch | Why |
|--------------|-----|
| `EAccessViolation`, `EStackOverflow`, other `EExternal` | The process state is already corrupt. Let it die where you can debug it |
| `EOutOfMemory` | Anything you do in the handler needs memory |
| `EAbort` | It exists to propagate quietly |
| A bare `except` around a whole method | You will hide the bug you are about to write |

Catch what you can **do something about**: a specific I/O failure, a specific parse failure, a specific
remote call. Everything else belongs to the caller, or to the top-level handler that logs and exits.

---

## `try/finally` is not error handling

`finally` is for **releasing**, not for deciding. It runs on the normal path and on the exception path, and
it must not raise — an exception escaping a `finally` replaces the one in flight and the original is lost.

```delphi
finally
  try
    lConn.Close;                 // Close can raise; do not let it eat the real error
  except
    on E: Exception do Log(E.Message);
  end;
  lConn.Free;
end;
```

An `Exit` inside a `try` still runs the `finally`. So does a `raise`. Nothing skips it except process death.

---

## Assertions

From the docwiki style guide: assertions state a condition that must hold **regardless of input** — a
precondition of a private helper, a class invariant. They can be compiled out (`{$ASSERTIONS OFF}` /
`{$C-}`), so they must never carry logic the program depends on.

```delphi
Assert(fItems <> nil, 'BuildIndex called before Initialize');
```

Anything that depends on user input, a file, the network or OS configuration is **not** an assertion —
test it and raise.
