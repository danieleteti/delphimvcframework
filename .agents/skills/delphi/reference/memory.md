# Delphi — memory and lifetime

Delphi has **three** lifetime models running at once. Almost every leak, double free and access violation
comes from treating a value as if it belonged to a different one.

| Model | Applies to | Freed by |
|-------|-----------|----------|
| Manual | `class` instances | you, exactly once |
| Reference counting | `interface` references, `string`, dynamic arrays | the compiler, when the last reference goes out of scope |
| Compiler-managed records | `record` with managed fields, or with `Initialize`/`Finalize` operators | the compiler, at end of scope, **even on an exception** |

There is no ARC for objects on any current platform. The mobile ARC compilers were removed in 10.4 Sydney;
`TObject.DisposeOf` survives only as `deprecated 'Use Free instead'` (`System.pas`). Do not write it.

---

## 1. Manual: `try/finally`, and the one place the `Create` goes

```delphi
// CORRECT
var lList := TStringList.Create;
try
  lList.Add('x');
finally
  lList.Free;
end;

// BROKEN — the constructor is inside the try
var lList: TStringList;
try
  lList := TStringList.Create;   // if this raises, lList is uninitialised garbage
  ...
finally
  lList.Free;                    // ...and this calls Destroy on it
end;
```

The community style guide reproduced in `style.md` contains exactly this bug in its own example. It is
wrong. Acquire, **then** `try`.

`Free` is nil-safe — `System.pas` implements it as `if Self <> nil then Destroy`. So:

```delphi
lObj.Free;                       // correct
if lObj <> nil then lObj.Free;   // noise
if Assigned(lObj) then lObj.Free;// noise
```

**Two objects, two `try` blocks** — or one block and a nil-initialised pair:

```delphi
var lA := TFoo.Create;
try
  var lB := TBar.Create;
  try
    ...
  finally
    lB.Free;
  end;
finally
  lA.Free;
end;
```

### `FreeAndNil` — and when it is a smell

`System.SysUtils`: `procedure FreeAndNil(const [ref] Obj: TObject);` — it nils the reference **first**, then
frees, so a re-entrant call cannot see a dangling pointer.

Use it when the reference **outlives the free** and something later tests it:

```delphi
procedure TEditor.CloseDocument;
begin
  FreeAndNil(fDocument);       // fDocument is checked for nil elsewhere — correct
end;
```

Do **not** use it on a local that is one line from going out of scope. It costs a write, and it signals
"this variable may be read again", which is false — that misdirection is the smell:

```delphi
finally
  FreeAndNil(lList);           // lList dies on the next line. Just lList.Free.
end;
```

The community style guide says "always use `FreeAndNil()` instead of `.Free`". That is one convention, not
a rule: the RTL itself uses plain `Free` in destructors and local blocks throughout. Neither the docwiki
style guide nor the RTL endorses "always".

Note the `[ref]` in the signature: `FreeAndNil` takes an untyped-by-reference parameter, so it accepts any
object variable — but **not a property**. `FreeAndNil(Self.SomeProperty)` does not compile.

### Destructors

```delphi
destructor TFoo.Destroy;
begin
  fOwned.Free;        // your own fields first
  inherited Destroy;  // then the ancestor
end;
```

`Destroy` also runs when the **constructor raised**. Delphi zeroes the instance memory before the
constructor body (`TObject.InitInstance`), so every field is `nil`/`0` and `fOwned.Free` on a
half-constructed object is safe — *provided you do not dereference*. `fList.Clear` in a destructor on an
object whose constructor died before `fList := ...` is an access violation.

---

## 2. Reference counting: interfaces

```delphi
type
  ILogger = interface
    ['{3A1F5E2C-9C7B-4A0E-B0E3-8E4A1D6C2B71}']   // Ctrl+Shift+G generates one
    procedure Log(const AMsg: string);
  end;

  TLogger = class(TInterfacedObject, ILogger)
    procedure Log(const AMsg: string);
  end;

procedure Use;
begin
  var lLog: ILogger := TLogger.Create;   // refcount 1
  lLog.Log('hi');
end;                                     // refcount 0 -> the object frees itself
```

- **Never `Free` an object you hold through an interface.** The refcount will drop to zero later and free
  it again.
- The GUID is not decoration. Without it, `Supports` and `as` on the interface do not compile.
- `TInterfacedObject` is the refcounting base. `TNoRefCountObject` (added in **11 Alexandria**;
  `System.pas`) is the base for singletons and stack-lifetime helpers that implement an interface but must
  not be destroyed by the count.

### The classic mixed object/interface bug

```delphi
// BROKEN
var lObj := TLogger.Create;      // an OBJECT reference — refcount is still 0
var lIntf: ILogger := lObj;      // refcount 1
lIntf := nil;                    // refcount 0 -> object destroyed
lObj.Log('boom');                // lObj now dangles
```

And the mirror image:

```delphi
// BROKEN — freed twice
var lObj := TLogger.Create;
try
  var lIntf: ILogger := lObj;    // 1
  DoWork(lIntf);
finally
  lObj.Free;                     // manual free... and lIntf's release already did it
end;
```

**Rule: pick one. If a class implements an interface, hold it only as an interface, from the moment it is
created.** If you genuinely need both — a component that is owned by a form *and* implements an interface —
derive from `TNoRefCountObject` (or `TComponent`, whose `_AddRef`/`_Release` do not count) so the interface
reference has no say in the lifetime.

`Supports` (`System.SysUtils`) is the safe cast; it does not raise:

```delphi
var lSaver: IStreamPersist;
if Supports(lObj, IStreamPersist, lSaver) then
  lSaver.SaveToStream(lStream);
```

---

## 3. Ownership in containers

### `TObjectList<T>` and friends — `System.Generics.Collections`

```delphi
constructor TObjectList<T>.Create; overload;                                    // OwnsObjects := True
constructor TObjectList<T>.Create(AOwnsObjects: Boolean); overload;
constructor TObjectList<T>.Create(const AComparer: IComparer<T>; AOwnsObjects: Boolean = True); overload;
constructor TObjectList<T>.Create(const Collection: TEnumerable<T>; AOwnsObjects: Boolean = True); overload;
property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
```

**The parameterless constructor owns.** That is the opposite default from `TStringList.Create`, which does
not own its `Objects[]` unless you use `TStringList.Create(OwnsObjects: Boolean)`.

What frees and what does not:

| Call | With `OwnsObjects = True` |
|------|---------------------------|
| `Remove`, `Delete`, `DeleteRange`, `Clear`, `SetCount` down, destructor | **frees** the item |
| `Extract`, `ExtractAt`, `ExtractItem` | removes it and **hands it to you unfreed** — you now own it |

(The implementation frees on the `cnRemoved` notification only; the extract path uses a different one.)

`TObjectDictionary<K,V>` takes a set instead of a flag:

```delphi
TDictionaryOwnership = (doOwnsKeys, doOwnsValues);
TDictionaryOwnerships = set of TDictionaryOwnership;

var lCache := TObjectDictionary<string, TBitmap>.Create([doOwnsValues]);
```

`TObjectQueue<T>`, `TObjectStack<T>`, `TObjectHashSet<T>` follow `TObjectList<T>`'s `AOwnsObjects` pattern.

### `TComponent` — `System.Classes`

```delphi
constructor Create(AOwner: TComponent); virtual;
property Owner: TComponent read FOwner;
property Components[Index: Integer]: TComponent read GetComponent;
procedure FreeNotification(AComponent: TComponent);
```

A component with a non-nil `AOwner` is destroyed by that owner. `TTimer.Create(Self)` inside a form is
freed with the form — freeing it yourself is a double free. `Create(nil)` means *you* own it, so
`try/finally` it like any object.

`FreeNotification` is how a component that merely *references* another (without owning it) learns that the
other has been destroyed, via `Notification(AComponent, opRemove)`. Use it instead of hoping a pointer
stays valid.

---

## 4. Custom managed records — deterministic cleanup, no `try/finally`

Available since **10.4 Sydney**, so safe on Delphi 11. A record can declare:

```delphi
type
  TMyRecord = record
    Value: Integer;
    class operator Initialize(out Dest: TMyRecord);
    class operator Finalize(var Dest: TMyRecord);
    class operator Assign(var Dest: TMyRecord; const [ref] Src: TMyRecord);
  end;
```

The exact parameter modifiers are enforced by the compiler (`E2617` if the first `Assign` parameter is not
`var`, `H2618` if the second is not `const [ref]` or `var`). Copy them as written.

`Initialize` runs at the declaration point, `Finalize` at end of scope — **including when an exception
unwinds the block**, which is precisely what makes it useful:

```delphi
procedure Work;
begin
  var lRec: TMyRecord;   // Initialize here
  raise Exception.Create('boom');
end;                     // Finalize still runs
```

On **Delphi 13 Florence only**, `Initialize`/`Finalize` may be declared without the parameter
(`class operator Initialize;`) with `Self` implicit. On 11 and 12 you must write the parameter.

Behaviour of managed records as parameters, from the docwiki:

| Passing style | Copy / `Assign` call |
|---------------|----------------------|
| `procedure P(ARec: TMyRecord)` (by value) | new record + `Assign`, temporary finalized on exit |
| `const ARec: TMyRecord` | none |
| `var ARec: TMyRecord` | none |
| `const [ref] ARec: TMyRecord` | none |
| `function F: TMyRecord` | `Initialize` for the result, then `Assign` at the call site |

A static array of managed records is initialized at its declaration; a dynamic one when `SetLength` grows it.

### Smart pointer

This is the idiomatic use, and it needs no `try/finally` at the call site:

```delphi
type
  TSmartPtr<T: class, constructor> = record
  private
    fValue: T;
  public
    class operator Initialize(out Dest: TSmartPtr<T>);
    class operator Finalize(var Dest: TSmartPtr<T>);
    property Value: T read fValue;
  end;

class operator TSmartPtr<T>.Initialize(out Dest: TSmartPtr<T>);
begin
  Dest.fValue := T.Create;
end;

class operator TSmartPtr<T>.Finalize(var Dest: TSmartPtr<T>);
begin
  Dest.fValue.Free;
end;
```

Do not add an `Assign` operator that shallow-copies the pointer, or two records will free the same object.
Either forbid copying (give `Assign` a body that raises or deep-copies) or accept that the record must not
be assigned.

---

## 5. Leak detection

For any console/service target, turn the leak report on in the `.dpr` **before** anything allocates:

```delphi
begin
  ReportMemoryLeaksOnShutdown := True;   // System unit, FastMM reports on exit
  ...
end.
```

At process exit it names the leaked classes — but **only for small blocks**. Medium and large blocks are
reported as sizes alone, with no class name (`getmem.inc`: *"The sizes of unexpected leaked medium and large
blocks are: "*). Leak a big buffer and the report looks unhelpfully vague; that is the tool, not you.

The RTL puts one caveat directly on the declaration: *"This setting has no effect if this module is sharing a
memory manager owned by another module."* In a DLL or a BPL that shares the host's manager, the report you
are waiting for will never appear — and silence is not a clean bill of health.

Use it in the debug configuration; it is the cheapest possible test that your `try/finally` blocks are right.
Reading the report, narrowing a leak without a stack trace, and failing a build on one:
`delphi-code-smells`, `reference/leaks.md`.

---

## Checklist

- [ ] Is every `Create` outside its `try`?
- [ ] Does any object reached through an interface also get `Free`d?
- [ ] Does any class hold both an object reference and an interface reference to the same instance?
- [ ] Did you `Free` something a `TObjectList<T>` / `TObjectDictionary` / owner component also frees?
- [ ] Did you `Extract` and then forget that you now own the item?
- [ ] Does the destructor survive a constructor that raised (no dereference of possibly-nil fields)?
- [ ] Does a `TComponent` created with an owner get freed manually as well?
