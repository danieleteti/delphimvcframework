# Delphi — generics, collections, RTTI and attributes

## `System.Generics.Collections` — what exists

| Type | Notes |
|------|-------|
| `TList<T>` | dynamic array with `Add`/`Insert`/`Delete`/`Sort`/`BinarySearch`/`Contains`/`IndexOf`/`Extract`/`ToArray` |
| `TObjectList<T: class>` | `TList<T>` + `OwnsObjects` |
| `TQueue<T>` / `TStack<T>` | `Enqueue`/`Dequeue`/`Peek`, `Push`/`Pop`/`Peek` |
| `TObjectQueue<T>` / `TObjectStack<T>` | + `AOwnsObjects` |
| `TDictionary<K,V>` | hash map |
| `TObjectDictionary<K,V>` | + `TDictionaryOwnerships` |
| `TPair<K,V>` | record with `Key` and `Value` |
| `TThreadList<T>` / `TThreadedQueue<T>` | locked wrappers |
| `TEnumerable<T>` / `TEnumerator<T>` | the abstract bases every container derives from; `ToArray` lives here |
| `TArray` (the class) | static helpers: `Sort<T>`, `BinarySearch<T>`, `IndexOf<T>`, `LastIndexOf<T>`, `Contains<T>`, `Copy<T>`, `Concat<T>`, `FreeValues<T>`, `ToString<T>` |
| `THashSet<T>`, `TOrderedDictionary<K,V>`, `TObjectHashSet<T>` | **not in Delphi 11** — verify before targeting 12 |

`TArray<T>` (with angle brackets) is the *dynamic array type* `array of T`, declared in `System`. `TArray`
(no brackets) is the helper *class* in `System.Generics.Collections`. Two different things, one letter
apart. A dynamic array has no methods — `SetLength`, `Length`, `High`, `Copy`, `Insert`, `Delete` are the
API.

### `TDictionary<K,V>` — the calls you actually need

```delphi
var lAges := TDictionary<string, Integer>.Create;
try
  lAges.Add('anna', 31);              // raises EListError if the key exists
  lAges.AddOrSetValue('anna', 32);    // overwrites
  if lAges.TryAdd('bob', 40) then ...;// False if it existed; no exception

  var lAge: Integer;
  if lAges.TryGetValue('anna', lAge) then ...;   // the correct read
  lAges['anna'];                                 // raises if absent

  lAges.ContainsKey('anna');
  lAges.Remove('anna');
  var lPair := lAges.ExtractPair('bob');         // removes and returns it

  for var lPair in lAges do
    Writeln(lPair.Key, '=', lPair.Value);
  for var lKey in lAges.Keys do ...;
finally
  lAges.Free;
end;
```

`TryGetValue`'s second parameter is `var`, not `out`, so the variable must exist — it does not need to be
initialised, but the compiler will not warn if you read it after `False`.

**Enumeration order is not insertion order and is not stable.** If you need order, keep a `TList<K>` beside
it, or use `TOrderedDictionary` on a version that has it.

### Comparers — `System.Generics.Defaults`

```delphi
IComparer<T> = interface
  function Compare(const Left, Right: T): Integer;
end;

IEqualityComparer<T> = interface
  function Equals(const Left, Right: T): Boolean;
  function GetHashCode(const Value: T): Integer;
end;
```

`TComparer<T>.Default` is what every container uses when you do not pass one. For a custom order, build one
inline instead of declaring a class:

```delphi
lPeople.Sort(TComparer<TPerson>.Construct(
  function(const L, R: TPerson): Integer
  begin
    Result := CompareText(L.LastName, R.LastName);
    if Result = 0 then
      Result := CompareText(L.FirstName, R.FirstName);
  end));
```

`TEqualityComparer<T>.Construct(EqualityComparison, Hasher)` is the equivalent for a dictionary key.
`TDelegatedComparer<T>` / `TDelegatedEqualityComparer<T>` are the class forms if you need to keep one
around. For case-insensitive string keys the RTL ships `TIStringComparer.Ordinal`.

A dictionary's comparer is passed at construction only:

```delphi
var lIndex := TDictionary<string, TThing>.Create(TIStringComparer.Ordinal);
```

**`Equals` and `GetHashCode` must agree.** Two values that compare equal and hash differently make a
dictionary lose entries silently.

### Iteration invalidation

```delphi
// BROKEN — mutating while enumerating
for var lItem in lList do
  if lItem.Expired then
    lList.Remove(lItem);          // the enumerator's index is now wrong; items get skipped

// CORRECT — backwards by index
for var i := lList.Count - 1 downto 0 do
  if lList[i].Expired then
    lList.Delete(i);

// CORRECT — collect, then delete
var lDoomed := TList<TThing>.Create;
try
  for var lItem in lList do
    if lItem.Expired then
      lDoomed.Add(lItem);
  for var lItem in lDoomed do
    lList.Remove(lItem);
finally
  lDoomed.Free;
end;
```

Delphi's enumerators do **not** raise a "collection modified" error the way .NET does. They just give you
the wrong answer. The same applies to `TDictionary` — an `Add` during enumeration can rehash the whole
table.

Note also that `for var lItem in lList` copies the element for a value type: modifying `lItem` inside the
loop changes the copy, not the list. For records, index instead.

---

## Generics — declaring them

```delphi
type
  TRepository<T: class, constructor> = class
  private
    fItems: TObjectList<T>;
  public
    function CreateNew: T;
  end;

function TRepository<T>.CreateNew: T;
begin
  Result := T.Create;         // legal only because of the "constructor" constraint
end;
```

Constraints, and what each buys:

| Constraint | Meaning | Since |
|-----------|---------|-------|
| `T: class` | any class; allows `nil`, `Free`, `is`/`as` | always |
| `T: record` | a value type (no `nil`) | always |
| `T: constructor` | has a parameterless public constructor; allows `T.Create` | always |
| `T: TBaseClass` | that class or a descendant; you may call its members | always |
| `T: IMyInterface` | implements that interface | always |
| `T: class, constructor` | combine with commas | always |
| `T: interface` | any interface type | **13 Florence** |
| `T: unmanaged` | a type with no compiler-managed fields | **13 Florence** |

Without a constraint, `T` is opaque: you cannot call anything on it, compare it with `=`, or create it.
That is why `TComparer<T>.Default` exists — it reaches the comparison through RTTI instead.

Naming: the docwiki style guide says use `T` for a single placeholder and **descriptive** names when there
are several (`TKey`, `TValue`), not `T, U, V`.

Two traps:

- **A `class var` in a generic class is per-instantiation.** `TCache<string>` and `TCache<Integer>` have
  separate copies. Sometimes what you want, usually a surprise.
- **Generic methods are expanded per type argument.** A large generic method used with ten types is ten
  copies in the binary.

---

## Anonymous methods and capture

```delphi
TProc = reference to procedure;                        // System.SysUtils
TProc<T> = reference to procedure(Arg1: T);
TFunc<TResult> = reference to function: TResult;
TFunc<T, TResult> = reference to function(Arg1: T): TResult;
TPredicate<T> = reference to function(Arg1: T): Boolean;
```

`reference to` is a **managed, reference-counted** type: assigning one keeps the closure and everything it
captured alive. That is the whole point, and it is also the whole problem.

**Capture is by reference, of the variable, not of its value.** The classic bite:

```delphi
// BROKEN — every closure sees the final value of i
var lActions := TList<TProc>.Create;
for var i := 1 to 3 do
  lActions.Add(procedure begin Writeln(i); end);   // prints 3, 3, 3? — depends on the loop variable's
                                                   // lifetime; do not rely on it either way

// CORRECT — capture a fresh variable per iteration
for var i := 1 to 3 do
begin
  var lCaptured := i;                              // a new variable each pass
  lActions.Add(procedure begin Writeln(lCaptured); end);
end;
```

More consequences worth knowing:

- A closure that captures `Self` keeps the **whole object** alive. A form that stores a closure capturing
  itself will not be destroyed. Break the cycle by capturing a local copy of just what you need.
- Capturing a **local object** and then freeing it at the end of the method leaves the closure holding a
  dangling reference. Either transfer ownership into the closure or capture an interface.
- The captured variable is shared: two closures created in the same scope see each other's writes.
- You cannot capture a `var`/`out` parameter, nor the loop variable of a classic `for` in a way you can
  rely on. Copy into a local first.

---

## RTTI — `System.Rtti`

```delphi
TRttiContext = record
  class function Create: TRttiContext; static;
  procedure Free;
  class procedure KeepContext; static;
  class procedure DropContext; static;
  function GetType(ATypeInfo: Pointer): TRttiType; overload;
  function GetType(AClass: TClass): TRttiType; overload;
  function GetTypes: TArray<TRttiType>;
  function FindType(const AQualifiedName: string): TRttiType;
  function GetPackages: TArray<TRttiPackage>;
end;
```

`TRttiContext` is a **record**, not a class: declare it, use it, and let it go out of scope. `Free` exists
but is a cache release, not a destructor — you do not need `try/finally` around it. The `TRttiType`,
`TRttiProperty`, `TRttiMethod` objects it hands out are owned by the context's pool; **never free them**.

```delphi
var lCtx := TRttiContext.Create;
var lType := lCtx.GetType(AObject.ClassType);
for var lProp in lType.GetProperties do
  if lProp.IsReadable then
    Writeln(lProp.Name, ' = ', lProp.GetValue(AObject).ToString);
```

Creating a context repeatedly in a hot loop rebuilds the type pool. `KeepContext` / `DropContext` pin it
across many short-lived contexts.

`TValue` is the boxed any-type:

```delphi
var lValue := TValue.From<Integer>(42);
lValue.IsType<Integer>;      lValue.AsType<Integer>;
lValue.AsInteger;            lValue.AsString;      lValue.AsObject;
lValue.ToString;             lValue.IsEmpty;
```

Other pieces: `TRttiMethod.Invoke(Instance, [Args])`, `TRttiField.GetValue/SetValue`,
`TVirtualMethodInterceptor` for intercepting virtual calls (`TInterceptBeforeNotify` /
`TInterceptAfterNotify` / `TInterceptExceptionNotify`).

`System.TypInfo` is the lighter, older API — `GetEnumName`, `GetEnumValue`, `GetPropValue`, `TTypeKind`.
Reach for it when all you need is an enum name; it links far less code than `System.Rtti`.

### Visibility

Extended RTTI is emitted per the `{$RTTI}` directive in force where the type is declared. **Public and
published members are the safe assumption; private and protected members may not be visible** unless the
declaring unit raised the visibility:

```delphi
{$RTTI EXPLICIT METHODS([vcPublic, vcPublished])
       PROPERTIES([vcPublic, vcPublished])
       FIELDS([vcPrivate, vcProtected, vcPublic, vcPublished])}
```

If a framework must map private fields (an ORM, a serializer), it has to say so with a directive like the
above in the unit that declares the entity — or map public properties instead. Do not assume a private
field is reachable.

---

## Attributes

An attribute is a class descending from `TCustomAttribute` (`System`). Naming, per the docwiki style guide:
**no `T` prefix, and the name ends in `Attribute`** — the suffix may be dropped at the use site.

```delphi
type
  TableAttribute = class(TCustomAttribute)
  private
    fName: string;
  public
    constructor Create(const AName: string);
    property Name: string read fName;
  end;

  [Table('customers')]                 // on a type: its own line, before the declaration
  TCustomer = class
  private
    [Column('id')] fID: Integer;       // on a field or parameter: inline, one space
  end;
```

Reading them back:

```delphi
var lCtx := TRttiContext.Create;
for var lAttr in lCtx.GetType(TCustomer).GetAttributes do
  if lAttr is TableAttribute then
    lTableName := TableAttribute(lAttr).Name;
```

`GetAttributes: TArray<TCustomAttribute>` exists on `TRttiObject` and therefore on types, fields,
properties, methods and parameters alike.

Three things that bite:

1. **Attribute arguments must be compile-time constants** — ordinals, strings, sets, `class of`. No object
   construction, no expressions calling functions.
2. **The attribute instances are created lazily by the RTTI machinery and owned by it.** Do not free them.
3. **The linker can remove a class that is only ever referenced through RTTI.** Force a reference (mention
   the class in a registration call in the `initialization` section) or the type will not be found at run
   time. `{$WEAKLINKRTTI ON}` makes this worse deliberately, to shrink binaries — know which state your
   project is in before debugging a "type not found".
