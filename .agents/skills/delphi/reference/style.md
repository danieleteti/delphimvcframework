# Delphi — naming and formatting

Three sources are in play, and they do not fully agree. In order of authority:

1. **Embarcadero's own style guide on the docwiki** — *Delphi's Object Pascal Style Guide*, curated by
   Marco Cantù from Charlie Calvert's original. Pages: *General Rules*, *Source Code Files Units and Their
   Structure*, *White Space Usage*, *Comments*, *Statements*, *Type Declarations*.
2. **The RTL/VCL source itself**, which is what the guide describes (imperfectly — the guide admits several
   of its own rules "are not applied consistently in the core Delphi libraries").
3. **Community guides**, of which one is quoted below. Where a community guide contradicts 1 or 2, **1 and
   2 win**. Where it is one convention among several, it is presented as such.

**And above all three: the file you are editing.** Match its existing conventions. Consistency inside a
unit beats any external guide.

---

## What the docwiki mandates

| Rule | Detail |
|------|--------|
| Casing | **Pascal casing** for every identifier (`MyName`). No underscores — except in header translations, which keep the original convention (`WM_LBUTTONDOWN`) |
| Keywords | **lowercase**: `begin`, `uses`, `string`, `nil`. `Self` is an identifier, not a keyword, so it is capitalised |
| Indentation | **two spaces** per level. **Never tabs** |
| Margin | `unit`, `uses`, `type`, `interface`, `implementation`, `initialization`, `finalization`, the final `end`, and **compiler directives** all sit flush left |
| Line length | 80 was the tradition; longer is acceptable "as long as that code will fit in the editor on most screens without scrolling horizontally". Continuation lines indent **two spaces**, and never start with a binary operator |
| Types | `T` prefix (`TCustomer`) — except `E` for `Exception` descendants, `I` for interfaces, `P` for pointers, and custom attributes, which take **no prefix and end in `Attribute`** |
| Fields | `F` + Pascal casing (`FConnectionString`). Public record fields drop the `F` |
| Properties | no prefix. Accessors are `GetX` / `SetX`; do not write a trivial getter that only reads the field |
| Events | property `OnFoo`, field `FOnFoo`, trigger method `DoFoo` |
| Methods | imperative verb phrases: `ShowStatus`, `DrawCircle`. Boolean queries `IsVisible` / `HasParent` / `CanPaint` |
| Enum values (unscoped) | short type-derived prefix — `fsBold`, `bkCancel` — because the values are global |
| Enum values (scoped) | plain Pascal casing, no prefix. **Scoped enumerations are recommended over the traditional form** for new code |
| Generic placeholders | `T` alone; descriptive names (`TKey`, `TValue`) when there is more than one — not `T, U, V` |
| `uses` | full dotted names (`System.SysUtils`, never `SysUtils`), on the line after `uses`, indented 2. One unit per line is explicitly accepted, for diffs |
| Blocks | `begin` and `end` each on their own line. One statement per line. `if A < B then` never on one line with its statement |
| `if`/`else` | `begin` goes on the line **after** `then`, not on the same line. `end else begin` is called out as incorrect |
| Comments | `//` for single lines (one space after the slashes), `{ }` for blocks. **The Delphi team requires curly braces, not `(* *)`**. `///` for XMLDoc |
| Parameters | `const` on managed types (string, interface, dynamic array, record) that the method does not modify. Recommended, not required, for unmanaged types |
| Assertions | for invariants and preconditions that must hold regardless of input — not for user input, files, or OS state |

Two places the docwiki deliberately declines to rule:

> "Regarding naming local variables, some developers define their own rules like an initial `L`, which makes
> sense, but we don't have a strict recommendation for a local variables prefix."

> "Regarding the naming of parameters, some developers define their own rules like an initial `A`, which
> stands for 'Argument' and is often used in the Delphi libraries source code — but we don't have a strict
> recommendation for a parameter name prefix."

So `L`/`A` prefixes are **conventions, widely followed, not law**. Both are used throughout the RTL.

### Inline variables, per the docwiki

The style guide is unusually direct here — it calls the old form *incorrect*:

```delphi
// classic, but now considered incorrect
var
  I: Integer;
begin
  for I := 1 to 10 do

// correct
begin
  for var I: Integer := 1 to 10 do
```

and recommends inline variables generally for block-local scope: "do not declare in the method `var` block
variables that are used only in one specific block".

### Anonymous methods, per the docwiki

```delphi
// correct
ForEachOutputParameter(ProxyMethod,
  procedure(Param: TDSProxyParameter)
  begin
    ...
  end);

// incorrect — begin on the same line as procedure
TThread.Queue(nil, procedure begin
  ...
end);

// incorrect — fully inlined
TThread.Synchronize(nil, procedure begin FCallback(X); end);
```

---

## A community guide, and where it differs

Olaf Monien's *Delphi Style Guide* (v2.1, 2025-10-08), shipped with the `DX.Logger` project, is a
widely-circulated short guide. Its naming table:

| Element | Convention | Example |
|---------|-----------|---------|
| Local variables | `L` + PascalCase | `LCustomerName` |
| Fields | `F` + PascalCase | `FConnectionString` |
| Parameters | `A` + PascalCase | `AValue` |
| Loop counters | lowercase, no prefix | `i`, `j` |
| Constants | `c` prefix; `sc` for strings | `cMaxRetries`, `scErrorMsg` |
| Types/Classes | `T` + PascalCase | `TCustomer` |
| Interfaces | `I` + PascalCase | `ILogger` |
| Exceptions | `E` + PascalCase | `EFileNotFound` |
| Methods | verb + PascalCase | `SaveDocument` |

Plus: 2-space indent, no tabs, **120 characters** maximum line length, `//` `{}` `///` as the docwiki has
them, and dotted unit names mapping to class names (`Main.Form.pas` → `TFormMain`).

Where it agrees with the docwiki (`T`/`I`/`E` prefixes, `F` fields, 2 spaces, no tabs, verb method names),
treat it as confirmation. Where it goes further:

| Community guide says | Status |
|---------------------|--------|
| `L` prefix on locals, `A` on parameters | **A convention, not a rule.** The docwiki explicitly declines to mandate either. Widely used, including in the RTL. Adopt it — but do not "correct" code that does not use it |
| `c` / `sc` prefix on constants | **Contradicts the docwiki**, which says constants follow the general identifier rules and reserves special casing for header translations. Minority convention |
| `(* *)` for temporarily disabled code | **Contradicts the docwiki**, which states the Delphi team *requires* curly-brace block comments rather than `(* *)`. Use `{ }`, or better, delete the code — version control remembers it |
| Max 120 characters | A specific number where the docwiki gives a principle. Harmless; adopt whatever the project already does |
| "Always use `FreeAndNil()` instead of `.Free`" | **One position among several.** The RTL uses plain `Free` throughout. See `memory.md`: `FreeAndNil` earns its keep on a field that is tested for `nil` later, and is noise on a local. Note also that the guide's own example puts the `Create` **inside** the `try` — that is the classic bug, not a pattern to copy |
| `Main.Form.pas` → `TFormMain` | A project-level naming scheme. The docwiki only requires dotted unit names and Pascal casing. Fine if the project already does it; do not impose it |

---

## This repository's house style

The DMVCFramework skills in this repo — and DMVCFramework's own source — use **lowercase** prefixes:

```delphi
type
  TPerson = class(TMVCActiveRecord)
  private
    fName: NullableString;          // lowercase f, not F
  end;

function TPeopleController.Update(const id: Integer;
  const [MVCFromBody] Resource: TPerson): IMVCResponse;
var
  lExisting: TPerson;               // lowercase l
begin
  ...
end;

procedure TMyHandler.OnRequest(const AContext: TWebContext;   // uppercase A on parameters
  var AAuthenticationRequired: Boolean);
```

So: **`f` fields, `l` locals, `A` parameters**, `T`/`I`/`E` types. The `f`/`l` lowercase form contradicts
the docwiki's capital `F` and the community guide's capital `L`; it is DMVCFramework's own convention and
it is what the surrounding code looks like. When you write code for a DMVCFramework project, match it.
When you write code for a project that uses `F`/`L`, match that instead.

---

## Reference skeleton

```delphi
unit Acme.Billing.Invoices;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections;

type
  EInvoiceError = class(Exception);

  TInvoiceState = (Draft, Issued, Paid, Void);      // scoped-style values, no prefix

  IInvoiceStore = interface
    ['{7C6C0C1A-2B0D-4E37-8D2F-9E5A1B3C4D50}']
    function Load(const AID: Integer): TObject;
  end;

  TInvoice = class(TObject)
  private
    fID: Integer;
    fState: TInvoiceState;
    fOnChanged: TNotifyEvent;
    procedure SetState(const AValue: TInvoiceState);
  protected
    procedure DoChanged; virtual;
  public
    constructor Create(const AID: Integer);
    function IsPayable: Boolean;
    property ID: Integer read fID;
    property State: TInvoiceState read fState write SetState;
    property OnChanged: TNotifyEvent read fOnChanged write fOnChanged;
  end;

implementation

constructor TInvoice.Create(const AID: Integer);
begin
  inherited Create;
  fID := AID;
  fState := TInvoiceState.Draft;
end;

function TInvoice.IsPayable: Boolean;
begin
  Result := fState = TInvoiceState.Issued;
end;

procedure TInvoice.SetState(const AValue: TInvoiceState);
begin
  if fState = AValue then
    Exit;
  fState := AValue;
  DoChanged;
end;

procedure TInvoice.DoChanged;
begin
  if Assigned(fOnChanged) then
    fOnChanged(Self);
end;

end.
```

Note `{$SCOPEDENUMS ON}` is what makes `TInvoiceState.Draft` *required* rather than optional; without it,
`Draft` is also a global identifier and the docwiki's prefix rule (`isDraft`, `isIssued`, …) applies
instead. Turn scoped enums on for new code and use the qualified form either way — it reads better and it
survives a later switch.
