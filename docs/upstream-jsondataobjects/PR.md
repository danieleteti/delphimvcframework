# PR: add a nesting-depth limit to the parser

Repository: https://github.com/ahausladen/JsonDataObjects
Base: `master` @ `4fdd3057` ("feat: Added FailOnPrimitive...", 2026-06-03)
Patch: `jsondataobjects-max-nesting-depth.patch` (160 lines, **additions only**, applies clean)
Repro: `repro_Depth.dpr`

**Title:** Limit the parser's nesting depth (a 34 KB document can kill the process)

---

## The problem

`TJsonReader` is recursive-descent: `ParseObjectBody` and `ParseArrayBody` call each
other through `ParseObjectProperty`/`ParseArrayPropertyValue`, one stack frame per `{`
or `[`. Nothing bounds the recursion, so the nesting depth of the *input* decides how
much stack the parser uses.

Past a certain depth the stack guard page is passed over instead of being touched, and
the process is gone: no exception, nothing to catch, exit code 139.

Measured with `repro_Depth.dpr` (Delphi 13, default 1 MB stack, Win 11):

| Platform | depth | payload | result |
|---|---:|---:|---|
| Win64 | 1 000 | 4 KB | parses |
| Win64 | 8 000 | 32 KB | `EStackOverflow`, still catchable |
| Win64 | **8 500** | **34 KB** | **process dead, no exception, exit 139** |
| Win32 | 20 000 | 80 KB | parses |
| Win32 | 30 000 | 120 KB | `EStackOverflow`, still catchable |
| Win32 | **50 000** | **200 KB** | **process dead, exit 139** |

Two details that make this worse than a plain stack overflow:

- **`IsValidJSON` does not protect you.** It is the natural gate for untrusted input,
  and its `except` clause returns `False` — but an except clause cannot catch what
  never raises. At depth 8 500 on Win64 it dies inside the validation call itself.
- **Freeing is recursive too.** At depth 8 000 the document parses, and then
  `TJsonBaseObject.Free` raises `EStackOverflow` while unwinding the tree.

The payload is a few tens of KB of *perfectly valid* JSON — well below any request-size
limit a server would set. Found while hardening DelphiMVCFramework, where the parser
sits behind an unauthenticated HTTP endpoint.

## The fix

A depth counter in `TJsonReader`, checked at the two points that are entered exactly
once per nesting level, raising the library's own `EJsonParserException` with the usual
line/column/position:

```pascal
procedure TJsonReader.EnterNestingLevel;
var
  LMaxDepth: Integer;
begin
  Inc(FDepth);
  LMaxDepth := JsonMaxNestingDepth;
  if LMaxDepth <= 0 then // a cleared limit is the default one, never "no limit"
    LMaxDepth := DefaultJsonMaxNestingDepth;
  if FDepth > LMaxDepth then
    NestingTooDeepError(LMaxDepth);
end;
```

Notes on the shape of the patch:

- **One choke point covers everything.** Since the `TJsonAbstractParser` refactor both
  `TJsonObjectParser` and `TJsonValidateParser` are driven by the same `TJsonReader`, so
  `Parse`, `ParseUtf8`, `ParseFromStream`, `IsValidJSON`, `IsValidJSONObject` and
  `IsValidJSONArray` are all covered by this single change.
- **`FDepth` is reset at the top of `Parse()`** rather than unwound in a `finally`, so an
  aborted parse cannot leave the counter dirty and the recursion stays free of
  exception-handling frames.
- **`JsonMaxNestingDepth`** is a global, mirroring the existing `JsonSerializationConfig`
  and, like it, not thread-safe. `DefaultJsonMaxNestingDepth = 512` is far deeper than
  any real document and an order of magnitude below the measured limit on both platforms.
- **The limit cannot be switched off.** Zero and negative values mean
  `DefaultJsonMaxNestingDepth`, not "unlimited": a limit that a single line of
  configuration can clear is a limit an attacker eventually gets. An application that
  genuinely needs deeper documents raises the value — `MaxInt` restores the current
  behaviour for anyone who wants it, but it has to be spelled out.

## Cost

One `Inc`, one integer compare and one `Dec` **per nesting level**. Nothing per token,
nothing per character, no `try..finally` in the recursion path.

Parsing a 40 000-record array (7.6 MB, the shape a REST API actually sees), Win64,
best of 7, three separate runs:

```
pristine: 34 ms / 33 ms / 33 ms
patched : 34 ms / 33 ms / 33 ms
```

## Tests

`UnitTest/TestJsonDataObjects.pas` gains `TestMaxNestingDepth` in the existing
`CheckException` idiom: a document exactly at the limit parses; one level deeper raises
on all four entry points (`Parse`/`ParseUtf8` × objects/arrays); `IsValidJSON` answers
`False` instead of dying; the counter does not leak from an aborted parse into the next
one; a lowered limit takes effect immediately; a cleared limit falls back to the default
instead of disabling the check.

```
DUnit / Testing
............................................................
OK: 60 tests        (Win32 and Win64)
```

## Compatibility

The patch is additions only — no existing line is modified or removed. The one
behavioural change is intended: a document nested deeper than 512 levels now raises
`EJsonParserException` instead of parsing (or killing the process). Anything that was
parsing successfully before is a document 512 levels deep or less, and is unaffected.

## How to reproduce

```
dcc32 -B -U<path-to>\JsonDataObjects\Source repro_Depth.dpr
dcc64 -B -U<path-to>\JsonDataObjects\Source repro_Depth.dpr

repro_Depth 1000     # parses
repro_Depth 8000     # EStackOverflow (Win64)
repro_Depth 8500     # no output after "IsValidJSON...", exit code 139
```
