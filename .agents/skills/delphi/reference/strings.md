# Delphi — strings, encodings and text

## The types

| Type | What it is | Where |
|------|-----------|-------|
| `string` = `UnicodeString` | **UTF-16**, reference counted, copy-on-write, 1-based | `System` |
| `AnsiString` | 8-bit + a code page, reference counted | `System` |
| `UTF8String` | `type AnsiString(65001)` — 8-bit bytes tagged UTF-8 | `System` |
| `RawByteString` | `type AnsiString($ffff)` — bytes with **no** code page; assigning it never converts | `System` |
| `TBytes` = `TArray<Byte>` | plain bytes, no encoding attached | `System` |
| `Char` | `WideChar`, 2 bytes | `System` |

`string` is UTF-16, so `Length(s)` counts **code units, not characters**. An emoji or any character above
U+FFFF is a surrogate pair and costs 2. Do not use `Length` to count "letters".

Assigning between `string` and `AnsiString`/`UTF8String` performs a **conversion**, silently and at every
assignment. `RawByteString` is the escape hatch: it carries bytes through untouched, which is why it is the
right parameter type for a function that must not transcode. Use `TBytes` when the data is not text.

```delphi
var lUtf8: UTF8String := 'ciao';        // converted UTF-16 -> UTF-8 here
var lBack: string := lUtf8;             // and back again here
```

---

## 1-based, except when it is 0-based

This is the single most common LLM error in Delphi string code.

```delphi
var s := 'Delphi';

s[1]                  // 'D'   — strings are 1-based on desktop
Copy(s, 1, 3)         // 'Del' — 1-based
Pos('lp', s)          // 3     — 1-based, 0 when not found
Length(s)             // 6

s.Chars[0]            // 'D'   — TStringHelper is 0-BASED
s.IndexOf('lp')       // 2     — 0-based, -1 when not found
s.Substring(0, 3)     // 'Del' — 0-based
s.Length              // 6
```

Both live in `System.SysUtils`. The reason is in the source: the `TStringHelper` implementation is wrapped
in `{$ZEROBASEDSTRINGS ON}` while the rest of the RTL, and your code, compile with it `OFF` (the file
explicitly ends the region with `{$ZEROBASEDSTRINGS OFF} // Desktop platforms use One-based string`).

**Pick one style per routine and do not mix.** Never enable `{$ZEROBASEDSTRINGS ON}` in your own unit —
it changes `s[i]` under every line of existing code.

---

## `TStringHelper` — the useful half

All of these are on any `string` value, including a literal. 0-based indices throughout.

```delphi
s.IsEmpty                        s.Trim / s.TrimLeft / s.TrimRight
string.IsNullOrEmpty(s)          s.ToLower / s.ToUpper / s.ToLowerInvariant / s.ToUpperInvariant
string.IsNullOrWhiteSpace(s)     s.PadLeft(10) / s.PadRight(10, '.')
s.StartsWith(v) / .EndsWith(v)   s.Replace(old, new, [rfReplaceAll, rfIgnoreCase])
s.Contains(v)                    s.Split([',', ';'], TStringSplitOptions.ExcludeEmpty)  -> TArray<string>
s.IndexOf(v) / s.LastIndexOf(v)  string.Join(', ', ['a', 'b'])
s.Substring(start, len)          s.CountChar('a')
s.ToInteger / s.ToDouble         string.Create('-', 20)      // 20 dashes
s.QuotedString / s.DeQuotedString
```

`TStringSplitOptions = (None, ExcludeEmpty, ExcludeLastEmpty)`.
`TReplaceFlag = (rfReplaceAll, rfIgnoreCase)` — **`Replace` without `rfReplaceAll` replaces only the first
occurrence.** That default surprises everyone once.

Version note: the case-insensitive `s.Contains(Value, IgnoreCase)` overload was added in **13 Florence**.
On 11/12 use `ContainsText` from `System.StrUtils`.

---

## Building strings

```delphi
// Concatenation in a loop: O(n^2) reallocations. Do not.
for var i := 0 to 10000 do
  lResult := lResult + IntToStr(i) + ',';

// TStringBuilder (System.SysUtils) — Append returns Self, so it chains
var lSb := TStringBuilder.Create;
try
  for var i := 0 to 10000 do
    lSb.Append(i).Append(',');
  lResult := lSb.ToString;
finally
  lSb.Free;
end;
```

`TStringBuilder` also has `AppendLine`, `AppendFormat`, `Insert`, `Remove`, `Replace`, `Clear`, and
`Capacity`. It is worth it above a few hundred appends; below that, `Format` or plain `+` is clearer.

For a handful of pieces, `Format` beats concatenation for readability and for translation:

```delphi
Format('%s owes %.2f as of %s', [lName, lAmount, DateToStr(lDue)])
```

| Spec | Argument |
|------|----------|
| `%s` | string, `Char`, `PChar` |
| `%d` / `%u` | signed / unsigned integer |
| `%f` / `%n` / `%m` | float / float with thousands separators / currency |
| `%e` / `%g` | scientific / shortest of `%f`/`%e` |
| `%x` | hexadecimal integer |
| `%p` | pointer |
| `%%` | a literal `%` |
| `%2:s` | index specifier — the 3rd argument (0-based) |
| `%-10s` / `%10s` | left / right justified, width 10 |
| `%.2f` | precision |

The argument list is an `array of const`, so it is **not type checked**: `Format('%d', ['x'])` compiles and
raises `EConvertError` at runtime. Match the specifiers to the arguments by eye, every time.

---

## Comparison — the trap that ships bugs

| Function | Case | Locale |
|----------|------|--------|
| `S1 = S2` | sensitive | ordinal (binary) |
| `CompareStr` / `SameStr` | sensitive | **ordinal** by default |
| `CompareText` / `SameText` | **insensitive** | ordinal, and it only folds **ASCII A–Z** |
| `AnsiCompareStr` / `AnsiSameStr` | sensitive | user locale |
| `AnsiCompareText` / `AnsiSameText` | insensitive | user locale |

All in `System.SysUtils`. Each of the four non-`Ansi` ones has an overload taking
`TLocaleOptions = (loInvariantLocale, loUserLocale)`.

Rules that keep this out of trouble:

- Comparing **identifiers, keys, protocol tokens, file extensions**: `SameText`. Ordinal and fast, and you
  do not want Turkish `I` deciding whether your header matched.
- Comparing or sorting **text a human will read**: `AnsiCompareText` / a locale-aware `IComparer<string>`.
- Never lowercase-then-compare (`LowerCase(a) = LowerCase(b)`) — two allocations and worse Unicode
  behaviour than `SameText`.
- Never compare a **password or token** with `=`. It short-circuits and leaks the length and prefix through
  timing; compare in constant time.

`System.StrUtils` adds the ones you reach for constantly:
`ContainsText`, `StartsText`, `EndsText`, `MatchText(s, ['a','b'])`, `IndexText`, their case-sensitive
`…Str` twins, `SplitString(S, Delimiters)` and `IfThen`.

---

## Encoding: `TEncoding`

`System.SysUtils`. Class properties, all singletons you must **not** free:
`TEncoding.UTF8`, `.Unicode` (UTF-16LE), `.BigEndianUnicode`, `.ASCII`, `.ANSI`, `.Default`, `.UTF7`.

```delphi
var lBytes := TEncoding.UTF8.GetBytes(lText);      // string -> TBytes
var lText  := TEncoding.UTF8.GetString(lBytes);    // TBytes -> string
var lBom   := TEncoding.UTF8.GetPreamble;          // the BOM, if any
```

`TEncoding.Default` is the **system** encoding — it differs between machines. Never use it for a file
format, a wire protocol or anything that leaves the process. Pick `TEncoding.UTF8` explicitly.

Streams and files take an encoding; if you omit it you inherit a default you did not choose:

```delphi
lStrings.LoadFromFile(lPath, TEncoding.UTF8);
lStrings.SaveToFile(lPath, TEncoding.UTF8);
TFile.WriteAllText(lPath, lText, TEncoding.UTF8);     // System.IOUtils
var lReader := TStreamReader.Create(lStream, TEncoding.UTF8);
```

`TEncoding.GetBufferEncoding(Buffer, AEncoding)` detects a BOM and returns its length. From
**12 Athens** there is also `TEncoding.IsBufferValid` and a `UseBOM` property, for BOM-less UTF-8 detection.

---

## `TFormatSettings` and thread safety

The global `FormatSettings` variable carries the decimal separator, date order and so on. The RTL source
says it plainly: *"Using the global FormatSettings formatting variables is not thread-safe."*

Every conversion routine has an overload taking a `TFormatSettings` by value. Use it:

```delphi
var lFmt := TFormatSettings.Invariant;             // '.' decimal, English month names
lFmt.DecimalSeparator := '.';
var lValue := StrToFloat(lText, lFmt);
var lText2 := FormatFloat('0.00', lValue, lFmt);
```

`TFormatSettings.Create` (no args) snapshots the OS defaults; `Create(const LocaleName: string)` takes
`'en-US'`-style names; `Invariant` is the culture-neutral one you want for machine-readable output.

For dates on the wire, do not format by hand — `System.DateUtils` has `DateToISO8601` and `ISO8601ToDate`.

---

## Parsing — never use the raising variant on untrusted input

```delphi
StrToInt(s)                    // raises EConvertError
StrToIntDef(s, 0)              // returns the default
if TryStrToInt(s, lValue) then // tells you which happened  <- prefer this
```

The same triple exists for `Int64`, `Float`, `Currency`, `Date`, `Time`, `DateTime`, `Bool`.

---

## `TStrings` / `TStringList` — `System.Classes`

```delphi
var lLines := TStringList.Create;                 // does NOT own Objects[]
var lOwning := TStringList.Create(True);          // frees Objects[] — OwnsObjects overload
var lSorted := TStringList.Create(dupIgnore, {Sorted}True, {CaseSensitive}False);
```

Properties that change behaviour and are routinely forgotten:

| Property | Effect |
|----------|--------|
| `Sorted` | keeps the list sorted; enables the binary `Find`. Setting it re-sorts |
| `Duplicates` | `dupIgnore` / `dupAccept` / `dupError` — **only honoured when `Sorted` is True** |
| `CaseSensitive` | affects `IndexOf`, `Find` and sorting |
| `Delimiter`, `QuoteChar`, `DelimitedText` | CSV-ish round-tripping |
| `StrictDelimiter` | when False, `DelimitedText` also breaks on **spaces** and honours quotes. Set it True for real delimited data |
| `NameValueSeparator`, `Names[]`, `Values[]` | `key=value` access; the separator defaults to `=` |
| `Objects[]` | a `TObject` per line, freed only if `OwnsObjects` |

`TStringList` is not a `TList<string>`. For a plain collection of strings with no `Objects[]`, no sorting
flags and no file I/O, `TList<string>` or `TArray<string>` is smaller and faster.
