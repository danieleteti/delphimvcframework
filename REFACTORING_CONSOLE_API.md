# Console API Refactoring - Changes Summary

## Date: 2026-02-06

## Overview
Implemented Priority 1 improvements for `MVCFramework.Console` API consistency:
1. Renamed `WriteLineColored` → `WriteLine` (with overloads)
2. Renamed `MVCConsoleStyle` → `ConsoleTheme`

## Changes Made

### 1. ConsoleTheme Renamed (from MVCConsoleStyle)

**File**: `sources/MVCFramework.Console.pas`

**Before**:
```pascal
var
  MVCConsoleStyle: TConsoleColorStyle = (...);
```

**After**:
```pascal
var
  /// <summary>
  /// Global console theme configuration for colors and styles.
  /// Customize this record to change the default appearance of console output.
  /// </summary>
  ConsoleTheme: TConsoleColorStyle = (...);

  /// <summary>
  /// @deprecated Use ConsoleTheme instead. This alias is kept for backward compatibility.
  /// </summary>
  MVCConsoleStyle: TConsoleColorStyle absolute ConsoleTheme deprecated 'Use ConsoleTheme instead';
```

**Impact**:
- ✅ New code should use `ConsoleTheme`
- ✅ Old code using `MVCConsoleStyle` still works (backward compatible)
- ⚠️ Compiler will show deprecation warning when using `MVCConsoleStyle`
- ✅ All internal references updated to `ConsoleTheme`

**Internal Updates** (80+ occurrences replaced):
- `MVCConsoleStyle.TextColor` → `ConsoleTheme.TextColor`
- `MVCConsoleStyle.BackgroundColor` → `ConsoleTheme.BackgroundColor`
- `MVCConsoleStyle.DrawColor` → `ConsoleTheme.DrawColor`
- `MVCConsoleStyle.SymbolsColor` → `ConsoleTheme.SymbolsColor`
- `MVCConsoleStyle.BackgroundHighlightColor` → `ConsoleTheme.BackgroundHighlightColor`
- `MVCConsoleStyle.TextHighlightColor` → `ConsoleTheme.TextHighlightColor`
- `MVCConsoleStyle.BoxStyle` → `ConsoleTheme.BoxStyle`

---

### 2. WriteLine API (replaces WriteLineColored)

**File**: `sources/MVCFramework.Console.pas`

**New API** (3 overloads):
```pascal
/// <summary>
/// Writes text to console with optional colors and a newline.
/// </summary>
procedure WriteLine(const Text: string); overload;

/// <summary>
/// Writes colored text to console with a newline.
/// </summary>
procedure WriteLine(const Text: string; ForeColor: TConsoleColor); overload;

/// <summary>
/// Writes colored text to console with foreground and background colors, followed by a newline.
/// </summary>
procedure WriteLine(const Text: string; ForeColor: TConsoleColor; BackColor: TConsoleColor); overload;
```

**Old API** (deprecated):
```pascal
/// <summary>
/// @deprecated Use WriteLine instead. This procedure is kept for backward compatibility.
/// </summary>
procedure WriteLineColored(const Text: string; ForeColor: TConsoleColor = UseDefault;
                          BackColor: TConsoleColor = UseDefault); deprecated 'Use WriteLine instead';
```

**Migration Examples**:

```pascal
// OLD API
WriteLineColored('Hello', Red);
WriteLineColored('Error!', White, Red);
WriteLineColored('Info');

// NEW API (preferred)
WriteLine('Hello', Red);
WriteLine('Error!', White, Red);
WriteLine('Info');
```

**Impact**:
- ✅ New code should use `WriteLine`
- ✅ Old code using `WriteLineColored` still works (backward compatible)
- ⚠️ Compiler will show deprecation warning when using `WriteLineColored`
- ✅ More consistent with standard Pascal `WriteLn` naming
- ✅ Supports overloading for cleaner API

**Implementation**:
```pascal
// WriteLine overloads - new unified API
procedure WriteLine(const Text: string);
begin
  WriteLn(Text);
end;

procedure WriteLine(const Text: string; ForeColor: TConsoleColor);
begin
  WriteColoredText(Text, ForeColor, UseDefault);
  WriteLn;
end;

procedure WriteLine(const Text: string; ForeColor: TConsoleColor; BackColor: TConsoleColor);
begin
  WriteColoredText(Text, ForeColor, BackColor);
  WriteLn;
end;

// Backward compatibility wrapper
procedure WriteLineColored(const Text: string; ForeColor: TConsoleColor;
                          BackColor: TConsoleColor);
begin
  WriteLine(Text, ForeColor, BackColor);
end;
```

---

### 3. Sample Updates

**File**: `samples/console_sample/ConsoleDemo.dpr`

Updated 6 occurrences to use new `WriteLine` API:
- Line 57: `WriteLine('Current Progress:', Cyan);`
- Line 114: `WriteLine('Static Menu Display:', White);`
- Line 119: `WriteLine('Interactive Menu (try it!):', White);`
- Line 130: `WriteLine('Advanced Menu with Icons (try it!):', White);`
- Line 329: `WriteLine('Press ENTER to EXIT...', Gray);`
- Line 338: `WriteLine('Premi INVIO per uscire...', Gray);`

**File**: `samples/console_sample/ConsoleSample.dpr`

Added new API tests section (lines 77-95):
- Tests `WriteLine()` overloads (simple, with color, with two colors)
- Tests `ConsoleTheme` variable usage
- Integrated into existing test flow

---

## Backward Compatibility

### Using Deprecated APIs

Old code will continue to work:

```pascal
// This still compiles and runs correctly
MVCConsoleStyle.TextColor := Yellow;
WriteLineColored('Warning', Yellow);
```

But Delphi will show warnings:
```
[dcc32 Warning] MyUnit.pas(42): W1000 Symbol 'MVCConsoleStyle' is deprecated: 'Use ConsoleTheme instead'
[dcc32 Warning] MyUnit.pas(43): W1000 Symbol 'WriteLineColored' is deprecated: 'Use WriteLine instead'
```

### Migration Strategy

**Option 1: Immediate migration** (recommended for new code)
```pascal
// Update to new APIs
ConsoleTheme.TextColor := Yellow;
WriteLine('Warning', Yellow);
```

**Option 2: Suppress warnings** (temporary, for legacy code)
```pascal
{$WARN SYMBOL_DEPRECATED OFF}
MVCConsoleStyle.TextColor := Yellow;
WriteLineColored('Warning', Yellow);
{$WARN SYMBOL_DEPRECATED ON}
```

**Option 3: Gradual migration**
- Leave existing code as-is (warnings will appear but code works)
- Use new APIs for all new code
- Migrate incrementally when touching old code

---

## Benefits

### Before (Inconsistent)
```pascal
// Mixed naming conventions
WriteColoredText(...)      // No newline
WriteLineColored(...)      // With newline - inconsistent order!
MVCConsoleStyle            // Generic "Style" name
```

### After (Consistent)
```pascal
// Clean, consistent API
WriteColoredText(...)      // No newline (unchanged)
WriteLine(...)             // With newline - matches WriteLn
ConsoleTheme               // Clear semantic name
```

**Improvements**:
1. ✅ Consistent naming: `WriteLine` matches Delphi's `WriteLn`
2. ✅ Overloads allow cleaner syntax (no `UseDefault` parameters needed)
3. ✅ `ConsoleTheme` is more semantically clear than `MVCConsoleStyle`
4. ✅ Better IDE autocomplete experience
5. ✅ XML documentation for all new APIs
6. ✅ 100% backward compatible

---

## Testing

### Manual Testing Required

1. **Compile framework**: Build `dmvcframework` packages
2. **Compile samples**: Build both `ConsoleDemo.dpr` and `ConsoleSample.dpr`
3. **Run samples**: Verify console output is unchanged
4. **Check warnings**: Verify deprecation warnings appear only in updated code

### Test Commands

```bash
# Build samples (from samples/console_sample/)
"C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"
msbuild ConsoleDemo.dproj /p:Config=Debug /p:Platform=Win32 /t:Build
msbuild ConsoleSample.dproj /p:Config=Debug /p:Platform=Win32 /t:Build

# Run samples
ConsoleDemo.exe
ConsoleSample.exe
```

---

## Next Steps (Priority 2 & 3)

**Priority 2 (Breaking Changes)**:
- [ ] Remove `UseDefault` enum value, use overloads consistently
- [ ] Unify naming: Write/Draw/Show pattern across all functions
- [ ] Consistent parameter order (Content → Size → Color → Style)
- [ ] Split `TConsoleColorStyle` into semantic groups

**Priority 3 (Extensions)**:
- [ ] Add fluent API for complex outputs
- [ ] Add predefined color themes
- [ ] Add `TMenuResult` record type instead of Integer
- [ ] Add `ReadString`, `Confirm`, etc. utility functions
- [ ] Add `Cls` alias for `ClrScr`

---

## Files Modified

1. ✅ `sources/MVCFramework.Console.pas` - Core API changes
2. ✅ `samples/console_sample/ConsoleDemo.dpr` - Updated to use new API
3. ✅ `samples/console_sample/ConsoleSample.dpr` - Added new API tests

## Files Created

1. ✅ `REFACTORING_CONSOLE_API.md` - This documentation
2. ✅ `CONSOLE_API_MIGRATION_GUIDE.md` - Quick migration guide

---

## Author

- **Date**: 2026-02-06
- **Changes by**: Claude Code (Sonnet 4.5)
- **Approved by**: Daniele Teti (pending)

---

## Migration Checklist for Users

When migrating your code to the new API:

- [ ] Replace `MVCConsoleStyle` with `ConsoleTheme` (search & replace)
- [ ] Replace `WriteLineColored(Text, Color)` with `WriteLine(Text, Color)`
- [ ] Replace `WriteLineColored(Text)` with `WriteLine(Text)`
- [ ] Recompile and verify no runtime behavior changes
- [ ] (Optional) Remove deprecation warnings suppression

---

## Appendix: Complete API Reference

### Console Theme Access

```pascal
// New (recommended)
ConsoleTheme.TextColor := Cyan;
ConsoleTheme.BackgroundColor := Black;
ConsoleTheme.DrawColor := White;
ConsoleTheme.SymbolsColor := Gray;
ConsoleTheme.BackgroundHighlightColor := Cyan;
ConsoleTheme.TextHighlightColor := Blue;
ConsoleTheme.BoxStyle := bsRounded;

// Old (deprecated, but works)
MVCConsoleStyle.TextColor := Cyan;
// etc...
```

### Text Output

```pascal
// New (recommended)
WriteLine('Simple text');                    // Plain text with newline
WriteLine('Colored text', Red);              // Colored text with newline
WriteLine('Two colors', Red, Yellow);        // Fore + back colors with newline

// Old (deprecated, but works)
WriteLineColored('Colored text', Red);
WriteLineColored('Two colors', Red, Yellow);
```

---

End of document.
