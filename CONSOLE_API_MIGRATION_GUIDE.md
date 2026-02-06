# Console API Migration Guide

Quick reference guide for migrating from old Console API to the new refactored API.

## Quick Changes

### 1. MVCConsoleStyle → ConsoleTheme

```pascal
// OLD (deprecated)
MVCConsoleStyle.TextColor := Yellow;
MVCConsoleStyle.DrawColor := White;
MVCConsoleStyle.BoxStyle := bsRounded;

// NEW (recommended)
ConsoleTheme.TextColor := Yellow;
ConsoleTheme.DrawColor := White;
ConsoleTheme.BoxStyle := bsRounded;
```

### 2. WriteLineColored → WriteLine

```pascal
// OLD (deprecated)
WriteLineColored('Message');
WriteLineColored('Warning', Yellow);
WriteLineColored('Error', White, Red);

// NEW (recommended)
WriteLine('Message');
WriteLine('Warning', Yellow);
WriteLine('Error', White, Red);
```

## Search & Replace Commands

For quick migration in your codebase:

### Visual Studio Code / VS
1. Find: `MVCConsoleStyle`
2. Replace: `ConsoleTheme`
3. Find: `WriteLineColored`
4. Replace: `WriteLine`

### Command Line (ripgrep)
```bash
# Preview changes
rg "MVCConsoleStyle" --files-with-matches
rg "WriteLineColored" --files-with-matches

# Replace (requires rg with --replace)
rg "MVCConsoleStyle" --files-with-matches --replace "ConsoleTheme"
rg "WriteLineColored" --files-with-matches --replace "WriteLine"
```

## Compatibility

- ✅ **Old code still works** - No breaking changes
- ⚠️ **Deprecation warnings** - Compiler will warn but code compiles
- ✅ **No runtime changes** - Behavior is identical

## Examples

### Before (Old API)
```pascal
procedure ShowStatus;
begin
  ClrScr;
  MVCConsoleStyle.TextColor := Cyan;
  WriteLineColored('Server Status', White);
  WriteLineColored('Status: Running', Green);
  WriteLineColored('Port: 8080', Yellow);
end;
```

### After (New API)
```pascal
procedure ShowStatus;
begin
  ClrScr;
  ConsoleTheme.TextColor := Cyan;
  WriteLine('Server Status', White);
  WriteLine('Status: Running', Green);
  WriteLine('Port: 8080', Yellow);
end;
```

### Side-by-Side Comparison

| Old API | New API | Notes |
|---------|---------|-------|
| `MVCConsoleStyle.TextColor` | `ConsoleTheme.TextColor` | More semantic name |
| `WriteLineColored(Text)` | `WriteLine(Text)` | Simpler, matches WriteLn |
| `WriteLineColored(Text, Color)` | `WriteLine(Text, Color)` | Same functionality |
| `WriteLineColored(Text, FG, BG)` | `WriteLine(Text, FG, BG)` | Same functionality |

## Need Help?

- See `REFACTORING_CONSOLE_API.md` for detailed technical changes
- See `samples/console_sample/ConsoleDemo.dpr` for complete demo with all features
- See `samples/console_sample/ConsoleSample.dpr` for basic tests including new API

## Rollback

If you need to rollback:
```bash
git checkout HEAD -- sources/MVCFramework.Console.pas
git checkout HEAD -- samples/console_sample/ConsoleDemo.dpr
```

---

**Last Updated**: 2026-02-06
