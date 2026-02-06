# Console Simplified API - New Features

## Overview

Added unified, simplified API for the most common console operations. The new API reduces complexity by providing single, well-named functions instead of multiple confusing variants.

**Date**: 2026-02-06
**Version**: DMVCFramework Console v2.0

---

## 🎯 Design Goals

1. **One function per use case** - No more choosing between "Simple", "Interactive", "Advanced" variants
2. **Progressive complexity** - Start simple, add parameters only when needed via overloads
3. **Auto-cleanup** - Interfaces with reference counting for automatic resource management
4. **Consistent theming** - All functions respect `ConsoleTheme` settings
5. **Intuitive naming** - Clear function names that describe what they do

---

## 📚 New Unified API

### 1. Menu() - Unified Menu Function

**Before** (3 confusing functions):
```pascal
ShowSimpleMenu(Title, Items, SelectedIndex);          // Static display only
ShowInteractiveMenu(Title, Items, DefaultIndex);      // Interactive
ShowAdvancedMenu(Title, Items, DefaultIndex, Color);  // With styling
```

**After** (1 simple function):
```pascal
function Menu(const Items: TStringArray): Integer;
function Menu(const Title: string; const Items: TStringArray): Integer;
function Menu(const Title: string; const Items: TStringArray; DefaultIndex: Integer): Integer;

// Usage:
Choice := Menu(['Start', 'Settings', 'Exit']);
Choice := Menu('Main Menu', ['Start', 'Settings', 'Exit']);
Choice := Menu('Main Menu', ['Start', 'Settings', 'Exit'], 0);  // With default

// Returns: Selected index (0-based) or -1 if cancelled (ESC)
```

**Benefits:**
- ✅ Always interactive (keyboard navigation with arrows)
- ✅ Auto-styled via ConsoleTheme
- ✅ Progressive overloads - simple by default
- ✅ Clear return value (-1 = cancelled)

---

### 2. Table() - Unified Table with Auto-Sizing

**Before** (2 functions, manual width calculation):
```pascal
WriteSimpleTable(Headers, Data, Style);
WriteColoredTable(Headers, Data, HeaderColor, DrawColor, DataColor, BoxStyle);  // 6 params!
```

**After** (1 function, auto-sizing):
```pascal
procedure Table(const Headers: TStringArray; const Data: TStringMatrix);
procedure Table(const Headers: TStringArray; const Data: TStringMatrix; const Title: string);

// Usage:
Table(['ID', 'Name', 'Status'], Data);
Table(['ID', 'Name', 'Status'], Data, 'User List');  // With title

// Auto-calculates column widths based on content!
// Auto-styled via ConsoleTheme
```

**Benefits:**
- ✅ Automatic column sizing - no manual calculation
- ✅ Optional title
- ✅ Auto-styled via ConsoleTheme
- ✅ Clean, simple parameters

---

### 2b. TableMenu() - Interactive Table with Selection ✨ NEW

**New feature** (table + menu combined):
```pascal
function TableMenu(const Headers: TStringArray; const Data: TStringMatrix): Integer;
function TableMenu(const Title: string; const Headers: TStringArray; const Data: TStringMatrix): Integer;
function TableMenu(const Title: string; const Headers: TStringArray; const Data: TStringMatrix; DefaultIndex: Integer): Integer;

// Usage:
SelectedRow := TableMenu(Headers, Data);
SelectedRow := TableMenu('Select User', Headers, Data);
SelectedRow := TableMenu('Select User', Headers, Data, 0);  // With default

// Use arrow keys to navigate rows
// Press Enter to select
// Press ESC to cancel
// Returns: Selected row index (0-based) or -1 if cancelled
```

**Benefits:**
- ✅ Combines table display + menu navigation
- ✅ Perfect for database record selection
- ✅ Full keyboard navigation (arrows, Enter, ESC)
- ✅ Highlights selected row visually

**Use Cases:**
- Select a record from database results
- Choose an item from a list
- Navigate large datasets with visual feedback

---

### 3. Box() - Unified Box with Auto-Positioning

**Before** (2 functions, different concepts):
```pascal
DrawBox(X, Y, Width, Height, Style, Title);           // Manual positioning
DrawSimpleBox(Title, Content, Width, TextColor, Style);  // Auto-positioning
```

**After** (1 function):
```pascal
procedure Box(const Content: TStringArray);
procedure Box(const Title: string; const Content: TStringArray);
procedure Box(const Title: string; const Content: TStringArray; Width: Integer);

// Usage:
Box(['Server: ONLINE', 'CPU: 45%']);
Box('Status', ['Server: ONLINE', 'CPU: 45%']);
Box('Status', ['Server: ONLINE', 'CPU: 45%'], 50);  // Custom width

// Auto-positioning - no manual X,Y coordinates!
// Auto-width if not specified
```

**Benefits:**
- ✅ No manual positioning needed
- ✅ Auto-width by default
- ✅ Auto-styled via ConsoleTheme
- ✅ Progressive complexity

---

### 4. Progress() - Unified Progress with Interface

**Before** (4 different ways!):
```pascal
TMVCConsoleProgressBar.Create(...)  // OOP, manual cleanup
ShowSimpleProgressBar(...)          // Procedural, snapshot only
ShowProgressAnimation(...)          // Animation
ShowLoadingSpinner(...)             // Spinner
```

**After** (1 function with interface):
```pascal
type
  IProgress = interface
    procedure Update(Value: Integer);
    procedure Increment(Amount: Integer = 1);
    procedure SetMessage(const Msg: string);
    procedure Complete;
  end;

function Progress(const Title: string; MaxValue: Integer): IProgress;
function Progress(const Title: string): IProgress;  // Indeterminate

// Usage - Determinate:
var P := Progress('Downloading', 100);
for i := 1 to 100 do
begin
  P.Update(i);
  Sleep(50);
end;
P := nil;  // Auto cleanup!

// Usage - Indeterminate (spinner):
var P := Progress('Loading...');
// long operation...
P := nil;  // Auto cleanup!
```

**Benefits:**
- ✅ Interface with reference counting = auto cleanup
- ✅ Same API for determinate/indeterminate
- ✅ Real-time updates
- ✅ No manual resource management

---

### 5. Confirm() - Quick Yes/No Prompts

**New function** (didn't exist before):
```pascal
function Confirm(const Question: string): Boolean;
function Confirm(const Question: string; DefaultYes: Boolean): Boolean;

// Usage:
if Confirm('Delete file?') then
  Delete(FileName);

if Confirm('Continue?', False) then  // Default = No
  Process();

// Returns: True if user confirms (Y/Yes), False otherwise
```

**Benefits:**
- ✅ Very common use case, now built-in
- ✅ Clear boolean return
- ✅ Configurable default

---

### 6. Choose() - Quick Single Choice

**New function** (alternative to full Menu for simple cases):
```pascal
function Choose(const Question: string; const Options: TStringArray): Integer;

// Usage:
case Choose('Select mode:', ['Fast', 'Normal', 'Safe']) of
  0: RunFast();
  1: RunNormal();
  2: RunSafe();
  -1: WriteLn('Cancelled');
end;

// Displays numbered list, user enters number
// Returns: Selected index (0-based) or -1 if invalid
```

**Benefits:**
- ✅ Simpler than Menu() for quick choices
- ✅ Text-based input (no keyboard navigation)
- ✅ Perfect for non-interactive scripts

---

## 📊 Comparison Summary

| Feature | Old API | New API | Improvement |
|---------|---------|---------|-------------|
| **Menu** | 3 functions<br>(Simple/Interactive/Advanced) | **1 function**<br>`Menu()` | 66% fewer functions |
| **Table** | 2 functions<br>Manual width calculation | **1 function**<br>Auto-sizing | 50% fewer + auto-sizing |
| **Box** | 2 functions<br>Manual positioning | **1 function**<br>Auto-positioning | 50% fewer + auto-positioning |
| **Progress** | 4 different ways<br>Manual cleanup | **1 function**<br>Auto-cleanup | 75% fewer + interface |
| **Confirm** | ❌ Missing | **New!**<br>`Confirm()` | Essential utility added |
| **Choose** | ❌ Missing | **New!**<br>`Choose()` | Quick choice added |

---

## 🎨 ConsoleTheme Integration

All new functions automatically use `ConsoleTheme` for consistent styling:

```pascal
// Change global theme
ConsoleTheme.TextColor := Yellow;
ConsoleTheme.DrawColor := Magenta;
ConsoleTheme.BoxStyle := bsDouble;

// All subsequent calls use new theme
Menu(['Option 1', 'Option 2']);     // Styled with Yellow/Magenta
Box('Status', ['Line 1', 'Line 2']); // Double-line box
Table(Headers, Data);                // Yellow/Magenta colors
```

No need to pass colors to every function!

---

## 📝 Code Examples

### Before: Complex and Verbose

```pascal
// Menu - which function to use?
ShowInteractiveMenu('Main Menu', Items, 0, 'Use arrows...');

// Table - manual color management
WriteColoredTable(Headers, Data, Yellow, White, Gray, bsRounded);

// Progress - manual cleanup
var Progress := TMVCConsoleProgressBar.Create('Download', 100, 50);
try
  for i := 1 to 100 do
  begin
    Progress.SetPosition(i);
    Progress.UpdateDisplay;  // Manual update!
    Sleep(50);
  end;
finally
  Progress.Free;  // Manual cleanup!
end;

// Box - manual positioning
DrawSimpleBox('Status', Content, 50, Yellow, bsRounded);

// Confirm - had to implement manually
Write('Continue? [Y/N]: ');
ReadLn(Response);
if UpperCase(Response) = 'Y' then
  // ...
```

### After: Simple and Clean

```pascal
// Menu - obvious choice
Menu('Main Menu', Items);

// Table - auto-styled
Table(Headers, Data);

// Progress - auto cleanup
var P := Progress('Download', 100);
for i := 1 to 100 do
begin
  P.Update(i);  // Auto display!
  Sleep(50);
end;
P := nil;  // Auto cleanup!

// Box - auto-styled
Box('Status', Content);

// Confirm - built-in
if Confirm('Continue?') then
  // ...
```

**Result**: ~60% less code, much clearer intent!

---

## 🔄 Backward Compatibility

All old functions are still available (not deprecated) for backward compatibility:

```pascal
// These still work:
ShowInteractiveMenu(...)
WriteSimpleTable(...)
DrawSimpleBox(...)
// etc.
```

**Recommendation**: Use new unified API for new code, but no need to migrate existing code.

---

## 🚀 Migration Guide

### Quick Migration Checklist

**Menu**:
- ❌ `ShowInteractiveMenu(Title, Items, Index)`
- ✅ `Menu(Title, Items, Index)`

**Table**:
- ❌ `WriteSimpleTable(Headers, Data, Style)`
- ✅ `Table(Headers, Data)`

**Box**:
- ❌ `DrawSimpleBox(Title, Content, Width, Color, Style)`
- ✅ `Box(Title, Content, Width)`

**Progress**:
- ❌ `var P := TMVCConsoleProgressBar.Create(...); try ... finally P.Free; end;`
- ✅ `var P := Progress(...); ... P := nil;`

**Confirm** (new):
- ✅ `if Confirm('Question?') then ...`

**Choose** (new):
- ✅ `case Choose('Select:', Options) of ...`

---

## 📖 Full API Reference

### Menu()

```pascal
function Menu(const Items: TStringArray): Integer;
// Simplest form - no title, default index 0

function Menu(const Title: string; const Items: TStringArray): Integer;
// With title, default index 0

function Menu(const Title: string; const Items: TStringArray;
             DefaultIndex: Integer): Integer;
// Full control - title and default selection
```

**Returns**: Selected index (0-based) or -1 if cancelled with ESC

---

### Table()

```pascal
procedure Table(const Headers: TStringArray; const Data: TStringMatrix);
// Auto-sizing, no title

procedure Table(const Headers: TStringArray; const Data: TStringMatrix;
               const Title: string);
// Auto-sizing with title
```

**Features**:
- Auto-calculates column widths based on content
- Uses ConsoleTheme colors automatically
- Supports any number of columns

---

### Box()

```pascal
procedure Box(const Content: TStringArray);
// Auto-width, no title, auto-positioning

procedure Box(const Title: string; const Content: TStringArray);
// Auto-width with title

procedure Box(const Title: string; const Content: TStringArray;
             Width: Integer);
// Custom width
```

**Features**:
- Auto-positioning (no X,Y coordinates needed)
- Auto-width if not specified
- Uses ConsoleTheme style automatically

---

### Progress()

```pascal
function Progress(const Title: string; MaxValue: Integer): IProgress;
// Determinate progress (0 to MaxValue)

function Progress(const Title: string): IProgress;
// Indeterminate spinner
```

**IProgress Interface**:
```pascal
procedure Update(Value: Integer);     // Set progress to specific value
procedure Increment(Amount: Integer); // Increment by amount (default 1)
procedure SetMessage(const Msg: string); // Change title
procedure Complete;                   // Mark as complete
```

**Features**:
- Auto-cleanup via interface reference counting
- Real-time visual updates
- Determinate (percentage) or indeterminate (spinner)

---

### Confirm()

```pascal
function Confirm(const Question: string): Boolean;
// Default = Yes

function Confirm(const Question: string; DefaultYes: Boolean): Boolean;
// Custom default
```

**Returns**: True if user confirms (Y/Yes), False otherwise
**Accepts**: Y, y, Yes, yes, YES (case-insensitive)

---

### Choose()

```pascal
function Choose(const Question: string; const Options: TStringArray): Integer;
```

**Returns**: Selected index (0-based) or -1 if invalid input
**Displays**: Numbered list (1, 2, 3, ...)
**Input**: User enters number

---

## 🎓 Best Practices

1. **Use new unified API for all new code**
   ```pascal
   ✅ Menu(Items)
   ❌ ShowInteractiveMenu(Items, 0, '')
   ```

2. **Let functions auto-style via ConsoleTheme**
   ```pascal
   ✅ Table(Headers, Data)
   ❌ WriteColoredTable(Headers, Data, Yellow, White, Gray, bsRounded)
   ```

3. **Use interface auto-cleanup for Progress**
   ```pascal
   ✅ var P := Progress('Title', 100); ... P := nil;
   ❌ var P := TMVCConsoleProgressBar.Create(...); try ... finally P.Free; end;
   ```

4. **Use Confirm() for yes/no prompts**
   ```pascal
   ✅ if Confirm('Delete?') then ...
   ❌ Write('Delete? [Y/N]: '); ReadLn(R); if UpperCase(R) = 'Y' then ...
   ```

5. **Use Choose() for simple numbered choices**
   ```pascal
   ✅ case Choose('Mode:', ['Fast', 'Normal']) of ...
   ❌ WriteLn('[1] Fast'); WriteLn('[2] Normal'); ReadLn(R); ...
   ```

---

## 📦 What's Next?

Future enhancements being considered:

- **ReadString(Prompt)** - Input with prompt
- **ReadPassword(Prompt)** - Masked password input
- **ReadInteger(Prompt, Min, Max)** - Validated numeric input
- **ApplyTheme(Preset)** - Quick theme switching (Dark, Light, Matrix, etc.)
- **Advanced table features** - Sorting, pagination, filtering

---

## 🙏 Credits

**Author**: Daniele Teti
**Contributors**: DMVCFramework Team
**License**: Apache License 2.0

---

End of document.
