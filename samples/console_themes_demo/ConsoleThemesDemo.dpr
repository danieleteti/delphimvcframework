program ConsoleThemesDemo;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MVCFramework.Console;

const
  TOTAL_THEMES = 15; // 14 built-in + 1 custom derivation

// ---------------------------------------------------------------------------
// Screen 1: style swatches + static widgets
// ---------------------------------------------------------------------------
procedure ShowThemeStatic(const AName: string; AThemeIdx: Integer);
var
  Headers: TStringArray;
  Data:    TStringMatrix;
begin
  ClrScr;
  WriteHeader(Format('%s Theme  [%d/%d]', [AName, AThemeIdx, TOTAL_THEMES]), 70);
  WriteLn;

  // ── Style swatches ─────────────────────────────────────────────────────────
  WriteLine('TConsoleColorStyle slots:', White);
  WriteLn;
  WriteAnsiLine(ConsoleTheme.Text,          '  Text          : Normal text color                        ');
  WriteAnsiLine(ConsoleTheme.Draw,          '  Draw          : Borders and box-drawing characters       ');
  WriteAnsiLine(ConsoleTheme.Symbols,       '  Symbols (dim) : Dimmed prefix for list items             ');
  WriteAnsiLine(ConsoleTheme.Highlight,     '  Highlight     : Selected / active item (solid bg+bright) ');
  WriteAnsiLine(ConsoleTheme.HighlightText, '  HighlightText : Header and title text (bright, no bg)    ');
  WriteLn;

  // ── Log messages ───────────────────────────────────────────────────────────
  WriteInfo   ('This is an informational message');
  WriteSuccess('Operation completed successfully');
  WriteWarning('Something looks unusual here');
  WriteError  ('A critical failure occurred');
  WriteLn;

  // ── Box ────────────────────────────────────────────────────────────────────
  Box(AName + ' Status', [
    'Server  : ONLINE',
    'Database: CONNECTED',
    'Cache   : WARNING',
    'Backup  : ERROR'
  ], 42);
  WriteLn;

  // ── Table ──────────────────────────────────────────────────────────────────
  SetLength(Headers, 3);
  Headers[0] := 'Framework';
  Headers[1] := 'Language';
  Headers[2] := 'Stars';

  SetLength(Data, 3);
  SetLength(Data[0], 3); Data[0][0] := 'DMVCFramework'; Data[0][1] := 'Delphi'; Data[0][2] := '5/5';
  SetLength(Data[1], 3); Data[1][0] := 'Spring Boot';   Data[1][1] := 'Java';   Data[1][2] := '4/5';
  SetLength(Data[2], 3); Data[2][0] := 'FastAPI';       Data[2][1] := 'Python'; Data[2][2] := '4/5';

  Table(Headers, Data, 'Top Frameworks');
  WriteLn;

  // ── Bulleted list ──────────────────────────────────────────────────────────
  WriteFormattedList('Features applied by this theme', [
    'Custom foreground + background colors',
    'ANSI Bright and Dim style attributes',
    'Solid-background highlight for selection',
    'Theme-aware box-drawing style'
  ], TListStyle.lsBullet);
  WriteLn;

  WriteLine('Press ENTER to see interactive widgets...', Gray);
  ReadLn;
end;

// ---------------------------------------------------------------------------
// Screen 2: interactive menu + progress bar (fresh screen, no overlap risk)
// ---------------------------------------------------------------------------
procedure ShowThemeInteractive(const AName: string);
var
  MenuItems: TStringArray;
  P:         IProgress;
  Sel, I:    Integer;
begin
  ClrScr;
  WriteHeader(AName + ' Theme - Interactive Widgets', 70);
  WriteLn;

  // ── Interactive menu ───────────────────────────────────────────────────────
  WriteLine('Menu - use arrow keys, ENTER to select, ESC to cancel:', White);
  WriteLn;
  SetLength(MenuItems, 4);
  MenuItems[0] := 'Start server';
  MenuItems[1] := 'Stop server';
  MenuItems[2] := 'View logs';
  MenuItems[3] := 'Cancel';

  Sel := Menu('Choose an action', MenuItems);
  if Sel >= 0 then
    WriteSuccess('Selected: ' + MenuItems[Sel])
  else
    WriteWarning('Selection cancelled');
  WriteLn;

  // ── Progress bars ──────────────────────────────────────────────────────────
  HideCursor;
  try
    WriteAnsiLine(ConsoleTheme.HighlightText, 'Determinate progress:');
    WriteLn;
    P := Progress('Processing ' + AName + ' theme', 25);
    for I := 1 to 25 do
    begin
      P.Update(I);
      Sleep(30);
    end;
    P := nil;
    WriteLn;
    WriteLn;

    WriteAnsiLine(ConsoleTheme.HighlightText, 'Indeterminate spinner:');
    WriteLn;
    P := Progress('Loading data');
    for I := 1 to 20 do
    begin
      P.Update(I);
      Sleep(40);
    end;
    P.Complete;
    P := nil;
    WriteLn;
  finally
    ShowCursor;
  end;
  WriteLn;
end;

// ---------------------------------------------------------------------------
// Screen 1b: custom theme - shows the code, then static widgets
// Derived from the last built-in theme (Midnight) as a live example.
// ---------------------------------------------------------------------------
procedure ShowCustomThemeStatic;
begin
  ClrScr;
  WriteHeader(Format('Custom Theme  [%d/%d]', [TOTAL_THEMES, TOTAL_THEMES]), 70);
  WriteLn;

  WriteAnsiLine(ConsoleTheme.HighlightText,
    'Derive a custom theme by copying a built-in and overriding fields:');
  WriteLn;

  // ── Code snippet ──────────────────────────────────────────────────────────
  WriteAnsiLine(ConsoleTheme.Symbols, '  var');
  WriteAnsiLine(ConsoleTheme.Symbols, '    MyTheme: TConsoleColorStyle;');
  WriteAnsiLine(ConsoleTheme.Symbols, '  begin');
  WriteAnsiLine(ConsoleTheme.Text,
    '    MyTheme              := ConsoleThemeMidnight;                    // base');
  WriteAnsiLine(ConsoleTheme.Highlight,
    '    MyTheme.Highlight    := Back.Yellow + Fore.Black + Style.Bright;// override ');
  WriteAnsiLine(ConsoleTheme.HighlightText,
    '    MyTheme.HighlightText:= Fore.Yellow + Style.Bright;             // override');
  WriteAnsiLine(ConsoleTheme.Draw,
    '    MyTheme.BoxStyle     := TBoxStyle.bsThick;                      // override');
  WriteAnsiLine(ConsoleTheme.Symbols, '    SetConsoleTheme(MyTheme);');
  WriteAnsiLine(ConsoleTheme.Symbols, '  end;');
  WriteLn;

  // ── Swatches ──────────────────────────────────────────────────────────────
  WriteAnsiLine(ConsoleTheme.HighlightText, 'Result - all widgets now use the custom theme:');
  WriteLn;
  WriteAnsiLine(ConsoleTheme.Text,          '  Text          : Normal text color                        ');
  WriteAnsiLine(ConsoleTheme.Draw,          '  Draw          : Borders (now thick-line)                 ');
  WriteAnsiLine(ConsoleTheme.Symbols,       '  Symbols (dim) : Dimmed prefix for list items             ');
  WriteAnsiLine(ConsoleTheme.Highlight,     '  Highlight     : Yellow solid background (was teal)       ');
  WriteAnsiLine(ConsoleTheme.HighlightText, '  HighlightText : Yellow bright text (was yellow/default)  ');
  WriteLn;

  // ── Log messages ───────────────────────────────────────────────────────────
  WriteInfo   ('This is an informational message');
  WriteSuccess('Operation completed successfully');
  WriteWarning('Something looks unusual here');
  WriteError  ('A critical failure occurred');
  WriteLn;

  // ── Box (double-line style) ────────────────────────────────────────────────
  Box('Custom Status', [
    'Server  : ONLINE',
    'Database: CONNECTED',
    'Cache   : WARNING',
    'Backup  : ERROR'
  ], 42);
  WriteLn;

  WriteLine('Press ENTER to see interactive widgets...', Gray);
  ReadLn;
end;

// ---------------------------------------------------------------------------
// Cycles through all predefined themes + one custom derivation.
// ---------------------------------------------------------------------------
procedure RunDemo;
var
  Saved:    TConsoleColorStyle;
  ThemeIdx: Integer;
  Custom:   TConsoleColorStyle;

  procedure NextTheme(const AName: string; const ATheme: TConsoleColorStyle);
  begin
    Inc(ThemeIdx);
    SetConsoleTheme(ATheme);
    ShowThemeStatic(AName, ThemeIdx);
    ShowThemeInteractive(AName);
    if ThemeIdx < TOTAL_THEMES then
    begin
      WriteLine(Format('Theme %d/%d  -  press ENTER for the next theme...',
        [ThemeIdx, TOTAL_THEMES]), Gray);
      ReadLn;
    end;
  end;

begin
  Saved := ConsoleTheme;
  ThemeIdx := 0;
  try
    NextTheme('Default',    ConsoleThemeDefault);
    NextTheme('Classic',    ConsoleThemeClassic);
    NextTheme('Matrix',     ConsoleThemeMatrix);
    NextTheme('Sunset',     ConsoleThemeSunset);
    NextTheme('Ocean',      ConsoleThemeOcean);
    NextTheme('Monochrome', ConsoleThemeMonochrome);
    NextTheme('Magenta',    ConsoleThemeMagenta);
    NextTheme('Alert',      ConsoleThemeAlert);
    NextTheme('Navy',       ConsoleThemeNavy);
    NextTheme('Forest',     ConsoleThemeForest);
    NextTheme('Slate',      ConsoleThemeSlate);
    NextTheme('Paper',      ConsoleThemePaper);
    NextTheme('Burgundy',   ConsoleThemeBurgundy);
    NextTheme('Midnight',   ConsoleThemeMidnight);

    // ── Custom theme derived from Midnight (the last built-in) ───────────────
    WriteLine(Format('Theme %d/%d  -  press ENTER for the next theme...',
      [ThemeIdx, TOTAL_THEMES]), Gray);
    ReadLn;

    Custom              := ConsoleThemeMidnight;
    Custom.Highlight    := Back.Yellow + Fore.Black + Style.Bright;
    Custom.HighlightText:= Fore.Yellow + Style.Bright;
    Custom.BoxStyle     := TBoxStyle.bsThick;
    SetConsoleTheme(Custom);

    ShowCustomThemeStatic;
    ShowThemeInteractive('Custom');

    WriteLn;
    WriteLine(Format('All %d themes shown - press ENTER to exit.', [TOTAL_THEMES]), Gray);
    ReadLn;

    ClrScr;
    WriteHeader('DEMO COMPLETE', 70);
    WriteLn;
  finally
    SetConsoleTheme(Saved);
  end;
end;

begin
  try
    EnableUTF8Console;
    ClrScr;

    WriteHeader('MVCFramework.Console - Theme Showcase', 70);
    WriteLn;
    WriteLine(Format('Cycles through %d themes (%d built-in + 1 custom derivation).',
      [TOTAL_THEMES, TOTAL_THEMES - 1]), White);
    WriteLine('6 themes use a non-black background (Navy, Forest, Slate, Paper, Burgundy, Midnight).', White);
    WriteLine('Each theme: style swatches, log messages, box, table, list,', White);
    WriteLine('then a fresh screen with interactive menu and progress bars.', White);
    WriteLn;
    WriteLine('Press ENTER to start...', Gray);
    ReadLn;

    RunDemo;

  except
    on E: Exception do
    begin
      WriteLn;
      WriteError('Unexpected error: ' + E.Message);
      WriteLn;
      WriteLine('Press ENTER to exit...', Gray);
      ReadLn;
    end;
  end;
end.
