program ConsoleColors;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MVCFramework.Console;

procedure ShowColorPalette;
const
  SAMPLE = '  Sample  ';
begin
  WriteLn;
  WriteLn('=== Foreground Colors ===');
  WriteLn;
  WriteLn(Fore.Black       + Back.White + SAMPLE + Style.ResetAll + '  Fore.Black');
  WriteLn(Fore.DarkRed     + SAMPLE + Style.ResetAll + '  Fore.DarkRed');
  WriteLn(Fore.DarkGreen   + SAMPLE + Style.ResetAll + '  Fore.DarkGreen');
  WriteLn(Fore.DarkYellow  + SAMPLE + Style.ResetAll + '  Fore.DarkYellow');
  WriteLn(Fore.DarkBlue    + SAMPLE + Style.ResetAll + '  Fore.DarkBlue');
  WriteLn(Fore.DarkMagenta + SAMPLE + Style.ResetAll + '  Fore.DarkMagenta');
  WriteLn(Fore.DarkCyan    + SAMPLE + Style.ResetAll + '  Fore.DarkCyan');
  WriteLn(Fore.Gray        + SAMPLE + Style.ResetAll + '  Fore.Gray');
  WriteLn(Fore.DarkGray    + SAMPLE + Style.ResetAll + '  Fore.DarkGray');
  WriteLn(Fore.Red         + SAMPLE + Style.ResetAll + '  Fore.Red');
  WriteLn(Fore.Green       + SAMPLE + Style.ResetAll + '  Fore.Green');
  WriteLn(Fore.Yellow      + SAMPLE + Style.ResetAll + '  Fore.Yellow');
  WriteLn(Fore.Blue        + SAMPLE + Style.ResetAll + '  Fore.Blue');
  WriteLn(Fore.Magenta     + SAMPLE + Style.ResetAll + '  Fore.Magenta');
  WriteLn(Fore.Cyan        + SAMPLE + Style.ResetAll + '  Fore.Cyan');
  WriteLn(Fore.White       + SAMPLE + Style.ResetAll + '  Fore.White');

  WriteLn;
  WriteLn('=== Background Colors ===');
  WriteLn;
  WriteLn(Back.DarkRed     + '          ' + Style.ResetAll + '  Back.DarkRed');
  WriteLn(Back.DarkGreen   + '          ' + Style.ResetAll + '  Back.DarkGreen');
  WriteLn(Back.DarkYellow  + '          ' + Style.ResetAll + '  Back.DarkYellow');
  WriteLn(Back.DarkBlue    + '          ' + Style.ResetAll + '  Back.DarkBlue');
  WriteLn(Back.DarkMagenta + '          ' + Style.ResetAll + '  Back.DarkMagenta');
  WriteLn(Back.DarkCyan    + '          ' + Style.ResetAll + '  Back.DarkCyan');
  WriteLn(Back.Red         + '          ' + Style.ResetAll + '  Back.Red');
  WriteLn(Back.Green       + '          ' + Style.ResetAll + '  Back.Green');
  WriteLn(Back.Yellow      + '          ' + Style.ResetAll + '  Back.Yellow');
  WriteLn(Back.Blue        + '          ' + Style.ResetAll + '  Back.Blue');
  WriteLn(Back.Magenta     + '          ' + Style.ResetAll + '  Back.Magenta');
  WriteLn(Back.Cyan        + '          ' + Style.ResetAll + '  Back.Cyan');
end;

procedure ShowStyles;
begin
  WriteLn;
  WriteLn('=== Styles ===');
  WriteLn;
  WriteLn(Style.Bright + 'Style.Bright (bold)' + Style.ResetAll);
  WriteLn(Style.Dim    + 'Style.Dim (faint)'   + Style.ResetAll);
  WriteLn(Style.Normal + 'Style.Normal'         + Style.ResetAll);
end;

procedure ShowComposition;
begin
  WriteLn;
  WriteLn('=== Composing Colors ===');
  WriteLn;
  WriteLn('Colors compose by simple string concatenation, just like Python''s colorama:');
  WriteLn;

  // Fore + Back
  WriteLn(Fore.White + Back.DarkBlue + ' White on Blue ' + Style.ResetAll);
  WriteLn(Fore.Black + Back.Yellow   + ' Black on Yellow ' + Style.ResetAll);
  WriteLn(Fore.White + Back.DarkRed  + ' White on DarkRed ' + Style.ResetAll);

  WriteLn;

  // Style + Fore
  WriteLn(Style.Bright + Fore.Green + 'Bright Green' + Style.ResetAll);
  WriteLn(Style.Dim    + Fore.Cyan  + 'Dim Cyan'     + Style.ResetAll);

  WriteLn;

  // Inline coloring within a sentence
  WriteLn(
    'Status: ' +
    Fore.Green + 'OK' + Style.ResetAll +
    ' | Items: ' +
    Fore.Cyan + '42' + Style.ResetAll +
    ' | Errors: ' +
    Fore.Red + '0' + Style.ResetAll
  );
end;

procedure ShowPracticalExamples;
begin
  WriteLn;
  WriteLn('=== Practical Examples ===');
  WriteLn;

  // Log-style output
  WriteLn(Fore.DarkGray + '[' + Fore.Green  + 'INFO ' + Fore.DarkGray + ']' + Style.ResetAll + ' Server started on port 8080');
  WriteLn(Fore.DarkGray + '[' + Fore.Yellow + 'WARN ' + Fore.DarkGray + ']' + Style.ResetAll + ' Cache miss rate above 50%');
  WriteLn(Fore.DarkGray + '[' + Fore.Red    + 'ERROR' + Fore.DarkGray + ']' + Style.ResetAll + ' Connection to database lost');
  WriteLn(Fore.DarkGray + '[' + Fore.Cyan   + 'DEBUG' + Fore.DarkGray + ']' + Style.ResetAll + ' Processing request #1234');

  WriteLn;

  // Status badges
  WriteLn('  ' + Back.Green   + Fore.Black + ' PASS ' + Style.ResetAll + '  TestUserAuthentication');
  WriteLn('  ' + Back.Green   + Fore.Black + ' PASS ' + Style.ResetAll + '  TestOrderCreation');
  WriteLn('  ' + Back.Red     + Fore.White + ' FAIL ' + Style.ResetAll + '  TestPaymentTimeout');
  WriteLn('  ' + Back.Yellow  + Fore.Black + ' SKIP ' + Style.ResetAll + '  TestExternalAPI');

  WriteLn;

  // HTTP status badges (Gin-style)
  WriteLn('  ' + Fore.Green  + ' 200 ' + Style.ResetAll + ' ' + Fore.Cyan    + 'GET    ' + Style.ResetAll + ' /api/people');
  WriteLn('  ' + Fore.Green  + ' 201 ' + Style.ResetAll + ' ' + Fore.Magenta + 'POST   ' + Style.ResetAll + ' /api/people');
  WriteLn('  ' + Fore.Yellow + ' 404 ' + Style.ResetAll + ' ' + Fore.Cyan    + 'GET    ' + Style.ResetAll + ' /api/unknown');
  WriteLn('  ' + Fore.Red    + ' 500 ' + Style.ResetAll + ' ' + Fore.Cyan    + 'GET    ' + Style.ResetAll + ' /api/crash');
end;

begin
  try
    EnableANSIColorConsole;

    WriteLn(Style.Bright + 'DMVCFramework Console Colors' + Style.ResetAll);
    WriteLn(Fore.DarkGray + 'Colorama-style ANSI primitives for Delphi' + Style.ResetAll);

    ShowColorPalette;
    ShowStyles;
    ShowComposition;
    ShowPracticalExamples;

    WriteLn;
    WriteLn(Fore.DarkGray + 'Press ENTER to exit...' + Style.ResetAll);
    ReadLn;
  except
    on E: Exception do
    begin
      WriteLn(Fore.Red + 'Error: ' + E.Message + Style.ResetAll);
      ReadLn;
    end;
  end;
end.
