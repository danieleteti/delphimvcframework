program ColorComposition;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MVCFramework.Console;

begin
  try
    EnableANSIColorConsole;

    WriteLn(Style.Bright + 'Color Composition' + Style.ResetAll);
    WriteLn(Fore.DarkGray + 'Combining Fore, Back, and Style for rich output' + Style.ResetAll);
    WriteLn;

    // === Fore + Back combinations ===
    WriteLn('--- Fore + Back Combinations ---');
    WriteLn(Fore.White + Back.DarkBlue    + ' Navigation Bar '  + Style.ResetAll);
    WriteLn(Fore.Black + Back.Yellow      + ' Warning Banner '  + Style.ResetAll);
    WriteLn(Fore.White + Back.DarkMagenta + ' Feature Flag '    + Style.ResetAll);
    WriteLn(Fore.Black + Back.Cyan        + ' Info Highlight '  + Style.ResetAll);
    WriteLn;

    // === Style.Bright and Style.Dim ===
    WriteLn('--- Bright and Dim ---');
    WriteLn(Style.Bright + Fore.Green  + 'Style.Bright + Fore.Green'  + Style.ResetAll);
    WriteLn(Style.Bright + Fore.Red    + 'Style.Bright + Fore.Red'    + Style.ResetAll);
    WriteLn(Style.Dim    + Fore.Cyan   + 'Style.Dim + Fore.Cyan'      + Style.ResetAll);
    WriteLn(Style.Dim    + Fore.Yellow + 'Style.Dim + Fore.Yellow'    + Style.ResetAll);
    WriteLn(Style.Normal + 'Style.Normal (default weight)' + Style.ResetAll);
    WriteLn;

    // === Inline coloring within a sentence ===
    WriteLn('--- Inline Coloring ---');
    WriteLn(
      'Server ' + Fore.Green + 'RUNNING' + Style.ResetAll +
      ' on port ' + Fore.Cyan + '8080' + Style.ResetAll +
      ' with ' + Fore.Yellow + '3' + Style.ResetAll + ' workers'
    );
    WriteLn(
      'Tests: ' +
      Fore.Green + '42 passed' + Style.ResetAll + ', ' +
      Fore.Red + '2 failed' + Style.ResetAll + ', ' +
      Fore.DarkGray + '1 skipped' + Style.ResetAll
    );
    WriteLn(
      'CPU: ' + Style.Bright + Fore.Cyan + '23%' + Style.ResetAll +
      '  Mem: ' + Style.Bright + Fore.Yellow + '512 MB' + Style.ResetAll +
      '  Disk: ' + Style.Bright + Fore.Green + '18 GB free' + Style.ResetAll
    );
    WriteLn;

    // === Progress-bar style output using Back colors ===
    WriteLn('--- Progress Bar with Back Colors ---');
    Write('  Download: ');
    Write(Back.Green + '                              ' + Style.ResetAll);
    Write(Back.DarkGray + '            ' + Style.ResetAll);
    WriteLn(' 71%');
    Write('  Upload:   ');
    Write(Back.Cyan + '                    ' + Style.ResetAll);
    Write(Back.DarkGray + '                      ' + Style.ResetAll);
    WriteLn(' 48%');
    Write('  Build:    ');
    Write(Back.Green + '                                          ' + Style.ResetAll);
    WriteLn(' 100%');

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
