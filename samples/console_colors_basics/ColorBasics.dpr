program ColorBasics;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MVCFramework.Console;

begin
  try
    EnableANSIColorConsole;

    WriteLn(Style.Bright + 'Color Basics' + Style.ResetAll);
    WriteLn(Fore.DarkGray + 'Essential foreground colors and simple composition' + Style.ResetAll);
    WriteLn;

    // === Most useful foreground colors ===
    WriteLn('--- Foreground Colors ---');
    WriteLn(Fore.Red      + '  This is Fore.Red'      + Style.ResetAll);
    WriteLn(Fore.Green    + '  This is Fore.Green'    + Style.ResetAll);
    WriteLn(Fore.Yellow   + '  This is Fore.Yellow'   + Style.ResetAll);
    WriteLn(Fore.Blue     + '  This is Fore.Blue'     + Style.ResetAll);
    WriteLn(Fore.Magenta  + '  This is Fore.Magenta'  + Style.ResetAll);
    WriteLn(Fore.Cyan     + '  This is Fore.Cyan'     + Style.ResetAll);
    WriteLn(Fore.White    + '  This is Fore.White'    + Style.ResetAll);
    WriteLn(Fore.DarkGray + '  This is Fore.DarkGray' + Style.ResetAll);
    WriteLn;

    // === Simple Fore + Back composition ===
    WriteLn('--- Fore + Back ---');
    WriteLn(Fore.White + Back.DarkBlue + ' White on DarkBlue ' + Style.ResetAll);
    WriteLn(Fore.Black + Back.Green    + ' Black on Green '    + Style.ResetAll);
    WriteLn(Fore.White + Back.DarkRed  + ' White on DarkRed '  + Style.ResetAll);
    WriteLn(Fore.Black + Back.Yellow   + ' Black on Yellow '   + Style.ResetAll);
    WriteLn;

    // === That is it - just strings ===
    WriteLn(Fore.DarkGray + 'Colors are plain strings. Concatenate and go.' + Style.ResetAll);

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
