program ColorBadges;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MVCFramework.Console;

begin
  try
    EnableANSIColorConsole;

    WriteLn(Style.Bright + 'Practical Badges' + Style.ResetAll);
    WriteLn(Fore.DarkGray + 'Real-world badge patterns for test results, HTTP status, and logging' + Style.ResetAll);
    WriteLn;

    // === Test Result Badges ===
    WriteLn('--- Test Results ---');
    WriteLn('  ' + Back.Green  + Fore.Black + ' PASS ' + Style.ResetAll + '  TestUserAuthentication');
    WriteLn('  ' + Back.Green  + Fore.Black + ' PASS ' + Style.ResetAll + '  TestOrderCreation');
    WriteLn('  ' + Back.Red    + Fore.White + ' FAIL ' + Style.ResetAll + '  TestPaymentTimeout');
    WriteLn('  ' + Back.Yellow + Fore.Black + ' SKIP ' + Style.ResetAll + '  TestExternalAPIIntegration');
    WriteLn('  ' + Back.Green  + Fore.Black + ' PASS ' + Style.ResetAll + '  TestInventoryUpdate');
    WriteLn;

    // === HTTP Status Badges ===
    WriteLn('--- HTTP Status Codes ---');
    WriteLn('  ' + Back.Green    + Fore.Black + ' 200 ' + Style.ResetAll + '  OK');
    WriteLn('  ' + Back.DarkGray + Fore.White + ' 301 ' + Style.ResetAll + '  Moved Permanently');
    WriteLn('  ' + Back.Yellow   + Fore.Black + ' 404 ' + Style.ResetAll + '  Not Found');
    WriteLn('  ' + Back.Red      + Fore.White + ' 500 ' + Style.ResetAll + '  Internal Server Error');
    WriteLn;

    // === Log Level Badges (Gin-style colors) ===
    WriteLn('--- Log Levels ---');
    WriteLn('  ' + Back.DarkCyan + Fore.White + ' DEBUG ' + Style.ResetAll + '  Detailed diagnostic information');
    WriteLn('  ' + Back.Green    + Fore.Black + ' INFO  ' + Style.ResetAll + '  Normal operational messages');
    WriteLn('  ' + Back.Yellow   + Fore.Black + ' WARN  ' + Style.ResetAll + '  Something unexpected happened');
    WriteLn('  ' + Back.Red      + Fore.White + ' ERROR ' + Style.ResetAll + '  A failure that needs attention');
    WriteLn('  ' + Back.DarkRed  + Fore.White + Style.Bright + ' FATAL ' + Style.ResetAll + '  Application cannot continue');
    WriteLn;

    // === Mini Gin-style HTTP log ===
    WriteLn('--- Gin-Style Request Log ---');
    WriteLn(
      Fore.DarkGray + '2026-04-09 14:32:01' + Style.ResetAll + ' ' +
      Back.Green + Fore.Black + ' 200 ' + Style.ResetAll + ' ' +
      Fore.DarkGray + '  1.2ms' + Style.ResetAll + ' ' +
      Fore.DarkGray + '192.168.1.10' + Style.ResetAll + ' ' +
      Fore.Cyan + 'GET    ' + Style.ResetAll +
      Fore.White + '"/api/people"' + Style.ResetAll
    );
    WriteLn(
      Fore.DarkGray + '2026-04-09 14:32:01' + Style.ResetAll + ' ' +
      Back.Green + Fore.Black + ' 201 ' + Style.ResetAll + ' ' +
      Fore.DarkGray + '  3.8ms' + Style.ResetAll + ' ' +
      Fore.DarkGray + '192.168.1.10' + Style.ResetAll + ' ' +
      Fore.Magenta + 'POST   ' + Style.ResetAll +
      Fore.White + '"/api/people"' + Style.ResetAll
    );
    WriteLn(
      Fore.DarkGray + '2026-04-09 14:32:02' + Style.ResetAll + ' ' +
      Back.Yellow + Fore.Black + ' 404 ' + Style.ResetAll + ' ' +
      Fore.DarkGray + '  0.4ms' + Style.ResetAll + ' ' +
      Fore.DarkGray + '192.168.1.22' + Style.ResetAll + ' ' +
      Fore.Cyan + 'GET    ' + Style.ResetAll +
      Fore.White + '"/api/unknown"' + Style.ResetAll
    );
    WriteLn(
      Fore.DarkGray + '2026-04-09 14:32:03' + Style.ResetAll + ' ' +
      Back.Red + Fore.White + ' 500 ' + Style.ResetAll + ' ' +
      Fore.DarkGray + ' 12.1ms' + Style.ResetAll + ' ' +
      Fore.DarkGray + '192.168.1.15' + Style.ResetAll + ' ' +
      Fore.Cyan + 'GET    ' + Style.ResetAll +
      Fore.White + '"/api/crash"' + Style.ResetAll
    );

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
