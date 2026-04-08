program ConsoleDemo;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Math,
  MVCFramework.Console;

procedure ShowTableExample;
var
  Headers: TStringArray;
  Data: TStringMatrix;
  SelectedRow: Integer;
begin
  ClrScr;
  WriteHeader('Tables Demo');
  WriteLn;

  // Example developer table
  SetLength(Headers, 4);
  Headers[0] := 'ID';
  Headers[1] := 'Name';
  Headers[2] := 'Framework';
  Headers[3] := 'Language';

  SetLength(Data, 5);

  SetLength(Data[0], 4);
  Data[0][0] := '001'; Data[0][1] := 'Mario Rossi'; Data[0][2] := 'DMVCFramework'; Data[0][3] := 'Delphi';

  SetLength(Data[1], 4);
  Data[1][0] := '002'; Data[1][1] := 'Luigi Verdi'; Data[1][2] := 'Spring Boot'; Data[1][3] := 'Java';

  SetLength(Data[2], 4);
  Data[2][0] := '003'; Data[2][1] := 'Anna Bianchi'; Data[2][2] := 'ASP.NET Core'; Data[2][3] := 'C#';

  SetLength(Data[3], 4);
  Data[3][0] := '004'; Data[3][1] := 'Paolo Neri'; Data[3][2] := 'Laravel'; Data[3][3] := 'PHP';

  SetLength(Data[4], 4);
  Data[4][0] := '005'; Data[4][1] := 'Sara Gialli'; Data[4][2] := 'FastAPI'; Data[4][3] := 'Python';

  WriteLine('Test 1: Static Table Display', Cyan);
  WriteLn;

  // Simple table without title
  WriteLine('  a) Simple table (no title):', White);
  WriteLn;
  Table(Headers, Data);
  WriteLn;

  // With title
  WriteLine('  b) Table with title:', White);
  WriteLn;
  Table(Headers, Data, 'Development Team');
  WriteLn;

  WriteLine('Press ENTER to continue to interactive table...', Gray);
  ReadLn;
  ClrScr;

  WriteLine('Test 2: Interactive Table (Menu-style selection)', Cyan);
  WriteLn;
  WriteLine('Use arrow keys to navigate, ENTER to select, ESC to cancel', Gray);
  WriteLn;

  // NEW: Interactive table with row selection!
  SelectedRow := TableMenu('Select a Developer', Headers, Data);

  if SelectedRow >= 0 then
  begin
    WriteSuccess(Format('You selected: %s - %s (%s)',
      [Data[SelectedRow][1], Data[SelectedRow][2], Data[SelectedRow][3]]));
  end
  else
    WriteWarning('Selection cancelled');
end;

procedure ShowProgressExample;
var
  P: IProgress;
  I: Integer;
begin
  ClrScr;
  WriteHeader('Progress Bar Demo');
  WriteLn;

  HideCursor;
  try
    WriteLine('Test 1: Determinate Progress', Cyan);
    WriteLn;

    P := Progress('Downloading files', 100);
    for I := 1 to 100 do
    begin
      P.Update(I);
      Sleep(30);
    end;
    P := nil;  // Auto cleanup
    WriteLn;
    WriteLn;

    WriteLine('Test 2: Indeterminate Spinner', Cyan);
    WriteLn;

    P := Progress('Loading data');
    for I := 1 to 50 do
    begin
      P.Update(I);
      Sleep(50);
    end;
    P.Complete;
    P := nil;
  finally
    ShowCursor;
  end;
end;

procedure ShowBoxExample;
var
  Content: TStringArray;
begin
  ClrScr;
  WriteHeader('Box and Layout Demo');
  WriteLn;

  // NEW: Unified Box API - auto positioning, auto width
  SetLength(Content, 4);
  Content[0] := 'Server: ONLINE';
  Content[1] := 'Database: CONNECTED';
  Content[2] := 'Memory: 65%';
  Content[3] := 'CPU: 42%';

  Box('System Status', Content);
  WriteLn;

  // Custom width
  SetLength(Content, 2);
  Content[0] := 'Cache server not responding';
  Content[1] := 'Check network connection';

  Box('WARNING', Content, 50);
end;

procedure ShowMenuExample;
var
  MainMenuItems: TStringArray;
  FileMenuItems: TStringArray;
  SelectedItem: Integer;
begin
  ClrScr;
  WriteHeader('Menu Demo');
  WriteLn;

  // NEW: Unified Menu API - much simpler!
  SetLength(MainMenuItems, 5);
  MainMenuItems[0] := 'Start Server';
  MainMenuItems[1] := 'Stop Server';
  MainMenuItems[2] := 'View Logs';
  MainMenuItems[3] := 'Settings';
  MainMenuItems[4] := 'Exit';

  WriteLine('Interactive Menu (try it!):', White);
  WriteLn;

  SelectedItem := Menu('Main Menu', MainMenuItems);

  if SelectedItem >= 0 then
    WriteSuccess('You selected: ' + MainMenuItems[SelectedItem])
  else
    WriteWarning('Menu cancelled');

  WriteLn;
  WriteLn;

  // Advanced menu with default selection
  SetLength(FileMenuItems, 4);
  FileMenuItems[0] := 'New File';
  FileMenuItems[1] := 'Open File';
  FileMenuItems[2] := 'Save File';
  FileMenuItems[3] := 'Exit';

  SelectedItem := Menu('File Menu', FileMenuItems, 2);  // Default to "Save File"

  if SelectedItem >= 0 then
    WriteSuccess('You selected: ' + FileMenuItems[SelectedItem])
  else
    WriteWarning('Menu cancelled');
end;

procedure ShowConfirmExample;
begin
  ClrScr;
  WriteHeader('Confirm and Choose Demo');
  WriteLn;

  // NEW: Confirm API
  WriteLine('Test 1: Confirm with default Yes', Cyan);
  if Confirm('Do you want to continue?') then
    WriteSuccess('User confirmed!')
  else
    WriteWarning('User declined');

  WriteLn;

  // NEW: Confirm with default No
  WriteLine('Test 2: Confirm with default No', Cyan);
  if Confirm('Delete all files?', False) then
    WriteSuccess('User confirmed deletion')
  else
    WriteInfo('Deletion cancelled');

  WriteLn;

  // NEW: Choose API (quick choice without full menu)
  WriteLine('Test 3: Quick choice', Cyan);
  var Options: TStringArray;
  SetLength(Options, 3);
  Options[0] := 'Fast Mode';
  Options[1] := 'Normal Mode';
  Options[2] := 'Safe Mode';

  var Choice := Choose('Select processing mode:', Options);
  if Choice >= 0 then
    WriteSuccess('You chose: ' + Options[Choice])
  else
    WriteWarning('No choice made');
end;

procedure ShowDashboardExample;
begin
  ClrScr;
  WriteHeader('Dashboard Demo');
  WriteLn;

  Box('DMVC Server Status', [
    'Web Server: ONLINE',
    'Database:   ONLINE',
    'Cache:      WARNING',
    'Backup:     ERROR'
  ], 50);

  WriteLn;
  WriteLine('Server Status:', White);
  WriteLine('  Web Server: ONLINE', Green);
  WriteLine('  Database:   ONLINE', Green);
  WriteLine('  Cache:      WARNING', Yellow);
  WriteLine('  Backup:     ERROR', Red);
end;

procedure ShowThemeExample;
begin
  ClrScr;
  WriteHeader('Theme Demo');
  WriteLn;

  WriteLine('ConsoleTheme controls global styling for all widgets.', White);
  WriteLn;

  WriteLine('Default theme:', Cyan);
  Box('Default', ['This uses the default theme']);
  WriteLn;

  // Change theme
  ConsoleTheme.TextColor := Yellow;
  ConsoleTheme.DrawColor := Magenta;
  ConsoleTheme.BoxStyle := bsDouble;

  WriteLine('Custom theme (Yellow/Magenta/Double):', Cyan);
  Box('Custom', ['This uses the modified theme!']);

  // Reset
  ConsoleTheme.TextColor := Cyan;
  ConsoleTheme.DrawColor := White;
  ConsoleTheme.BoxStyle := bsRounded;
end;

begin
  try
    EnableUTF8Console;
    ClrScr;

    WriteHeader('DMVCFramework Console Library', 80);
    WriteLn;

    // Demo all new features
    ShowTableExample;
    WriteLn;
    WriteLine('Press ENTER to continue...', Gray);
    ReadLn;

    ShowProgressExample;
    WriteLn;
    WriteLine('Press ENTER to continue...', Gray);
    ReadLn;

    ShowBoxExample;
    WriteLn;
    WriteLine('Press ENTER to continue...', Gray);
    ReadLn;

    ShowMenuExample;
    WriteLn;
    WriteLine('Press ENTER to continue...', Gray);
    ReadLn;

    ShowConfirmExample;
    WriteLn;
    WriteLine('Press ENTER to continue...', Gray);
    ReadLn;

    ShowDashboardExample;
    WriteLn;
    WriteLine('Press ENTER to continue...', Gray);
    ReadLn;

    ShowThemeExample;
    WriteLn;

    ClrScr;
    WriteHeader('DEMO COMPLETED', 80);
    WriteLn;
    WriteLine('Press ENTER to exit...', Gray);
    ReadLn;

  except
    on E: Exception do
    begin
      WriteLn;
      WriteError('Error during execution: ' + E.Message);
      WriteLn;
      WriteLine('Press ENTER to exit...', Gray);
      ReadLn;
    end;
  end;
end.
