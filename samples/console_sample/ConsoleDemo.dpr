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

  WriteLine('Test 1: Determinate Progress', Cyan);
  WriteLn;

  // NEW: Unified Progress API with interface
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

  // NEW: Indeterminate progress (spinner)
  P := Progress('Loading data');
  for I := 1 to 50 do
  begin
    P.Update(I);  // Updates spinner animation
    Sleep(50);
  end;
  P.Complete;
  P := nil;
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
var
  ServerStatuses: TStringArray;
  ServerColors: TConsoleColorArray;
  MetricNames: TStringArray;
  MetricValues: TIntegerArray;
begin
  ClrScr;
  WriteHeader('Dashboard Demo');
  WriteLn;

  SetLength(ServerStatuses, 4);
  SetLength(ServerColors, 4);
  ServerStatuses[0] := 'Web Server: ONLINE';
  ServerColors[0] := Green;
  ServerStatuses[1] := 'Database: ONLINE';
  ServerColors[1] := Green;
  ServerStatuses[2] := 'Cache: WARNING';
  ServerColors[2] := Yellow;
  ServerStatuses[3] := 'Backup: ERROR';
  ServerColors[3] := Red;

  SetLength(MetricNames, 4);
  SetLength(MetricValues, 4);
  MetricNames[0] := 'CPU';
  MetricValues[0] := 75;
  MetricNames[1] := 'Memory';
  MetricValues[1] := 60;
  MetricNames[2] := 'Disk I/O';
  MetricValues[2] := 25;
  MetricNames[3] := 'Network';
  MetricValues[3] := 95;

  ShowSystemDashboard('DMVC Server Status', ServerStatuses, ServerColors, MetricNames, MetricValues);
end;

procedure ShowNewAPIFeatures;
begin
  ClrScr;
  WriteHeader('New Simplified API Features');
  WriteLn;

  WriteLine('The new API provides:', White);
  WriteLn;

  WriteLine('  1. Menu()     - One function instead of 3 (Simple/Interactive/Advanced)', Green);
  WriteLine('  2. Table()    - Auto-sizing columns, simpler parameters', Green);
  WriteLine('  3. Box()      - Auto-positioning, no manual width calculation', Green);
  WriteLine('  4. Progress() - Interface with auto-cleanup, determinate/indeterminate', Green);
  WriteLine('  5. Confirm()  - Quick yes/no prompts', Green);
  WriteLine('  6. Choose()   - Quick single choice without full menu', Green);

  WriteLn;
  WriteLine('All functions use ConsoleTheme for consistent styling!', Yellow);
  WriteLn;

  // Demo ConsoleTheme
  WriteLine('Demo: Changing ConsoleTheme affects all output', Cyan);
  WriteLn;

  ConsoleTheme.TextColor := Yellow;
  ConsoleTheme.DrawColor := Magenta;
  ConsoleTheme.BoxStyle := bsDouble;

  Box('Custom Theme', ['This box uses', 'the modified theme!']);

  // Reset
  ConsoleTheme.TextColor := Cyan;
  ConsoleTheme.DrawColor := White;
  ConsoleTheme.BoxStyle := bsRounded;
end;

begin
  try
    EnableUTF8Console;
    ClrScr;

    WriteHeader('DMVCFramework Console Library - NEW SIMPLIFIED API', 80);
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

    ShowNewAPIFeatures;
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
