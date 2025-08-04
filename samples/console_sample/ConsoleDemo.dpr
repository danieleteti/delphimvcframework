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
begin
  WriteHeader('Tables DEMO');
  WriteLn;

  // Esempio tabella sviluppatori
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

  WriteSimpleTable(Headers, Data);

  WriteLn;
  WriteColoredTable(Headers, Data, Yellow, White);
end;

procedure ShowProgressExample;
begin
  WriteHeader('Progress Bar DEMO');
  WriteLn;

  WriteInfo('Starting download simulation...');
  WriteLn;

  // Progress bar statico
  WriteLineColored('Current Progress:', Cyan);
  ShowSimpleProgressBar('Download', 75, 100, 40);
  WriteLn;
  WriteLn;

  // Progress animation
  ShowProgressAnimation('Processing files', 25, 100);
  WriteLn;

  // Loading spinner
  ShowLoadingSpinner('Initializing system', 15);

  WriteSuccess('All operations completed successfully!');
end;

procedure ShowBoxExample;
var
  Content: TStringArray;
begin
  WriteHeader('Box and Layout DEMO');
  WriteLn;

  // Simple box
  SetLength(Content, 4);
  Content[0] := 'Server: ONLINE';
  Content[1] := 'Database: CONNECTED';
  Content[2] := 'Memory: 65%';
  Content[3] := 'CPU: 42%';

  DrawSimpleBox('System Status', Content, 40);
  WriteLn;

  // Warning box
  SetLength(Content, 2);
  Content[0] := 'Cache server not responding';
  Content[1] := 'Check network connection';

  DrawSimpleBox('WARNING', Content, 45);
end;

procedure ShowMenuExample;
var
  MenuItems: TStringArray;
  AdvancedItems: TMenuItemsArray;
  SelectedItem: Integer;
begin
  WriteHeader('Menu DEMO');
  WriteLn;

  SetLength(MenuItems, 5);
  MenuItems[0] := 'Start Server';
  MenuItems[1] := 'Stop Server';
  MenuItems[2] := 'View Logs';
  MenuItems[3] := 'Settings';
  MenuItems[4] := 'Exit';

  // Static menu display
  WriteLineColored('Static Menu Display:', White);
  ShowSimpleMenu('Main Menu', MenuItems, 2); // Item 2 selected
  WriteLn;

  // Interactive menu
  WriteLineColored('Interactive Menu (try it!):', White);
  SelectedItem := ShowInteractiveMenu('Main Menu', MenuItems, 0);

  if SelectedItem >= 0 then
    WriteSuccess('You selected: ' + MenuItems[SelectedItem])
  else
    WriteWarning('Menu cancelled');

  WriteLn;

  // Advanced menu with icons and disabled items
  WriteLineColored('Advanced Menu with Icons (try it!):', White);
  SetLength(AdvancedItems, 6);

  // Using the helper function
  AdvancedItems[0] := CreateMenuItem('New File', '+', True);
  AdvancedItems[1] := CreateMenuItem('Open File', 'O', True);
  AdvancedItems[2] := CreateMenuItem('Save File', 'S', False); // Disabled
  AdvancedItems[3] := CreateMenuItem('Print', 'P', False); // Disabled
  AdvancedItems[4] := CreateMenuItem('Settings', '*', True);
  AdvancedItems[5] := CreateMenuItem('Exit', 'X', True);

  SelectedItem := ShowAdvancedMenu('File Menu', AdvancedItems, 0, DarkGreen);

  if SelectedItem >= 0 then
    WriteSuccess('You selected: ' + AdvancedItems[SelectedItem].Text)
  else
    WriteWarning('Menu cancelled');
end;

procedure ShowListExample;
var
  Features: TStringArray;
  Tasks: TStringArray;
begin
  WriteHeader('Lists DEMO');
  WriteLn;

  // Feature list
  SetLength(Features, 4);
  Features[0] := 'Cross-platform console support';
  Features[1] := 'ASCII-based tables and boxes';
  Features[2] := 'Progress bars and animations';
  Features[3] := 'Colorized output';

  WriteFormattedList('DMVCFramework Console Features:', Features, lsBullet);
  WriteLn;

  // Task list
  SetLength(Tasks, 3);
  Tasks[0] := 'Initialize database connection';
  Tasks[1] := 'Load configuration files';
  Tasks[2] := 'Start web server';

  WriteFormattedList('Startup Tasks:', Tasks, lsNumbered);
end;

procedure ShowDashboardExample;
var
  ServerStatuses: TStringArray;
  ServerColors: TConsoleColorArray;
  MetricNames: TStringArray;
  MetricValues: TIntegerArray;
begin
  WriteHeader('Demo Dashboard');
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

procedure ShowUtilityExample;
begin
  WriteHeader('Demo Utility Functions');
  WriteLn;

  WriteInfo('System initialization started');
  WriteSuccess('Configuration loaded successfully');
  WriteWarning('Cache server response time is high');
  WriteError('Failed to connect to backup server');
  WriteLn;

  WriteSeparator(50);
  WriteLn;

  WriteAlignedText('Left aligned text', 50, taLeft);
  WriteAlignedText('Center aligned text', 50, taCenter);
  WriteAlignedText('Right aligned text', 50, taRight);
  WriteLn;

  WriteSeparator(50, '=');
end;

procedure ShowReportExample;
var
  Sections: TStringArray;
  SectionContents: TStringMatrix;
begin
  WriteHeader('Demo Report');

  SetLength(Sections, 3);
  SetLength(SectionContents, 3);

  Sections[0] := 'Performance Summary';
  SetLength(SectionContents[0], 3);
  SectionContents[0][0] := 'Average response time: 245ms';
  SectionContents[0][1] := 'Total requests processed: 15,432';
  SectionContents[0][2] := 'Error rate: 0.02%';

  Sections[1] := 'System Resources';
  SetLength(SectionContents[1], 4);
  SectionContents[1][0] := 'CPU utilization: 68%';
  SectionContents[1][1] := 'Memory usage: 4.2GB / 8GB';
  SectionContents[1][2] := 'Disk space: 125GB / 500GB';
  SectionContents[1][3] := 'Network throughput: 45 Mbps';

  Sections[2] := 'Recommendations';
  SetLength(SectionContents[2], 2);
  SectionContents[2][0] := 'Consider increasing cache size';
  SectionContents[2][1] := 'Schedule disk cleanup routine';

  WriteReport('Daily System Report', Sections, SectionContents);
end;

procedure ShowStatusLineExample;
var
  Items: TStringArray;
  Statuses: TStringArray;
  Colors: TConsoleColorArray;
begin
  WriteHeader('Demo Status Line');
  WriteLn;

  SetLength(Items, 3);
  SetLength(Statuses, 3);
  SetLength(Colors, 3);

  Items[0] := 'Web Server';
  Statuses[0] := 'Running';
  Colors[0] := Green;

  Items[1] := 'Database';
  Statuses[1] := 'Connected';
  Colors[1] := Green;

  Items[2] := 'Cache';
  Statuses[2] := 'Disconnected';
  Colors[2] := Red;

  WriteStatusLine(Items, Statuses, Colors);
end;

begin
  try
    EnableUTF8Console;
    ClrScr;

    WriteHeader('DMVCFramework Console Library Demo', 80);
    WriteLn;

    // Demo delle varie funzioni
    ShowTableExample;
    WriteLn;

    ShowProgressExample;
    WriteLn;

    ShowBoxExample;
    WriteLn;

    ShowMenuExample;
    WriteLn;

    ShowListExample;
    WriteLn;

    ShowDashboardExample;
    WriteLn;

    ShowUtilityExample;
    WriteLn;

    ShowStatusLineExample;
    WriteLn;

    ShowReportExample;

    WriteHeader('DEMO TERMINATED', 80);
    WriteLn;
    WriteLineColored('Press ENTER to EXIT...', Gray);
    ReadLn;

  except
    on E: Exception do
    begin
      WriteLn;
      WriteError('Errore durante l''esecuzione: ' + E.Message);
      WriteLn;
      WriteLineColored('Premi INVIO per uscire...', Gray);
      ReadLn;
    end;
  end;
end.
