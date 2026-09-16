unit MariaDBUtilsU;

{ Starts and stops the MariaDB instance bundled under TestClient\mariadb, the
  same way PGUtilsU does for PostgreSQL.

  Everything here is deliberately independent from any MariaDB installed on the
  machine running the tests: the executables come from the bundled bin folder,
  the data directory is created from scratch under it, the server is started
  with --defaults-file pointing at the my.ini that mariadb-install-db generates
  inside that data directory (so no system my.ini and no registry entry is
  read), the port is the test port, and the client library FireDAC loads is the
  bundled lib\libmariadb.dll. Nothing is installed as a service. }

interface

type
  TMariaDBUtil = class
  private
    fHome: string;
    fDataDir: string;
    fPort: UInt16;
    fInstallDBExecutable: string;
    fServerExecutable: string;
    fAdminExecutable: string;
    fClientExecutable: string;
    function DefaultsFile: string;
  public
    constructor Create(const AHome, ADataDir: string; const APort: UInt16);
    procedure InitDB;
    procedure StartDB;
    procedure CreateDatabase(const ADatabaseName: string);
    procedure StopDB;
    procedure RemoveDataDir;
    function IsRunning: Boolean;
    property Home: string read fHome;
    property DataDir: string read fDataDir;
    /// <summary>The client library FireDAC must load, so the tests never bind
    /// to a libmariadb.dll that happens to be on the PATH.</summary>
    function ClientLib: string;
  end;

implementation

uses
  Winapi.Windows, System.IOUtils, System.SysUtils;

{ Runs a command, hides its window, and waits at most ATimeoutMS for it.

  Output is deliberately NOT captured through a pipe. mariadb-install-db starts
  the server to bootstrap the system tables, that child inherits the write end
  of the pipe, and a reader waiting for end-of-file then waits forever: the
  first version of this unit hung the whole test run there. Exit codes plus the
  state on disk say everything the caller needs. }
function RunAndWait(const ACommandLine: string; const ATimeoutMS: Cardinal = 120000): Cardinal;
var
  SI: TStartupInfo;
  PI: TProcessInformation;
begin
  FillChar(SI, SizeOf(SI), 0);
  SI.cb := SizeOf(SI);
  SI.dwFlags := STARTF_USESHOWWINDOW;
  SI.wShowWindow := SW_HIDE;
  { The executable is launched directly, not through cmd.exe /C: the paths here
    are quoted, and cmd has its own rules about stripping the outer quotes of a
    /C string, which turned the command into something that never returned. }
  if not CreateProcess(nil, PChar(ACommandLine), nil, nil, False,
    CREATE_NO_WINDOW, nil, nil, SI, PI) then
    Exit(GetLastError);
  try
    if WaitForSingleObject(PI.hProcess, ATimeoutMS) = WAIT_TIMEOUT then
    begin
      TerminateProcess(PI.hProcess, 1);
      Exit(WAIT_TIMEOUT);
    end;
    GetExitCodeProcess(PI.hProcess, Result);
  finally
    CloseHandle(PI.hThread);
    CloseHandle(PI.hProcess);
  end;
end;

function SysStartExecute(const ACommandLine: string): Boolean;
var
  SI: TStartupInfo;
  PI: TProcessInformation;
begin
  FillChar(SI, SizeOf(SI), 0);
  SI.cb := SizeOf(SI);
  SI.dwFlags := STARTF_USESHOWWINDOW;
  SI.wShowWindow := SW_HIDE;
  { DETACHED_PROCESS, not CREATE_NEW_CONSOLE: the server must outlive the
    console of whoever started it, and it writes to its own .err file in the
    data directory anyway. }
  Result := CreateProcess(nil, PChar(ACommandLine), nil, nil, False,
    DETACHED_PROCESS, nil, nil, SI, PI);
  if Result then
  begin
    CloseHandle(PI.hThread);
    CloseHandle(PI.hProcess);
  end;
end;

{ TMariaDBUtil }

constructor TMariaDBUtil.Create(const AHome, ADataDir: string; const APort: UInt16);
begin
  inherited Create;
  fHome := AHome;
  fDataDir := ADataDir;
  if fDataDir.Contains(' ') then
  begin
    raise Exception.Create('Cannot RUN test in a path with spaces');
  end;
  fPort := APort;
  fInstallDBExecutable := TPath.Combine(fHome, 'bin\mariadb-install-db.exe');
  fServerExecutable := TPath.Combine(fHome, 'bin\mariadbd.exe');
  fAdminExecutable := TPath.Combine(fHome, 'bin\mariadb-admin.exe');
  fClientExecutable := TPath.Combine(fHome, 'bin\mariadb.exe');
end;

function TMariaDBUtil.ClientLib: string;
begin
  { The server runs as its own process, so its bitness is irrelevant here, but
    the client library is loaded INTO the test executable and must match it.
    Mixing them gives FireDAC error -314, "Library has unsupported
    architecture". Hence one folder per architecture. }
{$IFDEF WIN64}
  Result := TPath.Combine(fHome, 'lib\x64\libmariadb.dll');
{$ELSE}
  Result := TPath.Combine(fHome, 'lib\x86\libmariadb.dll');
{$ENDIF}
  if not TFile.Exists(Result) then
  begin
    raise Exception.Create('The MariaDB client library for this platform is missing: ' +
      Result + '. Drop the libmariadb.dll of the matching bitness there ' +
      '(MariaDB Connector/C, https://mariadb.com/downloads/connectors/).');
  end;
end;

function TMariaDBUtil.DefaultsFile: string;
begin
  { Written by mariadb-install-db inside the data directory. Passing it
    explicitly is what keeps a system-wide my.ini out of the picture. }
  Result := TPath.Combine(fDataDir, 'my.ini');
end;

procedure TMariaDBUtil.InitDB;
begin
  if RunAndWait(Format('"%s" --datadir=%s --port=%d',
    [fInstallDBExecutable, fDataDir, fPort])) <> 0 then
  begin
    raise Exception.Create('Cannot init the MariaDB data directory at ' + fDataDir);
  end;
  if not TFile.Exists(DefaultsFile) then
  begin
    raise Exception.Create('mariadb-install-db did not write ' + DefaultsFile);
  end;
end;

procedure TMariaDBUtil.StartDB;
begin
  { --bind-address=127.0.0.1 is not a detail: a server listening on every
    interface makes Windows Defender Firewall pop its "allow access?" dialog the
    first time this executable runs on a machine, which on an unattended run is
    a prompt nobody answers. The tests only ever connect over loopback, so
    listening there is both sufficient and silent. }
  if not SysStartExecute(Format('"%s" --defaults-file=%s --datadir=%s --port=%d --bind-address=127.0.0.1',
    [fServerExecutable, DefaultsFile, fDataDir, fPort])) then
  begin
    raise Exception.Create('Cannot start MariaDB');
  end;
end;

procedure TMariaDBUtil.CreateDatabase(const ADatabaseName: string);
var
  I: Integer;
begin
  { StartDB returns as soon as the process exists; the server accepts
    connections a moment later. }
  for I := 1 to 60 do
  begin
    if IsRunning then
      Break;
    Sleep(500);
  end;
  if RunAndWait(Format('"%s" --host=127.0.0.1 --port=%d --user=root ' +
    '--execute="create database if not exists %s"',
    [fClientExecutable, fPort, ADatabaseName]), 30000) <> 0 then
  begin
    raise Exception.Create('Cannot create the database ' + ADatabaseName);
  end;
end;

function TMariaDBUtil.IsRunning: Boolean;
begin
  Result := RunAndWait(Format('"%s" --host=127.0.0.1 --port=%d --user=root --execute="select 1"',
    [fClientExecutable, fPort]), 10000) = 0;
end;

procedure TMariaDBUtil.StopDB;
begin
  RunAndWait(Format('"%s" --host=127.0.0.1 --port=%d --user=root shutdown',
    [fAdminExecutable, fPort]), 30000);
end;

procedure TMariaDBUtil.RemoveDataDir;
begin
  StopDB;
  if TDirectory.Exists(fDataDir) then
  begin
    TDirectory.Delete(fDataDir, True);
  end;
end;

end.
