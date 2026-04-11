program HttpSysServer;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Signal,
  MVCFramework.Logger,
  MVCFramework.Server.Intf,
  MVCFramework.Server.HttpSys,
  MVCFramework.HttpSysApi,
  MVCFramework.HttpSys.Request,
  MVCFramework.HttpSys.Response,
  ControllersU in '..\commons\ControllersU.pas',
  EngineConfigU in '..\commons\EngineConfigU.pas';

{$R *.res}

var
  LEngine: TMVCEngine;
  LServer: IMVCServer;
begin
  ReportMemoryLeaksOnShutdown := True;
  try
    LEngine := TMVCEngine.Create(TProc<TMVCConfig>(nil));
    try
      ConfigureEngine(LEngine);
      LServer := TMVCHttpSysServer.Create(LEngine);
      LServer.Listen(8080, 'localhost');
      LogI('HTTP.sys Server started on port 8080');
      LogI('Server type: Windows HTTP.sys (kernel-mode IOCP)');
      LogI('URL: http://localhost:8080/');
      LogI('Press CTRL+C to stop');
      LogI('');
      LogI('NOTE: If you get error 5 (Access Denied), run as Administrator or execute:');
      LogI('  netsh http add urlacl url=http://localhost:8080/ user=Everyone');
      WaitForTerminationSignal;
      EnterInShutdownState;
      LServer.Stop;
      LServer := nil;
    finally
      LEngine.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
