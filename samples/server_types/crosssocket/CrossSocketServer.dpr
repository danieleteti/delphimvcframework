program CrossSocketServer;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Signal,
  MVCFramework.Logger,
  MVCFramework.Server.Intf,
  MVCFramework.Server.CrossSocket,
  ControllersU in '..\commons\ControllersU.pas',
  EngineConfigU in '..\commons\EngineConfigU.pas';

{$R *.res}

var
  LEngine: TMVCEngine;
  LServer: IMVCServer;
  LConfigAction: TProc<TMVCConfig>;
begin
  ReportMemoryLeaksOnShutdown := True;
  try
    LConfigAction := nil;
    LEngine := TMVCEngine.Create(LConfigAction);
    try
      ConfigureEngine(LEngine);
      LServer := TMVCCrossSocketServer.Create(LEngine);
      LServer.Listen(8080);
      LogI('CrossSocket Server started on port 8080');
      LogI('Server type: Delphi-Cross-Socket (IOCP/epoll/kqueue)');
      LogI('Press CTRL+C to stop');
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
