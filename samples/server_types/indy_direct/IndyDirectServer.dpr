program IndyDirectServer;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Signal,
  MVCFramework.Logger,
  MVCFramework.Server.Intf,
  MVCFramework.Server.Indy,
  MVCFramework.Indy.Request,
  MVCFramework.Indy.Response,
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
      LServer := TMVCIndyServer.Create(LEngine);
      LServer.Listen(8080);
      LogI('Indy Direct Server started on port 8080');
      LogI('Server type: Indy Direct (no WebBroker)');
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
