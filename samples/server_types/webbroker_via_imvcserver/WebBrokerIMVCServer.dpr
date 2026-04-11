program WebBrokerIMVCServer;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Signal,
  MVCFramework.Logger,
  MVCFramework.Server.Intf,
  MVCFramework.Server.WebBroker,
  ControllersU in '..\commons\ControllersU.pas',
  EngineConfigU in '..\commons\EngineConfigU.pas';

{$R *.res}

var
  LServer: IMVCServer;
begin
  ReportMemoryLeaksOnShutdown := True;
  try
    LServer := TMVCWebBrokerServer.Create(
      procedure(AEngine: TMVCEngine)
      begin
        ConfigureEngine(AEngine);
      end);
    LServer.Listen(8080);
    LogI('WebBroker Server (via IMVCServer) started on port 8080');
    LogI('Server type: WebBroker (TIdHTTPWebBrokerBridge behind IMVCServer)');
    LogI('Press CTRL+C to stop');
    WaitForTerminationSignal;
    EnterInShutdownState;
    LServer.Stop;
    LServer := nil;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
