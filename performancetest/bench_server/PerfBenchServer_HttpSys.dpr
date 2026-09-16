program PerfBenchServer_HttpSys;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Signal,
  MVCFramework.Server.Intf,
  MVCFramework.Server.HttpSys,
  PerfBenchControllerU in 'PerfBenchControllerU.pas',
  PerfBenchEngineConfigU in 'PerfBenchEngineConfigU.pas';

{$R *.res}

procedure RunServer(APort: Integer);
var
  LEngine: TMVCEngine;
  LServer: IMVCServer;
begin
  Writeln('** DMVCFramework Perf Bench ** build ' + DMVCFRAMEWORK_VERSION);
  Writeln('Backend: HTTP.sys (kernel-mode IOCP)');
  Writeln('Listening on port: ', APort);

  LEngine := TMVCEngine.Create(
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.DefaultContentType] := TMVCConstants.DEFAULT_CONTENT_TYPE;
      Config[TMVCConfigKey.DefaultContentCharset] := TMVCConstants.DEFAULT_CONTENT_CHARSET;
      Config[TMVCConfigKey.ExposeServerSignature] := 'false';
    end);
  try
    ConfigureBenchEngine(LEngine);
    LServer := TMVCHttpSysServer.Create(LEngine);
    LServer.Listen(APort, 'localhost');
    WriteLn('CTRL+C to shutdown the server');
    WaitForTerminationSignal;
    EnterInShutdownState;
    LServer.Stop;
    LServer := nil;
  finally
    LEngine.Free;
  end;
end;

begin
  IsMultiThread := True;
  try
    RunServer(9999);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
