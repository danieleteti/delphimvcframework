program PerfBenchServer_IndyDirect_MSHeap;

{$APPTYPE CONSOLE}

uses
  // MSHeap MUST be the first unit in the uses clause so the memory-manager
  // replacement installs before any other unit's initialization allocates.
  {$IF Defined(MSWINDOWS)}MSHeap,{$ENDIF}
  System.SysUtils,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Signal,
  MVCFramework.Server.Intf,
  MVCFramework.Server.Indy,
  PerfBenchControllerU in 'PerfBenchControllerU.pas',
  PerfBenchEngineConfigU in 'PerfBenchEngineConfigU.pas';

{$R *.res}

procedure RunServer(APort: Integer);
var
  LEngine: TMVCEngine;
  LServer: IMVCServer;
begin
  Writeln('** DMVCFramework Perf Bench ** build ' + DMVCFRAMEWORK_VERSION);
  Writeln('Backend: Indy Direct (TMVCIndyServer) + MSHeap');
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
    LServer := TMVCIndyServer.Create(LEngine);
    LServer.Listen(APort);
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
