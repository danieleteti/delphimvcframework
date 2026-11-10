program ExeWatchCaptureTest;

// A DMVC server whose logger has the ExeWatch appender (the wizard's wiring). test_exewatch_capture.py runs it against
// a fake ExeWatch server and checks what arrives. Needs the ExeWatch SDK (EXEWATCH_SDK), so it is not in the main suite.

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.DotEnv,
  MVCFramework.Logger,
  MVCFramework.Signal,
  MVCFramework.Server.Intf,
  MVCFramework.Server.Factory,
  LoggerPro,
  LoggerPro.Builder,
  LoggerPro.ExeWatchAppender,
  ExeWatchSDKv1,
  ControllersU in 'ControllersU.pas';

procedure Run;
var
  lConfig: TExeWatchConfig;
  lKey, lCustomer: string;
  lEngine: TMVCEngine;
  lServer: IMVCServer;
begin
  // dotEnv without a logger first: SetDefaultLogger is ignored once a default logger exists
  dotEnvConfigure(
    function: IMVCDotEnv
    begin
      Result := NewDotEnv.UseStrategy(TMVCDotEnvPriority.FileThenEnv).Build(AppPath);
    end);
  lKey := dotEnv.Env('exewatch.apikey', '');
  if lKey.StartsWith('ew_') then
  begin
    lCustomer := dotEnv.Env('exewatch.customerid', 'capture-test');
    lConfig := TExeWatchConfig.Create(lKey, lCustomer);
    if dotEnv.Env('exewatch.endpoint', '') <> '' then
      lConfig.Endpoint := dotEnv.Env('exewatch.endpoint', ''); // the fake server of the test
    InitializeExeWatch(lConfig);
    SetDefaultLogger(WithExeWatch(LoggerProBuilder).WithAPIKey(lKey).WithCustomerId(lCustomer).Done.Build);
  end;
  lEngine := TMVCEngine.Create(
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.DefaultContentType] := TMVCConstants.DEFAULT_CONTENT_TYPE;
    end);
  try
    lEngine.AddController(TTestController);
    lServer := TMVCServerFactory.CreateIndyDirect(lEngine);
    lServer.Listen(dotEnv.Env('server.port', 8080));
    try
      WriteLn('listening');
      WaitForTerminationSignal;
    finally
      lServer.Stop;
      lServer := nil;
    end;
  finally
    lEngine.Free;
  end;
  if ExeWatchIsInitialized then
    EW.WaitForSending(20); // the SDK ships every 7 seconds
end;

begin
  IsMultiThread := True;
  try
    Run;
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;
end.
