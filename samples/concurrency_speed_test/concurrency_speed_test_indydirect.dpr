program concurrency_speed_test_indydirect;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MVCFramework,
  MVCFramework.Logger,
  MVCFramework.DotEnv,
  MVCFramework.Commons,
  MVCFramework.Signal,
  MVCFramework.Server.Intf,
  MVCFramework.Server.Indy,
  MVCFramework.Middleware.Compression,
  MainControllerU in 'MainControllerU.pas';

{$R *.res}

procedure RunServer(APort: Integer);
var
  LEngine: TMVCEngine;
  LServer: IMVCServer;
  LConfigAction: TProc<TMVCConfig>;
begin
  Writeln('** DMVCFramework Speed Test ** build ' + DMVCFRAMEWORK_VERSION);
  Writeln('Server type: Indy Direct (TMVCIndyServer)');
  Writeln('Listening on port: ', APort);

  LConfigAction :=
    procedure(Config: TMVCConfig)
    begin
      Config.dotEnv := dotEnv;
      Config[TMVCConfigKey.DefaultContentType] := Config.dotEnv.Env('dmvc.default.content_type', TMVCConstants.DEFAULT_CONTENT_TYPE);
      Config[TMVCConfigKey.DefaultContentCharset] := Config.dotEnv.Env('dmvc.default.content_charset', TMVCConstants.DEFAULT_CONTENT_CHARSET);
      Config[TMVCConfigKey.ExposeServerSignature] := 'false';
    end;
  LEngine := TMVCEngine.Create(LConfigAction);
  try
    LEngine.AddController(TMyController);
    LEngine.AddMiddleware(TMVCCompressionMiddleware.Create);
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
    dotEnvConfigure(
      function: IMVCDotEnv
      begin
        Result := NewDotEnv
          .UseStrategy(TMVCDotEnvPriority.FileThenEnv)
          .UseProfile('test')
          .UseProfile('prod')
          .Build();
      end);
    RunServer(dotEnv.Env('dmvc.server.port', 9999));
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
