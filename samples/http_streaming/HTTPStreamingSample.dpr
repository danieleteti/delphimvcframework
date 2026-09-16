program HTTPStreamingSample;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Console,
  MVCFramework.Signal,
  MVCFramework.Logger,
  MVCFramework.Server.Intf,
  MVCFramework.Server.Factory,
  MVCFramework.Middleware.StaticFiles,
  StreamingControllerU in 'StreamingControllerU.pas';

{$R *.res}

const
  PORT = 8080;

procedure RunServer;
var
  LEngine: TMVCEngine;
  LServer: IMVCServer;
begin
  WriteLn('** DMVCFramework HTTP Streaming Sample (SSE / JSONL / CSV) **');
  WriteLn;

  LEngine := TMVCEngine.Create(
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.DefaultContentType] := TMVCMediaType.APPLICATION_JSON;
    end);
  try
    LEngine.AddController(TStreamingController);
    LEngine.AddMiddleware(TMVCStaticFilesMiddleware.Create('/static', 'www'));

    LServer := TMVCServerFactory.CreateIndyDirect(LEngine);
    LServer.Listen(PORT);
    try
      WriteLn('Server started on port ', PORT, ' (Indy Direct)');
      WriteLn;
      WriteLn('Open your browser at:');
      WriteLn;
      WriteLn('  ==> http://localhost:', PORT, '/static/index.html');
      WriteLn;
      WriteLn('The web page has 4 interactive demos:');
      WriteLn('  1. AI Chat Stream  (SSE)   - text streamed word by word');
      WriteLn('  2. Progress Stream (SSE)   - real-time progress bar');
      WriteLn('  3. People Stream   (JSONL) - table filled row by row');
      WriteLn('  4. People CSV      (CSV)   - rows streamed as text/csv');
      WriteLn;
      WriteLn('Press Ctrl+C to stop.');
      WaitForTerminationSignal;
      WriteLn('Shutting down...');
    finally
      LServer.Stop;
      LServer := nil;
    end;
  finally
    LEngine.Free;
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  IsMultiThread := True;
  try
    RunServer;
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;
end.
