program StreamingSample;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Console,
  MVCFramework.Signal,
  {$IF Defined(SYDNEYORBETTER)}
  MVCFramework.Logger,
  {$ENDIF}
  IdHTTPWebBrokerBridge,
  Web.WebReq,
  WebModuleU in 'WebModuleU.pas' {WebModule1: TWebModule},
  StreamingControllerU in 'StreamingControllerU.pas';

{$R *.res}

procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  WriteLn('** DMVCFramework SSE & JSONL Streaming Sample **');
  WriteLn;
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;
    LServer.Active := True;
    WriteLn('Server started on port ', APort);
    WriteLn;
    WriteLn('Open your browser at:');
    WriteLn;
    WriteLn('  ==> http://localhost:', APort, '/static/index.html');
    WriteLn;
    WriteLn('The web page has 3 interactive demos:');
    WriteLn('  1. AI Chat Stream  (SSE)   - text streamed word by word');
    WriteLn('  2. Progress Stream (SSE)   - real-time progress bar');
    WriteLn('  3. People Stream   (JSONL) - table filled row by row');
    WriteLn;
    WriteLn('Press Ctrl+C to stop.');
    WaitForTerminationSignal;
    WriteLn('Shutting down...');
  finally
    LServer.Free;
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  IsMultiThread := True;
  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    RunServer(8080);
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;
end.
