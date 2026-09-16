program RangeMediaSample;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Web.ReqMulti,
  Web.WebReq,
  Web.WebBroker,
  IdContext,
  IdHTTPWebBrokerBridge,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.DotEnv,
  MVCFramework.Signal,
  WebModuleU in 'WebModuleU.pas' {RangeMediaWebModule: TWebModule},
  HomeControllerU in 'HomeControllerU.pas';


procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.OnParseAuthentication := TMVCParseAuthentication.OnParseAuthentication;
    LServer.DefaultPort := APort;
    LServer.Active := True;
    Writeln('========================================');
    Writeln(' Range Media Middleware Sample');
    Writeln('========================================');
    Writeln;
    Writeln(Format('Listening on http://localhost:%d', [APort]));
    Writeln;
    Writeln('Endpoints:');
    Writeln('  GET  /              - Media player page');
    Writeln('  GET  /media/<file>  - Serves media files with Range support');
    Writeln;
    Writeln('Place audio/video files in the "media" folder, then');
    Writeln('open http://localhost:8080 in your browser.');
    Writeln;
    Writeln('Press Ctrl+C to stop.');
    WaitForTerminationSignal;
    EnterInShutdownState;
    LServer.Active := False;
  finally
    LServer.Free;
  end;
end;

begin
  IsMultiThread := True;
  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    WebRequestHandlerProc.MaxConnections := 1024;
    RunServer(dotEnv.Env('dmvc.server.port', 8080));
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
