program OIDCSample;

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
  WebModuleU in 'WebModuleU.pas' {OIDCSampleWebModule: TWebModule},
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
    Writeln(' OIDC Middleware Sample');
    Writeln('========================================');
    Writeln;
    Writeln(Format('Listening on http://localhost:%d', [APort]));
    Writeln;
    Writeln('Endpoints:');
    Writeln('  GET  /            - Public landing page');
    Writeln('  GET  /dashboard   - Protected dashboard (shows OIDC claims)');
    Writeln('  GET  /auth/login  - Redirects to OIDC provider');
    Writeln('  GET  /auth/logout - Clears session and redirects to /');
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
