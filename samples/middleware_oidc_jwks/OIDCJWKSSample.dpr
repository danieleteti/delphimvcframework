program OIDCJWKSSample;

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
  WebModuleU in 'WebModuleU.pas' {OIDCJWKSSampleWebModule: TWebModule},
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
    Writeln(' OIDC + JWKS Signature Verification');
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
    Writeln('ID token signature verification: ENABLED');
    Writeln('  (set OIDC_VERIFY_SIGNATURE=false in .env to disable)');
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
  {$IFDEF DEBUG}ReportMemoryLeaksOnShutdown := True;{$ENDIF}
  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    WebRequestHandlerProc.MaxConnections := 1024;
    RunServer(dotEnv.Env('DMVC_SERVER_PORT', 8080));
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
