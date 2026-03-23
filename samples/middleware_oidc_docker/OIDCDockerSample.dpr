program OIDCDockerSample;

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
  MVCFramework.SQLGenerators.PostgreSQL,
  WebModuleU in 'WebModuleU.pas' {OIDCDockerSampleWebModule: TWebModule},
  AppConfigU in 'AppConfigU.pas',
  UserEntityU in 'UserEntityU.pas',
  MigrationServiceU in 'MigrationServiceU.pas',
  HomeControllerU in 'HomeControllerU.pas',
  DashboardControllerU in 'DashboardControllerU.pas',
  UserControllerU in 'UserControllerU.pas',
  HealthControllerU in 'HealthControllerU.pas';

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
    Writeln(' OIDC Docker Sample');
    Writeln('========================================');
    Writeln;
    Writeln(Format('Listening on http://localhost:%d', [APort]));
    Writeln;
    Writeln('Endpoints:');
    Writeln('  GET  /           - Public landing page');
    Writeln('  GET  /dashboard  - Protected dashboard (OIDC claims + role)');
    Writeln('  GET  /users      - Admin-only user list');
    Writeln('  GET  /health     - Docker health check');
    Writeln('  GET  /auth/login - Redirects to OIDC provider');
    Writeln('  GET  /auth/logout- Clears session, redirects to /');
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
    RunServer(dotEnv.Env('DMVC_SERVER_PORT', 8080));
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
