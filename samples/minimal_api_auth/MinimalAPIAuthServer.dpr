program MinimalAPIAuthServer;

{$APPTYPE CONSOLE}

// =============================================================================
//  DMVCFramework Minimal API — Auth + Logging sample
// =============================================================================
//
//  Demonstrates the full TMVCRouteGroup hook chain in a realistic setting:
//
//    OnBefore (proc)    — request logging, timing
//    OnBefore (filter)  — Bearer token auth (short-circuits 401)
//    OnSuccess (proc)   — outgoing status code logging
//    OnError (proc)     — exception observer
//    OnError (handler)  — domain exception -> custom response
//    OnAlways (proc)    — finally-style elapsed-time + DONE log
//
//  Route map:
//
//    PUBLIC (only logging hooks)
//      GET  /health        -> 200
//      GET  /throw         -> Exception -> default 500 envelope
//      GET  /throw-token   -> ETokenError -> 401 (handler-mapped)
//
//    AUTHENTICATED (+ Bearer auth)
//      GET  /api/me        -> 200 + principal (user + role)
//      GET  /api/throw-token -> ETokenError -> 401 (handler-mapped)
//
//    ADMIN-ONLY (auth + RequireRole('admin'))
//      GET    /api/admin/audit -> 200 + JSON array of hook-emitted lines
//      DELETE /api/admin/audit -> 204 (clears the audit log)
//
//  Tokens used by the integration tests:
//    Authorization: Bearer alice-token   (role=user)
//    Authorization: Bearer bob-token     (role=admin)
//
//  Routes wired in RoutesU.ConfigureRoutes.
// =============================================================================

uses
  System.SysUtils,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Logger,
  MVCFramework.Server.Intf,
  MVCFramework.Server.Factory,
  MVCFramework.MinimalAPI in '..\..\sources\MVCFramework.MinimalAPI.pas',
  HooksU in 'HooksU.pas',
  RoutesU in 'RoutesU.pas';

// {$R *.res} -- omitted; the IDE creates this on first open

const
  PORT = 8081;

procedure RunServer;
var
  lEngine: TMVCEngine;
begin
  WriteLn('** DMVCFramework Minimal API Auth + Logging Sample **');
  WriteLn;

  lEngine := TMVCEngine.Create(
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.DefaultContentType] := TMVCMediaType.APPLICATION_JSON;
    end);
  try
    ConfigureRoutes(lEngine.Root);

    WriteLn(Format('Server starting on http://localhost:%d', [PORT]));
    WriteLn;
    WriteLn('Try:');
    WriteLn('  curl http://localhost:8081/health');
    WriteLn('  curl http://localhost:8081/api/me');
    WriteLn('  curl http://localhost:8081/api/me -H "Authorization: Bearer alice-token"');
    WriteLn('  curl http://localhost:8081/api/admin/audit -H "Authorization: Bearer bob-token"');
    WriteLn;
    WriteLn('Press Ctrl+C to stop.');
    TMVCServerFactory.CreateIndyDirect(lEngine).RunAndWait(PORT);
    WriteLn('Shutting down...');
  finally
    lEngine.Free;
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
