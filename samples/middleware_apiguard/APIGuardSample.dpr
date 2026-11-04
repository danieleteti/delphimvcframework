program APIGuardSample;

// APIGuard middleware sample. Contributor: Giordano Giaccaglia - KD Software S.r.l.s.
// https://www.kdsoftware.it - info@kdsoftware.it

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.IOUtils,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Console,
  MVCFramework.Signal,
  MVCFramework.Logger,
  MVCFramework.Server.Intf,
  MVCFramework.Server.Factory,
  MVCFramework.Middleware.APIGuard,
  MVCFramework.Middleware.StaticFiles,
  MainControllerU in 'MainControllerU.pas';

{$R *.res}

const
  PORT = 8080;
  BLOCK_SECONDS = 60; // short, so the demo can be replayed; production: hours, or 0 = permanent

function CreateGuard: TMVCAPIGuardMiddleware;
begin
  // The blacklist survives a restart in a text file next to the exe.
  // For a database: TMVCAPIGuardFireDACStorage.Create('MyConnDef')
  // (unit MVCFramework.Middleware.APIGuard.FireDAC).
  Result := TMVCAPIGuardMiddleware.Create(
    TMVCAPIGuardFileStorage.Create(TPath.Combine(AppPath, 'blacklist.txt')));

  // The history of blocks, unblocks and 403s, appended to a text file next to the exe
  // For a database: TMVCAPIGuardFireDACAuditLog.Create('MyConnDef')
  Result.AuditLog := TMVCAPIGuardFileAuditLog.Create(TPath.Combine(AppPath, 'apiguard_audit.txt'));

  // Every built-in rule tuned: threshold, window (s), block (s)
  Result.Rule(GUARD_RULE_SCAN).Configure(10, 60, BLOCK_SECONDS);
  Result.Rule(GUARD_RULE_BRUTEFORCE).Configure(5, 300, BLOCK_SECONDS).OnPaths(['/api/login']);
  Result.Rule(GUARD_RULE_FLOOD).Configure(100, 10, BLOCK_SECONDS);
  Result.Rule(GUARD_RULE_INJECTION).Configure(1, 60, BLOCK_SECONDS);
  Result.Rule(GUARD_RULE_BAD_AGENT).Configure(1, 60, BLOCK_SECONDS);

  // A custom rule: nobody legitimate asks for these files, one hit is enough
  Result.AddRule('honeypot', gpRequest, 1, 60, BLOCK_SECONDS,
    function(const AContext: TWebContext): Boolean
    var
      lPath: string;
    begin
      lPath := AContext.Request.PathInfo.ToLower;
      Result := (lPath = '/backup.zip') or (lPath = '/admin.php') or (lPath = '/config.json');
    end);

  // One more signature for the "injection" rule
  Result.AddSignature('/actuator/');

  // The dashboard (bin\www\index.html, served below) answers only the monitor IPs
  Result.AddMonitorOnlyPath('/api-guard/ui');

  // Who may read /api-guard/status and the dashboard besides localhost
  // Result.AddMonitorIP('10.0.0.5');

  // IPs never blocked (e.g. your own monitoring probe)
  // Result.AddWhitelistIP('10.0.0.6');

  Result.OnAlert :=
    procedure(const AAlert: TMVCAPIGuardAlert)
    begin
      WriteLn(Format('[ALERT] %s  ip=%s  rule=%s  path=%s  %s',
        [FormatDateTime('hh:nn:ss', AAlert.Timestamp), AAlert.IP, AAlert.Rule, AAlert.Path, AAlert.Message]));
    end;
end;

procedure RunServer;
var
  lEngine: TMVCEngine;
  lServer: IMVCServer;
begin
  WriteLn('** DMVCFramework APIGuard Middleware Sample ** build ' + DMVCFRAMEWORK_VERSION);
  WriteLn;

  lEngine := TMVCEngine.Create(
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.DefaultContentType] := TMVCConstants.DEFAULT_CONTENT_TYPE;
      Config[TMVCConfigKey.DefaultContentCharset] := TMVCConstants.DEFAULT_CONTENT_CHARSET;
      Config[TMVCConfigKey.AllowUnhandledAction] := 'false';
      Config[TMVCConfigKey.ExposeServerSignature] := 'false';
    end);
  try
    // FIRST middleware: one that handles a request before it would hide it from the guard
    lEngine.AddMiddleware(CreateGuard);
    lEngine.AddMiddleware(TMVCStaticFilesMiddleware.Create('/api-guard/ui', TPath.Combine(AppPath, 'www')));
    lEngine.AddController(TMainController);

    lServer := TMVCServerFactory.CreateIndyDirect(lEngine);
    lServer.Listen(PORT);
    try
      WriteLn(Format('Server started on http://localhost:%d (Indy Direct)', [PORT]));
      WriteLn;
      WriteLn(Format('  dashboard (localhost)   : http://localhost:%d/api-guard/ui/', [PORT]));
      WriteLn(Format('  status (localhost only) : curl http://localhost:%d/api-guard/status', [PORT]));
      WriteLn(Format('  normal traffic          : curl http://localhost:%d/api/hello', [PORT]));
      WriteLn('  simulated attacks       : python test_apiguard.py all');
      WriteLn;
      WriteLn(Format('Blocks last %d seconds; the blacklist is in blacklist.txt next to the exe.', [BLOCK_SECONDS]));
      WriteLn('Press Ctrl+C to stop.');
      WaitForTerminationSignal;
      WriteLn('Shutting down...');
    finally
      lServer.Stop;
      lServer := nil;
    end;
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
