program APIGuardFilterSample;

// APIGuard HTTP filter sample. Contributor: Giordano Giaccaglia - KD Software S.r.l.s.
// https://www.kdsoftware.it - info@kdsoftware.it
//
// The intrusion guard as an HTTP filter, in front of a Minimal API and of the
// static files of its dashboard. See README.md.

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.IOUtils,
  JsonDataObjects,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Signal,
  MVCFramework.Server.Intf,
  MVCFramework.Server.Factory,
  MVCFramework.MinimalAPI,
  MVCFramework.Filters,
  MVCFramework.Filters.APIGuard,
  MVCFramework.Filters.APIGuard.FireDAC,
  FireDAC.Phys.SQLite,FireDAC.DApt;

{$R *.res}

const
  PORT = 8080;
  BLOCK_SECONDS = 60; // short, so the demo can be replayed; production: hours, or 0 = permanent

function CreateGuard: IMVCAPIGuard;
begin
  // The blacklist survives a restart in a text file next to the exe.
  // For a database: TMVCAPIGuardFireDACStorage.Create('MyConnDef')
  // (unit MVCFramework.Filters.APIGuard.FireDAC).
  //DB
  //Result := TMVCAPIGuard.Create(
  //  TMVCAPIGuardFireDACStorage.Create('MyConnDef'));

  //FileStorage
  Result := TMVCAPIGuard.Create(
    TMVCAPIGuardFileStorage.Create(TPath.Combine(AppPath, 'blacklist.txt')));

  // The history of blocks, unblocks and 403s, appended to a text file next to the exe
  // For a database: TMVCAPIGuardFireDACAuditLog.Create('MyConnDef')
  //DB
  //Result.AuditLog := TMVCAPIGuardFireDACAuditLog.Create('MyConnDef');
  //FileStorage
  Result.AuditLog := TMVCAPIGuardFileAuditLog.Create(TPath.Combine(AppPath, 'apiguard_audit.txt'));

  // Every built-in rule tuned: threshold, window (s), block (s)
  Result.Scan.Configure(10, 60, BLOCK_SECONDS);
  Result.BruteForce.Configure(5, 300, BLOCK_SECONDS).OnPaths(['/api/login']);
  Result.Flood.Configure(100, 10, BLOCK_SECONDS);
  Result.Injection.Configure(1, 60, BLOCK_SECONDS);
  Result.BadAgent.Configure(1, 60, BLOCK_SECONDS);

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

  // The dashboard and the admin routes answer only the monitor IPs (localhost by default)
  Result.AddMonitorOnlyPath('/api-guard/ui');
  Result.AddMonitorOnlyPath('/admin');
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

procedure ConfigureRoutes(const ARoot: TMVCRouteGroup<TObject>; const AGuard: IMVCAPIGuard);
var
  lApi, lAdmin: TMVCRouteGroup<TObject>;
begin
  lApi := ARoot.Prefix('/api');

  // normal traffic never trips a rule
  lApi.MapGet('/hello',
    function: IMVCResponse
    begin
      Result := Ok('Hello from a guarded API');
    end);

  // demo login: the 401s on wrong credentials are what "bruteforce" counts
  lApi.MapPost<TWebContext>('/login',
    function(AContext: TWebContext): IMVCResponse
    var
      lBody: TJsonObject;
      lValid: Boolean;
    begin
      lBody := StrToJSONObject(AContext.Request.Body); // nil when the body is not a JSON object
      try
        // demo credentials only: a real service checks a hashed password
        lValid := Assigned(lBody) and (lBody.S['username'] = 'admin') and (lBody.S['password'] = 'secret');
      finally
        lBody.Free;
      end;
      if lValid then
        Result := Ok('Logged in')
      else
        Result := Unauthorized('Invalid credentials');
    end);

  // the guard at run time: these routes use the object the filter keeps alive.
  // "/admin" is a monitor-only path: anyone else gets a 404, which "scan" counts
  lAdmin := ARoot.Prefix('/admin/blocked');
  lAdmin.MapPost<string>('/($ip)',
    function(AIP: string): IMVCResponse
    begin
      AGuard.BlockIP(AIP, 'blocked from /admin', BLOCK_SECONDS);
      Result := Ok('Blocked ' + AIP);
    end);
  lAdmin.MapDelete<string>('/($ip)',
    function(AIP: string): IMVCResponse
    begin
      AGuard.UnblockIP(AIP);
      Result := Ok('Unblocked ' + AIP);
    end);
end;

procedure RunServer;
var
  lEngine: TMVCEngine;
  lServer: IMVCServer;
  lGuard: IMVCAPIGuard;
begin
  WriteLn('** DMVCFramework APIGuard HTTP Filter Sample ** build ' + DMVCFRAMEWORK_VERSION);
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
    lGuard := CreateGuard;
    // FIRST filter: the outermost one sees every request and every final status
    lEngine.Use(APIGuardHTTPFilter(lGuard)); // the filter owns the guard from here on
    lEngine.Use(StaticFilesHTTPFilter('/api-guard/ui', TPath.Combine(AppPath, 'www')));
    ConfigureRoutes(lEngine.Root, lGuard);

    lServer := TMVCServerFactory.CreateIndyDirect(lEngine);
    lServer.Listen(PORT);
    try
      WriteLn(Format('Server started on http://localhost:%d (Indy Direct)', [PORT]));
      WriteLn;
      WriteLn(Format('  dashboard (localhost)   : http://localhost:%d/api-guard/ui/', [PORT]));
      WriteLn(Format('  status (localhost only) : curl http://localhost:%d/api-guard/status', [PORT]));
      WriteLn(Format('  normal traffic          : curl http://localhost:%d/api/hello', [PORT]));
      WriteLn(Format('  unblock (localhost only): curl -X DELETE http://localhost:%d/admin/blocked/127.0.0.3', [PORT]));
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
    lEngine.Free; // releases the filters, and with them the guard
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
