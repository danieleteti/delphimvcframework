unit APIGuardMiddlewareTestsU;

// Contributor of this file: Giordano Giaccaglia - KD Software S.r.l.s.
// https://www.kdsoftware.it - info@kdsoftware.it

// End-to-end tests of TMVCAPIGuardMiddleware: an embedded server, real requests
// from 127.0.0.1, the guard reset before every test.

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.Net.HttpClient,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Server.Intf,
  MVCFramework.Middleware.APIGuard;

const
  APIGUARD_PORT = 8993;

type
  [MVCPath('/api')]
  TAPIGuardTestController = class(TMVCController)
  public
    [MVCPath('/ok')]
    [MVCHTTPMethod([httpGET])]
    function OK: String;
    [MVCPath('/login')]
    [MVCHTTPMethod([httpPOST])]
    function Login: IMVCResponse;
    [MVCPath('/monitor/data')]
    [MVCHTTPMethod([httpGET])]
    function MonitorData: String;
  end;

  [TestFixture]
  TAPIGuardMiddlewareTests = class
  private
    fServer: IMVCServer;
    fEngine: TMVCEngine;
    fGuard: TMVCAPIGuardMiddleware;
    fClient: THTTPClient;
    function Get(const APath: string; const AUserAgent: string = ''): IHTTPResponse;
    function Post(const APath: string): Integer;
    function WaitForLines(const AFile: string; const ACount: Integer): TArray<string>;
  public
    [SetupFixture]
    procedure SetupFixture;
    [TeardownFixture]
    procedure TeardownFixture;
    [Setup]
    procedure Setup;

    [Test]
    procedure CleanTraffic_PassesThrough;
    [Test]
    procedure Scan_TooMany404_BlocksIP;
    [Test]
    procedure BruteForce_TooMany401OnLogin_BlocksIP;
    [Test]
    procedure Injection_TraversalInQuery_BlocksIP;
    [Test]
    procedure Injection_DoubleEncoded_BlocksIP;
    [Test]
    procedure BadAgent_Sqlmap_BlocksIP;
    [Test]
    procedure Flood_OverThreshold_BlocksIP;
    [Test]
    procedure CustomRule_BlocksIP;
    [Test]
    procedure DisabledRule_DoesNotBlock;
    [Test]
    procedure Status_FromLoopback_ReportsBlockEvenWhenBlocked;
    [Test]
    procedure Unblock_LetsTheIPBackIn;
    [Test]
    procedure ExpiredBlock_IsGoneAndReblockable;
    [Test]
    procedure FileStorage_SurvivesRestart;
    [Test]
    procedure FileStorage_HandEditedLine_IsPermanent;
    [Test]
    procedure FileAuditLog_RecordsBlockAndUnblock;
    [Test]
    procedure FileAuditLog_RecordsDeniedRequest;
    [Test]
    procedure FileAuditLog_Retention_PurgesOldLines;
    [Test]
    procedure UnknownRule_Raises;
    [Test]
    procedure Status_FromIPv6Loopback_Answers;
    [Test]
    procedure MonitorOnlyPath_FromLoopback_PassesThrough;
    [Test]
    procedure Monitor_ForwardedFromLocalProxy_Denied;
    [Test]
    [TestCase('v4', '10.1.2.3,10.1.2.3')]
    [TestCase('v6 loopback expanded (Indy)', '0:0:0:0:0:0:0:1,::1')]
    [TestCase('v6 loopback', '::1,::1')]
    [TestCase('v6 brackets and zone', '[FE80:0:0:0:0:0:0:1%12],fe80::1')]
    [TestCase('v6 leading zeros', '2001:0db8:0000:0000:0000:ff00:0042:8329,2001:db8::ff00:42:8329')]
    [TestCase('v6 longest run wins', '2001:0:0:1:0:0:0:1,2001:0:0:1::1')]
    [TestCase('v6 single zero not compressed', '2001:db8:0:1:1:1:1:1,2001:db8:0:1:1:1:1:1')]
    [TestCase('v4 mapped dotted', '::ffff:127.0.0.1,127.0.0.1')]
    [TestCase('v4 mapped expanded', '0:0:0:0:0:ffff:7f00:1,127.0.0.1')]
    [TestCase('garbage kept', 'not:an:ip,not:an:ip')]
    procedure NormalizeIP(const AInput, AExpected: string);
  end;

implementation

uses
  System.IOUtils,
  JsonDataObjects,
  MVCFramework.Serializer.Commons,
  MVCFramework.Server.Factory;

{ TAPIGuardTestController }

function TAPIGuardTestController.OK: String;
begin
  Result := 'ok';
end;

function TAPIGuardTestController.Login: IMVCResponse;
begin
  Result := UnauthorizedResponse;
end;

function TAPIGuardTestController.MonitorData: String;
begin
  Result := 'monitor';
end;

{ TAPIGuardMiddlewareTests }

procedure TAPIGuardMiddlewareTests.SetupFixture;
begin
  fEngine := TMVCEngine.Create;
  fEngine.AddController(TAPIGuardTestController);
  fGuard := TMVCAPIGuardMiddleware.Create;
  fGuard.Rule(GUARD_RULE_SCAN).Configure(3, 60, 60);
  fGuard.Rule(GUARD_RULE_BRUTEFORCE).Configure(3, 60, 60).OnPaths(['/api/login']);
  fGuard.Rule(GUARD_RULE_FLOOD).Configure(40, 60, 60);
  fGuard.Rule(GUARD_RULE_INJECTION).Configure(1, 60, 60);
  fGuard.AddRule('secret_header', gpRequest, 1, 60, 60,
    function(const AContext: TWebContext): Boolean
    begin
      Result := AContext.Request.Headers['X-Evil'] <> '';
    end);
  fGuard.AddMonitorOnlyPath('/api/monitor');
  fEngine.AddMiddleware(fGuard); // the engine holds the interface reference
  fServer := TMVCServerFactory.CreateIndyDirect(fEngine);
  fServer.Listen(APIGUARD_PORT);
  fClient := THTTPClient.Create;
end;

procedure TAPIGuardMiddlewareTests.TeardownFixture;
begin
  fClient.Free;
  fServer.Stop;
  fServer := nil;
  fEngine.Free; // the server does not own it; the engine releases the guard
end;

procedure TAPIGuardMiddlewareTests.Setup;
begin
  fGuard.Reset;
  fGuard.Rule(GUARD_RULE_SCAN).Enabled := True;
end;

function TAPIGuardMiddlewareTests.Get(const APath, AUserAgent: string): IHTTPResponse;
var
  lReq: IHTTPRequest;
begin
  lReq := fClient.GetRequest('GET', 'http://127.0.0.1:' + APIGUARD_PORT.ToString + APath);
  if AUserAgent <> '' then
    lReq.AddHeader('User-Agent', AUserAgent);
  Result := fClient.Execute(lReq);
end;

function TAPIGuardMiddlewareTests.Post(const APath: string): Integer;
var
  lBody: TStringStream;
begin
  lBody := TStringStream.Create('');
  try
    Result := fClient.Post('http://127.0.0.1:' + APIGUARD_PORT.ToString + APath, lBody).StatusCode;
  finally
    lBody.Free;
  end;
end;

procedure TAPIGuardMiddlewareTests.CleanTraffic_PassesThrough;
var
  I: Integer;
begin
  for I := 1 to 10 do
    Assert.AreEqual(200, Get('/api/ok').StatusCode);
  Assert.IsFalse(fGuard.IsBlocked('127.0.0.1'));
end;

procedure TAPIGuardMiddlewareTests.Scan_TooMany404_BlocksIP;
begin
  Assert.AreEqual(404, Get('/nope1').StatusCode);
  Assert.AreEqual(404, Get('/nope2').StatusCode);
  Assert.AreEqual(404, Get('/nope3').StatusCode); // third 404 trips the rule
  Assert.AreEqual(403, Get('/api/ok').StatusCode);
end;

procedure TAPIGuardMiddlewareTests.BruteForce_TooMany401OnLogin_BlocksIP;
begin
  Assert.AreEqual(401, Post('/api/login'));
  Assert.AreEqual(401, Post('/api/login'));
  Assert.AreEqual(401, Post('/api/login'));
  Assert.AreEqual(403, Get('/api/ok').StatusCode);
  Assert.AreEqual(GUARD_RULE_BRUTEFORCE, fGuard.BlockedIPs[0].Rule);
end;

procedure TAPIGuardMiddlewareTests.Injection_TraversalInQuery_BlocksIP;
begin
  Assert.AreEqual(403, Get('/api/ok?file=..%2F..%2Fetc%2Fpasswd').StatusCode);
  Assert.AreEqual(403, Get('/api/ok').StatusCode);
end;

procedure TAPIGuardMiddlewareTests.Injection_DoubleEncoded_BlocksIP;
begin
  Assert.AreEqual(403, Get('/api/ok?q=1%2527%2520union%2520select%2520').StatusCode);
end;

procedure TAPIGuardMiddlewareTests.BadAgent_Sqlmap_BlocksIP;
begin
  Assert.AreEqual(403, Get('/api/ok', 'sqlmap/1.7#stable').StatusCode);
end;

procedure TAPIGuardMiddlewareTests.Flood_OverThreshold_BlocksIP;
var
  I: Integer;
begin
  for I := 1 to 39 do
    Assert.AreEqual(200, Get('/api/ok').StatusCode);
  Assert.AreEqual(403, Get('/api/ok').StatusCode);
end;

procedure TAPIGuardMiddlewareTests.CustomRule_BlocksIP;
var
  lReq: IHTTPRequest;
begin
  lReq := fClient.GetRequest('GET', 'http://127.0.0.1:' + APIGUARD_PORT.ToString + '/api/ok');
  lReq.AddHeader('X-Evil', '1');
  Assert.AreEqual(403, fClient.Execute(lReq).StatusCode);
  Assert.AreEqual('secret_header', fGuard.BlockedIPs[0].Rule);
end;

procedure TAPIGuardMiddlewareTests.DisabledRule_DoesNotBlock;
var
  I: Integer;
begin
  fGuard.Rule(GUARD_RULE_SCAN).Enabled := False;
  for I := 1 to 5 do
    Assert.AreEqual(404, Get('/nope').StatusCode);
  Assert.AreEqual(200, Get('/api/ok').StatusCode);
end;

procedure TAPIGuardMiddlewareTests.Status_FromLoopback_ReportsBlockEvenWhenBlocked;
var
  lResp: IHTTPResponse;
  lJSON: TJsonObject;
begin
  fGuard.BlockIP('127.0.0.1', 'test');
  Assert.AreEqual(403, Get('/api/ok').StatusCode);
  lResp := Get('/api-guard/status');
  Assert.AreEqual(200, lResp.StatusCode);
  lJSON := TJsonObject.Parse(lResp.ContentAsString) as TJsonObject;
  try
    Assert.AreEqual(1, lJSON.A['blocked_ips'].Count);
    Assert.AreEqual('127.0.0.1', lJSON.A['blocked_ips'].O[0].S['ip']);
    Assert.IsTrue(lJSON.A['blocked_ips'].O[0].IsNull('expires_at'));
    Assert.AreEqual(1, lJSON.I['blocked_requests_total']);
    // the status call itself is not counted: only the refused /api/ok
    Assert.AreEqual(1, lJSON.I['requests_last_minute']);
    Assert.AreEqual(1, lJSON.I['requests_total']);
    Assert.AreEqual(GUARD_RULE_MANUAL, lJSON.A['alerts'].O[0].S['rule']);
    Assert.AreEqual(6, lJSON.A['rules'].Count);
  finally
    lJSON.Free;
  end;
end;

procedure TAPIGuardMiddlewareTests.Unblock_LetsTheIPBackIn;
begin
  fGuard.BlockIP('127.0.0.1', 'test', 60);
  Assert.AreEqual(403, Get('/api/ok').StatusCode);
  fGuard.UnblockIP('127.0.0.1');
  Assert.AreEqual(200, Get('/api/ok').StatusCode);
end;

procedure TAPIGuardMiddlewareTests.ExpiredBlock_IsGoneAndReblockable;
begin
  fGuard.BlockIP('127.0.0.1', 'short', 1);
  Sleep(1100);
  // expired but not yet swept (the cleanup runs once a minute): neither listed nor blocking
  Assert.AreEqual(0, Length(fGuard.BlockedIPs));
  Assert.AreEqual(200, Get('/api/ok').StatusCode);
  // and a rule can block the IP again straight away
  Assert.AreEqual(403, Get('/api/ok?f=../../etc/passwd').StatusCode);
  Assert.IsTrue(fGuard.IsBlocked('127.0.0.1'));
end;

procedure TAPIGuardMiddlewareTests.FileStorage_SurvivesRestart;
var
  lFile: string;
  lGuard: TMVCAPIGuardMiddleware;
  lRef: IMVCMiddleware;
begin
  lFile := TPath.GetTempFileName;
  try
    lGuard := TMVCAPIGuardMiddleware.Create(TMVCAPIGuardFileStorage.Create(lFile));
    lRef := lGuard;
    lGuard.BlockIP('10.1.2.3', 'tab'#9'in reason', 3600);
    lGuard.BlockIP('10.1.2.4', 'permanent');
    lRef := nil; // a restart: the writer flushes the list before the guard goes
    lGuard := TMVCAPIGuardMiddleware.Create(TMVCAPIGuardFileStorage.Create(lFile));
    lRef := lGuard;
    Assert.IsTrue(lGuard.IsBlocked('10.1.2.3'));
    Assert.IsTrue(lGuard.IsBlocked('10.1.2.4'));
    Assert.IsFalse(lGuard.IsBlocked('10.1.2.5'));
  finally
    lRef := nil;
    TFile.Delete(lFile);
  end;
end;

procedure TAPIGuardMiddlewareTests.FileStorage_HandEditedLine_IsPermanent;
var
  lFile: string;
  lGuard: TMVCAPIGuardMiddleware;
  lRef: IMVCMiddleware;
begin
  lFile := TPath.GetTempFileName;
  try
    TFile.WriteAllText(lFile, '# comment' + sLineBreak + '192.168.0.9' + sLineBreak +
      '192.168.0.10'#9'scan'#9'not-a-date'#9'not-a-date'#9'x' + sLineBreak, TEncoding.UTF8);
    lGuard := TMVCAPIGuardMiddleware.Create(TMVCAPIGuardFileStorage.Create(lFile));
    lRef := lGuard;
    Assert.IsTrue(lGuard.IsBlocked('192.168.0.9'));
    Assert.IsTrue(lGuard.IsBlocked('192.168.0.10'));
    Assert.AreEqual(2, Length(lGuard.BlockedIPs));
  finally
    lRef := nil;
    TFile.Delete(lFile);
  end;
end;

function TAPIGuardMiddlewareTests.WaitForLines(const AFile: string;
  const ACount: Integer): TArray<string>;
var
  I: Integer;
begin
  // the audit log is written by the guard's writer thread
  for I := 1 to 100 do
  begin
    if TFile.Exists(AFile) then
    begin
      Result := TFile.ReadAllLines(AFile, TEncoding.UTF8);
      if Length(Result) >= ACount then
        Exit;
    end;
    Sleep(50);
  end;
  Assert.Fail(Format('%s: expected %d lines', [AFile, ACount]));
end;

procedure TAPIGuardMiddlewareTests.FileAuditLog_RecordsBlockAndUnblock;
var
  lFile: string;
  lGuard: TMVCAPIGuardMiddleware;
  lRef: IMVCMiddleware;
  lLines: TArray<string>;
begin
  lFile := TPath.Combine(TPath.GetTempPath, TPath.GetGUIDFileName + '.txt');
  try
    lGuard := TMVCAPIGuardMiddleware.Create;
    lRef := lGuard;
    lGuard.AuditLog := TMVCAPIGuardFileAuditLog.Create(lFile);
    lGuard.BlockIP('10.1.2.3', 'first');
    lGuard.UnblockIP('10.1.2.3');
    lGuard.BlockIP('10.1.2.4', 'forged'#13#10'line'#9'field');
    lGuard.Reset; // unblocks 10.1.2.4; the history survives it
    lRef := nil; // the destructor flushes the queue
    lLines := TFile.ReadAllLines(lFile, TEncoding.UTF8);
    Assert.AreEqual(5, Length(lLines), 'header + 4 events');
    Assert.StartsWith('#', lLines[0]);
    Assert.AreEqual('block|10.1.2.3|manual', string.Join('|', Copy(lLines[1].Split([#9]), 1, 3)));
    Assert.AreEqual('unblock|10.1.2.3|manual', string.Join('|', Copy(lLines[2].Split([#9]), 1, 3)));
    Assert.AreEqual(6, Length(lLines[3].Split([#9])), 'tabs and line breaks in the reason are flattened');
    Assert.AreEqual('block|10.1.2.4|manual', string.Join('|', Copy(lLines[3].Split([#9]), 1, 3)));
    Assert.AreEqual('unblock|10.1.2.4|manual', string.Join('|', Copy(lLines[4].Split([#9]), 1, 3)));
  finally
    lRef := nil;
    if TFile.Exists(lFile) then
      TFile.Delete(lFile);
  end;
end;

procedure TAPIGuardMiddlewareTests.FileAuditLog_RecordsDeniedRequest;
var
  lFile: string;
  lLines, lFields: TArray<string>;
begin
  lFile := TPath.Combine(TPath.GetTempPath, TPath.GetGUIDFileName + '.txt');
  fGuard.AuditLog := TMVCAPIGuardFileAuditLog.Create(lFile);
  try
    fGuard.BlockIP('127.0.0.1', 'test');
    Assert.AreEqual(403, Get('/api/ok').StatusCode);
    lLines := WaitForLines(lFile, 3);
    lFields := lLines[2].Split([#9]);
    Assert.AreEqual(GUARD_AUDIT_DENIED, lFields[1]);
    Assert.AreEqual('127.0.0.1', lFields[2]);
    Assert.AreEqual(GUARD_RULE_MANUAL, lFields[3]);
    Assert.AreEqual('/api/ok', lFields[4]);
  finally
    fGuard.AuditLog := nil;
    fGuard.UnblockIP('127.0.0.1');
    if TFile.Exists(lFile) then
      TFile.Delete(lFile);
  end;
end;

procedure TAPIGuardMiddlewareTests.FileAuditLog_Retention_PurgesOldLines;
var
  lFile: string;
  lGuard: TMVCAPIGuardMiddleware;
  lRef: IMVCMiddleware;
  lLines: TArray<string>;
begin
  lFile := TPath.Combine(TPath.GetTempPath, TPath.GetGUIDFileName + '.txt');
  try
    TFile.WriteAllText(lFile, '# header' + sLineBreak +
      '2000-01-01T00:00:00.000+01:00'#9'block'#9'10.0.0.1'#9'scan'#9#9'old' + sLineBreak +
      'not-a-date'#9'block'#9'10.0.0.2'#9'scan'#9#9'undated, kept' + sLineBreak +
      DateTimeToISOTimeStamp(Now - 1)+ #9'block'#9'10.0.0.3'#9'scan'#9#9'recent' + sLineBreak, TEncoding.UTF8);
    lGuard := TMVCAPIGuardMiddleware.Create;
    lRef := lGuard;
    lGuard.AuditLog := TMVCAPIGuardFileAuditLog.Create(lFile);
    lGuard.AuditRetentionDays := 30;
    lGuard.BlockIP('10.1.2.3', 'wakes the writer');
    lRef := nil;
    lLines := TFile.ReadAllLines(lFile, TEncoding.UTF8);
    Assert.AreEqual(4, Length(lLines), 'header, undated, recent, new');
    Assert.Contains(lLines[1], 'undated');
    Assert.Contains(lLines[2], 'recent');
    Assert.Contains(lLines[3], '10.1.2.3');
  finally
    lRef := nil;
    if TFile.Exists(lFile) then
      TFile.Delete(lFile);
  end;
end;

procedure TAPIGuardMiddlewareTests.Status_FromIPv6Loopback_Answers;
begin
  // browsers resolve localhost to ::1, which Indy reports as 0:0:0:0:0:0:0:1
  Assert.AreEqual(200, fClient.Get('http://[::1]:' + APIGUARD_PORT.ToString + '/api-guard/status').StatusCode);
end;

procedure TAPIGuardMiddlewareTests.MonitorOnlyPath_FromLoopback_PassesThrough;
begin
  // the denial (404 from a non-monitor IP) needs another source address: see the sample's test script
  Assert.AreEqual(200, Get('/api/monitor/data').StatusCode);
  Assert.AreEqual(200, fClient.Get('http://[::1]:' + APIGUARD_PORT.ToString + '/api/monitor/data').StatusCode);
end;

procedure TAPIGuardMiddlewareTests.Monitor_ForwardedFromLocalProxy_Denied;

  function GetForwarded(const APath: string): Integer;
  var
    lReq: IHTTPRequest;
  begin
    lReq := fClient.GetRequest('GET', 'http://127.0.0.1:' + APIGUARD_PORT.ToString + APath);
    lReq.AddHeader('X-Forwarded-For', '203.0.113.9');
    Result := fClient.Execute(lReq).StatusCode;
  end;

begin
  // a reverse proxy on the same host: the peer is loopback, the client is not
  fGuard.Rule(GUARD_RULE_SCAN).Enabled := False; // three 404s would block 127.0.0.1; Setup re-enables it
  Assert.AreEqual(404, GetForwarded('/api-guard/status'), 'untrusted proxy, status');
  Assert.AreEqual(404, GetForwarded('/api/monitor/data'), 'untrusted proxy, monitor path');
  MVCTrustProxyForwardedHeaders := True;
  try
    Assert.AreEqual(404, GetForwarded('/api-guard/status'), 'trusted proxy, client not a monitor');
  finally
    MVCTrustProxyForwardedHeaders := False;
  end;
end;

procedure TAPIGuardMiddlewareTests.NormalizeIP(const AInput, AExpected: string);
begin
  Assert.AreEqual(AExpected, APIGuardNormalizeIP(AInput));
end;

procedure TAPIGuardMiddlewareTests.UnknownRule_Raises;
begin
  Assert.WillRaise(
    procedure
    begin
      fGuard.Rule('does_not_exist');
    end, EMVCException);
end;

initialization

TDUnitX.RegisterTestFixture(TAPIGuardMiddlewareTests);

end.
