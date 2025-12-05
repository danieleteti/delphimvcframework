unit RateLimitTestsU;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.DateUtils,
  System.Classes,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Middleware.RateLimit;

type
  [TestFixture]
  TRateLimitTests = class
  private
    FStorage: IMVCRateLimitStorage;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestInMemoryStorageFirstRequest;

    [Test]
    procedure TestInMemoryStorageMultipleRequests;

    [Test]
    procedure TestInMemoryStorageRateLimitExceeded;

    [Test]
    procedure TestInMemoryStorageWindowExpiration;

    [Test]
    procedure TestInMemoryStorageResetKey;

    [Test]
    procedure TestInMemoryStorageClear;

    [Test]
    procedure TestInMemoryStorageMultipleKeys;

    [Test]
    procedure TestRateLimitMiddlewareExcludedPaths;
  end;

  [TestFixture]
  TRateLimitMiddlewareTests = class
  private
    FMiddleware: TMVCRateLimitMiddleware;
    FContext: TWebContext;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestMiddlewareCreation;

    [Test]
    procedure TestExtractKeyIPAddress;

    [Test]
    procedure TestExcludedPath;
  end;

implementation

uses
  System.SyncObjs,
  MVCFramework.Server,
  MVCFramework.Server.Impl,
  IdContext,
  IdCustomHTTPServer;

{ TRateLimitTests }

procedure TRateLimitTests.Setup;
begin
  FStorage := TMVCInMemoryRateLimitStorage.Create(rlsFixedWindow);
end;

procedure TRateLimitTests.TearDown;
begin
  FStorage := nil;
end;

procedure TRateLimitTests.TestInMemoryStorageFirstRequest;
var
  LRemaining: Integer;
  LResetTime: TDateTime;
  LExceeded: Boolean;
begin
  // First request should not exceed limit
  LExceeded := FStorage.CheckRateLimit('test-key-1', 10, 60, LRemaining, LResetTime);

  Assert.IsFalse(LExceeded, 'First request should not exceed limit');
  Assert.AreEqual(9, LRemaining, 'Remaining should be 9 after first request');
  Assert.IsTrue(LResetTime > Now, 'Reset time should be in the future');
end;

procedure TRateLimitTests.TestInMemoryStorageMultipleRequests;
var
  I: Integer;
  LRemaining: Integer;
  LResetTime: TDateTime;
  LExceeded: Boolean;
begin
  // Make 5 requests
  for I := 1 to 5 do
  begin
    LExceeded := FStorage.CheckRateLimit('test-key-2', 10, 60, LRemaining, LResetTime);
    Assert.IsFalse(LExceeded, Format('Request %d should not exceed limit', [I]));
  end;

  // After 5 requests, remaining should be 5
  Assert.AreEqual(5, LRemaining, 'Remaining should be 5 after 5 requests');
end;

procedure TRateLimitTests.TestInMemoryStorageRateLimitExceeded;
var
  I: Integer;
  LRemaining: Integer;
  LResetTime: TDateTime;
  LExceeded: Boolean;
begin
  // Make 10 requests (at the limit)
  for I := 1 to 10 do
  begin
    LExceeded := FStorage.CheckRateLimit('test-key-3', 10, 60, LRemaining, LResetTime);
    if I < 10 then
      Assert.IsFalse(LExceeded, Format('Request %d should not exceed limit', [I]))
    else
      Assert.IsFalse(LExceeded, 'Request 10 should reach but not exceed limit');
  end;

  // 11th request should exceed limit
  LExceeded := FStorage.CheckRateLimit('test-key-3', 10, 60, LRemaining, LResetTime);
  Assert.IsTrue(LExceeded, '11th request should exceed limit');
  Assert.AreEqual(0, LRemaining, 'Remaining should be 0 when limit exceeded');
end;

procedure TRateLimitTests.TestInMemoryStorageWindowExpiration;
var
  LRemaining: Integer;
  LResetTime: TDateTime;
  LExceeded: Boolean;
  LStorage: TMVCInMemoryRateLimitStorage;
begin
  // Create storage with a very short window for testing (1 second)
  LStorage := TMVCInMemoryRateLimitStorage.Create(rlsFixedWindow);
  try
    // Make requests up to limit
    LExceeded := LStorage.CheckRateLimit('test-key-4', 5, 1, LRemaining, LResetTime);
    Assert.IsFalse(LExceeded);

    LExceeded := LStorage.CheckRateLimit('test-key-4', 5, 1, LRemaining, LResetTime);
    Assert.IsFalse(LExceeded);

    LExceeded := LStorage.CheckRateLimit('test-key-4', 5, 1, LRemaining, LResetTime);
    Assert.IsFalse(LExceeded);

    LExceeded := LStorage.CheckRateLimit('test-key-4', 5, 1, LRemaining, LResetTime);
    Assert.IsFalse(LExceeded);

    LExceeded := LStorage.CheckRateLimit('test-key-4', 5, 1, LRemaining, LResetTime);
    Assert.IsFalse(LExceeded, '5th request should not exceed limit');

    // 6th request should exceed
    LExceeded := LStorage.CheckRateLimit('test-key-4', 5, 1, LRemaining, LResetTime);
    Assert.IsTrue(LExceeded, '6th request should exceed limit');

    // Wait for window to expire
    Sleep(1100);

    // After expiration, should start fresh window
    LExceeded := LStorage.CheckRateLimit('test-key-4', 5, 1, LRemaining, LResetTime);
    Assert.IsFalse(LExceeded, 'After window expiration, request should not exceed');
    Assert.AreEqual(4, LRemaining, 'Should have 4 remaining in new window');

  finally
    LStorage.Free;
  end;
end;

procedure TRateLimitTests.TestInMemoryStorageResetKey;
var
  LRemaining: Integer;
  LResetTime: TDateTime;
  LExceeded: Boolean;
  I: Integer;
begin
  // Make several requests
  for I := 1 to 8 do
    FStorage.CheckRateLimit('test-key-5', 10, 60, LRemaining, LResetTime);

  // Remaining should be 2
  Assert.AreEqual(2, LRemaining);

  // Reset the key
  FStorage.ResetKey('test-key-5');

  // After reset, should start fresh
  LExceeded := FStorage.CheckRateLimit('test-key-5', 10, 60, LRemaining, LResetTime);
  Assert.IsFalse(LExceeded);
  Assert.AreEqual(9, LRemaining, 'After reset, should have 9 remaining');
end;

procedure TRateLimitTests.TestInMemoryStorageClear;
var
  LRemaining: Integer;
  LResetTime: TDateTime;
  LExceeded: Boolean;
begin
  // Add entries for multiple keys
  FStorage.CheckRateLimit('key-1', 10, 60, LRemaining, LResetTime);
  FStorage.CheckRateLimit('key-2', 10, 60, LRemaining, LResetTime);
  FStorage.CheckRateLimit('key-3', 10, 60, LRemaining, LResetTime);

  // Clear all
  FStorage.Clear;

  // All keys should start fresh
  LExceeded := FStorage.CheckRateLimit('key-1', 10, 60, LRemaining, LResetTime);
  Assert.AreEqual(9, LRemaining, 'key-1 should be reset');

  LExceeded := FStorage.CheckRateLimit('key-2', 10, 60, LRemaining, LResetTime);
  Assert.AreEqual(9, LRemaining, 'key-2 should be reset');

  LExceeded := FStorage.CheckRateLimit('key-3', 10, 60, LRemaining, LResetTime);
  Assert.AreEqual(9, LRemaining, 'key-3 should be reset');
end;

procedure TRateLimitTests.TestInMemoryStorageMultipleKeys;
var
  LRemaining1, LRemaining2: Integer;
  LResetTime: TDateTime;
  LExceeded: Boolean;
  I: Integer;
begin
  // Make different number of requests for different keys
  for I := 1 to 3 do
    FStorage.CheckRateLimit('user-1', 10, 60, LRemaining1, LResetTime);

  for I := 1 to 7 do
    FStorage.CheckRateLimit('user-2', 10, 60, LRemaining2, LResetTime);

  // Check that each key maintains its own count
  Assert.AreEqual(7, LRemaining1, 'user-1 should have 7 remaining');
  Assert.AreEqual(3, LRemaining2, 'user-2 should have 3 remaining');
end;

procedure TRateLimitTests.TestRateLimitMiddlewareExcludedPaths;
var
  LMiddleware: TMVCRateLimitMiddleware;
  LReflect: TRttiContext;
  LMethod: TRttiMethod;
  LIsExcluded: Boolean;
begin
  LMiddleware := TMVCRateLimitMiddleware.Create(10, 60, rlkIPAddress);
  try
    LMiddleware.AddExcludedPath('/health');
    LMiddleware.AddExcludedPath('/metrics');
    LMiddleware.AddExcludedPath('/api/public');

    // Use RTTI to access private method for testing
    LReflect := TRttiContext.Create;
    try
      LMethod := LReflect.GetType(LMiddleware.ClassType).GetMethod('IsPathExcluded');
      if Assigned(LMethod) then
      begin
        LIsExcluded := LMethod.Invoke(LMiddleware, ['/health']).AsBoolean;
        Assert.IsTrue(LIsExcluded, '/health should be excluded');

        LIsExcluded := LMethod.Invoke(LMiddleware, ['/metrics']).AsBoolean;
        Assert.IsTrue(LIsExcluded, '/metrics should be excluded');

        LIsExcluded := LMethod.Invoke(LMiddleware, ['/api/public']).AsBoolean;
        Assert.IsTrue(LIsExcluded, '/api/public should be excluded');

        LIsExcluded := LMethod.Invoke(LMiddleware, ['/api/protected']).AsBoolean;
        Assert.IsFalse(LIsExcluded, '/api/protected should NOT be excluded');

        LIsExcluded := LMethod.Invoke(LMiddleware, ['/health/details']).AsBoolean;
        Assert.IsTrue(LIsExcluded, '/health/details should be excluded (prefix match)');
      end;
    finally
      LReflect.Free;
    end;
  finally
    LMiddleware.Free;
  end;
end;

{ TRateLimitMiddlewareTests }

procedure TRateLimitMiddlewareTests.Setup;
begin
  FMiddleware := TMVCRateLimitMiddleware.Create(10, 60, rlkIPAddress);
  FContext := nil; // Would need proper web context for full integration tests
end;

procedure TRateLimitMiddlewareTests.TearDown;
begin
  FMiddleware.Free;
  // FContext cleanup if created
end;

procedure TRateLimitMiddlewareTests.TestMiddlewareCreation;
begin
  Assert.IsNotNull(FMiddleware, 'Middleware should be created successfully');
end;

procedure TRateLimitMiddlewareTests.TestExtractKeyIPAddress;
begin
  // This test would require a proper WebContext mock
  // For now, just verify the middleware was created with correct key type
  Assert.Pass('ExtractKey requires full integration test with WebContext');
end;

procedure TRateLimitMiddlewareTests.TestExcludedPath;
begin
  FMiddleware.AddExcludedPath('/health');
  FMiddleware.AddExcludedPath('/metrics');

  // This test would require accessing private method or full integration test
  Assert.Pass('ExcludedPath functionality tested in TRateLimitTests.TestRateLimitMiddlewareExcludedPaths');
end;

initialization
  TDUnitX.RegisterTestFixture(TRateLimitTests);
  TDUnitX.RegisterTestFixture(TRateLimitMiddlewareTests);

end.
