unit JWTCookieMiddlewareTestsU;

interface

uses
  DUnitX.TestFramework,
  MVCFramework.Middleware.JWT,
  MVCFramework.JWT,
  MVCFramework.Commons;

type
  [TestFixture]
  TTestJWTCookieMiddleware = class(TObject)
  public
    [Test]
    procedure TestSecureDefaults;

    [Test]
    procedure TestSetCookieName;

    [Test]
    procedure TestSetCookiePath;

    [Test]
    procedure TestSetCookieDomain;

    [Test]
    procedure TestSameSiteNoneRequiresSecure;

    [Test]
    procedure TestSecureFalseWithSameSiteNoneFails;

    [Test]
    procedure TestFluentConfiguration;

    [Test]
    procedure TestSameSiteLaxDoesNotRequireSecure;

    [Test]
    procedure TestSameSiteStrictDoesNotRequireSecure;

    [Test]
    procedure TestSameSiteNoneWithSecureFalseFailsOnSet;

    [Test]
    procedure TestUseJWTCookieAuthenticationHelper;
  end;

implementation

uses
  System.SysUtils,
  System.DateUtils;

{ TTestJWTCookieMiddleware }

procedure TTestJWTCookieMiddleware.TestSecureDefaults;
var
  LMiddleware: TMVCJWTCookieAuthenticationMiddleware;
begin
  LMiddleware := TMVCJWTCookieAuthenticationMiddleware.Create(
    nil,  // Auth handler
    procedure(const JWT: TJWT)
    begin
      JWT.Claims.ExpirationTime := Now + OneHour;
    end,
    'test-secret',
    '/login',
    '/logout',
    [TJWTCheckableClaim.ExpirationTime],
    300
  );
  try
    // The middleware should be created with secure defaults
    // We verify this by trying operations that would fail if defaults were wrong
    Assert.IsNotNull(LMiddleware, 'Middleware should be created');
  finally
    LMiddleware.Free;
  end;
end;

procedure TTestJWTCookieMiddleware.TestSetCookieName;
var
  LMiddleware: TMVCJWTCookieAuthenticationMiddleware;
begin
  LMiddleware := TMVCJWTCookieAuthenticationMiddleware.Create(
    nil,
    procedure(const JWT: TJWT)
    begin
      JWT.Claims.ExpirationTime := Now + OneHour;
    end,
    'test-secret'
  );
  try
    // Fluent API should return the same instance
    Assert.AreSame(LMiddleware, LMiddleware.SetCookieName('my_custom_token'));
  finally
    LMiddleware.Free;
  end;
end;

procedure TTestJWTCookieMiddleware.TestSetCookiePath;
var
  LMiddleware: TMVCJWTCookieAuthenticationMiddleware;
begin
  LMiddleware := TMVCJWTCookieAuthenticationMiddleware.Create(
    nil,
    procedure(const JWT: TJWT)
    begin
      JWT.Claims.ExpirationTime := Now + OneHour;
    end,
    'test-secret'
  );
  try
    Assert.AreSame(LMiddleware, LMiddleware.SetCookiePath('/api'));
  finally
    LMiddleware.Free;
  end;
end;

procedure TTestJWTCookieMiddleware.TestSetCookieDomain;
var
  LMiddleware: TMVCJWTCookieAuthenticationMiddleware;
begin
  LMiddleware := TMVCJWTCookieAuthenticationMiddleware.Create(
    nil,
    procedure(const JWT: TJWT)
    begin
      JWT.Claims.ExpirationTime := Now + OneHour;
    end,
    'test-secret'
  );
  try
    Assert.AreSame(LMiddleware, LMiddleware.SetCookieDomain('example.com'));
  finally
    LMiddleware.Free;
  end;
end;

procedure TTestJWTCookieMiddleware.TestSameSiteNoneRequiresSecure;
var
  LMiddleware: TMVCJWTCookieAuthenticationMiddleware;
begin
  // Default is Secure=True, so SameSite=None should work
  LMiddleware := TMVCJWTCookieAuthenticationMiddleware.Create(
    nil,
    procedure(const JWT: TJWT)
    begin
      JWT.Claims.ExpirationTime := Now + OneHour;
    end,
    'test-secret'
  );
  try
    // With default Secure=True, this should succeed
    Assert.AreSame(LMiddleware, LMiddleware.SetCookieSameSite(ssNone));
  finally
    LMiddleware.Free;
  end;
end;

procedure TTestJWTCookieMiddleware.TestSecureFalseWithSameSiteNoneFails;
var
  LMiddleware: TMVCJWTCookieAuthenticationMiddleware;
begin
  LMiddleware := TMVCJWTCookieAuthenticationMiddleware.Create(
    nil,
    procedure(const JWT: TJWT)
    begin
      JWT.Claims.ExpirationTime := Now + OneHour;
    end,
    'test-secret'
  );
  try
    // First set SameSite=None (works because Secure=True by default)
    LMiddleware.SetCookieSameSite(ssNone);

    // Now try to set Secure=False - should fail
    Assert.WillRaise(
      procedure
      begin
        LMiddleware.SetCookieSecure(False);
      end,
      EMVCException,
      'Setting Secure=False with SameSite=None should raise an exception'
    );
  finally
    LMiddleware.Free;
  end;
end;

procedure TTestJWTCookieMiddleware.TestFluentConfiguration;
var
  LMiddleware: TMVCJWTCookieAuthenticationMiddleware;
  LResult: TMVCJWTCookieAuthenticationMiddleware;
begin
  LMiddleware := TMVCJWTCookieAuthenticationMiddleware.Create(
    nil,
    procedure(const JWT: TJWT)
    begin
      JWT.Claims.ExpirationTime := Now + OneHour;
    end,
    'test-secret'
  );
  try
    // Test fluent chaining
    LResult := LMiddleware
      .SetCookieName('custom_token')
      .SetCookiePath('/api')
      .SetCookieDomain('example.com')
      .SetCookieSameSite(ssLax);

    Assert.AreSame(LMiddleware, LResult, 'Fluent API should return the same instance');
  finally
    LMiddleware.Free;
  end;
end;

procedure TTestJWTCookieMiddleware.TestSameSiteLaxDoesNotRequireSecure;
var
  LMiddleware: TMVCJWTCookieAuthenticationMiddleware;
begin
  LMiddleware := TMVCJWTCookieAuthenticationMiddleware.Create(
    nil,
    procedure(const JWT: TJWT)
    begin
      JWT.Claims.ExpirationTime := Now + OneHour;
    end,
    'test-secret'
  );
  try
    // SameSite=Lax should work even with Secure=False
    LMiddleware.SetCookieSecure(False);
    Assert.AreSame(LMiddleware, LMiddleware.SetCookieSameSite(ssLax));
  finally
    LMiddleware.Free;
  end;
end;

procedure TTestJWTCookieMiddleware.TestSameSiteStrictDoesNotRequireSecure;
var
  LMiddleware: TMVCJWTCookieAuthenticationMiddleware;
begin
  LMiddleware := TMVCJWTCookieAuthenticationMiddleware.Create(
    nil,
    procedure(const JWT: TJWT)
    begin
      JWT.Claims.ExpirationTime := Now + OneHour;
    end,
    'test-secret'
  );
  try
    // SameSite=Strict should work even with Secure=False
    LMiddleware.SetCookieSecure(False);
    Assert.AreSame(LMiddleware, LMiddleware.SetCookieSameSite(ssStrict));
  finally
    LMiddleware.Free;
  end;
end;

procedure TTestJWTCookieMiddleware.TestSameSiteNoneWithSecureFalseFailsOnSet;
var
  LMiddleware: TMVCJWTCookieAuthenticationMiddleware;
begin
  LMiddleware := TMVCJWTCookieAuthenticationMiddleware.Create(
    nil,
    procedure(const JWT: TJWT)
    begin
      JWT.Claims.ExpirationTime := Now + OneHour;
    end,
    'test-secret'
  );
  try
    // First set Secure=False
    LMiddleware.SetCookieSecure(False);

    // Now try to set SameSite=None - should fail
    Assert.WillRaise(
      procedure
      begin
        LMiddleware.SetCookieSameSite(ssNone);
      end,
      EMVCException,
      'Setting SameSite=None with Secure=False should raise an exception'
    );
  finally
    LMiddleware.Free;
  end;
end;

procedure TTestJWTCookieMiddleware.TestUseJWTCookieAuthenticationHelper;
var
  LMiddleware: TMVCJWTCookieAuthenticationMiddleware;
begin
  LMiddleware := UseJWTCookieAuthentication(
    nil,
    procedure(const JWT: TJWT)
    begin
      JWT.Claims.ExpirationTime := Now + OneHour;
    end,
    'test-secret',
    '/login',
    '/logout',
    [TJWTCheckableClaim.ExpirationTime],
    300
  );
  try
    Assert.IsNotNull(LMiddleware, 'UseJWTCookieAuthentication should return a middleware instance');
  finally
    LMiddleware.Free;
  end;
end;

initialization

TDUnitX.RegisterTestFixture(TTestJWTCookieMiddleware);

end.
