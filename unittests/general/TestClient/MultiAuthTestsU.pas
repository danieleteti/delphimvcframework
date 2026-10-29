unit MultiAuthTestsU;

// Issue #643: Basic and JWT authentication on the same engine. Both middleware
// ask the handler's OnRequest whether a request needs authentication, so a
// handler that looks at the Authorization scheme lets each middleware take only
// the requests meant for it: Basic credentials go to the Basic middleware, a
// Bearer token to the JWT one.

interface

{$WARN SYMBOL_DEPRECATED OFF} // the middleware are tested on purpose

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Net.HttpClient,
  System.Net.URLClient,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Server.Intf;

const
  MULTIAUTH_PORT = 8899;

type
  [MVCPath('/api')]
  TMultiAuthController = class(TMVCController)
  public
    [MVCPath('/secure')]
    [MVCHTTPMethod([httpGET])]
    function Secure: String;
  end;

  // IsBasic = True: the Basic middleware's handler, it wants only "Authorization: Basic".
  // IsBasic = False: the JWT middleware's handler, it wants everything else.
  TMultiAuthHandler = class(TInterfacedObject, IMVCAuthenticationHandler)
  private
    fIsBasic: Boolean;
  protected
    procedure OnRequest(const AContext: TWebContext; const AControllerQualifiedClassName: string;
      const AActionName: string; var AAuthenticationRequired: Boolean);
    procedure OnAuthentication(const AContext: TWebContext; const AUserName: string;
      const APassword: string; AUserRoles: TList<string>; var AIsValid: Boolean;
      const ASessionData: TDictionary<string, string>);
    procedure OnAuthorization(const AContext: TWebContext; AUserRoles: TList<string>;
      const AControllerQualifiedClassName: string; const AActionName: string;
      var AIsAuthorized: Boolean);
  public
    constructor Create(const aIsBasic: Boolean);
  end;

  [TestFixture]
  TMultiAuthTests = class
  private
    fServer: IMVCServer;
    fEngine: TMVCEngine;
    function Get(const aAuthorization: string): IHTTPResponse;
  protected
    function Port: Integer; virtual;
    procedure AddAuthentication(const AEngine: TMVCEngine); virtual;
  public
    [SetupFixture]
    procedure SetupFixture;
    [TearDownFixture]
    procedure TeardownFixture;
    [Test]
    procedure NoCredentialsIsRefused;
    [Test]
    procedure BasicCredentialsAreAccepted;
    [Test]
    procedure WrongBasicCredentialsAreRefused;
    [Test]
    procedure BearerTokenIsAccepted;
    [Test]
    procedure WrongBearerTokenIsRefused;
    [Test]
    procedure BearerAfterBasicSessionOnTheSameClientIsAccepted;
  end;

  // the same tests with the filters: MemorySessionHTTPFilter, BasicAuthenticationBundle and JWTAuthenticationBundle
  [TestFixture]
  TMultiAuthFilterTests = class(TMultiAuthTests)
  protected
    function Port: Integer; override;
    procedure AddAuthentication(const AEngine: TMVCEngine); override;
  end;

implementation

uses
  System.NetEncoding,
  MVCFramework.Filters,
  MVCFramework.Filters.Authentication,
  MVCFramework.Middleware.Authentication,
  MVCFramework.Middleware.JWT,
  MVCFramework.Middleware.Session,
  MVCFramework.JWT,
  JsonDataObjects,
  MVCFramework.Server.Factory;

const
  USER = 'multi';

function BasicHeader(const aUser, aPassword: string): string;
begin
  Result := 'Basic ' + TNetEncoding.Base64.Encode(aUser + ':' + aPassword);
end;

// the token of a login answer {"token": "..."}
function TokenOf(const AResponse: IHTTPResponse): string;
var
  lJSON: TJsonObject;
begin
  lJSON := StrToJSONObject(AResponse.ContentAsString);
  try
    Result := lJSON.S['token'];
  finally
    lJSON.Free;
  end;
end;

{ TMultiAuthController }

function TMultiAuthController.Secure: String;
begin
  Result := 'ok';
end;

{ TMultiAuthHandler }

constructor TMultiAuthHandler.Create(const aIsBasic: Boolean);
begin
  inherited Create;
  fIsBasic := aIsBasic;
end;

procedure TMultiAuthHandler.OnRequest(const AContext: TWebContext;
  const AControllerQualifiedClassName: string; const AActionName: string;
  var AAuthenticationRequired: Boolean);
var
  lIsBasic: Boolean;
begin
  lIsBasic := AContext.Request.Headers['Authorization'].StartsWith('Basic ', True);
  AAuthenticationRequired := lIsBasic = fIsBasic;
end;

procedure TMultiAuthHandler.OnAuthentication(const AContext: TWebContext;
  const AUserName: string; const APassword: string; AUserRoles: TList<string>;
  var AIsValid: Boolean; const ASessionData: TDictionary<string, string>);
begin
  AIsValid := (AUserName = USER) and (APassword = USER);
end;

procedure TMultiAuthHandler.OnAuthorization(const AContext: TWebContext;
  AUserRoles: TList<string>; const AControllerQualifiedClassName: string;
  const AActionName: string; var AIsAuthorized: Boolean);
begin
  AIsAuthorized := True;
end;

{ TMultiAuthTests }

const
  MULTIAUTH_SECRET = 'multi_auth_tests_secret_not_used_to_sign_anything_3d9a';

procedure MultiAuthClaims(const AJWT: TJWT);
begin
  AJWT.Claims.ExpirationTime := Now + 1;
end;

function TMultiAuthTests.Port: Integer;
begin
  Result := MULTIAUTH_PORT;
end;

procedure TMultiAuthTests.AddAuthentication(const AEngine: TMVCEngine);
begin
  AEngine.AddMiddleware(UseMemorySessionMiddleware);
  AEngine.AddMiddleware(TMVCBasicAuthenticationMiddleware.Create(TMultiAuthHandler.Create(True)));
  AEngine.AddMiddleware(TMVCJWTAuthenticationMiddleware.Create(
    TMultiAuthHandler.Create(False),
    MultiAuthClaims,
    MULTIAUTH_SECRET,
    '/login',
    [TJWTCheckableClaim.ExpirationTime],
    300));
end;

procedure TMultiAuthTests.SetupFixture;
var
  lEngine: TMVCEngine;
begin
  lEngine := TMVCEngine.Create;
  fEngine := lEngine;
  lEngine.AddController(TMultiAuthController);
  AddAuthentication(lEngine);
  fServer := TMVCServerFactory.CreateIndyDirect(lEngine);
  fServer.Listen(Port);
end;

{ TMultiAuthFilterTests }

function TMultiAuthFilterTests.Port: Integer;
begin
  Result := MULTIAUTH_PORT + 63; // 8962
end;

procedure TMultiAuthFilterTests.AddAuthentication(const AEngine: TMVCEngine);
begin
  AEngine.Use(MemorySessionHTTPFilter(0));
  AEngine.Use(BasicAuthenticationBundle(TMultiAuthHandler.Create(True)));
  AEngine.Use(JWTAuthenticationBundle(TMultiAuthHandler.Create(False), MultiAuthClaims, MULTIAUTH_SECRET, '/login'));
end;

procedure TMultiAuthTests.TeardownFixture;
begin
  fServer.Stop;
  fServer := nil;
  FreeAndNil(fEngine); // the server does not own the engine
end;

function TMultiAuthTests.Get(const aAuthorization: string): IHTTPResponse;
var
  lHeaders: TNetHeaders;
  lClient: THTTPClient;
begin
  if aAuthorization.IsEmpty then
    lHeaders := nil
  else
    lHeaders := [TNameValuePair.Create('Authorization', aAuthorization)];
  // a client of its own: the Basic middleware opens a session, a shared cookie would carry the login over
  lClient := THTTPClient.Create;
  try
    Result := lClient.Get('http://localhost:' + Port.ToString + '/api/secure', nil, lHeaders);
  finally
    lClient.Free;
  end;
end;

procedure TMultiAuthTests.NoCredentialsIsRefused;
begin
  Assert.AreEqual(401, Get('').StatusCode);
end;

procedure TMultiAuthTests.BasicCredentialsAreAccepted;
var
  lRes: IHTTPResponse;
begin
  lRes := Get(BasicHeader(USER, USER));
  Assert.AreEqual(200, lRes.StatusCode, lRes.ContentAsString);
  Assert.AreEqual('ok', lRes.ContentAsString);
end;

procedure TMultiAuthTests.WrongBasicCredentialsAreRefused;
begin
  Assert.AreEqual(401, Get(BasicHeader(USER, 'wrong')).StatusCode);
end;

procedure TMultiAuthTests.BearerTokenIsAccepted;
var
  lLogin, lRes: IHTTPResponse;
  lToken: string;
  lClient: THTTPClient;
  lBody: TStringStream;
begin
  // the JWT middleware issues the token on /login, from Basic credentials
  lClient := THTTPClient.Create;
  lBody := TStringStream.Create('');
  try
    lLogin := lClient.Post('http://localhost:' + Port.ToString + '/login', lBody, nil,
      [TNameValuePair.Create('Authorization', BasicHeader(USER, USER))]);
  finally
    lBody.Free;
    lClient.Free;
  end;
  Assert.AreEqual(200, lLogin.StatusCode, lLogin.ContentAsString);
  lToken := TokenOf(lLogin);
  lRes := Get('Bearer ' + lToken);
  Assert.AreEqual(200, lRes.StatusCode, lRes.ContentAsString);
  Assert.AreEqual('ok', lRes.ContentAsString);
end;

procedure TMultiAuthTests.WrongBearerTokenIsRefused;
var
  lRes: IHTTPResponse;
begin
  lRes := Get('Bearer not.a.token');
  Assert.AreEqual(401, lRes.StatusCode, lRes.ContentAsString);
end;

procedure TMultiAuthTests.BearerAfterBasicSessionOnTheSameClientIsAccepted;
var
  lRes: IHTTPResponse;
  lToken: string;
  lClient: THTTPClient;
  lBody: TStringStream;
  lUrl: string;
begin
  // one client keeps the session cookie the Basic middleware opens: the token must still be read
  lUrl := 'http://localhost:' + Port.ToString;
  lClient := THTTPClient.Create;
  lBody := TStringStream.Create('');
  try
    lRes := lClient.Post(lUrl + '/login', lBody, nil, [TNameValuePair.Create('Authorization', BasicHeader(USER, USER))]);
    Assert.AreEqual(200, lRes.StatusCode, lRes.ContentAsString);
    lToken := TokenOf(lRes);
    lRes := lClient.Get(lUrl + '/api/secure', nil, [TNameValuePair.Create('Authorization', BasicHeader(USER, USER))]);
    Assert.AreEqual(200, lRes.StatusCode, 'Basic: ' + lRes.ContentAsString);
    lRes := lClient.Get(lUrl + '/api/secure', nil, [TNameValuePair.Create('Authorization', 'Bearer ' + lToken)]);
    Assert.AreEqual(200, lRes.StatusCode, 'Bearer after Basic: ' + lRes.ContentAsString);
    Assert.AreEqual('ok', lRes.ContentAsString);
  finally
    lBody.Free;
    lClient.Free;
  end;
end;

initialization

TDUnitX.RegisterTestFixture(TMultiAuthTests);
TDUnitX.RegisterTestFixture(TMultiAuthFilterTests);

end.
