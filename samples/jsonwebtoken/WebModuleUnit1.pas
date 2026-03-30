unit WebModuleUnit1;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework,
  MVCFramework.Commons;

type
  TWebModule1 = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);

  private
    MVC: TMVCEngine;

  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

{$R *.dfm}

// ============================================================
// Uncomment the following line to switch from HS256 to RS256.
// Requires TaurusTLS in the search path and OpenSSL 1.1.1+ DLLs.
//
// Generate test keys with:
//   openssl genrsa -out keys\private.pem 2048
//   openssl rsa -in keys\private.pem -pubout -out keys\public.pem
// ============================================================

{$DEFINE USE_RSA_JWT}

uses
  AppControllerU,
  System.Generics.Collections,
  AuthenticationU,
  MVCFramework.Middleware.JWT,
  MVCFramework.Middleware.StaticFiles,
  MVCFramework.JWT,
{$IFDEF USE_RSA_JWT}
  System.IOUtils,
  MVCFramework.JWT.RSA,  // <-- requires TaurusTLS + OpenSSL 1.1.1+
{$ENDIF}
  System.DateUtils;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
var
  lClaimsSetup: TJWTClaimsSetup;
{$IFDEF USE_RSA_JWT}
  lSigner: IJWTSigner;
{$ENDIF}
begin
  lClaimsSetup := procedure(const JWT: TJWT)
    begin
      JWT.Claims.Issuer := 'Delphi MVC Framework JWT Middleware Sample';
      if TMVCWebRequest(JWT.Data).QueryStringParamExists('rememberme') then
      begin
        JWT.Claims.ExpirationTime := Now + (OneHour * 10); // valid for 10 hour
      end
      else
      begin
        JWT.Claims.ExpirationTime := Now + OneHour; // valid for 1 hour
      end;
      JWT.Claims.NotBefore := Now - OneMinute * 5; // valid since 5 minutes ago
      JWT.Claims.IssuedAt := Now;
      JWT.CustomClaims['mycustomvalue'] := 'hello there';
    end;

  MVC := TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.DefaultContentType] := TMVCMediaType.APPLICATION_JSON;
    end);

{$IFDEF USE_RSA_JWT}
  // RSA asymmetric JWT (RS256)
  // Signs tokens with private key, verifies with public key.
  // You can also use: JWT_PS256 (RSA-PSS), JWT_ES256 (ECDSA), or TEdDSAJWTSigner (Ed25519)
  lSigner := TRSAJWTSigner.CreateFromFiles(
    JWT_RS256,
    TPath.Combine(AppPath, 'keys\private.pem'),
    TPath.Combine(AppPath, 'keys\public.pem'));
{$ENDIF}

  MVC
    .AddController(TApp1MainController)
    .AddController(TAdminController)
    .AddMiddleware(UseJWTMiddleware(TAuthenticationSample.Create,
                                    lClaimsSetup,
{$IFDEF USE_RSA_JWT}
                                    lSigner,           // <-- RSA signer (asymmetric)
{$ELSE}
                                    'mys3cr37',        // <-- shared secret (symmetric)
{$ENDIF}
                                    '/login',
                                    [
                                      TJWTCheckableClaim.ExpirationTime,
                                      TJWTCheckableClaim.NotBefore,
                                      TJWTCheckableClaim.IssuedAt
                                    ], 300))
    .AddMiddleware(UseStaticFilesMiddleware('/static', '..\..\www'));
end;

end.
