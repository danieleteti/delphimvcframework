// ***************************************************************************
//
// Delphi MVC Framework - JWT Asymmetric Auth Sample
//
// WebModule configuration with RS256 JWT middleware.
//
// KEY DIFFERENCE FROM HS256:
//   - HS256: TMVCJWTAuthenticationMiddleware.Create(Handler, Claims, 'shared_secret')
//   - RS256: TMVCJWTAuthenticationMiddleware.Create(Handler, Claims, TRSAJWTSigner)
//
// The RSA signer uses a PRIVATE key to sign tokens and a PUBLIC key to verify them.
// This means the verification key (public) can be shared freely without compromising
// the ability to forge tokens - only the private key can create valid signatures.
//
// ***************************************************************************

unit WebModuleU;

interface

uses
  System.Classes,
  Web.HTTPApp,
  MVCFramework;

type
  TMyWebModule = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    fMVC: TMVCEngine;
  end;

var
  WebModuleClass: TComponentClass = TMyWebModule;

implementation

{$R *.dfm}

uses
  System.SysUtils,
  System.IOUtils,
  System.DateUtils,
  MVCFramework.Commons,
  MVCFramework.JWT,          // IJWTSigner
  MVCFramework.JWT.RSA,      // TRSAJWTSigner, JWT_RS256 (requires TaurusTLS)
  MVCFramework.Middleware.JWT,
  MVCFramework.Middleware.StaticFiles,
  MVCFramework.Middleware.CORS,
  AuthHandlerU,
  ControllerU;

procedure TMyWebModule.WebModuleCreate(Sender: TObject);
var
  LKeysPath: string;
  LSigner: IJWTSigner;
begin
  fMVC := TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.DefaultContentType] := TMVCMediaType.APPLICATION_JSON;
      Config[TMVCConfigKey.LoadSystemControllers] := 'true';
    end);

  // ============================================================
  // RSA JWT Signer Setup
  //
  // Load RSA keys from PEM files.
  // - Private key: used to SIGN tokens (keep this secret!)
  // - Public key: used to VERIFY tokens (can be shared)
  //
  // In a microservice architecture, the auth service holds the
  // private key, and other services only need the public key.
  // ============================================================
  LKeysPath := TPath.Combine(AppPath, 'keys');

  LSigner := TRSAJWTSigner.CreateFromFiles(
    JWT_RS256,                                         // Algorithm: RS256
    TPath.Combine(LKeysPath, 'private.pem'),           // Private key for signing
    TPath.Combine(LKeysPath, 'public.pem')             // Public key for verification
  );

  // Controllers
  fMVC.AddController(TApiController);

  // Middleware
  fMVC.AddMiddleware(TMVCCORSMiddleware.Create);
  fMVC.AddMiddleware(TMVCStaticFilesMiddleware.Create('/', TPath.Combine(AppPath, 'www'), 'index.html'));

  // JWT Authentication with RSA signer
  // Compare with the HS256 version:
  //   TMVCJWTAuthenticationMiddleware.Create(Handler, Claims, 'my_secret', ...)
  // Here we pass the RSA signer instead of a shared secret:
  fMVC.AddMiddleware(TMVCJWTAuthenticationMiddleware.Create(
    TSampleAuthHandler.Create,
    procedure(const JWT: TJWT)
    begin
      JWT.Claims.Issuer := 'dmvc-jwt-rsa-sample';
      JWT.Claims.ExpirationTime := Now + OneHour;
      JWT.Claims.IssuedAt := Now;
      JWT.CustomClaims['scope'] := 'api';
    end,
    LSigner,            // <-- RSA signer instead of shared secret
    '/api/login',
    [TJWTCheckableClaim.ExpirationTime, TJWTCheckableClaim.IssuedAt],
    300                 // 5 minutes leeway
  ));
end;

procedure TMyWebModule.WebModuleDestroy(Sender: TObject);
begin
  fMVC.Free;
end;

end.
