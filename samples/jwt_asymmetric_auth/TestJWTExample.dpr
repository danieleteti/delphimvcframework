// ***************************************************************************
//
// Minimal JWT RS256 Example - DelphiMVCFramework
//
// Shows the simplest possible code to:
//   1. Create a JWT token signed with RS256 (private key)
//   2. Verify the token and read claims (public key)
//
// ***************************************************************************

program TestJWTExample;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.IOUtils,
  System.DateUtils,
  MVCFramework.JWT,
  MVCFramework.JWT.RSA;

var
  lKeysPath: string;
  lSigner: IJWTSigner;
  lJWT: TJWT;
  lToken: string;
  lError: string;
begin
  lKeysPath := TPath.Combine(ExtractFilePath(ParamStr(0)), 'keys');

  // ============================================================
  // 1. CREATE AND SIGN A TOKEN (auth server / token issuer)
  // ============================================================

  // Create an RSA signer with private key (for signing) and public key (for verification)
  lSigner := TRSAJWTSigner.CreateFromFiles(
    JWT_RS256,                                       // algorithm
    TPath.Combine(lKeysPath, 'rsa\private.pem'),     // private key (sign)
    TPath.Combine(lKeysPath, 'rsa\public.pem'));      // public key (verify)

  lJWT := TJWT.Create(lSigner);
  try
    // Standard claims
    lJWT.Claims.Issuer := 'my-auth-server';
    lJWT.Claims.Subject := 'user42';
    lJWT.Claims.ExpirationTime := Now + OneHour;
    lJWT.Claims.IssuedAt := Now;

    // Custom claims
    lJWT.CustomClaims['role'] := 'admin';
    lJWT.CustomClaims['department'] := 'engineering';

    // Generate the signed token
    lToken := lJWT.GetToken;
  finally
    lJWT.Free;
  end;

  WriteLn('=== JWT RS256 Example ===');
  WriteLn;
  WriteLn('Token (' , Length(lToken), ' chars):');
  WriteLn(lToken);
  WriteLn;

  // ============================================================
  // 2. VERIFY THE TOKEN AND READ CLAIMS (microservice / API server)
  //    In production, this service would only have the public key.
  // ============================================================

  // A verify-only signer needs just the public key
  lSigner := TRSAJWTSigner.CreateVerifyOnly(
    JWT_RS256,
    TFile.ReadAllText(TPath.Combine(lKeysPath, 'rsa\public.pem')));

  lJWT := TJWT.Create(lSigner);
  try
    lJWT.RegClaimsToChecks := [TJWTCheckableClaim.ExpirationTime, TJWTCheckableClaim.IssuedAt];

    if lJWT.LoadToken(lToken, lError) then
    begin
      WriteLn('Token VALID');
      WriteLn('  Issuer:     ', lJWT.Claims.Issuer);
      WriteLn('  Subject:    ', lJWT.Claims.Subject);
      WriteLn('  Role:       ', lJWT.CustomClaims['role']);
      WriteLn('  Department: ', lJWT.CustomClaims['department']);
      WriteLn('  Expires:    ', DateTimeToStr(lJWT.Claims.ExpirationTime));
    end
    else
      WriteLn('Token INVALID: ', lError);
  finally
    lJWT.Free;
  end;

  WriteLn;
  WriteLn('Press Enter to exit...');
  ReadLn;
end.
