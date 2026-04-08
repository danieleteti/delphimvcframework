// ***************************************************************************
//
// JWT Asymmetric Signing Examples - DelphiMVCFramework
//
// Demonstrates real-world JWT usage patterns with asymmetric keys:
//   1. Auth server signs tokens with private key (RS256)
//   2. Microservice verifies tokens with public key only
//   3. Multi-algorithm support (RS256, ES256, EdDSA)
//   4. Token introspection and claim extraction
//
// ***************************************************************************

program JWTAsymmetricBasicSample;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.IOUtils,
  System.DateUtils,
  MVCFramework.JWT,
  MVCFramework.JWT.RSA,
  MVCFramework.Console;

var
  GKeysPath: string;

// ============================================================
// Scenario 1: Auth Server + Microservice (RS256)
//
// The auth server holds the private key and issues tokens.
// Microservices only have the public key and verify tokens.
// ============================================================

procedure Scenario_AuthServer_And_Microservice;
var
  lSigner: IJWTSigner;
  lJWT: TJWT;
  lToken, lError: string;
begin
  WriteHeader('Scenario 1: Auth Server + Microservice (RS256)');
  WriteLn;

  // --- AUTH SERVER: create and sign a token ---
  WriteLine('  [Auth Server] Signing token with private key...', Cyan);

  lSigner := TRSAJWTSigner.CreateFromFiles(
    JWT_RS256,
    TPath.Combine(GKeysPath, 'rsa\private.pem'),
    TPath.Combine(GKeysPath, 'rsa\public.pem'));

  lJWT := TJWT.Create(lSigner);
  try
    lJWT.Claims.Issuer := 'auth.mycompany.com';
    lJWT.Claims.Subject := 'user-42';
    lJWT.Claims.ExpirationTime := Now + OneHour;
    lJWT.Claims.IssuedAt := Now;
    lJWT.CustomClaims['role'] := 'admin';
    lJWT.CustomClaims['department'] := 'engineering';
    lJWT.CustomClaims['tenant_id'] := 'acme-corp';
    lToken := lJWT.GetToken;
  finally
    lJWT.Free;
  end;

  WriteSuccess('Token signed (' + Length(lToken).ToString + ' chars)');
  WriteLine('  Token: ' + Copy(lToken, 1, 60) + '...', DarkGray);
  WriteLn;

  // --- MICROSERVICE: verify with public key only ---
  WriteLine('  [Microservice] Verifying token with public key only...', Cyan);

  // CreateVerifyOnly: the microservice never sees the private key
  lSigner := TRSAJWTSigner.CreateVerifyOnly(
    JWT_RS256,
    TFile.ReadAllText(TPath.Combine(GKeysPath, 'rsa\public.pem')));

  lJWT := TJWT.Create(lSigner, 60); // 60 seconds leeway
  try
    lJWT.RegClaimsToChecks := [TJWTCheckableClaim.ExpirationTime, TJWTCheckableClaim.IssuedAt];
    if lJWT.LoadToken(lToken, lError) then
    begin
      WriteSuccess('Token verified');
      WriteLine('  Issuer:     ' + lJWT.Claims.Issuer, White);
      WriteLine('  Subject:    ' + lJWT.Claims.Subject, White);
      WriteLine('  Role:       ' + lJWT.CustomClaims['role'], White);
      WriteLine('  Department: ' + lJWT.CustomClaims['department'], White);
      WriteLine('  Tenant:     ' + lJWT.CustomClaims['tenant_id'], White);
      WriteLine('  Expires:    ' + DateTimeToStr(lJWT.Claims.ExpirationTime), White);
    end
    else
      WriteError('Verification failed: ' + lError);
  finally
    lJWT.Free;
  end;
  WriteLn;
end;

// ============================================================
// Scenario 2: Tampered Token Detection
//
// Shows that modifying any part of the token invalidates
// the signature - the core security guarantee of JWT.
// ============================================================

procedure Scenario_TamperedToken;
var
  lSigner: IJWTSigner;
  lJWT: TJWT;
  lToken, lTampered, lError: string;
begin
  WriteHeader('Scenario 2: Tampered Token Detection');
  WriteLn;

  lSigner := TRSAJWTSigner.CreateFromFiles(
    JWT_RS256,
    TPath.Combine(GKeysPath, 'rsa\private.pem'),
    TPath.Combine(GKeysPath, 'rsa\public.pem'));

  // Create a valid token
  lJWT := TJWT.Create(lSigner);
  try
    lJWT.Claims.Issuer := 'auth.mycompany.com';
    lJWT.Claims.Subject := 'user-42';
    lJWT.Claims.ExpirationTime := Now + OneHour;
    lJWT.CustomClaims['role'] := 'user';  // original role
    lToken := lJWT.GetToken;
  finally
    lJWT.Free;
  end;

  // Tamper: flip a character in the middle (payload area)
  lTampered := lToken;
  lTampered[Length(lTampered) div 2] := 'X';

  WriteLine('  Original token: ' + Copy(lToken, 1, 50) + '...', Green);
  WriteLine('  Tampered token: ' + Copy(lTampered, 1, 50) + '...', Red);
  WriteLn;

  // Try to verify the tampered token
  lJWT := TJWT.Create(lSigner);
  try
    lJWT.RegClaimsToChecks := [];
    if lJWT.LoadToken(lTampered, lError) then
      WriteError('SECURITY ISSUE: tampered token accepted!')
    else
      WriteSuccess('Tampered token correctly rejected: ' + lError);
  finally
    lJWT.Free;
  end;
  WriteLn;
end;

// ============================================================
// Scenario 3: Multiple Algorithms
//
// Shows signing with different algorithms using the same API.
// Each algorithm has different security/performance trade-offs.
// ============================================================

procedure Scenario_MultipleAlgorithms;

  procedure DemoAlgorithm(const AName, AAlgorithm, APrivKeyPath, APubKeyPath: string;
    ACreateSigner: TFunc<string, string, string, IJWTSigner>);
  var
    lSigner: IJWTSigner;
    lJWT: TJWT;
    lToken, lError: string;
  begin
    lSigner := ACreateSigner(AAlgorithm, APrivKeyPath, APubKeyPath);
    lJWT := TJWT.Create(lSigner);
    try
      lJWT.Claims.Issuer := 'multi-alg-demo';
      lJWT.Claims.Subject := 'user-1';
      lJWT.Claims.ExpirationTime := Now + OneHour;
      lToken := lJWT.GetToken;
    finally
      lJWT.Free;
    end;

    // Verify
    lJWT := TJWT.Create(lSigner);
    try
      lJWT.RegClaimsToChecks := [];
      if lJWT.LoadToken(lToken, lError) then
        WriteSuccess(AName + ' - token size: ' + Length(lToken).ToString + ' chars')
      else
        WriteError(AName + ' - ' + lError);
    finally
      lJWT.Free;
    end;
  end;

begin
  WriteHeader('Scenario 3: Multiple Algorithms Comparison');
  WriteLn;
  WriteInfo('Same API, different algorithms and key types');
  WriteLn;

  // RSA PKCS#1 v1.5 (most common, widely supported)
  DemoAlgorithm('RS256 (RSA 2048-bit)', JWT_RS256,
    TPath.Combine(GKeysPath, 'rsa\private.pem'),
    TPath.Combine(GKeysPath, 'rsa\public.pem'),
    function(Alg, Priv, Pub: string): IJWTSigner
    begin
      Result := TRSAJWTSigner.CreateFromFiles(Alg, Priv, Pub);
    end);

  // RSA-PSS (stronger padding, recommended over PKCS#1)
  DemoAlgorithm('PS256 (RSA-PSS)', JWT_PS256,
    TPath.Combine(GKeysPath, 'rsa\private.pem'),
    TPath.Combine(GKeysPath, 'rsa\public.pem'),
    function(Alg, Priv, Pub: string): IJWTSigner
    begin
      Result := TRSAPSSJWTSigner.CreateFromFiles(Alg, Priv, Pub);
    end);

  // ECDSA (smaller keys and tokens, fast verification)
  DemoAlgorithm('ES256 (ECDSA P-256)', JWT_ES256,
    TPath.Combine(GKeysPath, 'ec\es256_private.pem'),
    TPath.Combine(GKeysPath, 'ec\es256_public.pem'),
    function(Alg, Priv, Pub: string): IJWTSigner
    begin
      Result := TECDSAJWTSigner.CreateFromFiles(Alg, Priv, Pub);
    end);

  // EdDSA (modern, fastest, smallest signatures)
  DemoAlgorithm('EdDSA (Ed25519)', JWT_EDDSA,
    TPath.Combine(GKeysPath, 'ed25519\private.pem'),
    TPath.Combine(GKeysPath, 'ed25519\public.pem'),
    function(Alg, Priv, Pub: string): IJWTSigner
    begin
      Result := TEdDSAJWTSigner.CreateFromFiles(Priv, Pub);
    end);

  WriteLn;
  WriteInfo('RS256: max compatibility (Azure AD, Google, AWS)');
  WriteInfo('PS256: stronger RSA padding, same key as RS256');
  WriteInfo('ES256: smaller tokens, ideal for bandwidth-constrained scenarios');
  WriteInfo('EdDSA: fastest, smallest keys, modern choice');
  WriteLn;
end;

// ============================================================
// Scenario 4: Expired Token Handling
//
// Shows how claim validation catches expired tokens.
// ============================================================

procedure Scenario_ExpiredToken;
var
  lSigner: IJWTSigner;
  lJWT: TJWT;
  lToken, lError: string;
begin
  WriteHeader('Scenario 4: Expired Token Handling');
  WriteLn;

  lSigner := TRSAJWTSigner.CreateFromFiles(
    JWT_RS256,
    TPath.Combine(GKeysPath, 'rsa\private.pem'),
    TPath.Combine(GKeysPath, 'rsa\public.pem'));

  // Create a token that expired 1 hour ago
  lJWT := TJWT.Create(lSigner);
  try
    lJWT.Claims.Issuer := 'auth.mycompany.com';
    lJWT.Claims.Subject := 'user-42';
    lJWT.Claims.ExpirationTime := Now - OneHour;  // already expired
    lJWT.Claims.IssuedAt := Now - (2 * OneHour);
    lToken := lJWT.GetToken;
  finally
    lJWT.Free;
  end;

  WriteLine('  Token created with exp = 1 hour ago', Cyan);

  // Try to verify - should fail on expiration check
  lJWT := TJWT.Create(lSigner, 60); // 60 seconds leeway
  try
    lJWT.RegClaimsToChecks := [TJWTCheckableClaim.ExpirationTime];
    if lJWT.LoadToken(lToken, lError) then
      WriteError('Expired token was accepted!')
    else
      WriteSuccess('Expired token correctly rejected');
  finally
    lJWT.Free;
  end;
  WriteLn;
end;

// ============================================================

begin
  ReportMemoryLeaksOnShutdown := True;
  EnableUTF8Console;
  try
    GKeysPath := TPath.Combine(ExtractFilePath(ParamStr(0)), 'keys');

    if not TDirectory.Exists(GKeysPath) then
    begin
      WriteError('Keys directory not found: ' + GKeysPath);
      WriteInfo('Copy the keys/ folder next to the executable');
      ReadLn;
      Exit;
    end;

    ClrScr;
    WriteHeader('DMVCFramework - JWT Asymmetric Signing Examples', 60);
    WriteLn;

    Scenario_AuthServer_And_Microservice;
    Scenario_TamperedToken;
    Scenario_MultipleAlgorithms;
    Scenario_ExpiredToken;

    WriteHeader('All scenarios completed', 60);
    WriteLn;
    Write('Press Enter to exit...');
    ReadLn;
  except
    on E: Exception do
    begin
      WriteError(E.ClassName + ': ' + E.Message);
      ReadLn;
    end;
  end;
end.
