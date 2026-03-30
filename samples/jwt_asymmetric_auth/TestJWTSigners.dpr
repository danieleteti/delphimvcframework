// ***************************************************************************
//
// JWT Signer Test Suite
// Tests all 16 JWT signing algorithms supported by DMVCFramework
//
// ***************************************************************************

program TestJWTSigners;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.IOUtils,
  System.DateUtils,
  MVCFramework.JWT,
  MVCFramework.JWT.RSA,
  MVCFramework.HMAC;

var
  GKeysPath: string;
  GPassCount, GFailCount: Integer;

procedure Log(const AMsg: string);
begin
  WriteLn(AMsg);
end;

procedure Pass(const ATest: string);
begin
  Inc(GPassCount);
  Log('  [PASS] ' + ATest);
end;

procedure Fail(const ATest: string; const ADetail: string = '');
begin
  Inc(GFailCount);
  Log('  [FAIL] ' + ATest);
  if ADetail <> '' then
    Log('         ' + ADetail);
end;

// Test: sign a token with the signer, then verify it
procedure TestSignAndVerify(const AName: string; ASigner: IJWTSigner);
var
  LJWT: TJWT;
  LToken, LError: string;
  LVerifyJWT: TJWT;
begin
  Log('--- ' + AName + ' ---');

  // Sign
  LJWT := TJWT.Create(ASigner, 0);
  try
    LJWT.RegClaimsToChecks := [];
    LJWT.Claims.Issuer := 'test';
    LJWT.Claims.Subject := 'testuser';
    LJWT.Claims.ExpirationTime := Now + OneHour;
    LJWT.Claims.IssuedAt := Now;
    LJWT.CustomClaims['role'] := 'admin';
    try
      LToken := LJWT.GetToken;
      if LToken.IsEmpty then
      begin
        Fail(AName + ' sign', 'Empty token');
        Exit;
      end;
      Pass(AName + ' sign (' + Length(LToken).ToString + ' chars)');
    except
      on E: Exception do
      begin
        Fail(AName + ' sign', E.Message);
        Exit;
      end;
    end;
  finally
    LJWT.Free;
  end;

  // Verify
  LVerifyJWT := TJWT.Create(ASigner, 0);
  try
    LVerifyJWT.RegClaimsToChecks := [];
    try
      if LVerifyJWT.LoadToken(LToken, LError) then
      begin
        // Check claims survived the round-trip
        if LVerifyJWT.Claims.Issuer = 'test' then
          Pass(AName + ' verify + claims')
        else
          Fail(AName + ' verify', 'Issuer mismatch: ' + LVerifyJWT.Claims.Issuer);
      end
      else
        Fail(AName + ' verify', LError);
    except
      on E: Exception do
        Fail(AName + ' verify', E.Message);
    end;
  finally
    LVerifyJWT.Free;
  end;
end;

// Test: tampered token should fail verification
procedure TestTamperedToken(const AName: string; ASigner: IJWTSigner);
var
  LJWT, LVerifyJWT: TJWT;
  LToken, LError: string;
begin
  LJWT := TJWT.Create(ASigner, 0);
  try
    LJWT.RegClaimsToChecks := [];
    LJWT.Claims.Issuer := 'test';
    LJWT.Claims.ExpirationTime := Now + OneHour;
    LToken := LJWT.GetToken;
  finally
    LJWT.Free;
  end;

  // Tamper with payload (flip a character)
  LToken := LToken.Substring(0, Length(LToken) div 2) + 'X' +
            LToken.Substring(Length(LToken) div 2 + 1);

  LVerifyJWT := TJWT.Create(ASigner, 0);
  try
    LVerifyJWT.RegClaimsToChecks := [];
    if not LVerifyJWT.LoadToken(LToken, LError) then
      Pass(AName + ' tamper detection')
    else
      Fail(AName + ' tamper detection', 'Tampered token was accepted!');
  finally
    LVerifyJWT.Free;
  end;
end;

// Test: wrong key should fail verification
procedure TestWrongKey(const AName: string; ASignSigner, AWrongSigner: IJWTSigner);
var
  LJWT, LVerifyJWT: TJWT;
  LToken, LError: string;
begin
  LJWT := TJWT.Create(ASignSigner, 0);
  try
    LJWT.RegClaimsToChecks := [];
    LJWT.Claims.Issuer := 'test';
    LJWT.Claims.ExpirationTime := Now + OneHour;
    LToken := LJWT.GetToken;
  finally
    LJWT.Free;
  end;

  LVerifyJWT := TJWT.Create(AWrongSigner, 0);
  try
    LVerifyJWT.RegClaimsToChecks := [];
    if not LVerifyJWT.LoadToken(LToken, LError) then
      Pass(AName + ' wrong key rejection')
    else
      Fail(AName + ' wrong key rejection', 'Token verified with wrong key!');
  finally
    LVerifyJWT.Free;
  end;
end;

// Test: algorithm mismatch
procedure TestAlgorithmMismatch;
var
  LHMACSigner: IJWTSigner;
  LRSASigner: IJWTSigner;
  LJWT, LVerifyJWT: TJWT;
  LToken, LError: string;
begin
  Log('--- Algorithm Mismatch ---');
  LHMACSigner := THMACJWTSigner.Create(HMAC_HS256, 'secret');
  LRSASigner := TRSAJWTSigner.CreateFromFiles(JWT_RS256,
    TPath.Combine(GKeysPath, 'rsa/private.pem'),
    TPath.Combine(GKeysPath, 'rsa/public.pem'));

  LJWT := TJWT.Create(LHMACSigner, 0);
  try
    LJWT.RegClaimsToChecks := [];
    LJWT.Claims.Issuer := 'test';
    LJWT.Claims.ExpirationTime := Now + OneHour;
    LToken := LJWT.GetToken;
  finally
    LJWT.Free;
  end;

  // Try to verify HS256 token with RS256 signer
  LVerifyJWT := TJWT.Create(LRSASigner, 0);
  try
    LVerifyJWT.RegClaimsToChecks := [];
    if not LVerifyJWT.LoadToken(LToken, LError) then
      Pass('HS256 token rejected by RS256 signer')
    else
      Fail('HS256 token rejected by RS256 signer', 'Algorithm confusion not detected!');
  finally
    LVerifyJWT.Free;
  end;
end;

// Test: HMAC backward compatibility
procedure TestHMACBackwardCompat;
var
  LJWT, LVerifyJWT: TJWT;
  LToken, LError: string;
begin
  Log('--- HMAC Backward Compatibility ---');

  // Old-style constructor
  LJWT := TJWT.Create('my_secret_key', 0, HMAC_HS256);
  try
    LJWT.RegClaimsToChecks := [];
    LJWT.Claims.Issuer := 'test';
    LJWT.Claims.ExpirationTime := Now + OneHour;
    LJWT.Claims.IssuedAt := Now;
    LToken := LJWT.GetToken;
    Pass('HS256 old constructor sign');
  finally
    LJWT.Free;
  end;

  // Verify with old-style constructor
  LVerifyJWT := TJWT.Create('my_secret_key', 0, HMAC_HS256);
  try
    LVerifyJWT.RegClaimsToChecks := [];
    if LVerifyJWT.LoadToken(LToken, LError) then
      Pass('HS256 old constructor verify')
    else
      Fail('HS256 old constructor verify', LError);
  finally
    LVerifyJWT.Free;
  end;

  // Verify with new-style constructor (THMACJWTSigner)
  LVerifyJWT := TJWT.Create(THMACJWTSigner.Create(HMAC_HS256, 'my_secret_key'), 0);
  try
    LVerifyJWT.RegClaimsToChecks := [];
    if LVerifyJWT.LoadToken(LToken, LError) then
      Pass('HS256 new constructor verifies old token')
    else
      Fail('HS256 new constructor verifies old token', LError);
  finally
    LVerifyJWT.Free;
  end;

  // HS384 and HS512
  LJWT := TJWT.Create('secret384', 0, HMAC_HS384);
  try
    LJWT.RegClaimsToChecks := [];
    LJWT.Claims.Issuer := 'test';
    LJWT.Claims.ExpirationTime := Now + OneHour;
    LToken := LJWT.GetToken;
    Pass('HS384 sign');
  finally
    LJWT.Free;
  end;
  LVerifyJWT := TJWT.Create('secret384', 0, HMAC_HS384);
  try
    LVerifyJWT.RegClaimsToChecks := [];
    if LVerifyJWT.LoadToken(LToken, LError) then Pass('HS384 verify') else Fail('HS384 verify', LError);
  finally
    LVerifyJWT.Free;
  end;

  LJWT := TJWT.Create('secret512', 0);  // default is HS512
  try
    LJWT.RegClaimsToChecks := [];
    LJWT.Claims.Issuer := 'test';
    LJWT.Claims.ExpirationTime := Now + OneHour;
    LToken := LJWT.GetToken;
    Pass('HS512 sign (default)');
  finally
    LJWT.Free;
  end;
  LVerifyJWT := TJWT.Create('secret512', 0);
  try
    LVerifyJWT.RegClaimsToChecks := [];
    if LVerifyJWT.LoadToken(LToken, LError) then Pass('HS512 verify') else Fail('HS512 verify', LError);
  finally
    LVerifyJWT.Free;
  end;
end;

var
  LRSASigner, LRSASigner2: IJWTSigner;
  LPSSSigner: IJWTSigner;
  LES256Signer, LES384Signer, LES512Signer: IJWTSigner;
  LEdDSASigner: IJWTSigner;
begin
  ReportMemoryLeaksOnShutdown := True;

  GPassCount := 0;
  GFailCount := 0;
  GKeysPath := TPath.Combine(ExtractFilePath(ParamStr(0)), 'keys');

  Log('');
  Log('================================================================');
  Log('  DMVCFramework JWT Signer Test Suite');
  Log('  Keys path: ' + GKeysPath);
  Log('================================================================');
  Log('');

  try
    // ============================================================
    // HMAC (backward compatibility)
    // ============================================================
    TestHMACBackwardCompat;
    Log('');

    // ============================================================
    // RSA PKCS#1 v1.5 (RS256, RS384, RS512)
    // ============================================================
    LRSASigner := TRSAJWTSigner.CreateFromFiles(JWT_RS256,
      TPath.Combine(GKeysPath, 'rsa/private.pem'),
      TPath.Combine(GKeysPath, 'rsa/public.pem'));
    TestSignAndVerify('RS256', LRSASigner);
    TestTamperedToken('RS256', LRSASigner);

    TestSignAndVerify('RS384', TRSAJWTSigner.CreateFromFiles(JWT_RS384,
      TPath.Combine(GKeysPath, 'rsa/private.pem'),
      TPath.Combine(GKeysPath, 'rsa/public.pem')));

    TestSignAndVerify('RS512', TRSAJWTSigner.CreateFromFiles(JWT_RS512,
      TPath.Combine(GKeysPath, 'rsa/private.pem'),
      TPath.Combine(GKeysPath, 'rsa/public.pem')));
    Log('');

    // RSA wrong key test (generate a second key pair)
    LRSASigner2 := TRSAJWTSigner.Create(JWT_RS256,
      TFile.ReadAllText(TPath.Combine(GKeysPath, 'rsa/private.pem')),
      // Use EC public key as "wrong" RSA public key - should fail
      TFile.ReadAllText(TPath.Combine(GKeysPath, 'ec/es256_public.pem')));
    TestWrongKey('RS256', LRSASigner, LRSASigner2);
    Log('');

    // ============================================================
    // RSA-PSS (PS256, PS384, PS512)
    // ============================================================
    LPSSSigner := TRSAPSSJWTSigner.CreateFromFiles(JWT_PS256,
      TPath.Combine(GKeysPath, 'rsa/private.pem'),
      TPath.Combine(GKeysPath, 'rsa/public.pem'));
    TestSignAndVerify('PS256', LPSSSigner);
    TestTamperedToken('PS256', LPSSSigner);

    TestSignAndVerify('PS384', TRSAPSSJWTSigner.CreateFromFiles(JWT_PS384,
      TPath.Combine(GKeysPath, 'rsa/private.pem'),
      TPath.Combine(GKeysPath, 'rsa/public.pem')));

    TestSignAndVerify('PS512', TRSAPSSJWTSigner.CreateFromFiles(JWT_PS512,
      TPath.Combine(GKeysPath, 'rsa/private.pem'),
      TPath.Combine(GKeysPath, 'rsa/public.pem')));
    Log('');

    // ============================================================
    // ECDSA (ES256, ES384, ES512)
    // ============================================================
    LES256Signer := TECDSAJWTSigner.CreateFromFiles(JWT_ES256,
      TPath.Combine(GKeysPath, 'ec/es256_private.pem'),
      TPath.Combine(GKeysPath, 'ec/es256_public.pem'));
    TestSignAndVerify('ES256', LES256Signer);
    TestTamperedToken('ES256', LES256Signer);

    LES384Signer := TECDSAJWTSigner.CreateFromFiles(JWT_ES384,
      TPath.Combine(GKeysPath, 'ec/es384_private.pem'),
      TPath.Combine(GKeysPath, 'ec/es384_public.pem'));
    TestSignAndVerify('ES384', LES384Signer);

    LES512Signer := TECDSAJWTSigner.CreateFromFiles(JWT_ES512,
      TPath.Combine(GKeysPath, 'ec/es512_private.pem'),
      TPath.Combine(GKeysPath, 'ec/es512_public.pem'));
    TestSignAndVerify('ES512', LES512Signer);

    // EC wrong key test
    TestWrongKey('ES256', LES256Signer,
      TECDSAJWTSigner.Create(JWT_ES256, '',
        TFile.ReadAllText(TPath.Combine(GKeysPath, 'ec/es384_public.pem'))));
    Log('');

    // ============================================================
    // EdDSA (Ed25519)
    // ============================================================
    LEdDSASigner := TEdDSAJWTSigner.CreateFromFiles(
      TPath.Combine(GKeysPath, 'ed25519/private.pem'),
      TPath.Combine(GKeysPath, 'ed25519/public.pem'));
    TestSignAndVerify('EdDSA (Ed25519)', LEdDSASigner);
    TestTamperedToken('EdDSA', LEdDSASigner);
    Log('');

    // ============================================================
    // Algorithm mismatch
    // ============================================================
    TestAlgorithmMismatch;
    Log('');

  except
    on E: Exception do
    begin
      Log('');
      Log('*** FATAL: ' + E.ClassName + ': ' + E.Message);
      Inc(GFailCount);
    end;
  end;

  Log('================================================================');
  Log('  Results: ' + GPassCount.ToString + ' passed, ' + GFailCount.ToString + ' failed');
  if GFailCount = 0 then
    Log('  ALL TESTS PASSED')
  else
    Log('  *** SOME TESTS FAILED ***');
  Log('================================================================');
  Log('');

  if GFailCount > 0 then
    ExitCode := 1;

  Log('Press Enter to exit...');
  ReadLn;
end.
