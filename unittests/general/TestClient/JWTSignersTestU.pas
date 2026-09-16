// ***************************************************************************
//
// JWT Signers Test Suite
// Tests all JWT signing algorithms: HS256/384/512, RS256/384/512,
// PS256/384/512, ES256/384/512, EdDSA (Ed25519)
//
// Requires OpenSSL 1.1.1+ DLLs in the test executable directory
// and RSA/EC/Ed25519 PEM keys in a "keys" subdirectory.
//
// ***************************************************************************

unit JWTSignersTestU;

interface

uses
  DUnitX.TestFramework,
  MVCFramework.JWT;

type
  [TestFixture]
  TTestHMACSigners = class
  public
    [Test]
    procedure TestHS256_SignAndVerify;
    [Test]
    procedure TestHS384_SignAndVerify;
    [Test]
    procedure TestHS512_SignAndVerify;
    [Test]
    procedure TestHS256_OldConstructor_BackwardCompat;
    [Test]
    procedure TestHS256_NewSignerVerifiesOldToken;
    [Test]
    procedure TestHS256_WrongKeyRejected;
    [Test]
    procedure TestHS256_TamperedTokenRejected;
    [Test]
    procedure TestHMAC_ConstantTimeComparison;
  end;

  [TestFixture]
  TTestRSASigners = class
  private
    function KeysPath: string;
  public
    [Test]
    procedure TestRS256_SignAndVerify;
    [Test]
    procedure TestRS384_SignAndVerify;
    [Test]
    procedure TestRS512_SignAndVerify;
    [Test]
    procedure TestRS256_TamperedTokenRejected;
    [Test]
    procedure TestRS256_WrongKeyRejected;
    [Test]
    procedure TestRS256_SignOnlyMode;
    [Test]
    procedure TestRS256_VerifyOnlyMode;
  end;

  [TestFixture]
  TTestRSAPSSSigners = class
  private
    function KeysPath: string;
  public
    [Test]
    procedure TestPS256_SignAndVerify;
    [Test]
    procedure TestPS384_SignAndVerify;
    [Test]
    procedure TestPS512_SignAndVerify;
    [Test]
    procedure TestPS256_TamperedTokenRejected;
    [Test]
    procedure TestPS256_UsesSameRSAKeys;
  end;

  [TestFixture]
  TTestECDSASigners = class
  private
    function KeysPath: string;
  public
    [Test]
    procedure TestES256_SignAndVerify;
    [Test]
    procedure TestES384_SignAndVerify;
    [Test]
    procedure TestES512_SignAndVerify;
    [Test]
    procedure TestES256_TamperedTokenRejected;
    [Test]
    procedure TestES256_WrongCurveKeyRejected;
  end;

  [TestFixture]
  TTestEdDSASigner = class
  private
    function KeysPath: string;
  public
    [Test]
    procedure TestEdDSA_SignAndVerify;
    [Test]
    procedure TestEdDSA_TamperedTokenRejected;
    [Test]
    procedure TestEdDSA_ClaimsRoundTrip;
  end;

  [TestFixture]
  TTestJWTSignerSecurity = class
  private
    function KeysPath: string;
  public
    [Test]
    procedure TestAlgorithmMismatch_HS256_vs_RS256;
    [Test]
    procedure TestAlgorithmMismatch_RS256_vs_ES256;
    [Test]
    procedure TestNoPrivateKey_SignRaisesException;
    [Test]
    procedure TestNoPublicKey_VerifyRaisesException;
    [Test]
    procedure TestInvalidAlgorithm_RaisesException;
    [Test]
    procedure TestEmptyKeys_RaisesException;
  end;

  // Regression guards for the signature-tamper tests. The original TamperToken
  // helper overwrote the last two base64url chars with 'XX', which always
  // decodes to byte $5D. Because the final base64url char of a 256-/64-byte
  // signature carries only 2 significant bits, whenever a genuine signature
  // ended in $5D (~1/256, varying per run via the iat/exp claims) the "tamper"
  // was a no-op: the unchanged valid token verified and *_TamperedTokenRejected
  // failed intermittently. These tests pin the invariants deterministically.
  [TestFixture]
  TTestJWTSignatureTamper = class
  public
    // The shared TamperToken helper must ALWAYS change the decoded signature,
    // for every possible final-byte value. (The old helper was a no-op exactly
    // when the signature ended in $5D.)
    [Test]
    procedure TamperTokenAlwaysAltersDecodedSignature;
    // Deterministic, non-flaky proof that the RSA verify path rejects a token
    // whose signature has a single corrupted byte.
    [Test]
    procedure RS256SingleByteSignatureCorruptionIsRejected;
  end;

  // GHSA-hgv7-ch4w-2f47: the demo secret shipped as default parameter value of
  // the JWT middleware/filter constructors is public, so tokens signed with it
  // can be forged by anyone. It must be rejected instead of used.
  [TestFixture]
  TTestJWTInsecureSecret = class
  public
    [Test]
    procedure ShippedDefaultSecretIsRejected;
    [Test]
    procedure EmptySecretIsRejected;
    [Test]
    procedure OwnSecretIsAccepted;
    // The middleware and the endpoint filter must refuse the secret while they
    // are being built (server startup), not on the first request they serve.
    [Test]
    procedure MiddlewareConstructorRejectsShippedDefaultSecret;
    [Test]
    procedure EndpointFilterFactoryRejectsShippedDefaultSecret;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.DateUtils,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.HMAC,
  MVCFramework.JWT.RSA,
  MVCFramework.MinimalAPI,
  MVCFramework.Middleware.JWT,
  MVCFramework.Filters;

// ============================================================
// Helper
// ============================================================

function CreateTestToken(ASigner: IJWTSigner): string;
var
  LJWT: TJWT;
begin
  LJWT := TJWT.Create(ASigner, 0);
  try
    LJWT.RegClaimsToChecks := [];
    LJWT.Claims.Issuer := 'dmvc-test';
    LJWT.Claims.Subject := 'testuser';
    LJWT.Claims.ExpirationTime := Now + OneHour;
    LJWT.Claims.IssuedAt := Now;
    LJWT.CustomClaims['role'] := 'admin';
    Result := LJWT.GetToken;
  finally
    LJWT.Free;
  end;
end;

function VerifyTestToken(ASigner: IJWTSigner; const AToken: string): Boolean;
var
  LJWT: TJWT;
  LError: string;
begin
  LJWT := TJWT.Create(ASigner, 0);
  try
    LJWT.RegClaimsToChecks := [];
    Result := LJWT.LoadToken(AToken, LError);
  finally
    LJWT.Free;
  end;
end;

function VerifyAndGetClaims(ASigner: IJWTSigner; const AToken: string;
  out AIssuer, ASubject, ARole: string): Boolean;
var
  LJWT: TJWT;
  LError: string;
begin
  LJWT := TJWT.Create(ASigner, 0);
  try
    LJWT.RegClaimsToChecks := [];
    Result := LJWT.LoadToken(AToken, LError);
    if Result then
    begin
      AIssuer := LJWT.Claims.Issuer;
      ASubject := LJWT.Claims.Subject;
      ARole := LJWT.CustomClaims['role'];
    end;
  finally
    LJWT.Free;
  end;
end;

function TamperToken(const AToken: string): string;
var
  LParts: TArray<string>;
  LFirst: Char;
begin
  // Corrupt the signature so header/payload stay valid but verification must
  // fail. We flip the FIRST base64url char of the signature: it encodes the
  // top 6 bits of signature byte 0 (a fully-significant position), so any
  // change always alters the decoded signature.
  //
  // NOTE: the previous version overwrote the LAST two chars with 'XX'. For
  // 256- and 64-byte signatures the final base64url char carries only 2
  // significant bits (4 are don't-care padding bits), so 'XX' always decoded
  // the last byte to 0x5D. Whenever the genuine signature's last byte was
  // already 0x5D (~1/256, and it varies per run via the iat/exp claims) the
  // "tamper" was a no-op -> the valid token verified -> intermittent failures.
  LParts := AToken.Split(['.']);
  if (Length(LParts) = 3) and (LParts[2].Length > 0) then
  begin
    LFirst := LParts[2].Chars[0];
    if LFirst = 'A' then
      LParts[2] := 'B' + LParts[2].Substring(1)
    else
      LParts[2] := 'A' + LParts[2].Substring(1);
    Result := LParts[0] + '.' + LParts[1] + '.' + LParts[2];
  end
  else
    Result := AToken + 'X';
end;

function GetTestKeysPath: string;
begin
  Result := TPath.Combine(ExtractFilePath(ParamStr(0)), 'keys');
  if not TDirectory.Exists(Result) then
    Result := TPath.Combine(
      TPath.GetFullPath(TPath.Combine(ExtractFilePath(ParamStr(0)), '..\..\..\..\samples\jwt_asymmetric_auth\bin')),
      'keys');
end;


// ============================================================
// TTestHMACSigners
// ============================================================

procedure TTestHMACSigners.TestHS256_SignAndVerify;
var
  LSigner: IJWTSigner;
  LToken: string;
begin
  LSigner := THMACJWTSigner.Create(HMAC_HS256, 'test-secret');
  LToken := CreateTestToken(LSigner);
  Assert.IsNotEmpty(LToken);
  Assert.IsTrue(VerifyTestToken(LSigner, LToken));
end;

procedure TTestHMACSigners.TestHS384_SignAndVerify;
var
  LSigner: IJWTSigner;
  LToken: string;
begin
  LSigner := THMACJWTSigner.Create(HMAC_HS384, 'test-secret-384');
  LToken := CreateTestToken(LSigner);
  Assert.IsNotEmpty(LToken);
  Assert.IsTrue(VerifyTestToken(LSigner, LToken));
end;

procedure TTestHMACSigners.TestHS512_SignAndVerify;
var
  LSigner: IJWTSigner;
  LToken: string;
begin
  LSigner := THMACJWTSigner.Create(HMAC_HS512, 'test-secret-512');
  LToken := CreateTestToken(LSigner);
  Assert.IsNotEmpty(LToken);
  Assert.IsTrue(VerifyTestToken(LSigner, LToken));
end;

procedure TTestHMACSigners.TestHS256_OldConstructor_BackwardCompat;
var
  LJWT, LVerify: TJWT;
  LToken, LError: string;
begin
  // Old-style constructor must still work
  LJWT := TJWT.Create('my_secret', 0, HMAC_HS256);
  try
    LJWT.RegClaimsToChecks := [];
    LJWT.Claims.Issuer := 'test';
    LJWT.Claims.ExpirationTime := Now + OneHour;
    LToken := LJWT.GetToken;
  finally
    LJWT.Free;
  end;

  LVerify := TJWT.Create('my_secret', 0, HMAC_HS256);
  try
    LVerify.RegClaimsToChecks := [];
    Assert.IsTrue(LVerify.LoadToken(LToken, LError), LError);
  finally
    LVerify.Free;
  end;
end;

procedure TTestHMACSigners.TestHS256_NewSignerVerifiesOldToken;
var
  LJWT, LVerify: TJWT;
  LToken, LError: string;
begin
  // Token created with old constructor
  LJWT := TJWT.Create('my_secret', 0, HMAC_HS256);
  try
    LJWT.RegClaimsToChecks := [];
    LJWT.Claims.Issuer := 'test';
    LJWT.Claims.ExpirationTime := Now + OneHour;
    LToken := LJWT.GetToken;
  finally
    LJWT.Free;
  end;

  // Verified with new IJWTSigner constructor
  LVerify := TJWT.Create(THMACJWTSigner.Create(HMAC_HS256, 'my_secret'), 0);
  try
    LVerify.RegClaimsToChecks := [];
    Assert.IsTrue(LVerify.LoadToken(LToken, LError), LError);
  finally
    LVerify.Free;
  end;
end;

procedure TTestHMACSigners.TestHS256_WrongKeyRejected;
var
  LToken: string;
begin
  LToken := CreateTestToken(THMACJWTSigner.Create(HMAC_HS256, 'correct-key'));
  Assert.IsFalse(VerifyTestToken(THMACJWTSigner.Create(HMAC_HS256, 'wrong-key'), LToken));
end;

procedure TTestHMACSigners.TestHS256_TamperedTokenRejected;
var
  LToken: string;
  LSigner: IJWTSigner;
begin
  LSigner := THMACJWTSigner.Create(HMAC_HS256, 'secret');
  LToken := TamperToken(CreateTestToken(LSigner));
  Assert.IsFalse(VerifyTestToken(LSigner, LToken));
end;

procedure TTestHMACSigners.TestHMAC_ConstantTimeComparison;
var
  LSigner: THMACJWTSigner;
  LGoodSig, LBadSig: TBytes;
begin
  // Verify that Verify method works correctly (constant-time is internal)
  LSigner := THMACJWTSigner.Create(HMAC_HS256, 'key');
  try
    LGoodSig := LSigner.Sign('test-input');
    LBadSig := Copy(LGoodSig);
    LBadSig[0] := LBadSig[0] xor $FF;
    Assert.IsTrue(LSigner.Verify('test-input', LGoodSig));
    Assert.IsFalse(LSigner.Verify('test-input', LBadSig));
    // Different length
    Assert.IsFalse(LSigner.Verify('test-input', Copy(LGoodSig, 0, Length(LGoodSig) - 1)));
  finally
    LSigner.Free;
  end;
end;


// ============================================================
// TTestRSASigners
// ============================================================

function TTestRSASigners.KeysPath: string;
begin
  Result := GetTestKeysPath;
end;

procedure TTestRSASigners.TestRS256_SignAndVerify;
var
  LSigner: IJWTSigner;
  LToken: string;
  LIssuer, LSubject, LRole: string;
begin
  LSigner := TRSAJWTSigner.CreateFromFiles(JWT_RS256,
    TPath.Combine(KeysPath, 'rsa/private.pem'),
    TPath.Combine(KeysPath, 'rsa/public.pem'));
  LToken := CreateTestToken(LSigner);
  Assert.IsNotEmpty(LToken);
  Assert.IsTrue(VerifyAndGetClaims(LSigner, LToken, LIssuer, LSubject, LRole));
  Assert.AreEqual('dmvc-test', LIssuer);
  Assert.AreEqual('testuser', LSubject);
  Assert.AreEqual('admin', LRole);
end;

procedure TTestRSASigners.TestRS384_SignAndVerify;
var LSigner: IJWTSigner;
begin
  LSigner := TRSAJWTSigner.CreateFromFiles(JWT_RS384,
    TPath.Combine(KeysPath, 'rsa/private.pem'), TPath.Combine(KeysPath, 'rsa/public.pem'));
  Assert.IsTrue(VerifyTestToken(LSigner, CreateTestToken(LSigner)));
end;

procedure TTestRSASigners.TestRS512_SignAndVerify;
var LSigner: IJWTSigner;
begin
  LSigner := TRSAJWTSigner.CreateFromFiles(JWT_RS512,
    TPath.Combine(KeysPath, 'rsa/private.pem'), TPath.Combine(KeysPath, 'rsa/public.pem'));
  Assert.IsTrue(VerifyTestToken(LSigner, CreateTestToken(LSigner)));
end;

procedure TTestRSASigners.TestRS256_TamperedTokenRejected;
var LSigner: IJWTSigner;
begin
  LSigner := TRSAJWTSigner.CreateFromFiles(JWT_RS256,
    TPath.Combine(KeysPath, 'rsa/private.pem'), TPath.Combine(KeysPath, 'rsa/public.pem'));
  Assert.IsFalse(VerifyTestToken(LSigner, TamperToken(CreateTestToken(LSigner))));
end;

procedure TTestRSASigners.TestRS256_WrongKeyRejected;
var LSignSigner, LVerifySigner: IJWTSigner; LToken: string;
begin
  LSignSigner := TRSAJWTSigner.CreateFromFiles(JWT_RS256,
    TPath.Combine(KeysPath, 'rsa/private.pem'), TPath.Combine(KeysPath, 'rsa/public.pem'));
  LToken := CreateTestToken(LSignSigner);
  // Verify with EC public key (wrong key type)
  LVerifySigner := TRSAJWTSigner.Create(JWT_RS256, '',
    TFile.ReadAllText(TPath.Combine(KeysPath, 'ec/es256_public.pem')));
  Assert.IsFalse(VerifyTestToken(LVerifySigner, LToken));
end;

procedure TTestRSASigners.TestRS256_SignOnlyMode;
var LSigner: IJWTSigner; LToken: string;
begin
  LSigner := TRSAJWTSigner.CreateSignOnly(JWT_RS256,
    TFile.ReadAllText(TPath.Combine(KeysPath, 'rsa/private.pem')));
  LToken := CreateTestToken(LSigner);
  Assert.IsNotEmpty(LToken);
  Assert.WillRaise(
    procedure begin VerifyTestToken(LSigner, LToken); end,
    EMVCJWTSignerException);
end;

procedure TTestRSASigners.TestRS256_VerifyOnlyMode;
var LSignSigner, LVerifySigner: IJWTSigner; LToken: string;
begin
  LSignSigner := TRSAJWTSigner.CreateFromFiles(JWT_RS256,
    TPath.Combine(KeysPath, 'rsa/private.pem'), TPath.Combine(KeysPath, 'rsa/public.pem'));
  LToken := CreateTestToken(LSignSigner);

  LVerifySigner := TRSAJWTSigner.CreateVerifyOnly(JWT_RS256,
    TFile.ReadAllText(TPath.Combine(KeysPath, 'rsa/public.pem')));
  Assert.IsTrue(VerifyTestToken(LVerifySigner, LToken));
  Assert.WillRaise(
    procedure begin CreateTestToken(LVerifySigner); end,
    EMVCJWTSignerException);
end;


// ============================================================
// TTestRSAPSSSigners
// ============================================================

function TTestRSAPSSSigners.KeysPath: string;
begin
  Result := GetTestKeysPath;
end;

procedure TTestRSAPSSSigners.TestPS256_SignAndVerify;
var LSigner: IJWTSigner;
begin
  LSigner := TRSAPSSJWTSigner.CreateFromFiles(JWT_PS256,
    TPath.Combine(KeysPath, 'rsa/private.pem'), TPath.Combine(KeysPath, 'rsa/public.pem'));
  Assert.IsTrue(VerifyTestToken(LSigner, CreateTestToken(LSigner)));
end;

procedure TTestRSAPSSSigners.TestPS384_SignAndVerify;
var LSigner: IJWTSigner;
begin
  LSigner := TRSAPSSJWTSigner.CreateFromFiles(JWT_PS384,
    TPath.Combine(KeysPath, 'rsa/private.pem'), TPath.Combine(KeysPath, 'rsa/public.pem'));
  Assert.IsTrue(VerifyTestToken(LSigner, CreateTestToken(LSigner)));
end;

procedure TTestRSAPSSSigners.TestPS512_SignAndVerify;
var LSigner: IJWTSigner;
begin
  LSigner := TRSAPSSJWTSigner.CreateFromFiles(JWT_PS512,
    TPath.Combine(KeysPath, 'rsa/private.pem'), TPath.Combine(KeysPath, 'rsa/public.pem'));
  Assert.IsTrue(VerifyTestToken(LSigner, CreateTestToken(LSigner)));
end;

procedure TTestRSAPSSSigners.TestPS256_TamperedTokenRejected;
var LSigner: IJWTSigner;
begin
  LSigner := TRSAPSSJWTSigner.CreateFromFiles(JWT_PS256,
    TPath.Combine(KeysPath, 'rsa/private.pem'), TPath.Combine(KeysPath, 'rsa/public.pem'));
  Assert.IsFalse(VerifyTestToken(LSigner, TamperToken(CreateTestToken(LSigner))));
end;

procedure TTestRSAPSSSigners.TestPS256_UsesSameRSAKeys;
var
  LRSASigner, LPSSSigner: IJWTSigner;
  LToken: string;
begin
  // PS256 uses the same RSA key files as RS256 (different padding scheme)
  LRSASigner := TRSAJWTSigner.CreateFromFiles(JWT_RS256,
    TPath.Combine(KeysPath, 'rsa/private.pem'), TPath.Combine(KeysPath, 'rsa/public.pem'));
  LPSSSigner := TRSAPSSJWTSigner.CreateFromFiles(JWT_PS256,
    TPath.Combine(KeysPath, 'rsa/private.pem'), TPath.Combine(KeysPath, 'rsa/public.pem'));
  // But tokens are NOT interchangeable (different algorithms in header)
  LToken := CreateTestToken(LRSASigner);
  Assert.IsFalse(VerifyTestToken(LPSSSigner, LToken),
    'RS256 token should not verify with PS256 signer (algorithm mismatch)');
end;


// ============================================================
// TTestECDSASigners
// ============================================================

function TTestECDSASigners.KeysPath: string;
begin
  Result := GetTestKeysPath;
end;

procedure TTestECDSASigners.TestES256_SignAndVerify;
var LSigner: IJWTSigner; LIssuer, LSubject, LRole: string;
begin
  LSigner := TECDSAJWTSigner.CreateFromFiles(JWT_ES256,
    TPath.Combine(KeysPath, 'ec/es256_private.pem'), TPath.Combine(KeysPath, 'ec/es256_public.pem'));
  Assert.IsTrue(VerifyAndGetClaims(LSigner, CreateTestToken(LSigner), LIssuer, LSubject, LRole));
  Assert.AreEqual('dmvc-test', LIssuer);
  Assert.AreEqual('admin', LRole);
end;

procedure TTestECDSASigners.TestES384_SignAndVerify;
var LSigner: IJWTSigner;
begin
  LSigner := TECDSAJWTSigner.CreateFromFiles(JWT_ES384,
    TPath.Combine(KeysPath, 'ec/es384_private.pem'), TPath.Combine(KeysPath, 'ec/es384_public.pem'));
  Assert.IsTrue(VerifyTestToken(LSigner, CreateTestToken(LSigner)));
end;

procedure TTestECDSASigners.TestES512_SignAndVerify;
var LSigner: IJWTSigner;
begin
  LSigner := TECDSAJWTSigner.CreateFromFiles(JWT_ES512,
    TPath.Combine(KeysPath, 'ec/es512_private.pem'), TPath.Combine(KeysPath, 'ec/es512_public.pem'));
  Assert.IsTrue(VerifyTestToken(LSigner, CreateTestToken(LSigner)));
end;

procedure TTestECDSASigners.TestES256_TamperedTokenRejected;
var LSigner: IJWTSigner;
begin
  LSigner := TECDSAJWTSigner.CreateFromFiles(JWT_ES256,
    TPath.Combine(KeysPath, 'ec/es256_private.pem'), TPath.Combine(KeysPath, 'ec/es256_public.pem'));
  Assert.IsFalse(VerifyTestToken(LSigner, TamperToken(CreateTestToken(LSigner))));
end;

procedure TTestECDSASigners.TestES256_WrongCurveKeyRejected;
var LSignSigner, LVerifySigner: IJWTSigner; LToken: string;
begin
  LSignSigner := TECDSAJWTSigner.CreateFromFiles(JWT_ES256,
    TPath.Combine(KeysPath, 'ec/es256_private.pem'), TPath.Combine(KeysPath, 'ec/es256_public.pem'));
  LToken := CreateTestToken(LSignSigner);
  // Verify with P-384 public key (wrong curve)
  LVerifySigner := TECDSAJWTSigner.Create(JWT_ES256, '',
    TFile.ReadAllText(TPath.Combine(KeysPath, 'ec/es384_public.pem')));
  Assert.IsFalse(VerifyTestToken(LVerifySigner, LToken));
end;


// ============================================================
// TTestEdDSASigner
// ============================================================

function TTestEdDSASigner.KeysPath: string;
begin
  Result := GetTestKeysPath;
end;

procedure TTestEdDSASigner.TestEdDSA_SignAndVerify;
var LSigner: IJWTSigner;
begin
  LSigner := TEdDSAJWTSigner.CreateFromFiles(
    TPath.Combine(KeysPath, 'ed25519/private.pem'), TPath.Combine(KeysPath, 'ed25519/public.pem'));
  Assert.IsTrue(VerifyTestToken(LSigner, CreateTestToken(LSigner)));
end;

procedure TTestEdDSASigner.TestEdDSA_TamperedTokenRejected;
var LSigner: IJWTSigner;
begin
  LSigner := TEdDSAJWTSigner.CreateFromFiles(
    TPath.Combine(KeysPath, 'ed25519/private.pem'), TPath.Combine(KeysPath, 'ed25519/public.pem'));
  Assert.IsFalse(VerifyTestToken(LSigner, TamperToken(CreateTestToken(LSigner))));
end;

procedure TTestEdDSASigner.TestEdDSA_ClaimsRoundTrip;
var LSigner: IJWTSigner; LIssuer, LSubject, LRole: string;
begin
  LSigner := TEdDSAJWTSigner.CreateFromFiles(
    TPath.Combine(KeysPath, 'ed25519/private.pem'), TPath.Combine(KeysPath, 'ed25519/public.pem'));
  Assert.IsTrue(VerifyAndGetClaims(LSigner, CreateTestToken(LSigner), LIssuer, LSubject, LRole));
  Assert.AreEqual('dmvc-test', LIssuer);
  Assert.AreEqual('testuser', LSubject);
  Assert.AreEqual('admin', LRole);
end;


// ============================================================
// TTestJWTSignerSecurity
// ============================================================

function TTestJWTSignerSecurity.KeysPath: string;
begin
  Result := GetTestKeysPath;
end;

procedure TTestJWTSignerSecurity.TestAlgorithmMismatch_HS256_vs_RS256;
var
  LHMACSigner, LRSASigner: IJWTSigner;
  LToken: string;
begin
  LHMACSigner := THMACJWTSigner.Create(HMAC_HS256, 'secret');
  LRSASigner := TRSAJWTSigner.CreateFromFiles(JWT_RS256,
    TPath.Combine(KeysPath, 'rsa/private.pem'), TPath.Combine(KeysPath, 'rsa/public.pem'));
  LToken := CreateTestToken(LHMACSigner);
  Assert.IsFalse(VerifyTestToken(LRSASigner, LToken),
    'HS256 token must not verify with RS256 signer');
end;

procedure TTestJWTSignerSecurity.TestAlgorithmMismatch_RS256_vs_ES256;
var
  LRSASigner, LECSigner: IJWTSigner;
  LToken: string;
begin
  LRSASigner := TRSAJWTSigner.CreateFromFiles(JWT_RS256,
    TPath.Combine(KeysPath, 'rsa/private.pem'), TPath.Combine(KeysPath, 'rsa/public.pem'));
  LECSigner := TECDSAJWTSigner.CreateFromFiles(JWT_ES256,
    TPath.Combine(KeysPath, 'ec/es256_private.pem'), TPath.Combine(KeysPath, 'ec/es256_public.pem'));
  LToken := CreateTestToken(LRSASigner);
  Assert.IsFalse(VerifyTestToken(LECSigner, LToken),
    'RS256 token must not verify with ES256 signer');
end;

procedure TTestJWTSignerSecurity.TestNoPrivateKey_SignRaisesException;
begin
  Assert.WillRaise(
    procedure
    var LSigner: IJWTSigner;
    begin
      LSigner := TRSAJWTSigner.CreateVerifyOnly(JWT_RS256,
        TFile.ReadAllText(TPath.Combine(KeysPath, 'rsa/public.pem')));
      CreateTestToken(LSigner);
    end,
    EMVCJWTSignerException);
end;

procedure TTestJWTSignerSecurity.TestNoPublicKey_VerifyRaisesException;
begin
  Assert.WillRaise(
    procedure
    var LSigner: IJWTSigner; LToken: string;
    begin
      LSigner := TRSAJWTSigner.CreateSignOnly(JWT_RS256,
        TFile.ReadAllText(TPath.Combine(KeysPath, 'rsa/private.pem')));
      LToken := CreateTestToken(LSigner);
      VerifyTestToken(LSigner, LToken);
    end,
    EMVCJWTSignerException);
end;

procedure TTestJWTSignerSecurity.TestInvalidAlgorithm_RaisesException;
begin
  Assert.WillRaise(
    procedure begin TRSAJWTSigner.Create('XX999', 'key', 'key'); end,
    EMVCJWTSignerException);
  Assert.WillRaise(
    procedure begin TRSAPSSJWTSigner.Create('RS256', 'key', 'key'); end,
    EMVCJWTSignerException);
  Assert.WillRaise(
    procedure begin TECDSAJWTSigner.Create('RS256', 'key', 'key'); end,
    EMVCJWTSignerException);
end;

procedure TTestJWTSignerSecurity.TestEmptyKeys_RaisesException;
begin
  Assert.WillRaise(
    procedure begin TRSAJWTSigner.Create(JWT_RS256, '', ''); end,
    EMVCJWTSignerException);
  Assert.WillRaise(
    procedure begin TECDSAJWTSigner.Create(JWT_ES256, '', ''); end,
    EMVCJWTSignerException);
  Assert.WillRaise(
    procedure begin TEdDSAJWTSigner.Create('', ''); end,
    EMVCJWTSignerException);
end;

{ TTestJWTSignatureTamper }

function SameBytes(const A, B: TBytes): Boolean;
var
  I: Integer;
begin
  Result := Length(A) = Length(B);
  if Result then
    for I := 0 to High(A) do
      if A[I] <> B[I] then
        Exit(False);
end;

procedure TTestJWTSignatureTamper.TamperTokenAlwaysAltersDecodedSignature;
var
  LSig, LOrig, LNew: TBytes;
  LSeg, LTampered: string;
  B, I: Integer;
begin
  // Sweep every possible final signature byte. URLSafeB64encode/Decode round-trip
  // the bytes, so LOrig is the genuine signature; after TamperToken the decoded
  // signature must differ. The old helper produced an identical signature for
  // B = $5D, which this loop would catch.
  SetLength(LSig, 256);
  for B := 0 to 255 do
  begin
    for I := 0 to 254 do
      LSig[I] := Byte(I);
    LSig[255] := Byte(B);
    LSeg := URLSafeB64encode(LSig, False);
    LOrig := URLSafeB64DecodeBytes(LSeg);
    LTampered := TamperToken('aaaa.bbbb.' + LSeg);
    LNew := URLSafeB64DecodeBytes(LTampered.Split(['.'])[2]);
    Assert.IsFalse(SameBytes(LOrig, LNew),
      Format('TamperToken left the signature unchanged for final byte $%.2x', [B]));
  end;
end;

procedure TTestJWTSignatureTamper.RS256SingleByteSignatureCorruptionIsRejected;
var
  LSigner: IJWTSigner;
  LToken, LCorrupted: string;
  LParts: TArray<string>;
  LSig: TBytes;
begin
  LSigner := TRSAJWTSigner.CreateFromFiles(JWT_RS256,
    TPath.Combine(GetTestKeysPath, 'rsa/private.pem'),
    TPath.Combine(GetTestKeysPath, 'rsa/public.pem'));
  LToken := CreateTestToken(LSigner);
  Assert.IsTrue(VerifyTestToken(LSigner, LToken),
    'sanity: a freshly signed token must verify');

  // Deterministically corrupt one signature byte and re-encode. Flipping byte 0
  // (xor $FF) always changes the signature, so verification must always fail —
  // unlike a base64-tail edit, this can never be a no-op.
  LParts := LToken.Split(['.']);
  LSig := URLSafeB64DecodeBytes(LParts[2]);
  LSig[0] := LSig[0] xor $FF;
  LCorrupted := LParts[0] + '.' + LParts[1] + '.' + URLSafeB64encode(LSig, False);

  Assert.IsFalse(VerifyTestToken(LSigner, LCorrupted),
    'a token with one corrupted signature byte must be rejected');
end;

{ TTestJWTInsecureSecret }

procedure TTestJWTInsecureSecret.ShippedDefaultSecretIsRejected;
begin
  Assert.WillRaise(
    procedure
    begin
      TJWT.Create(MVC_JWT_INSECURE_DEFAULT_SECRET, 0).Free;
    end, EMVCJWTException);
end;

procedure TTestJWTInsecureSecret.EmptySecretIsRejected;
begin
  Assert.WillRaise(
    procedure
    begin
      TJWT.Create('', 0).Free;
    end, EMVCJWTException);
end;

procedure TTestJWTInsecureSecret.OwnSecretIsAccepted;
var
  LJWT: TJWT;
begin
  LJWT := TJWT.Create('a_secret_of_my_own_2f47', 0);
  try
    Assert.IsNotEmpty(LJWT.GetToken);
  finally
    LJWT.Free;
  end;
end;

procedure TTestJWTInsecureSecret.MiddlewareConstructorRejectsShippedDefaultSecret;
begin
  Assert.WillRaise(
    procedure
    var
      LMiddleware: IMVCMiddleware;
    begin
      LMiddleware := TMVCJWTAuthenticationMiddleware.Create(
        nil, nil, MVC_JWT_INSECURE_DEFAULT_SECRET);
    end, EMVCJWTException);
  // Control: the raise above must come from the secret, not from the nil
  // handler/claims-setup passed to keep the test small.
  Assert.WillNotRaiseAny(
    procedure
    var
      LMiddleware: IMVCMiddleware;
    begin
      LMiddleware := TMVCJWTAuthenticationMiddleware.Create(
        nil, nil, 'a_secret_of_my_own_2f47');
    end);
end;

procedure TTestJWTInsecureSecret.EndpointFilterFactoryRejectsShippedDefaultSecret;
begin
  Assert.WillRaise(
    procedure
    var
      LFilter: TMVCEndpointFilter;
    begin
      LFilter := MVCFramework.Filters.JWT(
        nil, nil, MVC_JWT_INSECURE_DEFAULT_SECRET);
    end, EMVCJWTException);
  Assert.WillNotRaiseAny(
    procedure
    var
      LFilter: TMVCEndpointFilter;
    begin
      LFilter := MVCFramework.Filters.JWT(
        nil, nil, 'a_secret_of_my_own_2f47');
    end);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestHMACSigners);
  TDUnitX.RegisterTestFixture(TTestRSASigners);
  TDUnitX.RegisterTestFixture(TTestRSAPSSSigners);
  TDUnitX.RegisterTestFixture(TTestECDSASigners);
  TDUnitX.RegisterTestFixture(TTestEdDSASigner);
  TDUnitX.RegisterTestFixture(TTestJWTSignerSecurity);
  TDUnitX.RegisterTestFixture(TTestJWTSignatureTamper);
  TDUnitX.RegisterTestFixture(TTestJWTInsecureSecret);

end.
