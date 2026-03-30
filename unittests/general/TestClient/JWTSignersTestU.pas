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

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.DateUtils,
  MVCFramework.HMAC,
  MVCFramework.JWT.RSA;

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
begin
  // Tamper with the signature part (last segment) to ensure
  // the header and payload JSON remain valid but verification fails.
  LParts := AToken.Split(['.']);
  if Length(LParts) = 3 then
  begin
    // Flip a character near the end of the signature
    LParts[2] := LParts[2].Substring(0, Length(LParts[2]) - 2) + 'XX';
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

initialization
  TDUnitX.RegisterTestFixture(TTestHMACSigners);
  TDUnitX.RegisterTestFixture(TTestRSASigners);
  TDUnitX.RegisterTestFixture(TTestRSAPSSSigners);
  TDUnitX.RegisterTestFixture(TTestECDSASigners);
  TDUnitX.RegisterTestFixture(TTestEdDSASigner);
  TDUnitX.RegisterTestFixture(TTestJWTSignerSecurity);

end.
