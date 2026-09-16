// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ***************************************************************************
//
// Asymmetric JWT Signers for DMVCFramework.
// Uses TaurusTLS (OpenSSL 1.1.1+) for all cryptographic operations.
//
// Supported algorithms:
//   RS256, RS384, RS512    - RSA PKCS#1 v1.5
//   PS256, PS384, PS512    - RSA-PSS (more secure than RS*)
//   ES256, ES384, ES512    - ECDSA (Elliptic Curve)
//   EdDSA (Ed25519)        - Edwards Curve (newest, fastest)
//
// IMPORTANT: This unit requires modern OpenSSL (1.1.1+ or 3.x) via TaurusTLS.
// It does NOT use the old OpenSSL 1.0.x libraries bundled with Delphi/Indy.
//
// This unit is NOT included in the framework packages (dmvcframeworkRT).
// Add it to your project's uses clause only if you need asymmetric JWT signing.
// Requires TaurusTLS source in the search path and OpenSSL DLLs at runtime.
//
// ***************************************************************************

unit MVCFramework.JWT.RSA;

{$I dmvcframework.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  MVCFramework.JWT;

const
  // RSA PKCS#1 v1.5
  JWT_RS256 = 'RS256';
  JWT_RS384 = 'RS384';
  JWT_RS512 = 'RS512';
  // RSA-PSS (recommended over RS* for new applications)
  JWT_PS256 = 'PS256';
  JWT_PS384 = 'PS384';
  JWT_PS512 = 'PS512';
  // ECDSA (Elliptic Curve DSA)
  JWT_ES256 = 'ES256';   // P-256 + SHA-256
  JWT_ES384 = 'ES384';   // P-384 + SHA-384
  JWT_ES512 = 'ES512';   // P-521 + SHA-512
  // EdDSA (Edwards-curve Digital Signature Algorithm)
  JWT_EdDSA = 'EdDSA';   // Ed25519

type
  EMVCJWTSignerException = class(Exception);
  // Keep backward compatibility
  EMVCJWTRSAException = EMVCJWTSignerException;

  /// <summary>
  /// RSA-based JWT signer (RS256, RS384, RS512).
  /// Uses RSA PKCS#1 v1.5 signature scheme.
  /// Signs with a private key, verifies with a public key.
  /// </summary>
  TRSAJWTSigner = class(TInterfacedObject, IJWTSigner)
  private
    FAlgorithm: string;
    FPrivateKeyPEM: string;
    FPublicKeyPEM: string;
  protected
    class var FOpenSSLLoaded: Boolean;
    class procedure EnsureOpenSSL; static;
    procedure DoSign(const AInput: TBytes; out ASignature: TBytes; APSS: Boolean = False); virtual;
    function DoVerify(const AInput, ASignature: TBytes; APSS: Boolean = False): Boolean; virtual;
  public
    constructor Create(const AAlgorithm: string;
      const APrivateKeyPEM: string; const APublicKeyPEM: string);
    class function CreateFromFiles(const AAlgorithm: string;
      const APrivateKeyFile: string;
      const APublicKeyFile: string = ''): TRSAJWTSigner;
    class function CreateSignOnly(const AAlgorithm: string;
      const APrivateKeyPEM: string): TRSAJWTSigner;
    class function CreateVerifyOnly(const AAlgorithm: string;
      const APublicKeyPEM: string): TRSAJWTSigner;
    function GetAlgorithm: string;
    function Sign(const Input: string): TBytes;
    function Verify(const Input: string; const Signature: TBytes): Boolean;
  end;

  /// <summary>
  /// RSA-PSS JWT signer (PS256, PS384, PS512).
  /// Uses RSA Probabilistic Signature Scheme - more secure than PKCS#1 v1.5.
  /// Recommended for new applications over RS256/RS384/RS512.
  /// </summary>
  TRSAPSSJWTSigner = class(TRSAJWTSigner)
  public
    constructor Create(const AAlgorithm: string;
      const APrivateKeyPEM: string; const APublicKeyPEM: string);
    class function CreateFromFiles(const AAlgorithm: string;
      const APrivateKeyFile: string;
      const APublicKeyFile: string = ''): TRSAPSSJWTSigner;
    function Sign(const Input: string): TBytes;
    function Verify(const Input: string; const Signature: TBytes): Boolean;
  end;

  /// <summary>
  /// ECDSA JWT signer (ES256, ES384, ES512).
  /// Uses Elliptic Curve keys - much smaller keys than RSA for equivalent security.
  /// ES256 uses P-256, ES384 uses P-384, ES512 uses P-521.
  /// </summary>
  TECDSAJWTSigner = class(TInterfacedObject, IJWTSigner)
  private
    FAlgorithm: string;
    FPrivateKeyPEM: string;
    FPublicKeyPEM: string;
  public
    constructor Create(const AAlgorithm: string;
      const APrivateKeyPEM: string; const APublicKeyPEM: string);
    class function CreateFromFiles(const AAlgorithm: string;
      const APrivateKeyFile: string;
      const APublicKeyFile: string = ''): TECDSAJWTSigner;
    class function CreateSignOnly(const AAlgorithm: string;
      const APrivateKeyPEM: string): TECDSAJWTSigner;
    class function CreateVerifyOnly(const AAlgorithm: string;
      const APublicKeyPEM: string): TECDSAJWTSigner;
    function GetAlgorithm: string;
    function Sign(const Input: string): TBytes;
    function Verify(const Input: string; const Signature: TBytes): Boolean;
  end;

  /// <summary>
  /// EdDSA JWT signer (Ed25519).
  /// Edwards-curve Digital Signature Algorithm - newest and fastest.
  /// Ed25519 keys are only 32 bytes, signatures are 64 bytes.
  /// Note: EdDSA uses a built-in hash, no separate digest is needed.
  /// </summary>
  TEdDSAJWTSigner = class(TInterfacedObject, IJWTSigner)
  private
    FPrivateKeyPEM: string;
    FPublicKeyPEM: string;
  public
    constructor Create(const APrivateKeyPEM: string; const APublicKeyPEM: string);
    class function CreateFromFiles(
      const APrivateKeyFile: string;
      const APublicKeyFile: string = ''): TEdDSAJWTSigner;
    class function CreateSignOnly(const APrivateKeyPEM: string): TEdDSAJWTSigner;
    class function CreateVerifyOnly(const APublicKeyPEM: string): TEdDSAJWTSigner;
    function GetAlgorithm: string;
    function Sign(const Input: string): TBytes;
    function Verify(const Input: string; const Signature: TBytes): Boolean;
  end;

implementation

uses
  System.StrUtils,
  IdGlobal,
  IdCTypes,
  TaurusTLSLoader,
  TaurusTLSHeaders_types,
  TaurusTLSHeaders_evp,
  TaurusTLSHeaders_pem,
  TaurusTLSHeaders_bio,
  TaurusTLSHeaders_rsa;

// ============================================================
// Helper: load PEM key from string via BIO
// ============================================================
type
  TKeyKind = (kkPrivate, kkPublic);

function LoadPEMKey(const APEM: string; AKind: TKeyKind): PEVP_PKEY;
var
  LBytes: TIdBytes;
  LBio: PBIO;
begin
  LBytes := ToBytes(APEM, IndyTextEncoding_UTF8);
  LBio := BIO_new_mem_buf(LBytes[0], Length(LBytes));
  if not Assigned(LBio) then
    raise EMVCJWTSignerException.Create('Failed to create BIO for key.');
  try
    Result := nil;
    case AKind of
      kkPrivate: Result := PEM_read_bio_PrivateKey(LBio, nil, nil, nil);
      kkPublic:  Result := PEM_read_bio_PUBKEY(LBio, nil, nil, nil);
    end;
    if not Assigned(Result) then
      raise EMVCJWTSignerException.CreateFmt('Failed to load %s key from PEM. Check key format.',
        [IfThen(AKind = kkPrivate, 'private', 'public')]);
  finally
    BIO_free(LBio);
  end;
end;

function LoadPEMFile(const AFileName: string): string;
begin
  if AFileName.IsEmpty then
    Result := ''
  else
    Result := TFile.ReadAllText(AFileName, TEncoding.UTF8);
end;

// Maps algorithm name to EVP digest
function AlgorithmToDigest(const AAlgorithm: string): PEVP_MD;
begin
  if SameText(AAlgorithm, JWT_RS256) or SameText(AAlgorithm, JWT_PS256) or SameText(AAlgorithm, JWT_ES256) then
    Result := EVP_sha256()
  else if SameText(AAlgorithm, JWT_RS384) or SameText(AAlgorithm, JWT_PS384) or SameText(AAlgorithm, JWT_ES384) then
    Result := EVP_sha384()
  else if SameText(AAlgorithm, JWT_RS512) or SameText(AAlgorithm, JWT_PS512) or SameText(AAlgorithm, JWT_ES512) then
    Result := EVP_sha512()
  else
    raise EMVCJWTSignerException.CreateFmt('No digest for algorithm: %s', [AAlgorithm]);
end;

// ECDSA signatures: OpenSSL produces DER-encoded ASN.1, but JWT uses
// raw R||S concatenation (IEEE P1363). These helpers convert between formats.
// For ES256: R and S are each 32 bytes; ES384: 48 bytes; ES512: 66 bytes.

function ECComponentSize(const AAlgorithm: string): Integer;
begin
  if SameText(AAlgorithm, JWT_ES256) then
    Result := 32
  else if SameText(AAlgorithm, JWT_ES384) then
    Result := 48
  else if SameText(AAlgorithm, JWT_ES512) then
    Result := 66
  else
    raise EMVCJWTSignerException.CreateFmt('Unknown EC algorithm: %s', [AAlgorithm]);
end;

// Parse DER length field (handles short and long forms)
function ParseDERLength(const ADER: TBytes; var Offset: Integer): Integer;
begin
  if ADER[Offset] <= 127 then
  begin
    Result := ADER[Offset];
    Inc(Offset);
  end
  else if ADER[Offset] = $81 then
  begin
    Inc(Offset);
    Result := ADER[Offset];
    Inc(Offset);
  end
  else if ADER[Offset] = $82 then
  begin
    Inc(Offset);
    Result := (ADER[Offset] shl 8) or ADER[Offset + 1];
    Inc(Offset, 2);
  end
  else
    raise EMVCJWTSignerException.Create('Unsupported DER length encoding');
end;

// Parse a DER integer: skip tag (0x02) + length, return value bytes
function ParseDERInteger(const ADER: TBytes; var Offset: Integer): TBytes;
var
  LLen: Integer;
begin
  if ADER[Offset] <> $02 then
    raise EMVCJWTSignerException.Create('Invalid DER: expected INTEGER tag');
  Inc(Offset); // skip tag
  LLen := ParseDERLength(ADER, Offset);
  SetLength(Result, LLen);
  Move(ADER[Offset], Result[0], LLen);
  Inc(Offset, LLen);
end;

// Convert OpenSSL DER-encoded ECDSA signature to JWT R||S format
function DERToJWTSignature(const ADER: TBytes; AComponentSize: Integer): TBytes;
var
  LOffset, LSrcOfs, LCopyLen: Integer;
  LR, LS: TBytes;
begin
  SetLength(Result, AComponentSize * 2);
  FillChar(Result[0], Length(Result), 0);
  LOffset := 0;
  // Skip SEQUENCE tag + length
  if ADER[LOffset] <> $30 then
    raise EMVCJWTSignerException.Create('Invalid DER: expected SEQUENCE tag');
  Inc(LOffset);
  ParseDERLength(ADER, LOffset); // skip sequence length

  LR := ParseDERInteger(ADER, LOffset);
  LS := ParseDERInteger(ADER, LOffset);

  // Copy R (right-aligned, skip leading zero if present)
  LSrcOfs := 0;
  if (Length(LR) > AComponentSize) and (LR[0] = 0) then
    LSrcOfs := 1;
  LCopyLen := Length(LR) - LSrcOfs;
  Move(LR[LSrcOfs], Result[AComponentSize - LCopyLen], LCopyLen);

  // Copy S (right-aligned)
  LSrcOfs := 0;
  if (Length(LS) > AComponentSize) and (LS[0] = 0) then
    LSrcOfs := 1;
  LCopyLen := Length(LS) - LSrcOfs;
  Move(LS[LSrcOfs], Result[AComponentSize + AComponentSize - LCopyLen], LCopyLen);
end;

// Convert JWT R||S format to DER-encoded ECDSA signature for OpenSSL
function JWTSignatureToDER(const AJWT: TBytes; AComponentSize: Integer): TBytes;
var
  LR, LS: TBytes;
  LDer: TBytes;
  LPos: Integer;

  procedure WriteDERInteger(const AValue: TBytes; var ADest: TBytes; var APos: Integer);
  var
    LStart, LLen: Integer;
  begin
    // Skip leading zeros
    LStart := 0;
    while (LStart < Length(AValue) - 1) and (AValue[LStart] = 0) do
      Inc(LStart);
    LLen := Length(AValue) - LStart;
    // If high bit set, prepend a zero byte
    if AValue[LStart] >= $80 then
    begin
      ADest[APos] := $02; Inc(APos);
      ADest[APos] := LLen + 1; Inc(APos);
      ADest[APos] := $00; Inc(APos);
    end
    else
    begin
      ADest[APos] := $02; Inc(APos);
      ADest[APos] := LLen; Inc(APos);
    end;
    Move(AValue[LStart], ADest[APos], LLen);
    Inc(APos, LLen);
  end;

begin
  SetLength(LR, AComponentSize);
  SetLength(LS, AComponentSize);
  Move(AJWT[0], LR[0], AComponentSize);
  Move(AJWT[AComponentSize], LS[0], AComponentSize);

  // Max DER size: 3 (seq header, long form) + 2*(2 tag+len + 1 padding + componentSize)
  SetLength(LDer, 7 + AComponentSize * 2 + 2);
  LPos := 3; // reserve space for SEQUENCE header (max 3 bytes)
  WriteDERInteger(LR, LDer, LPos);
  WriteDERInteger(LS, LDer, LPos);
  // Write SEQUENCE header
  if LPos - 3 > 127 then
  begin
    // Long form: $30 $81 LEN
    LDer[0] := $30;
    LDer[1] := $81;
    LDer[2] := LPos - 3;
    SetLength(LDer, LPos);
    Result := LDer;
  end
  else
  begin
    // Short form: $30 LEN (shift content left by 1)
    LDer[1] := $30;
    LDer[2] := LPos - 3;
    SetLength(LDer, LPos);
    Result := Copy(LDer, 1, LPos - 1);
  end;
end;


// ============================================================
// TRSAJWTSigner - RS256, RS384, RS512
// ============================================================

constructor TRSAJWTSigner.Create(const AAlgorithm, APrivateKeyPEM, APublicKeyPEM: string);
begin
  inherited Create;
  if not (SameText(AAlgorithm, JWT_RS256) or SameText(AAlgorithm, JWT_RS384) or SameText(AAlgorithm, JWT_RS512)) then
    raise EMVCJWTSignerException.CreateFmt('Unsupported RSA algorithm: %s. Use RS256, RS384, or RS512.', [AAlgorithm]);
  if APrivateKeyPEM.IsEmpty and APublicKeyPEM.IsEmpty then
    raise EMVCJWTSignerException.Create('At least one key (private or public) must be provided.');
  FAlgorithm := AAlgorithm;
  FPrivateKeyPEM := APrivateKeyPEM;
  FPublicKeyPEM := APublicKeyPEM;
end;

class function TRSAJWTSigner.CreateFromFiles(const AAlgorithm, APrivateKeyFile, APublicKeyFile: string): TRSAJWTSigner;
begin
  Result := TRSAJWTSigner.Create(AAlgorithm, LoadPEMFile(APrivateKeyFile), LoadPEMFile(APublicKeyFile));
end;

class function TRSAJWTSigner.CreateSignOnly(const AAlgorithm, APrivateKeyPEM: string): TRSAJWTSigner;
begin
  Result := TRSAJWTSigner.Create(AAlgorithm, APrivateKeyPEM, '');
end;

class function TRSAJWTSigner.CreateVerifyOnly(const AAlgorithm, APublicKeyPEM: string): TRSAJWTSigner;
begin
  Result := TRSAJWTSigner.Create(AAlgorithm, '', APublicKeyPEM);
end;

class procedure TRSAJWTSigner.EnsureOpenSSL;
begin
  if not FOpenSSLLoaded then
  begin
    if not GetOpenSSLLoader.Load then
      raise EMVCJWTSignerException.Create(
        'Cannot load OpenSSL libraries. ' +
        'Ensure libcrypto and libssl DLLs (1.1.1+ or 3.x) are in the application directory or system PATH.');
    FOpenSSLLoaded := True;
  end;
end;

function TRSAJWTSigner.GetAlgorithm: string;
begin
  Result := FAlgorithm;
end;

procedure TRSAJWTSigner.DoSign(const AInput: TBytes; out ASignature: TBytes; APSS: Boolean);
var
  LPKey: PEVP_PKEY;
  LCtx: PEVP_MD_CTX;
  LPKeyCtx: PEVP_PKEY_CTX;
  LSigLen: TIdC_SIZET;
begin
  if FPrivateKeyPEM.IsEmpty then
    raise EMVCJWTSignerException.Create('Cannot sign: no private key configured.');
  EnsureOpenSSL;

  LPKey := LoadPEMKey(FPrivateKeyPEM, kkPrivate);
  try
    LCtx := EVP_MD_CTX_new();
    if not Assigned(LCtx) then
      raise EMVCJWTSignerException.Create('Failed to create EVP_MD_CTX.');
    try
      LPKeyCtx := nil;
      if EVP_DigestSignInit(LCtx, @LPKeyCtx, AlgorithmToDigest(FAlgorithm), nil, LPKey) <> 1 then
        raise EMVCJWTSignerException.Create('EVP_DigestSignInit failed.');

      // Set PSS padding if requested
      if APSS and Assigned(LPKeyCtx) then
      begin
        if EVP_PKEY_CTX_ctrl(LPKeyCtx, EVP_PKEY_RSA, EVP_PKEY_OP_SIGN,
          EVP_PKEY_CTRL_RSA_PADDING, RSA_PKCS1_PSS_PADDING, nil) <= 0 then
          raise EMVCJWTSignerException.Create('Failed to set PSS padding.');
      end;

      if EVP_DigestUpdate(LCtx, @AInput[0], Length(AInput)) <> 1 then
        raise EMVCJWTSignerException.Create('EVP_DigestUpdate failed.');

      LSigLen := 0;
      if EVP_DigestSignFinal(LCtx, nil, @LSigLen) <> 1 then
        raise EMVCJWTSignerException.Create('EVP_DigestSignFinal (get length) failed.');

      SetLength(ASignature, LSigLen);
      if EVP_DigestSignFinal(LCtx, @ASignature[0], @LSigLen) <> 1 then
        raise EMVCJWTSignerException.Create('EVP_DigestSignFinal (sign) failed.');
      SetLength(ASignature, LSigLen);
    finally
      EVP_MD_CTX_free(LCtx);
    end;
  finally
    EVP_PKEY_free(LPKey);
  end;
end;

function TRSAJWTSigner.DoVerify(const AInput, ASignature: TBytes; APSS: Boolean): Boolean;
var
  LPKey: PEVP_PKEY;
  LCtx: PEVP_MD_CTX;
  LPKeyCtx: PEVP_PKEY_CTX;
begin
  if FPublicKeyPEM.IsEmpty then
    raise EMVCJWTSignerException.Create('Cannot verify: no public key configured.');
  EnsureOpenSSL;

  LPKey := LoadPEMKey(FPublicKeyPEM, kkPublic);
  try
    LCtx := EVP_MD_CTX_new();
    if not Assigned(LCtx) then
      raise EMVCJWTSignerException.Create('Failed to create EVP_MD_CTX.');
    try
      LPKeyCtx := nil;
      if EVP_DigestVerifyInit(LCtx, @LPKeyCtx, AlgorithmToDigest(FAlgorithm), nil, LPKey) <> 1 then
        raise EMVCJWTSignerException.Create('EVP_DigestVerifyInit failed.');

      if APSS and Assigned(LPKeyCtx) then
      begin
        if EVP_PKEY_CTX_ctrl(LPKeyCtx, EVP_PKEY_RSA, EVP_PKEY_OP_VERIFY,
          EVP_PKEY_CTRL_RSA_PADDING, RSA_PKCS1_PSS_PADDING, nil) <= 0 then
          raise EMVCJWTSignerException.Create('Failed to set PSS padding for verification.');
      end;

      if EVP_DigestUpdate(LCtx, @AInput[0], Length(AInput)) <> 1 then
        raise EMVCJWTSignerException.Create('EVP_DigestUpdate failed.');

      Result := EVP_DigestVerifyFinal(LCtx, @ASignature[0], Length(ASignature)) = 1;
    finally
      EVP_MD_CTX_free(LCtx);
    end;
  finally
    EVP_PKEY_free(LPKey);
  end;
end;

function TRSAJWTSigner.Sign(const Input: string): TBytes;
var
  LInputBytes: TIdBytes;
begin
  LInputBytes := ToBytes(Input, IndyTextEncoding_UTF8);
  DoSign(TBytes(LInputBytes), Result, False);
end;

function TRSAJWTSigner.Verify(const Input: string; const Signature: TBytes): Boolean;
var
  LInputBytes: TIdBytes;
begin
  LInputBytes := ToBytes(Input, IndyTextEncoding_UTF8);
  Result := DoVerify(TBytes(LInputBytes), Signature, False);
end;


// ============================================================
// TRSAPSSJWTSigner - PS256, PS384, PS512
// ============================================================

constructor TRSAPSSJWTSigner.Create(const AAlgorithm, APrivateKeyPEM, APublicKeyPEM: string);
begin
  inherited Create(JWT_RS256, APrivateKeyPEM, APublicKeyPEM); // base validates RSA key format
  if not (SameText(AAlgorithm, JWT_PS256) or SameText(AAlgorithm, JWT_PS384) or SameText(AAlgorithm, JWT_PS512)) then
    raise EMVCJWTSignerException.CreateFmt('Unsupported PSS algorithm: %s. Use PS256, PS384, or PS512.', [AAlgorithm]);
  FAlgorithm := AAlgorithm; // override the RS256 set by base
end;

class function TRSAPSSJWTSigner.CreateFromFiles(const AAlgorithm, APrivateKeyFile, APublicKeyFile: string): TRSAPSSJWTSigner;
begin
  Result := TRSAPSSJWTSigner.Create(AAlgorithm, LoadPEMFile(APrivateKeyFile), LoadPEMFile(APublicKeyFile));
end;

function TRSAPSSJWTSigner.Sign(const Input: string): TBytes;
var
  LInputBytes: TIdBytes;
begin
  LInputBytes := ToBytes(Input, IndyTextEncoding_UTF8);
  DoSign(TBytes(LInputBytes), Result, True); // PSS = True
end;

function TRSAPSSJWTSigner.Verify(const Input: string; const Signature: TBytes): Boolean;
var
  LInputBytes: TIdBytes;
begin
  LInputBytes := ToBytes(Input, IndyTextEncoding_UTF8);
  Result := DoVerify(TBytes(LInputBytes), Signature, True); // PSS = True
end;


// ============================================================
// TECDSAJWTSigner - ES256, ES384, ES512
// ============================================================

constructor TECDSAJWTSigner.Create(const AAlgorithm, APrivateKeyPEM, APublicKeyPEM: string);
begin
  inherited Create;
  if not (SameText(AAlgorithm, JWT_ES256) or SameText(AAlgorithm, JWT_ES384) or SameText(AAlgorithm, JWT_ES512)) then
    raise EMVCJWTSignerException.CreateFmt('Unsupported ECDSA algorithm: %s. Use ES256, ES384, or ES512.', [AAlgorithm]);
  if APrivateKeyPEM.IsEmpty and APublicKeyPEM.IsEmpty then
    raise EMVCJWTSignerException.Create('At least one key (private or public) must be provided.');
  FAlgorithm := AAlgorithm;
  FPrivateKeyPEM := APrivateKeyPEM;
  FPublicKeyPEM := APublicKeyPEM;
end;

class function TECDSAJWTSigner.CreateFromFiles(const AAlgorithm, APrivateKeyFile, APublicKeyFile: string): TECDSAJWTSigner;
begin
  Result := TECDSAJWTSigner.Create(AAlgorithm, LoadPEMFile(APrivateKeyFile), LoadPEMFile(APublicKeyFile));
end;

class function TECDSAJWTSigner.CreateSignOnly(const AAlgorithm, APrivateKeyPEM: string): TECDSAJWTSigner;
begin
  Result := TECDSAJWTSigner.Create(AAlgorithm, APrivateKeyPEM, '');
end;

class function TECDSAJWTSigner.CreateVerifyOnly(const AAlgorithm, APublicKeyPEM: string): TECDSAJWTSigner;
begin
  Result := TECDSAJWTSigner.Create(AAlgorithm, '', APublicKeyPEM);
end;

function TECDSAJWTSigner.GetAlgorithm: string;
begin
  Result := FAlgorithm;
end;

function TECDSAJWTSigner.Sign(const Input: string): TBytes;
var
  LInputBytes: TIdBytes;
  LPKey: PEVP_PKEY;
  LCtx: PEVP_MD_CTX;
  LDERSig: TBytes;
  LSigLen: TIdC_SIZET;
begin
  if FPrivateKeyPEM.IsEmpty then
    raise EMVCJWTSignerException.Create('Cannot sign: no private key configured.');
  TRSAJWTSigner.EnsureOpenSSL;

  LPKey := LoadPEMKey(FPrivateKeyPEM, kkPrivate);
  try
    LCtx := EVP_MD_CTX_new();
    if not Assigned(LCtx) then
      raise EMVCJWTSignerException.Create('Failed to create EVP_MD_CTX.');
    try
      if EVP_DigestSignInit(LCtx, nil, AlgorithmToDigest(FAlgorithm), nil, LPKey) <> 1 then
        raise EMVCJWTSignerException.Create('EVP_DigestSignInit failed for ECDSA.');

      LInputBytes := ToBytes(Input, IndyTextEncoding_UTF8);
      if EVP_DigestUpdate(LCtx, @LInputBytes[0], Length(LInputBytes)) <> 1 then
        raise EMVCJWTSignerException.Create('EVP_DigestUpdate failed.');

      LSigLen := 0;
      if EVP_DigestSignFinal(LCtx, nil, @LSigLen) <> 1 then
        raise EMVCJWTSignerException.Create('EVP_DigestSignFinal (get length) failed.');

      SetLength(LDERSig, LSigLen);
      if EVP_DigestSignFinal(LCtx, @LDERSig[0], @LSigLen) <> 1 then
        raise EMVCJWTSignerException.Create('EVP_DigestSignFinal (sign) failed.');
      SetLength(LDERSig, LSigLen);

      // Convert DER-encoded signature to JWT R||S format
      Result := DERToJWTSignature(LDERSig, ECComponentSize(FAlgorithm));
    finally
      EVP_MD_CTX_free(LCtx);
    end;
  finally
    EVP_PKEY_free(LPKey);
  end;
end;

function TECDSAJWTSigner.Verify(const Input: string; const Signature: TBytes): Boolean;
var
  LInputBytes: TIdBytes;
  LPKey: PEVP_PKEY;
  LCtx: PEVP_MD_CTX;
  LDERSig: TBytes;
begin
  if FPublicKeyPEM.IsEmpty then
    raise EMVCJWTSignerException.Create('Cannot verify: no public key configured.');
  TRSAJWTSigner.EnsureOpenSSL;

  // Convert JWT R||S format to DER for OpenSSL
  LDERSig := JWTSignatureToDER(Signature, ECComponentSize(FAlgorithm));

  LPKey := LoadPEMKey(FPublicKeyPEM, kkPublic);
  try
    LCtx := EVP_MD_CTX_new();
    if not Assigned(LCtx) then
      raise EMVCJWTSignerException.Create('Failed to create EVP_MD_CTX.');
    try
      if EVP_DigestVerifyInit(LCtx, nil, AlgorithmToDigest(FAlgorithm), nil, LPKey) <> 1 then
        raise EMVCJWTSignerException.Create('EVP_DigestVerifyInit failed for ECDSA.');

      LInputBytes := ToBytes(Input, IndyTextEncoding_UTF8);
      if EVP_DigestUpdate(LCtx, @LInputBytes[0], Length(LInputBytes)) <> 1 then
        raise EMVCJWTSignerException.Create('EVP_DigestUpdate failed.');

      Result := EVP_DigestVerifyFinal(LCtx, @LDERSig[0], Length(LDERSig)) = 1;
    finally
      EVP_MD_CTX_free(LCtx);
    end;
  finally
    EVP_PKEY_free(LPKey);
  end;
end;


// ============================================================
// TEdDSAJWTSigner - EdDSA (Ed25519)
// Ed25519 has the hash built-in, so digest is nil.
// Uses the one-shot EVP_DigestSign/Verify (no Update step).
// ============================================================

constructor TEdDSAJWTSigner.Create(const APrivateKeyPEM, APublicKeyPEM: string);
begin
  inherited Create;
  if APrivateKeyPEM.IsEmpty and APublicKeyPEM.IsEmpty then
    raise EMVCJWTSignerException.Create('At least one key (private or public) must be provided.');
  FPrivateKeyPEM := APrivateKeyPEM;
  FPublicKeyPEM := APublicKeyPEM;
end;

class function TEdDSAJWTSigner.CreateFromFiles(const APrivateKeyFile, APublicKeyFile: string): TEdDSAJWTSigner;
begin
  Result := TEdDSAJWTSigner.Create(LoadPEMFile(APrivateKeyFile), LoadPEMFile(APublicKeyFile));
end;

class function TEdDSAJWTSigner.CreateSignOnly(const APrivateKeyPEM: string): TEdDSAJWTSigner;
begin
  Result := TEdDSAJWTSigner.Create(APrivateKeyPEM, '');
end;

class function TEdDSAJWTSigner.CreateVerifyOnly(const APublicKeyPEM: string): TEdDSAJWTSigner;
begin
  Result := TEdDSAJWTSigner.Create('', APublicKeyPEM);
end;

function TEdDSAJWTSigner.GetAlgorithm: string;
begin
  Result := JWT_EdDSA;
end;

function TEdDSAJWTSigner.Sign(const Input: string): TBytes;
var
  LInputBytes: TIdBytes;
  LPKey: PEVP_PKEY;
  LCtx: PEVP_MD_CTX;
  LSigLen: TIdC_SIZET;
begin
  if FPrivateKeyPEM.IsEmpty then
    raise EMVCJWTSignerException.Create('Cannot sign: no private key configured.');
  TRSAJWTSigner.EnsureOpenSSL;

  LPKey := LoadPEMKey(FPrivateKeyPEM, kkPrivate);
  try
    LCtx := EVP_MD_CTX_new();
    if not Assigned(LCtx) then
      raise EMVCJWTSignerException.Create('Failed to create EVP_MD_CTX.');
    try
      // Ed25519: digest must be nil (hash is built into the algorithm)
      if EVP_DigestSignInit(LCtx, nil, nil, nil, LPKey) <> 1 then
        raise EMVCJWTSignerException.Create('EVP_DigestSignInit failed for EdDSA.');

      LInputBytes := ToBytes(Input, IndyTextEncoding_UTF8);

      // Ed25519 uses one-shot signing (no Update step)
      // Note: EVP_DigestSign declared with PEVP_CIPHER_CTX in TaurusTLS headers
      // is actually EVP_MD_CTX in OpenSSL - cast to work around the header mismatch.
      LSigLen := 0;
      if EVP_DigestSign(PEVP_CIPHER_CTX(LCtx), nil, @LSigLen, @LInputBytes[0], Length(LInputBytes)) <> 1 then
        raise EMVCJWTSignerException.Create('EVP_DigestSign (get length) failed.');

      SetLength(Result, LSigLen);
      if EVP_DigestSign(PEVP_CIPHER_CTX(LCtx), @Result[0], @LSigLen, @LInputBytes[0], Length(LInputBytes)) <> 1 then
        raise EMVCJWTSignerException.Create('EVP_DigestSign failed.');
      SetLength(Result, LSigLen);
    finally
      EVP_MD_CTX_free(LCtx);
    end;
  finally
    EVP_PKEY_free(LPKey);
  end;
end;

function TEdDSAJWTSigner.Verify(const Input: string; const Signature: TBytes): Boolean;
var
  LInputBytes: TIdBytes;
  LPKey: PEVP_PKEY;
  LCtx: PEVP_MD_CTX;
begin
  if FPublicKeyPEM.IsEmpty then
    raise EMVCJWTSignerException.Create('Cannot verify: no public key configured.');
  TRSAJWTSigner.EnsureOpenSSL;

  LPKey := LoadPEMKey(FPublicKeyPEM, kkPublic);
  try
    LCtx := EVP_MD_CTX_new();
    if not Assigned(LCtx) then
      raise EMVCJWTSignerException.Create('Failed to create EVP_MD_CTX.');
    try
      if EVP_DigestVerifyInit(LCtx, nil, nil, nil, LPKey) <> 1 then
        raise EMVCJWTSignerException.Create('EVP_DigestVerifyInit failed for EdDSA.');

      LInputBytes := ToBytes(Input, IndyTextEncoding_UTF8);

      // Ed25519 uses one-shot verification (no Update step)
      // Cast: see note in Sign about TaurusTLS header mismatch
      Result := EVP_DigestVerify(PEVP_CIPHER_CTX(LCtx), @Signature[0], Length(Signature),
        @LInputBytes[0], Length(LInputBytes)) = 1;
    finally
      EVP_MD_CTX_free(LCtx);
    end;
  finally
    EVP_PKEY_free(LPKey);
  end;
end;

end.
