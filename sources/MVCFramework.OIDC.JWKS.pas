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
// JWKS (JSON Web Key Set) client for OIDC ID token signature verification.
//
// This unit fetches public keys from a JWKS endpoint, converts JWK format
// to PEM, and creates the appropriate IJWTSigner for each key.
//
// Supported key types:
//   RSA  (kty=RSA)  -> RS256, RS384, RS512 via TRSAJWTSigner
//   EC   (kty=EC)   -> ES256, ES384, ES512 via TECDSAJWTSigner
//   OKP  (kty=OKP)  -> EdDSA (Ed25519) via TEdDSAJWTSigner
//
// IMPORTANT: This unit requires TaurusTLS (OpenSSL 1.1.1+) at runtime.
// It is NOT included in the framework packages (dmvcframeworkRT).
// Add it to your project's uses clause only when you need OIDC ID token
// signature verification via JWKS.
//
// Architecture:
//   MVCFramework.JWT.pas          <- IJWTSigner, IJWKSProvider (no TaurusTLS)
//   MVCFramework.Middleware.OIDC   <- uses IJWKSProvider interface (no TaurusTLS)
//   MVCFramework.OIDC.JWKS (this) <- implements IJWKSProvider (requires TaurusTLS)
//
// ***************************************************************************

unit MVCFramework.OIDC.JWKS;

{$I dmvcframework.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  MVCFramework.JWT,
  MVCFramework.JWT.RSA,
  MVCFramework.Commons,
  JsonDataObjects;

type
  EMVCJWKSException = class(Exception);

  /// <summary>
  /// Parsed JWK key with its PEM representation and metadata.
  /// </summary>
  TJWKKey = class
  private
    FKeyId: string;
    FAlgorithm: string;
    FKeyType: string;  // RSA, EC, OKP
    FPublicKeyPEM: string;
  public
    property KeyId: string read FKeyId;
    property Algorithm: string read FAlgorithm;
    property KeyType: string read FKeyType;
    property PublicKeyPEM: string read FPublicKeyPEM;
  end;

  /// <summary>
  /// JWKS client that fetches, caches, and provides JWT signers from a
  /// JSON Web Key Set endpoint. Thread-safe with configurable cache TTL.
  ///
  /// Usage:
  ///   // From OIDC issuer (auto-discovers jwks_uri):
  ///   Provider := TMVCJWKSClient.CreateFromIssuer('https://login.microsoftonline.com/tenant/v2.0');
  ///
  ///   // From explicit JWKS URI:
  ///   Provider := TMVCJWKSClient.Create('https://login.microsoftonline.com/common/discovery/v2.0/keys');
  ///
  ///   // Pass to OIDC middleware:
  ///   UseOIDCAuthentication(...).SetJWKSProvider(Provider);
  /// </summary>
  TMVCJWKSClient = class(TInterfacedObject, IJWKSProvider)
  private
    FJWKSURI: string;
    FKeys: TObjectDictionary<string, TJWKKey>;
    FCacheTTLSeconds: Integer;
    FLastFetchTime: TDateTime;
    FLock: TObject;
    procedure FetchKeys;
    procedure ParseJWKS(const AJWKSJSON: string);
    function ParseJWK(const AJWK: TJsonObject): TJWKKey;
    function JWKRSAToPEM(const AJWK: TJsonObject): string;
    function JWKECToPEM(const AJWK: TJsonObject): string;
    function JWKOKPToPEM(const AJWK: TJsonObject): string;
    function IsCacheExpired: Boolean;
    function CreateSignerForKey(const AKey: TJWKKey): IJWTSigner;
    function InferAlgorithm(const AKey: TJWKKey; const ATokenAlg: string): string;
  public
    /// <summary>
    /// Creates a JWKS client with an explicit JWKS endpoint URI.
    /// </summary>
    /// <param name="AJWKSURI">Full URL to the JWKS endpoint (e.g., https://.../.well-known/jwks.json)</param>
    /// <param name="ACacheTTLSeconds">How long to cache keys before refetching (default: 1 hour)</param>
    constructor Create(const AJWKSURI: string; ACacheTTLSeconds: Integer = 3600);
    /// <summary>
    /// Creates a JWKS client by discovering the jwks_uri from an OIDC issuer.
    /// Fetches .well-known/openid-configuration and extracts the jwks_uri field.
    /// </summary>
    /// <param name="AIssuerURL">OIDC issuer URL (e.g., https://login.microsoftonline.com/tenant/v2.0)</param>
    /// <param name="ACacheTTLSeconds">How long to cache keys before refetching (default: 1 hour)</param>
    class function CreateFromIssuer(const AIssuerURL: string;
      ACacheTTLSeconds: Integer = 3600): TMVCJWKSClient;
    destructor Destroy; override;
    /// <summary>
    /// IJWKSProvider implementation. Returns a signer for the given JWT header,
    /// matching by kid (Key ID) and alg (algorithm). Returns nil if no match.
    /// If the kid is not found in cache, forces a refetch (handles key rotation).
    /// </summary>
    function GetSignerForToken(const AJWTHeaderJSON: string): IJWTSigner;
    /// <summary>
    /// Forces an immediate refresh of the cached keys from the JWKS endpoint.
    /// </summary>
    procedure RefreshKeys;
    /// <summary>
    /// Returns the number of currently cached keys.
    /// </summary>
    function KeyCount: Integer;
  end;

implementation

uses
  System.DateUtils,
  System.NetEncoding,
  System.Math,
  System.StrUtils,
  System.SyncObjs,
  IdGlobal,
  IdCTypes,
  TaurusTLSLoader,
  TaurusTLSHeaders_types,
  TaurusTLSHeaders_evp,
  TaurusTLSHeaders_pem,
  TaurusTLSHeaders_bio,
  TaurusTLSHeaders_bn,
  TaurusTLSHeaders_rsa,
  TaurusTLSHeaders_ec,
  TaurusTLSHeaders_obj_mac,
  MVCFramework.RESTClient,
  MVCFramework.RESTClient.Intf,
  MVCFramework.Logger;

var
  GOpenSSLLoaded: Integer = 0;  // 0=not loaded, 1=loaded (uses TInterlocked for thread safety)

procedure EnsureOpenSSLLoaded;
begin
  if TInterlocked.CompareExchange(GOpenSSLLoaded, 0, 0) = 1 then
    Exit;
  if not GetOpenSSLLoader.Load then
    raise EMVCJWKSException.Create(
      'Cannot load OpenSSL libraries. ' +
      'Ensure libcrypto and libssl DLLs (1.1.1+ or 3.x) are in the application directory or system PATH.');
  TInterlocked.Exchange(GOpenSSLLoaded, 1);
end;

// ============================================================
// Base64URL decoding (no padding required)
// ============================================================

// ============================================================
// JWK RSA -> PEM conversion
// ============================================================
// JWK RSA format: { "kty":"RSA", "n":"<base64url>", "e":"<base64url>" }
// Steps: decode n,e -> BN_bin2bn -> RSA_new -> RSA_set0_key ->
//        EVP_PKEY_new -> EVP_PKEY_set1_RSA -> PEM_write_bio_PUBKEY

function TMVCJWKSClient.JWKRSAToPEM(const AJWK: TJsonObject): string;
var
  LNBytes, LEBytes: TBytes;
  LBN_N, LBN_E: PBIGNUM;
  LRSA: PRSA;
  LPKey: PEVP_PKEY;
  LBio: PBIO;
  LBuf: TBytes;
  LLen: Integer;
begin
  EnsureOpenSSLLoaded;

  LNBytes := URLSafeB64DecodeBytes(AJWK.S['n']);
  LEBytes := URLSafeB64DecodeBytes(AJWK.S['e']);

  if (Length(LNBytes) = 0) or (Length(LEBytes) = 0) then
    raise EMVCJWKSException.Create('JWK RSA key missing n or e component');

  LBN_N := BN_bin2bn(@LNBytes[0], Length(LNBytes), nil);
  if not Assigned(LBN_N) then
    raise EMVCJWKSException.Create('Failed to create BIGNUM for RSA modulus');
  LBN_E := BN_bin2bn(@LEBytes[0], Length(LEBytes), nil);
  if not Assigned(LBN_E) then
  begin
    BN_free(LBN_N);
    raise EMVCJWKSException.Create('Failed to create BIGNUM for RSA exponent');
  end;

  LRSA := RSA_new();
  if not Assigned(LRSA) then
  begin
    BN_free(LBN_N);
    BN_free(LBN_E);
    raise EMVCJWKSException.Create('RSA_new failed');
  end;

  // RSA_set0_key takes ownership of the BIGNUMs on success
  if RSA_set0_key(LRSA, LBN_N, LBN_E, nil) <> 1 then
  begin
    BN_free(LBN_N);
    BN_free(LBN_E);
    RSA_free(LRSA);
    raise EMVCJWKSException.Create('RSA_set0_key failed');
  end;

  LPKey := EVP_PKEY_new();
  if not Assigned(LPKey) then
  begin
    RSA_free(LRSA);
    raise EMVCJWKSException.Create('EVP_PKEY_new failed');
  end;

  // EVP_PKEY_set1_RSA increments RSA ref count; we must still free LRSA
  if EVP_PKEY_set1_RSA(LPKey, LRSA) <> 1 then
  begin
    RSA_free(LRSA);
    EVP_PKEY_free(LPKey);
    raise EMVCJWKSException.Create('EVP_PKEY_set1_RSA failed');
  end;
  RSA_free(LRSA);

  // Write PEM to memory BIO
  LBio := BIO_new(BIO_s_mem());
  if not Assigned(LBio) then
  begin
    EVP_PKEY_free(LPKey);
    raise EMVCJWKSException.Create('BIO_new failed');
  end;
  try
    if PEM_write_bio_PUBKEY(LBio, LPKey) <> 1 then
    begin
      EVP_PKEY_free(LPKey);
      raise EMVCJWKSException.Create('PEM_write_bio_PUBKEY failed for RSA key');
    end;
    EVP_PKEY_free(LPKey);

    // Read PEM string from BIO
    LLen := BIO_ctrl(LBio, BIO_CTRL_PENDING_const, 0, nil);
    if LLen <= 0 then
      raise EMVCJWKSException.Create('BIO_ctrl returned no pending data for PEM output');
    SetLength(LBuf, LLen);
    if BIO_read(LBio, PByte(LBuf)^, LLen) <> LLen then
      raise EMVCJWKSException.Create('BIO_read returned unexpected length');
    Result := TEncoding.ASCII.GetString(LBuf);
  finally
    BIO_free(LBio);
  end;
end;


// ============================================================
// JWK EC -> PEM conversion
// ============================================================
// JWK EC format: { "kty":"EC", "crv":"P-256", "x":"<base64url>", "y":"<base64url>" }
// Steps: decode x,y -> create uncompressed point (04||x||y) ->
//        EC_KEY_new_by_curve_name -> EC_POINT_oct2point ->
//        EC_KEY_set_public_key -> EVP_PKEY -> PEM

function TMVCJWKSClient.JWKECToPEM(const AJWK: TJsonObject): string;
var
  LCrv: string;
  LNID: Integer;
  LXBytes, LYBytes: TBytes;
  LPointBytes: TBytes;
  LECKey: PEC_KEY;
  LGroup: PEC_GROUP;
  LPoint: PEC_POINT;
  LPKey: PEVP_PKEY;
  LBio: PBIO;
  LBuf: TBytes;
  LLen: Integer;
begin
  EnsureOpenSSLLoaded;

  LCrv := AJWK.S['crv'];
  if SameText(LCrv, 'P-256') then
    LNID := NID_X9_62_prime256v1
  else if SameText(LCrv, 'P-384') then
    LNID := NID_secp384r1
  else if SameText(LCrv, 'P-521') then
    LNID := NID_secp521r1
  else
    raise EMVCJWKSException.CreateFmt('Unsupported EC curve: %s', [LCrv]);

  LXBytes := URLSafeB64DecodeBytes(AJWK.S['x']);
  LYBytes := URLSafeB64DecodeBytes(AJWK.S['y']);

  if (Length(LXBytes) = 0) or (Length(LYBytes) = 0) then
    raise EMVCJWKSException.Create('JWK EC key missing x or y component');

  // Build uncompressed point: 0x04 || x || y
  SetLength(LPointBytes, 1 + Length(LXBytes) + Length(LYBytes));
  LPointBytes[0] := $04;
  Move(LXBytes[0], LPointBytes[1], Length(LXBytes));
  Move(LYBytes[0], LPointBytes[1 + Length(LXBytes)], Length(LYBytes));

  LECKey := EC_KEY_new_by_curve_name(LNID);
  if not Assigned(LECKey) then
    raise EMVCJWKSException.CreateFmt('EC_KEY_new_by_curve_name failed for NID %d', [LNID]);

  try
    LGroup := EC_KEY_get0_group(LECKey);
    if not Assigned(LGroup) then
      raise EMVCJWKSException.Create('EC_KEY_get0_group returned nil');

    LPoint := EC_POINT_new(LGroup);
    if not Assigned(LPoint) then
      raise EMVCJWKSException.Create('EC_POINT_new failed');

    try
      if EC_POINT_oct2point(LGroup, LPoint, @LPointBytes[0], Length(LPointBytes), nil) <> 1 then
        raise EMVCJWKSException.Create('EC_POINT_oct2point failed - invalid EC point data');

      if EC_KEY_set_public_key(LECKey, LPoint) <> 1 then
        raise EMVCJWKSException.Create('EC_KEY_set_public_key failed');
    finally
      EC_POINT_free(LPoint);
    end;

    LPKey := EVP_PKEY_new();
    if not Assigned(LPKey) then
      raise EMVCJWKSException.Create('EVP_PKEY_new failed');

    if EVP_PKEY_set1_EC_KEY(LPKey, LECKey) <> 1 then
    begin
      EVP_PKEY_free(LPKey);
      raise EMVCJWKSException.Create('EVP_PKEY_set1_EC_KEY failed');
    end;
  finally
    EC_KEY_free(LECKey);
  end;

  // Write PEM
  LBio := BIO_new(BIO_s_mem());
  if not Assigned(LBio) then
  begin
    EVP_PKEY_free(LPKey);
    raise EMVCJWKSException.Create('BIO_new failed');
  end;
  try
    if PEM_write_bio_PUBKEY(LBio, LPKey) <> 1 then
    begin
      EVP_PKEY_free(LPKey);
      raise EMVCJWKSException.Create('PEM_write_bio_PUBKEY failed for EC key');
    end;
    EVP_PKEY_free(LPKey);

    LLen := BIO_ctrl(LBio, BIO_CTRL_PENDING_const, 0, nil);
    if LLen <= 0 then
      raise EMVCJWKSException.Create('BIO_ctrl returned no pending data for PEM output');
    SetLength(LBuf, LLen);
    if BIO_read(LBio, PByte(LBuf)^, LLen) <> LLen then
      raise EMVCJWKSException.Create('BIO_read returned unexpected length');
    Result := TEncoding.ASCII.GetString(LBuf);
  finally
    BIO_free(LBio);
  end;
end;


// ============================================================
// JWK OKP (Ed25519) -> PEM conversion
// ============================================================
// JWK OKP format: { "kty":"OKP", "crv":"Ed25519", "x":"<base64url>" }
// Steps: decode x (32 bytes) -> EVP_PKEY_new_raw_public_key -> PEM

function TMVCJWKSClient.JWKOKPToPEM(const AJWK: TJsonObject): string;
var
  LCrv: string;
  LXBytes: TBytes;
  LPKey: PEVP_PKEY;
  LBio: PBIO;
  LBuf: TBytes;
  LLen: Integer;
begin
  EnsureOpenSSLLoaded;

  LCrv := AJWK.S['crv'];
  if not SameText(LCrv, 'Ed25519') then
    raise EMVCJWKSException.CreateFmt('Unsupported OKP curve: %s (only Ed25519 is supported)', [LCrv]);

  LXBytes := URLSafeB64DecodeBytes(AJWK.S['x']);
  if Length(LXBytes) <> 32 then
    raise EMVCJWKSException.CreateFmt('Ed25519 public key must be 32 bytes, got %d', [Length(LXBytes)]);

  LPKey := EVP_PKEY_new_raw_public_key(EVP_PKEY_ED25519, nil, @LXBytes[0], Length(LXBytes));
  if not Assigned(LPKey) then
    raise EMVCJWKSException.Create('EVP_PKEY_new_raw_public_key failed for Ed25519');

  LBio := BIO_new(BIO_s_mem());
  if not Assigned(LBio) then
  begin
    EVP_PKEY_free(LPKey);
    raise EMVCJWKSException.Create('BIO_new failed');
  end;
  try
    if PEM_write_bio_PUBKEY(LBio, LPKey) <> 1 then
    begin
      EVP_PKEY_free(LPKey);
      raise EMVCJWKSException.Create('PEM_write_bio_PUBKEY failed for Ed25519 key');
    end;
    EVP_PKEY_free(LPKey);

    LLen := BIO_ctrl(LBio, BIO_CTRL_PENDING_const, 0, nil);
    if LLen <= 0 then
      raise EMVCJWKSException.Create('BIO_ctrl returned no pending data for PEM output');
    SetLength(LBuf, LLen);
    if BIO_read(LBio, PByte(LBuf)^, LLen) <> LLen then
      raise EMVCJWKSException.Create('BIO_read returned unexpected length');
    Result := TEncoding.ASCII.GetString(LBuf);
  finally
    BIO_free(LBio);
  end;
end;


// ============================================================
// JWK parsing
// ============================================================

function TMVCJWKSClient.ParseJWK(const AJWK: TJsonObject): TJWKKey;
var
  LKeyType, LUse: string;
begin
  Result := nil;
  LKeyType := AJWK.S['kty'];
  LUse := AJWK.S['use'];

  // Skip encryption keys - we only need signature verification keys
  if (not LUse.IsEmpty) and (not SameText(LUse, 'sig')) then
    Exit;

  Result := TJWKKey.Create;
  try
    Result.FKeyId := AJWK.S['kid'];
    Result.FAlgorithm := AJWK.S['alg'];
    Result.FKeyType := LKeyType;

    if SameText(LKeyType, 'RSA') then
      Result.FPublicKeyPEM := JWKRSAToPEM(AJWK)
    else if SameText(LKeyType, 'EC') then
      Result.FPublicKeyPEM := JWKECToPEM(AJWK)
    else if SameText(LKeyType, 'OKP') then
      Result.FPublicKeyPEM := JWKOKPToPEM(AJWK)
    else
    begin
      FreeAndNil(Result);
      LogW('JWKS: Skipping unsupported key type: ' + LKeyType);
    end;
  except
    on E: Exception do
    begin
      LogW('JWKS: Failed to parse JWK (kid=' + AJWK.S['kid'] + '): ' + E.Message);
      FreeAndNil(Result);
    end;
  end;
end;

procedure TMVCJWKSClient.ParseJWKS(const AJWKSJSON: string);
var
  LJWKS: TJsonObject;
  LKeysArray: TJsonArray;
  I: Integer;
  LKey: TJWKKey;
begin
  LJWKS := TJsonObject.Parse(AJWKSJSON) as TJsonObject;
  try
    if not LJWKS.Contains('keys') then
      raise EMVCJWKSException.Create('JWKS response does not contain "keys" array');

    LKeysArray := LJWKS.A['keys'];

    TMonitor.Enter(FLock);
    try
      FKeys.Clear;
      for I := 0 to LKeysArray.Count - 1 do
      begin
        LKey := ParseJWK(LKeysArray.O[I]);
        if Assigned(LKey) then
        begin
          if LKey.KeyId.IsEmpty then
          begin
            // Keys without kid are stored with a synthetic key
            FKeys.AddOrSetValue('__no_kid_' + I.ToString, LKey);
          end
          else
            FKeys.AddOrSetValue(LKey.KeyId, LKey);
        end;
      end;
      FLastFetchTime := Now;
    finally
      TMonitor.Exit(FLock);
    end;

    LogI('JWKS: Loaded ' + FKeys.Count.ToString + ' keys from ' + FJWKSURI);
  finally
    LJWKS.Free;
  end;
end;


// ============================================================
// JWKS fetching
// ============================================================

procedure TMVCJWKSClient.FetchKeys;
var
  lClient: IMVCRESTClient;
  lResponse: IMVCRESTResponse;
begin
  LogI('JWKS: Fetching keys from ' + FJWKSURI);
  lClient := TMVCRESTClient.New.BaseURL(FJWKSURI);
  lResponse := lClient.Get;
  if not lResponse.Success then
    raise EMVCJWKSException.CreateFmt(
      'JWKS: Failed to fetch keys from %s - HTTP %d %s',
      [FJWKSURI, lResponse.StatusCode, lResponse.StatusText]);
  ParseJWKS(lResponse.Content);
end;

function TMVCJWKSClient.IsCacheExpired: Boolean;
begin
  Result := (FLastFetchTime = 0) or
    (SecondsBetween(Now, FLastFetchTime) >= FCacheTTLSeconds);
end;


// ============================================================
// Signer creation
// ============================================================

function IsAlgorithmCompatibleWithKeyType(const AAlgorithm, AKeyType: string): Boolean;
begin
  if SameText(AKeyType, 'RSA') then
    Result := SameText(AAlgorithm, JWT_RS256) or SameText(AAlgorithm, JWT_RS384) or
              SameText(AAlgorithm, JWT_RS512) or SameText(AAlgorithm, JWT_PS256) or
              SameText(AAlgorithm, JWT_PS384) or SameText(AAlgorithm, JWT_PS512)
  else if SameText(AKeyType, 'EC') then
    Result := SameText(AAlgorithm, JWT_ES256) or SameText(AAlgorithm, JWT_ES384) or
              SameText(AAlgorithm, JWT_ES512)
  else if SameText(AKeyType, 'OKP') then
    Result := SameText(AAlgorithm, JWT_EdDSA)
  else
    Result := False;
end;

function TMVCJWKSClient.InferAlgorithm(const AKey: TJWKKey;
  const ATokenAlg: string): string;
begin
  // If the JWK has an explicit algorithm, that is authoritative
  if not AKey.Algorithm.IsEmpty then
    Exit(AKey.Algorithm);

  // If the token specifies an algorithm, use it only if compatible with key type.
  // This prevents algorithm confusion attacks: an attacker cannot force
  // an ES256 algorithm on an RSA key or vice versa.
  if not ATokenAlg.IsEmpty then
  begin
    if IsAlgorithmCompatibleWithKeyType(ATokenAlg, AKey.KeyType) then
      Exit(ATokenAlg);
    // Token algorithm is incompatible with key type - fall through to default
  end;

  // Fallback: safest default algorithm for the key type
  if SameText(AKey.KeyType, 'RSA') then
    Result := JWT_RS256
  else if SameText(AKey.KeyType, 'EC') then
    Result := JWT_ES256
  else if SameText(AKey.KeyType, 'OKP') then
    Result := JWT_EdDSA
  else
    raise EMVCJWKSException.CreateFmt('Cannot infer algorithm for key type %s', [AKey.KeyType]);
end;

function TMVCJWKSClient.CreateSignerForKey(const AKey: TJWKKey): IJWTSigner;
begin
  // AKey.Algorithm is always set by the caller (either from JWK or inferred)
  if SameText(AKey.KeyType, 'RSA') then
  begin
    if SameText(AKey.Algorithm, JWT_PS256) or SameText(AKey.Algorithm, JWT_PS384) or
       SameText(AKey.Algorithm, JWT_PS512) then
      Result := TRSAPSSJWTSigner.Create(AKey.Algorithm, '', AKey.PublicKeyPEM)
    else
      Result := TRSAJWTSigner.CreateVerifyOnly(AKey.Algorithm, AKey.PublicKeyPEM);
  end
  else if SameText(AKey.KeyType, 'EC') then
    Result := TECDSAJWTSigner.CreateVerifyOnly(AKey.Algorithm, AKey.PublicKeyPEM)
  else if SameText(AKey.KeyType, 'OKP') then
    Result := TEdDSAJWTSigner.CreateVerifyOnly(AKey.PublicKeyPEM)
  else
    raise EMVCJWKSException.CreateFmt('Unsupported key type for signer: %s', [AKey.KeyType]);
end;


// ============================================================
// TMVCJWKSClient
// ============================================================

constructor TMVCJWKSClient.Create(const AJWKSURI: string;
  ACacheTTLSeconds: Integer);
begin
  inherited Create;
  if AJWKSURI.Trim.IsEmpty then
    raise EMVCJWKSException.Create('JWKS URI must not be empty');
  FJWKSURI := AJWKSURI;
  FCacheTTLSeconds := ACacheTTLSeconds;
  FLastFetchTime := 0;
  FKeys := TObjectDictionary<string, TJWKKey>.Create([doOwnsValues]);
  FLock := TObject.Create;
end;

class function TMVCJWKSClient.CreateFromIssuer(const AIssuerURL: string;
  ACacheTTLSeconds: Integer): TMVCJWKSClient;
var
  lDiscoveryURL: string;
  lClient: IMVCRESTClient;
  lResponse: IMVCRESTResponse;
  lJSON: TJsonObject;
  lJWKSURI: string;
begin
  lDiscoveryURL := AIssuerURL;
  if not lDiscoveryURL.EndsWith('/') then
    lDiscoveryURL := lDiscoveryURL + '/';
  lDiscoveryURL := lDiscoveryURL + '.well-known/openid-configuration';

  LogI('JWKS: Discovering jwks_uri from ' + lDiscoveryURL);
  lClient := TMVCRESTClient.New.BaseURL(lDiscoveryURL);
  lResponse := lClient.Get;
  if not lResponse.Success then
    raise EMVCJWKSException.CreateFmt(
      'JWKS: Failed to discover OIDC configuration from %s - HTTP %d %s',
      [lDiscoveryURL, lResponse.StatusCode, lResponse.StatusText]);

  lJSON := TJsonObject.Parse(lResponse.Content) as TJsonObject;
  try
    lJWKSURI := lJSON.S['jwks_uri'];
    if lJWKSURI.IsEmpty then
      raise EMVCJWKSException.Create(
        'JWKS: OIDC discovery response does not contain jwks_uri');
    LogI('JWKS: Discovered jwks_uri = ' + lJWKSURI);
  finally
    lJSON.Free;
  end;

  Result := TMVCJWKSClient.Create(lJWKSURI, ACacheTTLSeconds);
end;

destructor TMVCJWKSClient.Destroy;
begin
  FKeys.Free;
  FLock.Free;
  inherited;
end;

function TMVCJWKSClient.GetSignerForToken(
  const AJWTHeaderJSON: string): IJWTSigner;
var
  LHeader: TJsonObject;
  LKid, LAlg, LEffectiveAlg: string;
  LKey: TJWKKey;
  LEffectiveKey: TJWKKey;
  LPair: TPair<string, TJWKKey>;
begin
  Result := nil;

  LHeader := TJsonObject.Parse(AJWTHeaderJSON) as TJsonObject;
  try
    LKid := LHeader.S['kid'];
    LAlg := LHeader.S['alg'];
  finally
    LHeader.Free;
  end;

  // Ensure keys are loaded
  if IsCacheExpired then
  begin
    TMonitor.Enter(FLock);
    try
      if IsCacheExpired then
        FetchKeys;
    finally
      TMonitor.Exit(FLock);
    end;
  end;

  TMonitor.Enter(FLock);
  try
    if not LKid.IsEmpty then
    begin
      // Look up by kid
      if FKeys.TryGetValue(LKid, LKey) then
      begin
        // Validate algorithm compatibility
        if (not LAlg.IsEmpty) and (not LKey.Algorithm.IsEmpty) and
           (not SameText(LAlg, LKey.Algorithm)) then
        begin
          LogW('JWKS: Algorithm mismatch for kid=' + LKid +
            ' (token=' + LAlg + ', key=' + LKey.Algorithm + ')');
          Exit(nil);
        end;
        LKey := FKeys[LKid]; // re-read under lock
      end
      else
        LKey := nil;
    end
    else
    begin
      // No kid in token header - find first key matching algorithm
      LKey := nil;
      for LPair in FKeys do
      begin
        if (LPair.Value.Algorithm.IsEmpty) or SameText(LPair.Value.Algorithm, LAlg) then
        begin
          LKey := LPair.Value;
          Break;
        end;
      end;
    end;
  finally
    TMonitor.Exit(FLock);
  end;

  // If kid not found, try refetching (handles key rotation)
  if (not LKid.IsEmpty) and (LKey = nil) then
  begin
    LogI('JWKS: Key kid=' + LKid + ' not found in cache, refetching for key rotation');
    TMonitor.Enter(FLock);
    try
      FetchKeys;
      FKeys.TryGetValue(LKid, LKey);
    finally
      TMonitor.Exit(FLock);
    end;
  end;

  if LKey = nil then
  begin
    LogW('JWKS: No matching key found (kid=' + LKid + ', alg=' + LAlg + ')');
    Exit(nil);
  end;

  // Determine effective algorithm without mutating the cached key.
  // The JWK's declared algorithm is authoritative; if absent, infer from
  // token alg + key type with compatibility validation.
  LEffectiveAlg := InferAlgorithm(LKey, LAlg);
  if not IsAlgorithmCompatibleWithKeyType(LEffectiveAlg, LKey.KeyType) then
  begin
    LogW('JWKS: Algorithm ' + LEffectiveAlg + ' is not compatible with key type ' +
      LKey.KeyType + ' (kid=' + LKid + ')');
    Exit(nil);
  end;

  // Create a signer using the effective algorithm (never mutate the cached key)
  LEffectiveKey := TJWKKey.Create;
  try
    LEffectiveKey.FKeyId := LKey.KeyId;
    LEffectiveKey.FAlgorithm := LEffectiveAlg;
    LEffectiveKey.FKeyType := LKey.KeyType;
    LEffectiveKey.FPublicKeyPEM := LKey.PublicKeyPEM;
    Result := CreateSignerForKey(LEffectiveKey);
  finally
    LEffectiveKey.Free;
  end;
end;

procedure TMVCJWKSClient.RefreshKeys;
begin
  TMonitor.Enter(FLock);
  try
    FetchKeys;
  finally
    TMonitor.Exit(FLock);
  end;
end;

function TMVCJWKSClient.KeyCount: Integer;
begin
  TMonitor.Enter(FLock);
  try
    Result := FKeys.Count;
  finally
    TMonitor.Exit(FLock);
  end;
end;

end.
