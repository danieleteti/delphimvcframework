// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
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
// *************************************************************************** }

unit MVCFramework.JWT;

{$I dmvcframework.inc}

interface

uses
  System.Generics.Collections,
  JsonDataObjects,
  MVCFramework,
  MVCFramework.Patches, MVCFramework.HMAC;

type

{$SCOPEDENUMS ON}
  TJWTCheckableClaim = (ExpirationTime, NotBefore, IssuedAt);
  TJWTCheckableClaims = set of TJWTCheckableClaim;

  TJWTRegisteredClaimNames = class sealed
  public
    const
    /// <summary>
    /// The "iss" (issuer) claim identifies the principal that issued the
    /// JWT.  The processing of this claim is generally application specific.
    /// The "iss" value is a case-sensitive string containing a StringOrURI
    /// value.  Use of this claim is OPTIONAL.
    /// </summary>
    Issuer: string = 'iss';
    /// <summary>
    /// The "sub" (subject) claim identifies the principal that is the
    /// subject of the JWT.  The claims in a JWT are normally statements
    /// about the subject.  The subject value MUST either be scoped to be
    /// locally unique in the context of the issuer or be globally unique.
    /// The processing of this claim is generally application specific.  The
    /// "sub" value is a case-sensitive string containing a StringOrURI
    /// value.  Use of this claim is OPTIONAL.
    /// </summary>
    Subject: string = 'sub';
    /// <summary>
    /// The "aud" (audience) claim identifies the recipients that the JWT is
    /// intended for.  Each principal intended to process the JWT MUST
    /// identify itself with a value in the audience claim.  If the principal
    /// processing the claim does not identify itself with a value in the
    /// "aud" claim when this claim is present, then the JWT MUST be
    /// rejected.  In the general case, the "aud" value is an array of case-
    /// sensitive strings, each containing a StringOrURI value.  In the
    /// special case when the JWT has one audience, the "aud" value MAY be a
    /// single case-sensitive string containing a StringOrURI value.  The
    /// interpretation of audience values is generally application specific.
    /// Use of this claim is OPTIONAL.
    /// </summary>
    Audience: string = 'aud';
    /// <summary>
    /// The "exp" (expiration time) claim identifies the expiration time on
    /// or after which the JWT MUST NOT be accepted for processing.  The
    /// processing of the "exp" claim requires that the current date/time
    /// MUST be before the expiration date/time listed in the "exp" claim.
    /// Implementers MAY provide for some small leeway, usually no more than
    /// a few minutes, to account for clock skew.  Its value MUST be a number
    /// containing a NumericDate value.  Use of this claim is OPTIONAL.
    /// </summary>
    ExpirationTime: string = 'exp';
    /// <summary>
    /// The "nbf" (not before) claim identifies the time before which the JWT
    /// MUST NOT be accepted for processing.  The processing of the "nbf"
    /// claim requires that the current date/time MUST be after or equal to
    /// the not-before date/time listed in the "nbf" claim.  Implementers MAY
    /// provide for some small leeway, usually no more than a few minutes, to
    /// account for clock skew.  Its value MUST be a number containing a
    /// NumericDate value.  Use of this claim is OPTIONAL.
    /// </summary>
    NotBefore: string = 'nbf';
    /// <summary>
    /// The "iat" (issued at) claim identifies the time at which the JWT was
    /// issued.  This claim can be used to determine the age of the JWT.  Its
    /// value MUST be a number containing a NumericDate value.  Use of this
    /// claim is OPTIONAL.
    /// </summary>
    IssuedAt: string = 'iat';
    /// <summary>
    /// The "jti" (JWT ID) claim provides a unique identifier for the JWT.
    /// The identifier value MUST be assigned in a manner that ensures that
    /// there is a negligible probability that the same value will be
    /// accidentally assigned to a different data object; if the application
    /// uses multiple issuers, collisions MUST be prevented among values
    /// produced by different issuers as well.  The "jti" claim can be used
    /// to prevent the JWT from being replayed.  The "jti" value is a case-
    /// sensitive string.  Use of this claim is OPTIONAL.
    /// </summary>
    JWT_ID: string = 'jti';

    Names: array [0 .. 6] of string = (
      'iss',
      'sub',
      'aud',
      'exp',
      'nbf',
      'iat',
      'jti');
  end;

  TJWTDictionaryObject = class
  private
    FClaims: TDictionary<string, string>;
    function GetItem(const Index: string): string;
    procedure SetItem(const Index, Value: string);
    function GetItemAsDateTime(const Index: string): TDateTime;
    procedure SetItemAsDateTime(const Index: string; const Value: TDateTime);
    property ItemsAsDateTime[const index: string]: TDateTime read GetItemAsDateTime write SetItemAsDateTime;
    property Items[const index: string]: string read GetItem write SetItem; default;
  protected
    function Contains(const Index: string): Boolean;
    function Keys: TArray<string>;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  /// <summary>
  /// https://tools.ietf.org/html/rfc7519#section-4.1.1
  /// </summary>
  TJWTRegisteredClaims = class(TJWTDictionaryObject)
  private
    procedure SetAudience(const Value: string);
    procedure SetExpirationTime(const Value: TDateTime);
    procedure SetIssuedAt(const Value: TDateTime);
    procedure SetISSUER(const Value: string);
    procedure SetJWT_ID(const Value: string);
    procedure SetNotBefore(const Value: TDateTime);
    procedure SetSubject(const Value: string);
    function GetAudience: string;
    function GetExpirationTime: TDateTime;
    function GetIssuedAt: TDateTime;
    function GetJWT_ID: string;
    function GetNotBefore: TDateTime;
    function GetSubject: string;
    function GetIssuer: string;
  public
    /// <summary>
    /// "iss" (Issuer) Claim
    /// The " iss "(issuer) claim identifies The principal that issued The
    /// JWT. The processing of this claim is generally application specific.
    /// The " iss " value is a case-sensitive string containing a StringOrURI
    /// value.Use of this claim is OPTIONAL.
    /// </summary>
    property Issuer: string read GetIssuer write SetISSUER;
    /// <summary>
    /// "sub" (Subject) Claim
    /// The "sub" (subject) claim identifies the principal that is the
    /// subject of the JWT.  The claims in a JWT are normally statements
    /// about the subject.  The subject value MUST either be scoped to be
    /// locally unique in the context of the issuer or be globally unique.
    /// The processing of this claim is generally application specific.  The
    /// "sub" value is a case-sensitive string containing a StringOrURI
    /// value.  Use of this claim is OPTIONAL.
    /// </summary>
    property Subject: string read GetSubject write SetSubject;
    /// <summary>
    /// "aud" (Audience) Claim
    /// The "aud" (audience) claim identifies the recipients that the JWT is
    /// intended for.  Each principal intended to process the JWT MUST
    /// identify itself with a value in the audience claim.  If the principal
    /// processing the claim does not identify itself with a value in the
    /// "aud" claim when this claim is present, then the JWT MUST be
    /// rejected.  In the general case, the "aud" value is an array of case-
    /// sensitive strings, each containing a StringOrURI value.  In the
    /// special case when the JWT has one audience, the "aud" value MAY be a
    /// single case-sensitive string containing a StringOrURI value.  The
    /// interpretation of audience values is generally application specific.
    /// Use of this claim is OPTIONAL.
    /// </summary>
    property Audience: string read GetAudience write SetAudience;
    /// <summary>
    /// "exp" (Expiration Time) Claim
    /// The "exp" (expiration time) claim identifies the expiration time on
    /// or after which the JWT MUST NOT be accepted for processing.  The
    /// processing of the "exp" claim requires that the current date/time
    /// MUST be before the expiration date/time listed in the "exp" claim.
    /// Implementers MAY provide for some small leeway, usually no more than
    /// a few minutes, to account for clock skew.  Its value MUST be a number
    /// containing a NumericDate value.  Use of this claim is OPTIONAL.
    /// </summary>
    property ExpirationTime: TDateTime read GetExpirationTime write SetExpirationTime;
    /// <summary>
    /// "nbf" (Not Before) Claim
    /// The "nbf" (not before) claim identifies the time before which the JWT
    /// MUST NOT be accepted for processing.  The processing of the "nbf"
    /// claim requires that the current date/time MUST be after or equal to
    /// the not-before date/time listed in the "nbf" claim.  Implementers MAY
    /// provide for some small leeway, usually no more than a few minutes, to
    /// account for clock skew.  Its value MUST be a number containing a
    /// NumericDate value.  Use of this claim is OPTIONAL.
    /// </summary>
    property NotBefore: TDateTime read GetNotBefore write SetNotBefore;
    /// <summary>
    /// "iat" (Issued At) Claim
    /// The "iat" (issued at) claim identifies the time at which the JWT was
    /// issued.  This claim can be used to determine the age of the JWT.  Its
    /// value MUST be a number containing a NumericDate value.  Use of this
    /// claim is OPTIONAL.
    /// </summary>
    property IssuedAt: TDateTime read GetIssuedAt write SetIssuedAt;
    /// <summary>
    /// "jti" (JWT ID) Claim
    /// The "jti" (JWT ID) claim provides a unique identifier for the JWT.
    /// The identifier value MUST be assigned in a manner that ensures that
    /// there is a negligible probability that the same value will be
    /// accidentally assigned to a different data object; if the application
    /// uses multiple issuers, collisions MUST be prevented among values
    /// produced by different issuers as well.  The "jti" claim can be used
    /// to prevent the JWT from being replayed.  The "jti" value is a case-
    /// sensitive string.  Use of this claim is OPTIONAL.
    /// </summary>
    property JWT_ID: string read GetJWT_ID write SetJWT_ID;
  end;

  TJWTCustomClaims = class(TJWTDictionaryObject)
  public
    property Items; default;
    function AsCustomData: TMVCCustomData;
  end;

  TJWT = class
  private
    FSecretKey: string;
    FRegisteredClaims: TJWTRegisteredClaims;
    FCustomClaims: TJWTCustomClaims;
    FHMACAlgorithm: string;
    FRegClaimsToChecks: TJWTCheckableClaims;
    FLeewaySeconds: Cardinal;
    FData: TObject; //the jwt middleware will inject TMVCWebRequest here
    procedure SetHMACAlgorithm(const Value: string);
    procedure SetChecks(const Value: TJWTCheckableClaims);
    function CheckExpirationTime(Payload: TJDOJSONObject; out Error: string): Boolean;
    function CheckNotBefore(Payload: TJDOJSONObject; out Error: string): Boolean;
    function CheckIssuedAt(Payload: TJDOJSONObject; out Error: string): Boolean;
    procedure SetLiveValidityWindowInSeconds(const Value: Cardinal);
    function GetLiveValidityWindowInSeconds: Cardinal;
    function IsValidToken(const Token: string; out Header, Payload: TJDOJSONObject; out Error: string): Boolean;
  public
    constructor Create(
      const SecretKey: string;
      const ALeewaySeconds: Cardinal = 300;
      const HMACAlgorithm: String = HMAC_HS512); virtual;
    destructor Destroy; override;
    function GetToken: string;
    function LoadToken(const Token: string; out Error: string): Boolean;
    property Data: TObject read fData write fData;
    property Claims: TJWTRegisteredClaims read FRegisteredClaims;
    property CustomClaims: TJWTCustomClaims read FCustomClaims;
    property HMACAlgorithm: string read FHMACAlgorithm write SetHMACAlgorithm;
    property LeewaySeconds: Cardinal read FLeewaySeconds;
    property RegClaimsToChecks: TJWTCheckableClaims read FRegClaimsToChecks write SetChecks;
    /// <summary>
    /// Use LiveValidityWindowInSeconds to make the ExpirationTime dynamic at each request.
    /// ExpirationTime will be incremented by LiveValidityWindowInSeconds seconds automatically
    /// if the remaining seconds are less than the LiveValidityWindowInSeconds.
    /// </summary>
    property LiveValidityWindowInSeconds: Cardinal read GetLiveValidityWindowInSeconds
      write SetLiveValidityWindowInSeconds;
  end;

implementation

uses
  System.SysUtils,
  MVCFramework.Commons,
  System.DateUtils,
  IdGlobal;

{ TJWTRegisteredClaims }

function TJWTRegisteredClaims.GetAudience: string;
begin
  Result := Items[TJWTRegisteredClaimNames.Audience];
end;

function TJWTRegisteredClaims.GetExpirationTime: TDateTime;
begin
  Result := ItemsAsDateTime[TJWTRegisteredClaimNames.ExpirationTime];
end;

function TJWTRegisteredClaims.GetIssuedAt: TDateTime;
begin
  Result := ItemsAsDateTime[TJWTRegisteredClaimNames.IssuedAt];
end;

function TJWTRegisteredClaims.GetIssuer: string;
begin
  Result := Items[TJWTRegisteredClaimNames.Issuer];
end;

function TJWTRegisteredClaims.GetJWT_ID: string;
begin
  Result := Items[TJWTRegisteredClaimNames.JWT_ID];
end;

function TJWTRegisteredClaims.GetNotBefore: TDateTime;
begin
  Result := ItemsAsDateTime[TJWTRegisteredClaimNames.NotBefore];
end;

function TJWTRegisteredClaims.GetSubject: string;
begin
  Result := Items[TJWTRegisteredClaimNames.Subject];
end;

procedure TJWTRegisteredClaims.SetAudience(const Value: string);
begin
  Items[TJWTRegisteredClaimNames.Audience] := Value;
end;

procedure TJWTRegisteredClaims.SetExpirationTime(const Value: TDateTime);
begin
  ItemsAsDateTime[TJWTRegisteredClaimNames.ExpirationTime] := Value;
end;

procedure TJWTRegisteredClaims.SetIssuedAt(const Value: TDateTime);
begin
  ItemsAsDateTime[TJWTRegisteredClaimNames.IssuedAt] := Value;
end;

procedure TJWTRegisteredClaims.SetISSUER(const Value: string);
begin
  Items[TJWTRegisteredClaimNames.Issuer] := Value;
end;

procedure TJWTRegisteredClaims.SetJWT_ID(const Value: string);
begin
  Items[TJWTRegisteredClaimNames.JWT_ID] := Value;
end;

procedure TJWTRegisteredClaims.SetNotBefore(const Value: TDateTime);
begin
  ItemsAsDateTime[TJWTRegisteredClaimNames.NotBefore] := Value;
end;

procedure TJWTRegisteredClaims.SetSubject(const Value: string);
begin
  Items[TJWTRegisteredClaimNames.Subject] := Value;
end;

{ TJWTCustomClaims }

function TJWTDictionaryObject.Contains(const Index: string): Boolean;
begin
  Result := FClaims.ContainsKey(index);
end;

constructor TJWTDictionaryObject.Create;
begin
  inherited;
  FClaims := TDictionary<string, string>.Create;
end;

destructor TJWTDictionaryObject.Destroy;
begin
  FClaims.Free;
  inherited;
end;

function TJWTDictionaryObject.GetItem(const Index: string): string;
begin
  if not FClaims.TryGetValue(index, Result) then
    Result := '';
end;

function TJWTDictionaryObject.GetItemAsDateTime(const Index: string): TDateTime;
var
  lIntValue: Int64;
begin
  Result := -693594;
  if Trim(Items[index]) <> EmptyStr then
  begin
    if not TryStrToInt64(Items[index], lIntValue) then
      raise Exception.Create('Item cannot be converted as Unix Epoch');
    Result := UnixToDateTime(lIntValue, False);
  end;
end;

function TJWTDictionaryObject.Keys: TArray<string>;
begin
  Result := FClaims.Keys.ToArray;
end;

procedure TJWTDictionaryObject.SetItem(const Index, Value: string);
begin
  FClaims.AddOrSetValue(index, Value);
end;

procedure TJWTDictionaryObject.SetItemAsDateTime(const Index: string;
  const Value: TDateTime);
begin
  Items[index] := IntToStr(DateTimeToUnix(Value, False));
end;

{ TJWTCustomClaims }

function TJWTCustomClaims.AsCustomData: TMVCCustomData;
begin
  Result := TMVCCustomData.Create(FClaims);
end;

{ TJWT }

function TJWT.CheckExpirationTime(Payload: TJDOJSONObject; out Error: string): Boolean;
var
  lIntValue: Int64;
  lValue: string;
  lExpirationTimeAsDateTime: TDateTime;
begin
  if not Payload.Contains(TJWTRegisteredClaimNames.ExpirationTime) then
  begin
    Error := TJWTRegisteredClaimNames.ExpirationTime + ' not set';
    Exit(False);
  end;

  lValue := Payload.S[TJWTRegisteredClaimNames.ExpirationTime];
  if not TryStrToInt64(lValue, lIntValue) then
  begin
    Error := TJWTRegisteredClaimNames.ExpirationTime + ' is not an integer';
    Exit(False);
  end;

  lExpirationTimeAsDateTime := UnixToDateTime(lIntValue, False);
  if lExpirationTimeAsDateTime <= Now - FLeewaySeconds * OneSecond then
  begin
    Error := 'Token expired';
    Exit(False);
  end;

  Result := True;
end;

function TJWT.CheckIssuedAt(Payload: TJDOJSONObject; out Error: string): Boolean;
var
  lIntValue: Int64;
  lValue: string;
begin
  if not Payload.Contains(TJWTRegisteredClaimNames.IssuedAt) then
  begin
    Error := TJWTRegisteredClaimNames.IssuedAt + ' not set';
    Exit(False);
  end;

  lValue := Payload.S[TJWTRegisteredClaimNames.IssuedAt];
  if not TryStrToInt64(lValue, lIntValue) then
  begin
    Error := TJWTRegisteredClaimNames.IssuedAt + ' is not an integer';
    Exit(False);
  end;

  if UnixToDateTime(lIntValue, False) >= Now + FLeewaySeconds * OneSecond then
  begin
    Error := 'Token is issued in the future';
    Exit(False);
  end;

  Result := True;
end;

function TJWT.CheckNotBefore(Payload: TJDOJSONObject; out Error: string): Boolean;
var
  lIntValue: Int64;
  lValue: string;
begin
  if not Payload.Contains(TJWTRegisteredClaimNames.NotBefore) then
  begin
    Error := TJWTRegisteredClaimNames.NotBefore + ' not set';
    Exit(False);
  end;

  lValue := Payload.S[TJWTRegisteredClaimNames.NotBefore];
  if not TryStrToInt64(lValue, lIntValue) then
  begin
    Error := TJWTRegisteredClaimNames.NotBefore + ' is not an integer';
    Exit(False);
  end;

  if UnixToDateTime(lIntValue, False) >= Now + FLeewaySeconds * OneSecond then
  begin
    Error := 'Token still not valid';
    Exit(False);
  end;

  Result := True;
end;

constructor TJWT.Create(const SecretKey: string; const ALeewaySeconds: Cardinal; const HMACAlgorithm: String);
begin
  inherited Create;
  FSecretKey := SecretKey;
  FRegisteredClaims := TJWTRegisteredClaims.Create;
  FCustomClaims := TJWTCustomClaims.Create;
  FHMACAlgorithm := HMACAlgorithm;
  FLeewaySeconds := ALeewaySeconds;
  FRegClaimsToChecks := [TJWTCheckableClaim.ExpirationTime, TJWTCheckableClaim.NotBefore, TJWTCheckableClaim.IssuedAt];
end;

destructor TJWT.Destroy;
begin
  FRegisteredClaims.Free;
  FCustomClaims.Free;
  inherited;
end;

function TJWT.GetLiveValidityWindowInSeconds: Cardinal;
begin
  Result := StrToIntDef(FCustomClaims.Items['lvw'], 0);
end;

function TJWT.GetToken: string;
var
  lHeader, lPayload: TJDOJSONObject;
  lHeaderEncoded, lPayloadEncoded, lToken, lHash: string;
  lBytes: TBytes;
  lRegClaimName: string;
  lCustomClaimName: string;
begin
  lHeader := TJDOJSONObject.Create;
  try
    lPayload := TJDOJSONObject.Create;
    try
      lHeader.S['alg'] := HMACAlgorithm;
      lHeader.S['typ'] := 'JWT';
      for lRegClaimName in TJWTRegisteredClaimNames.Names do
      begin
        if FRegisteredClaims.Contains(lRegClaimName) then
        begin
          if (lRegClaimName = TJWTRegisteredClaimNames.ExpirationTime) or
            (lRegClaimName = TJWTRegisteredClaimNames.NotBefore) or
            (lRegClaimName = TJWTRegisteredClaimNames.IssuedAt) then
            lPayload.L[lRegClaimName] := StrToInt64(FRegisteredClaims[lRegClaimName])
          else
            lPayload.S[lRegClaimName] := FRegisteredClaims[lRegClaimName];
        end;
      end;

      for lCustomClaimName in FCustomClaims.Keys do
      begin
        lPayload.S[lCustomClaimName] := FCustomClaims[lCustomClaimName];
      end;

      lHeaderEncoded := URLSafeB64encode(lHeader.ToString, False, IndyTextEncoding_UTF8);
      lPayloadEncoded := URLSafeB64encode(lPayload.ToString, False, IndyTextEncoding_UTF8);
      lToken := lHeaderEncoded + '.' + lPayloadEncoded;
      lBytes := HMAC(HMACAlgorithm, lToken, FSecretKey);
      lHash := URLSafeB64encode(lBytes, False);
      Result := lToken + '.' + lHash;
    finally
      lPayload.Free;
    end;
  finally
    lHeader.Free;
  end;
end;

function TJWT.IsValidToken(const Token: string; out Header, Payload: TJDOJSONObject; out Error: string): Boolean;
var
  lPieces: TArray<string>;
  lAlgName: string;
begin
  Result := False;
  Error := 'Invalid Token';
  lPieces := Token.Split(['.']);
  if Length(lPieces) <> 3 then
  begin
    Error := Error + ' (step1)';
    Exit(False);
  end;

  Header := TJDOJsonBaseObject.Parse(URLSafeB64Decode(lPieces[0], IndyTextEncoding_UTF8)) as TJDOJSONObject;
  try
    if not Assigned(Header) then
    begin
      Error := Error + ' (step2)';
      Exit(False);
    end;

    Payload := TJDOJsonBaseObject.Parse(URLSafeB64Decode(lPieces[1], IndyTextEncoding_UTF8)) as TJDOJSONObject;
    try
      if not Assigned(Payload) then
      begin
        Error := Error + ' (step3)';
        Exit(False);
      end;

      if not Header.Contains('alg') then
      begin
        Error := Error + ' (step4)';
        Exit(False);
      end;

      lAlgName := Header.S['alg'];
      Result := Token = lPieces[0] + '.' + lPieces[1] + '.' +
        URLSafeB64encode(HMAC(lAlgName, lPieces[0] + '.' + lPieces[1], FSecretKey), False);

      // if the token is correctly signed and has not been tampered,
      // let's check it's validity usinf nbf, exp, iat as configured in
      // the RegClaimsToCheck property
      if Result then
      begin
        if TJWTCheckableClaim.ExpirationTime in RegClaimsToChecks then
        begin
          if not CheckExpirationTime(Payload, Error) then
          begin
            Exit(False);
            Error := Error + ' (step6)';
          end;

        end;

        if TJWTCheckableClaim.NotBefore in RegClaimsToChecks then
        begin
          if not CheckNotBefore(Payload, Error) then
          begin
            Exit(False);
            Error := Error + ' (step7)';
          end;
        end;

        if TJWTCheckableClaim.IssuedAt in RegClaimsToChecks then
        begin
          if not CheckIssuedAt(Payload, Error) then
          begin
            Exit(False);
            Error := Error + ' (step8)';
          end;
        end;

        Error := '';
      end
      else
      begin
        Error := Error + ' (step5)';
      end;
    finally
      if not Result then
        FreeAndNil(Payload);
    end;
  finally
    if not Result then
      FreeAndNil(Header);
  end;
end;

function TJWT.LoadToken(const Token: string; out Error: string): Boolean;
var
  lPieces: TArray<string>;
  lJHeader: TJDOJSONObject;
  lJPayload: TJDOJSONObject;
  i: Integer;
  lName: string;
  j: Integer;
  lIsRegistered: Boolean;
  lValue: string;
begin
  lJHeader := nil;
  lJPayload := nil;
  Result := IsValidToken(Token, lJHeader, lJPayload, Error);
  try
    if not Result then
      Exit(False);

    lPieces := Token.Split(['.']);
    // loading data from token into self
    FHMACAlgorithm := lJHeader.Values['alg'].Value;
    // registered claims
    FRegisteredClaims.FClaims.Clear;

    // custom claims
    FCustomClaims.FClaims.Clear;
    for i := 0 to lJPayload.Count - 1 do
    begin
      lIsRegistered := False;

      lName := lJPayload.Names[i];
      lValue := lJPayload.Items[i].Value;

      // if is a registered claim, load it in the proper dictionary...
      for j := 0 to high(TJWTRegisteredClaimNames.Names) do
      begin
        if lName = TJWTRegisteredClaimNames.Names[j] then
        begin
          FRegisteredClaims.FClaims.AddOrSetValue(lName, lValue);
          lIsRegistered := True;
          Break;
        end;
      end;
      if not lIsRegistered then
        FCustomClaims.FClaims.AddOrSetValue(lName, lValue);
    end;

    FCustomClaims.FClaims.TrimExcess;
    FRegisteredClaims.FClaims.TrimExcess;
  finally
    FreeAndNil(lJHeader);
    FreeAndNil(lJPayload);
  end;
end;

procedure TJWT.SetChecks(const Value: TJWTCheckableClaims);
begin
  FRegClaimsToChecks := Value;
end;

procedure TJWT.SetHMACAlgorithm(const Value: string);
begin
  FHMACAlgorithm := Value;
end;

procedure TJWT.SetLiveValidityWindowInSeconds(const Value: Cardinal);
begin
  FCustomClaims.Items['lvw'] := Value.ToString;
end;

end.
