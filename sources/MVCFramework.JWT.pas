unit MVCFramework.JWT;

interface

uses
  System.Generics.Collections;

type
  TJWTRegisteredClaimNames = class sealed
  public
    const
    Issuer: String = 'iss';
    Subject: String = 'sub';
    Audience: String = 'aud';
    ExpirationTime: String = 'exp';
    NotBefore: String = 'nbf';
    IssuedAt: String = 'iat';
    JWT_ID: String = 'jti';
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
    function GetItem(const Index: String): String;
    procedure SetItem(const Index, Value: String);
    function GetItemAsDateTime(const Index: String): TDateTime;
    procedure SetItemAsDateTime(const Index: String; const Value: TDateTime);
    property ItemsAsDateTime[const Index: String]: TDateTime read GetItemAsDateTime
      write SetItemAsDateTime;
    property Items[const Index: String]: String read GetItem write SetItem; default;
  protected
    function Contains(const Index: String): Boolean;
    function Keys: TArray<String>;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  /// <summary>
  /// https://tools.ietf.org/html/rfc7519#section-4.1.1
  /// </summary>
  TJWTRegisteredClaims = class(TJWTDictionaryObject)
  private
    procedure SetAudience(const Value: String);
    procedure SetExpirationTime(const Value: TDateTime);
    procedure SetIssuedAt(const Value: TDateTime);
    procedure SetISSUER(const Value: String);
    procedure SetJWT_ID(const Value: String);
    procedure SetNotBefore(const Value: TDateTime);
    procedure SetSubject(const Value: String);
    function GetAudience: String;
    function GetExpirationTime: TDateTime;
    function GetIssuedAt: TDateTime;
    function GetJWT_ID: String;
    function GetNotBefore: TDateTime;
    function GetSubject: String;
    function GetIssuer: String;
  public
    /// <summary>
    /// "iss" (Issuer) Claim
    /// The " iss "(issuer) claim identifies The principal that issued The
    /// JWT. The processing of this claim is generally application specific.
    /// The " iss " value is a case-sensitive string containing a StringOrURI
    /// value.Use of this claim is OPTIONAL.
    /// </summary>
    property Issuer: String read GetIssuer write SetISSUER;
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
    property Subject: String read GetSubject write SetSubject;
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
    property Audience: String read GetAudience write SetAudience;
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
    property JWT_ID: String read GetJWT_ID write SetJWT_ID;
  end;

  TJWTCustomClaims = class(TJWTDictionaryObject)
    property Items; default;
  end;

  TJWT = class
  private
    FSecretKey: string;
    FRegisteredClaims: TJWTRegisteredClaims;
    FCustomClaims: TJWTCustomClaims;
    FHMACAlgorithm: String;
    procedure SetHMACAlgorithm(const Value: String);
  public
    constructor Create(const SecretKey: String); virtual;
    destructor Destroy; override;
    function GetToken: String;
    function IsValidToken(const Token: String): Boolean;
    procedure LoadToken(const Token: String);
    property Claims: TJWTRegisteredClaims read FRegisteredClaims;
    property CustomClaims: TJWTCustomClaims read FCustomClaims;
    property HMACAlgorithm: String read FHMACAlgorithm write SetHMACAlgorithm;
  end;

implementation

uses
  System.SysUtils, System.JSON, MVCFramework.Commons, MVCFramework.HMAC, System.DateUtils;

{ TJWTRegisteredClaims }

function TJWTRegisteredClaims.GetAudience: String;
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

function TJWTRegisteredClaims.GetIssuer: String;
begin
  Result := Items[TJWTRegisteredClaimNames.Issuer];
end;

function TJWTRegisteredClaims.GetJWT_ID: String;
begin
  Result := Items[TJWTRegisteredClaimNames.JWT_ID];
end;

function TJWTRegisteredClaims.GetNotBefore: TDateTime;
begin
  Result := ItemsAsDateTime[TJWTRegisteredClaimNames.NotBefore];
end;

function TJWTRegisteredClaims.GetSubject: String;
begin
  Result := Items[TJWTRegisteredClaimNames.Subject];
end;

procedure TJWTRegisteredClaims.SetAudience(const Value: String);
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

procedure TJWTRegisteredClaims.SetISSUER(const Value: String);
begin
  Items[TJWTRegisteredClaimNames.Issuer] := Value;
end;

procedure TJWTRegisteredClaims.SetJWT_ID(const Value: String);
begin
  Items[TJWTRegisteredClaimNames.JWT_ID] := Value;
end;

procedure TJWTRegisteredClaims.SetNotBefore(const Value: TDateTime);
begin
  ItemsAsDateTime[TJWTRegisteredClaimNames.NotBefore] := Value;
end;

procedure TJWTRegisteredClaims.SetSubject(const Value: String);
begin
  Items[TJWTRegisteredClaimNames.Subject] := Value;
end;

{ TJWTCustomClaims }

function TJWTDictionaryObject.Contains(const Index: String): Boolean;
begin
  Result := FClaims.ContainsKey(Index);
end;

constructor TJWTDictionaryObject.Create;
begin
  inherited;
  FClaims := TDictionary<String, String>.Create;
end;

destructor TJWTDictionaryObject.Destroy;
begin
  FClaims.Free;
  inherited;
end;

function TJWTDictionaryObject.GetItem(const Index: String): String;
begin
  if not FClaims.TryGetValue(Index, Result) then
    Result := '';
end;

function TJWTDictionaryObject.GetItemAsDateTime(const Index: String): TDateTime;
var
  lIntValue: Int64;
begin
  if not TryStrToInt64(Items[Index], lIntValue) then
    raise Exception.Create('Item cannot be converted as Unix Epoch');
  Result := UnixToDateTime(lIntValue);
end;

function TJWTDictionaryObject.Keys: TArray<String>;
begin
  Result := FClaims.Keys.ToArray;
end;

procedure TJWTDictionaryObject.SetItem(const Index, Value: String);
begin
  FClaims.AddOrSetValue(Index, Value);
end;

procedure TJWTDictionaryObject.SetItemAsDateTime(const Index: String;
  const Value: TDateTime);
begin
  Items[Index] := DateTimeToUnix(Value).ToString;
end;

{ TJWT }

constructor TJWT.Create(const SecretKey: String);
begin
  inherited Create;
  FSecretKey := SecretKey;
  FRegisteredClaims := TJWTRegisteredClaims.Create;
  FCustomClaims := TJWTCustomClaims.Create;
  FHMACAlgorithm := 'HS256';
end;

destructor TJWT.Destroy;
begin
  FRegisteredClaims.Free;
  FCustomClaims.Free;
  inherited;
end;

function TJWT.GetToken: String;
var
  lHeader, lPayload: TJSONObject;
  lHeaderEncoded, lPayloadEncoded, lToken, lHash: String;
  lBytes: TBytes;
  lRegClaimName: String;
  lCustomClaimName: String;
begin
  lHeader := TJSONObject.Create;
  try
    lPayload := TJSONObject.Create;
    try
      lHeader.AddPair('alg', HMACAlgorithm).AddPair('typ', 'JWT');
      for lRegClaimName in TJWTRegisteredClaimNames.Names do
      begin
        if FRegisteredClaims.Contains(lRegClaimName) then
          lPayload.AddPair(lRegClaimName, FRegisteredClaims[lRegClaimName]);
      end;

      for lCustomClaimName in FCustomClaims.Keys do
      begin
        lPayload.AddPair(lCustomClaimName, FCustomClaims[lCustomClaimName]);
      end;

      lHeaderEncoded := B64Encode(lHeader.ToJSON);
      lPayloadEncoded := B64Encode(lPayload.ToJSON);
      lToken := lHeaderEncoded + '.' + lPayloadEncoded;
      lBytes := HMAC(HMACAlgorithm, lToken, FSecretKey);
      lHash := B64Encode(lBytes);
      Result := lToken + '.' + lHash;
    finally
      lPayload.Free;
    end;
  finally
    lHeader.Free;
  end;
end;

function TJWT.IsValidToken(const Token: String): Boolean;
var
  lPieces: TArray<String>;
  lJHeader: TJSONObject;
  lJAlg: TJSONString;
  lAlgName: string;
  lJPayload: TJSONObject;
begin
  lPieces := Token.Split(['.']);
  if Length(lPieces) <> 3 then
    Exit(False);
  lJHeader := TJSONObject.ParseJSONValue(B64Decode(lPieces[0])) as TJSONObject;
  try
    if not Assigned(lJHeader) then
      Exit(False);

    lJPayload := TJSONObject.ParseJSONValue(B64Decode(lPieces[1])) as TJSONObject;
    try
      if not Assigned(lJPayload) then
        Exit(False);

      if not lJHeader.TryGetValue<TJSONString>('alg', lJAlg) then
        Exit(False);

      lAlgName := lJAlg.Value;
      Result := Token = lPieces[0] + '.' + lPieces[1] + '.' +
        B64Encode(
        HMAC(lAlgName, lPieces[0] + '.' + lPieces[1], FSecretKey)
        );
    finally
      lJPayload.Free;
    end;
  finally
    lJHeader.Free;
  end;
end;

procedure TJWT.LoadToken(const Token: String);
var
  lPieces: TArray<String>;
  lJHeader: TJSONObject;
  lJPayload: TJSONObject;
  lJPair: TJSONPair;
  i: Integer;
  lName: string;
  j: Integer;
  lIsRegistered: Boolean;
  lValue: string;
begin
  if not IsValidToken(Token) then
    raise EMVCJWTException.Create('Invalid token');

  lPieces := Token.Split(['.']);
  lJHeader := TJSONObject.ParseJSONValue(B64Decode(lPieces[0])) as TJSONObject;
  try
    lJPayload := TJSONObject.ParseJSONValue(B64Decode(lPieces[1])) as TJSONObject;
    try
      // loading data from token into self
      FHMACAlgorithm := lJHeader.Values['alg'].Value;
      // registered claims
      FRegisteredClaims.FClaims.Clear;

      // custom claims
      FCustomClaims.FClaims.Clear;
      for i := 0 to lJPayload.Count - 1 do
      begin
        lIsRegistered := False;
        lJPair := lJPayload.Pairs[i];
        lName := lJPair.JsonString.Value;
        lValue := lJPair.JsonValue.Value;

        // if is a registered claim, load it in the proper dictionary...
        for j := 0 to High(TJWTRegisteredClaimNames.Names) do
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

    finally
      lJPayload.Free;
    end;
  finally
    lJHeader.Free;
  end;
end;

procedure TJWT.SetHMACAlgorithm(const Value: String);
begin
  FHMACAlgorithm := Value;
end;

end.
