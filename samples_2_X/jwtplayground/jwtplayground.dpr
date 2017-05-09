program jwtplayground;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  System.NetEncoding,
  Soap.EncdDecd,
  System.JSON,
  MVCFramework.HMAC in '..\..\sources\MVCFramework.HMAC.pas',
  MVCFramework.JWT in '..\..\sources\MVCFramework.JWT.pas',
  MVCFramework.Commons in '..\..\sources\MVCFramework.Commons.pas';

const
  HMAC_SECRET = 'myk3y';

function IsValidToken(const Token: String; const Key: String): Boolean;
var
  lPieces: TArray<String>;
  lJHeader: TJSONObject;
  lJAlg: TJSONString;
  lAlgName: string;
  lJPayload: TJSONObject;
begin
  lPieces := Token.Split(['.']);
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
      Result :=
        Token = lPieces[0] + '.' + lPieces[1] + '.' +
        B64Encode(
        HMAC(lAlgName, lPieces[0] + '.' + lPieces[1], Key)
        );
    finally
      lJPayload.Free;
    end;
  finally
    lJHeader.Free;
  end;
end;

procedure Main;
var
  lHeader, lPayload: TJSONObject;
  lHeaderEncoded, lPayloadEncoded, lToken, lHash: String;
  lBytes: TBytes;
begin
  lHeader := TJSONObject.Create;
  try
    lPayload := TJSONObject.Create;
    try
      lHeader.AddPair('alg', 'HS256').AddPair('typ', 'JWT');
      lPayload.AddPair('sub', 'daniele').AddPair('name', 'Daniele Teti');

      lHeaderEncoded := B64Encode(lHeader.ToJSON);
      lPayloadEncoded := B64Encode(lPayload.ToJSON);
      lToken := lHeaderEncoded + '.' + lPayloadEncoded;
      lBytes := HMAC('HS256', lToken, HMAC_SECRET);
      lHash := B64Encode(lBytes);
      lToken := lToken + '.' + lHash;
      WriteLn(lToken);

      if IsValidToken(lToken, HMAC_SECRET) then
        WriteLn('Token is valid')
      else
        WriteLn('Token is NOT valid');
    finally
      lPayload.Free;
    end;
  finally
    lHeader.Free;
  end;
end;

begin
  try
    Main;
    Readln;
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
