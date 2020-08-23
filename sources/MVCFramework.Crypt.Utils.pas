unit MVCFramework.Crypt.Utils;

interface

uses
  System.SysUtils, System.Math, IdHMAC, IdHMACSHA1, IdGlobal;

type
  TIdHMACClass = class of TIdHMAC;
function PBKDF2(const Password: TBytes; const Salt: TBytes; const IterationsCount: Integer;
  const KeyLengthInBytes: Integer; PRFC: TIdHMACClass = nil): TBytes;
function BytesToHexString(const _B: TIdBytes): string; overload;
function BytesToHexString(const _B: TBytes): string; overload;
procedure TestPBKDF2SHA1HMAC;
procedure MVCCryptInit;

implementation

uses
  IdSSLOpenSSL, MVCFramework.Commons;

{
  PBKDF2 algorithm implementation from
  https://stackoverflow.com/a/20134669/6825479
  and then slighly modified. Copyright, if any, is from the original owner.
}
// Modeled after http://www.di-mgt.com.au/cryptoKDFs.html#PKCS5
function PBKDF2(const Password: TBytes; const Salt: TBytes; const IterationsCount: Integer;
  const KeyLengthInBytes: Integer; PRFC: TIdHMACClass = nil): TBytes;
var
  PRF: TIdHMAC;
  D: Integer;
  I: Int32;
  F: TIdBytes;
  U: TIdBytes;
  J: Integer;
  T: TIdBytes;
  lPassword, lSalt: TIdBytes;

  function _ConcatenateBytes(const _B1: TIdBytes; const _B2: TIdBytes): TIdBytes; inline;
  begin
    SetLength(Result, Length(_B1) + Length(_B2));
    if Length(_B1) > 0 then
      Move(_B1[low(_B1)], Result[low(Result)], Length(_B1));
    if Length(_B2) > 0 then
      Move(_B2[low(_B2)], Result[low(Result) + Length(_B1)], Length(_B2));
  end;

  function _INT_32_BE(const _I: Int32): TIdBytes; inline;
  begin
    Result := TIdBytes.Create(_I shr 24, _I shr 16, _I shr 8, _I);
  end;

  procedure _XorBytes(var _B1: TIdBytes; const _B2: TIdBytes); inline;
  var
    _I: Integer;
  begin
    for _I := low(_B1) to high(_B1) do
      _B1[_I] := _B1[_I] xor _B2[_I];
  end;

begin
  if not Assigned(PRFC) then
    PRFC := TIdHMACSHA1;

  PRF := PRFC.Create;
  try
    {
      Conversion TBytes -> TidBytes as Remy Lebeau says
      https://stackoverflow.com/a/18854367/6825479
    }
    lPassword := TIdBytes(Password);
    lSalt := TIdBytes(Salt);

    D := Ceil(KeyLengthInBytes / PRF.HashSize);
    PRF.Key := lPassword;
    for I := 1 to D do
    begin
      F := PRF.HashValue(_ConcatenateBytes(lSalt, _INT_32_BE(I)));
      U := Copy(F);
      for J := 2 to IterationsCount do
      begin
        U := PRF.HashValue(U);
        _XorBytes(F, U);
      end;
      T := _ConcatenateBytes(T, F);
    end;
    Result := TBytes(Copy(T, low(T), KeyLengthInBytes));
  finally
    PRF.Free;
  end;
end;

function BytesToHexString(const _B: TIdBytes): string; overload;
var
  _I: Integer;
begin
  for _I := low(_B) to high(_B) do
    Result := Result + IntToHex(_B[_I], 2);
end;

function BytesToHexString(const _B: TBytes): string; overload;
begin
  Result := BytesToHexString(TIdBytes(_B));
end;

procedure TestPBKDF2SHA1HMAC;
var
  P: TBytes;
  S: TBytes;
  K: TBytes;
begin
  P := TBytes.Create($70, $61, $73, $73, $77, $6F, $72, $64);
  S := TBytes.Create($78, $57, $8E, $5A, $5D, $63, $CB, $06);

  K := PBKDF2(P, S, 2048, 24);
  Assert('BFDE6BE94DF7E11DD409BCE20A0255EC327CB936FFE93643' = BytesToHexString(K));

  P := TBytes.Create($70, $61, $73, $73, $77, $6F, $72, $64);
  S := TBytes.Create($73, $61, $6C, $74);

  K := PBKDF2(P, S, 1, 20);
  Assert('0C60C80F961F0E71F3A9B524AF6012062FE037A6' = BytesToHexString(K));
  K := PBKDF2(P, S, 2, 20);
  Assert('EA6C014DC72D6F8CCD1ED92ACE1D41F0D8DE8957' = BytesToHexString(K));
  K := PBKDF2(P, S, 4096, 20);
  Assert('4B007901B765489ABEAD49D926F721D065A429C1' = BytesToHexString(K));
  K := PBKDF2(P, S, 16777216, 20);
  Assert('EEFE3D61CD4DA4E4E9945B3D6BA2158C2634E984' = BytesToHexString(K));

  P := TBytes.Create($70, $61, $73, $73, $77, $6F, $72, $64, $50, $41, $53, $53, $57, $4F, $52, $44,
    $70, $61, $73, $73, $77, $6F, $72, $64);
  S := TBytes.Create($73, $61, $6C, $74, $53, $41, $4C, $54, $73, $61, $6C, $74, $53, $41, $4C, $54,
    $73, $61, $6C, $74, $53, $41, $4C, $54, $73, $61, $6C, $74, $53, $41, $4C, $54, $73, $61,
    $6C, $74);

  K := PBKDF2(P, S, 4096, 25);
  Assert('3D2EEC4FE41C849B80C8D83662C0E44A8B291A964CF2F07038' = BytesToHexString(K));
end;

procedure MVCCryptInit;
begin
  if not LoadOpenSSLLibrary then
  begin
    raise EMVCException.Create('Cannot load OpenSSL');
  end;
end;

end.
