// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// Some code in this file is from Thijs van Dien (https://stackoverflow.com/users/1163893/thijs-van-dien)
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

  try
    PRF := PRFC.Create;
  except
    on E: Exception do
    begin
      if E.Message.Contains('is not available') then
        raise EMVCException.Create(HTTP_STATUS.InternalServerError,
          E.Message +
          ' [HINT: May be OpenSSL is not been loaded yet. Did you invoked MVCCryptInit?]');
      raise EMVCException.Create(HTTP_STATUS.InternalServerError, E.Message);
    end;
  end;
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

procedure MVCCryptInit;
begin
  if not LoadOpenSSLLibrary then
  begin
    raise EMVCException.Create('Cannot load OpenSSL');
  end;
end;

end.
