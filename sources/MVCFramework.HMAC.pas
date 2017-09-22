// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2017 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.HMAC;

interface

uses
  System.SysUtils,
  EncdDecd,
  IdHMAC;

type
  EMVCHMACException = class(Exception)

  end;

  IHMAC = interface
    ['{95134024-BBAF-4E52-A4C3-54189672E18A}']
    function HashValue(const Input, key: String): TBytes;
  end;


function HMAC(const Algorithm: String; const Input, Key: string): TBytes;
procedure RegisterHMACAlgorithm(const Algorithm: String; Impl: IHMAC);
procedure UnRegisterHMACAlgorithm(const Algorithm: String);

implementation

uses

  IdSSLOpenSSL, IdHash, IdGlobal, IdHMACMD5,
  IdHMACSHA1, System.Generics.Collections;

var
  GHMACRegistry: TDictionary<string, IHMAC>;

type
  THMACClass = class of TIdHMAC;

  TIdHMACWrapper = class(TInterfacedObject, IHMAC)
  private
    FClass: THMACClass;
  public
    constructor Create(IdClass: THMACClass);
    function HashValue(const Input: string;
      const key: string): System.TArray<System.Byte>;

  end;

function HMAC(const Algorithm: String; const Input, Key: string): TBytes;
begin
  if not GHMACRegistry.ContainsKey(Algorithm) then
    raise EMVCHMACException.CreateFmt('Unknown HMAC algorithm [%s]', [Algorithm]);

  result := GHMACRegistry[Algorithm].HashValue(Input, Key);
end;

procedure RegisterHMACAlgorithm(const Algorithm: String; Impl: IHMAC);
begin
  if GHMACRegistry.ContainsKey(Algorithm) then
    raise EMVCHMACException.Create('Algorithm already registered');
  GHMACRegistry.Add(Algorithm, Impl);
end;

procedure UnRegisterHMACAlgorithm(const Algorithm: String);
begin
  GHMACRegistry.Remove(Algorithm);
end;



{ TIdHMACWrapper }

constructor TIdHMACWrapper.Create(IdClass: THMACClass);
begin
  FClass := IdClass;
end;

function TIdHMACWrapper.HashValue(const Input,
  key: string): System.TArray<System.Byte>;
var
  lHMAC: TIdHMAC;
begin
  Assert(IdSSLOpenSSL.LoadOpenSSLLibrary, 'HMAC requires OpenSSL libraries');
  lHMAC := FClass.Create;
  try
    lHMAC.Key := ToBytes(Key, IndyTextEncoding_UTF8);
    Result := TBytes(lHMAC.HashValue(ToBytes(Input, IndyTextEncoding_UTF8)));
  finally
    lHMAC.Free;
  end;
end;

initialization



GHMACRegistry := TDictionary<string, IHMAC>.Create;

//registering based on hash function
RegisterHMACAlgorithm('md5', TIdHMACWrapper.create(TIdHMACMD5));
RegisterHMACAlgorithm('sha1', TIdHMACWrapper.create(TIdHMACSHA1));
RegisterHMACAlgorithm('sha224', TIdHMACWrapper.create(TIdHMACSHA224));
RegisterHMACAlgorithm('sha256', TIdHMACWrapper.create(TIdHMACSHA256));
RegisterHMACAlgorithm('sha384', TIdHMACWrapper.create(TIdHMACSHA384));
RegisterHMACAlgorithm('sha512', TIdHMACWrapper.create(TIdHMACSHA512));


//the same using the JWT naming
RegisterHMACAlgorithm('HS256', TIdHMACWrapper.create(TIdHMACSHA256));
RegisterHMACAlgorithm('HS384', TIdHMACWrapper.create(TIdHMACSHA384));
RegisterHMACAlgorithm('HS512', TIdHMACWrapper.create(TIdHMACSHA512));

finalization

GHMACRegistry.Free;

end.
