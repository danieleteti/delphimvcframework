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

unit MVCFramework.HMAC;

interface

uses
  System.SysUtils;

type
  EMVCHMACException = class(Exception)

  end;

  IHMAC = interface
    ['{95134024-BBAF-4E52-A4C3-54189672E18A}']
    function HashValue(const Input, key: string): TBytes;
  end;

function HMAC(const Algorithm: string; const Input, Key: string): TBytes;
procedure RegisterHMACAlgorithm(const Algorithm: string; Impl: IHMAC);
procedure UnRegisterHMACAlgorithm(const Algorithm: string);

const
  // registering based on hash function
  HMAC_MD5 = 'md5';
  HMAC_SHA1 = 'sha1';
  HMAC_SHA224 = 'sha224';
  HMAC_SHA256 = 'sha256';
  HMAC_SHA384 = 'sha384';
  HMAC_SHA512 = 'sha512';

  // the same using the JWT naming
  HMAC_HS256 = 'HS256';
  HMAC_HS384 = 'HS384';
  HMAC_HS512 = 'HS512';

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 30.0}
    {$DEFINE USEBUILTINHMAC}
  {$ENDIF}
{$ENDIF}


implementation

uses

{$IFDEF USEBUILTINHMAC}
  System.Hash,
{$ELSE}
  IdHMAC, IdSSLOpenSSL, IdHash, IdGlobal, IdHMACMD5, IdHMACSHA1,
{$ENDIF}
  System.Generics.Collections;

var
  GHMACRegistry: TDictionary<string, IHMAC>;

type

{$IFDEF USEBUILTINHMAC}

  TSHA2HMACWrapper = class(TInterfacedObject, IHMAC)
  private
    FHMACType : THashSHA2.TSHA2Version;
  public
    constructor Create(HMACType: THashSHA2.TSHA2Version);
    function HashValue(const Input: string; const key: string): TBytes;
 end;

  TSHA1HMACWrapper = class(TInterfacedObject, IHMAC)
  public
    function HashValue(const Input: string; const key: string): TBytes;
 end;

  TMD5HMACWrapper = class(TInterfacedObject, IHMAC)
  public
    function HashValue(const Input: string; const key: string): TBytes;
 end;

{$ELSE}

  THMACClass = class of TIdHMAC;

  TIdHMACWrapper = class(TInterfacedObject, IHMAC)
  private
    FClass: THMACClass;
  public
    constructor Create(IdClass: THMACClass);
    function HashValue(const Input: string;
      const key: string): System.TArray<System.Byte>;

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

{$ENDIF}


function HMAC(const Algorithm: string; const Input, Key: string): TBytes;
begin
  if not GHMACRegistry.ContainsKey(Algorithm) then
    raise EMVCHMACException.CreateFmt('Unknown HMAC algorithm [%s]', [Algorithm]);

  result := GHMACRegistry[Algorithm].HashValue(Input, Key);
end;

procedure RegisterHMACAlgorithm(const Algorithm: string; Impl: IHMAC);
begin
  if GHMACRegistry.ContainsKey(Algorithm) then
    raise EMVCHMACException.Create('Algorithm already registered');
  GHMACRegistry.Add(Algorithm, Impl);
end;

procedure UnRegisterHMACAlgorithm(const Algorithm: string);
begin
  GHMACRegistry.Remove(Algorithm);
end;



{ THMACSHA256 }

{$IFDEF USEBUILTINHMAC}
constructor TSHA2HMACWrapper.Create(HMACType: THashSHA2.TSHA2Version);
begin
  FHMACType := HMACType;
end;

function TSHA2HMACWrapper.HashValue(const Input, key: string): TBytes;
begin
  Result := THashSHA2.GetHMACAsBytes(Input,key,FHMACType);
end;

{ TSHA1HMACWrapper }

function TSHA1HMACWrapper.HashValue(const Input, key: string): TBytes;
begin
  Result := THashSHA1.GetHMACAsBytes(Input,Key);
end;

{ TMD5HMACWrapper }

function TMD5HMACWrapper.HashValue(const Input, key: string): TBytes;
begin
  Result := THashMD5.GetHMACAsBytes(Input,Key);
end;

procedure RegisterBuiltinHMACFunctions;
begin
  RegisterHMACAlgorithm('md5', TMD5HMACWrapper.create);
  RegisterHMACAlgorithm('sha1', TSHA1HMACWrapper.create);
  RegisterHMACAlgorithm('sha224', TSHA2HMACWrapper.create(THashSHA2.TSHA2Version.SHA224));
  RegisterHMACAlgorithm('sha256', TSHA2HMACWrapper.create(THashSHA2.TSHA2Version.SHA256));
  RegisterHMACAlgorithm('sha384', TSHA2HMACWrapper.create(THashSHA2.TSHA2Version.SHA384));
  RegisterHMACAlgorithm('sha512', TSHA2HMACWrapper.create(THashSHA2.TSHA2Version.SHA512));

  // the same using the JWT naming
  RegisterHMACAlgorithm('HS256', TSHA2HMACWrapper.create(THashSHA2.TSHA2Version.SHA256));
  RegisterHMACAlgorithm('HS384', TSHA2HMACWrapper.create(THashSHA2.TSHA2Version.SHA384));
  RegisterHMACAlgorithm('HS512', TSHA2HMACWrapper.create(THashSHA2.TSHA2Version.SHA512));
end;
{$ELSE}

procedure RegisterIndyHMACFunctions;
begin
  RegisterHMACAlgorithm('md5', TIdHMACWrapper.create(TIdHMACMD5));
  RegisterHMACAlgorithm('sha1', TIdHMACWrapper.create(TIdHMACSHA1));
  RegisterHMACAlgorithm('sha224', TIdHMACWrapper.create(TIdHMACSHA224));
  RegisterHMACAlgorithm('sha256', TIdHMACWrapper.create(TIdHMACSHA256));
  RegisterHMACAlgorithm('sha384', TIdHMACWrapper.create(TIdHMACSHA384));
  RegisterHMACAlgorithm('sha512', TIdHMACWrapper.create(TIdHMACSHA512));

  // the same using the JWT naming
  RegisterHMACAlgorithm('HS256', TIdHMACWrapper.create(TIdHMACSHA256));
  RegisterHMACAlgorithm('HS384', TIdHMACWrapper.create(TIdHMACSHA384));
  RegisterHMACAlgorithm('HS512', TIdHMACWrapper.create(TIdHMACSHA512));
end;
{$ENDIF}

initialization


GHMACRegistry := TDictionary<string, IHMAC>.Create;

// registering based on hash function

{$IFDEF USEBUILTINHMAC}
RegisterBuiltinHMACFunctions;
{$ELSE}
RegisterIndyHMACFunctions;
{$ENDIF}

finalization

GHMACRegistry.Free;

end.
