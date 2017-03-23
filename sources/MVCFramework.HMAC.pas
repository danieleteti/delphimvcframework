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

{$I dmvcframework.inc}

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  EncdDecd,
  IdHMAC;

type

  EMVCHMACException = class(Exception)
  private
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }
  end;

  THMACClass = class of TIdHMAC;

function HMAC(const AAlgorithm: String; const AInput, AKey: string): TBytes;
procedure RegisterHMACAlgorithm(const AAlgorithm: String; AClazz: THMACClass);
procedure UnRegisterHMACAlgorithm(const AAlgorithm: String);

implementation

uses
  IdSSLOpenSSL,
  IdHash,
  IdGlobal,
  IdHMACMD5,
  IdHMACSHA1;

var
  GHMACRegistry: TDictionary<string, THMACClass>;

function HMAC(const AAlgorithm: String; const AInput, AKey: string): TBytes;
var
  LHMAC: TIdHMAC;
begin
  if not GHMACRegistry.ContainsKey(AAlgorithm) then
    raise EMVCHMACException.CreateFmt('Unknown HMAC algorithm [%s]', [AAlgorithm]);

  LHMAC := GHMACRegistry[AAlgorithm].Create;
  try
    LHMAC.Key := ToBytes(AKey);
    Result := TBytes(LHMAC.HashValue(ToBytes(AInput)));
  finally
    LHMAC.Free;
  end;
end;

procedure RegisterHMACAlgorithm(const AAlgorithm: String; AClazz: THMACClass);
begin
  if GHMACRegistry.ContainsKey(AAlgorithm) then
    raise EMVCHMACException.Create('Algorithm already registered');
  GHMACRegistry.Add(AAlgorithm, AClazz);
end;

procedure UnRegisterHMACAlgorithm(const AAlgorithm: String);
begin
  GHMACRegistry.Remove(AAlgorithm);
end;

initialization

Assert(IdSSLOpenSSL.LoadOpenSSLLibrary, 'HMAC requires OpenSSL libraries');

GHMACRegistry := TDictionary<string, THMACClass>.Create;

// registering based on hash function
RegisterHMACAlgorithm('md5', TIdHMACMD5);
RegisterHMACAlgorithm('sha1', TIdHMACSHA1);
RegisterHMACAlgorithm('sha224', TIdHMACSHA224);
RegisterHMACAlgorithm('sha256', TIdHMACSHA256);
RegisterHMACAlgorithm('sha384', TIdHMACSHA384);
RegisterHMACAlgorithm('sha512', TIdHMACSHA512);

// the same using the JWT naming
RegisterHMACAlgorithm('HS256', TIdHMACSHA256);
RegisterHMACAlgorithm('HS384', TIdHMACSHA384);
RegisterHMACAlgorithm('HS512', TIdHMACSHA512);

finalization

GHMACRegistry.Free;

end.
