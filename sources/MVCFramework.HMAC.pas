unit MVCFramework.HMAC;

interface

uses
  System.SysUtils,
  EncdDecd,
  IdHMAC;

type
  EMVCHMACException = class(Exception)

  end;

  THMACClass = class of TIdHMAC;

function HMAC(const Algorithm: String; const Input, Key: string): TBytes;
procedure RegisterHMACAlgorithm(const Algorithm: String; Clazz: THMACClass);
procedure UnRegisterHMACAlgorithm(const Algorithm: String);

implementation

uses
  IdSSLOpenSSL, IdHash, IdGlobal, IdHMACMD5,
  IdHMACSHA1, System.Generics.Collections;

var
  GHMACRegistry: TDictionary<string, THMACClass>;

function HMAC(const Algorithm: String; const Input, Key: string): TBytes;
var
  lHMAC: TIdHMAC;
begin
  if not GHMACRegistry.ContainsKey(Algorithm) then
    raise EMVCHMACException.CreateFmt('Unknown HMAC algorithm [%s]', [Algorithm]);

  lHMAC := GHMACRegistry[Algorithm].Create;
  try
    lHMAC.Key := ToBytes(Key);
    Result := TBytes(lHMAC.HashValue(ToBytes(Input)));
  finally
    lHMAC.Free;
  end;
end;

procedure RegisterHMACAlgorithm(const Algorithm: String; Clazz: THMACClass);
begin
  if GHMACRegistry.ContainsKey(Algorithm) then
    raise EMVCHMACException.Create('Algorithm already registered');
  GHMACRegistry.Add(Algorithm, Clazz);
end;

procedure UnRegisterHMACAlgorithm(const Algorithm: String);
begin
  GHMACRegistry.Remove(Algorithm);
end;

initialization

Assert(IdSSLOpenSSL.LoadOpenSSLLibrary, 'HMAC requires OpenSSL libraries');

GHMACRegistry := TDictionary<string, THMACClass>.Create;

//registering based on hash function
RegisterHMACAlgorithm('md5', TIdHMACMD5);
RegisterHMACAlgorithm('sha1', TIdHMACSHA1);
RegisterHMACAlgorithm('sha224', TIdHMACSHA224);
RegisterHMACAlgorithm('sha256', TIdHMACSHA256);
RegisterHMACAlgorithm('sha384', TIdHMACSHA384);
RegisterHMACAlgorithm('sha512', TIdHMACSHA512);


//the same using the JWT naming
RegisterHMACAlgorithm('HS256', TIdHMACSHA256);
RegisterHMACAlgorithm('HS384', TIdHMACSHA384);
RegisterHMACAlgorithm('HS512', TIdHMACSHA512);

finalization

GHMACRegistry.Free;

end.
