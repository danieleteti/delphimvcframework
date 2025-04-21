// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2025 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.Utils;

{$I dmvcframework.inc}

interface

uses
  MVCFramework.Serializer.Commons, JsonDataObjects,
  MVCFramework.DuckTyping, System.Classes, System.SysUtils,
  System.Generics.Collections;

type
  ICSRFTokenManager = interface
    ['{0A7F62EB-1DF6-4F69-945C-75F6A336CF35}']
    /// <summary>
    /// Generates a valid CSRF token for a specified number of minutes.
    /// </summary>
    function GenerateToken(const SecretKey: string; const ExpirationSeconds: Integer): string;

    /// <summary>
    /// Verifies if a given CSRF token is valid and not expired.
    /// </summary>
    function IsTokenValid(const SecretKey: string; const Token: string): Boolean;

    /// <summary>
    /// Verifies if a given CSRF token is expired and, so, not valid.
    /// </summary>
    function IsTokenExpired(const SecretKey: string; const Token: string): Boolean;
  end;

  /// <summary>
  /// Class to handle the generation and verification of signed CSRF tokens.
  /// </summary>
  TCSRFTokenManager = class(TInterfacedObject, ICSRFTokenManager)
  private
    function GetExpiryTime(ExpirationSeconds: Integer): Int64;
    function GenerateHMAC(const SecretKey: string; const Payload: string): string;
    function ExtractPayloadAndSignature(Token: string; out Payload, Signature: string): Boolean;
  public
    constructor Create; virtual;

    /// <summary>
    /// Generates a valid CSRF token for a specified number of minutes.
    /// </summary>
    function GenerateToken(const SecretKey: string; const ExpirationSeconds: Integer): string;

    /// <summary>
    /// Verifies if a given CSRF token is valid and not expired.
    /// </summary>
    function IsTokenValid(const SecretKey: string; const Token: string): Boolean;

    /// <summary>
    /// Verifies if a given CSRF token is expired and, so, not valid. It is an alias for "not IsTokenValid(...,...)"
    /// </summary>
    function IsTokenExpired(const SecretKey: string; const Token: string): Boolean;
  end;

  EHOError = class(Exception)

  end;

  EHOMapError = class(EHOError)

  end;

  EHOFilterError = class(EHOError)

  end;

  EHOForEachError = class(EHOError)

  end;

  EHOReduceError = class(EHOError)

  end;

  TMapClosure<T> = reference to function(const Item: T): T;
  TMapClosure2<T1,T2> = reference to function(const Item: T1): T2;
  TForEachClosure<T> = reference to procedure(const Item: T);
  TMapReduceClosure<T> = reference to function(const Left: T; const Right: T): T;
  TPredicateClosure<T> = reference to function(const Item: T): Boolean;

  HigherOrder = class sealed
  protected
    const INITIAL_RESULT_SIZE = 16;
    const GROW_FACTOR = 1.4;
  public
    class function Map<T>(const InputArray: TArray<T>;
      const MapClosure: TMapClosure<T>): TArray<T>; overload;
    class function Map<T1,T2>(const List: TEnumerable<T1>;
      const MapClosure: TMapClosure2<T1, T2>): TArray<T2>; overload;
    class function Map<T1,T2>(const InputArray: TArray<T1>;
      const MapClosure: TMapClosure2<T1, T2>): TArray<T2>; overload;

    class function Reduce<T>(const InputArray: TArray<T>;
      const ReduceFunction: TMapReduceClosure<T>; InitValue: T): T;

    class function Filter<T>(const InputArray: TArray<T>;
      const FilterFunction: TPredicateClosure<T>): TArray<T>; overload;
    class function Filter<T>(const Enumerable: TEnumerable<T>;
      const FilterFunction: TPredicateClosure<T>): TArray<T>; overload;

    class procedure ForEach<T>(const InputArray: TArray<T>;
      const ForEachClosure: TForEachClosure<T>); overload;
    class procedure ForEach<T>(const Enumerable: TEnumerable<T>;
      const ForEachClosure: TForEachClosure<T>); overload;
  end;

  _ = HigherOrder;

function NewJSONSerializer: IMVCJSONSerializer;
function StrToJSONObject(const aString: String; aRaiseExceptionOnError: Boolean = False): TJsonObject;
function StrToJSONArray(const aString: String; aRaiseExceptionOnError: Boolean = False): TJsonArray;
function WrapAsList(const aObject: TObject; const aOwnsObject: Boolean = False): IMVCList;
function GetMD5HashFromStream(const aStream: TStream): string;
function GetMD5HashFromString(const aString: String): string;
function GetSHA1HashFromString(const aString: String): string;
function GetSHA1HashFromStream(const aStream: TStream): string;
function BytesToHex(const Bytes: TBytes): string;

implementation

uses
{$IF defined(TOKYOORBETTER)}
  System.Hash,
{$ELSE}
  IdHashMessageDigest,
  IdHashSHA,
{$ENDIF}
  MVCFramework.Serializer.JsonDataObjects,
  MVCFramework.Commons,
  System.TypInfo, System.DateUtils;

function GetMD5HashFromStream(const aStream: TStream): string;
{$IF not defined(TOKYOORBETTER)}
var
  lMD5Hash: TIdHashMessageDigest5;
{$ENDIF}
begin
{$IF defined(TOKYOORBETTER)}
  Result := THashMD5.GetHashString(aStream);
{$ELSE}
  lMD5Hash := TIdHashMessageDigest5.Create;
  try
    Result := lMD5Hash.HashStreamAsHex(aStream);
  finally
    lMD5Hash.Free;
  end;
{$ENDIF}
end;

function GetSHA1HashFromStream(const aStream: TStream): string;
{$IF not defined(TOKYOORBETTER)}
var
  lSHA1Hash: TIdHashSHA1;
{$ENDIF}
begin
{$IF defined(TOKYOORBETTER)}
  Result := THashSHA1.GetHashString(aStream);
{$ELSE}
  lSHA1Hash := TIdHashSHA1.Create;
  try
    Result := lSHA1Hash.HashStreamAsHex(aStream);
  finally
    lSHA1Hash.Free;
  end;
{$ENDIF}
end;

function GetMD5HashFromString(const aString: String): string;
{$IF not defined(TOKYOORBETTER)}
var
  lMD5Hash: TIdHashMessageDigest5;
{$ENDIF}
begin
{$IF defined(TOKYOORBETTER)}
  Result := THashMD5.GetHashString(aString);
{$ELSE}
  lMD5Hash := TIdHashMessageDigest5.Create;
  try
    Result := lMD5Hash.HashStringAsHex(aString);
  finally
    lMD5Hash.Free;
  end;
{$ENDIF}
end;

function GetSHA1HashFromString(const aString: String): string;
{$IF not defined(TOKYOORBETTER)}
var
  lSHA1Hash: TIdHashSHA1;
{$ENDIF}
begin
{$IF defined(TOKYOORBETTER)}
  Result := THashSHA1.GetHashString(aString);
{$ELSE}
  lSHA1Hash := TIdHashSHA1.Create;
  try
    Result := lSHA1Hash.HashStringAsHex(aString);
  finally
    lSHA1Hash.Free;
  end;
{$ENDIF}
end;


function NewJSONSerializer: IMVCJSONSerializer;
begin
  Result := TMVCJsonDataObjectsSerializer.Create;
end;

function StrToJSONObject(const aString: String; aRaiseExceptionOnError: Boolean = False): TJsonObject;
begin
  Result := MVCFramework.Serializer.JSONDataObjects.StrToJSONObject(aString, aRaiseExceptionOnError);
end;

function StrToJSONArray(const aString: String; aRaiseExceptionOnError: Boolean = False): TJsonArray;
begin
  Result := MVCFramework.Serializer.JSONDataObjects.StrToJSONArray(aString, aRaiseExceptionOnError);
end;

function WrapAsList(const aObject: TObject; const aOwnsObject: Boolean = False): IMVCList;
begin
  Result := MVCFramework.DuckTyping.WrapAsList(aObject, aOwnsObject);
end;

{ HigherOrder }

class function HigherOrder.Filter<T>(const InputArray: TArray<T>;
  const FilterFunction: TPredicateClosure<T>): TArray<T>;
var
  lIdx, I: Integer;
begin
  lIdx := 0;
  try
    SetLength(Result, INITIAL_RESULT_SIZE);
    for I := 0 to Length(InputArray) - 1 do
    begin
      if FilterFunction(InputArray[I]) then
      begin
        Result[lIdx] := InputArray[I];
        Inc(lIdx);
        if lIdx = Length(Result) then
        begin
          SetLength(Result, Trunc(lIdx * GROW_FACTOR));
        end;
      end;
    end;
    SetLength(Result, lIdx);
  except
    on E: Exception do
    begin
      raise EHOFilterError.CreateFmt
        ('Filter error at index %d - [Class: %s][Message: %s]',
        [lIdx, E.ClassName, E.Message]);
    end;
  end;
end;

class procedure HigherOrder.ForEach<T>(const InputArray: TArray<T>;
  const ForEachClosure: TForEachClosure<T>);
var
  I, lIdx: Integer;
begin
  lIdx := 0;
  try
    for I := Low(InputArray) to High(InputArray) do
    begin
      lIdx := I;
      ForEachClosure(InputArray[I]);
    end;
  except
    on E: Exception do
    begin
      raise EHOForEachError.CreateFmt
        ('ForEach error at index %d - [Class: %s][Message: %s]',
        [lIdx, E.ClassName, E.Message]);
    end;
  end;
end;

class function HigherOrder.Filter<T>(const Enumerable: TEnumerable<T>;
  const FilterFunction: TPredicateClosure<T>): TArray<T>;
var
  lIdx: Integer;
  lItem: T;
begin
  lIdx := 0;
  try
    SetLength(Result, INITIAL_RESULT_SIZE);
    for lItem in Enumerable do
    begin
      if FilterFunction(lItem) then
      begin
        Result[lIdx] := lItem;
        Inc(lIdx);
        if Length(Result) = lIdx then
        begin
          SetLength(Result, Trunc(lIdx * GROW_FACTOR));
        end;
      end;
    end;
    SetLength(Result, lIdx);
  except
    on E: Exception do
    begin
      raise EHOFilterError.CreateFmt
        ('Filter error at index %d - [Class: %s][Message: %s]',
        [lIdx, E.ClassName, E.Message]);
    end;
  end;
end;

class procedure HigherOrder.ForEach<T>(const Enumerable: TEnumerable<T>; const ForEachClosure: TForEachClosure<T>);
var
  lIdx: Integer;
  lItem: T;
begin
  lIdx := 0;
  try
    for lItem in Enumerable do
    begin
      ForEachClosure(lItem);
      Inc(lIdx);
    end;
  except
    on E: Exception do
    begin
      raise EHOForEachError.CreateFmt
        ('ForEach error at element %d - [Class: %s][Message: %s]',
        [lIdx, E.ClassName, E.Message]);
    end;
  end;
end;

class function HigherOrder.Map<T1, T2>(const List: TEnumerable<T1>; const MapClosure: TMapClosure2<T1, T2>): TArray<T2>;
var
  lIdx: Integer;
  lItem: T1;
begin
  Result := nil;
  lIdx := 0;
  try
    SetLength(Result, INITIAL_RESULT_SIZE);
    for lItem in List do
    begin
      Result[lIdx] := MapClosure(lItem);
      Inc(lIdx);
      if lIdx = Length(Result) then
      begin
        SetLength(Result, Trunc(lIdx * GROW_FACTOR));
      end;
    end;
    SetLength(Result, lIdx);
  except
    on E: Exception do
    begin
      raise EHOMapError.CreateFmt
        ('Map error at index %d - [Class: %s][Message: %s]',
        [lIdx, E.ClassName, E.Message]);
    end;
  end;
end;

class function HigherOrder.Map<T1, T2>(const InputArray: TArray<T1>; const MapClosure: TMapClosure2<T1, T2>): TArray<T2>;
var
  lIdx: Integer;
  lItem: T1;
begin
  Result := nil;
  lIdx := 0;
  try
    SetLength(Result, Length(InputArray));
    for lItem in InputArray do
    begin
      Result[lIdx] := MapClosure(lItem);
      Inc(lIdx);
    end;
  except
    on E: Exception do
    begin
      raise EHOMapError.CreateFmt
        ('Map error at index %d - [Class: %s][Message: %s]',
        [lIdx, E.ClassName, E.Message]);
    end;
  end;
end;

class function HigherOrder.Map<T>(const InputArray: TArray<T>;
  const MapClosure: TMapClosure<T>): TArray<T>;
var
  lIdx, I: Integer;
begin
  lIdx := 0;
  try
    SetLength(Result, Length(InputArray));
    for I := 0 to Length(InputArray) - 1 do
    begin
      lIdx := I;
      Result[I] := MapClosure(InputArray[I]);
    end;
  except
    on E: Exception do
    begin
      raise EHOMapError.CreateFmt
        ('Map error at index %d - [Class: %s][Message: %s]',
        [lIdx, E.ClassName, E.Message]);
    end;
  end;
end;

class function HigherOrder.Reduce<T>(const InputArray: TArray<T>;
  const ReduceFunction: TMapReduceClosure<T>; InitValue: T): T;
var
  I: T;
  lIdx: Integer;
begin
  lIdx := 0;
  Result := InitValue;
  try
    for I in InputArray do
    begin
      Result := ReduceFunction(Result, I);
      Inc(lIdx);
    end;
  except
    on E: Exception do
    begin
      raise EHOReduceError.CreateFmt
        ('Reduce error at index %d - [Class: %s][Message: %s]',
        [lIdx, E.ClassName, E.Message]);
    end;
  end;
end;

function BytesToHex(const Bytes: TBytes): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Length(Bytes) - 1 do
    Result := Result + IntToHex(Bytes[I], 2); // Formatta ciascun byte come due cifre esadecimali
end;

{ TCSRFTokenManager }

constructor TCSRFTokenManager.Create;
begin
  inherited;
end;

/// <summary>
/// Calculates the expiration timestamp in milliseconds.
/// </summary>
function TCSRFTokenManager.GetExpiryTime(ExpirationSeconds: Integer): Int64;
begin
  Result := DateTimeToUnix(Now() + OneSecond * ExpirationSeconds);
end;

/// <summary>
/// Generates an HMAC-SHA256 signature for the given payload.
/// </summary>
function TCSRFTokenManager.GenerateHMAC(const SecretKey: string; const Payload: string): string;
begin
  Result := BytesToHex(THashSHA2.GetHMACAsBytes(Payload, SecretKey));
end;

/// <summary>
/// Extracts the payload and signature from a token.
/// </summary>
function TCSRFTokenManager.ExtractPayloadAndSignature(Token: string; out Payload, Signature: string): Boolean;
var
  Parts: TArray<string>;
begin
  Parts := Token.Split(['.']);
  if Length(Parts) = 2 then
  begin
    Payload := Parts[0];
    Signature := Parts[1];
    Result := True;
  end
  else
  begin
    Payload := '';
    Signature := '';
    Result := False;
  end;
end;

/// <summary>
/// Generates a signed CSRF token.
/// </summary>
function TCSRFTokenManager.GenerateToken(const SecretKey: string; const ExpirationSeconds: Integer): string;
var
  Payload: string;
  ExpiryTime: Int64;
begin
  // Calculate the expiration timestamp
  ExpiryTime := GetExpiryTime(ExpirationSeconds);

  // Create the payload with the expiration timestamp
  Payload := IntToStr(ExpiryTime);

  // Generate the token as: <payload>.<signature>
  Result := Payload + '.' + GenerateHMAC(SecretKey, Payload);
end;

function TCSRFTokenManager.IsTokenExpired(const SecretKey, Token: string): Boolean;
begin
  Result := not IsTokenValid(SecretKey, Token);
end;

/// <summary>
/// Verifies if a given CSRF token is valid and not expired.
/// </summary>
function TCSRFTokenManager.IsTokenValid(const SecretKey: string; const Token: string): Boolean;
var
  Payload, Signature, ExpectedSignature: string;
  CurrentTime, ExpiryTime: Int64;
begin
  // Extract the payload and signature from the token
  if not ExtractPayloadAndSignature(Token, Payload, Signature) then
  begin
    Result := False;
    Exit;
  end;

  // Recreate the expected signature using the payload and secret key
  ExpectedSignature := GenerateHMAC(SecretKey, Payload);
  if not SameText(Signature, ExpectedSignature) then
  begin
    Result := False;
    Exit;
  end;

  // Extract the expiration timestamp from the payload
  try
    ExpiryTime := StrToInt64(Payload);
  except
    Result := False;
    Exit;
  end;

  // Get the current time in milliseconds
  CurrentTime := DateTimeToUnix(Now());

  // Check if the token has expired
  Result := CurrentTime <= ExpiryTime;
end;

end.
