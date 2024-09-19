// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2024 Daniele Teti and the DMVCFramework Team
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
  MVCFramework.DuckTyping, System.Classes, System.SysUtils;

type
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
  TForEachClosure<T> = reference to procedure(const Item: T);
  TMapReduceClosure<T> = reference to function(const Left: T; const Right: T): T;
  TPredicateClosure<T> = reference to function(const Item: T): Boolean;

  HigherOrder = class sealed
    class function Map<T>(const InputArray: TArray<T>;
      const MapClosure: TMapClosure<T>): TArray<T>;
    class function Reduce<T>(const InputArray: TArray<T>;
      const ReduceFunction: TMapReduceClosure<T>; InitValue: T): T;
    class function Filter<T>(const InputArray: TArray<T>;
      const FilterFunction: TPredicateClosure<T>): TArray<T>;
    class procedure ForEach<T>(const InputArray: TArray<T>;
      const ForEachClosure: TForEachClosure<T>);
  end;

function NewJSONSerializer: IMVCJSONSerializer;
function StrToJSONObject(const aString: String; aRaiseExceptionOnError: Boolean = False): TJsonObject;
function StrToJSONArray(const aString: String; aRaiseExceptionOnError: Boolean = False): TJsonArray;
function WrapAsList(const aObject: TObject; const aOwnsObject: Boolean = False): IMVCList;
function GetMD5HashFromStream(const aStream: TStream): string;
function GetMD5HashFromString(const aString: String): string;
function GetSHA1HashFromString(const aString: String): string;
function GetSHA1HashFromStream(const aStream: TStream): string;

implementation

uses
{$IF defined(TOKYOORBETTER)}
  System.Hash,
{$ELSE}
  IdHashMessageDigest,
  IdHashSHA,
{$ENDIF}
  System.Generics.Collections,
  MVCFramework.Serializer.JsonDataObjects,
  MVCFramework.Commons,
  System.TypInfo;

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
  lList: TList<T>;
begin
  lIdx := -1;
  try
    lList := TList<T>.Create;
    try
      for I := 0 to Length(InputArray) - 1 do
      begin
        lIdx := I;
        if FilterFunction(InputArray[I]) then
        begin
          lList.add(InputArray[I]);
        end;
      end;
      Result := lList.ToArray;
    finally
      lList.Free;
    end;
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

end.
