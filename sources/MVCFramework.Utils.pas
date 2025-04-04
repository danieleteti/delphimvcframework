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

end.
