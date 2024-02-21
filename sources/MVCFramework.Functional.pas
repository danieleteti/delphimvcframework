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
// *************************************************************************** }

unit MVCFramework.Functional;

interface

uses
  System.SysUtils, System.Classes;


type
  EFunctionalError = class(Exception)

  end;
  EFunctionalMapError = class(EFunctionalError)

  end;

  EFunctionalFilterError = class(EFunctionalError)

  end;

  EFunctionalForEachError = class(EFunctionalError)

  end;

  EFunctionalReduceError = class(EFunctionalError)

  end;

  TMapClosure<T> = reference to function(const Item: T): T;
  TForEachClosure<T> = reference to procedure(const Item: T);
  TMapReduce<T> = reference to function(const Left: T; const Right: T): T;
  TPredicate<T> = reference to function(const Item: T): Boolean;
  Functional = class sealed
    class function Map<T>(InputArray: TArray<T>; MapClosure: TMapClosure<T>): TArray<T>;
    class function Reduce<T: record >(InputArray: TArray<T>; ReduceFunction: TMapReduce<T>; InitValue: T): T;
    class function Filter<T>(InputArray: TArray<T>; FilterFunction: TPredicate<T>): TArray<T>;
    class procedure ForEach<T>(InputArray: TArray<T>; ForEachClosure: TForEachClosure<T>);
  end;

implementation

uses
  System.Generics.Collections;

{ Functional }

class function Functional.Filter<T>(InputArray: TArray<T>; FilterFunction: TPredicate<T>): TArray<T>;
var
  lIdx, I: Integer;
  List: TList<T>;
begin
  lIdx := -1;
  try
    List := TList<T>.Create;
    try
      for I := 0 to length(InputArray) - 1 do
      begin
        lIdx := I;
        if FilterFunction(InputArray[I]) then
        begin
          List.add(InputArray[I]);
        end;
      end;
      Result := List.ToArray;
    finally
      List.Free;
    end;
  except
    on E: Exception do
    begin
      raise EFunctionalFilterError.CreateFmt('Filter error at index %d - [Class: %s][Message: %s]', [lIdx, E.ClassName, E.Message]);
    end;
  end;
end;

class procedure Functional.ForEach<T>(InputArray: TArray<T>; ForEachClosure: TForEachClosure<T>);
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
      raise EFunctionalForEachError.CreateFmt('ForEach error at index %d - [Class: %s][Message: %s]', [lIdx, E.ClassName, E.Message]);
    end;
  end;
end;

class function Functional.Map<T>(InputArray: TArray<T>; MapClosure: TMapClosure<T>): TArray<T>;
var
  lIdx, I: Integer;
begin
  lIdx := 0;
  try
    SetLength(Result, Length(InputArray));
    for I := 0 to length(InputArray) - 1 do
    begin
      lIdx := I;
      Result[I] := MapClosure(InputArray[I]);
    end;
  except
    on E: Exception do
    begin
      raise EFunctionalMapError.CreateFmt('Map error at index %d - [Class: %s][Message: %s]', [lIdx, E.ClassName, E.Message]);
    end;
  end;
end;

class function Functional.Reduce<T>(InputArray: TArray<T>; ReduceFunction: TMapReduce<T>; InitValue: T): T;
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
      raise EFunctionalReduceError.CreateFmt('Reduce error at index %d - [Class: %s][Message: %s]', [lIdx, E.ClassName, E.Message]);
    end;
  end;
end;

end.
