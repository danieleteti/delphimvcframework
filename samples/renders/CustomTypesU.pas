// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// Collaborators with this file: Ezequiel Juliano Müller (ezequieljuliano@gmail.com)
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

unit CustomTypesU;

interface

uses
  System.Generics.Defaults, System.Generics.Collections;

type
  // custom serialization is by-type so we define a type alias
  // useful to identify all the fields that must be serialized
  // using the custom serializer defined for this type
  TUserRoles = TArray<string>;

  TNullableRecord<T> = record
    Value: T;
    HasValue: Boolean;
  end;

  TNullableRecordAlias = TNullableRecord<string>;

  // This is the main object which uses the
  // custom serialized type as property Roles
  TSysUser = class
  private
    FUserName: string;
    FRoles: TUserRoles;
    fRecordAlias: TNullableRecordAlias;
    procedure SetUserName(const Value: string);
    function GetUserRoles: TUserRoles;
  public
    constructor Create(aUserName: string; aRoles: TUserRoles);
    property UserName: string read FUserName write SetUserName;
    // Here we are using the custom-serialized type TUserRoles
    property Roles: TUserRoles read GetUserRoles;
    property RecordAlias: TNullableRecordAlias read fRecordAlias write fRecordAlias;
  end;

  TArrayTest = class
  private
    fStrings: TArray<string>;
    fIntegers: TArray<Integer>;
    fDoubles: TArray<Double>;
    fEmptyIntegers: TArray<Integer>;
    fNilArrayOfIntegers: TArray<Integer>;
  public
    constructor Create;
    property Integers: TArray<Integer> read fIntegers write fIntegers;
    property Strings: TArray<string> read fStrings write fStrings;
    property Doubles: TArray<Double> read fDoubles write fDoubles;
    property EmptyIntegers: TArray<Integer> read fEmptyIntegers write fEmptyIntegers;
    property NilArrayOfIntegers: TArray<Integer> read fNilArrayOfIntegers write fNilArrayOfIntegers;
  end;

  TSimpleListTest = class
  private
    fDoubles: TList<Double>;
    fStrings: TList<string>;
    fIntegers: TList<Integer>;
  public
    constructor Create;
    destructor Destroy; override;
    property Integers: TList<Integer> read fIntegers;
    property Strings: TList<string> read fStrings;
    property Doubles: TList<Double> read fDoubles;
  end;

implementation

uses
  System.SysUtils, System.Math;

{ TSysUser }

constructor TSysUser.Create(aUserName: string; aRoles: TUserRoles);
begin
  inherited Create;
  FUserName := aUserName;
  FRoles := aRoles;
end;

function TSysUser.GetUserRoles: TUserRoles;
begin
  Result := FRoles;
end;

procedure TSysUser.SetUserName(const Value: string);
begin
  FUserName := Value;
end;

{ TArrayTest }

constructor TArrayTest.Create;
var
  I: Integer;
begin
  inherited;
  fNilArrayOfIntegers := nil;
  SetLength(fEmptyIntegers, 0);
  SetLength(fStrings, 5);
  for I := 0 to Length(fStrings) - 1 do
  begin
    fStrings[I] := 'Value ' + I.ToString;
  end;
  SetLength(fIntegers, 5);
  for I := 0 to Length(fIntegers) - 1 do
  begin
    fIntegers[I] := I;
  end;
  SetLength(fDoubles, 5);
  for I := 0 to Length(fDoubles) - 1 do
  begin
    fDoubles[I] := Power(I, I) * 1.1;
  end;
end;

{ TSimpleListTest }

constructor TSimpleListTest.Create;
begin
  inherited;
  fIntegers := TList<Integer>.Create();
  fIntegers.AddRange([1, 2, 3, 4, 5]);
  fStrings := TList<string>.Create();
  fStrings.AddRange(['good', 'bye', 'cruel', 'world']);
  fDoubles := TList<Double>.Create();
  fDoubles.AddRange([1.2, 2.3, 3.4, 4.5, 5.6]);
end;

destructor TSimpleListTest.Destroy;
begin
  fIntegers.Free;
  fStrings.Free;
  fDoubles.Free;
  inherited;
end;

end.
