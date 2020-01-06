// *************************************************************************** }
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2020 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.Nullables;

interface

uses
  System.SysUtils, System.Classes;

type
  EMVCNullable = class(Exception)

  end;

  Nullable<T> = record
  private
    fValue: T;
    fHasValue: String;
    function GetHasValue: Boolean;
  public
    procedure CheckHasValue;
    function GetValue: T;
    procedure SetValue(const Value: T);
    class operator Implicit(const Value: T): Nullable<T>;
    class operator Implicit(const Value: Nullable<T>): T;
    property HasValue: Boolean read GetHasValue;
    procedure Clear;
    procedure SetNull;
    function ValueOrDefault: T;
    // properties
    property Value: T read GetValue write SetValue;
  end;

  NullableString = Nullable<String>;

  NullableInt32 = Nullable<Int32>;
  NullableUInt32 = Nullable<UInt32>;
  NullableInteger = NullableInt32;
  NullableLongWord = NullableUInt32;

  NullableUInt16 = Nullable<UInt16>;
  NullableInt16 = Nullable<Int16>;
  NullableWord = Nullable<UInt16>;

  NullableUInt64 = Nullable<UInt64>;
  NullableInt64 = Nullable<Int64>;

  NullableBoolean = Nullable<Boolean>;

  NullableSingle = Nullable<Single>;
  NullableDouble = Nullable<Double>;
  NullableExtended = Nullable<Extended>;

  NullableDate = Nullable<TDate>;
  NullableTime = Nullable<TTime>;
  NullableDateTime = Nullable<TDateTime>;

  NullableCurrency = Nullable<Currency>;

implementation

{ Nullable<T> }

procedure Nullable<T>.CheckHasValue;
begin
  if not GetHasValue then
  begin
    raise EMVCNullable.Create('Value is null');
  end;
end;

procedure Nullable<T>.Clear;
begin
  SetNull;
end;

function Nullable<T>.GetHasValue: Boolean;
begin
  Result := fHasValue = '_';
end;

function Nullable<T>.GetValue: T;
begin
  CheckHasValue;
  Result := fValue;
end;

class operator Nullable<T>.Implicit(const Value: Nullable<T>): T;
begin
  Result := Value.Value;
end;

class operator Nullable<T>.Implicit(const Value: T): Nullable<T>;
begin
  Result.Value := Value;
end;

procedure Nullable<T>.SetNull;
begin
  fValue := Default (T);
  fHasValue := '';
end;

procedure Nullable<T>.SetValue(const Value: T);
begin
  fValue := Value;
  fHasValue := '_';
end;

function Nullable<T>.ValueOrDefault: T;
begin
  if HasValue then
  begin
    Result := GetValue
  end
  else
  begin
    Result := Default (T);
  end;
end;

end.
