{******************************************************************************}
{                                                                              }
{  Delphi SwagDoc Library                                                      }
{  Copyright (c) 2018 Marcelo Jaloto                                           }
{  https://github.com/marcelojaloto/SwagDoc                                    }
{                                                                              }
{******************************************************************************}
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{******************************************************************************}

unit Json.Schema.Field.Numbers;

interface

uses
  System.Json,
  System.Rtti,
  Json.Schema.Field,
  Json.Schema.Common.Types;

type
  TJsonFieldNumber<T> = class abstract (TJsonField)
  strict private
    fDefaultValue: TValue;
    fMinValue: TValue;
    fMaxValue: TValue;
    fFormat: string;

    procedure SetDefaultValue(const Value: T);
    procedure SetMaxValue(const Value: T);
    procedure SetMinValue(const Value: T);

    function GetDefaultValue: T;
    function GetMaxValue: T;
    function GetMinValue: T;
  public
    function ToJsonSchema: TJsonObject; override;
    function Clone: TJsonField; override;

    property MinValue: T read GetMinValue write SetMinValue;
    property MaxValue: T read GetMaxValue write SetMaxValue;
    property DefaultValue: T read GetDefaultValue write SetDefaultValue;
    property Format: string read fFormat write fFormat;
  end;

  [ASchemaType(skInt64)]
  TJsonFieldInt64 = class(TJsonFieldNumber<Int64>)
  public
    function ToJsonSchema: TJsonObject; override;
  end;

  [ASchemaType(skInteger)]
  TJsonFieldInteger = class(TJsonFieldNumber<Integer>);

  [ASchemaType(skNumber)]
  TJsonFieldNumber = class(TJsonFieldNumber<Double>);

implementation

uses
  System.Classes,
  Json.Common.Helpers;

{ TJsonFieldNumber<T> }

function TJsonFieldNumber<T>.Clone: TJsonField;
begin
  Result := inherited Clone;
  TJsonFieldNumber<T>(Result).fMinValue := Self.fMinValue;
  TJsonFieldNumber<T>(Result).fMaxValue := Self.fMaxValue;
  TJsonFieldNumber<T>(Result).fDefaultValue := Self.fDefaultValue;
end;

function TJsonFieldNumber<T>.GetDefaultValue: T;
begin
  Result := fDefaultValue.AsType<T>;
end;

function TJsonFieldNumber<T>.GetMaxValue: T;
begin
  Result := fMaxValue.AsType<T>;
end;

function TJsonFieldNumber<T>.GetMinValue: T;
begin
  Result := fMinValue.AsType<T>;
end;

procedure TJsonFieldNumber<T>.SetDefaultValue(const Value: T);
begin
  fDefaultValue := TValue.From<T>(Value);
end;

procedure TJsonFieldNumber<T>.SetMaxValue(const Value: T);
begin
  fMaxValue := TValue.From<T>(Value);
end;

procedure TJsonFieldNumber<T>.SetMinValue(const Value: T);
begin
  fMinValue := TValue.From<T>(Value);
end;

function TJsonFieldNumber<T>.ToJsonSchema: TJsonObject;
begin
  Result := inherited ToJsonSchema;
  if not fMinValue.IsEmpty then
    Result.AddPair('minimum', fMinValue.AsExtended);
  if not fMaxValue.IsEmpty then
    Result.AddPair('maximum', fMaxValue.AsExtended);
  if not fDefaultValue.IsEmpty then
    Result.AddPair('default', fDefaultValue.AsExtended);
end;

{ TJsonFieldInt64 }

function TJsonFieldInt64.ToJsonSchema: TJsonObject;
begin
  Result := inherited ToJsonSchema;
  Result.AddPair('format', 'int64');
end;

initialization
  RegisterClass(TJsonFieldInt64);
  RegisterClass(TJsonFieldInteger);
  RegisterClass(TJsonFieldNumber);

end.
