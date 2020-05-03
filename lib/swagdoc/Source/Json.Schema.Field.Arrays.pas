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

unit Json.Schema.Field.Arrays;

interface

uses
  System.Json,
  Json.Schema.Field,
  Json.Schema.Common.Types;

type
  [ASchemaType(skArray)]
  TJsonFieldArray = class(TJsonField)
  strict private
    fItemFieldType: TJsonField;
    fMinLength: Integer;
    fMaxLength: Integer;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Clone: TJsonField; override;
    function ToJsonSchema: TJsonObject; override;

    property ItemFieldType: TJsonField read fItemFieldType write fItemFieldType;
    property MinLength: Integer read fMinLength write fMinLength;
    property MaxLength: Integer read fMaxLength write fMaxLength;
  end;

implementation

uses
  System.Classes,
  Json.Common.Helpers;

{ TJsonFieldArray }

function TJsonFieldArray.Clone: TJsonField;
begin
  Result := inherited Clone;
  TJsonFieldArray(Result).MinLength := Self.MinLength;
  TJsonFieldArray(Result).MaxLength := Self.MaxLength;
  TJsonFieldArray(Result).ItemFieldType := Self.ItemFieldType.Clone;
end;

constructor TJsonFieldArray.Create;
begin
  inherited Create;
  fItemFieldType := nil;
  fMinLength := 0;
  fMaxLength := 0;
end;

destructor TJsonFieldArray.Destroy;
begin
  if Assigned(fItemFieldType) then
    fItemFieldType.Free;
  inherited Destroy;
end;

function TJsonFieldArray.ToJsonSchema: TJsonObject;
begin
  Result := inherited ToJsonSchema;
  if Assigned(fItemFieldType) then
    Result.AddPair('items', fItemFieldType.ToJsonSchema);
  if (fMinLength > 0) then
    Result.AddPair('minLength', fMinLength);
  if (fMaxLength > 0) then
    Result.AddPair('maxLength', fMaxLength);
end;

initialization
  RegisterClass(TJsonFieldArray);

end.
