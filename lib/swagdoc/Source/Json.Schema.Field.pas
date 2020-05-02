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

unit Json.Schema.Field;

interface

uses
  System.Rtti,
  System.Classes,
  System.Json;

type
  TJsonFieldClass = class of TJsonField;

  TJsonField = class abstract(TPersistent)
  strict private
    fTypeName: string;
  strict protected
    fName: string;
    fDescription: string;
    fRequired: Boolean;
    fNullable: Boolean;

    function GetTypeName: string; virtual;

    property TypeName: string read GetTypeName;
  public
    constructor Create; reintroduce; virtual;

    function ToJsonSchema: TJsonObject; virtual;
    function Clone: TJsonField; virtual;

    property Name: string read fName write fName;
    property Description: string read fDescription write fDescription;
    property Required: Boolean read fRequired write fRequired;
    property Nullable: Boolean read fNullable write fNullable;
  end;

implementation

uses
  System.SysUtils,
  Json.Schema.Common.Types;

{ TJsonField }

function TJsonField.Clone: TJsonField;
begin
  Result := TJsonField(TJsonFieldClass(FindClass(Self.ClassName)).Create);
  Result.Name := Self.fName;
  Result.Description := Self.fDescription;
  Result.Required := Self.fRequired;
  Result.Nullable := Self.fNullable;
end;

constructor TJsonField.Create;
begin
  inherited Create;
end;

function TJsonField.GetTypeName: string;
var
  vContext: TRttiContext;
  vType: TRttiType;
  vAttribute: TCustomAttribute;
begin
  if not fTypeName.IsEmpty then
  begin
    Result := fTypeName;
    Exit;
  end;

  vContext := TRttiContext.Create;
  vType := vContext.GetType(Self.ClassType);
  for vAttribute in vType.GetAttributes do
    if vAttribute is ASchemaType then
    begin
      fTypeName := ASchemaType(vAttribute).Name;
      Break;
    end;

  Result := fTypeName;
end;

function TJsonField.ToJsonSchema: TJsonObject;
begin
  Result := TJsonObject.Create;
  Result.AddPair('type', GetTypeName);
  if not fDescription.IsEmpty then
    Result.AddPair('description', fDescription);
end;


end.
