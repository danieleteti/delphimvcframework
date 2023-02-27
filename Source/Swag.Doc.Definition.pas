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

unit Swag.Doc.Definition;

interface

uses
  Json.Schema,
  System.JSON;

type
  /// <summary>
  /// The Schema Object allows the definition of input and output data types.
  /// These types can be objects, but also primitives and arrays.
  /// This object is based on the JSON Schema Specification Draft 4 and uses a predefined subset of it.
  /// On top of this subset, there are extensions provided by this specification to allow for more complete documentation.
  /// Further information about the properties can be found in JSON Schema Core and JSON Schema Validation.
  /// Unless stated otherwise, the property definitions follow the JSON Schema specification as referenced here.
  /// </summary>
  TSwagDefinition = class(TObject)
  private
    fName: string;
    fJsonSchema: TJsonObject;
    procedure SetName(const pValue: string);
    procedure SetJsonSchema(const pValue: TJsonObject);
    function GetJsonSchema: TJsonObject;
  public
    destructor Destroy; override;

    function GenerateJsonRefDefinition: TJsonObject;
    /// <summary>
    /// The schema name alias.
    /// </summary>
    property Name: string read fName write SetName;

    /// <summary>
    /// See more in:
    ///  * http://json-schema.org
    ///  * https://github.com/OAI/OpenAPI-Specification/blob/master/versions/2.0.md#schemaObject
    /// </summary>
    property JsonSchema: TJsonObject read GetJsonSchema write SetJsonSchema;
  end;

implementation

uses
  System.SysUtils;

{ TSwagDefinition }

destructor TSwagDefinition.Destroy;
begin
  if Assigned(fJsonSchema) then
    FreeAndNil(fJsonSchema);
  inherited Destroy;
end;

function TSwagDefinition.GetJsonSchema: TJsonObject;
begin
  Result := fJsonSchema;
end;

procedure TSwagDefinition.SetJsonSchema(const pValue: TJsonObject);
begin
  fJsonSchema := pValue;
  fJsonSchema.Owned := False;
end;

procedure TSwagDefinition.SetName(const pValue: string);
begin
  fName := pValue;
end;

function TSwagDefinition.GenerateJsonRefDefinition: TJsonObject;
const
  c_SchemaRef = '$ref';
  c_PrefixDefinitionName = '#/definitions/';
begin
  Result := TJsonObject.Create;
  Result.AddPair(c_SchemaRef, c_PrefixDefinitionName + fName);
end;

end.
