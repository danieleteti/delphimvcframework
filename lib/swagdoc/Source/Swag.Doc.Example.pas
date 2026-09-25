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

unit Swag.Doc.Example;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.JSON,
  Swag.Doc.Extensions;

type
  /// <summary>
  /// An object grouping an internal or external example value with basic summary and description metadata.
  /// It is used by the examples of media types, parameters and headers and by the reusable examples of the
  /// components. Available in OpenAPI 3 only.
  /// </summary>
  TSwagExample = class(TObject)
  private
    fName: string;
    fRef: string;
    fSummary: string;
    fDescription: string;
    fValue: TJSONValue;
    fDataValue: TJSONValue;
    fSerializedValue: string;
    fExternalValue: string;
    fExtensions: TSwagExtensions;
    procedure SetValue(const pValue: TJSONValue);
    procedure SetDataValue(const pValue: TJSONValue);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    function GenerateJsonObject: TJSONObject;
    procedure Load(pJson: TJSONObject);

    /// <summary>
    /// Generates the map of examples, using the Name of each example as the key.
    /// </summary>
    class function GenerateMapJsonObject(pExamples: TObjectList<TSwagExample>): TJSONObject;

    /// <summary>
    /// Loads a map of examples into the list, using the key of each item as the Name of the example.
    /// </summary>
    class procedure LoadMap(pJson: TJSONObject; pExamples: TObjectList<TSwagExample>);

    /// <summary>
    /// The key of the example in the map that contains it.
    /// </summary>
    property Name: string read fName write fName;

    /// <summary>
    /// A reference to a reusable example, for example #/components/examples/employee. When defined, only the
    /// reference, the Summary and the Description are written.
    /// </summary>
    property Ref: string read fRef write fRef;

    /// <summary>
    /// Short description for the example.
    /// </summary>
    property Summary: string read fSummary write fSummary;

    /// <summary>
    /// Long description for the example. CommonMark syntax MAY be used for rich text representation.
    /// </summary>
    property Description: string read fDescription write fDescription;

    /// <summary>
    /// Embedded literal example. The Value and ExternalValue fields are mutually exclusive.
    /// The example takes ownership of the assigned value.
    /// </summary>
    property Value: TJSONValue read fValue write SetValue;

    /// <summary>
    /// An example of the data structure that MUST be valid according to the relevant schema. If this field is
    /// present, Value MUST be absent. The example takes ownership of the assigned value.
    /// </summary>
    property DataValue: TJSONValue read fDataValue write SetDataValue;

    /// <summary>
    /// An example of the serialized form of the value, including encoding and escaping.
    /// </summary>
    property SerializedValue: string read fSerializedValue write fSerializedValue;

    /// <summary>
    /// A URI that identifies the serialized example in a separate document.
    /// </summary>
    property ExternalValue: string read fExternalValue write fExternalValue;

    /// <summary>
    /// The Specification Extensions of the example.
    /// </summary>
    property Extensions: TSwagExtensions read fExtensions;
  end;

implementation

uses
  Swag.Common.Json;

const
  c_SwagExampleRef = '$ref';
  c_SwagExampleSummary = 'summary';
  c_SwagExampleDescription = 'description';
  c_SwagExampleValue = 'value';
  c_SwagExampleDataValue = 'dataValue';
  c_SwagExampleSerializedValue = 'serializedValue';
  c_SwagExampleExternalValue = 'externalValue';

{ TSwagExample }

constructor TSwagExample.Create;
begin
  inherited Create;
  fExtensions := TSwagExtensions.Create;
end;

destructor TSwagExample.Destroy;
begin
  FreeAndNil(fValue);
  FreeAndNil(fDataValue);
  FreeAndNil(fExtensions);
  inherited Destroy;
end;

procedure TSwagExample.SetValue(const pValue: TJSONValue);
begin
  if fValue = pValue then
    Exit;
  fValue.Free;
  fValue := pValue;
end;

procedure TSwagExample.SetDataValue(const pValue: TJSONValue);
begin
  if fDataValue = pValue then
    Exit;
  fDataValue.Free;
  fDataValue := pValue;
end;

function TSwagExample.GenerateJsonObject: TJSONObject;
begin
  Result := TJSONObject.Create;
  if not fRef.IsEmpty then
  begin
    Result.AddPair(c_SwagExampleRef, fRef);
    if not fSummary.IsEmpty then
      Result.AddPair(c_SwagExampleSummary, fSummary);
    if not fDescription.IsEmpty then
      Result.AddPair(c_SwagExampleDescription, fDescription);
    Exit;
  end;

  if not fSummary.IsEmpty then
    Result.AddPair(c_SwagExampleSummary, fSummary);
  if not fDescription.IsEmpty then
    Result.AddPair(c_SwagExampleDescription, fDescription);
  if Assigned(fDataValue) then
    Result.AddPair(c_SwagExampleDataValue, fDataValue.Clone as TJSONValue);
  if not fSerializedValue.IsEmpty then
    Result.AddPair(c_SwagExampleSerializedValue, fSerializedValue);
  if not fExternalValue.IsEmpty then
    Result.AddPair(c_SwagExampleExternalValue, fExternalValue);
  if Assigned(fValue) then
    Result.AddPair(c_SwagExampleValue, fValue.Clone as TJSONValue);
  fExtensions.WriteTo(Result);
end;

procedure TSwagExample.Load(pJson: TJSONObject);
begin
  if not Assigned(pJson) then
    Exit;

  fRef := TSwagJson.ReadString(pJson, c_SwagExampleRef);
  fSummary := TSwagJson.ReadString(pJson, c_SwagExampleSummary);
  fDescription := TSwagJson.ReadString(pJson, c_SwagExampleDescription);
  if not fRef.IsEmpty then
    Exit;

  SetDataValue(TSwagJson.CloneValue(pJson, c_SwagExampleDataValue));
  fSerializedValue := TSwagJson.ReadString(pJson, c_SwagExampleSerializedValue);
  fExternalValue := TSwagJson.ReadString(pJson, c_SwagExampleExternalValue);
  SetValue(TSwagJson.CloneValue(pJson, c_SwagExampleValue));
  fExtensions.ReadFrom(pJson);
end;

class function TSwagExample.GenerateMapJsonObject(pExamples: TObjectList<TSwagExample>): TJSONObject;
var
  vExample: TSwagExample;
begin
  Result := TJSONObject.Create;
  for vExample in pExamples do
    Result.AddPair(vExample.Name, vExample.GenerateJsonObject);
end;

class procedure TSwagExample.LoadMap(pJson: TJSONObject; pExamples: TObjectList<TSwagExample>);
var
  vIndex: Integer;
  vExample: TSwagExample;
begin
  if not Assigned(pJson) then
    Exit;

  for vIndex := 0 to pJson.Count - 1 do
  begin
    if not (pJson.Pairs[vIndex].JsonValue is TJSONObject) then
      Continue;

    vExample := TSwagExample.Create;
    vExample.Name := pJson.Pairs[vIndex].JsonString.Value;
    vExample.Load(TJSONObject(pJson.Pairs[vIndex].JsonValue));
    pExamples.Add(vExample);
  end;
end;

end.
