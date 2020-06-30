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

unit Swag.Doc.Path.Operation.Response;

interface

uses
  System.Generics.Collections,
  System.JSON,
  Swag.Common.Types,
  Swag.Doc.Path.Operation.ResponseHeaders,
  Swag.Doc.Definition;

type
  /// <summary>
  /// Describes a single response from an API Operation.
  /// A container for the expected responses of an operation.
  /// The container maps a HTTP response code to the expected response.
  /// It is not expected from the documentation to necessarily cover all possible HTTP response codes, since they may not be
  /// known in advance. However, it is expected from the documentation to cover a successful operation response and any known errors.
  /// The default can be used as the default response object for all HTTP codes that are not covered individually by the specification.
  /// The Responses Object MUST contain at least one response code, and it SHOULD be the response for a successful operation call.
  /// </summary>
  TSwagResponse = class(TObject)
  private
    fStatusCode: TSwagStatusCode;
    fSchema: TSwagDefinition;
    fHeaders: TObjectList<TSwagHeaders>;
    fDescription: string;
    fExamples: TObjectDictionary<TSwagJsonExampleDescription, TJSONObject>;
  protected
    function GenerateExamplesJsonObject: TJSONObject;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    function GenerateJsonObject: TJSONObject;
    procedure Load(pJson : TJSONObject);

    /// <summary>
    /// Any HTTP status code can be used as the property name (one property per HTTP status code).
    /// Describes the expected response for that HTTP status code.
    /// Reference Object can be used to link to a response that is defined at the Swagger Object's responses section.
    /// </summary>
    property StatusCode: TSwagStatusCode read fStatusCode write fStatusCode;

    /// <summary>
    /// Required. A short description of the response. GFM syntax can be used for rich text representation.
    /// </summary>
    property Description: string read fDescription write fDescription;

    /// <summary>
    /// A definition of the response structure.
    /// It can be a primitive, an array or an object.
    /// If this field does not exist, it means no content is returned as part of the response.
    /// As an extension to the Schema Object, its root type value may also be "file".
    /// This SHOULD be accompanied by a relevant produces mime-type.
    /// </summary>
    property Schema: TSwagDefinition read fSchema;

    /// <summary>
    /// A list of headers that are sent with the response.
    /// </summary>
    property Headers : TObjectList<TSwagHeaders> read fHeaders;

    /// <summary>
    /// An example list of the json response message.
    /// </summary>
    property Examples: TObjectDictionary<TSwagJsonExampleDescription, TJSONObject> read fExamples;
  end;

implementation

uses
  System.SysUtils;

const
  c_SwagResponseDescription = 'description';
  c_SwagResponseSchema = 'schema';
  c_SwagResponseExamples = 'examples';
  c_SwagResponseHeaders = 'headers';

{ TSwagResponse }

constructor TSwagResponse.Create;
begin
  inherited Create;
  fExamples := TObjectDictionary<TSwagJsonExampleDescription, TJSONObject>.Create([doOwnsValues]);
  fSchema := TSwagDefinition.Create;
  fHeaders := TObjectList<TSwagHeaders>.Create;
end;

destructor TSwagResponse.Destroy;
begin
  FreeAndNil(fExamples);
  FreeAndNil(fSchema);
  FreeAndNil(fHeaders);
  inherited Destroy;
end;

function TSwagResponse.GenerateExamplesJsonObject: TJSONObject;
var
  vKey: TSwagJsonExampleDescription;
  vExampleNumber: Integer;
begin
  Result := TJsonObject.Create;
  vExampleNumber := 0;
  for vKey in fExamples.Keys do
  begin
    Inc(vExampleNumber);
    Result.AddPair(vExampleNumber.ToString, TJsonObject(fExamples.Items[vKey].Clone));
  end;
end;

function TSwagResponse.GenerateJsonObject: TJSONObject;
var
  vJsonObject: TJsonObject;
  vIndex: Integer;
  vJsonHeaders: TJSONObject;
begin
  vJsonObject := TJsonObject.Create;
  vJsonObject.AddPair(c_SwagResponseDescription, fDescription);

  if (not fSchema.Name.IsEmpty) then
    vJsonObject.AddPair(c_SwagResponseSchema, fSchema.GenerateJsonRefDefinition)
  else if Assigned(fSchema.JsonSchema) then
    vJsonObject.AddPair(c_SwagResponseSchema, fSchema.JsonSchema.Clone as TJSONObject);

  if (fExamples.Count > 0) then
    vJsonObject.AddPair(c_SwagResponseExamples, GenerateExamplesJsonObject);

  if fHeaders.Count > 0 then
  begin
    vJsonHeaders := TJSONObject.Create;
    for vIndex := 0 to fHeaders.Count - 1 do
    begin
      vJsonHeaders.AddPair(fHeaders[vIndex].Name, fHeaders[vIndex].GenerateJsonObject);
    end;
    vJsonObject.AddPair(c_SwagResponseHeaders, vJsonHeaders);
  end;

  Result := vJsonObject;
end;

procedure TSwagResponse.Load(pJson: TJSONObject);
var
  vJSONHeaders: TJSONObject;
  vIndex: Integer;
  vHeader: TSwagHeaders;
begin
  if not Assigned(pJson) then
    Exit;
  if Assigned(pJson.Values[c_SwagResponseDescription]) then
    fDescription := pJson.Values[c_SwagResponseDescription].Value;

  if Assigned(pJson.Values[c_SwagResponseHeaders]) then
  begin
    vJSONHeaders := pJson.Values[c_SwagResponseHeaders] as TJSONObject;
    for vIndex := 0 to vJSONHeaders.Count - 1 do
    begin
      vHeader := TSwagHeaders.Create;
      vHeader.Load(vJSONHeaders.Pairs[vIndex].JsonValue as TJSONObject);
      vHeader.Name := vJSONHeaders.Pairs[vIndex].JsonString.Value;
      fHeaders.Add(vheader);
    end;
  end;

  if Assigned(pJson.Values[c_SwagResponseSchema]) then
    fSchema.JsonSchema := pJson.Values[c_SwagResponseSchema].Clone as TJSONObject;
end;

end.
