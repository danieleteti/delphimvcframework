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
  Swag.Doc.Extensions,
  Swag.Doc.Link,
  Swag.Doc.Path.Operation.Content,
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
    fName: string;
    fStatusCode: TSwagStatusCode;
    fSchema: TSwagDefinition;
    fHeaders: TObjectList<TSwagHeaders>;
    fSummary: string;
    fDescription: string;
    fExamples: TObjectDictionary<TSwagJsonExampleDescription, TJSONObject>;
    fRef: string;
    fContent: TObjectList<TSwagMediaType>;
    fLinks: TObjectList<TSwagLink>;
    fExtensions: TSwagExtensions;
  protected
    function GenerateExamplesJsonObject: TJSONObject;
    function GenerateContentExamplesJsonObject: TJSONObject;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    function GenerateJsonObject: TJSONObject;
    procedure Load(pJson : TJSONObject);

    /// <summary>
    /// Adds a media type to the content of the response and returns it, so the schema can be defined.
    /// The content is used by OpenAPI 3 documents. When it is empty, the Schema property is written for every
    /// media type produced by the operation.
    /// </summary>
    function AddMediaType(const pMediaType: TSwagMimeType): TSwagMediaType;

    /// <summary>
    /// Adds a header to the response and returns it.
    /// </summary>
    function AddHeader(const pName: string): TSwagHeaders;

    /// <summary>
    /// Adds a link to the response and returns it. Available in OpenAPI 3 only.
    /// </summary>
    function AddLink(const pName: string): TSwagLink;

    /// <summary>
    /// The name used as the key of the response when it is placed under the reusable responses of the document.
    /// </summary>
    property Name: string read fName write fName;

    /// <summary>
    /// Any HTTP status code can be used as the property name (one property per HTTP status code).
    /// Describes the expected response for that HTTP status code.
    /// Reference Object can be used to link to a response that is defined at the Swagger Object's responses section.
    /// </summary>
    property StatusCode: TSwagStatusCode read fStatusCode write fStatusCode;

    /// <summary>
    /// A short summary of the meaning of the response. Available in OpenAPI 3 only.
    /// </summary>
    property Summary: string read fSummary write fSummary;

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

    /// <summary>
    /// A reference to a reusable response, for example #/responses/notFound in Swagger 2.0 or
    /// #/components/responses/notFound in OpenAPI 3. When defined, only the reference and, in OpenAPI 3, the
    /// Summary and Description that override the referenced ones are written.
    /// </summary>
    property Ref: string read fRef write fRef;

    /// <summary>
    /// A map containing descriptions of potential response payloads. Each item maps a media type or media type range
    /// to its schema. Available in OpenAPI 3. In a Swagger 2.0 document the schema of the first media type is
    /// written when the Schema property is empty.
    /// </summary>
    property Content: TObjectList<TSwagMediaType> read fContent;

    /// <summary>
    /// The operations links that can be followed from the response. Available in OpenAPI 3 only.
    /// </summary>
    property Links: TObjectList<TSwagLink> read fLinks;

    /// <summary>
    /// The Specification Extensions of the response.
    /// </summary>
    property Extensions: TSwagExtensions read fExtensions;
  end;

implementation

uses
  System.SysUtils;

const
  c_SwagResponseDescription = 'description';
  c_SwagResponseSchema = 'schema';
  c_SwagResponseExamples = 'examples';
  c_SwagResponseHeaders = 'headers';
  c_SwagResponseRef = '$ref';

{ TSwagResponse }

constructor TSwagResponse.Create;
begin
  inherited Create;
  fExamples := TObjectDictionary<TSwagJsonExampleDescription, TJSONObject>.Create([doOwnsValues]);
  fSchema := TSwagDefinition.Create;
  fHeaders := TObjectList<TSwagHeaders>.Create;
  fContent := TObjectList<TSwagMediaType>.Create;
  fLinks := TObjectList<TSwagLink>.Create;
  fExtensions := TSwagExtensions.Create;
end;

destructor TSwagResponse.Destroy;
begin
  FreeAndNil(fExamples);
  FreeAndNil(fSchema);
  FreeAndNil(fHeaders);
  FreeAndNil(fContent);
  FreeAndNil(fLinks);
  FreeAndNil(fExtensions);
  inherited Destroy;
end;

function TSwagResponse.AddMediaType(const pMediaType: TSwagMimeType): TSwagMediaType;
begin
  Result := TSwagMediaType.Create;
  Result.MediaType := pMediaType;
  fContent.Add(Result);
end;

function TSwagResponse.AddHeader(const pName: string): TSwagHeaders;
begin
  Result := TSwagHeaders.Create;
  Result.Name := pName;
  fHeaders.Add(Result);
end;

function TSwagResponse.AddLink(const pName: string): TSwagLink;
begin
  Result := TSwagLink.Create;
  Result.Name := pName;
  fLinks.Add(Result);
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

function TSwagResponse.GenerateContentExamplesJsonObject: TJSONObject;
var
  vMediaType: TSwagMediaType;
  vExample: TJSONValue;
begin
  Result := nil;
  for vMediaType in fContent do
  begin
    vExample := vMediaType.Example;
    if (not Assigned(vExample)) and (vMediaType.Examples.Count > 0) then
    begin
      vExample := vMediaType.Examples[0].Value;
      if not Assigned(vExample) then
        vExample := vMediaType.Examples[0].DataValue;
    end;
    if not Assigned(vExample) then
      Continue;

    if not Assigned(Result) then
      Result := TJSONObject.Create;
    Result.AddPair(vMediaType.MediaType, vExample.Clone as TJSONValue);
  end;
end;

function TSwagResponse.GenerateJsonObject: TJSONObject;
var
  vJsonObject: TJsonObject;
  vJsonHeaders: TJSONObject;
  vJsonExamples: TJSONObject;
  vSchema: TSwagDefinition;
begin
  vJsonObject := TJsonObject.Create;

  if not fRef.IsEmpty then
  begin
    vJsonObject.AddPair(c_SwagResponseRef, fRef);
    Result := vJsonObject;
    Exit;
  end;

  vJsonObject.AddPair(c_SwagResponseDescription, fDescription);

  vSchema := fSchema;
  if vSchema.IsEmpty and (fContent.Count > 0) then
    vSchema := fContent[0].Schema;

  if (not vSchema.Name.IsEmpty) then
    vJsonObject.AddPair(c_SwagResponseSchema, vSchema.GenerateJsonRefDefinition)
  else if Assigned(vSchema.JsonSchema) then
    vJsonObject.AddPair(c_SwagResponseSchema, vSchema.JsonSchema.Clone as TJSONObject);

  if (fExamples.Count > 0) then
    vJsonObject.AddPair(c_SwagResponseExamples, GenerateExamplesJsonObject)
  else if fContent.Count > 0 then
  begin
    vJsonExamples := GenerateContentExamplesJsonObject;
    if Assigned(vJsonExamples) then
      vJsonObject.AddPair(c_SwagResponseExamples, vJsonExamples);
  end;

  if fHeaders.Count > 0 then
  begin
    vJsonHeaders := TSwagHeaders.GenerateMapJsonObject(fHeaders, svSwagger2);
    if vJsonHeaders.Count > 0 then
      vJsonObject.AddPair(c_SwagResponseHeaders, vJsonHeaders)
    else
      vJsonHeaders.Free;
  end;

  fExtensions.WriteTo(vJsonObject);
  Result := vJsonObject;
end;

procedure TSwagResponse.Load(pJson: TJSONObject);
var
  vJSONHeaders: TJSONObject;
  vJsonExamples: TJSONObject;
  vIndex: Integer;
  vHeader: TSwagHeaders;
begin
  if not Assigned(pJson) then
    Exit;
  if Assigned(pJson.Values[c_SwagResponseRef]) then
    fRef := pJson.Values[c_SwagResponseRef].Value;
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

  if pJson.Values[c_SwagResponseExamples] is TJSONObject then
  begin
    vJsonExamples := TJSONObject(pJson.Values[c_SwagResponseExamples]);
    for vIndex := 0 to vJsonExamples.Count - 1 do
      if vJsonExamples.Pairs[vIndex].JsonValue is TJSONObject then
        fExamples.AddOrSetValue(vJsonExamples.Pairs[vIndex].JsonString.Value,
          vJsonExamples.Pairs[vIndex].JsonValue.Clone as TJSONObject);
  end;

  fExtensions.ReadFrom(pJson);
end;

end.
