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

unit Swag.Doc.Path.Operation.Content;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.JSON,
  Swag.Common.Types,
  Swag.Doc.Definition,
  Swag.Doc.Example,
  Swag.Doc.Extensions;

type
  TSwagMediaType = class;
  TSwagEncoding = class;

  /// <summary>
  /// Describes a single header sent with a response or with a part of a multipart content. The Name, Description,
  /// ValueType and Format properties are written in Swagger 2.0 and OpenAPI 3 documents; the other properties
  /// follow the Header Object of OpenAPI 3 and are written in OpenAPI 3 documents only.
  /// </summary>
  TSwagHeaders = class(TObject)
  private
    fName: string;
    fDescription: string;
    fType: string;
    fFormat: string;
    fRef: string;
    fRequired: Boolean;
    fDeprecated: Boolean;
    fExample: TJSONValue;
    fExamples: TObjectList<TSwagExample>;
    fStyle: TSwagRequestParameterStyle;
    fExplode: Boolean;
    fSchema: TSwagDefinition;
    fContent: TObjectList<TSwagMediaType>;
    fExtensions: TSwagExtensions;
    procedure SetExample(const pValue: TJSONValue);
    function ReturnValueType: string;
    function ReturnFormat: string;
    function GenerateSchemaJsonObject: TJSONObject;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    /// <summary>
    /// Generates the Swagger 2.0 header object.
    /// </summary>
    function GenerateJsonObject: TJSONObject; overload;

    /// <summary>
    /// Generates the header object for the given specification family.
    /// </summary>
    function GenerateJsonObject(const pVersion: TSwagVersion): TJSONObject; overload;

    /// <summary>
    /// Loads the Swagger 2.0 header object.
    /// </summary>
    procedure Load(pJson : TJSONObject); overload;

    /// <summary>
    /// Loads the header object written for the given specification family.
    /// </summary>
    procedure Load(pJson: TJSONObject; const pVersion: TSwagVersion); overload;

    /// <summary>
    /// Adds a media type to the content of the header and returns it. The content MUST only contain one entry.
    /// </summary>
    function AddMediaType(const pMediaType: TSwagMimeType): TSwagMediaType;

    /// <summary>
    /// Adds an example of the header value and returns it.
    /// </summary>
    function AddExample(const pName: string): TSwagExample;

    /// <summary>
    /// Generates the map of headers, using the Name of each header as the key. In Swagger 2.0 the headers that
    /// are references are not written, because references are not allowed there.
    /// </summary>
    class function GenerateMapJsonObject(pHeaders: TObjectList<TSwagHeaders>; const pVersion: TSwagVersion): TJSONObject;

    /// <summary>
    /// Loads a map of headers into the list, using the key of each item as the Name of the header.
    /// </summary>
    class procedure LoadMap(pJson: TJSONObject; pHeaders: TObjectList<TSwagHeaders>; const pVersion: TSwagVersion);

    /// <summary>
    /// A header name alias.
    /// </summary>
    property Name: string read fName write fName;

    /// <summary>
    /// A short description of the header.
    /// </summary>
    property Description: string read fDescription write fDescription;

    /// <summary>
    /// Required. The type of the object. The value MUST be one of "string", "number", "integer", "boolean", or "array".
    /// </summary>
    property ValueType: string read fType write fType;

    property Format: string read fFormat write fFormat;

    /// <summary>
    /// A reference to a reusable header, for example #/components/headers/X-Rate-Limit. When defined, only the
    /// reference and the Description are written. Available in OpenAPI 3 only.
    /// </summary>
    property Ref: string read fRef write fRef;

    /// <summary>
    /// Determines whether this header is mandatory. The default value is false. Available in OpenAPI 3 only.
    /// </summary>
    property Required: Boolean read fRequired write fRequired;

    /// <summary>
    /// Specifies that the header is deprecated and SHOULD be transitioned out of usage. Available in OpenAPI 3 only.
    /// </summary>
    property Deprecated: Boolean read fDeprecated write fDeprecated;

    /// <summary>
    /// Example of the header's potential value. The header takes ownership of the assigned value.
    /// Available in OpenAPI 3 only.
    /// </summary>
    property Example: TJSONValue read fExample write SetExample;

    /// <summary>
    /// Examples of the header's potential value. Available in OpenAPI 3 only.
    /// </summary>
    property Examples: TObjectList<TSwagExample> read fExamples;

    /// <summary>
    /// Describes how the header value will be serialized. The only legal value for headers is simple.
    /// Available in OpenAPI 3 only.
    /// </summary>
    property Style: TSwagRequestParameterStyle read fStyle write fStyle;

    /// <summary>
    /// When this is true, header values of type array or object generate a single header whose value is a
    /// comma-separated list. It is written together with the Style property. Available in OpenAPI 3 only.
    /// </summary>
    property Explode: Boolean read fExplode write fExplode;

    /// <summary>
    /// The schema defining the type used for the header. When it is empty, ValueType and Format are used.
    /// Available in OpenAPI 3 only.
    /// </summary>
    property Schema: TSwagDefinition read fSchema;

    /// <summary>
    /// A map containing the representations for the header, used instead of the schema. Available in OpenAPI 3 only.
    /// </summary>
    property Content: TObjectList<TSwagMediaType> read fContent;

    /// <summary>
    /// The Specification Extensions of the header.
    /// </summary>
    property Extensions: TSwagExtensions read fExtensions;
  end;

  /// <summary>
  /// A single encoding definition applied to a single value of a multipart or application/x-www-form-urlencoded
  /// content, by property name or by position. Available in OpenAPI 3 only.
  /// </summary>
  TSwagEncoding = class(TObject)
  private
    fName: string;
    fContentType: string;
    fHeaders: TObjectList<TSwagHeaders>;
    fStyle: TSwagRequestParameterStyle;
    fExplode: Boolean;
    fAllowReserved: Boolean;
    fEncoding: TObjectList<TSwagEncoding>;
    fPrefixEncoding: TObjectList<TSwagEncoding>;
    fItemEncoding: TSwagEncoding;
    fExtensions: TSwagExtensions;
    procedure SetItemEncoding(const pValue: TSwagEncoding);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    function GenerateJsonObject: TJSONObject;
    procedure Load(pJson: TJSONObject);

    /// <summary>
    /// Adds a header of the encoded part and returns it.
    /// </summary>
    function AddHeader(const pName: string): TSwagHeaders;

    /// <summary>
    /// Adds a nested encoding for a property of the encoded value and returns it.
    /// </summary>
    function AddEncoding(const pPropertyName: string): TSwagEncoding;

    /// <summary>
    /// Adds a nested positional encoding and returns it.
    /// </summary>
    function AddPrefixEncoding: TSwagEncoding;

    /// <summary>
    /// Generates the map of encodings, using the Name of each encoding as the key.
    /// </summary>
    class function GenerateMapJsonObject(pEncodings: TObjectList<TSwagEncoding>): TJSONObject;

    /// <summary>
    /// Generates the array of positional encodings.
    /// </summary>
    class function GenerateJsonArray(pEncodings: TObjectList<TSwagEncoding>): TJSONArray;

    /// <summary>
    /// Loads a map of encodings into the list, using the key of each item as the Name of the encoding.
    /// </summary>
    class procedure LoadMap(pJson: TJSONObject; pEncodings: TObjectList<TSwagEncoding>);

    /// <summary>
    /// Loads an array of positional encodings into the list.
    /// </summary>
    class procedure LoadArray(pJson: TJSONArray; pEncodings: TObjectList<TSwagEncoding>);

    /// <summary>
    /// The name of the property the encoding applies to, used as its key in the encoding map.
    /// </summary>
    property Name: string read fName write fName;

    /// <summary>
    /// The Content-Type for encoding a specific property, as a comma-separated list of media types or media type ranges.
    /// </summary>
    property ContentType: string read fContentType write fContentType;

    /// <summary>
    /// Additional information to be provided as headers of a multipart part. Content-Type is described separately.
    /// </summary>
    property Headers: TObjectList<TSwagHeaders> read fHeaders;

    /// <summary>
    /// Describes how the property value will be serialized, following the style of a query parameter.
    /// </summary>
    property Style: TSwagRequestParameterStyle read fStyle write fStyle;

    /// <summary>
    /// When this is true, property values of type array or object generate separate parameters for each value.
    /// It is written together with the Style property.
    /// </summary>
    property Explode: Boolean read fExplode write fExplode;

    /// <summary>
    /// When this is true, the value is serialized using reserved expansion, as defined by RFC6570.
    /// </summary>
    property AllowReserved: Boolean read fAllowReserved write fAllowReserved;

    /// <summary>
    /// Nested encodings by property name.
    /// </summary>
    property Encoding: TObjectList<TSwagEncoding> read fEncoding;

    /// <summary>
    /// Nested encodings by position.
    /// </summary>
    property PrefixEncoding: TObjectList<TSwagEncoding> read fPrefixEncoding;

    /// <summary>
    /// A nested encoding applied to every array item. The encoding takes ownership of the assigned object.
    /// </summary>
    property ItemEncoding: TSwagEncoding read fItemEncoding write SetItemEncoding;

    /// <summary>
    /// The Specification Extensions of the encoding.
    /// </summary>
    property Extensions: TSwagExtensions read fExtensions;
  end;

  /// <summary>
  /// Each Media Type Object provides schema and examples for the media type identified by its key.
  /// It is used by the request bodies, responses, parameters and headers of OpenAPI 3, where the media types
  /// replace the consumes and produces lists of Swagger 2.0.
  /// </summary>
  TSwagMediaType = class(TObject)
  private
    fName: string;
    fRef: string;
    fMediaType: TSwagMimeType;
    fSchema: TSwagDefinition;
    fItemSchema: TSwagDefinition;
    fExample: TJSONValue;
    fExamples: TObjectList<TSwagExample>;
    fEncoding: TObjectList<TSwagEncoding>;
    fPrefixEncoding: TObjectList<TSwagEncoding>;
    fItemEncoding: TSwagEncoding;
    fExtensions: TSwagExtensions;
    procedure SetExample(const pValue: TJSONValue);
    procedure SetItemEncoding(const pValue: TSwagEncoding);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    function GenerateJsonObject: TJSONObject;
    procedure Load(pJson: TJSONObject);

    /// <summary>
    /// Adds an example of the media type and returns it.
    /// </summary>
    function AddExample(const pName: string): TSwagExample;

    /// <summary>
    /// Adds the encoding of a property of the content and returns it.
    /// </summary>
    function AddEncoding(const pPropertyName: string): TSwagEncoding;

    /// <summary>
    /// Adds a positional encoding of a multipart content and returns it.
    /// </summary>
    function AddPrefixEncoding: TSwagEncoding;

    /// <summary>
    /// Generates the map of media types. The key is the Name of each item when pKeyByName is True, as used by the
    /// reusable components, and the MediaType otherwise, as used by content maps.
    /// </summary>
    class function GenerateMapJsonObject(pMediaTypes: TObjectList<TSwagMediaType>; const pKeyByName: Boolean): TJSONObject;

    /// <summary>
    /// Loads a map of media types into the list. The key is stored in the Name of each item when pKeyByName is True
    /// and in the MediaType otherwise.
    /// </summary>
    class procedure LoadMap(pJson: TJSONObject; pMediaTypes: TObjectList<TSwagMediaType>; const pKeyByName: Boolean);

    /// <summary>
    /// The name used as the key of the media type when it is placed under the reusable components of the document.
    /// </summary>
    property Name: string read fName write fName;

    /// <summary>
    /// A reference to a reusable media type, for example #/components/mediaTypes/EmployeeJson. When defined, the
    /// other fields are not written.
    /// </summary>
    property Ref: string read fRef write fRef;

    /// <summary>
    /// The media type or media type range, for example application/json, text/plain or image/*.
    /// </summary>
    property MediaType: TSwagMimeType read fMediaType write fMediaType;

    /// <summary>
    /// The schema describing the complete content. Use the Name to reference a reusable schema or the JsonSchema
    /// for an inline schema.
    /// </summary>
    property Schema: TSwagDefinition read fSchema;

    /// <summary>
    /// A schema describing each item within a sequential media type, such as application/jsonl or text/event-stream.
    /// </summary>
    property ItemSchema: TSwagDefinition read fItemSchema;

    /// <summary>
    /// Example of the media type. The example object SHOULD be in the correct format as specified by the media type.
    /// The media type takes ownership of the assigned value.
    /// </summary>
    property Example: TJSONValue read fExample write SetExample;

    /// <summary>
    /// Examples of the media type. The Example and Examples fields are mutually exclusive.
    /// </summary>
    property Examples: TObjectList<TSwagExample> read fExamples;

    /// <summary>
    /// A map between a property name and its encoding information, used by multipart and
    /// application/x-www-form-urlencoded contents. It MUST NOT be present with PrefixEncoding or ItemEncoding.
    /// </summary>
    property Encoding: TObjectList<TSwagEncoding> read fEncoding;

    /// <summary>
    /// An array of positional encoding information, used by multipart contents.
    /// </summary>
    property PrefixEncoding: TObjectList<TSwagEncoding> read fPrefixEncoding;

    /// <summary>
    /// A single encoding applied to multiple array items of a multipart content. The media type takes ownership of
    /// the assigned object.
    /// </summary>
    property ItemEncoding: TSwagEncoding read fItemEncoding write SetItemEncoding;

    /// <summary>
    /// The Specification Extensions of the media type.
    /// </summary>
    property Extensions: TSwagExtensions read fExtensions;
  end;

implementation

uses
  Swag.Common.Consts,
  Swag.Common.Json,
  Swag.Common.Types.Helpers;

const
  c_SwagContentRef = '$ref';
  c_SwagContentDescription = 'description';
  c_SwagContentType = 'type';
  c_SwagContentFormat = 'format';
  c_SwagContentRequired = 'required';
  c_SwagContentDeprecated = 'deprecated';
  c_SwagContentExample = 'example';
  c_SwagContentExamples = 'examples';
  c_SwagContentStyle = 'style';
  c_SwagContentExplode = 'explode';
  c_SwagContentAllowReserved = 'allowReserved';
  c_SwagContentSchema = 'schema';
  c_SwagContentItemSchema = 'itemSchema';
  c_SwagContentContent = 'content';
  c_SwagContentContentType = 'contentType';
  c_SwagContentHeaders = 'headers';
  c_SwagContentEncoding = 'encoding';
  c_SwagContentPrefixEncoding = 'prefixEncoding';
  c_SwagContentItemEncoding = 'itemEncoding';
  c_SwagContentTypeNull = 'null';

function GenerateDefinitionJsonObject(pDefinition: TSwagDefinition): TJSONObject;
begin
  Result := nil;
  if not pDefinition.Name.IsEmpty then
    Result := pDefinition.GenerateJsonRefDefinition
  else if Assigned(pDefinition.JsonSchema) then
    Result := pDefinition.JsonSchema.Clone as TJSONObject;
end;

procedure LoadDefinition(pDefinition: TSwagDefinition; pJson: TJSONObject);
begin
  if Assigned(pJson) then
    pDefinition.JsonSchema := pJson.Clone as TJSONObject;
end;

{ TSwagHeaders }

constructor TSwagHeaders.Create;
begin
  inherited Create;
  fExamples := TObjectList<TSwagExample>.Create;
  fSchema := TSwagDefinition.Create;
  fContent := TObjectList<TSwagMediaType>.Create;
  fExtensions := TSwagExtensions.Create;
end;

destructor TSwagHeaders.Destroy;
begin
  FreeAndNil(fExample);
  FreeAndNil(fExamples);
  FreeAndNil(fSchema);
  FreeAndNil(fContent);
  FreeAndNil(fExtensions);
  inherited Destroy;
end;

procedure TSwagHeaders.SetExample(const pValue: TJSONValue);
begin
  if fExample = pValue then
    Exit;
  fExample.Free;
  fExample := pValue;
end;

function TSwagHeaders.AddMediaType(const pMediaType: TSwagMimeType): TSwagMediaType;
begin
  Result := TSwagMediaType.Create;
  Result.MediaType := pMediaType;
  fContent.Add(Result);
end;

function TSwagHeaders.AddExample(const pName: string): TSwagExample;
begin
  Result := TSwagExample.Create;
  Result.Name := pName;
  fExamples.Add(Result);
end;

function TSwagHeaders.ReturnValueType: string;
begin
  Result := fType;
  if Result.IsEmpty and Assigned(fSchema.JsonSchema) then
    Result := TSwagJson.ReadString(fSchema.JsonSchema, c_SwagContentType);
end;

function TSwagHeaders.ReturnFormat: string;
begin
  Result := fFormat;
  if Result.IsEmpty and fType.IsEmpty and Assigned(fSchema.JsonSchema) then
    Result := TSwagJson.ReadString(fSchema.JsonSchema, c_SwagContentFormat);
end;

function TSwagHeaders.GenerateJsonObject: TJSONObject;
var
  vJsonObject: TJsonObject;
  vType: string;
  vFormat: string;
begin
  vJsonObject := TJSONObject.Create;
  vType := ReturnValueType;
  vFormat := ReturnFormat;
  if fDescription.Length > 0 then
    vJsonObject.AddPair(c_SwagContentDescription, fDescription);
  if vType.Length > 0 then
    vJsonObject.AddPair(c_SwagContentType, vType);
  if vFormat.Length > 0 then
    vJsonObject.AddPair(c_SwagContentFormat, vFormat);
  fExtensions.WriteTo(vJsonObject);
  Result := vJsonObject;
end;

function TSwagHeaders.GenerateSchemaJsonObject: TJSONObject;
begin
  Result := GenerateDefinitionJsonObject(fSchema);
  if Assigned(Result) then
    Exit;

  Result := TJSONObject.Create;
  if not fType.IsEmpty then
    Result.AddPair(c_SwagContentType, fType);
  if not fFormat.IsEmpty then
    Result.AddPair(c_SwagContentFormat, fFormat);
end;

function TSwagHeaders.GenerateJsonObject(const pVersion: TSwagVersion): TJSONObject;
begin
  if pVersion <> svOpenApi3 then
    Exit(GenerateJsonObject);

  Result := TJSONObject.Create;
  if not fRef.IsEmpty then
  begin
    Result.AddPair(c_SwagContentRef, fRef);
    if not fDescription.IsEmpty then
      Result.AddPair(c_SwagContentDescription, fDescription);
    Exit;
  end;

  if not fDescription.IsEmpty then
    Result.AddPair(c_SwagContentDescription, fDescription);
  if fRequired then
    Result.AddPair(c_SwagContentRequired, TJSONBool.Create(True));
  if fDeprecated then
    Result.AddPair(c_SwagContentDeprecated, TJSONBool.Create(True));
  if Assigned(fExample) then
    Result.AddPair(c_SwagContentExample, fExample.Clone as TJSONValue);
  if fExamples.Count > 0 then
    Result.AddPair(c_SwagContentExamples, TSwagExample.GenerateMapJsonObject(fExamples));

  if fContent.Count > 0 then
    Result.AddPair(c_SwagContentContent, TSwagMediaType.GenerateMapJsonObject(fContent, False))
  else
  begin
    if fStyle <> rpsNotDefined then
    begin
      Result.AddPair(c_SwagContentStyle, c_SwagRequestParameterStyle[fStyle]);
      Result.AddPair(c_SwagContentExplode, TJSONBool.Create(fExplode));
    end;
    Result.AddPair(c_SwagContentSchema, GenerateSchemaJsonObject);
  end;

  fExtensions.WriteTo(Result);
end;

procedure TSwagHeaders.Load(pJson: TJSONObject);
begin
  if Assigned(pJson.Values[c_SwagContentDescription]) then
    fDescription := pJson.Values[c_SwagContentDescription].Value;
  if Assigned(pJson.Values[c_SwagContentType]) then
    fType := pJson.Values[c_SwagContentType].Value;
  if Assigned(pJson.Values[c_SwagContentFormat]) then
    fFormat := pJson.Values[c_SwagContentFormat].Value;
  fExtensions.ReadFrom(pJson);
end;

procedure TSwagHeaders.Load(pJson: TJSONObject; const pVersion: TSwagVersion);
var
  vJsonSchema: TJSONObject;
  vJsonTypes: TJSONArray;
  vIndex: Integer;
  vStyle: TSwagRequestParameterStyle;
begin
  if pVersion <> svOpenApi3 then
  begin
    Load(pJson);
    Exit;
  end;

  if not Assigned(pJson) then
    Exit;

  fRef := TSwagJson.ReadString(pJson, c_SwagContentRef);
  fDescription := TSwagJson.ReadString(pJson, c_SwagContentDescription);
  if not fRef.IsEmpty then
    Exit;

  fRequired := TSwagJson.ReadBoolean(pJson, c_SwagContentRequired);
  fDeprecated := TSwagJson.ReadBoolean(pJson, c_SwagContentDeprecated);
  SetExample(TSwagJson.CloneValue(pJson, c_SwagContentExample));
  TSwagExample.LoadMap(TSwagJson.ReadObject(pJson, c_SwagContentExamples), fExamples);
  vStyle.ToType(TSwagJson.ReadString(pJson, c_SwagContentStyle));
  fStyle := vStyle;
  fExplode := TSwagJson.ReadBoolean(pJson, c_SwagContentExplode);
  TSwagMediaType.LoadMap(TSwagJson.ReadObject(pJson, c_SwagContentContent), fContent, False);

  vJsonSchema := TSwagJson.ReadObject(pJson, c_SwagContentSchema);
  if Assigned(vJsonSchema) then
  begin
    LoadDefinition(fSchema, vJsonSchema);
    fType := TSwagJson.ReadString(vJsonSchema, c_SwagContentType);
    vJsonTypes := TSwagJson.ReadArray(vJsonSchema, c_SwagContentType);
    if Assigned(vJsonTypes) then
      for vIndex := 0 to vJsonTypes.Count - 1 do
        if vJsonTypes.Items[vIndex].Value <> c_SwagContentTypeNull then
        begin
          fType := vJsonTypes.Items[vIndex].Value;
          Break;
        end;
    fFormat := TSwagJson.ReadString(vJsonSchema, c_SwagContentFormat);
  end;

  fExtensions.ReadFrom(pJson);
end;

class function TSwagHeaders.GenerateMapJsonObject(pHeaders: TObjectList<TSwagHeaders>;
  const pVersion: TSwagVersion): TJSONObject;
var
  vHeader: TSwagHeaders;
begin
  Result := TJSONObject.Create;
  for vHeader in pHeaders do
    if (pVersion = svOpenApi3) or vHeader.Ref.IsEmpty then
      Result.AddPair(vHeader.Name, vHeader.GenerateJsonObject(pVersion));
end;

class procedure TSwagHeaders.LoadMap(pJson: TJSONObject; pHeaders: TObjectList<TSwagHeaders>;
  const pVersion: TSwagVersion);
var
  vIndex: Integer;
  vHeader: TSwagHeaders;
begin
  if not Assigned(pJson) then
    Exit;

  for vIndex := 0 to pJson.Count - 1 do
  begin
    if not (pJson.Pairs[vIndex].JsonValue is TJSONObject) then
      Continue;

    vHeader := TSwagHeaders.Create;
    vHeader.Name := pJson.Pairs[vIndex].JsonString.Value;
    vHeader.Load(TJSONObject(pJson.Pairs[vIndex].JsonValue), pVersion);
    pHeaders.Add(vHeader);
  end;
end;

{ TSwagEncoding }

constructor TSwagEncoding.Create;
begin
  inherited Create;
  fHeaders := TObjectList<TSwagHeaders>.Create;
  fEncoding := TObjectList<TSwagEncoding>.Create;
  fPrefixEncoding := TObjectList<TSwagEncoding>.Create;
  fExtensions := TSwagExtensions.Create;
end;

destructor TSwagEncoding.Destroy;
begin
  FreeAndNil(fHeaders);
  FreeAndNil(fEncoding);
  FreeAndNil(fPrefixEncoding);
  FreeAndNil(fItemEncoding);
  FreeAndNil(fExtensions);
  inherited Destroy;
end;

procedure TSwagEncoding.SetItemEncoding(const pValue: TSwagEncoding);
begin
  if fItemEncoding = pValue then
    Exit;
  fItemEncoding.Free;
  fItemEncoding := pValue;
end;

function TSwagEncoding.AddHeader(const pName: string): TSwagHeaders;
begin
  Result := TSwagHeaders.Create;
  Result.Name := pName;
  fHeaders.Add(Result);
end;

function TSwagEncoding.AddEncoding(const pPropertyName: string): TSwagEncoding;
begin
  Result := TSwagEncoding.Create;
  Result.Name := pPropertyName;
  fEncoding.Add(Result);
end;

function TSwagEncoding.AddPrefixEncoding: TSwagEncoding;
begin
  Result := TSwagEncoding.Create;
  fPrefixEncoding.Add(Result);
end;

function TSwagEncoding.GenerateJsonObject: TJSONObject;
begin
  Result := TJSONObject.Create;
  if not fContentType.IsEmpty then
    Result.AddPair(c_SwagContentContentType, fContentType);
  if fHeaders.Count > 0 then
    Result.AddPair(c_SwagContentHeaders, TSwagHeaders.GenerateMapJsonObject(fHeaders, svOpenApi3));
  if fStyle <> rpsNotDefined then
  begin
    Result.AddPair(c_SwagContentStyle, c_SwagRequestParameterStyle[fStyle]);
    Result.AddPair(c_SwagContentExplode, TJSONBool.Create(fExplode));
  end;
  if fAllowReserved then
    Result.AddPair(c_SwagContentAllowReserved, TJSONBool.Create(True));
  if fEncoding.Count > 0 then
    Result.AddPair(c_SwagContentEncoding, GenerateMapJsonObject(fEncoding));
  if fPrefixEncoding.Count > 0 then
    Result.AddPair(c_SwagContentPrefixEncoding, GenerateJsonArray(fPrefixEncoding));
  if Assigned(fItemEncoding) then
    Result.AddPair(c_SwagContentItemEncoding, fItemEncoding.GenerateJsonObject);
  fExtensions.WriteTo(Result);
end;

procedure TSwagEncoding.Load(pJson: TJSONObject);
var
  vStyle: TSwagRequestParameterStyle;
  vJsonItemEncoding: TJSONObject;
begin
  if not Assigned(pJson) then
    Exit;

  fContentType := TSwagJson.ReadString(pJson, c_SwagContentContentType);
  TSwagHeaders.LoadMap(TSwagJson.ReadObject(pJson, c_SwagContentHeaders), fHeaders, svOpenApi3);
  vStyle.ToType(TSwagJson.ReadString(pJson, c_SwagContentStyle));
  fStyle := vStyle;
  fExplode := TSwagJson.ReadBoolean(pJson, c_SwagContentExplode);
  fAllowReserved := TSwagJson.ReadBoolean(pJson, c_SwagContentAllowReserved);
  LoadMap(TSwagJson.ReadObject(pJson, c_SwagContentEncoding), fEncoding);
  LoadArray(TSwagJson.ReadArray(pJson, c_SwagContentPrefixEncoding), fPrefixEncoding);

  vJsonItemEncoding := TSwagJson.ReadObject(pJson, c_SwagContentItemEncoding);
  if Assigned(vJsonItemEncoding) then
  begin
    SetItemEncoding(TSwagEncoding.Create);
    fItemEncoding.Load(vJsonItemEncoding);
  end;

  fExtensions.ReadFrom(pJson);
end;

class function TSwagEncoding.GenerateMapJsonObject(pEncodings: TObjectList<TSwagEncoding>): TJSONObject;
var
  vEncoding: TSwagEncoding;
begin
  Result := TJSONObject.Create;
  for vEncoding in pEncodings do
    Result.AddPair(vEncoding.Name, vEncoding.GenerateJsonObject);
end;

class function TSwagEncoding.GenerateJsonArray(pEncodings: TObjectList<TSwagEncoding>): TJSONArray;
var
  vEncoding: TSwagEncoding;
begin
  Result := TJSONArray.Create;
  for vEncoding in pEncodings do
    Result.AddElement(vEncoding.GenerateJsonObject);
end;

class procedure TSwagEncoding.LoadMap(pJson: TJSONObject; pEncodings: TObjectList<TSwagEncoding>);
var
  vIndex: Integer;
  vEncoding: TSwagEncoding;
begin
  if not Assigned(pJson) then
    Exit;

  for vIndex := 0 to pJson.Count - 1 do
  begin
    if not (pJson.Pairs[vIndex].JsonValue is TJSONObject) then
      Continue;

    vEncoding := TSwagEncoding.Create;
    vEncoding.Name := pJson.Pairs[vIndex].JsonString.Value;
    vEncoding.Load(TJSONObject(pJson.Pairs[vIndex].JsonValue));
    pEncodings.Add(vEncoding);
  end;
end;

class procedure TSwagEncoding.LoadArray(pJson: TJSONArray; pEncodings: TObjectList<TSwagEncoding>);
var
  vIndex: Integer;
  vEncoding: TSwagEncoding;
begin
  if not Assigned(pJson) then
    Exit;

  for vIndex := 0 to pJson.Count - 1 do
  begin
    if not (pJson.Items[vIndex] is TJSONObject) then
      Continue;

    vEncoding := TSwagEncoding.Create;
    vEncoding.Load(TJSONObject(pJson.Items[vIndex]));
    pEncodings.Add(vEncoding);
  end;
end;

{ TSwagMediaType }

constructor TSwagMediaType.Create;
begin
  inherited Create;
  fSchema := TSwagDefinition.Create;
  fItemSchema := TSwagDefinition.Create;
  fExamples := TObjectList<TSwagExample>.Create;
  fEncoding := TObjectList<TSwagEncoding>.Create;
  fPrefixEncoding := TObjectList<TSwagEncoding>.Create;
  fExtensions := TSwagExtensions.Create;
end;

destructor TSwagMediaType.Destroy;
begin
  FreeAndNil(fSchema);
  FreeAndNil(fItemSchema);
  FreeAndNil(fExample);
  FreeAndNil(fExamples);
  FreeAndNil(fEncoding);
  FreeAndNil(fPrefixEncoding);
  FreeAndNil(fItemEncoding);
  FreeAndNil(fExtensions);
  inherited Destroy;
end;

procedure TSwagMediaType.SetExample(const pValue: TJSONValue);
begin
  if fExample = pValue then
    Exit;
  fExample.Free;
  fExample := pValue;
end;

procedure TSwagMediaType.SetItemEncoding(const pValue: TSwagEncoding);
begin
  if fItemEncoding = pValue then
    Exit;
  fItemEncoding.Free;
  fItemEncoding := pValue;
end;

function TSwagMediaType.AddExample(const pName: string): TSwagExample;
begin
  Result := TSwagExample.Create;
  Result.Name := pName;
  fExamples.Add(Result);
end;

function TSwagMediaType.AddEncoding(const pPropertyName: string): TSwagEncoding;
begin
  Result := TSwagEncoding.Create;
  Result.Name := pPropertyName;
  fEncoding.Add(Result);
end;

function TSwagMediaType.AddPrefixEncoding: TSwagEncoding;
begin
  Result := TSwagEncoding.Create;
  fPrefixEncoding.Add(Result);
end;

function TSwagMediaType.GenerateJsonObject: TJSONObject;
var
  vJsonSchema: TJSONObject;
begin
  Result := TJSONObject.Create;
  if not fRef.IsEmpty then
  begin
    Result.AddPair(c_SwagContentRef, fRef);
    Exit;
  end;

  vJsonSchema := GenerateDefinitionJsonObject(fSchema);
  if Assigned(vJsonSchema) then
    Result.AddPair(c_SwagContentSchema, vJsonSchema);
  vJsonSchema := GenerateDefinitionJsonObject(fItemSchema);
  if Assigned(vJsonSchema) then
    Result.AddPair(c_SwagContentItemSchema, vJsonSchema);
  if Assigned(fExample) then
    Result.AddPair(c_SwagContentExample, fExample.Clone as TJSONValue);
  if fExamples.Count > 0 then
    Result.AddPair(c_SwagContentExamples, TSwagExample.GenerateMapJsonObject(fExamples));
  if fEncoding.Count > 0 then
    Result.AddPair(c_SwagContentEncoding, TSwagEncoding.GenerateMapJsonObject(fEncoding));
  if fPrefixEncoding.Count > 0 then
    Result.AddPair(c_SwagContentPrefixEncoding, TSwagEncoding.GenerateJsonArray(fPrefixEncoding));
  if Assigned(fItemEncoding) then
    Result.AddPair(c_SwagContentItemEncoding, fItemEncoding.GenerateJsonObject);
  fExtensions.WriteTo(Result);
end;

procedure TSwagMediaType.Load(pJson: TJSONObject);
var
  vJsonItemEncoding: TJSONObject;
begin
  if not Assigned(pJson) then
    Exit;

  fRef := TSwagJson.ReadString(pJson, c_SwagContentRef);
  if not fRef.IsEmpty then
    Exit;

  LoadDefinition(fSchema, TSwagJson.ReadObject(pJson, c_SwagContentSchema));
  LoadDefinition(fItemSchema, TSwagJson.ReadObject(pJson, c_SwagContentItemSchema));
  SetExample(TSwagJson.CloneValue(pJson, c_SwagContentExample));
  TSwagExample.LoadMap(TSwagJson.ReadObject(pJson, c_SwagContentExamples), fExamples);
  TSwagEncoding.LoadMap(TSwagJson.ReadObject(pJson, c_SwagContentEncoding), fEncoding);
  TSwagEncoding.LoadArray(TSwagJson.ReadArray(pJson, c_SwagContentPrefixEncoding), fPrefixEncoding);

  vJsonItemEncoding := TSwagJson.ReadObject(pJson, c_SwagContentItemEncoding);
  if Assigned(vJsonItemEncoding) then
  begin
    SetItemEncoding(TSwagEncoding.Create);
    fItemEncoding.Load(vJsonItemEncoding);
  end;

  fExtensions.ReadFrom(pJson);
end;

class function TSwagMediaType.GenerateMapJsonObject(pMediaTypes: TObjectList<TSwagMediaType>;
  const pKeyByName: Boolean): TJSONObject;
var
  vMediaType: TSwagMediaType;
begin
  Result := TJSONObject.Create;
  for vMediaType in pMediaTypes do
    if pKeyByName then
      Result.AddPair(vMediaType.Name, vMediaType.GenerateJsonObject)
    else
      Result.AddPair(vMediaType.MediaType, vMediaType.GenerateJsonObject);
end;

class procedure TSwagMediaType.LoadMap(pJson: TJSONObject; pMediaTypes: TObjectList<TSwagMediaType>;
  const pKeyByName: Boolean);
var
  vIndex: Integer;
  vMediaType: TSwagMediaType;
begin
  if not Assigned(pJson) then
    Exit;

  for vIndex := 0 to pJson.Count - 1 do
  begin
    if not (pJson.Pairs[vIndex].JsonValue is TJSONObject) then
      Continue;

    vMediaType := TSwagMediaType.Create;
    if pKeyByName then
      vMediaType.Name := pJson.Pairs[vIndex].JsonString.Value
    else
      vMediaType.MediaType := pJson.Pairs[vIndex].JsonString.Value;
    vMediaType.Load(TJSONObject(pJson.Pairs[vIndex].JsonValue));
    pMediaTypes.Add(vMediaType);
  end;
end;

end.
