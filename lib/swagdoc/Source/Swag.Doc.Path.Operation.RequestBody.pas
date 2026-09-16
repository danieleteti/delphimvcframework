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

unit Swag.Doc.Path.Operation.RequestBody;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.JSON,
  Swag.Common.Types,
  Swag.Doc.Extensions,
  Swag.Doc.Path.Operation.Content;

type
  /// <summary>
  /// Describes a single request body. In OpenAPI 3 the request body replaces the body and formData
  /// parameters of Swagger 2.0 and allows a different schema for each media type accepted by the operation.
  /// When a Swagger 2.0 document is generated the request body is written as a body parameter and its media
  /// types are written in the consumes list of the operation.
  /// </summary>
  TSwagRequestBody = class(TObject)
  private
    fName: string;
    fRef: string;
    fDescription: string;
    fRequired: Boolean;
    fContent: TObjectList<TSwagMediaType>;
    fExtensions: TSwagExtensions;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    /// <summary>
    /// Adds a media type to the content of the request body and returns it, so the schema can be defined.
    /// </summary>
    function AddMediaType(const pMediaType: TSwagMimeType): TSwagMediaType;

    /// <summary>
    /// Returns True when neither a reference nor a content was defined.
    /// </summary>
    function IsEmpty: Boolean;

    function GenerateJsonObject: TJSONObject;

    /// <summary>
    /// Generates the Swagger 2.0 body parameter equivalent to this request body.
    /// The schema of the first media type is used as the schema of the parameter.
    /// </summary>
    function GenerateBodyParameterJsonObject: TJSONObject;

    /// <summary>
    /// Generates the Swagger 2.0 consumes array with the media types of the content.
    /// </summary>
    function GenerateMediaTypesJsonArray: TJSONArray;

    /// <summary>
    /// Returns True when the first media type of the content is a form media type with an object schema.
    /// Such a request body is written as formData parameters in a Swagger 2.0 document.
    /// </summary>
    function IsFormContent: Boolean;

    /// <summary>
    /// Adds to the array the Swagger 2.0 formData parameters equivalent to the properties of the form schema.
    /// </summary>
    procedure GenerateFormDataParameters(pJsonParameters: TJSONArray);

    procedure Load(pJson: TJSONObject);

    /// <summary>
    /// The name used as the key of the request body when it is placed under the reusable components of the document.
    /// </summary>
    property Name: string read fName write fName;

    /// <summary>
    /// A reference to a reusable request body, for example #/components/requestBodies/employee.
    /// When defined, only the reference and the Description, which overrides the referenced one, are written.
    /// </summary>
    property Ref: string read fRef write fRef;

    /// <summary>
    /// A brief description of the request body. This could contain examples of use.
    /// CommonMark syntax MAY be used for rich text representation.
    /// </summary>
    property Description: string read fDescription write fDescription;

    /// <summary>
    /// Determines if the request body is required in the request. Defaults to false.
    /// </summary>
    property Required: Boolean read fRequired write fRequired;

    /// <summary>
    /// Required. The content of the request body. Each item maps a media type or media type range to its schema.
    /// </summary>
    property Content: TObjectList<TSwagMediaType> read fContent;

    /// <summary>
    /// The Specification Extensions of the request body. Written in OpenAPI 3 documents.
    /// </summary>
    property Extensions: TSwagExtensions read fExtensions;
  end;

implementation

uses
  Swag.Common.Consts,
  Swag.Common.Json;

const
  c_SwagRequestBodyRef = '$ref';
  c_SwagRequestBodyDescription = 'description';
  c_SwagRequestBodyRequired = 'required';
  c_SwagRequestBodyContent = 'content';
  c_SwagRequestBodyParameterIn = 'in';
  c_SwagRequestBodyParameterName = 'name';
  c_SwagRequestBodyParameterSchema = 'schema';
  c_SwagRequestBodyParameterDefaultName = 'body';
  c_SwagRequestBodySchemaProperties = 'properties';
  c_SwagRequestBodySchemaRequired = 'required';
  c_SwagRequestBodySchemaType = 'type';
  c_SwagRequestBodySchemaFormat = 'format';
  c_SwagRequestBodySchemaContentMediaType = 'contentMediaType';
  c_SwagRequestBodySchemaTypeString = 'string';
  c_SwagRequestBodySchemaTypeObject = 'object';
  c_SwagRequestBodySchemaTypeFile = 'file';
  c_SwagRequestBodySchemaFormatBinary = 'binary';
  c_SwagRequestBodyFormDataFields: array[0..5] of string = ('type', 'format', 'enum', 'default', 'pattern', 'items');

{ TSwagRequestBody }

constructor TSwagRequestBody.Create;
begin
  inherited Create;
  fContent := TObjectList<TSwagMediaType>.Create;
  fExtensions := TSwagExtensions.Create;
end;

destructor TSwagRequestBody.Destroy;
begin
  FreeAndNil(fContent);
  FreeAndNil(fExtensions);
  inherited Destroy;
end;

function TSwagRequestBody.AddMediaType(const pMediaType: TSwagMimeType): TSwagMediaType;
begin
  Result := TSwagMediaType.Create;
  Result.MediaType := pMediaType;
  fContent.Add(Result);
end;

function TSwagRequestBody.IsEmpty: Boolean;
begin
  Result := fRef.IsEmpty and (fContent.Count = 0);
end;

function TSwagRequestBody.GenerateJsonObject: TJSONObject;
begin
  Result := TJSONObject.Create;
  if not fRef.IsEmpty then
  begin
    Result.AddPair(c_SwagRequestBodyRef, fRef);
    if not fDescription.IsEmpty then
      Result.AddPair(c_SwagRequestBodyDescription, fDescription);
    Exit;
  end;

  if not fDescription.IsEmpty then
    Result.AddPair(c_SwagRequestBodyDescription, fDescription);
  if fRequired then
    Result.AddPair(c_SwagRequestBodyRequired, TJSONBool.Create(True));
  Result.AddPair(c_SwagRequestBodyContent, TSwagMediaType.GenerateMapJsonObject(fContent, False));
  fExtensions.WriteTo(Result);
end;

function TSwagRequestBody.GenerateBodyParameterJsonObject: TJSONObject;
var
  vMediaType: TSwagMediaType;
begin
  Result := TJSONObject.Create;
  if not fRef.IsEmpty then
  begin
    Result.AddPair(c_SwagRequestBodyRef, fRef);
    Exit;
  end;

  Result.AddPair(c_SwagRequestBodyParameterIn, c_SwagRequestParameterInLocation[rpiBody]);
  if fName.IsEmpty then
    Result.AddPair(c_SwagRequestBodyParameterName, c_SwagRequestBodyParameterDefaultName)
  else
    Result.AddPair(c_SwagRequestBodyParameterName, fName);
  if not fDescription.IsEmpty then
    Result.AddPair(c_SwagRequestBodyDescription, fDescription);
  if fRequired then
    Result.AddPair(c_SwagRequestBodyRequired, TJSONBool.Create(True));

  if fContent.Count = 0 then
    Exit;

  vMediaType := fContent[0];
  if not vMediaType.Schema.Name.IsEmpty then
    Result.AddPair(c_SwagRequestBodyParameterSchema, vMediaType.Schema.GenerateJsonRefDefinition)
  else if Assigned(vMediaType.Schema.JsonSchema) then
    Result.AddPair(c_SwagRequestBodyParameterSchema, vMediaType.Schema.JsonSchema.Clone as TJSONObject);
end;

function TSwagRequestBody.GenerateMediaTypesJsonArray: TJSONArray;
var
  vIndex: Integer;
begin
  Result := TJSONArray.Create;
  for vIndex := 0 to fContent.Count - 1 do
    Result.Add(fContent[vIndex].MediaType);
end;

function TSwagRequestBody.IsFormContent: Boolean;
var
  vMediaType: TSwagMediaType;
begin
  Result := False;
  if (not fRef.IsEmpty) or (fContent.Count = 0) then
    Exit;

  vMediaType := fContent[0];
  Result := (SameText(vMediaType.MediaType, c_SwagMimeTypeFormUrlEncoded) or
    SameText(vMediaType.MediaType, c_SwagMimeTypeMultipartFormData)) and
    Assigned(vMediaType.Schema.JsonSchema) and
    (vMediaType.Schema.JsonSchema.Values[c_SwagRequestBodySchemaProperties] is TJSONObject);
end;

procedure TSwagRequestBody.GenerateFormDataParameters(pJsonParameters: TJSONArray);
var
  vJsonSchema: TJSONObject;
  vJsonProperties: TJSONObject;
  vJsonProperty: TJSONObject;
  vJsonParameter: TJSONObject;
  vJsonRequired: TJSONArray;
  vIndex: Integer;
  vRequiredIndex: Integer;
  vFieldIndex: Integer;
  vName: string;
  vRequired: Boolean;
  vIsFile: Boolean;
begin
  if not IsFormContent then
    Exit;

  vJsonSchema := fContent[0].Schema.JsonSchema;
  vJsonProperties := TJSONObject(vJsonSchema.Values[c_SwagRequestBodySchemaProperties]);
  vJsonRequired := nil;
  if vJsonSchema.Values[c_SwagRequestBodySchemaRequired] is TJSONArray then
    vJsonRequired := TJSONArray(vJsonSchema.Values[c_SwagRequestBodySchemaRequired]);

  for vIndex := 0 to vJsonProperties.Count - 1 do
  begin
    if not (vJsonProperties.Pairs[vIndex].JsonValue is TJSONObject) then
      Continue;

    vName := vJsonProperties.Pairs[vIndex].JsonString.Value;
    vJsonProperty := TJSONObject(vJsonProperties.Pairs[vIndex].JsonValue);

    vRequired := False;
    if Assigned(vJsonRequired) then
      for vRequiredIndex := 0 to vJsonRequired.Count - 1 do
        if vJsonRequired.Items[vRequiredIndex].Value = vName then
        begin
          vRequired := True;
          Break;
        end;

    vIsFile := (TSwagJson.ReadString(vJsonProperty, c_SwagRequestBodySchemaType) = c_SwagRequestBodySchemaTypeString) and
      ((TSwagJson.ReadString(vJsonProperty, c_SwagRequestBodySchemaFormat) = c_SwagRequestBodySchemaFormatBinary) or
      (not TSwagJson.ReadString(vJsonProperty, c_SwagRequestBodySchemaContentMediaType).IsEmpty));

    vJsonParameter := TJSONObject.Create;
    vJsonParameter.AddPair(c_SwagRequestBodyParameterIn, c_SwagRequestParameterInLocation[rpiFormData]);
    vJsonParameter.AddPair(c_SwagRequestBodyParameterName, vName);
    if vJsonProperty.Values[c_SwagRequestBodyDescription] is TJSONString then
      vJsonParameter.AddPair(c_SwagRequestBodyDescription, vJsonProperty.Values[c_SwagRequestBodyDescription].Value);
    if vRequired then
      vJsonParameter.AddPair(c_SwagRequestBodyRequired, TJSONBool.Create(True));

    if vIsFile then
      vJsonParameter.AddPair(c_SwagRequestBodySchemaType, c_SwagRequestBodySchemaTypeFile)
    else if TSwagJson.ReadString(vJsonProperty, c_SwagRequestBodySchemaType) = c_SwagRequestBodySchemaTypeObject then
      vJsonParameter.AddPair(c_SwagRequestBodySchemaType, c_SwagRequestBodySchemaTypeString)
    else
      for vFieldIndex := Low(c_SwagRequestBodyFormDataFields) to High(c_SwagRequestBodyFormDataFields) do
        if Assigned(vJsonProperty.Values[c_SwagRequestBodyFormDataFields[vFieldIndex]]) then
          vJsonParameter.AddPair(c_SwagRequestBodyFormDataFields[vFieldIndex],
            vJsonProperty.Values[c_SwagRequestBodyFormDataFields[vFieldIndex]].Clone as TJSONValue);

    pJsonParameters.Add(vJsonParameter);
  end;
end;

procedure TSwagRequestBody.Load(pJson: TJSONObject);
begin
  if not Assigned(pJson) then
    Exit;

  fRef := TSwagJson.ReadString(pJson, c_SwagRequestBodyRef);
  fDescription := TSwagJson.ReadString(pJson, c_SwagRequestBodyDescription);
  if not fRef.IsEmpty then
    Exit;

  fRequired := TSwagJson.ReadBoolean(pJson, c_SwagRequestBodyRequired);
  TSwagMediaType.LoadMap(TSwagJson.ReadObject(pJson, c_SwagRequestBodyContent), fContent, False);
  fExtensions.ReadFrom(pJson);
end;

end.
