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

unit Swag.Doc.Path.Operation.RequestParameter;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.JSON,
  Swag.Common.Types,
  Swag.Doc.Definition,
  Swag.Doc.Example,
  Swag.Doc.Extensions,
  Swag.Doc.Path.Operation.Content;

type
  /// <summary>
  /// Describes a single operation parameter.
  /// A unique parameter is defined by a combination of a name and location.
  /// </summary>
  TSwagRequestParameter = class(TObject)
  private
    fName: string;
    fInLocation: TSwagRequestParameterInLocation;
    fRequired: Boolean;
    fSchema: TSwagDefinition;
    fDescription: string;
    fTypeParameter: TSwagTypeParameter;
    fPattern: string;
    fItems: TJSONObject;
    fFormat: string;
    fDefault: string;
    fEnum: TStringList;
    fAllowEmptyValue: Boolean;
    fRef: string;
    fDeprecated: Boolean;
    fStyle: TSwagRequestParameterStyle;
    fExplode: Boolean;
    fAllowReserved: Boolean;
    fExample: TJSONValue;
    fExamples: TObjectList<TSwagExample>;
    fContent: TObjectList<TSwagMediaType>;
    fExtensions: TSwagExtensions;
    procedure SetAllowEmptyValue(const pValue: Boolean);
    procedure SetItems(const pValue: TJSONObject);
    procedure SetExample(const pValue: TJSONValue);
  protected
    function ReturnInLocationToString: string;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    function GenerateJsonObject: TJSONObject;
    procedure Load(pJson: TJSONObject);

    /// <summary>
    /// Adds an example of the parameter value and returns it. Available in OpenAPI 3 only.
    /// </summary>
    function AddExample(const pName: string): TSwagExample;

    /// <summary>
    /// Adds a media type to the content of the parameter and returns it. The content MUST only contain one entry.
    /// Available in OpenAPI 3 only.
    /// </summary>
    function AddMediaType(const pMediaType: TSwagMimeType): TSwagMediaType;

    /// <summary>
    /// There are seven possible parameter types: Query, Header, Path, Form, Body, Cookie and QueryString.
    /// Form and Body are written as the request body of the operation in OpenAPI 3, and Cookie and QueryString
    /// are only available in OpenAPI 3.
    /// </summary>
    property InLocation: TSwagRequestParameterInLocation read fInLocation write fInLocation;

    /// <summary>
    /// Required. The name of the parameter. Parameter names are case sensitive.
    /// If in is "path", the name field MUST correspond to the associated path segment from the path field in the Paths Object.
    /// See Path Templating for further information.
    /// For all other cases, the name corresponds to the parameter name used based on the in property.
    /// </summary>
    property Name: string read fName write fName;

    /// <summary>
    /// A brief description of the parameter. This could contain examples of use. GFM syntax can be used for rich text representation.
    /// In OpenAPI 3, when the parameter is a reference, the description overrides the referenced one.
    /// </summary>
    property Description: string read fDescription write fDescription;

    /// <summary>
    /// Determines whether this parameter is mandatory. If the parameter is in "path", this property is required and its value MUST be true.
    /// Otherwise, the property MAY be included and its default value is false.
    /// </summary>
    property Required: Boolean read fRequired write fRequired;

    /// <summary>
    /// See https://tools.ietf.org/html/draft-fge-json-schema-validation-00#section-5.2.3.
    /// </summary>
    property Pattern: string read fPattern write fPattern;

    /// <summary>
    /// If in is "body"
    /// Required. The schema defining the type used for the body parameter.
    /// In OpenAPI 3 the schema can be defined for any parameter location and, when assigned, it is written
    /// instead of the type, format, pattern, default, enum and items properties.
    /// </summary>
    property Schema: TSwagDefinition read fSchema;

    /// <summary>
    /// Required if type is "array". Describes the type of items in the array.
    /// The parameter takes ownership of the assigned object.
    /// </summary>
    property Items: TJSONObject read fItems write SetItems;

    /// <summary>
    /// If in is any value other than "body"
    /// Required. The type of the parameter. Since the parameter is not located at the request body, it is limited to
    /// simple types (that is, not an object).
    /// The value MUST be one of "string", "number", "integer", "boolean", "array" or "file".
    /// If type is "file", the consumes MUST be either "multipart/form-data", " application/x-www-form-urlencoded" or both
    /// and the parameter MUST be in "formData".
    /// </summary>
    property TypeParameter: TSwagTypeParameter read fTypeParameter write fTypeParameter;

    /// <summary>
    /// Primitives have an optional modifier property format. Swagger uses several known formats to more finely define
    /// the data type being used. However, the format property is an open string-valued property, and can have any value
    /// to support documentation needs. Formats such as "email", "uuid", etc., can be used even though they are not
    /// defined by this specification.
    ///  </summary>
    property Format: string read fFormat write fFormat;

    /// <summary>
    /// Declares the value of the parameter that the server will use if none is provided, for example a "count" to
    /// control the number of results per page might default to 100 if not supplied by the client in the request.
    /// (Note: "default" has no meaning for required parameters.)
    /// See https://tools.ietf.org/html/draft-fge-json-schema-validation-00#section-6.2. Unlike JSON Schema
    /// this value MUST conform to the defined type for this parameter.
    property Default: string read fDefault write fDefault;


    property Enum: TStringList read fEnum;

    property AllowEmptyValue: Boolean read fAllowEmptyValue write SetAllowEmptyValue;

    property Ref: string read fRef write fRef;

    /// <summary>
    /// Specifies that a parameter is deprecated and SHOULD be transitioned out of usage. Default value is false.
    /// Available in OpenAPI 3 only.
    /// </summary>
    property Deprecated: Boolean read fDeprecated write fDeprecated;

    /// <summary>
    /// Describes how the parameter value will be serialized depending on the type of the parameter value.
    /// Available in OpenAPI 3 only.
    /// </summary>
    property Style: TSwagRequestParameterStyle read fStyle write fStyle;

    /// <summary>
    /// When this is true, parameter values of type array or object generate separate parameters for each value
    /// of the array or key-value pair of the map. For other types of parameters this property has no effect.
    /// It is written together with the Style property. Available in OpenAPI 3 only.
    /// </summary>
    property Explode: Boolean read fExplode write fExplode;

    /// <summary>
    /// Determines whether the parameter value SHOULD allow reserved characters, as defined by RFC3986,
    /// to be included without percent-encoding. This property only applies to parameters with an in value of query.
    /// The default value is false. Available in OpenAPI 3 only.
    /// </summary>
    property AllowReserved: Boolean read fAllowReserved write fAllowReserved;

    /// <summary>
    /// Example of the parameter's potential value. The Example and Examples fields are mutually exclusive.
    /// The parameter takes ownership of the assigned value. Available in OpenAPI 3 only.
    /// </summary>
    property Example: TJSONValue read fExample write SetExample;

    /// <summary>
    /// Examples of the parameter's potential value. Available in OpenAPI 3 only.
    /// </summary>
    property Examples: TObjectList<TSwagExample> read fExamples;

    /// <summary>
    /// A map containing the representation of the parameter, used instead of the schema. It MUST only contain one
    /// entry. A QueryString parameter without content is written with the application/x-www-form-urlencoded
    /// media type and the schema of the parameter. Available in OpenAPI 3 only.
    /// </summary>
    property Content: TObjectList<TSwagMediaType> read fContent;

    /// <summary>
    /// The Specification Extensions of the parameter.
    /// </summary>
    property Extensions: TSwagExtensions read fExtensions;
  end;

implementation

uses
  System.StrUtils,
  Swag.Common.Consts,
  Swag.Common.Types.Helpers;

const
  c_SwagRequestParameterIn = 'in';
  c_SwagRequestParameterName = 'name';
  c_SwagRequestParameterDescription = 'description';
  c_SwagRequestParameterRequired = 'required';
  c_SwagRequestParameterSchema = 'schema';
  c_SwagRequestParameterType = 'type';
  c_SwagRequestParameterDefault = 'default';
  c_SwagRequestParameterPattern = 'pattern';
  c_SwagRequestParameterFormat = 'format';
  c_SwagRequestParameterEnum = 'enum';
  c_SwagRequestParameterItems = 'items';
  c_SwagRequestParameterAllowEmptyValue = 'allowEmptyValue';
  c_SwagRequestParameterRef = '$ref';

{ TSwagRequestParameter }

constructor TSwagRequestParameter.Create;
begin
  inherited Create;
  fSchema := TSwagDefinition.Create;
  fEnum := TStringList.Create;
  fExamples := TObjectList<TSwagExample>.Create;
  fContent := TObjectList<TSwagMediaType>.Create;
  fExtensions := TSwagExtensions.Create;
end;

destructor TSwagRequestParameter.Destroy;
begin
  FreeAndNil(fSchema);
  FreeAndNil(fEnum);
  if Assigned(fItems) then
    FreeAndNil(fItems);
  FreeAndNil(fExample);
  FreeAndNil(fExamples);
  FreeAndNil(fContent);
  FreeAndNil(fExtensions);
  inherited Destroy;
end;

function TSwagRequestParameter.AddExample(const pName: string): TSwagExample;
begin
  Result := TSwagExample.Create;
  Result.Name := pName;
  fExamples.Add(Result);
end;

function TSwagRequestParameter.AddMediaType(const pMediaType: TSwagMimeType): TSwagMediaType;
begin
  Result := TSwagMediaType.Create;
  Result.MediaType := pMediaType;
  fContent.Add(Result);
end;

function TSwagRequestParameter.GenerateJsonObject: TJSONObject;
var
  vJsonObject: TJsonObject;
  vJsonEnum: TJSONArray;
  vIndex: Integer;
begin
  vJsonObject := TJsonObject.Create;

  if fRef.Length > 0 then
  begin
    vJsonObject.AddPair('$ref', fRef);
    Result := vJsonObject;
    Exit;
  end;

  vJsonObject.AddPair(c_SwagRequestParameterIn, ReturnInLocationToString);
  vJsonObject.AddPair(c_SwagRequestParameterName, fName);
  if not fDescription.IsEmpty then
    vJsonObject.AddPair(c_SwagRequestParameterDescription, fDescription);
  if not fPattern.IsEmpty then
    vJsonObject.AddPair(c_SwagRequestParameterPattern, fPattern);
  if not fDefault.IsEmpty then
    vJsonObject.AddPair(c_SwagRequestParameterDefault, fDefault);

  if fRequired or (fInLocation = rpiPath) then
    vJsonObject.AddPair(c_SwagRequestParameterRequired, TJSONBool.Create(True));

  if fAllowEmptyValue and ((fInLocation = rpiQuery) or (fInLocation = rpiFormData)) then
    vJsonObject.AddPair(c_SwagRequestParameterAllowEmptyValue, TJSONBool.Create(True));

  if fInLocation = rpiBody then // schema only allow in body parameters
  begin
    if (not fSchema.Name.IsEmpty) then
      vJsonObject.AddPair(c_SwagRequestParameterSchema, fSchema.GenerateJsonRefDefinition)
    else if Assigned(fSchema.JsonSchema) then
      vJsonObject.AddPair(c_SwagRequestParameterSchema, fSchema.JsonSchema.Clone as TJSONObject);
  end;
  if (fTypeParameter <> stpNotDefined) then
    vJsonObject.AddPair(c_SwagRequestParameterType, c_SwagTypeParameter[fTypeParameter]);

  if Assigned(fItems) then
    vJsonObject.AddPair(c_SwagRequestParameterItems, fItems.Clone as TJSONObject);

  if not fFormat.IsEmpty then
    vJsonObject.AddPair(c_SwagRequestParameterFormat, fFormat);

  if fEnum.Count > 0 then
  begin
    vJsonEnum := TJSONArray.Create;
    for vIndex := 0 to fEnum.Count - 1 do
    begin
      vJsonEnum.Add(fEnum[vIndex]);
    end;
    vJsonObject.AddPair(c_SwagRequestParameterEnum, vJsonEnum);
  end;

  fExtensions.WriteTo(vJsonObject);
  Result := vJsonObject;
end;

procedure TSwagRequestParameter.Load(pJson: TJSONObject);
var
 vEnum: TJSONArray;
  i: Integer;
begin
  fExtensions.ReadFrom(pJson);

  if Assigned(pJson.Values[c_SwagRequestParameterRef]) then
  begin
    fRef := pJson.Values[c_SwagRequestParameterRef].Value;
    Exit;
  end;

  if Assigned(pJson.Values[c_SwagRequestParameterIn]) then
    fInLocation.ToType(pJson.Values[c_SwagRequestParameterIn].Value);

  if Assigned(pJson.Values[c_SwagRequestParameterAllowEmptyValue]) and
    ((fInLocation = rpiQuery) or (fInLocation = rpiFormData)) then
    fAllowEmptyValue := (pJson.Values[c_SwagRequestParameterAllowEmptyValue] as TJSONBool).AsBoolean;

  if Assigned(pJson.Values[c_SwagRequestParameterRequired]) then
    fRequired := (pJson.Values[c_SwagRequestParameterRequired] as TJSONBool).AsBoolean
  else
    fRequired := False;

  if Assigned(pJson.Values[c_SwagRequestParameterPattern]) then
    fPattern := pJson.Values[c_SwagRequestParameterPattern].Value;

  if Assigned(pJson.Values[c_SwagRequestParameterName]) then
    fName := pJson.Values[c_SwagRequestParameterName].Value;

  if Assigned(pJson.Values[c_SwagRequestParameterDescription]) then
    fDescription := pJson.Values[c_SwagRequestParameterDescription].Value;

  if Assigned(pJson.Values[c_SwagRequestParameterFormat]) then
    fFormat := pJson.Values[c_SwagRequestParameterFormat].Value;

  if Assigned(pJson.Values[c_SwagRequestParameterType]) then
    fTypeParameter.ToType(pJson.Values[c_SwagRequestParameterType].Value);

  if Assigned(pJson.Values[c_SwagRequestParameterDefault]) then
    fDefault := pJson.Values[c_SwagRequestParameterDefault].Value;

  if Assigned(pJson.Values[c_SwagRequestParameterSchema]) then
    fSchema.JsonSchema := pJson.Values[c_SwagRequestParameterSchema].Clone as TJSONObject;

  if pJson.Values[c_SwagRequestParameterItems] is TJSONObject then
    SetItems(pJson.Values[c_SwagRequestParameterItems].Clone as TJSONObject);

  if Assigned(pJson.Values[c_SwagRequestParameterEnum]) then
  begin
    vEnum := pJson.Values[c_SwagRequestParameterEnum] as TJSONArray;
    for i := 0 to vEnum.Count - 1 do
    begin
      fEnum.Add(vEnum.Items[i].Value);
    end;
  end;
end;

function TSwagRequestParameter.ReturnInLocationToString: string;
begin
  Result := c_SwagRequestParameterInLocation[fInLocation];
end;

procedure TSwagRequestParameter.SetAllowEmptyValue(const pValue: Boolean);
begin
  if (fInLocation = rpiQuery) or (fInLocation = rpiFormData) then
    fAllowEmptyValue := pValue
  else
    raise Exception.Create('allowEmptyValue not allowed to be set on ' + ReturnInLocationToString);
end;

procedure TSwagRequestParameter.SetItems(const pValue: TJSONObject);
begin
  if fItems = pValue then
    Exit;
  if Assigned(fItems) then
    fItems.Free;
  fItems := pValue;
end;

procedure TSwagRequestParameter.SetExample(const pValue: TJSONValue);
begin
  if fExample = pValue then
    Exit;
  fExample.Free;
  fExample := pValue;
end;

end.
