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

unit Swag.Doc.Path.Operation;

interface

uses
  System.Generics.Collections,
  System.JSON,
  Swag.Common.Types,
  Swag.Doc.Path.Operation.Response,
  Swag.Doc.Tags,
  Swag.Doc.Path.Operation.RequestParameter;

type
  /// <summary>
  /// Describes a single API operation on a path.
  /// </summary>
  TSwagPathOperation = class(TObject)
  private
    fOperation: TSwagPathTypeOperation;
    fDescription: string;
    fConsumes: TList<TSwagMimeType>;
    fProduces: TList<TSwagMimeType>;
    fParameters: TObjectList<TSwagRequestParameter>;
    fResponses: TObjectDictionary<TSwagStatusCode, TSwagResponse>;
    fSecurity: TList<TSwagSecuritySchemeName>;
    fTags: TList<string>;
    fExternalDocs: TSwagExternalDocs;
    fOperationId: string;
    fDeprecated: Boolean;
    fSummary: string;
    function GetOperationToString: string;
  protected
    function GenerateTagsJsonArray(pTagList: TList<string>): TJSONArray;
    function GenerateMimeTypesJsonArray(pMimeTypesList: TList<TSwagMimeType>): TJSONArray;
    function GenerateParametersJsonArray: TJSONArray;
    function GenarateResponsesJsonObject: TJSONObject;
    function GenerateSecurityJsonArray: TJSONArray;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    function GenerateJsonObject: TJSONObject;

    property Operation: TSwagPathTypeOperation read fOperation write fOperation;
    property OperationToString: string read GetOperationToString;
    property OperationId : string read fOperationId write fOperationId;
    property Summary: string read fSummary write fSummary;

    /// <summary>
    /// A list of tags for API documentation control.
    /// Tags can be used for logical grouping of operations by resources or any other qualifier.
    /// </summary>
    property Tags: TList<string> read fTags;

    /// <summary>
    /// A verbose explanation of the operation behavior. GFM syntax can be used for rich text representation.
    /// </summary>
    property Description: string read fDescription write fDescription;

    /// <summary>
    /// A list of MIME types the operation can consume. This overrides the consumes definition at the Swagger Object.
    /// An empty value MAY be used to clear the global definition. Value MUST be as described under Mime Types.
    /// </summary>
    property Consumes: TList<TSwagMimeType> read fConsumes;

    /// <summary>
    /// A list of MIME types the operation can produce. This overrides the produces definition at the Swagger Object.
    /// An empty value MAY be used to clear the global definition. Value MUST be as described under Mime Types.
    /// </summary>
    property Produces: TList<TSwagMimeType> read fProduces;

    /// <summary>
    /// A list of parameters that are applicable for this operation.
    /// If a parameter is already defined at the Path Item, the new definition will override it, but can never remove it.
    /// The list MUST NOT include duplicated parameters.
    /// A unique parameter is defined by a combination of a name and location.
    /// The list can use the Reference Object to link to parameters that are defined at the Swagger Object's parameters.
    /// There can be one "body" parameter at most.
    /// </summary>
    property Parameters: TObjectList<TSwagRequestParameter> read fParameters;

    /// <summary>
    /// Required. The list of possible responses as they are returned from executing this operation.
    /// </summary>
    property Responses: TObjectDictionary<TSwagStatusCode, TSwagResponse> read fResponses;

    /// <summary>
    /// Declares this operation to be deprecated. Usage of the declared operation should be refrained.
    /// Default value is false.
    /// </summary>
    property Deprecated: Boolean read fDeprecated write fDeprecated;

    /// <summary>
    /// Lists the required security schemes to execute this operation.
    /// The object can have multiple security schemes declared in it which are all required (that is, there is a logical
    /// AND between the schemes).
    /// The name used for each property MUST correspond to a security scheme declared in the Security Definitions.
    /// A declaration of which security schemes are applied for this operation.
    /// The list of values describes alternative security schemes that can be used (that is, there is a logical
    /// OR between the security requirements). This definition overrides any declared top-level security.
    /// To remove a top-level security declaration, an empty array can be used.
    /// </summary>
    property Security: TList<TSwagSecuritySchemeName> read fSecurity;

    property ExternalDocs: TSwagExternalDocs read fExternalDocs;
  end;

implementation

uses
  System.SysUtils,
  Swag.Doc.Path,
  Swag.Common.Consts;

const
  c_SwagPathOperationDescription = 'description';
  c_SwagPathOperationTags = 'tags';
  c_SwagPathOperationOperationId = 'operationId';
  c_SwagPathOperationDeprecated = 'deprecated';
  c_SwagPathOperationProduces = 'produces';
  c_SwagPathOperationConsumes = 'consumes';
  c_SwagPathOperationParameters = 'parameters';
  c_SwagPathOperationResponses = 'responses';
  c_SwagPathOperationSecurity = 'security';
  c_SwagPathOperationSummary = 'summary';
  c_SwagPathOperationExternalDocs = 'externalDocs';


{ TSwagPathOperation }

constructor TSwagPathOperation.Create;
begin
  inherited Create;
  fTags := TList<string>.Create;
  fConsumes := TList<TSwagMimeType>.Create;
  fProduces := TList<TSwagMimeType>.Create;
  fParameters := TObjectList<TSwagRequestParameter>.Create;
  fResponses := TObjectDictionary<TSwagStatusCode, TSwagResponse>.Create([doOwnsValues]);
  fSecurity := TList<TSwagSecuritySchemeName>.Create;
  fExternalDocs := TSwagExternalDocs.Create;
end;

destructor TSwagPathOperation.Destroy;
begin
  FreeAndNil(fProduces);
  FreeAndNil(fConsumes);
  FreeAndNil(fResponses);
  FreeAndNil(fParameters);
  FreeAndNil(fSecurity);
  FreeAndNil(fTags);
  FreeAndNil(fExternalDocs);

  inherited Destroy;
end;

function TSwagPathOperation.GetOperationToString: string;
begin
  Result := c_SwagPathOperationHttpVerbs[fOperation];
end;

function TSwagPathOperation.GenarateResponsesJsonObject: TJSONObject;
var
  vResponse: TSwagResponse;
  vResponsesSortedArray: TArray<TSwagStatusCode>;
  vStatusCode: TSwagStatusCode;
begin
  Result := TJsonObject.Create;
  vResponsesSortedArray := fResponses.Keys.ToArray;
  TArray.Sort<TSwagStatusCode>(vResponsesSortedArray);
  for vStatusCode in vResponsesSortedArray do
  begin
    vResponse := fResponses.Items[vStatusCode];
    Result.AddPair(vResponse.StatusCode, vResponse.GenerateJsonObject);
  end;
end;

function TSwagPathOperation.GenerateMimeTypesJsonArray(pMimeTypesList: TList<TSwagMimeType>): TJSONArray;
var
  vIndex: Integer;
begin
  Result := TJSONArray.Create;
  for vIndex := 0 to pMimeTypesList.Count -1 do
    Result.Add(pMimeTypesList.Items[vIndex]);
end;

function TSwagPathOperation.GenerateParametersJsonArray: TJSONArray;
var
  vIndex: Integer;
begin
  Result := TJSONArray.Create;
  for vIndex := 0 to fParameters.Count - 1 do
    Result.Add(fParameters.Items[vIndex].GenerateJsonObject);
end;

// suports only JWT in swagger version 2.0
function TSwagPathOperation.GenerateSecurityJsonArray: TJSONArray;
var
  vIndex: Integer;
  vJsonItem: TJsonObject;
  vJsonListSecurityScopes: TJSONArray;
begin
  Result := TJSONArray.Create;
  for vIndex := 0 to fSecurity.Count - 1 do
  begin
    vJsonListSecurityScopes := TJSONArray.Create;
    vJsonItem := TJsonObject.Create;
    vJsonItem.AddPair(fSecurity.Items[vIndex], vJsonListSecurityScopes);
    Result.Add(vJsonItem);
  end;
end;

function TSwagPathOperation.GenerateTagsJsonArray(pTagList: TList<string>): TJSONArray;
var
  vIndex: Integer;
begin
  Result := TJSONArray.Create;
  for vIndex := 0 to pTagList.Count -1 do
    Result.Add(pTagList.Items[vIndex]);
end;

function TSwagPathOperation.GenerateJsonObject: TJSONObject;
var
  vJsonObject: TJsonObject;
begin
  vJsonObject := TJsonObject.Create;
  if (fTags.Count > 0) then
    vJsonObject.AddPair(c_SwagPathOperationTags, GenerateTagsJsonArray(fTags));

  if fSummary.Length > 0 then
    vJsonObject.AddPair(c_SwagPathOperationSummary, fSummary);
  if fDescription.Length > 0  then
    vJsonObject.AddPair(c_SwagPathOperationDescription, fDescription);
  if (not fExternalDocs.url.IsEmpty) or (not fExternalDocs.description.IsEmpty) then
    vJsonObject.AddPair(c_SwagPathOperationExternalDocs, fExternalDocs.GenerateJsonObject);
  
  if fDeprecated then
    vJsonObject.AddPair(c_SwagPathOperationDeprecated, TJSONBool.Create(fDeprecated));
  if not fOperationId.IsEmpty then
    vJsonObject.AddPair(c_SwagPathOperationOperationId, fOperationId);
  if (fConsumes.Count > 0) then
    vJsonObject.AddPair(c_SwagPathOperationConsumes, GenerateMimeTypesJsonArray(fConsumes));
  if (fProduces.Count > 0) then
    vJsonObject.AddPair(c_SwagPathOperationProduces, GenerateMimeTypesJsonArray(fProduces));
  if (fParameters.Count > 0) then
    vJsonObject.AddPair(c_SwagPathOperationParameters, GenerateParametersJsonArray);
  if (fResponses.Count > 0) then
    vJsonObject.AddPair(c_SwagPathOperationResponses, GenarateResponsesJsonObject);
  if (fSecurity.Count > 0) then
    vJsonObject.AddPair(c_SwagPathOperationSecurity, GenerateSecurityJsonArray);
  Result := vJsonObject;
end;

end.
