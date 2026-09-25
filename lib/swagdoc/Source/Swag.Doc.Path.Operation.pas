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
  System.Classes,
  System.Generics.Collections,
  System.JSON,
  Swag.Common.Types,
  Swag.Doc.Server,
  Swag.Doc.Tags,
  Swag.Doc.Extensions,
  Swag.Doc.SecurityRequirement,
  Swag.Doc.Path.Operation.Response,
  Swag.Doc.Path.Operation.RequestParameter,
  Swag.Doc.Path.Operation.RequestBody;

type
  TSwagPath = class;

  /// <summary>
  /// A map of possible out-of band callbacks related to the parent operation. Each item of PathItems describes a
  /// request that may be initiated by the API provider and the expected responses, and its Uri is the runtime
  /// expression that identifies the URL of the callback, for example {$request.body#/callbackUrl}.
  /// Available in OpenAPI 3 only.
  /// </summary>
  TSwagCallback = class(TObject)
  private
    fName: string;
    fRef: string;
    fPathItems: TObjectList<TSwagPath>;
    fExtensions: TSwagExtensions;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    /// <summary>
    /// Adds a path item identified by a runtime expression and returns it, so its operations can be defined.
    /// </summary>
    function AddPathItem(const pExpression: string): TSwagPath;

    /// <summary>
    /// The key of the callback in the callbacks map of the operation or of the components.
    /// </summary>
    property Name: string read fName write fName;

    /// <summary>
    /// A reference to a reusable callback, for example #/components/callbacks/EmployeeExported.
    /// When defined, the other fields are not written.
    /// </summary>
    property Ref: string read fRef write fRef;

    /// <summary>
    /// The path items of the callback. The Uri of each item is a runtime expression.
    /// </summary>
    property PathItems: TObjectList<TSwagPath> read fPathItems;

    /// <summary>
    /// The Specification Extensions of the callback.
    /// </summary>
    property Extensions: TSwagExtensions read fExtensions;
  end;

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
    fRequestBody: TSwagRequestBody;
    fServers: TObjectList<TSwagServer>;
    fSecurityRequirements: TObjectList<TSwagSecurityRequirement>;
    fDisableSecurity: Boolean;
    fCallbacks: TObjectList<TSwagCallback>;
    fAdditionalMethod: string;
    fExtensions: TSwagExtensions;
    function GetOperationToString: string;
    function HasPayloadParameter: Boolean;
    function UseRequestBodyAsParameter: Boolean;
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

    /// <summary>
    /// Generates the security requirements of the operation: the SecurityRequirements list when it is not empty,
    /// otherwise one requirement for each name of the Security list, or an empty array when DisableSecurity is True.
    /// Returns nil when the operation does not declare its security.
    /// </summary>
    function GenerateSecurityRequirementsJsonArray: TJSONArray;

    /// <summary>
    /// Adds a security requirement to the operation and returns it, so its schemes and scopes can be defined.
    /// </summary>
    function AddSecurityRequirement: TSwagSecurityRequirement;

    /// <summary>
    /// Adds a callback to the operation and returns it. Available in OpenAPI 3 only.
    /// </summary>
    function AddCallback(const pName: string): TSwagCallback;

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
    /// In OpenAPI 3 the list defines the media types of the request body generated from the body and formData parameters.
    /// </summary>
    property Consumes: TList<TSwagMimeType> read fConsumes;

    /// <summary>
    /// A list of MIME types the operation can produce. This overrides the produces definition at the Swagger Object.
    /// An empty value MAY be used to clear the global definition. Value MUST be as described under Mime Types.
    /// In OpenAPI 3 the list defines the media types of the responses that do not declare their own content.
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
    /// The request body applicable for this operation. Available in OpenAPI 3, where it replaces the body and
    /// formData parameters. When it is defined and the operation has no body or formData parameter, a Swagger 2.0
    /// document writes it as a body parameter.
    /// </summary>
    property RequestBody: TSwagRequestBody read fRequestBody;

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
    /// Each name of this list is written as a requirement without scopes; use SecurityRequirements to declare scopes
    /// or schemes that are required together.
    /// </summary>
    property Security: TList<TSwagSecuritySchemeName> read fSecurity;

    property ExternalDocs: TSwagExternalDocs read fExternalDocs;

    /// <summary>
    /// An alternative server array to service this operation. If an alternative server object is specified at the
    /// Path Item Object or Root level, it will be overridden by this value. Available in OpenAPI 3 only.
    /// </summary>
    property Servers: TObjectList<TSwagServer> read fServers;

    /// <summary>
    /// The alternative security requirements of the operation (logical OR). Each requirement lists the schemes that
    /// are all required (logical AND) with their scopes. When the list is not empty, the Security list is ignored.
    /// </summary>
    property SecurityRequirements: TObjectList<TSwagSecurityRequirement> read fSecurityRequirements;

    /// <summary>
    /// When True and no security requirement is declared, the security of the operation is written as an empty
    /// array, which removes the security declared at the document level.
    /// </summary>
    property DisableSecurity: Boolean read fDisableSecurity write fDisableSecurity;

    /// <summary>
    /// The out-of band callbacks related to the operation. Available in OpenAPI 3 only.
    /// </summary>
    property Callbacks: TObjectList<TSwagCallback> read fCallbacks;

    /// <summary>
    /// The HTTP method of an operation listed in the AdditionalOperations of a path, with the same capitalization that
    /// is sent in the request, for example LINK or PURGE. Available in OpenAPI 3 only.
    /// </summary>
    property AdditionalMethod: string read fAdditionalMethod write fAdditionalMethod;

    /// <summary>
    /// The Specification Extensions of the operation.
    /// </summary>
    property Extensions: TSwagExtensions read fExtensions;
  end;

  /// <summary>
  /// Holds the relative paths to the individual endpoints.
  /// The path is appended to the basePath in order to construct the full URL.
  /// The Paths may be empty, due to ACL constraints.
  /// </summary>
  TSwagPath = class(TObject)
  private
    fOperations: TObjectList<TSwagPathOperation>;
    fUri: string;
    fParameters: TObjectList<TSwagRequestParameter>;
    fSummary: string;
    fDescription: string;
    fServers: TObjectList<TSwagServer>;
    fRef: string;
    fAdditionalOperations: TObjectList<TSwagPathOperation>;
    fExtensions: TSwagExtensions;
    procedure LoadResponse(pOperation: TSwagPathOperation; pJsonResponse: TJSONObject);
    procedure LoadOperationScopedParameters(pOperation: TSwagPathOperation; pJsonRequestParams: TJSONArray);
    procedure LoadPathScopedParameters(pJsonRequestParams: TJSONArray);
    procedure LoadTags(pOperation: TSwagPathOperation; pJsonTags: TJSONArray);
    procedure LoadProduces(pOperation: TSwagPathOperation; pJsonProduces: TJSONArray);
    procedure LoadConsumes(pOperation: TSwagPathOperation; pJsonConsumes: TJSONArray);
    procedure LoadSecurity(pOperation: TSwagPathOperation; pJsonSecurity: TJSONArray);
    function GenerateParametersJsonObject: TJSONArray;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    function GenerateJsonObject: TJSONObject;
    procedure Load(pJson: TJSONObject);

    /// <summary>
    /// Adds an operation with the given HTTP method to the path and returns it.
    /// </summary>
    function AddOperation(const pOperation: TSwagPathTypeOperation): TSwagPathOperation;

    /// <summary>
    /// Adds an operation with an HTTP method that has no fixed field in the Path Item Object, for example LINK,
    /// and returns it. Available in OpenAPI 3 only.
    /// </summary>
    function AddAdditionalOperation(const pMethod: string): TSwagPathOperation;

    /// <summary>
    /// A relative path to an individual endpoint. The field name MUST begin with a slash.
    /// The path is appended to the basePath in order to construct the full URL. Path templating is allowed.
    /// For webhooks, reusable path items and callbacks, it is the name or the runtime expression of the path item.
    /// </summary>
    property Uri: string read fUri write fUri;

    /// <summary>
    /// Describes a single API operation on a path.
    /// </summary>
    property Operations: TObjectList<TSwagPathOperation> read fOperations;

    property Parameters: TObjectList<TSwagRequestParameter> read fParameters;

    /// <summary>
    /// An optional, string summary, intended to apply to all operations in this path. Available in OpenAPI 3 only.
    /// </summary>
    property Summary: string read fSummary write fSummary;

    /// <summary>
    /// An optional, string description, intended to apply to all operations in this path.
    /// CommonMark syntax MAY be used for rich text representation. Available in OpenAPI 3 only.
    /// </summary>
    property Description: string read fDescription write fDescription;

    /// <summary>
    /// An alternative server array to service all operations in this path. Available in OpenAPI 3 only.
    /// </summary>
    property Servers: TObjectList<TSwagServer> read fServers;

    /// <summary>
    /// Allows for a referenced definition of this path item, for example #/components/pathItems/Health.
    /// </summary>
    property Ref: string read fRef write fRef;

    /// <summary>
    /// The operations whose HTTP methods have no fixed field in the Path Item Object. The AdditionalMethod of each
    /// operation is the key of the map. Available in OpenAPI 3 only.
    /// </summary>
    property AdditionalOperations: TObjectList<TSwagPathOperation> read fAdditionalOperations;

    /// <summary>
    /// The Specification Extensions of the path item.
    /// </summary>
    property Extensions: TSwagExtensions read fExtensions;
  end;

implementation

uses
  System.SysUtils,
  System.RegularExpressions,
  Swag.Common.Consts,
  Swag.Common.Types.Helpers;

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
  c_SwagPathRef = '$ref';

{ TSwagCallback }

constructor TSwagCallback.Create;
begin
  inherited Create;
  fPathItems := TObjectList<TSwagPath>.Create;
  fExtensions := TSwagExtensions.Create;
end;

destructor TSwagCallback.Destroy;
begin
  FreeAndNil(fPathItems);
  FreeAndNil(fExtensions);
  inherited Destroy;
end;

function TSwagCallback.AddPathItem(const pExpression: string): TSwagPath;
begin
  Result := TSwagPath.Create;
  Result.Uri := pExpression;
  fPathItems.Add(Result);
end;

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
  fRequestBody := TSwagRequestBody.Create;
  fServers := TObjectList<TSwagServer>.Create;
  fSecurityRequirements := TObjectList<TSwagSecurityRequirement>.Create;
  fCallbacks := TObjectList<TSwagCallback>.Create;
  fExtensions := TSwagExtensions.Create;
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
  FreeAndNil(fRequestBody);
  FreeAndNil(fServers);
  FreeAndNil(fSecurityRequirements);
  FreeAndNil(fCallbacks);
  FreeAndNil(fExtensions);

  inherited Destroy;
end;

function TSwagPathOperation.AddSecurityRequirement: TSwagSecurityRequirement;
begin
  Result := TSwagSecurityRequirement.Create;
  fSecurityRequirements.Add(Result);
end;

function TSwagPathOperation.AddCallback(const pName: string): TSwagCallback;
begin
  Result := TSwagCallback.Create;
  Result.Name := pName;
  fCallbacks.Add(Result);
end;

function TSwagPathOperation.GetOperationToString: string;
begin
  Result := c_SwagPathOperationHttpVerbs[fOperation];
end;

function TSwagPathOperation.HasPayloadParameter: Boolean;
var
  vIndex: Integer;
begin
  Result := False;
  for vIndex := 0 to fParameters.Count - 1 do
    if fParameters.Items[vIndex].InLocation in [rpiBody, rpiFormData] then
      Exit(True);
end;

function TSwagPathOperation.UseRequestBodyAsParameter: Boolean;
begin
  Result := (not fRequestBody.IsEmpty) and (not HasPayloadParameter);
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
    if not (fParameters.Items[vIndex].InLocation in [rpiCookie, rpiQueryString]) then
      Result.Add(fParameters.Items[vIndex].GenerateJsonObject);
  if UseRequestBodyAsParameter then
  begin
    if fRequestBody.IsFormContent then
      fRequestBody.GenerateFormDataParameters(Result)
    else
      Result.Add(fRequestBody.GenerateBodyParameterJsonObject);
  end;
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

function TSwagPathOperation.GenerateSecurityRequirementsJsonArray: TJSONArray;
begin
  Result := nil;
  if fSecurityRequirements.Count > 0 then
    Result := TSwagSecurityRequirement.GenerateJsonArray(fSecurityRequirements)
  else if fSecurity.Count > 0 then
    Result := GenerateSecurityJsonArray
  else if fDisableSecurity then
    Result := TJSONArray.Create;
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
  vJsonSecurity: TJSONArray;
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
    vJsonObject.AddPair(c_SwagPathOperationConsumes, GenerateMimeTypesJsonArray(fConsumes))
  else if UseRequestBodyAsParameter and (fRequestBody.Content.Count > 0) then
    vJsonObject.AddPair(c_SwagPathOperationConsumes, fRequestBody.GenerateMediaTypesJsonArray);
  if (fProduces.Count > 0) then
    vJsonObject.AddPair(c_SwagPathOperationProduces, GenerateMimeTypesJsonArray(fProduces));
  if (fParameters.Count > 0) or UseRequestBodyAsParameter then
    vJsonObject.AddPair(c_SwagPathOperationParameters, GenerateParametersJsonArray);
  if (fResponses.Count > 0) then
    vJsonObject.AddPair(c_SwagPathOperationResponses, GenarateResponsesJsonObject);

  vJsonSecurity := GenerateSecurityRequirementsJsonArray;
  if Assigned(vJsonSecurity) then
    vJsonObject.AddPair(c_SwagPathOperationSecurity, vJsonSecurity);

  fExtensions.WriteTo(vJsonObject);
  Result := vJsonObject;
end;

{ TSwagPath }

constructor TSwagPath.Create;
begin
  inherited Create;
  fOperations := TObjectList<TSwagPathOperation>.Create;
  fParameters := TObjectList<TSwagRequestParameter>.Create;
  fServers := TObjectList<TSwagServer>.Create;
  fAdditionalOperations := TObjectList<TSwagPathOperation>.Create;
  fExtensions := TSwagExtensions.Create;
end;

destructor TSwagPath.Destroy;
begin
  FreeAndNil(fOperations);
  FreeAndNil(fParameters);
  FreeAndNil(fServers);
  FreeAndNil(fAdditionalOperations);
  FreeAndNil(fExtensions);
  inherited Destroy;
end;

function TSwagPath.AddOperation(const pOperation: TSwagPathTypeOperation): TSwagPathOperation;
begin
  Result := TSwagPathOperation.Create;
  Result.Operation := pOperation;
  fOperations.Add(Result);
end;

function TSwagPath.AddAdditionalOperation(const pMethod: string): TSwagPathOperation;
begin
  Result := TSwagPathOperation.Create;
  Result.AdditionalMethod := pMethod;
  fAdditionalOperations.Add(Result);
end;

function TSwagPath.GenerateParametersJsonObject: TJSONArray;
var
  vIndex: Integer;
begin
  Result := TJSONArray.Create;
  for vIndex := 0 to fParameters.Count - 1 do
  begin
    if not (fParameters[vIndex].InLocation in [rpiCookie, rpiQueryString]) then
      Result.Add(fParameters[vIndex].GenerateJsonObject);
  end;
end;

function TSwagPath.GenerateJsonObject: TJSONObject;
var
  vIndex: integer;
begin
  Result := TJsonObject.Create;
  if not fRef.IsEmpty then
    Result.AddPair(c_SwagPathRef, fRef);
  if fParameters.Count > 0 then
    Result.AddPair('parameters', GenerateParametersJsonObject);
  for vIndex := 0 to fOperations.Count -1 do
    if fOperations.Items[vIndex].Operation <> ohvQuery then
      Result.AddPair(fOperations.Items[vIndex].OperationToString, fOperations.Items[vIndex].GenerateJsonObject);
  fExtensions.WriteTo(Result);
end;

procedure TSwagPath.Load(pJson: TJSONObject);
var
  vIndex: Integer;
  vOperation: TSwagPathOperation;
  vOperationJson: TJSONObject;
  vOperationExternalDocs: TJSONObject;
  vOperationName: string;
begin
  if not Assigned(pJson) then
    Exit;

  fExtensions.ReadFrom(pJson);
  if pJson.Values[c_SwagPathRef] is TJSONString then
    fRef := pJson.Values[c_SwagPathRef].Value;

  for vIndex := 0 to pJson.Count - 1 do
  begin
    vOperationName := pJson.Pairs[vIndex].JsonString.Value;
    if vOperationName = 'parameters' then
    begin
      LoadPathScopedParameters(pJson.Pairs[vIndex].JsonValue as TJSONArray);
      continue;
    end;
    if TRegEx.IsMatch(vOperationName, '(^x-)') then
    begin
      // This is an extension value - ignore
      continue;
    end;
    if not (pJson.Pairs[vIndex].JsonValue is TJSONObject) then
    begin
      // This shouldn't happen - although it may be valid in openapi documents
      continue;
    end;

    vOperation := TSwagPathOperation.Create;
    vOperationJson := pJson.Pairs[vIndex].JsonValue as TJSONObject;
    if Assigned(vOperationJson.Values['description']) then
      vOperation.Description := vOperationJson.Values['description'].Value;
    if Assigned(vOperationJson.Values['summary']) then
      vOperation.Summary := vOperationJson.Values['summary'].Value;

    vOperationExternalDocs := vOperationJson.Values['externalDocs'] as TJSONObject;
    if Assigned(vOperationExternalDocs) then
    begin
      if Assigned(vOperationExternalDocs.Values['url']) then
        vOperation.ExternalDocs.Url := vOperationExternalDocs.Values['url'].Value;
      if Assigned(vOperationExternalDocs.Values['description'])then
        vOperation.ExternalDocs.Description := vOperationExternalDocs.Values['description'].Value;
    end;

    vOperation.Operation.ToType(pJson.Pairs[vIndex].JsonString.Value);

    if Assigned(vOperationJson.Values['operationId']) then
      vOperation.OperationId := vOperationJson.Values['operationId'].Value;

    if Assigned(vOperationJson.Values['deprecated']) then
      vOperation.Deprecated := (vOperationJson.Values['deprecated'] as TJSONBool).AsBoolean;

    LoadTags(vOperation, vOperationJson.Values['tags'] as TJSONArray);
    LoadProduces(vOperation, vOperationJson.Values['produces'] as TJSONArray);
    LoadConsumes(vOperation, vOperationJson.Values['consumes'] as TJSONArray);
    LoadOperationScopedParameters(vOperation, vOperationJson.Values['parameters'] as TJSONArray);
    LoadResponse(vOperation, vOperationJson.Values['responses'] as TJSONObject);
    LoadSecurity(vOperation, vOperationJson.Values['security'] as TJSONArray);
    vOperation.Extensions.ReadFrom(vOperationJson);

    fOperations.Add(vOperation);
  end;
end;

procedure TSwagPath.LoadSecurity(pOperation: TSwagPathOperation; pJsonSecurity: TJSONArray);
begin
  if not Assigned(pJsonSecurity) then
    Exit;

  if pJsonSecurity.Count = 0 then
    pOperation.DisableSecurity := True
  else
    TSwagSecurityRequirement.LoadArray(pJsonSecurity, pOperation.SecurityRequirements);
end;

procedure TSwagPath.LoadTags(pOperation: TSwagPathOperation; pJsonTags: TJSONArray);
var
  vIndex: Integer;
  vTag: string;
begin
  if not Assigned(pJsonTags) then
    Exit;

  for vIndex := 0 to pJsonTags.Count - 1 do
  begin
    vTag := pJsonTags.Items[vIndex].Value;
    pOperation.Tags.Add(vTag);
  end;
end;

procedure TSwagPath.LoadPathScopedParameters(pJsonRequestParams: TJSONArray);
var
  vIndex: Integer;
  vRequestParam: TSwagRequestParameter;
begin
  if not Assigned(pJsonRequestParams) then
    Exit;

  for vIndex := 0 to pJsonRequestParams.Count - 1 do
  begin
    vRequestParam := TSwagRequestParameter.Create;
    vRequestParam.Load(pJsonRequestParams.Items[vIndex] as TJSONObject);
    Parameters.Add(vRequestParam);
  end;
end;

procedure TSwagPath.LoadOperationScopedParameters(pOperation: TSwagPathOperation; pJsonRequestParams: TJSONArray);
var
  vIndex: Integer;
  vRequestParam: TSwagRequestParameter;
begin
  if not Assigned(pJsonRequestParams) then
    Exit;

  for vIndex := 0 to pJsonRequestParams.Count - 1 do
  begin
    vRequestParam := TSwagRequestParameter.Create;
    vRequestParam.Load(pJsonRequestParams.Items[vIndex] as TJSONObject);
    pOperation.Parameters.Add(vRequestParam);
  end;
end;

procedure TSwagPath.LoadProduces(pOperation: TSwagPathOperation; pJsonProduces: TJSONArray);
var
  vIndex: Integer;
  vProduces: string;
begin
  if not Assigned(pJsonProduces) then
    Exit;

  for vIndex := 0 to pJsonProduces.Count - 1 do
  begin
    vProduces := pJsonProduces.Items[vIndex].Value;
    pOperation.Produces.Add(vProduces);
  end;
end;

procedure TSwagPath.LoadConsumes(pOperation: TSwagPathOperation; pJsonConsumes: TJSONArray);
var
  vIndex: Integer;
  vConsumes: string;
begin
  if not Assigned(pJsonConsumes) then
    Exit;

  for vIndex := 0 to pJsonConsumes.Count - 1 do
  begin
    vConsumes := pJsonConsumes.Items[vIndex].Value;
    pOperation.Consumes.Add(vConsumes);
  end;
end;

procedure TSwagPath.LoadResponse(pOperation: TSwagPathOperation; pJsonResponse: TJSONObject);
var
  vIndex: Integer;
  vResponse: TSwagResponse;
begin
  if not Assigned(pJsonResponse) then
    Exit;

  for vIndex := 0 to pJsonResponse.Count - 1 do
  begin
    vResponse := TSwagResponse.Create;
    vResponse.StatusCode := pJsonResponse.Pairs[vIndex].JsonString.Value;
    vResponse.Load(pJsonResponse.Pairs[vIndex].JsonValue as TJSONObject);
    pOperation.Responses.Add(vResponse.StatusCode, vResponse);
  end;
end;

end.
