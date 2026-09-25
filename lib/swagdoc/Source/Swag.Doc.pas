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

unit Swag.Doc;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.JSON,
  Swag.Common.Types,
  Swag.Doc.Tags,
  Swag.Doc.Info,
  Swag.Doc.Extensions,
  Swag.Doc.Example,
  Swag.Doc.Link,
  Swag.Doc.SecurityDefinition,
  Swag.Doc.SecurityRequirement,
  Swag.Doc.Server,
  Swag.Doc.Path.Operation,
  Swag.Doc.Path.Operation.Content,
  Swag.Doc.Path.Operation.Response,
  Swag.Doc.Path.Operation.RequestParameter,
  Swag.Doc.Path.Operation.RequestBody,
  Swag.Doc.Definition;

type
  ESwagErrorLoadSwaggerJsonFile = class(Exception);

  /// <summary>
  /// This is the root document object for the API specification.
  /// It combines what previously was the Resource Listing and API Declaration (version 1.2 and earlier) together into one document.
  /// The same object model is written as a Swagger 2.0 or as an OpenAPI 3 document according to the SpecVersion property.
  /// </summary>
  TSwagDoc = class(TObject)
  private
    fInfo: TSwagInfo;
    fTags: TObjectList<TSwagTag>;
    fConsumes: TList<TSwagMimeType>;
    fProduces: TList<TSwagMimeType>;
    fBasePath: string;
    fHost: string;
    fSchemes: TSwagTransferProtocolSchemes;
    fPaths: TObjectList<TSwagPath>;
    fDefinitions: TObjectList<TSwagDefinition>;
    fSecurityDefinitions: TObjectList<TSwagSecurityDefinition>;
    fExternalDocs: TSwagExternalDocs;
    fSwaggerJson: TJSONValue;
    fSwaggerFilesFolder: string;
    fSwaggerFileName: string;
    fParameters: TObjectList<TSwagRequestParameter>;
    fSpecVersion: TSwagVersion;
    fServers: TObjectList<TSwagServer>;
    fRequestBodies: TObjectList<TSwagRequestBody>;
    fWebhooks: TObjectList<TSwagPath>;
    fSelfUri: string;
    fJsonSchemaDialect: string;
    fSecurityRequirements: TObjectList<TSwagSecurityRequirement>;
    fDisableSecurity: Boolean;
    fGlobalSecurityFromDefinitions: Boolean;
    fWriteNullableExtension: Boolean;
    fResponses: TObjectList<TSwagResponse>;
    fHeaders: TObjectList<TSwagHeaders>;
    fExamples: TObjectList<TSwagExample>;
    fLinks: TObjectList<TSwagLink>;
    fCallbacks: TObjectList<TSwagCallback>;
    fPathItems: TObjectList<TSwagPath>;
    fMediaTypes: TObjectList<TSwagMediaType>;
    fExtensions: TSwagExtensions;
    function GetSwaggerVersion: string;
    function GetSwaggerFileName: string;
    procedure SetSwaggerFilesFolder(const Value: string);
    function GenerateTagsJsonArray: TJSONArray;
    procedure RemoveUnsupportedSecurityRequirements(pJson: TJSONObject);
  protected
    function GenerateSchemesJsonArray: TJSONArray;
    function GenerateSecurityDefinitionsJsonObject: TJSONObject;
    function GenerateConsumesJsonArray: TJSONArray;
    function GenerateProducesJsonArray: TJSONArray;
    function GeneratePathsJsonObject: TJSONObject;
    function GenerateDefinitionsJsonObject: TJSONObject;
    function GenerateParametersJsonObject: TJSONObject;
    function GenerateResponsesJsonObject: TJSONObject;
    function GenerateSwaggerJsonObject: TJSONObject;
    function GenerateOpenApiJsonObject: TJSONObject;

    function GenerateMimeTypesJsonArray(pMimeTypesList: TList<TSwagMimeType>): TJSONArray;
    function ReturnSwaggerFileName: string;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    /// <summary>
    /// Generates the document for the version defined by SpecVersion and keeps it in the SwaggerJson property.
    /// </summary>
    procedure GenerateSwaggerJson;

    /// <summary>
    /// Saves the generated document in the SwaggerFilesFolder using the SwaggerFileName.
    /// </summary>
    procedure SaveSwaggerJsonToFile;

    /// <summary>
    /// Loads a Swagger 2.0 or an OpenAPI 3 document. The SpecVersion property is set according to the file,
    /// so the document can be written back in the same version or converted to the other one.
    /// </summary>
    procedure LoadFromFile(const pFilename: string);

    /// <summary>
    /// Adds a server to the Servers list and returns it. Used by OpenAPI 3 documents.
    /// </summary>
    function AddServer(const pUrl: string; const pDescription: string = ''): TSwagServer;

    /// <summary>
    /// Adds a security requirement to the document and returns it, so its schemes and scopes can be defined.
    /// </summary>
    function AddSecurityRequirement: TSwagSecurityRequirement;

    property SwaggerFilesFolder: string read fSwaggerFilesFolder write SetSwaggerFilesFolder;

    /// <summary>
    /// The name of the file written by SaveSwaggerJsonToFile. When it is not defined, swagger.json is used
    /// for Swagger 2.0 and openapi.json for OpenAPI 3.
    /// </summary>
    property SwaggerFileName: string read GetSwaggerFileName write fSwaggerFileName;

    property SwaggerJson: TJSONValue read fSwaggerJson;

    /// <summary>
    /// The version of the specification used to generate the document. The default value is Swagger 2.0,
    /// so the applications written for the previous releases of the library keep producing the same document.
    /// </summary>
    property SpecVersion: TSwagVersion read fSpecVersion write fSpecVersion;

    /// <summary>
    /// Required. Specifies the specification version of the document: "2.0" for Swagger or the latest supported 3.x release for OpenAPI,
    /// according to the SpecVersion property.
    /// It can be used by the Swagger UI and other clients to interpret the API listing.
    /// </summary>
    property SwaggerVersion: string read GetSwaggerVersion;

    /// <summary>
    /// The self-assigned URI of the document, which also serves as its base URI for the resolution of relative
    /// references. Available in OpenAPI 3 only.
    /// </summary>
    property SelfUri: string read fSelfUri write fSelfUri;

    /// <summary>
    /// The default value for the $schema keyword within the Schema Objects of the document. This MUST be in the form
    /// of a URI. When it is not defined, the OpenAPI dialect is used. Available in OpenAPI 3 only.
    /// </summary>
    property JsonSchemaDialect: string read fJsonSchemaDialect write fJsonSchemaDialect;

    /// <summary>
    /// Required. Provides metadata about the API. The metadata can be used by the clients if needed.
    /// </summary>
    property Info: TSwagInfo read fInfo;

    /// <summary>
    /// The host (name or ip) serving the API. This MUST be the host only and does not include the scheme nor sub-paths.
    /// It MAY include a port.
    /// If the host is not included, the host serving the documentation is to be used (including the port).
    /// The host does not support path templating.
    /// In OpenAPI 3 the host, base path and schemes are translated to a server when the Servers list is empty.
    /// </summary>
    property Host: string read fHost write fHost;

    /// <summary>
    /// The base path on which the API is served, which is relative to the host.
    /// If it is not included, the API is served directly under the host. The value MUST start with a leading slash (/).
    /// The basePath does not support path templating.
    /// </summary>
    property BasePath: string read fBasePath write fBasePath;

    /// <summary>
    /// The transfer protocol of the API. Values MUST be from the list: "http", "https", "ws", "wss".
    /// If the schemes is not included, the default scheme to be used is the one used to access the Swagger definition itself.
    /// </summary>
    property Schemes: TSwagTransferProtocolSchemes read fSchemes write fSchemes;

    /// <summary>
    /// An array of Server Objects, which provide connectivity information to a target server. Used by OpenAPI 3
    /// documents, where it replaces the host, basePath and schemes fields. When the list is empty those fields
    /// are used to write the default server.
    /// </summary>
    property Servers: TObjectList<TSwagServer> read fServers;

    /// <summary>
    /// A list of MIME types the APIs can consume. This is global to all APIs but can be overridden on specific API calls.
    /// Value MUST be as described under Mime Types.
    /// In OpenAPI 3 the list defines the media types of the request bodies generated from the body and formData parameters.
    /// </summary>
    property Consumes: TList<TSwagMimeType> read fConsumes;

    /// <summary>
    /// A list of MIME types the APIs can produce. This is global to all APIs but can be overridden on specific API calls.
    /// Value MUST be as described under Mime Types.
    /// In OpenAPI 3 the list defines the media types of the responses that do not declare their own content.
    /// </summary>
    property Produces: TList<TSwagMimeType> read fProduces;

    /// <summary>
    /// Required. The available paths and operations for the API.
    /// </summary>
    property Paths: TObjectList<TSwagPath> read fPaths;

    /// <summary>
    /// An object to hold data types produced and consumed by operations.
    /// Written under definitions in Swagger 2.0 and under components/schemas in OpenAPI 3.
    /// </summary>
    property Definitions: TObjectList<TSwagDefinition> read fDefinitions;

    /// <summary>
    /// Security scheme definitions that can be used across the specification.
    /// Written under securityDefinitions in Swagger 2.0 and under components/securitySchemes in OpenAPI 3.
    /// </summary>
    property SecurityDefinitions: TObjectList<TSwagSecurityDefinition> read fSecurityDefinitions;

    /// <summary>
    /// The alternative security requirements of the whole API (logical OR). Each requirement lists the schemes that
    /// are all required (logical AND) with their scopes. When the list is empty, the security of the document is
    /// written according to the DisableSecurity and GlobalSecurityFromDefinitions properties.
    /// </summary>
    property SecurityRequirements: TObjectList<TSwagSecurityRequirement> read fSecurityRequirements;

    /// <summary>
    /// When True and no security requirement is declared, the security of the document is written as an empty array,
    /// so no security scheme is required by default.
    /// </summary>
    property DisableSecurity: Boolean read fDisableSecurity write fDisableSecurity;

    /// <summary>
    /// When True and no security requirement is declared, every security definition is written as an alternative
    /// requirement of the document, without scopes. The default value is False, so the security of the document is
    /// only written when it is declared, and the operations keep the security they declare themselves.
    /// </summary>
    property GlobalSecurityFromDefinitions: Boolean read fGlobalSecurityFromDefinitions
      write fGlobalSecurityFromDefinitions;

    /// <summary>
    /// When True, a schema field marked as nullable is written with the x-nullable extension in a Swagger 2.0
    /// document. The default value is False, because Swagger 2.0 has no keyword for nullable values. The x-nullable
    /// extension read from a Swagger 2.0 document is always kept, and OpenAPI 3 always writes the nullable type.
    /// </summary>
    property WriteNullableExtension: Boolean read fWriteNullableExtension write fWriteNullableExtension;

    /// <summary>
    /// An object to hold parameters that can be used across operations. This property does not define global
    /// parameters for all operations. Written under parameters in Swagger 2.0 and under components/parameters
    /// in OpenAPI 3, where the body and formData parameters are written under components/requestBodies.
    /// </summary>
    property Parameters: TObjectList<TSwagRequestParameter> read fParameters;

    /// <summary>
    /// An object to hold reusable request bodies. Written under components/requestBodies in OpenAPI 3 and as
    /// body parameters in a Swagger 2.0 document.
    /// </summary>
    property RequestBodies: TObjectList<TSwagRequestBody> read fRequestBodies;

    /// <summary>
    /// An object to hold responses that can be used across operations. The Name of each response is its key.
    /// Written under responses in Swagger 2.0 and under components/responses in OpenAPI 3.
    /// </summary>
    property Responses: TObjectList<TSwagResponse> read fResponses;

    /// <summary>
    /// An object to hold reusable headers. The Name of each header is its key. Available in OpenAPI 3 only.
    /// </summary>
    property Headers: TObjectList<TSwagHeaders> read fHeaders;

    /// <summary>
    /// An object to hold reusable examples. The Name of each example is its key. Available in OpenAPI 3 only.
    /// </summary>
    property Examples: TObjectList<TSwagExample> read fExamples;

    /// <summary>
    /// An object to hold reusable links. The Name of each link is its key. Available in OpenAPI 3 only.
    /// </summary>
    property Links: TObjectList<TSwagLink> read fLinks;

    /// <summary>
    /// An object to hold reusable callbacks. The Name of each callback is its key. Available in OpenAPI 3 only.
    /// </summary>
    property Callbacks: TObjectList<TSwagCallback> read fCallbacks;

    /// <summary>
    /// An object to hold reusable path items. The Uri of each path item is its key. Available in OpenAPI 3 only.
    /// </summary>
    property PathItems: TObjectList<TSwagPath> read fPathItems;

    /// <summary>
    /// An object to hold reusable media types. The Name of each media type is its key. Available in OpenAPI 3 only.
    /// </summary>
    property MediaTypes: TObjectList<TSwagMediaType> read fMediaTypes;

    /// <summary>
    /// The incoming webhooks that MAY be received as part of this API and that the API consumer MAY choose to implement.
    /// The Uri of each item is the unique name of the webhook and its operations describe the requests initiated
    /// by the API provider. Available in OpenAPI 3 only.
    /// </summary>
    property Webhooks: TObjectList<TSwagPath> read fWebhooks;

    property Tags: TObjectList<TSwagTag> read fTags;

    property ExternalDocs: TSwagExternalDocs read fExternalDocs;

    /// <summary>
    /// The Specification Extensions of the document, for example x-logo.
    /// </summary>
    property Extensions: TSwagExtensions read fExtensions;
  end;

const
  c_Swagger = 'swagger';
  c_OpenApi = 'openapi';
  c_SwagSelf = '$self';
  c_SwagJsonSchemaDialect = 'jsonSchemaDialect';
  c_SwagInfo = 'info';
  c_SwagHost = 'host';
  c_SwagBasePath = 'basePath';
  c_SwagServers = 'servers';
  c_SwagTags = 'tags';
  c_SwagSchemes = 'schemes';
  c_SwagSecurity = 'security';
  c_SwagSecurityDefinitions = 'securityDefinitions';
  c_SwagSecurityDefinitionsType = 'type';
  c_SwagConsumes = 'consumes';
  c_SwagProduces = 'produces';
  c_SwagPaths = 'paths';
  c_SwagWebhooks = 'webhooks';
  c_SwagDefinitions = 'definitions';
  c_SwagExternalDocs = 'externalDocs';
  c_SwagExternalDocsDescription = 'description';
  c_SwagExternalDocsUrl = 'url';
  c_SwagParameters = 'parameters';
  c_SwagResponses = 'responses';
  c_SwagComponents = 'components';
  c_SwagComponentsSchemas = 'schemas';
  c_SwagComponentsResponses = 'responses';
  c_SwagComponentsParameters = 'parameters';
  c_SwagComponentsExamples = 'examples';
  c_SwagComponentsRequestBodies = 'requestBodies';
  c_SwagComponentsHeaders = 'headers';
  c_SwagComponentsSecuritySchemes = 'securitySchemes';
  c_SwagComponentsLinks = 'links';
  c_SwagComponentsCallbacks = 'callbacks';
  c_SwagComponentsPathItems = 'pathItems';
  c_SwagComponentsMediaTypes = 'mediaTypes';

implementation

uses
  System.IOUtils,
  Json.Common.Helpers,
  Swag.Common.Consts,
  Swag.Common.Types.Helpers,
  Swag.Doc.JsonConverter,
  Swag.Doc.OpenApi.Generator,
  Swag.Doc.FileLoader;

{ TSwagDoc }

constructor TSwagDoc.Create;
begin
  inherited Create;

  fSpecVersion := svSwagger2;
  fInfo := TSwagInfo.Create;
  fTags := TObjectList<TSwagTag>.Create;
  fSecurityDefinitions := TObjectList<TSwagSecurityDefinition>.Create;
  fConsumes := TList<string>.Create;
  fProduces := TList<string>.Create;
  fPaths := TObjectList<TSwagPath>.Create;
  fDefinitions := TObjectList<TSwagDefinition>.Create;
  fExternalDocs := TSwagExternalDocs.Create;
  fParameters := TObjectList<TSwagRequestParameter>.Create;
  fServers := TObjectList<TSwagServer>.Create;
  fRequestBodies := TObjectList<TSwagRequestBody>.Create;
  fWebhooks := TObjectList<TSwagPath>.Create;
  fSecurityRequirements := TObjectList<TSwagSecurityRequirement>.Create;
  fResponses := TObjectList<TSwagResponse>.Create;
  fHeaders := TObjectList<TSwagHeaders>.Create;
  fExamples := TObjectList<TSwagExample>.Create;
  fLinks := TObjectList<TSwagLink>.Create;
  fCallbacks := TObjectList<TSwagCallback>.Create;
  fPathItems := TObjectList<TSwagPath>.Create;
  fMediaTypes := TObjectList<TSwagMediaType>.Create;
  fExtensions := TSwagExtensions.Create;
end;

destructor TSwagDoc.Destroy;
begin
  FreeAndNil(fConsumes);
  FreeAndNil(fProduces);
  FreeAndNil(fDefinitions);
  FreeAndNil(fPaths);
  FreeAndNil(fInfo);
  FreeAndNil(fTags);
  FreeAndNil(fSecurityDefinitions);
  FreeAndNil(fExternalDocs);
  FreeAndNil(fParameters);
  FreeAndNil(fServers);
  FreeAndNil(fRequestBodies);
  FreeAndNil(fWebhooks);
  FreeAndNil(fSecurityRequirements);
  FreeAndNil(fResponses);
  FreeAndNil(fHeaders);
  FreeAndNil(fExamples);
  FreeAndNil(fLinks);
  FreeAndNil(fCallbacks);
  FreeAndNil(fPathItems);
  FreeAndNil(fMediaTypes);
  FreeAndNil(fExtensions);

  if Assigned(fSwaggerJson) then
    FreeAndNil(fSwaggerJson);

  inherited Destroy;
end;

function TSwagDoc.AddServer(const pUrl: string; const pDescription: string): TSwagServer;
begin
  Result := TSwagServer.Create;
  Result.Url := pUrl;
  Result.Description := pDescription;
  fServers.Add(Result);
end;

function TSwagDoc.AddSecurityRequirement: TSwagSecurityRequirement;
begin
  Result := TSwagSecurityRequirement.Create;
  fSecurityRequirements.Add(Result);
end;

procedure TSwagDoc.SaveSwaggerJsonToFile;
var
  vJsonFile: TStringStream;
begin
  if not Assigned(fSwaggerJson) then
    Exit;

  if not System.SysUtils.DirectoryExists(fSwaggerFilesFolder) then
    System.SysUtils.ForceDirectories(fSwaggerFilesFolder);

  vJsonFile := TStringStream.Create(fSwaggerJson.Format);
  try
    vJsonFile.SaveToFile(ReturnSwaggerFileName);
  finally
    FreeAndNil(vJsonFile);
  end;
end;

function TSwagDoc.GenerateMimeTypesJsonArray(pMimeTypesList: TList<TSwagMimeType>): TJSONArray;
var
  vIndex: Integer;
begin
  Result := TJSONArray.Create;
  for vIndex := 0 to pMimeTypesList.Count -1 do
    Result.Add(pMimeTypesList.Items[vIndex]);
end;

function TSwagDoc.GenerateConsumesJsonArray: TJSONArray;
begin
  Result := GenerateMimeTypesJsonArray(fConsumes);
end;

function TSwagDoc.GenerateProducesJsonArray: TJSONArray;
begin
  Result := GenerateMimeTypesJsonArray(fProduces);
end;

function TSwagDoc.GenerateDefinitionsJsonObject: TJSONObject;
var
  vIndex: integer;
begin
  Result := TJsonObject.Create;
  for vIndex := 0 to fDefinitions.Count -1 do
    if Assigned(fDefinitions.Items[vIndex].JsonSchema) then
      Result.AddPair(fDefinitions.Items[vIndex].Name, fDefinitions.Items[vIndex].JsonSchema.Clone as TJSONObject);
end;

function TSwagDoc.GenerateParametersJsonObject: TJSONObject;
var
  vIndex: integer;
begin
  Result := TJsonObject.Create;
  for vIndex := 0 to fParameters.Count -1 do
    if not (fParameters.Items[vIndex].InLocation in [rpiCookie, rpiQueryString]) then
      Result.AddPair(fParameters.Items[vIndex].Name, fParameters.Items[vIndex].GenerateJsonObject);
  for vIndex := 0 to fRequestBodies.Count - 1 do
    Result.AddPair(fRequestBodies.Items[vIndex].Name, fRequestBodies.Items[vIndex].GenerateBodyParameterJsonObject);
end;

function TSwagDoc.GenerateResponsesJsonObject: TJSONObject;
var
  vResponse: TSwagResponse;
  vKey: string;
begin
  Result := TJSONObject.Create;
  for vResponse in fResponses do
  begin
    vKey := vResponse.Name;
    if vKey.IsEmpty then
      vKey := vResponse.StatusCode;
    Result.AddPair(vKey, vResponse.GenerateJsonObject);
  end;
end;

function TSwagDoc.GeneratePathsJsonObject: TJSONObject;
const
  c_SwagComponentsPathItemsRefPrefix = '#/components/pathItems/';

  function ResolvePath(pPath: TSwagPath): TSwagPath;
  var
    vPathItem: TSwagPath;
  begin
    Result := pPath;
    if not pPath.Ref.StartsWith(c_SwagComponentsPathItemsRefPrefix) then
      Exit;
    for vPathItem in fPathItems do
      if SameStr(c_SwagComponentsPathItemsRefPrefix + vPathItem.Uri, pPath.Ref) then
        Exit(vPathItem);
  end;

var
  vIndex: integer;
begin
  Result := TJsonObject.Create;
  for vIndex := 0 to fPaths.Count -1 do
    Result.AddPair(fPaths.Items[vIndex].Uri, ResolvePath(fPaths.Items[vIndex]).GenerateJsonObject);
end;

function TSwagDoc.GenerateTagsJsonArray: TJSONArray;
var
  vTag: TSwagTag;
begin
  Result := TJSONArray.Create;
  for vTag in fTags do
  begin
    Result.Add(vTag.GenerateJsonObject);
  end;
end;

function TSwagDoc.GenerateSchemesJsonArray: TJSONArray;
var
  vScheme: TSwagTransferProtocolScheme;
begin
  Result := TJSONArray.Create;
  for vScheme := Low(TSwagTransferProtocolScheme) to high(TSwagTransferProtocolScheme) do
  begin
    if vScheme in fSchemes then
      Result.Add(c_SwagTransferProtocolScheme[vScheme]);
  end;
end;

function TSwagDoc.GenerateSecurityDefinitionsJsonObject: TJSONObject;
var
  vIndex: integer;
  vJsonDefinition: TJSONObject;
begin
  Result := TJsonObject.Create;
  for vIndex := 0 to fSecurityDefinitions.Count -1 do
    if fSecurityDefinitions.Items[vIndex].SupportsVersion(svSwagger2) then
    begin
      vJsonDefinition := fSecurityDefinitions.Items[vIndex].GenerateJsonObject;
      fSecurityDefinitions.Items[vIndex].Extensions.WriteTo(vJsonDefinition);
      Result.AddPair(fSecurityDefinitions.Items[vIndex].SchemeName, vJsonDefinition);
    end;
end;

function TSwagDoc.GenerateSwaggerJsonObject: TJSONObject;
var
  vJsonObject, lSecurity: TJsonObject;
  I: integer;
  lArraySecurity: TJSONArray;
  vJsonExternalDocs: TJSONObject;
begin
  vJsonObject := TJsonObject.Create;

  vJsonObject.AddPair(c_Swagger, c_SwaggerVersion);
  vJsonObject.AddPair(c_SwagInfo, fInfo.GenerateJsonObject);

  if not fHost.IsEmpty then
    vJsonObject.AddPair(c_SwagHost, fHost);
  vJsonObject.AddPair(c_SwagBasePath, fBasePath);

  if (fTags.Count > 0) then
    vJsonObject.AddPair(c_SwagTags, GenerateTagsJsonArray);

  if (fSchemes <> []) then
    vJsonObject.AddPair(c_SwagSchemes, GenerateSchemesJsonArray);

  if (fConsumes.Count > 0) then
    vJsonObject.AddPair(c_SwagConsumes, GenerateConsumesJsonArray);

  if (fProduces.Count > 0) then
    vJsonObject.AddPair(c_SwagProduces, GenerateProducesJsonArray);

  if (fPaths.Count > 0) then
    vJsonObject.AddPair(c_SwagPaths, GeneratePathsJsonObject);

  if (fParameters.Count > 0) or (fRequestBodies.Count > 0) then
    vJsonObject.AddPair(c_SwagParameters, GenerateParametersJsonObject);

  if (fResponses.Count > 0) then
    vJsonObject.AddPair(c_SwagResponses, GenerateResponsesJsonObject);

  if (fSecurityDefinitions.Count > 0) then begin
    vJsonObject.AddPair(c_SwagSecurityDefinitions, GenerateSecurityDefinitionsJsonObject);

    if fSecurityRequirements.Count > 0 then
      vJsonObject.AddPair(c_SwagSecurity, TSwagSecurityRequirement.GenerateJsonArray(fSecurityRequirements))
    else if fDisableSecurity then
      vJsonObject.AddPair(c_SwagSecurity, TJSONArray.Create)
    else if fGlobalSecurityFromDefinitions then
    begin
      lArraySecurity := TJSONArray.Create;

      for I := 0 to Pred(fSecurityDefinitions.Count) do begin
        if not fSecurityDefinitions.Items[I].SupportsVersion(svSwagger2) then
          Continue;
        lSecurity := TJsonObject.Create;
        lSecurity.AddPair(fSecurityDefinitions.Items[I].SchemeName, TJSONArray.Create);

        lArraySecurity.AddElement(lSecurity);
      end;

      vJsonObject.AddPair(c_SwagSecurity, lArraySecurity);
    end;
  end
  else if fSecurityRequirements.Count > 0 then
    vJsonObject.AddPair(c_SwagSecurity, TSwagSecurityRequirement.GenerateJsonArray(fSecurityRequirements));

  if (fDefinitions.Count > 0) then
    vJsonObject.AddPair(c_SwagDefinitions, GenerateDefinitionsJsonObject);

  vJsonExternalDocs := fExternalDocs.GenerateJsonObject;
  if Assigned(vJsonExternalDocs) then
    vJsonObject.AddPair(c_SwagExternalDocs, vJsonExternalDocs);

  fExtensions.WriteTo(vJsonObject);

  Result := vJsonObject;
end;

function TSwagDoc.GenerateOpenApiJsonObject: TJSONObject;
var
  vGenerator: TSwagOpenApiGenerator;
begin
  vGenerator := TSwagOpenApiGenerator.Create(Self);
  try
    Result := vGenerator.Generate;
  finally
    vGenerator.Free;
  end;
end;

procedure TSwagDoc.GenerateSwaggerJson;
var
  vJsonObject: TJSONObject;
begin
  case fSpecVersion of
    svOpenApi3:
      vJsonObject := GenerateOpenApiJsonObject;
  else
    begin
      vJsonObject := GenerateSwaggerJsonObject;
      RemoveUnsupportedSecurityRequirements(vJsonObject);
      TSwagJsonConverter.Convert(vJsonObject, svSwagger2, fWriteNullableExtension);
    end;
  end;

  if Assigned(fSwaggerJson) then
    fSwaggerJson.Free;
  fSwaggerJson := vJsonObject;
end;

procedure TSwagDoc.RemoveUnsupportedSecurityRequirements(pJson: TJSONObject);
var
  vUnsupported: TStringList;

  procedure FilterSecurity(pOwner: TJSONObject);
  var
    vJsonSecurity: TJSONArray;
    vJsonRequirement: TJSONObject;
    vIndex: Integer;
    vPairIndex: Integer;
    vRemove: Boolean;
  begin
    if not (pOwner.Values[c_SwagSecurity] is TJSONArray) then
      Exit;

    vJsonSecurity := TJSONArray(pOwner.Values[c_SwagSecurity]);
    if vJsonSecurity.Count = 0 then
      Exit;

    for vIndex := vJsonSecurity.Count - 1 downto 0 do
    begin
      if not (vJsonSecurity.Items[vIndex] is TJSONObject) then
        Continue;

      vJsonRequirement := TJSONObject(vJsonSecurity.Items[vIndex]);
      vRemove := False;
      for vPairIndex := 0 to vJsonRequirement.Count - 1 do
        if vUnsupported.IndexOf(vJsonRequirement.Pairs[vPairIndex].JsonString.Value) >= 0 then
          vRemove := True;
      if vRemove then
        vJsonSecurity.Remove(vIndex).Free;
    end;

    if vJsonSecurity.Count = 0 then
      pOwner.RemovePair(c_SwagSecurity).Free;
  end;

var
  vSecurityDefinition: TSwagSecurityDefinition;
  vJsonPaths: TJSONObject;
  vJsonPath: TJSONObject;
  vPathIndex: Integer;
  vOperationIndex: Integer;
begin
  vUnsupported := TStringList.Create;
  try
    for vSecurityDefinition in fSecurityDefinitions do
      if not vSecurityDefinition.SupportsVersion(svSwagger2) then
        vUnsupported.Add(vSecurityDefinition.SchemeName);

    if vUnsupported.Count = 0 then
      Exit;

    FilterSecurity(pJson);

    if not (pJson.Values[c_SwagPaths] is TJSONObject) then
      Exit;

    vJsonPaths := TJSONObject(pJson.Values[c_SwagPaths]);
    for vPathIndex := 0 to vJsonPaths.Count - 1 do
    begin
      if not (vJsonPaths.Pairs[vPathIndex].JsonValue is TJSONObject) then
        Continue;

      vJsonPath := TJSONObject(vJsonPaths.Pairs[vPathIndex].JsonValue);
      for vOperationIndex := 0 to vJsonPath.Count - 1 do
        if vJsonPath.Pairs[vOperationIndex].JsonValue is TJSONObject then
          FilterSecurity(TJSONObject(vJsonPath.Pairs[vOperationIndex].JsonValue));
    end;
  finally
    vUnsupported.Free;
  end;
end;

function TSwagDoc.GetSwaggerFileName: string;
begin
  if fSwaggerFileName.IsEmpty then
    Result := c_SwagSpecFileName[fSpecVersion]
  else
    Result := fSwaggerFileName;
end;

function TSwagDoc.GetSwaggerVersion: string;
begin
  Result := c_SwagSpecVersion[fSpecVersion];
end;

procedure TSwagDoc.LoadFromFile(const pFilename: string);
var
  vFileLoader: TSwagFileLoader;
begin
  vFileLoader := TSwagFileLoader.Create(Self);
  try
    vFileLoader.Load(pFilename);
  finally
    vFileLoader.Free;
  end;
end;

function TSwagDoc.ReturnSwaggerFileName: string;
begin
  Result := fSwaggerFilesFolder + GetSwaggerFileName;
end;

procedure TSwagDoc.SetSwaggerFilesFolder(const Value: string);
begin
  fSwaggerFilesFolder := IncludeTrailingPathDelimiter(Trim(Value));
end;

end.
