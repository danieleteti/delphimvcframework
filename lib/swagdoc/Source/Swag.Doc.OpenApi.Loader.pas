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

unit Swag.Doc.OpenApi.Loader;

interface

uses
  System.Generics.Collections,
  System.JSON,
  Swag.Common.Types,
  Swag.Doc,
  Swag.Doc.Server,
  Swag.Doc.SecurityRequirement,
  Swag.Doc.Path.Operation,
  Swag.Doc.Path.Operation.RequestParameter,
  Swag.Doc.Path.Operation.Response;

type
  /// <summary>
  /// Reads an OpenAPI 3 document into a TSwagDoc object model.
  /// Besides the OpenAPI 3 objects, the host, base path and schemes properties are filled from the first server,
  /// replacing its variables by their default values, so the loaded document can also be written back as a
  /// Swagger 2.0 document.
  /// </summary>
  TSwagOpenApiLoader = class(TObject)
  strict private
    fSwagDoc: TSwagDoc;
    procedure LoadServers(pJsonServers: TJSONArray; pServers: TObjectList<TSwagServer>);
    procedure LoadHostFromServers;
    procedure LoadTags(pJsonTags: TJSONArray);
    procedure LoadPaths(pJsonPaths: TJSONObject; pPaths: TObjectList<TSwagPath>);
    procedure LoadPath(pPath: TSwagPath; pJsonPath: TJSONObject);
    procedure LoadOperation(pOperation: TSwagPathOperation; pJsonOperation: TJSONObject);
    procedure LoadCallbacks(pJsonCallbacks: TJSONObject; pCallbacks: TObjectList<TSwagCallback>);
    procedure LoadCallback(pCallback: TSwagCallback; pJsonCallback: TJSONObject);
    procedure LoadStrings(pStrings: TList<string>; pJsonStrings: TJSONArray);
    procedure LoadParameters(pParameters: TObjectList<TSwagRequestParameter>; pJsonParameters: TJSONArray);
    procedure LoadParameter(pParameter: TSwagRequestParameter; pJsonParameter: TJSONObject);
    procedure LoadParameterSchema(pParameter: TSwagRequestParameter; pJsonSchema: TJSONObject);
    procedure LoadResponses(pOperation: TSwagPathOperation; pJsonResponses: TJSONObject);
    procedure LoadResponse(pResponse: TSwagResponse; pJsonResponse: TJSONObject);
    function LoadSecurity(pJsonSecurity: TJSONArray; pRequirements: TObjectList<TSwagSecurityRequirement>): Boolean;
    procedure LoadComponents(pJsonComponents: TJSONObject);
    procedure LoadSchemas(pJsonSchemas: TJSONObject);
    procedure LoadComponentResponses(pJsonResponses: TJSONObject);
    procedure LoadComponentParameters(pJsonParameters: TJSONObject);
    procedure LoadRequestBodies(pJsonRequestBodies: TJSONObject);
    procedure LoadSecuritySchemes(pJsonSecuritySchemes: TJSONObject);
  public
    constructor Create(pSwagDocTarget: TSwagDoc); reintroduce;

    /// <summary>
    /// Fills the target document with the content of the OpenAPI 3 JSON object.
    /// </summary>
    procedure Load(pJson: TJSONObject);
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  Swag.Common.Consts,
  Swag.Common.Json,
  Swag.Common.Types.Helpers,
  Swag.Doc.Tags,
  Swag.Doc.Definition,
  Swag.Doc.Example,
  Swag.Doc.Link,
  Swag.Doc.Extensions,
  Swag.Doc.SecurityDefinition,
  Swag.Doc.SecurityDefinitionBasic,
  Swag.Doc.SecurityDefinitionApiKey,
  Swag.Doc.SecurityDefinitionOAuth2,
  Swag.Doc.SecurityDefinitionHttp,
  Swag.Doc.SecurityDefinitionOpenIdConnect,
  Swag.Doc.SecurityDefinitionMutualTls,
  Swag.Doc.Path.Operation.Content,
  Swag.Doc.Path.Operation.RequestBody;

const
  c_OpenApiRef = '$ref';
  c_OpenApiSummary = 'summary';
  c_OpenApiDescription = 'description';
  c_OpenApiServers = 'servers';
  c_OpenApiParameters = 'parameters';
  c_OpenApiTags = 'tags';
  c_OpenApiExternalDocs = 'externalDocs';
  c_OpenApiOperationId = 'operationId';
  c_OpenApiDeprecated = 'deprecated';
  c_OpenApiRequestBody = 'requestBody';
  c_OpenApiResponses = 'responses';
  c_OpenApiCallbacks = 'callbacks';
  c_OpenApiSecurity = 'security';
  c_OpenApiAdditionalOperations = 'additionalOperations';
  c_OpenApiStyle = 'style';
  c_OpenApiExplode = 'explode';
  c_OpenApiAllowReserved = 'allowReserved';
  c_OpenApiExample = 'example';
  c_OpenApiExamples = 'examples';
  c_OpenApiSchema = 'schema';
  c_OpenApiType = 'type';
  c_OpenApiFormat = 'format';
  c_OpenApiPattern = 'pattern';
  c_OpenApiDefault = 'default';
  c_OpenApiEnum = 'enum';
  c_OpenApiItems = 'items';
  c_OpenApiHeaders = 'headers';
  c_OpenApiContent = 'content';
  c_OpenApiLinks = 'links';
  c_OpenApiSchemeSeparator = '://';
  c_OpenApiProtocolRelativePrefix = '//';
  c_OpenApiPathSeparator = '/';
  c_OpenApiVariableStart = '{';
  c_OpenApiVariableEnd = '}';
  c_OpenApiTypeNull = 'null';

{ TSwagOpenApiLoader }

constructor TSwagOpenApiLoader.Create(pSwagDocTarget: TSwagDoc);
begin
  inherited Create;
  fSwagDoc := pSwagDocTarget;
end;

procedure TSwagOpenApiLoader.Load(pJson: TJSONObject);
var
  vJsonInfo: TJSONObject;
  vJsonExternalDocs: TJSONObject;
begin
  if not Assigned(pJson) then
    Exit;

  fSwagDoc.SelfUri := TSwagJson.ReadString(pJson, c_SwagSelf);
  vJsonInfo := TSwagJson.ReadObject(pJson, c_SwagInfo);
  if Assigned(vJsonInfo) then
    fSwagDoc.Info.Load(vJsonInfo);
  fSwagDoc.JsonSchemaDialect := TSwagJson.ReadString(pJson, c_SwagJsonSchemaDialect);
  LoadServers(TSwagJson.ReadArray(pJson, c_SwagServers), fSwagDoc.Servers);
  LoadHostFromServers;
  LoadTags(TSwagJson.ReadArray(pJson, c_SwagTags));
  LoadPaths(TSwagJson.ReadObject(pJson, c_SwagPaths), fSwagDoc.Paths);
  LoadPaths(TSwagJson.ReadObject(pJson, c_SwagWebhooks), fSwagDoc.Webhooks);
  LoadComponents(TSwagJson.ReadObject(pJson, c_SwagComponents));
  fSwagDoc.DisableSecurity := LoadSecurity(TSwagJson.ReadArray(pJson, c_SwagSecurity), fSwagDoc.SecurityRequirements);

  vJsonExternalDocs := TSwagJson.ReadObject(pJson, c_SwagExternalDocs);
  if Assigned(vJsonExternalDocs) then
    fSwagDoc.ExternalDocs.Load(vJsonExternalDocs);

  fSwagDoc.Extensions.ReadFrom(pJson);
end;

procedure TSwagOpenApiLoader.LoadServers(pJsonServers: TJSONArray; pServers: TObjectList<TSwagServer>);
var
  vIndex: Integer;
  vServer: TSwagServer;
begin
  if not Assigned(pJsonServers) then
    Exit;

  for vIndex := 0 to pJsonServers.Count - 1 do
    if pJsonServers.Items[vIndex] is TJSONObject then
    begin
      vServer := TSwagServer.Create;
      vServer.Load(TJSONObject(pJsonServers.Items[vIndex]));
      pServers.Add(vServer);
    end;
end;

procedure TSwagOpenApiLoader.LoadHostFromServers;
var
  vServer: TSwagServer;
  vVariable: TSwagServerVariable;
  vUrl: string;
  vRemainder: string;
  vScheme: TSwagTransferProtocolScheme;
  vSchemeEnd: Integer;
  vPathStart: Integer;
begin
  if fSwagDoc.Servers.Count = 0 then
    Exit;

  vServer := fSwagDoc.Servers[0];
  vUrl := vServer.Url;
  for vVariable in vServer.Variables do
    vUrl := vUrl.Replace(c_OpenApiVariableStart + vVariable.Name + c_OpenApiVariableEnd, vVariable.Default);

  vSchemeEnd := vUrl.IndexOf(c_OpenApiSchemeSeparator);
  if vSchemeEnd > 0 then
  begin
    vScheme.ToType(vUrl.Substring(0, vSchemeEnd));
    if vScheme <> tpsNotDefined then
      fSwagDoc.Schemes := fSwagDoc.Schemes + [vScheme];
    vRemainder := vUrl.Substring(vSchemeEnd + Length(c_OpenApiSchemeSeparator));
  end
  else if vUrl.StartsWith(c_OpenApiProtocolRelativePrefix) then
    vRemainder := vUrl.Substring(Length(c_OpenApiProtocolRelativePrefix))
  else
  begin
    fSwagDoc.BasePath := vUrl;
    Exit;
  end;

  vPathStart := vRemainder.IndexOf(c_OpenApiPathSeparator);
  if vPathStart < 0 then
  begin
    fSwagDoc.Host := vRemainder;
    Exit;
  end;

  fSwagDoc.Host := vRemainder.Substring(0, vPathStart);
  fSwagDoc.BasePath := vRemainder.Substring(vPathStart);
  if (fSwagDoc.BasePath.Length > 1) and fSwagDoc.BasePath.EndsWith(c_OpenApiPathSeparator) then
    fSwagDoc.BasePath := fSwagDoc.BasePath.Substring(0, fSwagDoc.BasePath.Length - 1);
  if fSwagDoc.BasePath = c_OpenApiPathSeparator then
    fSwagDoc.BasePath := EmptyStr;
end;

procedure TSwagOpenApiLoader.LoadTags(pJsonTags: TJSONArray);
var
  vIndex: Integer;
  vTag: TSwagTag;
begin
  if not Assigned(pJsonTags) then
    Exit;

  for vIndex := 0 to pJsonTags.Count - 1 do
    if pJsonTags.Items[vIndex] is TJSONObject then
    begin
      vTag := TSwagTag.Create;
      vTag.Load(TJSONObject(pJsonTags.Items[vIndex]));
      fSwagDoc.Tags.Add(vTag);
    end;
end;

procedure TSwagOpenApiLoader.LoadPaths(pJsonPaths: TJSONObject; pPaths: TObjectList<TSwagPath>);
var
  vIndex: Integer;
  vName: string;
  vPath: TSwagPath;
begin
  if not Assigned(pJsonPaths) then
    Exit;

  for vIndex := 0 to pJsonPaths.Count - 1 do
  begin
    vName := pJsonPaths.Pairs[vIndex].JsonString.Value;
    if TSwagExtensions.IsExtensionName(vName) or not (pJsonPaths.Pairs[vIndex].JsonValue is TJSONObject) then
      Continue;

    vPath := TSwagPath.Create;
    vPath.Uri := vName;
    LoadPath(vPath, TJSONObject(pJsonPaths.Pairs[vIndex].JsonValue));
    pPaths.Add(vPath);
  end;
end;

procedure TSwagOpenApiLoader.LoadPath(pPath: TSwagPath; pJsonPath: TJSONObject);
var
  vIndex: Integer;
  vName: string;
  vOperationType: TSwagPathTypeOperation;
  vOperation: TSwagPathOperation;
  vJsonAdditionalOperations: TJSONObject;
begin
  pPath.Ref := TSwagJson.ReadString(pJsonPath, c_OpenApiRef);
  pPath.Summary := TSwagJson.ReadString(pJsonPath, c_OpenApiSummary);
  pPath.Description := TSwagJson.ReadString(pJsonPath, c_OpenApiDescription);
  LoadServers(TSwagJson.ReadArray(pJsonPath, c_OpenApiServers), pPath.Servers);
  LoadParameters(pPath.Parameters, TSwagJson.ReadArray(pJsonPath, c_OpenApiParameters));

  for vIndex := 0 to pJsonPath.Count - 1 do
  begin
    vName := pJsonPath.Pairs[vIndex].JsonString.Value;
    if TSwagExtensions.IsExtensionName(vName) or not (pJsonPath.Pairs[vIndex].JsonValue is TJSONObject) then
      Continue;

    vOperationType.ToType(vName);
    if vOperationType = ohvNotDefined then
      Continue;

    vOperation := pPath.AddOperation(vOperationType);
    LoadOperation(vOperation, TJSONObject(pJsonPath.Pairs[vIndex].JsonValue));
  end;

  vJsonAdditionalOperations := TSwagJson.ReadObject(pJsonPath, c_OpenApiAdditionalOperations);
  if Assigned(vJsonAdditionalOperations) then
    for vIndex := 0 to vJsonAdditionalOperations.Count - 1 do
      if vJsonAdditionalOperations.Pairs[vIndex].JsonValue is TJSONObject then
      begin
        vOperation := pPath.AddAdditionalOperation(vJsonAdditionalOperations.Pairs[vIndex].JsonString.Value);
        LoadOperation(vOperation, TJSONObject(vJsonAdditionalOperations.Pairs[vIndex].JsonValue));
      end;

  pPath.Extensions.ReadFrom(pJsonPath);
end;

procedure TSwagOpenApiLoader.LoadOperation(pOperation: TSwagPathOperation; pJsonOperation: TJSONObject);
var
  vJsonExternalDocs: TJSONObject;
begin
  LoadStrings(pOperation.Tags, TSwagJson.ReadArray(pJsonOperation, c_OpenApiTags));
  pOperation.Summary := TSwagJson.ReadString(pJsonOperation, c_OpenApiSummary);
  pOperation.Description := TSwagJson.ReadString(pJsonOperation, c_OpenApiDescription);

  vJsonExternalDocs := TSwagJson.ReadObject(pJsonOperation, c_OpenApiExternalDocs);
  if Assigned(vJsonExternalDocs) then
    pOperation.ExternalDocs.Load(vJsonExternalDocs);

  pOperation.OperationId := TSwagJson.ReadString(pJsonOperation, c_OpenApiOperationId);
  pOperation.Deprecated := TSwagJson.ReadBoolean(pJsonOperation, c_OpenApiDeprecated);
  LoadParameters(pOperation.Parameters, TSwagJson.ReadArray(pJsonOperation, c_OpenApiParameters));
  pOperation.RequestBody.Load(TSwagJson.ReadObject(pJsonOperation, c_OpenApiRequestBody));
  LoadResponses(pOperation, TSwagJson.ReadObject(pJsonOperation, c_OpenApiResponses));
  LoadCallbacks(TSwagJson.ReadObject(pJsonOperation, c_OpenApiCallbacks), pOperation.Callbacks);
  pOperation.DisableSecurity := LoadSecurity(TSwagJson.ReadArray(pJsonOperation, c_OpenApiSecurity),
    pOperation.SecurityRequirements);
  LoadServers(TSwagJson.ReadArray(pJsonOperation, c_OpenApiServers), pOperation.Servers);
  pOperation.Extensions.ReadFrom(pJsonOperation);
end;

procedure TSwagOpenApiLoader.LoadCallbacks(pJsonCallbacks: TJSONObject; pCallbacks: TObjectList<TSwagCallback>);
var
  vIndex: Integer;
  vCallback: TSwagCallback;
begin
  if not Assigned(pJsonCallbacks) then
    Exit;

  for vIndex := 0 to pJsonCallbacks.Count - 1 do
  begin
    if not (pJsonCallbacks.Pairs[vIndex].JsonValue is TJSONObject) then
      Continue;

    vCallback := TSwagCallback.Create;
    vCallback.Name := pJsonCallbacks.Pairs[vIndex].JsonString.Value;
    LoadCallback(vCallback, TJSONObject(pJsonCallbacks.Pairs[vIndex].JsonValue));
    pCallbacks.Add(vCallback);
  end;
end;

procedure TSwagOpenApiLoader.LoadCallback(pCallback: TSwagCallback; pJsonCallback: TJSONObject);
var
  vIndex: Integer;
  vName: string;
begin
  pCallback.Ref := TSwagJson.ReadString(pJsonCallback, c_OpenApiRef);
  if not pCallback.Ref.IsEmpty then
    Exit;

  for vIndex := 0 to pJsonCallback.Count - 1 do
  begin
    vName := pJsonCallback.Pairs[vIndex].JsonString.Value;
    if TSwagExtensions.IsExtensionName(vName) or not (pJsonCallback.Pairs[vIndex].JsonValue is TJSONObject) then
      Continue;

    LoadPath(pCallback.AddPathItem(vName), TJSONObject(pJsonCallback.Pairs[vIndex].JsonValue));
  end;

  pCallback.Extensions.ReadFrom(pJsonCallback);
end;

procedure TSwagOpenApiLoader.LoadStrings(pStrings: TList<string>; pJsonStrings: TJSONArray);
var
  vIndex: Integer;
begin
  if not Assigned(pJsonStrings) then
    Exit;

  for vIndex := 0 to pJsonStrings.Count - 1 do
    pStrings.Add(pJsonStrings.Items[vIndex].Value);
end;

procedure TSwagOpenApiLoader.LoadParameters(pParameters: TObjectList<TSwagRequestParameter>; pJsonParameters: TJSONArray);
var
  vIndex: Integer;
  vParameter: TSwagRequestParameter;
begin
  if not Assigned(pJsonParameters) then
    Exit;

  for vIndex := 0 to pJsonParameters.Count - 1 do
    if pJsonParameters.Items[vIndex] is TJSONObject then
    begin
      vParameter := TSwagRequestParameter.Create;
      LoadParameter(vParameter, TJSONObject(pJsonParameters.Items[vIndex]));
      pParameters.Add(vParameter);
    end;
end;

procedure TSwagOpenApiLoader.LoadParameter(pParameter: TSwagRequestParameter; pJsonParameter: TJSONObject);
var
  vStyle: TSwagRequestParameterStyle;
  vJsonContent: TJSONObject;
  vJsonSchema: TJSONObject;
begin
  pParameter.Load(pJsonParameter);
  if not pParameter.Ref.IsEmpty then
  begin
    pParameter.Description := TSwagJson.ReadString(pJsonParameter, c_OpenApiDescription);
    Exit;
  end;

  pParameter.Deprecated := TSwagJson.ReadBoolean(pJsonParameter, c_OpenApiDeprecated);
  vStyle.ToType(TSwagJson.ReadString(pJsonParameter, c_OpenApiStyle));
  pParameter.Style := vStyle;
  pParameter.Explode := TSwagJson.ReadBoolean(pJsonParameter, c_OpenApiExplode);
  pParameter.AllowReserved := TSwagJson.ReadBoolean(pJsonParameter, c_OpenApiAllowReserved);
  pParameter.Example := TSwagJson.CloneValue(pJsonParameter, c_OpenApiExample);
  TSwagExample.LoadMap(TSwagJson.ReadObject(pJsonParameter, c_OpenApiExamples), pParameter.Examples);

  vJsonSchema := TSwagJson.ReadObject(pJsonParameter, c_OpenApiSchema);
  vJsonContent := TSwagJson.ReadObject(pJsonParameter, c_OpenApiContent);
  if Assigned(vJsonContent) then
  begin
    TSwagMediaType.LoadMap(vJsonContent, pParameter.Content, False);
    if (not Assigned(vJsonSchema)) and (vJsonContent.Count > 0) then
      vJsonSchema := TSwagJson.ReadObject(TSwagJson.AsObject(vJsonContent.Pairs[0].JsonValue), c_OpenApiSchema);
  end;

  LoadParameterSchema(pParameter, vJsonSchema);
end;

procedure TSwagOpenApiLoader.LoadParameterSchema(pParameter: TSwagRequestParameter; pJsonSchema: TJSONObject);
var
  vTypeParameter: TSwagTypeParameter;
  vTypeName: string;
  vJsonTypes: TJSONArray;
  vJsonEnum: TJSONArray;
  vIndex: Integer;
begin
  if not Assigned(pJsonSchema) then
    Exit;

  vTypeName := TSwagJson.ReadString(pJsonSchema, c_OpenApiType);
  vJsonTypes := TSwagJson.ReadArray(pJsonSchema, c_OpenApiType);
  if Assigned(vJsonTypes) then
    for vIndex := 0 to vJsonTypes.Count - 1 do
      if vJsonTypes.Items[vIndex].Value <> c_OpenApiTypeNull then
      begin
        vTypeName := vJsonTypes.Items[vIndex].Value;
        Break;
      end;

  vTypeParameter.ToType(vTypeName);
  pParameter.TypeParameter := vTypeParameter;
  pParameter.Format := TSwagJson.ReadString(pJsonSchema, c_OpenApiFormat);
  pParameter.Pattern := TSwagJson.ReadString(pJsonSchema, c_OpenApiPattern);
  pParameter.Default := TSwagJson.ReadString(pJsonSchema, c_OpenApiDefault);

  vJsonEnum := TSwagJson.ReadArray(pJsonSchema, c_OpenApiEnum);
  if Assigned(vJsonEnum) then
    for vIndex := 0 to vJsonEnum.Count - 1 do
      if not (vJsonEnum.Items[vIndex] is TJSONNull) then
        pParameter.Enum.Add(vJsonEnum.Items[vIndex].Value);

  if pJsonSchema.Values[c_OpenApiItems] is TJSONObject then
    pParameter.Items := pJsonSchema.Values[c_OpenApiItems].Clone as TJSONObject;
end;

procedure TSwagOpenApiLoader.LoadResponses(pOperation: TSwagPathOperation; pJsonResponses: TJSONObject);
var
  vIndex: Integer;
  vName: string;
  vResponse: TSwagResponse;
begin
  if not Assigned(pJsonResponses) then
    Exit;

  for vIndex := 0 to pJsonResponses.Count - 1 do
  begin
    vName := pJsonResponses.Pairs[vIndex].JsonString.Value;
    if TSwagExtensions.IsExtensionName(vName) or not (pJsonResponses.Pairs[vIndex].JsonValue is TJSONObject) then
      Continue;

    vResponse := TSwagResponse.Create;
    vResponse.StatusCode := vName;
    LoadResponse(vResponse, TJSONObject(pJsonResponses.Pairs[vIndex].JsonValue));
    pOperation.Responses.Add(vResponse.StatusCode, vResponse);
  end;
end;

procedure TSwagOpenApiLoader.LoadResponse(pResponse: TSwagResponse; pJsonResponse: TJSONObject);
begin
  pResponse.Ref := TSwagJson.ReadString(pJsonResponse, c_OpenApiRef);
  pResponse.Summary := TSwagJson.ReadString(pJsonResponse, c_OpenApiSummary);
  pResponse.Description := TSwagJson.ReadString(pJsonResponse, c_OpenApiDescription);
  if not pResponse.Ref.IsEmpty then
    Exit;

  TSwagHeaders.LoadMap(TSwagJson.ReadObject(pJsonResponse, c_OpenApiHeaders), pResponse.Headers, svOpenApi3);
  TSwagMediaType.LoadMap(TSwagJson.ReadObject(pJsonResponse, c_OpenApiContent), pResponse.Content, False);
  TSwagLink.LoadMap(TSwagJson.ReadObject(pJsonResponse, c_OpenApiLinks), pResponse.Links);
  pResponse.Extensions.ReadFrom(pJsonResponse);
end;

function TSwagOpenApiLoader.LoadSecurity(pJsonSecurity: TJSONArray;
  pRequirements: TObjectList<TSwagSecurityRequirement>): Boolean;
begin
  Result := False;
  if not Assigned(pJsonSecurity) then
    Exit;

  if pJsonSecurity.Count = 0 then
    Exit(True);

  TSwagSecurityRequirement.LoadArray(pJsonSecurity, pRequirements);
end;

procedure TSwagOpenApiLoader.LoadComponents(pJsonComponents: TJSONObject);
begin
  if not Assigned(pJsonComponents) then
    Exit;

  LoadSchemas(TSwagJson.ReadObject(pJsonComponents, c_SwagComponentsSchemas));
  LoadComponentResponses(TSwagJson.ReadObject(pJsonComponents, c_SwagComponentsResponses));
  LoadComponentParameters(TSwagJson.ReadObject(pJsonComponents, c_SwagComponentsParameters));
  TSwagExample.LoadMap(TSwagJson.ReadObject(pJsonComponents, c_SwagComponentsExamples), fSwagDoc.Examples);
  LoadRequestBodies(TSwagJson.ReadObject(pJsonComponents, c_SwagComponentsRequestBodies));
  TSwagHeaders.LoadMap(TSwagJson.ReadObject(pJsonComponents, c_SwagComponentsHeaders), fSwagDoc.Headers, svOpenApi3);
  LoadSecuritySchemes(TSwagJson.ReadObject(pJsonComponents, c_SwagComponentsSecuritySchemes));
  TSwagLink.LoadMap(TSwagJson.ReadObject(pJsonComponents, c_SwagComponentsLinks), fSwagDoc.Links);
  LoadCallbacks(TSwagJson.ReadObject(pJsonComponents, c_SwagComponentsCallbacks), fSwagDoc.Callbacks);
  LoadPaths(TSwagJson.ReadObject(pJsonComponents, c_SwagComponentsPathItems), fSwagDoc.PathItems);
  TSwagMediaType.LoadMap(TSwagJson.ReadObject(pJsonComponents, c_SwagComponentsMediaTypes), fSwagDoc.MediaTypes, True);
end;

procedure TSwagOpenApiLoader.LoadSchemas(pJsonSchemas: TJSONObject);
var
  vIndex: Integer;
  vDefinition: TSwagDefinition;
begin
  if not Assigned(pJsonSchemas) then
    Exit;

  for vIndex := 0 to pJsonSchemas.Count - 1 do
  begin
    if not (pJsonSchemas.Pairs[vIndex].JsonValue is TJSONObject) then
      Continue;

    vDefinition := TSwagDefinition.Create;
    vDefinition.Name := pJsonSchemas.Pairs[vIndex].JsonString.Value;
    vDefinition.JsonSchema := pJsonSchemas.Pairs[vIndex].JsonValue.Clone as TJSONObject;
    fSwagDoc.Definitions.Add(vDefinition);
  end;
end;

procedure TSwagOpenApiLoader.LoadComponentResponses(pJsonResponses: TJSONObject);
var
  vIndex: Integer;
  vResponse: TSwagResponse;
begin
  if not Assigned(pJsonResponses) then
    Exit;

  for vIndex := 0 to pJsonResponses.Count - 1 do
  begin
    if not (pJsonResponses.Pairs[vIndex].JsonValue is TJSONObject) then
      Continue;

    vResponse := TSwagResponse.Create;
    vResponse.Name := pJsonResponses.Pairs[vIndex].JsonString.Value;
    LoadResponse(vResponse, TJSONObject(pJsonResponses.Pairs[vIndex].JsonValue));
    fSwagDoc.Responses.Add(vResponse);
  end;
end;

procedure TSwagOpenApiLoader.LoadComponentParameters(pJsonParameters: TJSONObject);
var
  vIndex: Integer;
  vParameter: TSwagRequestParameter;
begin
  if not Assigned(pJsonParameters) then
    Exit;

  for vIndex := 0 to pJsonParameters.Count - 1 do
  begin
    if not (pJsonParameters.Pairs[vIndex].JsonValue is TJSONObject) then
      Continue;

    vParameter := TSwagRequestParameter.Create;
    LoadParameter(vParameter, TJSONObject(pJsonParameters.Pairs[vIndex].JsonValue));
    vParameter.Name := pJsonParameters.Pairs[vIndex].JsonString.Value;
    fSwagDoc.Parameters.Add(vParameter);
  end;
end;

procedure TSwagOpenApiLoader.LoadRequestBodies(pJsonRequestBodies: TJSONObject);
var
  vIndex: Integer;
  vRequestBody: TSwagRequestBody;
begin
  if not Assigned(pJsonRequestBodies) then
    Exit;

  for vIndex := 0 to pJsonRequestBodies.Count - 1 do
  begin
    if not (pJsonRequestBodies.Pairs[vIndex].JsonValue is TJSONObject) then
      Continue;

    vRequestBody := TSwagRequestBody.Create;
    vRequestBody.Name := pJsonRequestBodies.Pairs[vIndex].JsonString.Value;
    vRequestBody.Load(TJSONObject(pJsonRequestBodies.Pairs[vIndex].JsonValue));
    fSwagDoc.RequestBodies.Add(vRequestBody);
  end;
end;

procedure TSwagOpenApiLoader.LoadSecuritySchemes(pJsonSecuritySchemes: TJSONObject);
var
  vIndex: Integer;
  vJsonSecurityScheme: TJSONObject;
  vSecurityType: TSwagSecurityDefinitionType;
  vSecurityClass: TPersistentClass;
  vSecurityDefinition: TSwagSecurityDefinition;
begin
  if not Assigned(pJsonSecuritySchemes) then
    Exit;

  for vIndex := 0 to pJsonSecuritySchemes.Count - 1 do
  begin
    vJsonSecurityScheme := TSwagJson.AsObject(pJsonSecuritySchemes.Pairs[vIndex].JsonValue);
    if not Assigned(vJsonSecurityScheme) then
      Continue;

    vSecurityType.ToType(TSwagJson.ReadString(vJsonSecurityScheme, c_SwagSecurityDefinitionsType));
    vSecurityClass := TSwagGetClassSecurityDefinition.Find(vSecurityType);
    if not Assigned(vSecurityClass) then
      Continue;

    vSecurityDefinition := TSwagSecurityDefinitionClass(vSecurityClass).Create;
    vSecurityDefinition.SchemeName := pJsonSecuritySchemes.Pairs[vIndex].JsonString.Value;
    vSecurityDefinition.Extensions.ReadFrom(vJsonSecurityScheme);
    vSecurityDefinition.Load(vJsonSecurityScheme, svOpenApi3);
    vSecurityDefinition.Deprecated := TSwagJson.ReadBoolean(vJsonSecurityScheme, c_OpenApiDeprecated);
    fSwagDoc.SecurityDefinitions.Add(vSecurityDefinition);
  end;
end;

end.
