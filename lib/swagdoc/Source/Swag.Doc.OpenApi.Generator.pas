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

unit Swag.Doc.OpenApi.Generator;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.JSON,
  Swag.Common.Types,
  Swag.Doc,
  Swag.Doc.Definition,
  Swag.Doc.Server,
  Swag.Doc.Path.Operation,
  Swag.Doc.Path.Operation.Content,
  Swag.Doc.Path.Operation.RequestParameter,
  Swag.Doc.Path.Operation.RequestBody,
  Swag.Doc.Path.Operation.Response;

type
  /// <summary>
  /// Writes the OpenAPI 3 document of a TSwagDoc object model.
  /// The Swagger 2.0 fields that no longer exist in OpenAPI 3 (host, basePath, schemes, consumes, produces,
  /// body and formData parameters) are translated to their OpenAPI 3 equivalents, so an object model built
  /// for Swagger 2.0 produces a valid OpenAPI 3 document without any change in the application code.
  /// </summary>
  TSwagOpenApiGenerator = class(TObject)
  strict private
    fSwagDoc: TSwagDoc;
    function ExtractRefName(const pRef: string): string;
    function FindDocParameter(const pName: string): TSwagRequestParameter;
    function FindDocRequestBody(const pName: string): TSwagRequestBody;
    function IsPayloadParameter(pParameter: TSwagRequestParameter): Boolean;
    function IsRequestBodyReference(pParameter: TSwagRequestParameter): Boolean;
    function FindBodyParameter(pParameters: TObjectList<TSwagRequestParameter>): TSwagRequestParameter;
    procedure CollectFormParameters(pParameters: TObjectList<TSwagRequestParameter>;
      pFormParameters: TList<TSwagRequestParameter>);
    function ResolveMimeTypes(pMimeTypes, pDefaultMimeTypes: TList<TSwagMimeType>): TArray<TSwagMimeType>;
    function ResolveFormMimeTypes(const pMimeTypes: TArray<TSwagMimeType>; const pHasFile: Boolean): TArray<TSwagMimeType>;
    procedure AddServerUrl(pJsonServers: TJSONArray; const pUrl: string);
    procedure AddJsonObject(pJsonOwner: TJSONObject; const pName: string; pJsonValue: TJSONObject);
    function GenerateServersJsonArray(pServers: TObjectList<TSwagServer>): TJSONArray;
    function GenerateServersFromHostJsonArray: TJSONArray;
    function GenerateTagsJsonArray: TJSONArray;
    function GenerateStringsJsonArray(pStrings: TList<string>): TJSONArray;
    function GenerateDocSecurityJsonArray: TJSONArray;
    function GeneratePathsJsonObject(pPaths: TObjectList<TSwagPath>): TJSONObject;
    function GeneratePathJsonObject(pPath: TSwagPath): TJSONObject;
    function GenerateOperationJsonObject(pPath: TSwagPath; pOperation: TSwagPathOperation): TJSONObject;
    function GenerateCallbacksJsonObject(pCallbacks: TObjectList<TSwagCallback>): TJSONObject;
    function GenerateCallbackJsonObject(pCallback: TSwagCallback): TJSONObject;
    function GenerateParametersJsonArray(pParameters: TObjectList<TSwagRequestParameter>): TJSONArray;
    function GenerateParameterJsonObject(pParameter: TSwagRequestParameter): TJSONObject;
    function GenerateParameterSchemaJsonObject(pParameter: TSwagRequestParameter): TJSONObject;
    function GenerateParameterValue(pParameter: TSwagRequestParameter; const pValue: string): TJSONValue;
    function GenerateSchemaJsonObject(pSchema: TSwagDefinition): TJSONObject;
    function GenerateRequestBodyJsonObject(pPath: TSwagPath; pOperation: TSwagPathOperation): TJSONObject;
    function GenerateBodyParameterRequestBodyJsonObject(pParameter: TSwagRequestParameter;
      const pMimeTypes: TArray<TSwagMimeType>): TJSONObject;
    function GenerateFormParametersRequestBodyJsonObject(pParameters: TList<TSwagRequestParameter>;
      const pMimeTypes: TArray<TSwagMimeType>): TJSONObject;
    function GenerateFormSchemaJsonObject(pParameters: TList<TSwagRequestParameter>): TJSONObject;
    function GenerateResponsesJsonObject(pOperation: TSwagPathOperation): TJSONObject;
    function GenerateResponseJsonObject(pResponse: TSwagResponse; const pMimeTypes: TArray<TSwagMimeType>): TJSONObject;
    function GenerateExamplesJsonObject(pExamples: TObjectDictionary<TSwagJsonExampleDescription, TJSONObject>): TJSONObject;
    function GenerateComponentsJsonObject: TJSONObject;
    function GenerateSchemasJsonObject: TJSONObject;
    function GenerateComponentResponsesJsonObject: TJSONObject;
    function GenerateComponentParametersJsonObject: TJSONObject;
    function GenerateComponentRequestBodiesJsonObject: TJSONObject;
    function GenerateSecuritySchemesJsonObject: TJSONObject;
  public
    constructor Create(pSwagDoc: TSwagDoc); reintroduce;

    /// <summary>
    /// Generates the OpenAPI 3 document. The caller owns the returned object.
    /// </summary>
    function Generate: TJSONObject;
  end;

implementation

uses
  Swag.Common.Consts,
  Swag.Doc.Tags,
  Swag.Doc.Example,
  Swag.Doc.Link,
  Swag.Doc.SecurityDefinition,
  Swag.Doc.SecurityRequirement,
  Swag.Doc.JsonConverter;

const
  c_OpenApiRef = '$ref';
  c_OpenApiUrl = 'url';
  c_OpenApiSummary = 'summary';
  c_OpenApiDescription = 'description';
  c_OpenApiTags = 'tags';
  c_OpenApiExternalDocs = 'externalDocs';
  c_OpenApiOperationId = 'operationId';
  c_OpenApiDeprecated = 'deprecated';
  c_OpenApiParameters = 'parameters';
  c_OpenApiRequestBody = 'requestBody';
  c_OpenApiResponses = 'responses';
  c_OpenApiCallbacks = 'callbacks';
  c_OpenApiSecurity = 'security';
  c_OpenApiServers = 'servers';
  c_OpenApiAdditionalOperations = 'additionalOperations';
  c_OpenApiName = 'name';
  c_OpenApiIn = 'in';
  c_OpenApiRequired = 'required';
  c_OpenApiAllowEmptyValue = 'allowEmptyValue';
  c_OpenApiStyle = 'style';
  c_OpenApiExplode = 'explode';
  c_OpenApiAllowReserved = 'allowReserved';
  c_OpenApiSchema = 'schema';
  c_OpenApiType = 'type';
  c_OpenApiFormat = 'format';
  c_OpenApiPattern = 'pattern';
  c_OpenApiDefault = 'default';
  c_OpenApiEnum = 'enum';
  c_OpenApiItems = 'items';
  c_OpenApiProperties = 'properties';
  c_OpenApiContent = 'content';
  c_OpenApiHeaders = 'headers';
  c_OpenApiLinks = 'links';
  c_OpenApiExample = 'example';
  c_OpenApiExamples = 'examples';
  c_OpenApiExampleValue = 'value';
  c_OpenApiTypeObject = 'object';
  c_OpenApiTypeString = 'string';
  c_OpenApiFormatBinary = 'binary';
  c_OpenApiBooleanTrue = 'true';
  c_OpenApiBooleanFalse = 'false';
  c_OpenApiRequestBodiesRefPrefix = '#/components/requestBodies/';
  c_OpenApiProtocolRelativePrefix = '//';
  c_OpenApiSchemeSeparator = '://';
  c_OpenApiRefSeparator = '/';

{ TSwagOpenApiGenerator }

constructor TSwagOpenApiGenerator.Create(pSwagDoc: TSwagDoc);
begin
  inherited Create;
  fSwagDoc := pSwagDoc;
end;

function TSwagOpenApiGenerator.Generate: TJSONObject;
var
  vJsonServers: TJSONArray;
  vJsonSecurity: TJSONArray;
  vJsonExternalDocs: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair(c_OpenApi, c_OpenApiVersion);
  if not fSwagDoc.SelfUri.IsEmpty then
    Result.AddPair(c_SwagSelf, fSwagDoc.SelfUri);
  Result.AddPair(c_SwagInfo, fSwagDoc.Info.GenerateJsonObject(svOpenApi3));
  if not fSwagDoc.JsonSchemaDialect.IsEmpty then
    Result.AddPair(c_SwagJsonSchemaDialect, fSwagDoc.JsonSchemaDialect);

  if fSwagDoc.Servers.Count > 0 then
    vJsonServers := GenerateServersJsonArray(fSwagDoc.Servers)
  else
    vJsonServers := GenerateServersFromHostJsonArray;
  if Assigned(vJsonServers) then
    Result.AddPair(c_SwagServers, vJsonServers);

  if fSwagDoc.Tags.Count > 0 then
    Result.AddPair(c_SwagTags, GenerateTagsJsonArray);

  Result.AddPair(c_SwagPaths, GeneratePathsJsonObject(fSwagDoc.Paths));

  if fSwagDoc.Webhooks.Count > 0 then
    Result.AddPair(c_SwagWebhooks, GeneratePathsJsonObject(fSwagDoc.Webhooks));

  AddJsonObject(Result, c_SwagComponents, GenerateComponentsJsonObject);

  vJsonSecurity := GenerateDocSecurityJsonArray;
  if Assigned(vJsonSecurity) then
    Result.AddPair(c_SwagSecurity, vJsonSecurity);

  vJsonExternalDocs := fSwagDoc.ExternalDocs.GenerateJsonObject;
  if Assigned(vJsonExternalDocs) then
    Result.AddPair(c_SwagExternalDocs, vJsonExternalDocs);

  fSwagDoc.Extensions.WriteTo(Result);

  TSwagJsonConverter.Convert(Result, svOpenApi3);
end;

function TSwagOpenApiGenerator.ExtractRefName(const pRef: string): string;
var
  vIndex: Integer;
begin
  vIndex := pRef.LastIndexOf(c_OpenApiRefSeparator);
  if vIndex < 0 then
    Result := pRef
  else
    Result := pRef.Substring(vIndex + 1);
end;

function TSwagOpenApiGenerator.FindDocParameter(const pName: string): TSwagRequestParameter;
var
  vParameter: TSwagRequestParameter;
begin
  Result := nil;
  for vParameter in fSwagDoc.Parameters do
    if SameStr(vParameter.Name, pName) then
      Exit(vParameter);
end;

function TSwagOpenApiGenerator.FindDocRequestBody(const pName: string): TSwagRequestBody;
var
  vRequestBody: TSwagRequestBody;
begin
  Result := nil;
  for vRequestBody in fSwagDoc.RequestBodies do
    if SameStr(vRequestBody.Name, pName) then
      Exit(vRequestBody);
end;

function TSwagOpenApiGenerator.IsPayloadParameter(pParameter: TSwagRequestParameter): Boolean;
begin
  Result := pParameter.Ref.IsEmpty and (pParameter.InLocation in [rpiBody, rpiFormData]);
end;

function TSwagOpenApiGenerator.IsRequestBodyReference(pParameter: TSwagRequestParameter): Boolean;
var
  vName: string;
  vDocParameter: TSwagRequestParameter;
begin
  Result := False;
  if pParameter.Ref.IsEmpty then
    Exit;

  vName := ExtractRefName(pParameter.Ref);
  if pParameter.Ref.StartsWith(c_OpenApiRequestBodiesRefPrefix) or Assigned(FindDocRequestBody(vName)) then
    Exit(True);

  vDocParameter := FindDocParameter(vName);
  Result := Assigned(vDocParameter) and (vDocParameter.InLocation in [rpiBody, rpiFormData]);
end;

function TSwagOpenApiGenerator.FindBodyParameter(pParameters: TObjectList<TSwagRequestParameter>): TSwagRequestParameter;
var
  vParameter: TSwagRequestParameter;
begin
  Result := nil;
  for vParameter in pParameters do
    if (vParameter.Ref.IsEmpty and (vParameter.InLocation = rpiBody)) or IsRequestBodyReference(vParameter) then
      Exit(vParameter);
end;

procedure TSwagOpenApiGenerator.CollectFormParameters(pParameters: TObjectList<TSwagRequestParameter>;
  pFormParameters: TList<TSwagRequestParameter>);
var
  vParameter: TSwagRequestParameter;
  vCollected: TSwagRequestParameter;
  vFound: Boolean;
begin
  for vParameter in pParameters do
  begin
    if not (vParameter.Ref.IsEmpty and (vParameter.InLocation = rpiFormData)) then
      Continue;

    vFound := False;
    for vCollected in pFormParameters do
      if SameStr(vCollected.Name, vParameter.Name) then
      begin
        vFound := True;
        Break;
      end;

    if not vFound then
      pFormParameters.Add(vParameter);
  end;
end;

function TSwagOpenApiGenerator.ResolveMimeTypes(pMimeTypes, pDefaultMimeTypes: TList<TSwagMimeType>): TArray<TSwagMimeType>;
begin
  if pMimeTypes.Count > 0 then
    Result := pMimeTypes.ToArray
  else if pDefaultMimeTypes.Count > 0 then
    Result := pDefaultMimeTypes.ToArray
  else
    Result := [c_SwagMimeTypeJson];
end;

function TSwagOpenApiGenerator.ResolveFormMimeTypes(const pMimeTypes: TArray<TSwagMimeType>;
  const pHasFile: Boolean): TArray<TSwagMimeType>;
var
  vMimeType: TSwagMimeType;
  vHasMultipart: Boolean;
begin
  Result := [];
  vHasMultipart := False;
  for vMimeType in pMimeTypes do
    if SameText(vMimeType, c_SwagMimeTypeFormUrlEncoded) or SameText(vMimeType, c_SwagMimeTypeMultipartFormData) then
    begin
      Result := Result + [vMimeType];
      vHasMultipart := vHasMultipart or SameText(vMimeType, c_SwagMimeTypeMultipartFormData);
    end;

  if pHasFile and (not vHasMultipart) then
    Result := [c_SwagMimeTypeMultipartFormData]
  else if Length(Result) = 0 then
    Result := [c_SwagMimeTypeFormUrlEncoded];
end;

procedure TSwagOpenApiGenerator.AddServerUrl(pJsonServers: TJSONArray; const pUrl: string);
var
  vJsonServer: TJSONObject;
begin
  vJsonServer := TJSONObject.Create;
  vJsonServer.AddPair(c_OpenApiUrl, pUrl);
  pJsonServers.Add(vJsonServer);
end;

procedure TSwagOpenApiGenerator.AddJsonObject(pJsonOwner: TJSONObject; const pName: string; pJsonValue: TJSONObject);
begin
  if pJsonValue.Count > 0 then
    pJsonOwner.AddPair(pName, pJsonValue)
  else
    pJsonValue.Free;
end;

function TSwagOpenApiGenerator.GenerateServersJsonArray(pServers: TObjectList<TSwagServer>): TJSONArray;
var
  vServer: TSwagServer;
begin
  Result := TJSONArray.Create;
  for vServer in pServers do
    Result.Add(vServer.GenerateJsonObject);
end;

function TSwagOpenApiGenerator.GenerateServersFromHostJsonArray: TJSONArray;
var
  vScheme: TSwagTransferProtocolScheme;
  vHasScheme: Boolean;
begin
  Result := nil;
  if fSwagDoc.Host.IsEmpty and fSwagDoc.BasePath.IsEmpty then
    Exit;

  Result := TJSONArray.Create;
  if fSwagDoc.Host.IsEmpty then
  begin
    AddServerUrl(Result, fSwagDoc.BasePath);
    Exit;
  end;

  vHasScheme := False;
  for vScheme := Low(TSwagTransferProtocolScheme) to High(TSwagTransferProtocolScheme) do
    if (vScheme <> tpsNotDefined) and (vScheme in fSwagDoc.Schemes) then
    begin
      AddServerUrl(Result, c_SwagTransferProtocolScheme[vScheme] + c_OpenApiSchemeSeparator + fSwagDoc.Host + fSwagDoc.BasePath);
      vHasScheme := True;
    end;

  if not vHasScheme then
    AddServerUrl(Result, c_OpenApiProtocolRelativePrefix + fSwagDoc.Host + fSwagDoc.BasePath);
end;

function TSwagOpenApiGenerator.GenerateTagsJsonArray: TJSONArray;
var
  vTag: TSwagTag;
begin
  Result := TJSONArray.Create;
  for vTag in fSwagDoc.Tags do
    Result.Add(vTag.GenerateJsonObject(svOpenApi3));
end;

function TSwagOpenApiGenerator.GenerateStringsJsonArray(pStrings: TList<string>): TJSONArray;
var
  vString: string;
begin
  Result := TJSONArray.Create;
  for vString in pStrings do
    Result.Add(vString);
end;

function TSwagOpenApiGenerator.GenerateDocSecurityJsonArray: TJSONArray;
var
  vSecurityDefinition: TSwagSecurityDefinition;
  vJsonRequirement: TJSONObject;
begin
  if fSwagDoc.SecurityRequirements.Count > 0 then
    Exit(TSwagSecurityRequirement.GenerateJsonArray(fSwagDoc.SecurityRequirements));

  if fSwagDoc.DisableSecurity then
    Exit(TJSONArray.Create);

  Result := nil;
  if fSwagDoc.SecurityDefinitions.Count = 0 then
    Exit;

  Result := TJSONArray.Create;
  for vSecurityDefinition in fSwagDoc.SecurityDefinitions do
  begin
    vJsonRequirement := TJSONObject.Create;
    vJsonRequirement.AddPair(vSecurityDefinition.SchemeName, TJSONArray.Create);
    Result.Add(vJsonRequirement);
  end;
end;

function TSwagOpenApiGenerator.GeneratePathsJsonObject(pPaths: TObjectList<TSwagPath>): TJSONObject;
var
  vPath: TSwagPath;
begin
  Result := TJSONObject.Create;
  for vPath in pPaths do
    Result.AddPair(vPath.Uri, GeneratePathJsonObject(vPath));
end;

function TSwagOpenApiGenerator.GeneratePathJsonObject(pPath: TSwagPath): TJSONObject;
var
  vJsonParameters: TJSONArray;
  vJsonAdditionalOperations: TJSONObject;
  vOperation: TSwagPathOperation;
begin
  Result := TJSONObject.Create;
  if not pPath.Ref.IsEmpty then
    Result.AddPair(c_OpenApiRef, pPath.Ref);
  if not pPath.Summary.IsEmpty then
    Result.AddPair(c_OpenApiSummary, pPath.Summary);
  if not pPath.Description.IsEmpty then
    Result.AddPair(c_OpenApiDescription, pPath.Description);
  if pPath.Servers.Count > 0 then
    Result.AddPair(c_OpenApiServers, GenerateServersJsonArray(pPath.Servers));

  vJsonParameters := GenerateParametersJsonArray(pPath.Parameters);
  if vJsonParameters.Count > 0 then
    Result.AddPair(c_OpenApiParameters, vJsonParameters)
  else
    vJsonParameters.Free;

  for vOperation in pPath.Operations do
    if vOperation.Operation <> ohvNotDefined then
      Result.AddPair(vOperation.OperationToString, GenerateOperationJsonObject(pPath, vOperation));

  if pPath.AdditionalOperations.Count > 0 then
  begin
    vJsonAdditionalOperations := TJSONObject.Create;
    for vOperation in pPath.AdditionalOperations do
      vJsonAdditionalOperations.AddPair(vOperation.AdditionalMethod, GenerateOperationJsonObject(pPath, vOperation));
    Result.AddPair(c_OpenApiAdditionalOperations, vJsonAdditionalOperations);
  end;

  pPath.Extensions.WriteTo(Result);
end;

function TSwagOpenApiGenerator.GenerateOperationJsonObject(pPath: TSwagPath; pOperation: TSwagPathOperation): TJSONObject;
var
  vJsonParameters: TJSONArray;
  vJsonSecurity: TJSONArray;
  vJsonRequestBody: TJSONObject;
  vJsonExternalDocs: TJSONObject;
begin
  Result := TJSONObject.Create;
  if pOperation.Tags.Count > 0 then
    Result.AddPair(c_OpenApiTags, GenerateStringsJsonArray(pOperation.Tags));
  if not pOperation.Summary.IsEmpty then
    Result.AddPair(c_OpenApiSummary, pOperation.Summary);
  if not pOperation.Description.IsEmpty then
    Result.AddPair(c_OpenApiDescription, pOperation.Description);

  vJsonExternalDocs := pOperation.ExternalDocs.GenerateJsonObject;
  if Assigned(vJsonExternalDocs) then
    Result.AddPair(c_OpenApiExternalDocs, vJsonExternalDocs);

  if not pOperation.OperationId.IsEmpty then
    Result.AddPair(c_OpenApiOperationId, pOperation.OperationId);
  if pOperation.Deprecated then
    Result.AddPair(c_OpenApiDeprecated, TJSONBool.Create(True));

  vJsonParameters := GenerateParametersJsonArray(pOperation.Parameters);
  if vJsonParameters.Count > 0 then
    Result.AddPair(c_OpenApiParameters, vJsonParameters)
  else
    vJsonParameters.Free;

  vJsonRequestBody := GenerateRequestBodyJsonObject(pPath, pOperation);
  if Assigned(vJsonRequestBody) then
    Result.AddPair(c_OpenApiRequestBody, vJsonRequestBody);

  Result.AddPair(c_OpenApiResponses, GenerateResponsesJsonObject(pOperation));

  if pOperation.Callbacks.Count > 0 then
    Result.AddPair(c_OpenApiCallbacks, GenerateCallbacksJsonObject(pOperation.Callbacks));

  vJsonSecurity := pOperation.GenerateSecurityRequirementsJsonArray;
  if Assigned(vJsonSecurity) then
    Result.AddPair(c_OpenApiSecurity, vJsonSecurity);

  if pOperation.Servers.Count > 0 then
    Result.AddPair(c_OpenApiServers, GenerateServersJsonArray(pOperation.Servers));

  pOperation.Extensions.WriteTo(Result);
end;

function TSwagOpenApiGenerator.GenerateCallbacksJsonObject(pCallbacks: TObjectList<TSwagCallback>): TJSONObject;
var
  vCallback: TSwagCallback;
begin
  Result := TJSONObject.Create;
  for vCallback in pCallbacks do
    Result.AddPair(vCallback.Name, GenerateCallbackJsonObject(vCallback));
end;

function TSwagOpenApiGenerator.GenerateCallbackJsonObject(pCallback: TSwagCallback): TJSONObject;
var
  vPathItem: TSwagPath;
begin
  Result := TJSONObject.Create;
  if not pCallback.Ref.IsEmpty then
  begin
    Result.AddPair(c_OpenApiRef, pCallback.Ref);
    Exit;
  end;

  for vPathItem in pCallback.PathItems do
    Result.AddPair(vPathItem.Uri, GeneratePathJsonObject(vPathItem));

  pCallback.Extensions.WriteTo(Result);
end;

function TSwagOpenApiGenerator.GenerateParametersJsonArray(pParameters: TObjectList<TSwagRequestParameter>): TJSONArray;
var
  vParameter: TSwagRequestParameter;
begin
  Result := TJSONArray.Create;
  for vParameter in pParameters do
  begin
    if IsPayloadParameter(vParameter) or IsRequestBodyReference(vParameter) then
      Continue;
    Result.Add(GenerateParameterJsonObject(vParameter));
  end;
end;

function TSwagOpenApiGenerator.GenerateParameterJsonObject(pParameter: TSwagRequestParameter): TJSONObject;
var
  vJsonContent: TJSONObject;
  vJsonMediaType: TJSONObject;
begin
  Result := TJSONObject.Create;
  if not pParameter.Ref.IsEmpty then
  begin
    Result.AddPair(c_OpenApiRef, pParameter.Ref);
    if not pParameter.Description.IsEmpty then
      Result.AddPair(c_OpenApiDescription, pParameter.Description);
    Exit;
  end;

  Result.AddPair(c_OpenApiName, pParameter.Name);
  Result.AddPair(c_OpenApiIn, c_SwagRequestParameterInLocation[pParameter.InLocation]);
  if not pParameter.Description.IsEmpty then
    Result.AddPair(c_OpenApiDescription, pParameter.Description);
  if pParameter.Required or (pParameter.InLocation = rpiPath) then
    Result.AddPair(c_OpenApiRequired, TJSONBool.Create(True));
  if pParameter.Deprecated then
    Result.AddPair(c_OpenApiDeprecated, TJSONBool.Create(True));
  if pParameter.AllowEmptyValue and (pParameter.InLocation = rpiQuery) then
    Result.AddPair(c_OpenApiAllowEmptyValue, TJSONBool.Create(True));
  if Assigned(pParameter.Example) then
    Result.AddPair(c_OpenApiExample, pParameter.Example.Clone as TJSONValue);
  if pParameter.Examples.Count > 0 then
    Result.AddPair(c_OpenApiExamples, TSwagExample.GenerateMapJsonObject(pParameter.Examples));

  if pParameter.Content.Count > 0 then
    Result.AddPair(c_OpenApiContent, TSwagMediaType.GenerateMapJsonObject(pParameter.Content, False))
  else if pParameter.InLocation = rpiQueryString then
  begin
    vJsonMediaType := TJSONObject.Create;
    vJsonMediaType.AddPair(c_OpenApiSchema, GenerateParameterSchemaJsonObject(pParameter));
    vJsonContent := TJSONObject.Create;
    vJsonContent.AddPair(c_SwagMimeTypeFormUrlEncoded, vJsonMediaType);
    Result.AddPair(c_OpenApiContent, vJsonContent);
  end
  else
  begin
    if pParameter.Style <> rpsNotDefined then
    begin
      Result.AddPair(c_OpenApiStyle, c_SwagRequestParameterStyle[pParameter.Style]);
      Result.AddPair(c_OpenApiExplode, TJSONBool.Create(pParameter.Explode));
    end;
    if pParameter.AllowReserved and (pParameter.InLocation = rpiQuery) then
      Result.AddPair(c_OpenApiAllowReserved, TJSONBool.Create(True));
    Result.AddPair(c_OpenApiSchema, GenerateParameterSchemaJsonObject(pParameter));
  end;

  pParameter.Extensions.WriteTo(Result);
end;

function TSwagOpenApiGenerator.GenerateParameterSchemaJsonObject(pParameter: TSwagRequestParameter): TJSONObject;
var
  vJsonEnum: TJSONArray;
  vIndex: Integer;
begin
  if not pParameter.Schema.IsEmpty then
    Exit(GenerateSchemaJsonObject(pParameter.Schema));

  Result := TJSONObject.Create;
  case pParameter.TypeParameter of
    stpNotDefined: ;
    stpFile:
    begin
      Result.AddPair(c_OpenApiType, c_OpenApiTypeString);
      Result.AddPair(c_OpenApiFormat, c_OpenApiFormatBinary);
    end;
  else
    Result.AddPair(c_OpenApiType, c_SwagTypeParameter[pParameter.TypeParameter]);
  end;

  if (pParameter.TypeParameter <> stpFile) and (not pParameter.Format.IsEmpty) then
    Result.AddPair(c_OpenApiFormat, pParameter.Format);
  if not pParameter.Pattern.IsEmpty then
    Result.AddPair(c_OpenApiPattern, pParameter.Pattern);
  if not pParameter.Default.IsEmpty then
    Result.AddPair(c_OpenApiDefault, GenerateParameterValue(pParameter, pParameter.Default));
  if pParameter.Enum.Count > 0 then
  begin
    vJsonEnum := TJSONArray.Create;
    for vIndex := 0 to pParameter.Enum.Count - 1 do
      vJsonEnum.AddElement(GenerateParameterValue(pParameter, pParameter.Enum[vIndex]));
    Result.AddPair(c_OpenApiEnum, vJsonEnum);
  end;
  if Assigned(pParameter.Items) then
    Result.AddPair(c_OpenApiItems, pParameter.Items.Clone as TJSONObject);
end;

function TSwagOpenApiGenerator.GenerateParameterValue(pParameter: TSwagRequestParameter; const pValue: string): TJSONValue;
var
  vNumber: Double;
begin
  case pParameter.TypeParameter of
    stpInteger, stpNumber:
      if TryStrToFloat(pValue, vNumber, TFormatSettings.Invariant) then
        Result := TJSONNumber.Create(pValue)
      else
        Result := TJSONString.Create(pValue);
    stpBoolean:
      if SameText(pValue, c_OpenApiBooleanTrue) then
        Result := TJSONBool.Create(True)
      else if SameText(pValue, c_OpenApiBooleanFalse) then
        Result := TJSONBool.Create(False)
      else
        Result := TJSONString.Create(pValue);
  else
    Result := TJSONString.Create(pValue);
  end;
end;

function TSwagOpenApiGenerator.GenerateSchemaJsonObject(pSchema: TSwagDefinition): TJSONObject;
begin
  Result := nil;
  if not pSchema.Name.IsEmpty then
    Result := pSchema.GenerateJsonRefDefinition
  else if Assigned(pSchema.JsonSchema) then
    Result := pSchema.JsonSchema.Clone as TJSONObject;
end;

function TSwagOpenApiGenerator.GenerateRequestBodyJsonObject(pPath: TSwagPath; pOperation: TSwagPathOperation): TJSONObject;
var
  vMimeTypes: TArray<TSwagMimeType>;
  vBodyParameter: TSwagRequestParameter;
  vFormParameters: TList<TSwagRequestParameter>;
begin
  Result := nil;
  if not pOperation.RequestBody.IsEmpty then
    Exit(pOperation.RequestBody.GenerateJsonObject);

  vMimeTypes := ResolveMimeTypes(pOperation.Consumes, fSwagDoc.Consumes);

  vBodyParameter := FindBodyParameter(pOperation.Parameters);
  if not Assigned(vBodyParameter) then
    vBodyParameter := FindBodyParameter(pPath.Parameters);
  if Assigned(vBodyParameter) then
    Exit(GenerateBodyParameterRequestBodyJsonObject(vBodyParameter, vMimeTypes));

  vFormParameters := TList<TSwagRequestParameter>.Create;
  try
    CollectFormParameters(pOperation.Parameters, vFormParameters);
    CollectFormParameters(pPath.Parameters, vFormParameters);
    if vFormParameters.Count > 0 then
      Result := GenerateFormParametersRequestBodyJsonObject(vFormParameters, vMimeTypes);
  finally
    vFormParameters.Free;
  end;
end;

function TSwagOpenApiGenerator.GenerateBodyParameterRequestBodyJsonObject(pParameter: TSwagRequestParameter;
  const pMimeTypes: TArray<TSwagMimeType>): TJSONObject;
var
  vJsonContent: TJSONObject;
  vJsonMediaType: TJSONObject;
  vJsonSchema: TJSONObject;
  vMimeType: TSwagMimeType;
begin
  Result := TJSONObject.Create;
  if not pParameter.Ref.IsEmpty then
  begin
    Result.AddPair(c_OpenApiRef, c_OpenApiRequestBodiesRefPrefix + ExtractRefName(pParameter.Ref));
    Exit;
  end;

  if not pParameter.Description.IsEmpty then
    Result.AddPair(c_OpenApiDescription, pParameter.Description);
  if pParameter.Required then
    Result.AddPair(c_OpenApiRequired, TJSONBool.Create(True));

  vJsonContent := TJSONObject.Create;
  for vMimeType in pMimeTypes do
  begin
    vJsonMediaType := TJSONObject.Create;
    vJsonSchema := GenerateSchemaJsonObject(pParameter.Schema);
    if Assigned(vJsonSchema) then
      vJsonMediaType.AddPair(c_OpenApiSchema, vJsonSchema);
    vJsonContent.AddPair(vMimeType, vJsonMediaType);
  end;
  Result.AddPair(c_OpenApiContent, vJsonContent);
end;

function TSwagOpenApiGenerator.GenerateFormParametersRequestBodyJsonObject(pParameters: TList<TSwagRequestParameter>;
  const pMimeTypes: TArray<TSwagMimeType>): TJSONObject;
var
  vParameter: TSwagRequestParameter;
  vHasFile: Boolean;
  vRequired: Boolean;
  vJsonContent: TJSONObject;
  vJsonMediaType: TJSONObject;
  vMimeType: TSwagMimeType;
begin
  vHasFile := False;
  vRequired := False;
  for vParameter in pParameters do
  begin
    vHasFile := vHasFile or (vParameter.TypeParameter = stpFile);
    vRequired := vRequired or vParameter.Required;
  end;

  Result := TJSONObject.Create;
  if vRequired then
    Result.AddPair(c_OpenApiRequired, TJSONBool.Create(True));

  vJsonContent := TJSONObject.Create;
  for vMimeType in ResolveFormMimeTypes(pMimeTypes, vHasFile) do
  begin
    vJsonMediaType := TJSONObject.Create;
    vJsonMediaType.AddPair(c_OpenApiSchema, GenerateFormSchemaJsonObject(pParameters));
    vJsonContent.AddPair(vMimeType, vJsonMediaType);
  end;
  Result.AddPair(c_OpenApiContent, vJsonContent);
end;

function TSwagOpenApiGenerator.GenerateFormSchemaJsonObject(pParameters: TList<TSwagRequestParameter>): TJSONObject;
var
  vParameter: TSwagRequestParameter;
  vJsonProperties: TJSONObject;
  vJsonProperty: TJSONObject;
  vJsonRequired: TJSONArray;
begin
  Result := TJSONObject.Create;
  Result.AddPair(c_OpenApiType, c_OpenApiTypeObject);

  vJsonProperties := TJSONObject.Create;
  vJsonRequired := TJSONArray.Create;
  for vParameter in pParameters do
  begin
    vJsonProperty := GenerateParameterSchemaJsonObject(vParameter);
    if (not vParameter.Description.IsEmpty) and (not Assigned(vJsonProperty.Values[c_OpenApiDescription])) then
      vJsonProperty.AddPair(c_OpenApiDescription, vParameter.Description);
    vJsonProperties.AddPair(vParameter.Name, vJsonProperty);
    if vParameter.Required then
      vJsonRequired.Add(vParameter.Name);
  end;
  Result.AddPair(c_OpenApiProperties, vJsonProperties);

  if vJsonRequired.Count > 0 then
    Result.AddPair(c_OpenApiRequired, vJsonRequired)
  else
    vJsonRequired.Free;
end;

function TSwagOpenApiGenerator.GenerateResponsesJsonObject(pOperation: TSwagPathOperation): TJSONObject;
var
  vMimeTypes: TArray<TSwagMimeType>;
  vStatusCodes: TArray<TSwagStatusCode>;
  vStatusCode: TSwagStatusCode;
  vResponse: TSwagResponse;
  vKey: string;
begin
  Result := TJSONObject.Create;
  vMimeTypes := ResolveMimeTypes(pOperation.Produces, fSwagDoc.Produces);
  vStatusCodes := pOperation.Responses.Keys.ToArray;
  TArray.Sort<TSwagStatusCode>(vStatusCodes);
  for vStatusCode in vStatusCodes do
  begin
    vResponse := pOperation.Responses.Items[vStatusCode];
    vKey := vResponse.StatusCode;
    if vKey.IsEmpty then
      vKey := vStatusCode;
    Result.AddPair(vKey, GenerateResponseJsonObject(vResponse, vMimeTypes));
  end;
end;

function TSwagOpenApiGenerator.GenerateResponseJsonObject(pResponse: TSwagResponse;
  const pMimeTypes: TArray<TSwagMimeType>): TJSONObject;
var
  vJsonContent: TJSONObject;
  vJsonMediaType: TJSONObject;
  vJsonSchema: TJSONObject;
  vMimeType: TSwagMimeType;
begin
  Result := TJSONObject.Create;
  if not pResponse.Ref.IsEmpty then
  begin
    Result.AddPair(c_OpenApiRef, pResponse.Ref);
    if not pResponse.Summary.IsEmpty then
      Result.AddPair(c_OpenApiSummary, pResponse.Summary);
    if not pResponse.Description.IsEmpty then
      Result.AddPair(c_OpenApiDescription, pResponse.Description);
    Exit;
  end;

  if not pResponse.Summary.IsEmpty then
    Result.AddPair(c_OpenApiSummary, pResponse.Summary);
  Result.AddPair(c_OpenApiDescription, pResponse.Description);
  if pResponse.Headers.Count > 0 then
    Result.AddPair(c_OpenApiHeaders, TSwagHeaders.GenerateMapJsonObject(pResponse.Headers, svOpenApi3));

  if pResponse.Content.Count > 0 then
    vJsonContent := TSwagMediaType.GenerateMapJsonObject(pResponse.Content, False)
  else
  begin
    vJsonContent := TJSONObject.Create;
    if (not pResponse.Schema.IsEmpty) or (pResponse.Examples.Count > 0) then
      for vMimeType in pMimeTypes do
      begin
        vJsonMediaType := TJSONObject.Create;
        vJsonSchema := GenerateSchemaJsonObject(pResponse.Schema);
        if Assigned(vJsonSchema) then
          vJsonMediaType.AddPair(c_OpenApiSchema, vJsonSchema);
        if pResponse.Examples.Count > 0 then
          vJsonMediaType.AddPair(c_OpenApiExamples, GenerateExamplesJsonObject(pResponse.Examples));
        vJsonContent.AddPair(vMimeType, vJsonMediaType);
      end;
  end;
  AddJsonObject(Result, c_OpenApiContent, vJsonContent);

  if pResponse.Links.Count > 0 then
    Result.AddPair(c_OpenApiLinks, TSwagLink.GenerateMapJsonObject(pResponse.Links));

  pResponse.Extensions.WriteTo(Result);
end;

function TSwagOpenApiGenerator.GenerateExamplesJsonObject(
  pExamples: TObjectDictionary<TSwagJsonExampleDescription, TJSONObject>): TJSONObject;
var
  vKey: TSwagJsonExampleDescription;
  vJsonExample: TJSONObject;
begin
  Result := TJSONObject.Create;
  for vKey in pExamples.Keys do
  begin
    vJsonExample := TJSONObject.Create;
    vJsonExample.AddPair(c_OpenApiExampleValue, pExamples.Items[vKey].Clone as TJSONObject);
    Result.AddPair(vKey, vJsonExample);
  end;
end;

function TSwagOpenApiGenerator.GenerateComponentsJsonObject: TJSONObject;
begin
  Result := TJSONObject.Create;
  AddJsonObject(Result, c_SwagComponentsSchemas, GenerateSchemasJsonObject);
  AddJsonObject(Result, c_SwagComponentsResponses, GenerateComponentResponsesJsonObject);
  AddJsonObject(Result, c_SwagComponentsParameters, GenerateComponentParametersJsonObject);
  AddJsonObject(Result, c_SwagComponentsExamples, TSwagExample.GenerateMapJsonObject(fSwagDoc.Examples));
  AddJsonObject(Result, c_SwagComponentsRequestBodies, GenerateComponentRequestBodiesJsonObject);
  AddJsonObject(Result, c_SwagComponentsHeaders, TSwagHeaders.GenerateMapJsonObject(fSwagDoc.Headers, svOpenApi3));
  AddJsonObject(Result, c_SwagComponentsSecuritySchemes, GenerateSecuritySchemesJsonObject);
  AddJsonObject(Result, c_SwagComponentsLinks, TSwagLink.GenerateMapJsonObject(fSwagDoc.Links));
  AddJsonObject(Result, c_SwagComponentsCallbacks, GenerateCallbacksJsonObject(fSwagDoc.Callbacks));
  AddJsonObject(Result, c_SwagComponentsPathItems, GeneratePathsJsonObject(fSwagDoc.PathItems));
  AddJsonObject(Result, c_SwagComponentsMediaTypes, TSwagMediaType.GenerateMapJsonObject(fSwagDoc.MediaTypes, True));
end;

function TSwagOpenApiGenerator.GenerateSchemasJsonObject: TJSONObject;
var
  vDefinition: TSwagDefinition;
begin
  Result := TJSONObject.Create;
  for vDefinition in fSwagDoc.Definitions do
    if Assigned(vDefinition.JsonSchema) then
      Result.AddPair(vDefinition.Name, vDefinition.JsonSchema.Clone as TJSONObject);
end;

function TSwagOpenApiGenerator.GenerateComponentResponsesJsonObject: TJSONObject;
var
  vResponse: TSwagResponse;
  vMimeTypes: TArray<TSwagMimeType>;
  vKey: string;
begin
  Result := TJSONObject.Create;
  vMimeTypes := ResolveMimeTypes(fSwagDoc.Produces, fSwagDoc.Produces);
  for vResponse in fSwagDoc.Responses do
  begin
    vKey := vResponse.Name;
    if vKey.IsEmpty then
      vKey := vResponse.StatusCode;
    Result.AddPair(vKey, GenerateResponseJsonObject(vResponse, vMimeTypes));
  end;
end;

function TSwagOpenApiGenerator.GenerateComponentParametersJsonObject: TJSONObject;
var
  vParameter: TSwagRequestParameter;
begin
  Result := TJSONObject.Create;
  for vParameter in fSwagDoc.Parameters do
    if not IsPayloadParameter(vParameter) then
      Result.AddPair(vParameter.Name, GenerateParameterJsonObject(vParameter));
end;

function TSwagOpenApiGenerator.GenerateComponentRequestBodiesJsonObject: TJSONObject;
var
  vParameter: TSwagRequestParameter;
  vRequestBody: TSwagRequestBody;
  vMimeTypes: TArray<TSwagMimeType>;
  vFormParameters: TList<TSwagRequestParameter>;
begin
  Result := TJSONObject.Create;
  vMimeTypes := ResolveMimeTypes(fSwagDoc.Consumes, fSwagDoc.Consumes);
  for vParameter in fSwagDoc.Parameters do
  begin
    if not IsPayloadParameter(vParameter) then
      Continue;

    if vParameter.InLocation = rpiBody then
      Result.AddPair(vParameter.Name, GenerateBodyParameterRequestBodyJsonObject(vParameter, vMimeTypes))
    else
    begin
      vFormParameters := TList<TSwagRequestParameter>.Create;
      try
        vFormParameters.Add(vParameter);
        Result.AddPair(vParameter.Name, GenerateFormParametersRequestBodyJsonObject(vFormParameters, vMimeTypes));
      finally
        vFormParameters.Free;
      end;
    end;
  end;

  for vRequestBody in fSwagDoc.RequestBodies do
    Result.AddPair(vRequestBody.Name, vRequestBody.GenerateJsonObject);
end;

function TSwagOpenApiGenerator.GenerateSecuritySchemesJsonObject: TJSONObject;
var
  vSecurityDefinition: TSwagSecurityDefinition;
  vJsonScheme: TJSONObject;
begin
  Result := TJSONObject.Create;
  for vSecurityDefinition in fSwagDoc.SecurityDefinitions do
  begin
    vJsonScheme := vSecurityDefinition.GenerateJsonObject(svOpenApi3);
    if vSecurityDefinition.Deprecated then
      vJsonScheme.AddPair(c_OpenApiDeprecated, TJSONBool.Create(True));
    vSecurityDefinition.Extensions.WriteTo(vJsonScheme);
    Result.AddPair(vSecurityDefinition.SchemeName, vJsonScheme);
  end;
end;

end.
