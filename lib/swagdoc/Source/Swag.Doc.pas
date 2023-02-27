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
  Swag.Doc.SecurityDefinition,
  Swag.Doc.Path,
  Swag.Doc.Path.Operation.RequestParameter,
  Swag.Doc.Definition;

type
  ESwagErrorLoadSwaggerJsonFile = class(Exception);

  /// <summary>
  /// This is the root document object for the API specification.
  /// It combines what previously was the Resource Listing and API Declaration (version 1.2 and earlier) together into one document.
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
    fParameters: TObjectList<TSwagRequestParameter>;
    function GetSwaggerVersion: string;
    procedure SetSwaggerFilesFolder(const Value: string);
    function GenerateTagsJsonArray: TJSONArray;
  protected
    function GenerateSchemesJsonArray: TJSONArray;
    function GenerateSecurityDefinitionsJsonObject: TJSONObject;
    function GenerateConsumesJsonArray: TJSONArray;
    function GenerateProducesJsonArray: TJSONArray;
    function GeneratePathsJsonObject: TJSONObject;
    function GenerateDefinitionsJsonObject: TJSONObject;
    function GenerateParametersJsonObject: TJSONObject;

    function GenerateMimeTypesJsonArray(pMimeTypesList: TList<TSwagMimeType>): TJSONArray;
    function ReturnSwaggerFileName: string;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    procedure GenerateSwaggerJson;
    procedure SaveSwaggerJsonToFile;

    procedure LoadFromFile(const pFilename: string);

    property SwaggerFilesFolder: string read fSwaggerFilesFolder write SetSwaggerFilesFolder;
    property SwaggerJson: TJSONValue read fSwaggerJson;

    /// <summary>
    /// Required. Specifies the Swagger Specification version being used.
    /// It can be used by the Swagger UI and other clients to interpret the API listing. The value MUST be "2.0".
    /// </summary>
    property SwaggerVersion: string read GetSwaggerVersion;

    /// <summary>
    /// Required. Provides metadata about the API. The metadata can be used by the clients if needed.
    /// </summary>
    property Info: TSwagInfo read fInfo;

    /// <summary>
    /// The host (name or ip) serving the API. This MUST be the host only and does not include the scheme nor sub-paths.
    /// It MAY include a port.
    /// If the host is not included, the host serving the documentation is to be used (including the port).
    /// The host does not support path templating.
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
    /// A list of MIME types the APIs can consume. This is global to all APIs but can be overridden on specific API calls.
    /// Value MUST be as described under Mime Types.
    /// </summary>
    property Consumes: TList<TSwagMimeType> read fConsumes;

    /// <summary>
    /// A list of MIME types the APIs can produce. This is global to all APIs but can be overridden on specific API calls.
    /// Value MUST be as described under Mime Types.
    /// </summary>
    property Produces: TList<TSwagMimeType> read fProduces;

    /// <summary>
    /// Required. The available paths and operations for the API.
    /// </summary>
    property Paths: TObjectList<TSwagPath> read fPaths;

    /// <summary>
    /// An object to hold data types produced and consumed by operations.
    /// </summary>
    property Definitions: TObjectList<TSwagDefinition> read fDefinitions;

    /// <summary>
    /// Security scheme definitions that can be used across the specification.
    /// </summary>
    property SecurityDefinitions: TObjectList<TSwagSecurityDefinition> read fSecurityDefinitions;


    property Parameters: TObjectList<TSwagRequestParameter> read fParameters;

    property Tags: TObjectList<TSwagTag> read fTags;

    property ExternalDocs: TSwagExternalDocs read fExternalDocs;
  end;

const
  c_Swagger = 'swagger';
  c_SwagInfo = 'info';
  c_SwagHost = 'host';
  c_SwagBasePath = 'basePath';
  c_SwagTags = 'tags';
  c_SwagSchemes = 'schemes';
  c_SwagSecurityDefinitions = 'securityDefinitions';
  c_SwagSecurityDefinitionsType = 'type';
  c_SwagConsumes = 'consumes';
  c_SwagProduces = 'produces';
  c_SwagPaths = 'paths';
  c_SwagDefinitions = 'definitions';
  c_SwagExternalDocs = 'externalDocs';
  c_SwagExternalDocsDescription = 'description';
  c_SwagExternalDocsUrl = 'url';
  c_SwagParameters = 'parameters';

implementation

uses
  System.IOUtils,
  Json.Common.Helpers,
  Swag.Common.Consts,
  Swag.Common.Types.Helpers,
  Swag.Doc.FileLoader;

{ TSwagDoc }

constructor TSwagDoc.Create;
begin
  inherited Create;

  fInfo := TSwagInfo.Create;
  fTags := TObjectList<TSwagTag>.Create;
  fSecurityDefinitions := TObjectList<TSwagSecurityDefinition>.Create;
  fConsumes := TList<string>.Create;
  fProduces := TList<string>.Create;
  fPaths := TObjectList<TSwagPath>.Create;
  fDefinitions := TObjectList<TSwagDefinition>.Create;
  fExternalDocs := TSwagExternalDocs.Create;
  fParameters := TObjectList<TSwagRequestParameter>.Create;
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

  if Assigned(fSwaggerJson) then
    FreeAndNil(fSwaggerJson);

  inherited Destroy;
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
    Result.AddPair(fParameters.Items[vIndex].Name, fParameters.Items[vIndex].GenerateJsonObject);
end;

function TSwagDoc.GeneratePathsJsonObject: TJSONObject;
var
  vIndex: integer;
begin
  Result := TJsonObject.Create;
  for vIndex := 0 to fPaths.Count -1 do
    Result.AddPair(fPaths.Items[vIndex].Uri, fPaths.Items[vIndex].GenerateJsonObject);
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
begin
  Result := TJsonObject.Create;
  for vIndex := 0 to fSecurityDefinitions.Count -1 do
    Result.AddPair(fSecurityDefinitions.Items[vIndex].SchemeName, fSecurityDefinitions.Items[vIndex].GenerateJsonObject);
end;

procedure TSwagDoc.GenerateSwaggerJson;
var
  vJsonObject: TJsonObject;
begin
  vJsonObject := TJsonObject.Create;

  vJsonObject.AddPair(c_Swagger, GetSwaggerVersion);
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

  if (fParameters.Count > 0) then
    vJsonObject.AddPair(c_SwagParameters, GenerateParametersJsonObject);

  if (fSecurityDefinitions.Count > 0) then
    vJsonObject.AddPair(c_SwagSecurityDefinitions, GenerateSecurityDefinitionsJsonObject);

  if (fDefinitions.Count > 0) then
    vJsonObject.AddPair(c_SwagDefinitions, GenerateDefinitionsJsonObject);

  if Assigned(fSwaggerJson) then
    fSwaggerJson.Free;
  fSwaggerJson := vJsonObject;
end;

function TSwagDoc.GetSwaggerVersion: string;
begin
  Result := c_SwaggerVersion;
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
  Result := fSwaggerFilesFolder + c_SwaggerFileName;
end;

procedure TSwagDoc.SetSwaggerFilesFolder(const Value: string);
begin
  fSwaggerFilesFolder := IncludeTrailingPathDelimiter(Trim(Value));
end;

end.
