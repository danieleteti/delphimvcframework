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

unit Sample.SwagDoc;

interface

uses
  Swag.Doc;

type
  /// <summary>
  /// Builds the OpenAPI 3 document of the sample API. The document level objects (info, servers, tags, security
  /// schemes and a reusable path item) are documented here and the employee operations in Sample.Api.Employee.
  /// </summary>
  TSampleApiSwagDocBuilder = class(TObject)
  strict private
    fSwagDoc: TSwagDoc;
    fDeployFolder: string;

    procedure DocumentApiInfo;
    procedure DocumentApiSettings;
    procedure DocumentApiServers;
    procedure DocumentApiTags;
    procedure DocumentApiSecurity;
    procedure DocumentApi;
    procedure DocumentApiHealth;
    procedure DocumentApiEmployee;

    procedure SaveSwaggerJson;
  private
    procedure SetDeployFolder(const Value: string);
  public
    /// <summary>
    /// Generates the document, saves the openapi.json file in the DeployFolder and returns its content.
    /// </summary>
    function Generate: string;
    property DeployFolder: string read fDeployFolder write SetDeployFolder;
  end;

implementation

uses
  System.JSON,
  Json.Common.Helpers,
  Swag.Common.Types,
  Swag.Doc.Server,
  Swag.Doc.Tags,
  Swag.Doc.SecurityRequirement,
  Swag.Doc.SecurityDefinitionApiKey,
  Swag.Doc.SecurityDefinitionBasic,
  Swag.Doc.SecurityDefinitionHttp,
  Swag.Doc.SecurityDefinitionMutualTls,
  Swag.Doc.SecurityDefinitionOAuth2,
  Swag.Doc.SecurityDefinitionOpenIdConnect,
  Swag.Doc.Path.Operation,
  Swag.Doc.Path.Operation.Response,
  Sample.Api.Employee;

{ TSampleApiSwagDocBuilder }

function TSampleApiSwagDocBuilder.Generate: string;
begin
  fSwagDoc := TSwagDoc.Create;
  try
    fSwagDoc.SpecVersion := svOpenApi3;
    DocumentApiInfo;
    DocumentApiSettings;
    DocumentApiServers;
    DocumentApiTags;
    DocumentApiSecurity;
    DocumentApi;
    fSwagDoc.GenerateSwaggerJson;
    SaveSwaggerJson;
    Result := fSwagDoc.SwaggerJson.Format;
  finally
    fSwagDoc.Free;
  end;
end;

procedure TSampleApiSwagDocBuilder.DocumentApiInfo;
begin
  fSwagDoc.Info.Title := 'Sample API';
  fSwagDoc.Info.Summary := 'Employee management API.';
  fSwagDoc.Info.Version := 'v1';
  fSwagDoc.Info.TermsOfService := 'http://www.apache.org/licenses/LICENSE-2.0.txt';
  fSwagDoc.Info.Description := 'Sample API documented with OpenAPI 3 using SwagDoc.';
  fSwagDoc.Info.Contact.Name := 'Marcelo Jaloto';
  fSwagDoc.Info.Contact.Email := 'marcelojaloto@gmail.com';
  fSwagDoc.Info.Contact.Url := 'https://github.com/marcelojaloto/SwagDoc';
  fSwagDoc.Info.License.Name := 'Apache License - Version 2.0, January 2004';
  fSwagDoc.Info.License.Identifier := 'Apache-2.0';
  fSwagDoc.Info.Extensions.Add('x-logo',
    TJSONObject.Create.AddPair('url', 'https://example.com/logo.png').AddPair('altText', 'Sample API'));
end;

procedure TSampleApiSwagDocBuilder.DocumentApiSettings;
begin
  fSwagDoc.SelfUri := 'https://api.example.com/api/v1/openapi.json';
  fSwagDoc.ExternalDocs.Description := 'SwagDoc repository';
  fSwagDoc.ExternalDocs.Url := 'https://github.com/marcelojaloto/SwagDoc';
  fSwagDoc.Extensions.Add('x-api-id', 'sample-employee-api');
end;

procedure TSampleApiSwagDocBuilder.DocumentApiServers;
var
  vServer: TSwagServer;
  vVariable: TSwagServerVariable;
begin
  vServer := fSwagDoc.AddServer('https://{environment}.example.com/api/{version}', 'Cloud server');
  vServer.Name := 'cloud';
  vVariable := vServer.AddVariable('environment', 'api', 'The environment of the API.');
  vVariable.Enum.Add('api');
  vVariable.Enum.Add('staging');
  vVariable.Enum.Add('sandbox');
  vServer.AddVariable('version', 'v1', 'The version of the API.');

  vServer := fSwagDoc.AddServer('http://localhost:8080/api/v1', 'Local development server');
  vServer.Name := 'local';
end;

procedure TSampleApiSwagDocBuilder.DocumentApiTags;
var
  vTag: TSwagTag;
begin
  vTag := TSwagTag.Create;
  vTag.Name := 'Employees';
  vTag.Summary := 'Employees';
  vTag.Description := 'Operations on the employees of the company.';
  vTag.Kind := 'nav';
  fSwagDoc.Tags.Add(vTag);

  vTag := TSwagTag.Create;
  vTag.Name := 'Employee documents';
  vTag.Summary := 'Documents';
  vTag.Description := 'Documents attached to an employee.';
  vTag.Parent := 'Employees';
  vTag.Kind := 'nav';
  fSwagDoc.Tags.Add(vTag);

  vTag := TSwagTag.Create;
  vTag.Name := 'Notifications';
  vTag.Summary := 'Notifications';
  vTag.Description := 'Requests sent by the API to the client applications.';
  vTag.Kind := 'nav';
  fSwagDoc.Tags.Add(vTag);

  vTag := TSwagTag.Create;
  vTag.Name := 'Health';
  vTag.Summary := 'Health';
  vTag.Kind := 'nav';
  fSwagDoc.Tags.Add(vTag);
end;

procedure TSampleApiSwagDocBuilder.DocumentApiSecurity;
var
  vBearer: TSwagSecurityDefinitionHttp;
  vApiKey: TSwagSecurityDefinitionApiKey;
  vOAuth2: TSwagSecurityDefinitionOAuth2;
  vFlow: TSwagSecurityDefinitionOAuth2Flow;
  vOpenIdConnect: TSwagSecurityDefinitionOpenIdConnect;
  vMutualTls: TSwagSecurityDefinitionMutualTls;
  vBasic: TSwagSecurityDefinitionBasic;
  vRequirement: TSwagSecurityRequirement;
begin
  vBearer := TSwagSecurityDefinitionHttp.Create;
  vBearer.SchemeName := 'bearerAuth';
  vBearer.Description := 'JSON Web Token sent in the Authorization header.';
  vBearer.Scheme := 'bearer';
  vBearer.BearerFormat := 'JWT';
  fSwagDoc.SecurityDefinitions.Add(vBearer);

  vApiKey := TSwagSecurityDefinitionApiKey.Create;
  vApiKey.SchemeName := 'apiKeyAuth';
  vApiKey.Description := 'Key of the client application.';
  vApiKey.Name := 'X-API-Key';
  vApiKey.InLocation := kilHeader;
  fSwagDoc.SecurityDefinitions.Add(vApiKey);

  vOAuth2 := TSwagSecurityDefinitionOAuth2.Create;
  vOAuth2.SchemeName := 'oauth2Auth';
  vOAuth2.Description := 'OAuth 2.0 authorization server of the company.';
  vOAuth2.OAuth2MetadataUrl := 'https://auth.example.com/.well-known/oauth-authorization-server';

  vFlow := vOAuth2.AddFlow(oftAuthorizationCode);
  vFlow.AuthorizationUrl := 'https://auth.example.com/authorize';
  vFlow.TokenUrl := 'https://auth.example.com/token';
  vFlow.RefreshUrl := 'https://auth.example.com/refresh';
  vFlow.AddScope('employees:read', 'Reads the employees.');
  vFlow.AddScope('employees:write', 'Creates, updates and deletes the employees.');

  vFlow := vOAuth2.AddFlow(oftClientCredentials);
  vFlow.TokenUrl := 'https://auth.example.com/token';
  vFlow.AddScope('employees:read', 'Reads the employees.');

  vFlow := vOAuth2.AddFlow(oftDeviceAuthorization);
  vFlow.DeviceAuthorizationUrl := 'https://auth.example.com/device';
  vFlow.TokenUrl := 'https://auth.example.com/token';
  vFlow.AddScope('employees:read', 'Reads the employees.');
  fSwagDoc.SecurityDefinitions.Add(vOAuth2);

  vOpenIdConnect := TSwagSecurityDefinitionOpenIdConnect.Create;
  vOpenIdConnect.SchemeName := 'openIdAuth';
  vOpenIdConnect.OpenIdConnectUrl := 'https://auth.example.com/.well-known/openid-configuration';
  fSwagDoc.SecurityDefinitions.Add(vOpenIdConnect);

  vMutualTls := TSwagSecurityDefinitionMutualTls.Create;
  vMutualTls.SchemeName := 'mutualTlsAuth';
  vMutualTls.Description := 'Client certificate issued by the company.';
  fSwagDoc.SecurityDefinitions.Add(vMutualTls);

  vBasic := TSwagSecurityDefinitionBasic.Create;
  vBasic.SchemeName := 'basicAuth';
  vBasic.Description := 'Replaced by the bearer authentication.';
  vBasic.Deprecated := True;
  fSwagDoc.SecurityDefinitions.Add(vBasic);

  vRequirement := fSwagDoc.AddSecurityRequirement;
  vRequirement.AddScheme('bearerAuth', []);

  vRequirement := fSwagDoc.AddSecurityRequirement;
  vRequirement.AddScheme('oauth2Auth', ['employees:read']);

  vRequirement := fSwagDoc.AddSecurityRequirement;
  vRequirement.AddScheme('apiKeyAuth', []);
  vRequirement.AddScheme('mutualTlsAuth', []);
end;

procedure TSampleApiSwagDocBuilder.DocumentApi;
begin
  DocumentApiHealth;
  DocumentApiEmployee;
end;

procedure TSampleApiSwagDocBuilder.DocumentApiHealth;
var
  vPathItem: TSwagPath;
  vPath: TSwagPath;
  vOperation: TSwagPathOperation;
  vResponse: TSwagResponse;
begin
  vPathItem := TSwagPath.Create;
  vPathItem.Uri := 'health';
  vPathItem.Summary := 'Health check';

  vOperation := vPathItem.AddOperation(ohvGet);
  vOperation.OperationId := 'getHealth';
  vOperation.Description := 'Returns the availability of the API.';
  vOperation.Tags.Add('Health');
  vOperation.DisableSecurity := True;

  vResponse := TSwagResponse.Create;
  vResponse.StatusCode := '200';
  vResponse.Description := 'The API is available.';
  vResponse.AddMediaType('application/json').Schema.JsonSchema :=
    TJSONObject.ParseJSONValue('{"type":"object","properties":{"status":{"type":"string","enum":["up","down"]}}}') as TJSONObject;
  vOperation.Responses.Add(vResponse.StatusCode, vResponse);
  fSwagDoc.PathItems.Add(vPathItem);

  vPath := TSwagPath.Create;
  vPath.Uri := '/health';
  vPath.Ref := '#/components/pathItems/health';
  fSwagDoc.Paths.Add(vPath);
end;

procedure TSampleApiSwagDocBuilder.DocumentApiEmployee;
var
  vApiEmployee: TFakeApiEmployee;
begin
  vApiEmployee := TFakeApiEmployee.Create;
  try
    vApiEmployee.DocumentApi(fSwagDoc);
  finally
    vApiEmployee.Free;
  end;
end;

procedure TSampleApiSwagDocBuilder.SaveSwaggerJson;
begin
  fSwagDoc.SwaggerFilesFolder := fDeployFolder;
  fSwagDoc.SaveSwaggerJsonToFile;
end;

procedure TSampleApiSwagDocBuilder.SetDeployFolder(const Value: string);
begin
  fDeployFolder := Value;
end;

end.
