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

unit Swag.Doc.SecurityDefinitionOAuth2;

interface

uses
  System.JSON,
  System.SysUtils,
  System.Generics.Collections,
  Swag.Common.Types,
  Swag.Doc.Extensions,
  Swag.Doc.SecurityDefinition;

type
  /// <summary>
  /// The OAuth2 flow types. The application and accessCode flows of Swagger 2.0 are the clientCredentials and
  /// authorizationCode flows of OpenAPI 3, and the deviceAuthorization flow is available in OpenAPI 3 only.
  /// </summary>
  TSwagOAuth2FlowType = (oftNotDefined, oftImplicit, oftPassword, oftClientCredentials, oftAuthorizationCode,
    oftDeviceAuthorization);

  TSwagSecurityDefinitionOAuth2Scope = class(TObject)
  private
    fScopeName: string;
    fDescription: string;
  public
    function GenerateJsonObject: TJSONObject;
    procedure Load(pJson: TJSONPair);

    property ScopeName: string read fScopeName write fScopeName;
    property Description: string read fDescription write fDescription;
  end;

  /// <summary>
  /// Configuration details for a supported OAuth2 flow, written as an OAuth Flow Object under the flows field of
  /// an OpenAPI 3 security scheme. A Swagger 2.0 document supports a single flow per scheme, so it writes the first
  /// flow of the list that has a Swagger 2.0 equivalent.
  /// </summary>
  TSwagSecurityDefinitionOAuth2Flow = class(TObject)
  private
    fFlowType: TSwagOAuth2FlowType;
    fAuthorizationUrl: string;
    fDeviceAuthorizationUrl: string;
    fTokenUrl: string;
    fRefreshUrl: string;
    fScopes: TObjectList<TSwagSecurityDefinitionOAuth2Scope>;
    fExtensions: TSwagExtensions;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    /// <summary>
    /// Generates the OpenAPI 3 OAuth Flow Object.
    /// </summary>
    function GenerateJsonObject: TJSONObject;

    procedure Load(pJson: TJSONObject);

    /// <summary>
    /// Adds a scope to the flow and returns it.
    /// </summary>
    function AddScope(const pScopeName, pDescription: string): TSwagSecurityDefinitionOAuth2Scope;

    /// <summary>
    /// Returns the name of the flow in the given specification family, or an empty string when the family has no
    /// equivalent flow.
    /// </summary>
    function ReturnFlowName(const pVersion: TSwagVersion): string;

    /// <summary>
    /// Returns the flow type of a Swagger 2.0 or OpenAPI 3 flow name.
    /// </summary>
    class function FlowTypeFromName(const pFlowName: string): TSwagOAuth2FlowType;

    /// <summary>
    /// The type of the flow.
    /// </summary>
    property FlowType: TSwagOAuth2FlowType read fFlowType write fFlowType;

    /// <summary>
    /// Required (implicit, authorizationCode). The authorization URL to be used for this flow.
    /// </summary>
    property AuthorizationUrl: string read fAuthorizationUrl write fAuthorizationUrl;

    /// <summary>
    /// Required (deviceAuthorization). The device authorization URL to be used for this flow.
    /// </summary>
    property DeviceAuthorizationUrl: string read fDeviceAuthorizationUrl write fDeviceAuthorizationUrl;

    /// <summary>
    /// Required (password, clientCredentials, authorizationCode, deviceAuthorization). The token URL to be used for this flow.
    /// </summary>
    property TokenUrl: string read fTokenUrl write fTokenUrl;

    /// <summary>
    /// The URL to be used for obtaining refresh tokens.
    /// </summary>
    property RefreshUrl: string read fRefreshUrl write fRefreshUrl;

    /// <summary>
    /// Required. The available scopes for the flow. The list MAY be empty.
    /// </summary>
    property Scopes: TObjectList<TSwagSecurityDefinitionOAuth2Scope> read fScopes;

    /// <summary>
    /// The Specification Extensions of the flow.
    /// </summary>
    property Extensions: TSwagExtensions read fExtensions;
  end;

  /// <summary>
  /// The security scheme object for OAuth2.
  /// A single flow can be described with the Flow, AuthorizationUrl, TokenUrl, RefreshUrl and Scopes properties,
  /// which are written with the flow fields of Swagger 2.0 or as one OAuth Flow Object in OpenAPI 3.
  /// When the Flows list is not empty, its flows are written instead: all of them in OpenAPI 3 and the first
  /// supported one in Swagger 2.0.
  /// </summary>
  [ASecurityDefinition(ssdOAuth2)]
  TSwagSecurityDefinitionOAuth2 = class(TSwagSecurityDefinition)
  private
    fName: string;
    fAuthorizationUrl: string;
    fTokenUrl: string;
    fRefreshUrl: string;
    fDeviceAuthorizationUrl: string;
    fOAuth2MetadataUrl: string;
    fFlow: string;
    fScopes : TObjectList<TSwagSecurityDefinitionOAuth2Scope>;
    fFlows: TObjectList<TSwagSecurityDefinitionOAuth2Flow>;
    function GenerateScopesJsonObject(pScopes: TObjectList<TSwagSecurityDefinitionOAuth2Scope>): TJSONObject;
    function ReturnFlowForVersion(const pVersion: TSwagVersion): string;
    function FindFirstFlow(const pVersion: TSwagVersion): TSwagSecurityDefinitionOAuth2Flow;
    procedure LoadScopes(pJsonScopes: TJSONObject; pScopes: TObjectList<TSwagSecurityDefinitionOAuth2Scope>);
  protected
    function GetTypeSecurity: TSwagSecurityDefinitionType; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function GenerateJsonObject: TJSONObject; overload; override;
    function GenerateJsonObject(const pVersion: TSwagVersion): TJSONObject; overload; override;
    procedure Load(pJson: TJSONObject); overload; override;
    procedure Load(pJson: TJSONObject; const pVersion: TSwagVersion); overload; override;

    /// <summary>
    /// Adds a scope to the list and returns it.
    /// </summary>
    function AddScope(const pScopeName, pDescription: string): TSwagSecurityDefinitionOAuth2Scope;

    /// <summary>
    /// Adds a flow to the Flows list and returns it, so its URLs and scopes can be defined.
    /// </summary>
    function AddFlow(const pFlowType: TSwagOAuth2FlowType): TSwagSecurityDefinitionOAuth2Flow;

    /// <summary>
    /// Returns False for Swagger 2.0 when the scheme only has flows without a Swagger 2.0 equivalent, such as
    /// deviceAuthorization.
    /// </summary>
    function SupportsVersion(const pVersion: TSwagVersion): Boolean; override;

    property Name: string read fName write fName;

    /// <summary>
    /// Required (implicit, accessCode). The authorization URL to be used for this flow. This SHOULD be in the form of a URL.
    /// </summary>
    property AuthorizationUrl: string read fAuthorizationUrl write fAuthorizationUrl;

    /// <summary>
    /// Required (password, application, accessCode). The token URL to be used for this flow. This SHOULD be in the form of a URL.
    /// </summary>
    property TokenUrl: string read fTokenUrl write fTokenUrl;

    /// <summary>
    /// The URL to be used for obtaining refresh tokens. This MUST be in the form of a URL. Available in OpenAPI 3 only.
    /// </summary>
    property RefreshUrl: string read fRefreshUrl write fRefreshUrl;

    /// <summary>
    /// Required (deviceAuthorization). The device authorization URL to be used for this flow. This MUST be in the
    /// form of a URL. Available in OpenAPI 3 only.
    /// </summary>
    property DeviceAuthorizationUrl: string read fDeviceAuthorizationUrl write fDeviceAuthorizationUrl;

    /// <summary>
    /// URL to the OAuth2 authorization server metadata defined by RFC8414. TLS is required. Available in OpenAPI 3 only.
    /// </summary>
    property OAuth2MetadataUrl: string read fOAuth2MetadataUrl write fOAuth2MetadataUrl;

    /// <summary>
    /// Required. The flow used by the OAuth2 security scheme. Valid values are "implicit", "password", "application"
    /// or "accessCode". The OpenAPI 3 names "clientCredentials" and "authorizationCode" are also accepted and
    /// every name is translated to the target family of the document. The "deviceAuthorization" flow is
    /// available in OpenAPI 3 only.
    /// </summary>
    property Flow: string read fFlow write fFlow;

    /// <summary>
    /// Required. The available scopes for the OAuth2 security scheme.
    /// </summary>
    property Scopes: TObjectList<TSwagSecurityDefinitionOAuth2Scope> read fScopes;

    /// <summary>
    /// The flows supported by the scheme. When the list is not empty it replaces the Flow, AuthorizationUrl,
    /// TokenUrl, RefreshUrl, DeviceAuthorizationUrl and Scopes properties in the generated document.
    /// </summary>
    property Flows: TObjectList<TSwagSecurityDefinitionOAuth2Flow> read fFlows;
  end;

implementation

uses
  System.Classes,
  Swag.Common.Consts,
  Swag.Common.Json;

const
  c_SwagSecurityDefinitionOAuth2Type = 'type';
  c_SwagSecurityDefinitionOAuth2Description = 'description';
  c_SwagSecurityDefinitionOAuth2Name = 'name';
  c_SwagSecurityDefinitionOAuth2AuthorizationUrl = 'authorizationUrl';
  c_SwagSecurityDefinitionOAuth2TokenUrl = 'tokenUrl';
  c_SwagSecurityDefinitionOAuth2RefreshUrl = 'refreshUrl';
  c_SwagSecurityDefinitionOAuth2DeviceAuthorizationUrl = 'deviceAuthorizationUrl';
  c_SwagSecurityDefinitionOAuth2MetadataUrl = 'oauth2MetadataUrl';
  c_SwagSecurityDefinitionOAuth2Flow = 'flow';
  c_SwagSecurityDefinitionOAuth2Flows = 'flows';
  c_SwagSecurityDefinitionOAuth2Scopes = 'scopes';

  c_SwagOAuth2FlowNamesSwagger2: array[TSwagOAuth2FlowType] of string =
    ('', 'implicit', 'password', 'application', 'accessCode', '');
  c_SwagOAuth2FlowNamesOpenApi3: array[TSwagOAuth2FlowType] of string =
    ('', 'implicit', 'password', 'clientCredentials', 'authorizationCode', 'deviceAuthorization');

{ TSwagSecurityDefinitionOAuth2Scopes }

function TSwagSecurityDefinitionOAuth2Scope.GenerateJsonObject: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair(fScopeName, fDescription);
end;

procedure TSwagSecurityDefinitionOAuth2Scope.Load(pJson: TJSONPair);
begin
  fScopeName := pJson.JsonString.Value;
  fDescription := pJson.JsonValue.Value;
end;

{ TSwagSecurityDefinitionOAuth2Flow }

constructor TSwagSecurityDefinitionOAuth2Flow.Create;
begin
  inherited Create;
  fScopes := TObjectList<TSwagSecurityDefinitionOAuth2Scope>.Create;
  fExtensions := TSwagExtensions.Create;
end;

destructor TSwagSecurityDefinitionOAuth2Flow.Destroy;
begin
  FreeAndNil(fScopes);
  FreeAndNil(fExtensions);
  inherited Destroy;
end;

function TSwagSecurityDefinitionOAuth2Flow.AddScope(const pScopeName, pDescription: string): TSwagSecurityDefinitionOAuth2Scope;
begin
  Result := TSwagSecurityDefinitionOAuth2Scope.Create;
  Result.ScopeName := pScopeName;
  Result.Description := pDescription;
  fScopes.Add(Result);
end;

function TSwagSecurityDefinitionOAuth2Flow.ReturnFlowName(const pVersion: TSwagVersion): string;
begin
  case pVersion of
    svOpenApi3: Result := c_SwagOAuth2FlowNamesOpenApi3[fFlowType];
  else
    Result := c_SwagOAuth2FlowNamesSwagger2[fFlowType];
  end;
end;

class function TSwagSecurityDefinitionOAuth2Flow.FlowTypeFromName(const pFlowName: string): TSwagOAuth2FlowType;
var
  vFlowType: TSwagOAuth2FlowType;
begin
  Result := oftNotDefined;
  for vFlowType := Succ(Low(TSwagOAuth2FlowType)) to High(TSwagOAuth2FlowType) do
    if SameText(pFlowName, c_SwagOAuth2FlowNamesOpenApi3[vFlowType]) or
      ((not c_SwagOAuth2FlowNamesSwagger2[vFlowType].IsEmpty) and SameText(pFlowName, c_SwagOAuth2FlowNamesSwagger2[vFlowType])) then
      Exit(vFlowType);
end;

function TSwagSecurityDefinitionOAuth2Flow.GenerateJsonObject: TJSONObject;
var
  vJsonScopes: TJSONObject;
  vScope: TSwagSecurityDefinitionOAuth2Scope;
begin
  Result := TJSONObject.Create;
  if not fAuthorizationUrl.IsEmpty then
    Result.AddPair(c_SwagSecurityDefinitionOAuth2AuthorizationUrl, fAuthorizationUrl);
  if not fDeviceAuthorizationUrl.IsEmpty then
    Result.AddPair(c_SwagSecurityDefinitionOAuth2DeviceAuthorizationUrl, fDeviceAuthorizationUrl);
  if not fTokenUrl.IsEmpty then
    Result.AddPair(c_SwagSecurityDefinitionOAuth2TokenUrl, fTokenUrl);
  if not fRefreshUrl.IsEmpty then
    Result.AddPair(c_SwagSecurityDefinitionOAuth2RefreshUrl, fRefreshUrl);

  vJsonScopes := TJSONObject.Create;
  for vScope in fScopes do
    vJsonScopes.AddPair(vScope.ScopeName, vScope.Description);
  Result.AddPair(c_SwagSecurityDefinitionOAuth2Scopes, vJsonScopes);

  fExtensions.WriteTo(Result);
end;

procedure TSwagSecurityDefinitionOAuth2Flow.Load(pJson: TJSONObject);
var
  vJsonScopes: TJSONObject;
  vIndex: Integer;
  vScope: TSwagSecurityDefinitionOAuth2Scope;
begin
  if not Assigned(pJson) then
    Exit;

  fAuthorizationUrl := TSwagJson.ReadString(pJson, c_SwagSecurityDefinitionOAuth2AuthorizationUrl);
  fDeviceAuthorizationUrl := TSwagJson.ReadString(pJson, c_SwagSecurityDefinitionOAuth2DeviceAuthorizationUrl);
  fTokenUrl := TSwagJson.ReadString(pJson, c_SwagSecurityDefinitionOAuth2TokenUrl);
  fRefreshUrl := TSwagJson.ReadString(pJson, c_SwagSecurityDefinitionOAuth2RefreshUrl);

  vJsonScopes := TSwagJson.ReadObject(pJson, c_SwagSecurityDefinitionOAuth2Scopes);
  if Assigned(vJsonScopes) then
    for vIndex := 0 to vJsonScopes.Count - 1 do
    begin
      vScope := TSwagSecurityDefinitionOAuth2Scope.Create;
      vScope.Load(vJsonScopes.Pairs[vIndex]);
      fScopes.Add(vScope);
    end;

  fExtensions.ReadFrom(pJson);
end;

{ TSwagSecurityDefinitionOAuth2 }

constructor TSwagSecurityDefinitionOAuth2.Create;
begin
  inherited;
  fScopes := TObjectList<TSwagSecurityDefinitionOAuth2Scope>.Create;
  fFlows := TObjectList<TSwagSecurityDefinitionOAuth2Flow>.Create;
end;

destructor TSwagSecurityDefinitionOAuth2.Destroy;
begin
  FreeAndNil(fScopes);
  FreeAndNil(fFlows);
  inherited;
end;

function TSwagSecurityDefinitionOAuth2.AddScope(const pScopeName, pDescription: string): TSwagSecurityDefinitionOAuth2Scope;
begin
  Result := TSwagSecurityDefinitionOAuth2Scope.Create;
  Result.ScopeName := pScopeName;
  Result.Description := pDescription;
  fScopes.Add(Result);
end;

function TSwagSecurityDefinitionOAuth2.AddFlow(const pFlowType: TSwagOAuth2FlowType): TSwagSecurityDefinitionOAuth2Flow;
begin
  Result := TSwagSecurityDefinitionOAuth2Flow.Create;
  Result.FlowType := pFlowType;
  fFlows.Add(Result);
end;

function TSwagSecurityDefinitionOAuth2.GenerateScopesJsonObject(
  pScopes: TObjectList<TSwagSecurityDefinitionOAuth2Scope>): TJSONObject;
var
  vScopeIndex: Integer;
begin
  Result := TJSONObject.Create;
  for vScopeIndex := 0 to pScopes.Count - 1 do
    Result.AddPair(pScopes[vScopeIndex].ScopeName, pScopes[vScopeIndex].Description);
end;

function TSwagSecurityDefinitionOAuth2.ReturnFlowForVersion(const pVersion: TSwagVersion): string;
var
  vIndex: Integer;
begin
  Result := fFlow;
  for vIndex := Low(c_SwagOAuth2FlowsSwagger2) to High(c_SwagOAuth2FlowsSwagger2) do
    if SameText(fFlow, c_SwagOAuth2FlowsSwagger2[vIndex]) or SameText(fFlow, c_SwagOAuth2FlowsOpenApi3[vIndex]) then
    begin
      case pVersion of
        svSwagger2: Result := c_SwagOAuth2FlowsSwagger2[vIndex];
        svOpenApi3: Result := c_SwagOAuth2FlowsOpenApi3[vIndex];
      end;
      Break;
    end;
end;

function TSwagSecurityDefinitionOAuth2.FindFirstFlow(const pVersion: TSwagVersion): TSwagSecurityDefinitionOAuth2Flow;
var
  vFlow: TSwagSecurityDefinitionOAuth2Flow;
begin
  Result := nil;
  for vFlow in fFlows do
    if not vFlow.ReturnFlowName(pVersion).IsEmpty then
      Exit(vFlow);
end;

function TSwagSecurityDefinitionOAuth2.SupportsVersion(const pVersion: TSwagVersion): Boolean;
begin
  if pVersion = svOpenApi3 then
    Result := True
  else if fFlows.Count > 0 then
    Result := Assigned(FindFirstFlow(svSwagger2))
  else
    Result := not SameText(fFlow, c_SwagOAuth2FlowDeviceAuthorization);
end;

function TSwagSecurityDefinitionOAuth2.GenerateJsonObject: TJSONObject;
var
  vJsonItem: TJsonObject;
  vFlow: TSwagSecurityDefinitionOAuth2Flow;
begin
  vJsonItem := TJsonObject.Create;
  vJsonItem.AddPair(c_SwagSecurityDefinitionOAuth2Type, ReturnTypeSecurityToString);
  if fDescription.Length > 0 then
    vJsonItem.AddPair(c_SwagSecurityDefinitionOAuth2Description, fDescription);

  vFlow := FindFirstFlow(svSwagger2);
  if Assigned(vFlow) then
  begin
    if not vFlow.AuthorizationUrl.IsEmpty then
      vJsonItem.AddPair(c_SwagSecurityDefinitionOAuth2AuthorizationUrl, vFlow.AuthorizationUrl);
    if not vFlow.TokenUrl.IsEmpty then
      vJsonItem.AddPair(c_SwagSecurityDefinitionOAuth2TokenUrl, vFlow.TokenUrl);
    vJsonItem.AddPair(c_SwagSecurityDefinitionOAuth2Flow, vFlow.ReturnFlowName(svSwagger2));
    vJsonItem.AddPair(c_SwagSecurityDefinitionOAuth2Scopes, GenerateScopesJsonObject(vFlow.Scopes));
    Exit(vJsonItem);
  end;

  vJsonItem.AddPair(c_SwagSecurityDefinitionOAuth2AuthorizationUrl, fAuthorizationUrl);
  if fTokenUrl.Length > 0 then
    vJsonItem.AddPair(c_SwagSecurityDefinitionOAuth2TokenUrl, fTokenUrl);
  vJsonItem.AddPair(c_SwagSecurityDefinitionOAuth2Flow, ReturnFlowForVersion(svSwagger2));

  if fScopes.Count > 0 then
    vJsonItem.AddPair(c_SwagSecurityDefinitionOAuth2Scopes, GenerateScopesJsonObject(fScopes));

  Result := vJsonItem;
end;

function TSwagSecurityDefinitionOAuth2.GenerateJsonObject(const pVersion: TSwagVersion): TJSONObject;
var
  vJsonFlow: TJSONObject;
  vJsonFlows: TJSONObject;
  vFlow: TSwagSecurityDefinitionOAuth2Flow;
begin
  if pVersion <> svOpenApi3 then
    Exit(GenerateJsonObject);

  Result := TJSONObject.Create;
  Result.AddPair(c_SwagSecurityDefinitionOAuth2Type, ReturnTypeSecurityToString);
  if fDescription.Length > 0 then
    Result.AddPair(c_SwagSecurityDefinitionOAuth2Description, fDescription);

  vJsonFlows := TJSONObject.Create;
  if fFlows.Count > 0 then
  begin
    for vFlow in fFlows do
      if not vFlow.ReturnFlowName(svOpenApi3).IsEmpty then
        vJsonFlows.AddPair(vFlow.ReturnFlowName(svOpenApi3), vFlow.GenerateJsonObject);
  end
  else
  begin
    vJsonFlow := TJSONObject.Create;
    if fAuthorizationUrl.Length > 0 then
      vJsonFlow.AddPair(c_SwagSecurityDefinitionOAuth2AuthorizationUrl, fAuthorizationUrl);
    if fDeviceAuthorizationUrl.Length > 0 then
      vJsonFlow.AddPair(c_SwagSecurityDefinitionOAuth2DeviceAuthorizationUrl, fDeviceAuthorizationUrl);
    if fTokenUrl.Length > 0 then
      vJsonFlow.AddPair(c_SwagSecurityDefinitionOAuth2TokenUrl, fTokenUrl);
    if fRefreshUrl.Length > 0 then
      vJsonFlow.AddPair(c_SwagSecurityDefinitionOAuth2RefreshUrl, fRefreshUrl);
    vJsonFlow.AddPair(c_SwagSecurityDefinitionOAuth2Scopes, GenerateScopesJsonObject(fScopes));
    vJsonFlows.AddPair(ReturnFlowForVersion(svOpenApi3), vJsonFlow);
  end;
  Result.AddPair(c_SwagSecurityDefinitionOAuth2Flows, vJsonFlows);

  if fOAuth2MetadataUrl.Length > 0 then
    Result.AddPair(c_SwagSecurityDefinitionOAuth2MetadataUrl, fOAuth2MetadataUrl);
end;

function TSwagSecurityDefinitionOAuth2.GetTypeSecurity: TSwagSecurityDefinitionType;
begin
  Result := TSwagSecurityDefinitionType.ssdOAuth2;
end;

procedure TSwagSecurityDefinitionOAuth2.LoadScopes(pJsonScopes: TJSONObject;
  pScopes: TObjectList<TSwagSecurityDefinitionOAuth2Scope>);
var
  vScopeIndex: Integer;
  vScope: TSwagSecurityDefinitionOAuth2Scope;
begin
  if not Assigned(pJsonScopes) then
    Exit;

  for vScopeIndex := 0 to pJsonScopes.Count - 1 do
  begin
    vScope := TSwagSecurityDefinitionOAuth2Scope.Create;
    vScope.Load(pJsonScopes.Pairs[vScopeIndex]);
    pScopes.Add(vScope);
  end;
end;

procedure TSwagSecurityDefinitionOAuth2.Load(pJson: TJSONObject);
begin
  if Assigned(pJson.Values[c_SwagSecurityDefinitionOAuth2Description]) then
    fDescription := pJson.Values[c_SwagSecurityDefinitionOAuth2Description].Value;
  if Assigned(pJson.Values[c_SwagSecurityDefinitionOAuth2AuthorizationUrl]) then
    fAuthorizationUrl := pJson.Values[c_SwagSecurityDefinitionOAuth2AuthorizationUrl].Value;
  if Assigned(pJson.Values[c_SwagSecurityDefinitionOAuth2TokenUrl]) then
    fTokenUrl := pJson.Values[c_SwagSecurityDefinitionOAuth2TokenUrl].Value;
  if Assigned(pJson.Values[c_SwagSecurityDefinitionOAuth2Flow]) then
    fFlow := pJson.Values[c_SwagSecurityDefinitionOAuth2Flow].Value;
  if pJson.Values[c_SwagSecurityDefinitionOAuth2Scopes] is TJSONObject then
    LoadScopes(TJSONObject(pJson.Values[c_SwagSecurityDefinitionOAuth2Scopes]), fScopes);
end;

procedure TSwagSecurityDefinitionOAuth2.Load(pJson: TJSONObject; const pVersion: TSwagVersion);
var
  vJsonFlows: TJSONObject;
  vJsonFlow: TJSONObject;
  vIndex: Integer;
  vFlowType: TSwagOAuth2FlowType;
begin
  if pVersion <> svOpenApi3 then
  begin
    Load(pJson);
    Exit;
  end;

  fDescription := TSwagJson.ReadString(pJson, c_SwagSecurityDefinitionOAuth2Description);
  fOAuth2MetadataUrl := TSwagJson.ReadString(pJson, c_SwagSecurityDefinitionOAuth2MetadataUrl);

  vJsonFlows := TSwagJson.ReadObject(pJson, c_SwagSecurityDefinitionOAuth2Flows);
  if not Assigned(vJsonFlows) then
    Exit;

  for vIndex := 0 to vJsonFlows.Count - 1 do
  begin
    vJsonFlow := TSwagJson.AsObject(vJsonFlows.Pairs[vIndex].JsonValue);
    vFlowType := TSwagSecurityDefinitionOAuth2Flow.FlowTypeFromName(vJsonFlows.Pairs[vIndex].JsonString.Value);
    if (not Assigned(vJsonFlow)) or (vFlowType = oftNotDefined) then
      Continue;

    AddFlow(vFlowType).Load(vJsonFlow);

    if fFlow.IsEmpty then
    begin
      fFlow := vJsonFlows.Pairs[vIndex].JsonString.Value;
      fAuthorizationUrl := TSwagJson.ReadString(vJsonFlow, c_SwagSecurityDefinitionOAuth2AuthorizationUrl);
      fDeviceAuthorizationUrl := TSwagJson.ReadString(vJsonFlow, c_SwagSecurityDefinitionOAuth2DeviceAuthorizationUrl);
      fTokenUrl := TSwagJson.ReadString(vJsonFlow, c_SwagSecurityDefinitionOAuth2TokenUrl);
      fRefreshUrl := TSwagJson.ReadString(vJsonFlow, c_SwagSecurityDefinitionOAuth2RefreshUrl);
      LoadScopes(TSwagJson.ReadObject(vJsonFlow, c_SwagSecurityDefinitionOAuth2Scopes), fScopes);
    end;
  end;
end;

initialization
  RegisterClass(TSwagSecurityDefinitionOAuth2);

end.
