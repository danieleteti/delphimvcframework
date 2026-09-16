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

unit Swag.Doc.SecurityDefinitionOpenIdConnect;

interface

uses
  System.SysUtils,
  System.JSON,
  Swag.Common.Types,
  Swag.Doc.SecurityDefinition;

type
  /// <summary>
  /// The security scheme object for OpenID Connect Discovery.
  /// It is available in OpenAPI 3 only. When a Swagger 2.0 document is generated it is written as an API key
  /// sent in the Authorization header and the discovery URL is kept in the x-openIdConnectUrl extension.
  /// </summary>
  [ASecurityDefinition(ssdOpenIdConnect)]
  TSwagSecurityDefinitionOpenIdConnect = class(TSwagSecurityDefinition)
  private
    fOpenIdConnectUrl: string;
  protected
    function GetTypeSecurity: TSwagSecurityDefinitionType; override;
  public
    function GenerateJsonObject: TJSONObject; overload; override;
    function GenerateJsonObject(const pVersion: TSwagVersion): TJSONObject; overload; override;
    procedure Load(pJson: TJSONObject); overload; override;
    procedure Load(pJson: TJSONObject; const pVersion: TSwagVersion); overload; override;

    /// <summary>
    /// Required. OpenId Connect URL to discover OAuth2 configuration values. This MUST be in the form of a URL.
    /// </summary>
    property OpenIdConnectUrl: string read fOpenIdConnectUrl write fOpenIdConnectUrl;
  end;

implementation

uses
  System.Classes,
  Swag.Common.Consts;

const
  c_SwagSecurityDefinitionOpenIdConnectType = 'type';
  c_SwagSecurityDefinitionOpenIdConnectDescription = 'description';
  c_SwagSecurityDefinitionOpenIdConnectUrl = 'openIdConnectUrl';
  c_SwagSecurityDefinitionOpenIdConnectUrlExtension = 'x-openIdConnectUrl';
  c_SwagSecurityDefinitionOpenIdConnectIn = 'in';
  c_SwagSecurityDefinitionOpenIdConnectName = 'name';
  c_SwagSecurityDefinitionOpenIdConnectInHeader = 'header';
  c_SwagSecurityDefinitionOpenIdConnectAuthorizationHeader = 'Authorization';

{ TSwagSecurityDefinitionOpenIdConnect }

function TSwagSecurityDefinitionOpenIdConnect.GenerateJsonObject: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair(c_SwagSecurityDefinitionOpenIdConnectType, c_SwagSecurityDefinitionType[ssdApiKey]);
  if fDescription.Length > 0 then
    Result.AddPair(c_SwagSecurityDefinitionOpenIdConnectDescription, fDescription);
  Result.AddPair(c_SwagSecurityDefinitionOpenIdConnectIn, c_SwagSecurityDefinitionOpenIdConnectInHeader);
  Result.AddPair(c_SwagSecurityDefinitionOpenIdConnectName, c_SwagSecurityDefinitionOpenIdConnectAuthorizationHeader);
  if fOpenIdConnectUrl.Length > 0 then
    Result.AddPair(c_SwagSecurityDefinitionOpenIdConnectUrlExtension, fOpenIdConnectUrl);
end;

function TSwagSecurityDefinitionOpenIdConnect.GenerateJsonObject(const pVersion: TSwagVersion): TJSONObject;
begin
  if pVersion <> svOpenApi3 then
    Exit(GenerateJsonObject);

  Result := TJSONObject.Create;
  Result.AddPair(c_SwagSecurityDefinitionOpenIdConnectType, ReturnTypeSecurityToString);
  if fDescription.Length > 0 then
    Result.AddPair(c_SwagSecurityDefinitionOpenIdConnectDescription, fDescription);
  Result.AddPair(c_SwagSecurityDefinitionOpenIdConnectUrl, fOpenIdConnectUrl);
end;

function TSwagSecurityDefinitionOpenIdConnect.GetTypeSecurity: TSwagSecurityDefinitionType;
begin
  Result := ssdOpenIdConnect;
end;

procedure TSwagSecurityDefinitionOpenIdConnect.Load(pJson: TJSONObject);
begin
  if Assigned(pJson.Values[c_SwagSecurityDefinitionOpenIdConnectDescription]) then
    fDescription := pJson.Values[c_SwagSecurityDefinitionOpenIdConnectDescription].Value;
  if Assigned(pJson.Values[c_SwagSecurityDefinitionOpenIdConnectUrlExtension]) then
    fOpenIdConnectUrl := pJson.Values[c_SwagSecurityDefinitionOpenIdConnectUrlExtension].Value;
  fExtensions.Remove(c_SwagSecurityDefinitionOpenIdConnectUrlExtension);
end;

procedure TSwagSecurityDefinitionOpenIdConnect.Load(pJson: TJSONObject; const pVersion: TSwagVersion);
begin
  Load(pJson);
  if Assigned(pJson.Values[c_SwagSecurityDefinitionOpenIdConnectUrl]) then
    fOpenIdConnectUrl := pJson.Values[c_SwagSecurityDefinitionOpenIdConnectUrl].Value;
end;

initialization
  RegisterClass(TSwagSecurityDefinitionOpenIdConnect);

end.
