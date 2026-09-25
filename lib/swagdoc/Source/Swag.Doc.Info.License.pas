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

unit Swag.Doc.Info.License;

interface

uses
  System.JSON,
  Swag.Common.Types,
  Swag.Doc.Extensions;

type
  /// <summary>
  /// License information for the exposed API.
  /// </summary>
  TSwagInfoLicense = class(TObject)
  private
    fName: string;
    fUrl: string;
    fIdentifier: string;
    fExtensions: TSwagExtensions;
    function ReturnIdentifierUrl: string;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    /// <summary>
    /// Generates the Swagger 2.0 license object. When only the Identifier is defined, the url field points to the
    /// SPDX page of the license.
    /// </summary>
    function GenerateJsonObject: TJsonObject; overload;

    /// <summary>
    /// Generates the license object for the given specification family. In OpenAPI 3 the identifier and the url
    /// fields are mutually exclusive, so the url is only written when the Identifier is not defined.
    /// </summary>
    function GenerateJsonObject(const pVersion: TSwagVersion): TJsonObject; overload;

    procedure Load(pJson: TJsonObject);
    function isEmpty: Boolean;

    /// <summary>
    /// Required. The license name used for the API.
    /// </summary>
    property Name: string read fName write fName;

    /// <summary>
    /// A URL to the license used for the API. MUST be in the format of a URL.
    /// </summary>
    property Url: string read fUrl write fUrl;

    /// <summary>
    /// An SPDX license expression for the API, for example Apache-2.0 or MIT. Available in OpenAPI 3 only.
    /// </summary>
    property Identifier: string read fIdentifier write fIdentifier;

    /// <summary>
    /// The Specification Extensions of the license.
    /// </summary>
    property Extensions: TSwagExtensions read fExtensions;
  end;

implementation

uses
  System.SysUtils;

const
  c_SwagInfoLicenseName = 'name';
  c_SwagInfoLicenseUrl = 'url';
  c_SwagInfoLicenseIdentifier = 'identifier';
  c_SwagInfoLicenseSpdxUrlPrefix = 'https://spdx.org/licenses/';
  c_SwagInfoLicenseSpdxUrlSuffix = '.html';

{ TSwagInfoLicense }

constructor TSwagInfoLicense.Create;
begin
  inherited Create;
  fExtensions := TSwagExtensions.Create;
end;

destructor TSwagInfoLicense.Destroy;
begin
  FreeAndNil(fExtensions);
  inherited Destroy;
end;

function TSwagInfoLicense.GenerateJsonObject: TJsonObject;
begin
  Result := TJsonObject.Create;
  Result.AddPair(c_SwagInfoLicenseName, fName);
  if fUrl.IsEmpty and (not fIdentifier.IsEmpty) then
    Result.AddPair(c_SwagInfoLicenseUrl, ReturnIdentifierUrl)
  else
    Result.AddPair(c_SwagInfoLicenseUrl, fUrl);
  fExtensions.WriteTo(Result);
end;

function TSwagInfoLicense.GenerateJsonObject(const pVersion: TSwagVersion): TJsonObject;
begin
  if pVersion <> svOpenApi3 then
    Exit(GenerateJsonObject);

  Result := TJsonObject.Create;
  Result.AddPair(c_SwagInfoLicenseName, fName);
  if not fIdentifier.IsEmpty then
    Result.AddPair(c_SwagInfoLicenseIdentifier, fIdentifier)
  else if not fUrl.IsEmpty then
    Result.AddPair(c_SwagInfoLicenseUrl, fUrl);
  fExtensions.WriteTo(Result);
end;

function TSwagInfoLicense.isEmpty: Boolean;
begin
  Result := fName.IsEmpty and fUrl.IsEmpty and fIdentifier.IsEmpty;
end;

procedure TSwagInfoLicense.Load(pJson: TJsonObject);
begin
  if not Assigned(pJson) then
    Exit;

  if Assigned(pJson.Values[c_SwagInfoLicenseName]) then
    fName := pJson.Values[c_SwagInfoLicenseName].Value;

  if Assigned(pJson.Values[c_SwagInfoLicenseUrl]) then
    fUrl := pJson.Values[c_SwagInfoLicenseUrl].Value;

  if Assigned(pJson.Values[c_SwagInfoLicenseIdentifier]) then
    fIdentifier := pJson.Values[c_SwagInfoLicenseIdentifier].Value;

  fExtensions.ReadFrom(pJson);
end;

function TSwagInfoLicense.ReturnIdentifierUrl: string;
begin
  if fIdentifier.Contains(' ') then
    Result := EmptyStr
  else
    Result := c_SwagInfoLicenseSpdxUrlPrefix + fIdentifier + c_SwagInfoLicenseSpdxUrlSuffix;
end;

end.
