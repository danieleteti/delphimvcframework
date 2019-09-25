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

unit Swag.Doc.Info;

interface

uses
  System.JSON,
  Swag.Doc.Info.License,
  Swag.Doc.Info.Contact;

type
  /// <summary>
  /// The object provides metadata about the API.
  /// The metadata can be used by the clients if needed, and can be presented in the Swagger-UI for convenience.
  /// </summary>
  TSwagInfo = class(TObject)
  private
    fVersion: string;
    fTitle: string;
    fDescription: string;
    fContact: TSwagInfoContact;
    fTermsOfService: string;
    fLicense: TSwagInfoLicense;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    function GenerateJsonObject: TJSONObject;
    procedure Load(pJson: TJSONObject);

    /// <summary>
    /// Required. The title of the application.
    /// </summary>
    property Title: string read fTitle write fTitle;

    /// <summary>
    /// A short description of the application. GFM syntax can be used for rich text representation.
    /// </summary>
    property Description: string read fDescription write fDescription;

    /// <summary>
    /// The Terms of Service for the API.
    /// </summary>
    property TermsOfService: string read fTermsOfService write fTermsOfService;

    /// <summary>
    /// The contact information for the exposed API.
    /// </summary>
    property Contact: TSwagInfoContact read fContact write fContact;

    /// <summary>
    /// The license information for the exposed API.
    /// </summary>
    property License: TSwagInfoLicense read fLicense;

    /// <summary>
    /// Required Provides the version of the application API (not to be confused with the specification version).
    /// </summary>
    property Version: string read fVersion write fVersion;
  end;

implementation

uses
  System.SysUtils;

const
  c_SwagInfoVersion = 'version';
  c_SwagInfoTitle = 'title';
  c_SwagInfoDescription = 'description';
  c_SwagInfoContact = 'contact';
  c_SwagInfoTermsOfService = 'termsOfService';
  c_SwagLicense = 'license';

{ TSwagInfo }

procedure TSwagInfo.Load(pJson: TJSONObject);
begin
  if Assigned(pJson.Values[c_SwagInfoVersion]) then
    fVersion := pJson.Values[c_SwagInfoVersion].Value;

  if Assigned(pJson.Values[c_SwagInfoTitle]) then
    fTitle := pJson.Values[c_SwagInfoTitle].Value;

  if Assigned(pJson.Values[c_SwagInfoDescription]) then
    fDescription := pJson.Values[c_SwagInfoDescription].Value;

  if Assigned(pJson.Values[c_SwagInfoTermsOfService]) then
    FTermsOfService := pJson.Values[c_SwagInfoTermsOfService].Value;

  if Assigned(pJson.Values[c_SwagInfoContact]) then
    fContact.Load(pJson.Values[c_SwagInfoContact] as TJSONObject);

  fLicense.Load((pJson as TJSONObject).Values[c_SwagLicense] as TJSONObject);
end;

constructor TSwagInfo.Create;
begin
  inherited Create;
  fContact := TSwagInfoContact.Create;
  fLicense := TSwagInfoLicense.Create;
end;

destructor TSwagInfo.Destroy;
begin
  FreeAndNil(fContact);
  FreeAndNil(fLicense);
  inherited Destroy;
end;

function TSwagInfo.GenerateJsonObject: TJSONObject;
begin
  Result := TJsonObject.Create;
  Result.AddPair(c_SwagInfoVersion, fVersion);
  Result.AddPair(c_SwagInfoTitle, fTitle);
  Result.AddPair(c_SwagInfoDescription, fDescription);
  if not fTermsOfService.IsEmpty then
    Result.AddPair(c_SwagInfoTermsOfService, fTermsOfService);
  if not fContact.IsEmpty then
    Result.AddPair(c_SwagInfoContact, fContact.GenerateJsonObject);
  if not fLicense.isEmpty then
    Result.AddPair(c_SwagLicense,fLicense.GenerateJsonObject);
end;


end.
