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

unit Swag.Doc.SecurityDefinitionApiKey;

interface

uses
  System.SysUtils,
  System.JSON,
  Swag.Common.Types,
  Swag.Doc.SecurityDefinition;

type
  TSwagSecurityDefinitionApiKeyInLocation = (kilNotDefined, kilQuery, kilHeader);

type
  /// <summary>
  /// The security scheme object API key (either as a header or as a query parameter)
  /// </summary>
  [ASecurityDefinition(ssdApiKey)]
  TSwagSecurityDefinitionApiKey = class(TSwagSecurityDefinition)
  private
    fName: string;
    fInLocation: TSwagSecurityDefinitionApiKeyInLocation;
  protected
    function GetTypeSecurity: TSwagSecurityDefinitionType; override;
  public
    function GenerateJsonObject: TJSONObject; override;
    procedure Load(pJson: TJSONObject); override;
    /// <summary>
    /// Required The location of the API key. Valid values are "query" or "header".
    /// </summary>
    property InLocation: TSwagSecurityDefinitionApiKeyInLocation read fInLocation write fInLocation;

    /// <summary>
    /// Required. The name of the header or query parameter to be used.
    /// </summary>
    property Name: string read fName write fName;
  end;

implementation

uses
  System.Classes;

const
  c_SwagSecurityDefinitionApiKeyType = 'type';
  c_SwagSecurityDefinitionApiKeyDescription = 'description';
  c_SwagSecurityDefinitionApiKeyIn = 'in';
  c_SwagSecurityDefinitionApiKeyName = 'name';

  c_SwagSecurityDefinitionApiKeyInLocation: array[TSwagSecurityDefinitionApiKeyInLocation] of string =
    ('', 'query', 'header');

{ TSwagSecurityDefinitionApiKey }

function TSwagSecurityDefinitionApiKey.GenerateJsonObject: TJSONObject;
var
  vJsonItem: TJsonObject;
begin
  vJsonItem := TJsonObject.Create;
  vJsonItem.AddPair(c_SwagSecurityDefinitionApiKeyType, ReturnTypeSecurityToString);
  if fDescription.Length > 0 then
    vJsonItem.AddPair(c_SwagSecurityDefinitionApiKeyDescription, fDescription);
  vJsonItem.AddPair(c_SwagSecurityDefinitionApiKeyIn, c_SwagSecurityDefinitionApiKeyInLocation[fInLocation]);
  vJsonItem.AddPair(c_SwagSecurityDefinitionApiKeyName, fName);

  Result := vJsonItem;
end;

function TSwagSecurityDefinitionApiKey.GetTypeSecurity: TSwagSecurityDefinitionType;
begin
  Result := ssdApiKey;
end;

procedure TSwagSecurityDefinitionApiKey.Load(pJson: TJSONObject);
var
  vIn: string;
begin
  if Assigned(pJson.Values[c_SwagSecurityDefinitionApiKeyDescription]) then
    fDescription := pJson.Values[c_SwagSecurityDefinitionApiKeyDescription].Value;
  if Assigned(pJson.Values[c_SwagSecurityDefinitionApiKeyName]) then
    fName := pJson.Values[c_SwagSecurityDefinitionApiKeyName].Value;
  if Assigned(pJson.Values[c_SwagSecurityDefinitionApiKeyIn]) then
  begin
    vIn := pJson.Values[c_SwagSecurityDefinitionApiKeyIn].Value;
    if vIn.ToLower = c_SwagSecurityDefinitionApiKeyInLocation[kilQuery] then
      fInLocation := kilQuery
    else if vIn.ToLower = c_SwagSecurityDefinitionApiKeyInLocation[kilHeader] then
      fInLocation := kilHeader
    else
      fInLocation := kilNotDefined;
  end;
end;

initialization
  RegisterClass(TSwagSecurityDefinitionApiKey);

end.
