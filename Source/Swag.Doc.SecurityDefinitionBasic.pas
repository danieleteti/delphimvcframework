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

unit Swag.Doc.SecurityDefinitionBasic;

interface

uses
  System.SysUtils,
  System.JSON,
  Swag.Common.Types,
  Swag.Doc.SecurityDefinition;

type
  /// <summary>
  /// The security scheme object Basic
  /// </summary>
  [ASecurityDefinition(ssdBasic)]
  TSwagSecurityDefinitionBasic = class(TSwagSecurityDefinition)
  private
    fName: string;
  protected
    function GetTypeSecurity: TSwagSecurityDefinitionType; override;
  public
    function GenerateJsonObject: TJSONObject; override;
    procedure Load(pJson: TJSONObject); override;

    property Name: string read fName write fName;
  end;

implementation

uses
  System.Classes;

const
  c_SwagSecurityDefinitionBasicType = 'type';
  c_SwagSecurityDefinitionBasicName = 'name';

{ TSwagSecurityDefinitionApiKey }

function TSwagSecurityDefinitionBasic.GenerateJsonObject: TJSONObject;
var
  vJsonItem: TJsonObject;
begin
  vJsonItem := TJsonObject.Create;
  vJsonItem.AddPair(c_SwagSecurityDefinitionBasicType, ReturnTypeSecurityToString);
  Result := vJsonItem;
end;

function TSwagSecurityDefinitionBasic.GetTypeSecurity: TSwagSecurityDefinitionType;
begin
  Result := ssdBasic;
end;

procedure TSwagSecurityDefinitionBasic.Load(pJson: TJSONObject);
begin
  if Assigned(pJson.Values[c_SwagSecurityDefinitionBasicName]) then
    fName := pJson.Values[c_SwagSecurityDefinitionBasicName].Value;
end;

initialization
  RegisterClass(TSwagSecurityDefinitionBasic);

end.
