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

unit Swag.Doc.SecurityDefinitionMutualTls;

interface

uses
  System.SysUtils,
  System.JSON,
  Swag.Common.Types,
  Swag.Doc.SecurityDefinition;

type
  /// <summary>
  /// The security scheme object for mutual TLS, where the client authenticates with a certificate during the
  /// TLS handshake. It is available in OpenAPI 3 only and it is not written in a Swagger 2.0 document, which has
  /// no equivalent scheme.
  /// </summary>
  [ASecurityDefinition(ssdMutualTls)]
  TSwagSecurityDefinitionMutualTls = class(TSwagSecurityDefinition)
  protected
    function GetTypeSecurity: TSwagSecurityDefinitionType; override;
  public
    function GenerateJsonObject: TJSONObject; overload; override;
    procedure Load(pJson: TJSONObject); overload; override;

    /// <summary>
    /// Returns True only for OpenAPI 3.
    /// </summary>
    function SupportsVersion(const pVersion: TSwagVersion): Boolean; override;
  end;

implementation

uses
  System.Classes;

const
  c_SwagSecurityDefinitionMutualTlsType = 'type';
  c_SwagSecurityDefinitionMutualTlsDescription = 'description';

{ TSwagSecurityDefinitionMutualTls }

function TSwagSecurityDefinitionMutualTls.GenerateJsonObject: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair(c_SwagSecurityDefinitionMutualTlsType, ReturnTypeSecurityToString);
  if fDescription.Length > 0 then
    Result.AddPair(c_SwagSecurityDefinitionMutualTlsDescription, fDescription);
end;

function TSwagSecurityDefinitionMutualTls.GetTypeSecurity: TSwagSecurityDefinitionType;
begin
  Result := ssdMutualTls;
end;

procedure TSwagSecurityDefinitionMutualTls.Load(pJson: TJSONObject);
begin
  if Assigned(pJson.Values[c_SwagSecurityDefinitionMutualTlsDescription]) then
    fDescription := pJson.Values[c_SwagSecurityDefinitionMutualTlsDescription].Value;
end;

function TSwagSecurityDefinitionMutualTls.SupportsVersion(const pVersion: TSwagVersion): Boolean;
begin
  Result := pVersion = svOpenApi3;
end;

initialization
  RegisterClass(TSwagSecurityDefinitionMutualTls);

end.
