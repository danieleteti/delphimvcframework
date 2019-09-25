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

unit Swag.Doc.SecurityDefinition;

interface

uses
  System.JSON,
  Swag.Common.Types;

type
  /// <summary>
  /// A declaration of the security schemes available to be used in the specification.
  /// This does not enforce the security schemes on the operations and only serves to provide the relevant details for each scheme.
  /// </summary>
  TSwagSecurityDefinition = class abstract(TObject)
  protected
    fSchemaName: TSwagSecuritySchemaName;
    fDescription: string;

    function GetTypeSecurity: TSwagSecurityDefinitionType; virtual; abstract;
    function ReturnTypeSecurityToString: string; virtual;
  public
    function GenerateJsonObject: TJSONObject; virtual; abstract;

    /// <summary>
    /// A single security scheme definition, mapping a "name" to the scheme it defines.
    /// </summary>
    property SchemaName: TSwagSecuritySchemaName read fSchemaName write fSchemaName;

    /// <summary>
    /// Required. The type of the security scheme. Valid values are "basic", "apiKey" or "oauth2".
    /// </summary>
    property TypeSecurity: TSwagSecurityDefinitionType read GetTypeSecurity;

    /// <summary>
    /// A short description for security scheme.
    /// </summary>
    property Description: string read fDescription write fDescription;
  end;

implementation

uses
  Swag.Common.Consts;

{ TSwagSecurityDefinition }

function TSwagSecurityDefinition.ReturnTypeSecurityToString: string;
begin
  Result := c_SwagSecurityDefinitionType[GetTypeSecurity];
end;

end.
