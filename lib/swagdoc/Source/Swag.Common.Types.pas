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

unit Swag.Common.Types;

interface

uses
  System.Generics.Collections;

type
  TSwagStatusCode = string;
  TSwagMimeType = string;
  TSwagJsonExampleDescription = string;

  TSwagSecuritySchemeName = string;

  /// <summary>
  /// Allows the definition of a security scheme that can be used by the operations.
  /// Supported schemes are:
  /// * basic authentication
  /// * API key (either as a header or as a query parameter)
  /// * OAuth2's common flows (implicit, password, application and access code).
  /// </summary>
  TSwagSecurityDefinitionType = (ssdNotDefined, ssdBasic, ssdApiKey, ssdOAuth2);
  TSwagSecurityDefinitionsType = set of TSwagSecurityDefinitionType;
  TSwagSecurityScopesSchemaName = string;
  TSwagSecurityScopesSchemaDescription = string;
  TSwagSecurityScopes = TDictionary<TSwagSecurityScopesSchemaName, TSwagSecurityScopesSchemaDescription>;

  /// <summary>
  /// The transfer protocol of the API. Values MUST be from the list: "http", "https", "ws", "wss".
  /// </summary>
  TSwagTransferProtocolScheme = (tpsNotDefined, tpsHttp, tpsHttps, tpsWs, tpsWss);
  TSwagTransferProtocolSchemes = set of TSwagTransferProtocolScheme;

  /// <summary>
  /// * Query - Parameters that are appended to the URL. For example, in /items?id=###, the query parameter is id.
  /// * Header - Custom headers that are expected as part of the request.
  /// * Path - Used together with Path Templating, where the parameter value is actually part of the operation's URL.
  ///        This does not include the host or base path of the API. For example, in /items/{itemId}, the path parameter is itemId.
  /// * Form - Used to describe the payload of an HTTP request when either application/x-www-form-urlencoded, multipart/form-data
  ///          or both are used as the content type of the request (in Swagger's definition, the consumes property of an operation).
  ///          This is the only parameter type that can be used to send files, thus supporting the file type.
  ///          Since form parameters are sent in the payload, they cannot be declared together with a body parameter for the same
  ///          operation. Form parameters have a different format based on the content-type used (for further details,
  ///          consult http://www.w3.org/TR/html401/interact/forms.html#h-17.13.4).
  /// * Body - The payload that's appended to the HTTP request. Since there can only be one payload, there can only be one body parameter.
  ///          The name of the body parameter has no effect on the parameter itself and is used for documentation purposes only.
  ///          Since Form parameters are also in the payload, body and form parameters cannot exist together for the same operation.
  /// </summary>
  TSwagRequestParameterInLocation = (rpiNotDefined, rpiQuery, rpiHeader, rpiPath, rpiFormData, rpiBody);

  TSwagPathTypeOperation = (ohvNotDefined, ohvGet, ohvPost, ohvPut, ohvDelete, ohvOptions, ohvHead, ohvPatch, ohvTrace);

  /// <summary>
  /// The type of the parameter. Since the parameter is not located at the request body, it is limited to
  /// simple types (that is, not an object).
  /// The value MUST be one of "string", "number", "integer", "boolean", "array" or "file".
  /// </summary>
  TSwagTypeParameter = (stpNotDefined, stpString, stpNumber, stpInteger, stpBoolean, stpArray, stpFile);

  ASecurityDefinition = class(TCustomAttribute)
  strict private
    fDefinition: TSwagSecurityDefinitionType;
    function GetName: string;
  public
    constructor Create(const pDefinition: TSwagSecurityDefinitionType);

    property Definition: TSwagSecurityDefinitionType read fDefinition;
    property Name: string read GetName;
  end;

implementation

uses
  Swag.Common.Consts;

{ ASecurityDefinition }

constructor ASecurityDefinition.Create(const pDefinition: TSwagSecurityDefinitionType);
begin
  inherited Create;
  fDefinition := pDefinition;
end;

function ASecurityDefinition.GetName: string;
begin
  Result := c_SwagSecurityDefinitionType[fDefinition];
end;

end.
