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
  /// The family of the specification used to generate or to load a document.
  /// * svSwagger2 - Swagger Specification 2.0. It is the default value, so the applications written for the previous
  ///   releases of the library keep producing the same document.
  /// * svOpenApi3 - OpenAPI Specification 3. The document is written according to the latest 3.x release supported by
  ///   the library, which is returned by the SwaggerVersion property of TSwagDoc.
  /// The releases of a family are supported by the same value. A new value is only created for a new family of the
  /// specification, or for a release that is incompatible with the documents already produced by its family.
  /// </summary>
  TSwagVersion = (svSwagger2, svOpenApi3);

  /// <summary>
  /// Allows the definition of a security scheme that can be used by the operations.
  /// Supported schemes are:
  /// * basic authentication
  /// * API key (either as a header, as a query parameter or, in OpenAPI 3, as a cookie)
  /// * OAuth2's common flows (implicit, password, application and access code).
  /// * HTTP authentication schemes defined by RFC9110, such as bearer (OpenAPI 3 only)
  /// * OpenID Connect Discovery (OpenAPI 3 only)
  /// * Mutual TLS, where the client authenticates with a certificate (OpenAPI 3 only)
  /// </summary>
  TSwagSecurityDefinitionType = (ssdNotDefined, ssdBasic, ssdApiKey, ssdOAuth2, ssdHttp, ssdOpenIdConnect, ssdMutualTls);
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
  ///          In OpenAPI 3 the form parameters are written as the request body of the operation.
  /// * Body - The payload that's appended to the HTTP request. Since there can only be one payload, there can only be one body parameter.
  ///          The name of the body parameter has no effect on the parameter itself and is used for documentation purposes only.
  ///          Since Form parameters are also in the payload, body and form parameters cannot exist together for the same operation.
  ///          In OpenAPI 3 the body parameter is written as the request body of the operation.
  /// * Cookie - Used to pass a specific cookie value to the API. Available in OpenAPI 3 only.
  /// * QueryString - Treats the entire URL query string as a single value, described by a media type and a schema.
  ///          It MUST NOT appear together with query parameters in the same operation. Available in OpenAPI 3 only.
  /// </summary>
  TSwagRequestParameterInLocation = (rpiNotDefined, rpiQuery, rpiHeader, rpiPath, rpiFormData, rpiBody, rpiCookie,
    rpiQueryString);

  /// <summary>
  /// The HTTP method of an operation. The QUERY method, defined by RFC10008, is available in OpenAPI 3 only.
  /// </summary>
  TSwagPathTypeOperation = (ohvNotDefined, ohvGet, ohvPost, ohvPut, ohvDelete, ohvOptions, ohvHead, ohvPatch, ohvTrace,
    ohvQuery);

  /// <summary>
  /// The type of the parameter. Since the parameter is not located at the request body, it is limited to
  /// simple types (that is, not an object).
  /// The value MUST be one of "string", "number", "integer", "boolean", "array" or "file".
  /// </summary>
  TSwagTypeParameter = (stpNotDefined, stpString, stpNumber, stpInteger, stpBoolean, stpArray, stpFile);

  /// <summary>
  /// Describes how the parameter value will be serialized depending on the type of the parameter value.
  /// Default values (based on value of in): for query - form; for path - simple; for header - simple; for cookie - form.
  /// Available in OpenAPI 3 only.
  /// </summary>
  TSwagRequestParameterStyle = (rpsNotDefined, rpsMatrix, rpsLabel, rpsSimple, rpsForm, rpsSpaceDelimited,
    rpsPipeDelimited, rpsDeepObject);

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
