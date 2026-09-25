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

unit Swag.Common.Consts;

interface

uses
  Swag.Common.Types;

const
  c_SwaggerFileName = 'swagger.json';
  c_OpenApiFileName = 'openapi.json';
  c_SwaggerVersion = '2.0';
  c_OpenApiVersion = '3.2.1';
  c_SwagSpecVersion: array[TSwagVersion] of string = (c_SwaggerVersion, c_OpenApiVersion);
  c_SwagSpecFileName: array[TSwagVersion] of string = (c_SwaggerFileName, c_OpenApiFileName);
  c_SwagMimeTypeJson = 'application/json';
  c_SwagMimeTypeFormUrlEncoded = 'application/x-www-form-urlencoded';
  c_SwagMimeTypeMultipartFormData = 'multipart/form-data';
  c_SwagTransferProtocolScheme: array[TSwagTransferProtocolScheme] of string = ('', 'http', 'https', 'ws', 'wss');
  c_SwagSecurityDefinitionType: array[TSwagSecurityDefinitionType] of string =
    ('', 'basic', 'apiKey', 'oauth2', 'http', 'openIdConnect', 'mutualTLS');
  c_SwagPathOperationHttpVerbs: array[TSwagPathTypeOperation] of string =
    ('', 'get', 'post', 'put', 'delete', 'options', 'head', 'patch', 'trace', 'query');
  c_SwagRequestParameterInLocation: array[TSwagRequestParameterInLocation] of string =
    ('', 'query', 'header', 'path', 'formData', 'body', 'cookie', 'querystring');
  c_SwagTypeParameter: array[TSwagTypeParameter] of string = ('', 'string', 'number', 'integer', 'boolean', 'array', 'file');
  c_SwagRequestParameterStyle: array[TSwagRequestParameterStyle] of string =
    ('', 'matrix', 'label', 'simple', 'form', 'spaceDelimited', 'pipeDelimited', 'deepObject');
  c_SwagOAuth2FlowsSwagger2: array[0..3] of string = ('implicit', 'password', 'application', 'accessCode');
  c_SwagOAuth2FlowsOpenApi3: array[0..3] of string = ('implicit', 'password', 'clientCredentials', 'authorizationCode');
  c_SwagOAuth2FlowDeviceAuthorization = 'deviceAuthorization';

implementation

end.
