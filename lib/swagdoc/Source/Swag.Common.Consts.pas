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
  c_SwaggerVersion = '2.0';
  c_SwagTransferProtocolScheme: array[TSwagTransferProtocolScheme] of string = ('', 'http', 'https', 'ws', 'wss');
  c_SwagSecurityDefinitionType: array[TSwagSecurityDefinitionType] of string = ('', 'basic', 'apiKey', 'oauth2');
  c_SwagPathOperationHttpVerbs: array[TSwagPathTypeOperation] of string =
    ('', 'get', 'post', 'put', 'delete', 'options', 'head', 'patch', 'trace');
  c_SwagRequestParameterInLocation: array[TSwagRequestParameterInLocation] of string =
    ('', 'query', 'header', 'path', 'formData', 'body');
  c_SwagTypeParameter: array[TSwagTypeParameter] of string = ('', 'string', 'number', 'integer', 'boolean', 'array', 'file');

implementation

end.