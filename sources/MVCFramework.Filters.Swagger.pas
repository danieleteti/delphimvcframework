// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ***************************************************************************
//
// The Swagger HTTP filter. A companion of MVCFramework.Filters, in a unit of
// its own so that the filters do not depend on SwagDoc.

unit MVCFramework.Filters.Swagger;

{$I dmvcframework.inc}

interface

uses
  MVCFramework,
  MVCFramework.Commons,                        // TMVCTransferProtocolSchemes
  MVCFramework.Swagger.Commons;                // TMVCSwaggerInfo, JWT_DEFAULT_DESCRIPTION

type
  // The authentications the document declares, for the Authorize button of Swagger UI.
  TMVCSwaggerAuthentication = (
    sauBasic,  // HTTP Basic
    sauBearer  // a JWT in "Authorization: Bearer <token>"
  );
  TMVCSwaggerAuthentications = set of TMVCSwaggerAuthentication;

// Swagger HTTPFilter — exposes the Swagger JSON document at AURL: Swagger 2.0
// by default, OpenAPI 3 with ASpecVersion = ssvOpenAPI3 (same URL, same
// attributes). Same document as TMVCSwaggerMiddleware: auto-discovers the JWT
// middleware and JWTAuthenticationBundle (the bearer scheme, and the login
// unless LoginURL is '' or the bundle has a refresh token store), supports
// MVCSwagAuthentication / MVCSwagJSONSchemaField /
// MVCSWAGDefaultModel / MVCRequiresAuthentication / TMVCActiveRecordController
// CRUD doc generation — features the newer OpenAPI 3 helper does not yet cover.
// SwagDoc is required.
//
//   lEngine.Use(SwaggerHTTPFilter(lEngine, lInfo, '/openapi.json', JWT_DEFAULT_DESCRIPTION,
//     [sauBearer], '', '', '', [psHTTP, psHTTPS], ssvOpenAPI3));
function SwaggerHTTPFilter(AEngine: TMVCEngine;
  const AInfo: TMVCSwaggerInfo;
  const AURL: string = '/swagger.json';
  const AJWTDescription: string = JWT_DEFAULT_DESCRIPTION;
  const AAuthentications: TMVCSwaggerAuthentications = [];
  const AHost: string = '';
  const ABasePath: string = '';
  const APathFilter: string = '';
  const ATransferProtocolSchemes: TMVCTransferProtocolSchemes = [psHTTP, psHTTPS];
  const ASpecVersion: TMVCSwaggerSpecVersion = ssvSwagger2): TMVCHTTPFilter;

implementation

uses
  MVCFramework.Filters.Swagger.Internal;       // TMVCSwaggerDocument

function SwaggerHTTPFilter(AEngine: TMVCEngine;
  const AInfo: TMVCSwaggerInfo;
  const AURL: string;
  const AJWTDescription: string;
  const AAuthentications: TMVCSwaggerAuthentications;
  const AHost: string;
  const ABasePath: string;
  const APathFilter: string;
  const ATransferProtocolSchemes: TMVCTransferProtocolSchemes;
  const ASpecVersion: TMVCSwaggerSpecVersion): TMVCHTTPFilter;
var
  lDocument: TMVCSwaggerDocument;
begin
  lDocument := TMVCSwaggerDocument.Create(AEngine, AInfo, AURL, AJWTDescription,
    AAuthentications, AHost, ABasePath, APathFilter, ATransferProtocolSchemes, ASpecVersion);
  Result :=
    procedure (const AContext: TWebContext;
               const ANext: TMVCHTTPFilterNext)
    begin
      if lDocument.TryServe(AContext) then
        Exit;
      ANext();
    end;
end;

end.
