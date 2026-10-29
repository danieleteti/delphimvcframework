// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// Collaborators on this file:
// Jo�o Ant�nio Duarte (https://github.com/joaoduarte19)
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
// *************************************************************************** }

unit MVCFramework.Middleware.Swagger;

{$I dmvcframework.inc}

{$WARN SYMBOL_DEPRECATED OFF} // the class is deprecated for its users, not inside its own unit

interface

uses
  MVCFramework,
  MVCFramework.Swagger.Commons,
  MVCFramework.Commons,
  MVCFramework.Filters.Swagger.Internal;       // TMVCSwaggerDocument

type
  TMVCSwaggerMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    fDocument: TMVCSwaggerDocument;
  public
    constructor Create(
      const AEngine: TMVCEngine;
      const ASwaggerInfo: TMVCSwaggerInfo;
      const ASwaggerDocumentationURL: string = '/swagger.json';
      const AJWTDescription: string = JWT_DEFAULT_DESCRIPTION;
      const AEnableBasicAuthentication: Boolean = False;
      const AHost: string = '';
      const ABasePath: string = '';
      const APathFilter: String = '';
      const ATransferProtocolSchemes: TMVCTransferProtocolSchemes = [psHTTP, psHTTPS];
      const AEnableBearerAuthentication: Boolean = False;
      const ASpecVersion: TMVCSwaggerSpecVersion = ssvSwagger2);
    destructor Destroy; override;
    /// <summary>
    /// Host written in a Swagger 2.0 document when none is configured: the request
    /// Host, plus the server port when the Host header carries no port of its own.
    /// </summary>
    class function DocumentHost(const ARequestHost: string; const AServerPort: Integer): string; static;
    procedure OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
    procedure OnBeforeControllerAction(AContext: TWebContext; const AControllerQualifiedClassName: string;
      const AActionName: string; var AHandled: Boolean);
    procedure OnAfterControllerAction(AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      const AHandled: Boolean);
    procedure OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
  end deprecated 'Use the Swagger HTTP filter (MVCFramework.Filters.Swagger): AEngine.Use(SwaggerHTTPFilter(...))';

implementation

uses
  MVCFramework.Filters.Swagger;                // TMVCSwaggerAuthentications

{ TMVCSwaggerMiddleware }

class function TMVCSwaggerMiddleware.DocumentHost(const ARequestHost: string;
  const AServerPort: Integer): string;
begin
  Result := TMVCSwaggerDocument.DocumentHost(ARequestHost, AServerPort);
end;

constructor TMVCSwaggerMiddleware.Create(const AEngine: TMVCEngine; const ASwaggerInfo: TMVCSwaggerInfo;
  const ASwaggerDocumentationURL, AJWTDescription: string; const AEnableBasicAuthentication: Boolean;
  const AHost, ABasePath: string;
  const APathFilter: String;
  const ATransferProtocolSchemes: TMVCTransferProtocolSchemes; const AEnableBearerAuthentication: Boolean;
  const ASpecVersion: TMVCSwaggerSpecVersion);
var
  lAuthentications: TMVCSwaggerAuthentications;
begin
  inherited Create;
  lAuthentications := [];
  if AEnableBasicAuthentication then
    Include(lAuthentications, sauBasic);
  if AEnableBearerAuthentication then
    Include(lAuthentications, sauBearer);
  fDocument := TMVCSwaggerDocument.Create(AEngine, ASwaggerInfo, ASwaggerDocumentationURL, AJWTDescription,
    lAuthentications, AHost, ABasePath, APathFilter, ATransferProtocolSchemes, ASpecVersion);
end;

destructor TMVCSwaggerMiddleware.Destroy;
begin
  inherited Destroy;
end;

procedure TMVCSwaggerMiddleware.OnAfterControllerAction(AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      const AHandled: Boolean);
begin
  // do nothing
end;

procedure TMVCSwaggerMiddleware.OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
begin
  // do nothing
end;

procedure TMVCSwaggerMiddleware.OnBeforeControllerAction(AContext: TWebContext;
  const AControllerQualifiedClassName, AActionName: string; var AHandled: Boolean);
begin
  // do nothing
end;

procedure TMVCSwaggerMiddleware.OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
begin
  if fDocument.TryServe(AContext) then
    AHandled := True;
end;

end.
