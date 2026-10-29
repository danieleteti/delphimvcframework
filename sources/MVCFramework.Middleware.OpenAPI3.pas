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
// HTTP middleware that exposes an OpenAPI 3.1 document over a configurable
// URL (default: /openapi.json). Auto-discovers sources on each request so
// the document always reflects the live route set:
//
//   * Minimal API routes — via TMVCMinimalAPIOpenAPISource, picked up from
//     the TMVCMinimalAPIMiddleware on the engine (if present).
//
// Explicit additional sources can be registered via AddSource() — useful for
// custom controller-source implementations or out-of-band route catalogs.
//
// Usage:
//
//   var lOAInfo: TMVCOpenAPIInfo;
//   lOAInfo.Title := 'My API';
//   lOAInfo.Version := '1.0';
//   lEngine.AddMiddleware(TMVCOpenAPI3Middleware.Create(lEngine, lOAInfo));
//
// ***************************************************************************

unit MVCFramework.Middleware.OpenAPI3;

{$I dmvcframework.inc}

{$WARN SYMBOL_DEPRECATED OFF} // the class is deprecated for its users, not inside its own unit

interface

uses
  System.Generics.Collections,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.OpenAPI3;

type
  TMVCOpenAPI3Middleware = class(TInterfacedObject, IMVCMiddleware)
  strict private
    fEngine: TMVCEngine;
    fInfo: TMVCOpenAPIInfo;
    fURL: string;
    fExtraSources: TList<IMVCOpenAPISource>;
    procedure RenderJSON(const AContent: string; AContext: TWebContext);
    function BuildDocumentJSON: string;
  protected
    procedure OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
    procedure OnBeforeControllerAction(AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      var AHandled: Boolean);
    procedure OnAfterControllerAction(AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      const AHandled: Boolean);
    procedure OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
  public
    constructor Create(AEngine: TMVCEngine; const AInfo: TMVCOpenAPIInfo;
      const AURL: string = '/openapi.json');
    destructor Destroy; override;
    // Register additional document sources. Built-in sources (currently:
    // Minimal API routes) are discovered automatically per-request — do not
    // add them here.
    procedure AddSource(const ASource: IMVCOpenAPISource);
  end deprecated 'Use the OpenAPI HTTP filter (MVCFramework.Filters.OpenAPI): AEngine.Use(OpenAPIHTTPFilter(...))';

implementation

uses
  System.SysUtils,
  System.Classes,
  MVCFramework.Filters.OpenAPI;                // OpenAPIDocumentJSON

{ TMVCOpenAPI3Middleware }

constructor TMVCOpenAPI3Middleware.Create(AEngine: TMVCEngine;
  const AInfo: TMVCOpenAPIInfo; const AURL: string);
begin
  inherited Create;
  fEngine := AEngine;
  fInfo := AInfo;
  fURL := AURL;
  fExtraSources := TList<IMVCOpenAPISource>.Create;
end;

destructor TMVCOpenAPI3Middleware.Destroy;
begin
  fExtraSources.Free;
  inherited;
end;

procedure TMVCOpenAPI3Middleware.AddSource(const ASource: IMVCOpenAPISource);
begin
  if ASource <> nil then
    fExtraSources.Add(ASource);
end;

procedure TMVCOpenAPI3Middleware.RenderJSON(const AContent: string;
  AContext: TWebContext);
var
  lContentType: string;
begin
  lContentType := BuildContentType(TMVCMediaType.APPLICATION_JSON, TMVCCharSet.UTF_8);
  AContext.Response.ContentType := lContentType;
  AContext.Response.SetContentStream(
    TStringStream.Create(AContent, TEncoding.UTF8), lContentType);
end;

function TMVCOpenAPI3Middleware.BuildDocumentJSON: string;
begin
  Result := OpenAPIDocumentJSON(fEngine, fInfo, fExtraSources.ToArray);
end;

procedure TMVCOpenAPI3Middleware.OnBeforeRouting(AContext: TWebContext;
  var AHandled: Boolean);
var
  lJSON: string;
begin
  if SameText(AContext.Request.PathInfo, fURL)
    and (AContext.Request.HTTPMethod = httpGET) then
  begin
    lJSON := BuildDocumentJSON;
    RenderJSON(lJSON, AContext);
    AHandled := True;
  end;
end;

procedure TMVCOpenAPI3Middleware.OnBeforeControllerAction(AContext: TWebContext;
  const AControllerQualifiedClassName, AActionName: string;
  var AHandled: Boolean);
begin
end;

procedure TMVCOpenAPI3Middleware.OnAfterControllerAction(AContext: TWebContext;
  const AControllerQualifiedClassName, AActionName: string;
  const AHandled: Boolean);
begin
end;

procedure TMVCOpenAPI3Middleware.OnAfterRouting(AContext: TWebContext;
  const AHandled: Boolean);
begin
end;

end.
