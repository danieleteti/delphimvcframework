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
// The OpenAPI HTTP filter. A companion of MVCFramework.Filters, in a unit of
// its own so that the filters do not depend on the OpenAPI emitter.

unit MVCFramework.Filters.OpenAPI;

{$I dmvcframework.inc}

interface

uses
  MVCFramework,
  MVCFramework.OpenAPI3;                       // TMVCOpenAPIInfo, IMVCOpenAPISource

// OpenAPI HTTPFilter — exposes a freshly-built OpenAPI 3.x document at AURL
// (default '/openapi.json') for GET. Auto-discovers the Minimal API routes and
// the classic controllers of AEngine.
//
//   var lInfo: TMVCOpenAPIInfo;
//   lInfo.Title := 'My API';
//   lInfo.Version := '1.0';
//   lEngine.Use(OpenAPIHTTPFilter(lEngine, lInfo));
function OpenAPIHTTPFilter(AEngine: TMVCEngine; const AInfo: TMVCOpenAPIInfo;
  const AURL: string = '/openapi.json'): TMVCHTTPFilter; overload;

// The same, plus document sources of your own, added after the built-in ones:
//   lEngine.Use(OpenAPIHTTPFilter(lEngine, lInfo, [TMyOpenAPISource.Create]));
function OpenAPIHTTPFilter(AEngine: TMVCEngine; const AInfo: TMVCOpenAPIInfo;
  const ASources: array of IMVCOpenAPISource;
  const AURL: string = '/openapi.json'): TMVCHTTPFilter; overload;

// The OpenAPI 3.x document of AEngine as JSON: its Minimal API routes, its
// controllers, then ASources. Built on every call, so routes added later show
// up. The OpenAPI HTTP filter serves it, and so does the deprecated
// TMVCOpenAPI3Middleware.
function OpenAPIDocumentJSON(AEngine: TMVCEngine; const AInfo: TMVCOpenAPIInfo;
  const ASources: array of IMVCOpenAPISource): string;

implementation

uses
  System.SysUtils,
  System.Classes,
  JsonDataObjects,
  MVCFramework.Commons,
  MVCFramework.MinimalAPI;                     // TMVCMinimalAPIMiddleware (the route registry)

function OpenAPIDocumentJSON(AEngine: TMVCEngine; const AInfo: TMVCOpenAPIInfo;
  const ASources: array of IMVCOpenAPISource): string;
var
  lBuilder: TMVCOpenAPIDocumentBuilder;
  lMW: IMVCMiddleware;
  lDoc: TJsonObject;
  I: Integer;
begin
  lBuilder := TMVCOpenAPIDocumentBuilder.Create(AInfo);
  try
    for lMW in AEngine.Middlewares do
      if lMW is TMVCMinimalAPIMiddleware then
      begin
        lBuilder.AddSource(TMVCMinimalAPIOpenAPISource.Create(TMVCMinimalAPIMiddleware(lMW).Registry));
        Break;
      end;
    if AEngine.Controllers.Count > 0 then
      lBuilder.AddSource(TMVCControllerOpenAPISource.Create(AEngine));
    for I := Low(ASources) to High(ASources) do
      if ASources[I] <> nil then
        lBuilder.AddSource(ASources[I]);
    lDoc := lBuilder.Build;
    try
      Result := lDoc.ToJSON(False);
    finally
      lDoc.Free;
    end;
  finally
    lBuilder.Free;
  end;
end;

function OpenAPIHTTPFilter(AEngine: TMVCEngine; const AInfo: TMVCOpenAPIInfo;
  const AURL: string): TMVCHTTPFilter;
begin
  Result := OpenAPIHTTPFilter(AEngine, AInfo, [], AURL);
end;

function OpenAPIHTTPFilter(AEngine: TMVCEngine; const AInfo: TMVCOpenAPIInfo;
  const ASources: array of IMVCOpenAPISource; const AURL: string): TMVCHTTPFilter;
var
  lSources: TArray<IMVCOpenAPISource>;
  I: Integer;
begin
  SetLength(lSources, Length(ASources)); // an open array cannot be captured
  for I := 0 to High(ASources) do
    lSources[I] := ASources[I];
  Result :=
    procedure (const AContext: TWebContext;
               const ANext: TMVCHTTPFilterNext)
    var
      lContentType: string;
    begin
      if (AContext.Request.HTTPMethod <> httpGET) or
        (not SameText(AContext.Request.PathInfo, AURL)) then
      begin
        ANext();
        Exit;
      end;
      lContentType := BuildContentType(TMVCMediaType.APPLICATION_JSON, TMVCCharSet.UTF_8);
      AContext.Response.ContentType := lContentType;
      AContext.Response.SetContentStream(
        TStringStream.Create(OpenAPIDocumentJSON(AEngine, AInfo, lSources), TEncoding.UTF8), lContentType);
    end;
end;

end.
