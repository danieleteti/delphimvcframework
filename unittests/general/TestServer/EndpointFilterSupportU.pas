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
// *************************************************************************** }

unit EndpointFilterSupportU;

// The server side of the endpoint filter tests (EndpointFilterTestsU in TestClient): a controller, a
// Minimal API route and two engine-wide endpoint filters. Shared by the TestServer, which every host
// runs, and by the in-process servers of the tests. The filters only act on this controller and this
// route; every other request goes through them untouched, so the whole suite also runs through the
// endpoint filter chain.

interface

uses
  MVCFramework,
  MVCFramework.Commons;

type
  [MVCPath('/ef')]
  TEndpointFilterController = class(TMVCController)
  public
    [MVCPath('/fine')]
    [MVCHTTPMethod([httpGET])]
    function Fine: String;
    // what the filters wrote in the context before the action ran
    [MVCPath('/trace')]
    [MVCHTTPMethod([httpGET])]
    function Trace: String;
    // the legacy shape: procedure + Render
    [MVCPath('/legacy')]
    [MVCHTTPMethod([httpGET])]
    procedure Legacy;
    [MVCPath('/boom')]
    [MVCHTTPMethod([httpGET])]
    function Boom: String;
    // how many times Fine and Legacy have run
    [MVCPath('/runs')]
    [MVCHTTPMethod([httpGET])]
    function Runs: String;
    // the HTTP filter below writes "pre" before the request, this adds "action", the filter adds "post" after
    [MVCPath('/hforder')]
    [MVCHTTPMethod([httpGET])]
    function HTTPFilterOrder: String;
    // what the filters of the engine-wide bundle wrote, in their order
    [MVCPath('/bundle')]
    [MVCHTTPMethod([httpGET])]
    function Bundle: String;
  end;

procedure ConfigureEndpointFilterSupport(const AEngine: TMVCEngine);

implementation

uses
  System.SysUtils,
  System.SyncObjs,
  MVCFramework.MinimalAPI;

var
  GActionRuns: Integer = 0;

{ TEndpointFilterController }

function TEndpointFilterController.Fine: String;
begin
  TInterlocked.Increment(GActionRuns);
  Result := 'fine';
end;

function TEndpointFilterController.Trace: String;
begin
  Result := Context.Data['trace'];
end;

procedure TEndpointFilterController.Legacy;
begin
  TInterlocked.Increment(GActionRuns);
  Render('legacy');
end;

function TEndpointFilterController.Boom: String;
begin
  raise Exception.Create('boom');
end;

function TEndpointFilterController.Runs: String;
begin
  Result := IntToStr(TInterlocked.CompareExchange(GActionRuns, 0, 0));
end;

function TEndpointFilterController.HTTPFilterOrder: String;
begin
  Context.Data['hforder'] := Context.Data['hforder'] + 'action ';
  Result := 'order';
end;

function TEndpointFilterController.Bundle: String;
begin
  Result := '[' + Context.Data['btrace'] + ']';
end;

// the filters of the bundles: each appends its tag to the trace of the requests under APrefix
function BundleTraceHTTPFilter(const APrefix, ATag: string): TMVCHTTPFilter;
begin
  Result :=
    procedure(const AContext: TWebContext; const ANext: TMVCHTTPFilterNext)
    begin
      if AContext.Request.PathInfo.StartsWith(APrefix) then
        AContext.Data['btrace'] := AContext.Data['btrace'] + ATag + ' ';
      ANext();
    end;
end;

function BundleTraceEndpointFilter(const APrefix, ATag: string): TMVCEndpointFilter;
begin
  Result :=
    function(const AContext: TWebContext; const ANext: TMVCEndpointFilterNext): IMVCResponse
    begin
      if AContext.Request.PathInfo.StartsWith(APrefix) then
        AContext.Data['btrace'] := AContext.Data['btrace'] + ATag + ' ';
      Result := ANext();
    end;
end;

function BundleTraceHandler(AContext: TWebContext): IMVCResponse;
begin
  Result := Ok('[' + AContext.Data['btrace'] + ']');
end;

function IsTarget(const AContext: TWebContext): Boolean;
begin
  Result := AContext.ControllerQualifiedClassName.EndsWith('TEndpointFilterController') or
    (AContext.ActionName = 'GET /efmin');
end;

procedure ConfigureEndpointFilterSupport(const AEngine: TMVCEngine);
var
  lGroupBundle: IMVCFilterBundle;
begin
  AEngine.AddController(TEndpointFilterController);
  // an HTTP filter: what it does after ANext() has to see the answer of the action
  AEngine.Use(
    procedure(const AContext: TWebContext; const ANext: TMVCHTTPFilterNext)
    begin
      if not AContext.Request.PathInfo.EndsWith('/ef/hforder') then
      begin
        ANext();
        Exit;
      end;
      AContext.Data['hforder'] := 'pre ';
      ANext();
      AContext.Response.SetCustomHeader('X-HF-Order', AContext.Data['hforder'] + 'post');
      // the answer is either a text or a stream; before the action has run it is neither
      if AContext.Response.ContentStream <> nil then
        AContext.Response.SetCustomHeader('X-HF-Saw-Body', IntToStr(AContext.Response.ContentStream.Size))
      else
        AContext.Response.SetCustomHeader('X-HF-Saw-Body', IntToStr(Length(AContext.Response.Content)));
    end);
  // the first registered is the outermost
  AEngine.Use(
    function(const AContext: TWebContext; const ANext: TMVCEndpointFilterNext): IMVCResponse
    begin
      if not IsTarget(AContext) then
        Exit(ANext());
      AContext.Data['trace'] := 'A>';
      Result := ANext();
      AContext.Response.SetCustomHeader('X-EF-Controller', AContext.ControllerQualifiedClassName);
      AContext.Response.SetCustomHeader('X-EF-Action', AContext.ActionName);
    end);
  AEngine.Use(
    function(const AContext: TWebContext; const ANext: TMVCEndpointFilterNext): IMVCResponse
    begin
      if not IsTarget(AContext) then
        Exit(ANext());
      if AContext.Request.Headers['X-EF-Deny'] <> '' then
        Exit(Forbidden('denied by the filter')); // no ANext(): the action does not run
      AContext.Data['trace'] := AContext.Data['trace'] + 'B>';
      Result := ANext();
    end);
  AEngine.Root.AsApi.MapGet('/efmin',
    function: IMVCResponse
    begin
      Result := Ok('from the route');
    end);

  // a bundle engine-wide, on /ef/bundle
  AEngine.Use(TMVCFilterBundle.Create(
    [BundleTraceHTTPFilter('/ef/bundle', 'h1'), BundleTraceHTTPFilter('/ef/bundle', 'h2')],
    [BundleTraceEndpointFilter('/ef/bundle', 'e1'), BundleTraceEndpointFilter('/ef/bundle', 'e2')]));
  // a bundle on two groups: its HTTP part goes on the engine, once, so it runs on /efb/out too
  lGroupBundle := TMVCFilterBundle.Create([BundleTraceHTTPFilter('/efb', 'h')], [BundleTraceEndpointFilter('/efb', 'e')]);
  AEngine.Prefix('/efb/g1').Use(lGroupBundle).MapGet<TWebContext>('/trace', BundleTraceHandler);
  AEngine.Prefix('/efb/g2').Use(lGroupBundle).MapGet<TWebContext>('/trace', BundleTraceHandler);
  AEngine.Root.MapGet<TWebContext>('/efb/out', BundleTraceHandler);
end;

end.
