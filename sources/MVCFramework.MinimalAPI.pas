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
// Minimal API support for DMVCFramework. EXPERIMENTAL / PREVIEW.
//
// Lets you register routes via lambda handlers without declaring a
// controller class. Every route belongs to a TMVCRouteGroup: use
// lEngine.Root for "no prefix, no group data, no filters" routes, or
// lEngine.Prefix(...) for prefixed groups (with optional typed group data
// and filter stack).
//
//   lEngine.Root
//     .MapGet('/health',
//       function: IMVCResponse
//       begin
//         Result := Ok('OK');
//       end);
//
//   lEngine.Prefix('/api/v1')
//     .Use(LoggingFilter())
//     .Use(BearerAuthFilter())
//     .MapPost<TPerson, IPeopleService>('/people',
//       function (Person: TPerson; Svc: IPeopleService): IMVCResponse
//       begin
//         Svc.Create(Person);
//         Result := Created('', Person);
//       end);
//
// Arguments are bound by type:
//   * TWebContext            -> request context
//   * Interface in container -> DI service (the ONLY way to inject a service)
//   * TMVCFormFile           -> first uploaded multipart file (nil if none)
//   * Class                  -> request body JSON (POST/PUT/PATCH) or, for
//                               GET/DELETE, public writable properties mapped
//                               from the query string. A class registered as
//                               group data (Prefix<T>) takes precedence.
//                               NOTE: concrete classes are NEVER resolved from
//                               the DI container — use an interface for DI.
//   * Record                 -> hybrid binding via [MVCFromBody]/[MVCFromQueryString]/
//                               [MVCFromHeader]/[MVCFromCookie]/[MVCFromContentField]/
//                               [MVCFromFile] on fields. Records are validated
//                               if any field carries validation attributes.
//   * Primitive (Integer, Int64, string, Boolean, Double, TGUID, TDateTime...)
//                            -> route param (if present), else query string.
//                               A trailing ($name:*) route segment captures the
//                               rest of the path (slashes included) as a string.
//
// Up to 4 generic arguments per Map call.
//
// ***************************************************************************

unit MVCFramework.MinimalAPI;

{$I dmvcframework.inc}

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  System.Rtti,
  System.TypInfo,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Container;

type
  // Classifies a route registered via the minimal-API surface.
  //   rkApi  - JSON API endpoint; appears in OpenAPI by default
  //   rkWeb  - server-rendered HTML endpoint; excluded from OpenAPI by default
  // All groups start as rkApi. Call .AsWeb on a group to mark it (and every
  // route + nested sub-group it produces) as rkWeb.
  TMVCRouteKind = (rkApi, rkWeb);

  EMVCMinimalAPI = class(EMVCException);

  // -------------------------------------------------------------------------
  // Uploaded multipart/form-data file, surfaced to Minimal API handlers.
  // Non-owning view over the request-owned part: the content stream is owned by
  // TWebContext.Request and stays valid for the request lifetime. Do NOT free
  // the stream. Bind by declaring a TMVCFormFile argument (first uploaded file)
  // or a TMVCFormFile / TArray<TMVCFormFile> record field with optional
  // [MVCFromFile('field')].
  // -------------------------------------------------------------------------
  TMVCFormFile = class
  strict private
    fFieldName: string;
    fFileName: string;
    fContentType: string;
    fStream: TStream;
  public
    constructor Create(const AFieldName, AFileName, AContentType: string;
      AStream: TStream);
    property FieldName: string read fFieldName;
    property FileName: string read fFileName;
    property ContentType: string read fContentType;
    /// <summary>The client-supplied FileName reduced to a bare base name, with
    /// any directory components / drive letter stripped, so it cannot be used
    /// for path traversal or absolute-path overwrite. Empty for a name that is
    /// only path separators or ".."/".". Prefer generating your own name.</summary>
    function SafeFileName: string;
    function Size: Int64;
    // Request-owned content stream, seeked to 0. Do not free.
    function ContentStream: TStream;
    function ContentAsBytes: TBytes;
    function ContentAsString(const AEncoding: TEncoding = nil): string;
    procedure SaveToFile(const APath: string);
  end;

  // -------------------------------------------------------------------------
  // Handler types: function returning IMVCResponse with 0..4 typed arguments.
  // -------------------------------------------------------------------------

  TMVCMinimalFunc = reference to function: IMVCResponse;
  TMVCMinimalFunc<T1> = reference to function(Arg1: T1): IMVCResponse;
  TMVCMinimalFunc<T1, T2> = reference to function(Arg1: T1; Arg2: T2): IMVCResponse;
  TMVCMinimalFunc<T1, T2, T3> = reference to function(Arg1: T1; Arg2: T2; Arg3: T3): IMVCResponse;
  TMVCMinimalFunc<T1, T2, T3, T4> = reference to function(Arg1: T1; Arg2: T2; Arg3: T3; Arg4: T4): IMVCResponse;

  // Internal thunk: untyped wrapper invoked by the dispatcher.
  TMVCMinimalThunk = reference to function(const AContext: TWebContext;
    const ARenderer: TMVCRenderer): IMVCResponse;

  // -------------------------------------------------------------------------
  // Endpoint filter (chain-of-responsibility, .NET-style)
  //
  // Each filter wraps the next call. Inside its body the filter MAY:
  //   * Run pre-handler logic (auth check, log REQ, start timer, ...)
  //   * Call ANext() to invoke the next filter in the chain (or, at the
  //     end of the chain, the actual handler)
  //   * Run post-handler logic (modify Result, log status, stop timer)
  //   * Wrap ANext() in a try/except to observe / replace exceptions
  //   * Wrap ANext() in a try/finally to run "always" code
  //   * Skip ANext() entirely and return its own response (short-circuit)
  //
  // Filters compose by registration order: the FIRST filter Use'd is the
  // OUTERMOST. The handler runs at the deepest nesting.
  //
  // Example (covers everything the old 4-hook API did, in one place):
  //
  //   group.Use(
  //     function (Ctx: TWebContext; Next: TMVCEndpointFilterNext): IMVCResponse
  //     var sw: TStopwatch;
  //     begin
  //       sw := TStopwatch.StartNew;
  //       Audit('REQ ' + Ctx.Request.PathInfo);
  //       try
  //         try
  //           Result := Next();
  //           Audit(Format('RES %d', [Result.StatusCode]));
  //         except
  //           on E: Exception do
  //           begin
  //             Audit('ERR ' + E.Message);
  //             raise;  // or return Status(500, ...) to swallow + replace
  //           end;
  //         end;
  //       finally
  //         Audit(Format('TIME %dms', [sw.ElapsedMilliseconds]));
  //       end;
  //     end);
  // -------------------------------------------------------------------------

  TMVCEndpointFilterNext = reference to function: IMVCResponse;
  TMVCEndpointFilter = reference to function(const AContext: TWebContext;
    const ANext: TMVCEndpointFilterNext): IMVCResponse;

  // -------------------------------------------------------------------------
  // HTTP filter — operates at the transport level, BEFORE routing happens.
  //
  // HTTPFilters wrap the entire minimal-API request handling (TryMatch +
  // EndpointFilter chain + render). They:
  //   * see Ctx.Request before any route is matched
  //   * mutate Ctx.Response directly (no IMVCResponse abstraction)
  //   * compose via chain-of-responsibility (Next() invokes the inner chain)
  //   * can short-circuit by NOT calling Next() — the request stops there
  //
  // Compared to EndpointFilter:
  //   * Endpoint filter: per-group, fires only when a route matches, wraps
  //     the handler. Reads/writes the semantic IMVCResponse.
  //   * HTTP filter: per-engine, fires for every request, wraps the entire
  //     dispatch including routing. Reads/writes the raw HTTP transport.
  //
  // Engine ordering: ALL HTTPFilters run BEFORE any EndpointFilter. Within
  // each kind, registration order = execution order (first registered =
  // outermost in the chain = first pre-Next code to run, last post-Next
  // code to run).
  //
  // Use cases that naturally fit HTTPFilter:
  //   * IP / geo block (pre-Next, short-circuit 403)
  //   * Rate limit (pre-Next, short-circuit 429)
  //   * Static files (pre-Next, short-circuit if file exists)
  //   * Compression / ETag (post-Next, transform Response bytes)
  //   * Security headers (post-Next, stamp on every response)
  //   * Request log / Analytics (pre+post around Next, capture timing)
  //
  // Wired on the engine via lEngine.UseHTTPFilter(...).
  // -------------------------------------------------------------------------
  TMVCHTTPFilterNext = reference to procedure;
  TMVCHTTPFilter = reference to procedure (
    const AContext: TWebContext;
    const ANext: TMVCHTTPFilterNext);

  // -------------------------------------------------------------------------
  // Route registry — TMVCMinimalRoute now carries group data + hook arrays.
  // Hooks are TArray refcounted, so copying a route's hook lists is cheap.
  // -------------------------------------------------------------------------

  TMVCMinimalRoute = class
  strict private
    fVerb: TMVCHTTPMethodType;
    fPathPattern: string;
    fThunk: TMVCMinimalThunk;
    fGroupData: TObject;
    fGroupDataTypeInfo: PTypeInfo;
    fFilters: TArray<TMVCEndpointFilter>;
    fName: string;
    fMetadata: TDictionary<string, TValue>;
    fParamTypes: TArray<PTypeInfo>;
    fRouteKind: TMVCRouteKind;
    procedure SetName(const AValue: string);
  private
    // Back-reference to the registry the route belongs to. Used by SetName
    // to enforce engine-wide uniqueness of operation names. Set in
    // TMVCMinimalRegistry.Add right after construction.
    fRegistry: TObject;
  public
    constructor Create(AVerb: TMVCHTTPMethodType; const APath: string;
      AThunk: TMVCMinimalThunk);
    destructor Destroy; override;
    property Verb: TMVCHTTPMethodType read fVerb;
    property PathPattern: string read fPathPattern;
    property Thunk: TMVCMinimalThunk read fThunk;
    property GroupData: TObject read fGroupData write fGroupData;
    property GroupDataTypeInfo: PTypeInfo read fGroupDataTypeInfo write fGroupDataTypeInfo;
    property Filters: TArray<TMVCEndpointFilter> read fFilters write fFilters;
    // Per-endpoint metadata for OpenAPI / introspection / auth policies.
    // Writing the Name goes through SetName, which rejects empty strings
    // and raises EMVCMinimalAPI on duplicates across the engine.
    property Name: string read fName write SetName;
    property Metadata: TDictionary<string, TValue> read fMetadata;
    // Type info for each generic handler parameter (T1, T2, T3, T4 in order).
    // Captured at registration time so OpenAPI emitter / introspection tools
    // can read the handler signature. Length() = 0 for parameter-less handlers.
    property ParamTypes: TArray<PTypeInfo> read fParamTypes write fParamTypes;
    // Classification of the route: rkApi (JSON) or rkWeb (HTML).
    // Stamped at registration time by the owning TMVCRouteGroup<T>.
    // Default rkApi via class-field zero-init.
    property RouteKind: TMVCRouteKind read fRouteKind write fRouteKind;
  end;

  // Per-endpoint chainable configuration. Returned by MapXxx — wraps the
  // just-registered route so the caller can apply typed configuration
  // (name, OpenAPI summary/description/tags/deprecated, response type)
  // and route-scoped filters without affecting the group.
  TMVCRouteHandle = record
  strict private
    // A handle can cover more than one route: MapMethods registers one route
    // per verb, and OpenAPI metadata / route-scoped filters set through the
    // handle apply to every one of them. WithName is the exception, because an
    // operationId must stay unique, so it targets the primary route only.
    fRoutes: TArray<TMVCMinimalRoute>;
    function GetPrimary: TMVCMinimalRoute;
    procedure SetMeta(const AKey: string; const AValue: TValue);
  public
    constructor Create(ARoute: TMVCMinimalRoute); overload;
    constructor Create(const ARoutes: TArray<TMVCMinimalRoute>); overload;
    // Symbolic name for the endpoint. Useful for OpenAPI operationId,
    // URL generation, logs, route enumeration, ...
    // Empty names and duplicate names across the engine raise
    // EMVCMinimalAPI at registration time.
    function WithName(const AName: string): TMVCRouteHandle;
    // OpenAPI 3.x: one-line operation summary shown in Swagger UI.
    function WithSummary(const ASummary: string): TMVCRouteHandle;
    // OpenAPI 3.x: long-form operation description (Markdown allowed).
    function WithDescription(const ADescription: string): TMVCRouteHandle;
    // OpenAPI 3.x: tag(s) used by Swagger UI to group operations.
    function WithTags(const ATag: string): TMVCRouteHandle; overload;
    function WithTags(const ATags: TArray<string>): TMVCRouteHandle; overload;
    // OpenAPI 3.x: marks the operation as deprecated.
    function WithDeprecated(const AValue: Boolean = True): TMVCRouteHandle;
    // OpenAPI 3.x: declares the schema of the 200 response body.
    function Produces<T>: TMVCRouteHandle;
    // Controls OpenAPI visibility for this route. Default is the route's
    // group kind: rkApi -> visible; rkWeb -> hidden. Pass True on a web
    // route to publish it (dual-output endpoints), or False on an API
    // route to hide it (internal endpoints).
    function WithOpenAPI(const AVisible: Boolean = True): TMVCRouteHandle;
    // Route-scoped filter, appended after the group's filter stack.
    function Use(const AFilter: TMVCEndpointFilter): TMVCRouteHandle;
    // Escape hatch: access the underlying (primary) route.
    property Route: TMVCMinimalRoute read GetPrimary;
  end;

  // Static helpers used by the OpenAPI emitter (and other introspection
  // tools) to interpret route metadata without poking the registry directly.
  TMVCMinimalRouteHelper = class
  public
    // Returns true if the route should be emitted into OpenAPI. Resolves the
    // route's kind and the optional 'openapi.visible' metadata override.
    class function IsVisibleInOpenAPI(const ARoute: TMVCMinimalRoute): Boolean; static;
  end;

  TMVCMinimalRegistry = class
  strict private
    fRoutes: TObjectList<TMVCMinimalRoute>;
    fOwnedData: TObjectList<TObject>;  // group data instances we own
    fHTTPFilters: TArray<TMVCHTTPFilter>;
  public
    constructor Create;
    destructor Destroy; override;
    // Append an HTTP filter to the engine-wide chain. First registered is
    // outermost in the chain (runs first pre-Next, last post-Next).
    procedure AddHTTPFilter(const AFilter: TMVCHTTPFilter);
    property HTTPFilters: TArray<TMVCHTTPFilter> read fHTTPFilters;
    function Add(AVerb: TMVCHTTPMethodType; const APath: string;
      AThunk: TMVCMinimalThunk): TMVCMinimalRoute;
    // Route match + content negotiation. When multiple routes match the same
    // verb+path (e.g. an rkApi and an rkWeb sharing /users), the winner is
    // picked by scoring each candidate against the request's Accept and
    // Content-Type headers — rkWeb prefers text/html responses and form
    // bodies, rkApi prefers application/json responses and JSON bodies.
    // Ties resolve to the first registered candidate.
    function TryMatch(AVerb: TMVCHTTPMethodType; const APath: string;
      const AAcceptHeader: string; const AContentTypeHeader: string;
      const AParamsTable: TMVCRequestParamsTable;
      out ARoute: TMVCMinimalRoute): Boolean;
    // Tracks an object whose lifetime is bound to the engine. Idempotent:
    // adding the same instance twice keeps a single ownership record.
    procedure TrackOwned(AObject: TObject);
    // Snapshot of registered routes (for introspection: OpenAPI emitter,
    // diagnostics, route listing). Returns a copy of the internal array so
    // the caller cannot mutate the registry through it.
    function AllRoutes: TArray<TMVCMinimalRoute>;
  end;

  // -------------------------------------------------------------------------
  // Synthetic renderer used to render IMVCResponse from middleware.
  // Acts as a TMVCRenderer with engine wiring, no controller class needed.
  // Carries a reference to the matched route so the resolver can read group
  // data without any thread-local state. The reference is cleared when the
  // renderer is freed (per-request lifecycle).
  // -------------------------------------------------------------------------

  TMVCMinimalRenderer = class(TMVCRenderer)
  strict private
    fRoute: TMVCMinimalRoute;
  public
    property Route: TMVCMinimalRoute read fRoute write fRoute;

    // Renders one or more server-side view templates using the engine's
    // configured TMVCViewEngineClass (TemplatePro / WebStencils / Mustache,
    // as set via TMVCEngine.SetViewEngine). Mirrors TMVCController.RenderView
    // for the minimal-API world: the data source is the per-request
    // TWebContext.ViewData populated through the global ViewData() helper,
    // since there is no controller instance to carry a FViewModel.
    function RenderView(const AViewName: string;
      const AOnBeforeRender: TMVCSSVBeforeRenderCallback = nil): string; overload;
    // AUseCommonHeadersAndFooters is accepted for API parity with
    // TMVCController.RenderViews but ignored — minimal-API handlers have no
    // per-handler page-header/footer state to wrap views with.
    function RenderViews(const AViewNames: TArray<string>;
      const AUseCommonHeadersAndFooters: Boolean = True): string; overload;
  end;

  // -------------------------------------------------------------------------
  // Argument resolver: type-driven binding.
  // -------------------------------------------------------------------------

  TMVCMinimalArgResolver = class
  public
    // Helpers used by Resolve<T>. Must be declared in interface because
    // Resolve<T> is a generic method of an interface-section class.
    class function ConvertStringTo(const AValue: string;
      ATypeInfo: PTypeInfo): TValue; static;
    class function BindRecordHybrid(const AContext: TWebContext;
      const ARecordTypeInfo: PTypeInfo;
      const ABoundObjects: TObjectList<TObject>): TValue; static;
    class function TryResolveDIService(const AContext: TWebContext;
      const ATypeInfo: PTypeInfo; out AService: IInterface): Boolean; static;
    class function IsHTTPMethodWithBody(
      const AMethod: TMVCHTTPMethodType): Boolean; static;
    class function GetParamFromContext(const AContext: TWebContext;
      const AName: string; out AValue: string): Boolean; static;

    class function Resolve<T>(const AContext: TWebContext;
      const ARenderer: TMVCRenderer;
      const ABoundObjects: TObjectList<TObject>): T; static;
  end;

  // -------------------------------------------------------------------------
  // Thunk factory: wraps a typed user handler in an untyped TMVCMinimalThunk
  // that resolves arguments by type at dispatch time.
  // Standalone generic functions are not allowed in Delphi — these MUST be
  // class static methods.
  // -------------------------------------------------------------------------

  TMVCThunkFactory = class
  public
    class function Make0(const AHandler: TMVCMinimalFunc): TMVCMinimalThunk; static;
    class function Make1<T1>(const AHandler: TMVCMinimalFunc<T1>): TMVCMinimalThunk; static;
    class function Make2<T1, T2>(const AHandler: TMVCMinimalFunc<T1, T2>): TMVCMinimalThunk; static;
    class function Make3<T1, T2, T3>(const AHandler: TMVCMinimalFunc<T1, T2, T3>): TMVCMinimalThunk; static;
    class function Make4<T1, T2, T3, T4>(const AHandler: TMVCMinimalFunc<T1, T2, T3, T4>): TMVCMinimalThunk; static;
  end;

  // -------------------------------------------------------------------------
  // Route group: a path prefix + optional typed group data + lifecycle hooks
  // shared by all routes registered via the group. Immutable record: every
  // OnXxx / Prefix call returns a new group with the change applied; this
  // makes route grouping side-effect-free and chainable.
  // -------------------------------------------------------------------------

  TMVCRouteGroup<T: class> = record
  strict private
    fEngine: TMVCEngine;
    fPrefix: string;
    fData: T;
    fFilters: TArray<TMVCEndpointFilter>;
    fRouteKind: TMVCRouteKind;
    function RegisterRoute(AVerb: TMVCHTTPMethodType; const APath: string;
      AThunk: TMVCMinimalThunk;
      const AParamTypes: TArray<PTypeInfo>): TMVCMinimalRoute;
    function RegisterMany(const AVerbs: array of TMVCHTTPMethodType;
      const APath: string; AThunk: TMVCMinimalThunk;
      const AParamTypes: TArray<PTypeInfo>): TMVCRouteHandle;
  public
    class function Create(AEngine: TMVCEngine; const APrefix: string;
      const AData: T; ARouteKind: TMVCRouteKind = rkApi): TMVCRouteGroup<T>; static;

    // Nested grouping. Returns a new group whose path is the concatenation
    // of the parent prefix and APath; the parent's filter chain is
    // inherited (same model as ASP.NET Core MapGroup / FastAPI
    // include_router). Sub-groups CAN add more filters via Use(), they
    // cannot drop the parent's.
    function Prefix(const APath: string): TMVCRouteGroup<T>; overload;

    // Same as above, with NEW typed group data. The data instance is
    // bound to the engine's lifetime (freed at engine shutdown) unless
    // AOwns is False. Filters still inherit — typed data is the only
    // thing that changes across the boundary.
    function Prefix<U: class>(const APath: string; const AData: U;
      AOwns: Boolean = True): TMVCRouteGroup<U>; overload;

    // Endpoint filter (chain-of-responsibility). Filters are stacked in
    // registration order: the first one Use'd is the outermost.
    function Use(const AFilter: TMVCEndpointFilter): TMVCRouteGroup<T>;

    // Marks this group (and every route + nested sub-group it produces) as
    // rkWeb: HTML endpoints excluded from OpenAPI by default. Returns the
    // same group so it chains naturally with .Use(...) and .MapXxx(...).
    //
    //   lEngine.Root.AsWeb.Use(MemorySession(10)).MapGet('/', HomeHandler);
    function AsWeb: TMVCRouteGroup<T>;

    // Marks this group as rkApi (JSON, included in OpenAPI). Same as the
    // default for fresh Root/Prefix groups, but useful for reverting a
    // nested sub-group of an .AsWeb parent back to API semantics — e.g. a
    // JSON autocomplete endpoint embedded in a web app's URL tree.
    //
    //   lEngine.Root.AsWeb
    //     .Prefix('/search').AsApi
    //     .MapGet('/users.json', SearchUsersHandler);
    function AsApi: TMVCRouteGroup<T>;

    // GET
    function MapGet(const APath: string;
      const AHandler: TMVCMinimalFunc): TMVCRouteHandle; overload;
    function MapGet<T1>(const APath: string;
      const AHandler: TMVCMinimalFunc<T1>): TMVCRouteHandle; overload;
    function MapGet<T1, T2>(const APath: string;
      const AHandler: TMVCMinimalFunc<T1, T2>): TMVCRouteHandle; overload;
    function MapGet<T1, T2, T3>(const APath: string;
      const AHandler: TMVCMinimalFunc<T1, T2, T3>): TMVCRouteHandle; overload;
    function MapGet<T1, T2, T3, T4>(const APath: string;
      const AHandler: TMVCMinimalFunc<T1, T2, T3, T4>): TMVCRouteHandle; overload;

    // POST
    function MapPost(const APath: string;
      const AHandler: TMVCMinimalFunc): TMVCRouteHandle; overload;
    function MapPost<T1>(const APath: string;
      const AHandler: TMVCMinimalFunc<T1>): TMVCRouteHandle; overload;
    function MapPost<T1, T2>(const APath: string;
      const AHandler: TMVCMinimalFunc<T1, T2>): TMVCRouteHandle; overload;
    function MapPost<T1, T2, T3>(const APath: string;
      const AHandler: TMVCMinimalFunc<T1, T2, T3>): TMVCRouteHandle; overload;
    function MapPost<T1, T2, T3, T4>(const APath: string;
      const AHandler: TMVCMinimalFunc<T1, T2, T3, T4>): TMVCRouteHandle; overload;

    // PUT
    function MapPut(const APath: string;
      const AHandler: TMVCMinimalFunc): TMVCRouteHandle; overload;
    function MapPut<T1>(const APath: string;
      const AHandler: TMVCMinimalFunc<T1>): TMVCRouteHandle; overload;
    function MapPut<T1, T2>(const APath: string;
      const AHandler: TMVCMinimalFunc<T1, T2>): TMVCRouteHandle; overload;
    function MapPut<T1, T2, T3>(const APath: string;
      const AHandler: TMVCMinimalFunc<T1, T2, T3>): TMVCRouteHandle; overload;
    function MapPut<T1, T2, T3, T4>(const APath: string;
      const AHandler: TMVCMinimalFunc<T1, T2, T3, T4>): TMVCRouteHandle; overload;

    // DELETE
    function MapDelete(const APath: string;
      const AHandler: TMVCMinimalFunc): TMVCRouteHandle; overload;
    function MapDelete<T1>(const APath: string;
      const AHandler: TMVCMinimalFunc<T1>): TMVCRouteHandle; overload;
    function MapDelete<T1, T2>(const APath: string;
      const AHandler: TMVCMinimalFunc<T1, T2>): TMVCRouteHandle; overload;
    function MapDelete<T1, T2, T3>(const APath: string;
      const AHandler: TMVCMinimalFunc<T1, T2, T3>): TMVCRouteHandle; overload;
    function MapDelete<T1, T2, T3, T4>(const APath: string;
      const AHandler: TMVCMinimalFunc<T1, T2, T3, T4>): TMVCRouteHandle; overload;

    // PATCH
    function MapPatch(const APath: string;
      const AHandler: TMVCMinimalFunc): TMVCRouteHandle; overload;
    function MapPatch<T1>(const APath: string;
      const AHandler: TMVCMinimalFunc<T1>): TMVCRouteHandle; overload;
    function MapPatch<T1, T2>(const APath: string;
      const AHandler: TMVCMinimalFunc<T1, T2>): TMVCRouteHandle; overload;
    function MapPatch<T1, T2, T3>(const APath: string;
      const AHandler: TMVCMinimalFunc<T1, T2, T3>): TMVCRouteHandle; overload;
    function MapPatch<T1, T2, T3, T4>(const APath: string;
      const AHandler: TMVCMinimalFunc<T1, T2, T3, T4>): TMVCRouteHandle; overload;

    // QUERY (RFC 10008). Safe and idempotent like GET, but with a request
    // body, so the typed overloads bind a criteria object the same way the
    // POST/PUT ones do.
    function MapQuery(const APath: string;
      const AHandler: TMVCMinimalFunc): TMVCRouteHandle; overload;
    function MapQuery<T1>(const APath: string;
      const AHandler: TMVCMinimalFunc<T1>): TMVCRouteHandle; overload;
    function MapQuery<T1, T2>(const APath: string;
      const AHandler: TMVCMinimalFunc<T1, T2>): TMVCRouteHandle; overload;
    function MapQuery<T1, T2, T3>(const APath: string;
      const AHandler: TMVCMinimalFunc<T1, T2, T3>): TMVCRouteHandle; overload;
    function MapQuery<T1, T2, T3, T4>(const APath: string;
      const AHandler: TMVCMinimalFunc<T1, T2, T3, T4>): TMVCRouteHandle; overload;

    // Multi-verb shortcut. Registers the same handler against every verb
    // in the array. The arity overloads mirror the single-verb Map* set.
    function MapMethods(const AVerbs: array of TMVCHTTPMethodType;
      const APath: string; const AHandler: TMVCMinimalFunc): TMVCRouteHandle; overload;
    function MapMethods<T1>(const AVerbs: array of TMVCHTTPMethodType;
      const APath: string; const AHandler: TMVCMinimalFunc<T1>): TMVCRouteHandle; overload;
    function MapMethods<T1, T2>(const AVerbs: array of TMVCHTTPMethodType;
      const APath: string; const AHandler: TMVCMinimalFunc<T1, T2>): TMVCRouteHandle; overload;
    function MapMethods<T1, T2, T3>(const AVerbs: array of TMVCHTTPMethodType;
      const APath: string; const AHandler: TMVCMinimalFunc<T1, T2, T3>): TMVCRouteHandle; overload;
    function MapMethods<T1, T2, T3, T4>(const AVerbs: array of TMVCHTTPMethodType;
      const APath: string; const AHandler: TMVCMinimalFunc<T1, T2, T3, T4>): TMVCRouteHandle; overload;
  end;

  // -------------------------------------------------------------------------
  // Middleware that performs route matching + handler dispatch.
  // Registered lazily on the engine at first MapXxx call.
  // -------------------------------------------------------------------------

  TMVCMinimalAPIMiddleware = class(TInterfacedObject, IMVCMiddleware)
  strict private
    fRegistry: TMVCMinimalRegistry;
    fEngine: TMVCEngine;
    // Inner-most dispatch: TryMatch + EndpointFilter chain + render. Wrapped
    // by the HTTPFilter chain in OnBeforeRouting.
    procedure DoMinimalDispatch(AContext: TWebContext; var AHandled: Boolean);
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
    constructor Create(AEngine: TMVCEngine);
    destructor Destroy; override;
    property Registry: TMVCMinimalRegistry read fRegistry;
  end;

  // -------------------------------------------------------------------------
  // Class helper extending TMVCEngine with the entry points to the minimal
  // API: Root (the top-level route group) and Prefix (a route group with a
  // path prefix and optional typed data).
  //
  // All route registration goes through a TMVCRouteGroup — there is no
  // shortcut on the engine itself. Use Root for "no prefix, no group data,
  // no filters" routes; use Prefix for everything else. This keeps the
  // engine surface small and makes the route-grouping model explicit in
  // every call site.
  //
  //   lEngine.Root.MapGet('/health', ...);
  //   lEngine.Prefix('/api/v1').Use(AuthFilter).MapGet('/users', ...);
  // -------------------------------------------------------------------------

  TMVCEngineMinimalAPIHelper = class helper for TMVCEngine
  strict private
    function GetOrCreateMiddleware: TMVCMinimalAPIMiddleware;
  public
    // The top-level route group. Equivalent to Prefix('').
    function Root: TMVCRouteGroup<TObject>;

    // Open a route group with a path prefix. Without group data, T = TObject
    // and the data slot is nil (no group-data resolution happens in handlers).
    function Prefix(const APrefix: string): TMVCRouteGroup<TObject>; overload;

    // Open a route group with typed group data. The data instance is bound
    // to the engine's lifetime: it will be freed at engine shutdown unless
    // AOwns is False (in which case the caller manages the lifetime).
    function Prefix<T: class>(const APrefix: string; const AData: T;
      AOwns: Boolean = True): TMVCRouteGroup<T>; overload;

    // Internal: wires a route into the registry from a TMVCRouteGroup<T>.
    function RegisterFromGroup(AVerb: TMVCHTTPMethodType; const APath: string;
      AThunk: TMVCMinimalThunk): TMVCMinimalRoute;

    // Append an HTTP filter to the engine-wide chain. See TMVCHTTPFilter
    // for semantics. Returns Self via the helper so calls can chain:
    //   lEngine.UseHTTPFilter(IPBlock(banned)).UseHTTPFilter(Compression);
    function UseHTTPFilter(const AFilter: TMVCHTTPFilter): TMVCEngine;
  end;

// Returns the ViewData dictionary for the request currently being dispatched
// by the minimal-API middleware. Raises EMVCMinimalAPI if called outside a
// minimal-API request (no current context).
//
// ViewData is the only ambient per-request helper by design. It carries
// data OUT to the view engine (a different layer), so threadvar access is
// semantically the right model — it is NOT a service the handler queries.
// Everything else (TWebContext, Session, Request, Response, services from
// the DI container) must be declared as a generic argument so the resolver
// injects it. Explicit dependencies in the handler signature keep code
// testable and intent-revealing — this matches ASP.NET Core Minimal APIs,
// which deliberately removed the static ambient HttpContext.Current that
// earlier ASP.NET versions had.
function ViewData: TMVCViewDataObject;

// Renders a server-side view template using the engine's configured
// TMVCViewEngineClass (TemplatePro / WebStencils / Mustache via
// TMVCEngine.SetViewEngine). The current request's ViewData() is the data
// source; the returned IMVCResponse carries the rendered HTML body with
// Content-Type text/html; charset=utf-8 and StatusCode 200 (override on the
// returned instance: Result.StatusCode := N).
//
// Raises EMVCMinimalAPI if called outside a minimal-API request scope.
function RenderView(const AViewName: string): IMVCResponse; overload;
function RenderView(const AViewName: string;
  const AOnBeforeRender: TMVCSSVBeforeRenderCallback): IMVCResponse; overload;

// Renders multiple view templates concatenated into a single HTML body. Useful
// for header/content/footer composition.
//
// AUseCommonHeadersAndFooters is accepted for API parity with
// TMVCController.RenderViews but ignored — minimal-API handlers have no
// per-handler page-header/footer state to wrap views with.
function RenderViews(const AViewNames: TArray<string>;
  const AUseCommonHeadersAndFooters: Boolean = True): IMVCResponse;

// -------------------------------------------------------------------------
// Filter helpers (MemorySession, CORS, JWT, ActiveRecord, StaticFiles,
// Compression, ETag, IPBlock, RateLimit, RequestLog, CORSFilter, ...) have
// moved to a dedicated unit: MVCFramework.Filters. Add it to your `uses` to
// keep using them.
// -------------------------------------------------------------------------

implementation

uses
  System.Diagnostics,
  System.IOUtils,
  System.StrUtils,
  System.SyncObjs,
  Web.HTTPApp,
  MVCFramework.Router,
  MVCFramework.Rtti.Utils,
  MVCFramework.Serializer.Commons,
  MVCFramework.Serializer.Intf,
  MVCFramework.Serializer.JsonDataObjects,
  MVCFramework.Validation,
  MVCFramework.ValidationEngine;

threadvar
  GCurrentContext: TWebContext;
  // Per-request renderer published by the dispatcher. RenderView /
  // RenderViews need an Engine + Context + view engine class to do their job;
  // the renderer carries all three. nil outside a minimal-API request scope.
  GCurrentMinimalRenderer: TMVCMinimalRenderer;

function ViewData: TMVCViewDataObject;
begin
  if GCurrentContext = nil then
    raise EMVCMinimalAPI.Create(
      'ViewData called outside a minimal-API request scope');
  Result := GCurrentContext.ViewData;
end;

function CurrentMinimalRendererOrFail: TMVCMinimalRenderer;
begin
  if GCurrentMinimalRenderer = nil then
    raise EMVCMinimalAPI.Create(
      'RenderView/RenderViews called outside a minimal-API request scope');
  Result := GCurrentMinimalRenderer;
end;

function BuildHTMLResponse(const AHtml: string): IMVCResponse;
var
  lResp: TMVCHTMLResponse;
begin
  lResp := TMVCHTMLResponse.Create;
  // StatusCode defaults to 200; caller can override via Result.StatusCode := N
  // after RenderView returns.
  lResp.StatusCode := http_status.OK;
  lResp.HTMLBody := AHtml;
  Result := lResp; // IMVCResponse holds the reference; ARC manages lifetime.
end;

function RenderView(const AViewName: string): IMVCResponse;
begin
  Result := RenderView(AViewName, nil);
end;

function RenderView(const AViewName: string;
  const AOnBeforeRender: TMVCSSVBeforeRenderCallback): IMVCResponse;
var
  lRenderer: TMVCMinimalRenderer;
  lHtml: string;
begin
  lRenderer := CurrentMinimalRendererOrFail;
  lHtml := lRenderer.RenderView(AViewName, AOnBeforeRender);
  Result := BuildHTMLResponse(lHtml);
end;

function RenderViews(const AViewNames: TArray<string>;
  const AUseCommonHeadersAndFooters: Boolean): IMVCResponse;
var
  lRenderer: TMVCMinimalRenderer;
  lHtml: string;
begin
  // AUseCommonHeadersAndFooters is accepted for API parity with
  // TMVCController.RenderViews but ignored — minimal-API handlers have no
  // per-handler page-header/footer state to wrap views with.
  lRenderer := CurrentMinimalRendererOrFail;
  lHtml := lRenderer.RenderViews(AViewNames, AUseCommonHeadersAndFooters);
  Result := BuildHTMLResponse(lHtml);
end;


{ -------------------------------------------------------------------------- }
{ TMVCFormFile                                                               }
{ -------------------------------------------------------------------------- }

constructor TMVCFormFile.Create(const AFieldName, AFileName, AContentType: string;
  AStream: TStream);
begin
  inherited Create;
  fFieldName := AFieldName;
  fFileName := AFileName;
  fContentType := AContentType;
  fStream := AStream;
end;

function TMVCFormFile.SafeFileName: string;
begin
  // Normalize forward slashes to the platform separator first so GetFileName
  // strips both "../" and "..\" style directory components (and any drive
  // letter). Reject a name that reduces to nothing meaningful.
  Result := TPath.GetFileName(fFileName.Replace('/', TPath.DirectorySeparatorChar, [rfReplaceAll]));
  if (Result = '.') or (Result = '..') then
    Result := '';
end;

function TMVCFormFile.Size: Int64;
begin
  if fStream = nil then
    Exit(0);
  Result := fStream.Size;
end;

function TMVCFormFile.ContentStream: TStream;
begin
  if fStream <> nil then
    fStream.Position := 0;
  Result := fStream;
end;

function TMVCFormFile.ContentAsBytes: TBytes;
begin
  SetLength(Result, 0);
  if (fStream = nil) or (fStream.Size = 0) then
    Exit;
  fStream.Position := 0;
  SetLength(Result, fStream.Size);
  fStream.ReadBuffer(Result[0], fStream.Size);
end;

function TMVCFormFile.ContentAsString(const AEncoding: TEncoding): string;
var
  lEnc: TEncoding;
  lBytes: TBytes;
begin
  if AEncoding = nil then
    lEnc := TEncoding.UTF8
  else
    lEnc := AEncoding;
  lBytes := ContentAsBytes;
  Result := lEnc.GetString(lBytes);
end;

procedure TMVCFormFile.SaveToFile(const APath: string);
var
  lFile: TFileStream;
begin
  lFile := TFileStream.Create(APath, fmCreate);
  try
    if fStream <> nil then
    begin
      fStream.Position := 0;
      lFile.CopyFrom(fStream, fStream.Size);
    end;
  finally
    lFile.Free;
  end;
end;

// Wrap a request-owned file part as a non-owning TMVCFormFile.
function WrapRequestFile(const AReqFile: TAbstractWebRequestFile): TMVCFormFile;
begin
  Result := TMVCFormFile.Create(AReqFile.FieldName, AReqFile.FileName,
    AReqFile.ContentType, AReqFile.Stream);
end;

// Return uploaded files whose FieldName matches AFieldName (case-insensitive).
// AFieldName = '' returns every uploaded file.
function FilesMatching(const AContext: TWebContext;
  const AFieldName: string): TArray<TAbstractWebRequestFile>;
var
  lFiles: TAbstractWebRequestFiles;
  I: Integer;
  lFile: TAbstractWebRequestFile;
begin
  SetLength(Result, 0);
  lFiles := AContext.Request.Files;
  if lFiles = nil then
    Exit;
  for I := 0 to lFiles.Count - 1 do
  begin
    lFile := lFiles[I];
    if (AFieldName = '') or SameText(lFile.FieldName, AFieldName) then
      Result := Result + [lFile];
  end;
end;

// Convert a list of raw string values into a typed dynamic-array TValue whose
// element type comes from AArrayType (e.g. TArray<string>, TArray<Integer>).
function StringArrayToTypedArray(const AValues: TArray<string>;
  const AArrayType: TRttiType): TValue;
var
  lDyn: TRttiDynamicArrayType;
  lElems: TArray<TValue>;
  I: Integer;
begin
  lDyn := AArrayType as TRttiDynamicArrayType;
  SetLength(lElems, Length(AValues));
  for I := 0 to High(AValues) do
    lElems[I] := TMVCMinimalArgResolver.ConvertStringTo(AValues[I],
      lDyn.ElementType.Handle);
  Result := TValue.FromArray(AArrayType.Handle, lElems);
end;

{ -------------------------------------------------------------------------- }
{ TMVCMinimalRenderer                                                        }
{ -------------------------------------------------------------------------- }

type
  // "Cracker" used solely to assign FBeforeRenderCallback on a view engine
  // instance from this unit. TMVCBaseViewEngine declares the field as
  // protected; same-class access from a different unit requires reopening
  // the class via inheritance to surface the protected member. The classic
  // TMVCController.GetRenderedView (same unit as TMVCBaseViewEngine) sets it
  // directly — we replicate that exact behaviour here.
  TMVCBaseViewEngineAccess = class(TMVCBaseViewEngine);

function TMVCMinimalRenderer.RenderView(const AViewName: string;
  const AOnBeforeRender: TMVCSSVBeforeRenderCallback): string;
var
  lView: TMVCBaseViewEngine;
  lStrStream: TStringBuilder;
begin
  // Mirrors TMVCController.GetRenderedView, but without a TMVCController
  // instance: the data source is TWebContext.ViewData (populated by the
  // global ViewData() helper) instead of FViewModel, and the controller
  // argument to the view-engine constructor is nil. FController is set on
  // TMVCBaseViewEngine but the framework never reads it back, so passing nil
  // is safe for the bundled engines (TemplatePro / WebStencils / Mustache).
  lStrStream := TStringBuilder.Create;
  try
    lView := Engine.ViewEngineClass.Create(
      Engine, GetContext, nil, GetContext.ViewData, ContentType);
    try
      TMVCBaseViewEngineAccess(lView).FBeforeRenderCallback := AOnBeforeRender;
      lView.Execute(AViewName, lStrStream);
    finally
      lView.Free;
    end;
    Result := lStrStream.ToString;
  finally
    lStrStream.Free;
  end;
end;

function TMVCMinimalRenderer.RenderViews(const AViewNames: TArray<string>;
  const AUseCommonHeadersAndFooters: Boolean): string;
var
  lView: TMVCBaseViewEngine;
  lViewName: string;
  lStrStream: TStringBuilder;
begin
  // AUseCommonHeadersAndFooters is accepted for API parity with
  // TMVCController.RenderViews but ignored — minimal-API handlers have no
  // per-handler page-header/footer state to wrap views with.
  //
  // Mirrors TMVCController.GetRenderedView, but without a TMVCController
  // instance: the data source is TWebContext.ViewData (populated by the
  // global ViewData() helper) instead of FViewModel, and the controller
  // argument to the view-engine constructor is nil. FController is set on
  // TMVCBaseViewEngine but the framework never reads it back, so passing nil
  // is safe for the bundled engines (TemplatePro / WebStencils / Mustache).
  lStrStream := TStringBuilder.Create;
  try
    lView := Engine.ViewEngineClass.Create(
      Engine, GetContext, nil, GetContext.ViewData, ContentType);
    try
      for lViewName in AViewNames do
      begin
        lView.Execute(lViewName, lStrStream);
      end;
    finally
      lView.Free;
    end;
    Result := lStrStream.ToString;
  finally
    lStrStream.Free;
  end;
end;

{ -------------------------------------------------------------------------- }
{ TMVCMinimalRoute                                                           }
{ -------------------------------------------------------------------------- }

constructor TMVCMinimalRoute.Create(AVerb: TMVCHTTPMethodType;
  const APath: string; AThunk: TMVCMinimalThunk);
begin
  inherited Create;
  fVerb := AVerb;
  fPathPattern := APath;
  fThunk := AThunk;
  fMetadata := TDictionary<string, TValue>.Create;
end;

destructor TMVCMinimalRoute.Destroy;
begin
  fMetadata.Free;
  inherited;
end;

procedure TMVCMinimalRoute.SetName(const AValue: string);
var
  lOther: TMVCMinimalRoute;
begin
  if AValue = '' then
    raise EMVCMinimalAPI.Create(http_status.InternalServerError,
      'WithName: route name cannot be empty.');
  if fRegistry <> nil then
    for lOther in TMVCMinimalRegistry(fRegistry).AllRoutes do
      if (lOther <> Self) and SameText(lOther.Name, AValue) then
        raise EMVCMinimalAPI.CreateFmt(http_status.InternalServerError,
          'WithName: duplicate route name "%s" (already used by %s %s). ' +
          'Operation names must be unique across the engine.',
          [AValue, GetEnumName(TypeInfo(TMVCHTTPMethodType), Ord(lOther.Verb)),
           lOther.PathPattern]);
  fName := AValue;
end;

{ -------------------------------------------------------------------------- }
{ TMVCRouteHandle                                                            }
{ -------------------------------------------------------------------------- }

constructor TMVCRouteHandle.Create(ARoute: TMVCMinimalRoute);
begin
  fRoutes := [ARoute];
end;

constructor TMVCRouteHandle.Create(const ARoutes: TArray<TMVCMinimalRoute>);
begin
  fRoutes := ARoutes;
end;

function TMVCRouteHandle.GetPrimary: TMVCMinimalRoute;
begin
  if Length(fRoutes) = 0 then
    Result := nil
  else
    Result := fRoutes[0];
end;

procedure TMVCRouteHandle.SetMeta(const AKey: string; const AValue: TValue);
var
  lRoute: TMVCMinimalRoute;
begin
  // OpenAPI metadata is per-operation, so it is stamped on every verb the
  // handle covers (one route for a single Map*, several for MapMethods).
  for lRoute in fRoutes do
    lRoute.Metadata.AddOrSetValue(AKey, AValue);
end;

function TMVCRouteHandle.WithName(const AName: string): TMVCRouteHandle;
begin
  // An operationId must be unique across the engine, so a multi-verb handle
  // names only its primary (first) route; the sibling verbs stay unnamed.
  GetPrimary.Name := AName;
  Result := Self;
end;

function TMVCRouteHandle.WithSummary(const ASummary: string): TMVCRouteHandle;
begin
  SetMeta('summary', TValue.From<string>(ASummary));
  Result := Self;
end;

function TMVCRouteHandle.WithDescription(const ADescription: string): TMVCRouteHandle;
begin
  SetMeta('description', TValue.From<string>(ADescription));
  Result := Self;
end;

function TMVCRouteHandle.WithTags(const ATag: string): TMVCRouteHandle;
begin
  SetMeta('tags', TValue.From<TArray<string>>([ATag]));
  Result := Self;
end;

function TMVCRouteHandle.WithTags(const ATags: TArray<string>): TMVCRouteHandle;
begin
  SetMeta('tags', TValue.From<TArray<string>>(ATags));
  Result := Self;
end;

function TMVCRouteHandle.WithDeprecated(const AValue: Boolean): TMVCRouteHandle;
begin
  SetMeta('deprecated', TValue.From<Boolean>(AValue));
  Result := Self;
end;

function TMVCRouteHandle.Produces<T>: TMVCRouteHandle;
begin
  SetMeta('produces.200', TValue.From<PTypeInfo>(TypeInfo(T)));
  Result := Self;
end;

function TMVCRouteHandle.WithOpenAPI(const AVisible: Boolean): TMVCRouteHandle;
begin
  SetMeta('openapi.visible', TValue.From<Boolean>(AVisible));
  Result := Self;
end;

function TMVCRouteHandle.Use(const AFilter: TMVCEndpointFilter): TMVCRouteHandle;
var
  lRoute: TMVCMinimalRoute;
  lLen: Integer;
  lArr: TArray<TMVCEndpointFilter>;
begin
  // A route-scoped filter applies to every verb the handle covers.
  for lRoute in fRoutes do
  begin
    lArr := lRoute.Filters;
    lLen := Length(lArr);
    SetLength(lArr, lLen + 1);
    lArr[lLen] := AFilter;
    lRoute.Filters := lArr;
  end;
  Result := Self;
end;

{ -------------------------------------------------------------------------- }
{ TMVCMinimalRouteHelper                                                     }
{ -------------------------------------------------------------------------- }

class function TMVCMinimalRouteHelper.IsVisibleInOpenAPI(
  const ARoute: TMVCMinimalRoute): Boolean;
var
  lVisible: TValue;
begin
  if ARoute.Metadata.TryGetValue('openapi.visible', lVisible)
    and not lVisible.IsEmpty then
    Result := lVisible.AsBoolean
  else
    Result := ARoute.RouteKind = rkApi;
end;

{ -------------------------------------------------------------------------- }
{ TMVCMinimalRegistry                                                        }
{ -------------------------------------------------------------------------- }

constructor TMVCMinimalRegistry.Create;
begin
  inherited Create;
  fRoutes := TObjectList<TMVCMinimalRoute>.Create(True);
  // OwnsObjects=True: group data freed at registry destroy.
  fOwnedData := TObjectList<TObject>.Create(True);
end;

destructor TMVCMinimalRegistry.Destroy;
begin
  fRoutes.Free;
  fOwnedData.Free;
  inherited;
end;

function TMVCMinimalRegistry.Add(AVerb: TMVCHTTPMethodType;
  const APath: string; AThunk: TMVCMinimalThunk): TMVCMinimalRoute;
begin
  Result := TMVCMinimalRoute.Create(AVerb, APath, AThunk);
  Result.fRegistry := Self;
  fRoutes.Add(Result);
end;

procedure TMVCMinimalRegistry.TrackOwned(AObject: TObject);
begin
  if AObject = nil then
    Exit;
  // idempotent: same instance registered twice -> only one entry, freed once
  if fOwnedData.IndexOf(AObject) < 0 then
    fOwnedData.Add(AObject);
end;

function TMVCMinimalRegistry.AllRoutes: TArray<TMVCMinimalRoute>;
var
  I: Integer;
begin
  SetLength(Result, fRoutes.Count);
  for I := 0 to fRoutes.Count - 1 do
    Result[I] := fRoutes[I];
end;

procedure TMVCMinimalRegistry.AddHTTPFilter(const AFilter: TMVCHTTPFilter);
var
  lLen: Integer;
begin
  // Cannot use `arr + [AFilter]` here: the compiler interprets the
  // open-array constructor over a procedure reference as an INVOCATION.
  // Explicit SetLength append works.
  lLen := Length(fHTTPFilters);
  SetLength(fHTTPFilters, lLen + 1);
  fHTTPFilters[lLen] := AFilter;
end;

// Apply a route constraint to a captured segment value. Returns False if the
// constraint rejects the value — the entire route then fails to match,
// letting the dispatcher consider other routes. Unknown constraint names
// are silently accepted (treated as "no constraint") so adding new ones
// later doesn't break existing routes.
function ApplyConstraint(const AConstraint, AValue: string): Boolean;
var
  lInt: Integer;
  lInt64: Int64;
  lFloat: Double;
  lGuid: TGUID;
  lDate: TDateTime;
begin
  if AConstraint = '' then Exit(True);
  if SameText(AConstraint, 'int') then
    Exit(TryStrToInt(AValue, lInt));
  if SameText(AConstraint, 'int64') then
    Exit(TryStrToInt64(AValue, lInt64));
  if SameText(AConstraint, 'float') then
    Exit(TryStrToFloat(AValue, lFloat, TFormatSettings.Invariant));
  if SameText(AConstraint, 'bool') then
  begin
    Result := SameText(AValue, 'true') or SameText(AValue, 'false')
      or (AValue = '0') or (AValue = '1');
    Exit;
  end;
  if SameText(AConstraint, 'guid') then
  begin
    try
      lGuid := StringToGUID('{' + AValue.Replace('{', '').Replace('}', '') + '}');
      Exit(True);
    except
      Exit(False);
    end;
  end;
  if SameText(AConstraint, 'date') then
    Exit(TryStrToDate(AValue, lDate, TFormatSettings.Invariant));
  Result := True; // unknown constraint -> accept
end;

function MatchPath(const APattern, APath: string;
  const AParamsTable: TMVCRequestParamsTable): Boolean;
var
  lPatternSegs, lPathSegs: TArray<string>;
  I, J, lColon: Integer;
  lPSeg, lASeg, lInner, lParamName, lConstraint, lRest, lWildName: string;
  lWildcard: Boolean;
begin
  // Segment matcher with optional constraints. Syntax:
  //   ($name)               unconstrained capture
  //   ($name:int)           accept only integers
  //   ($name:int64|guid|bool|float|date)  one of the predefined kinds
  // Static segments match literally. A failed constraint makes the whole
  // pattern fail to match (the dispatcher moves on to the next route).
  if (APattern = APath) or ((APath = '/') and (APattern = '')) then
    Exit(True);

  lPatternSegs := APattern.Trim(['/']).Split(['/']);
  lPathSegs := APath.Trim(['/']).Split(['/']);

  // Detect a trailing catch-all segment of the form ($name:*). It must be the
  // LAST pattern segment; it captures every remaining path segment (slashes
  // included) into the named param, and matches an empty tail.
  lWildcard := False;
  lWildName := '';
  if Length(lPatternSegs) > 0 then
  begin
    lPSeg := lPatternSegs[High(lPatternSegs)];
    if lPSeg.StartsWith('($') and lPSeg.EndsWith(':*)') then
    begin
      lWildcard := True;
      lWildName := Copy(lPSeg, 3, Length(lPSeg) - 5); // strip '($' and ':*)'
    end;
  end;

  if lWildcard then
  begin
    if Length(lPathSegs) < Length(lPatternSegs) - 1 then
      Exit(False);
  end
  else if Length(lPatternSegs) <> Length(lPathSegs) then
    Exit(False);

  for I := 0 to High(lPatternSegs) do
  begin
    lPSeg := lPatternSegs[I];

    if lWildcard and (I = High(lPatternSegs)) then
    begin
      lRest := '';
      for J := I to High(lPathSegs) do
      begin
        if lRest <> '' then
          lRest := lRest + '/';
        lRest := lRest + lPathSegs[J];
      end;
      AParamsTable.AddOrSetValue(lWildName, lRest);
      Break;
    end;

    lASeg := lPathSegs[I];
    if lPSeg.StartsWith('($') and lPSeg.EndsWith(')') then
    begin
      lInner := Copy(lPSeg, 3, Length(lPSeg) - 3);
      lColon := Pos(':', lInner);
      if lColon > 0 then
      begin
        lParamName := Copy(lInner, 1, lColon - 1);
        lConstraint := Copy(lInner, lColon + 1, MaxInt);
      end
      else
      begin
        lParamName := lInner;
        lConstraint := '';
      end;
      if not ApplyConstraint(lConstraint, lASeg) then
        Exit(False);
      AParamsTable.AddOrSetValue(lParamName, lASeg);
    end
    else if not SameText(lPSeg, lASeg) then
      Exit(False);
  end;
  Result := True;
end;

// Score a route candidate against the request's Accept and Content-Type.
// rkWeb prefers HTML responses and form/multipart bodies.
// rkApi prefers JSON responses and JSON bodies.
function ScoreRouteForNegotiation(AKind: TMVCRouteKind;
  const AAcceptLower, AContentTypeLower: string): Integer;
begin
  Result := 0;
  case AKind of
    rkWeb:
      begin
        if Pos('text/html', AAcceptLower) > 0 then Inc(Result, 10);
        if Pos('application/x-www-form-urlencoded', AContentTypeLower) > 0 then
          Inc(Result, 5);
        if Pos('multipart/form-data', AContentTypeLower) > 0 then Inc(Result, 5);
      end;
    rkApi:
      begin
        if Pos('application/json', AAcceptLower) > 0 then Inc(Result, 10);
        if Pos('application/json', AContentTypeLower) > 0 then Inc(Result, 5);
      end;
  end;
end;

function TMVCMinimalRegistry.TryMatch(AVerb: TMVCHTTPMethodType;
  const APath: string; const AAcceptHeader: string;
  const AContentTypeHeader: string;
  const AParamsTable: TMVCRequestParamsTable;
  out ARoute: TMVCMinimalRoute): Boolean;
var
  I: Integer;
  lRoute, lBest: TMVCMinimalRoute;
  lCandidates: TArray<TMVCMinimalRoute>;
  lAcceptL, lCtypeL: string;
  lBestScore, lScore: Integer;
begin
  Result := False;
  ARoute := nil;

  // Pass 1: collect every route that matches the verb + path.
  for I := 0 to fRoutes.Count - 1 do
  begin
    lRoute := fRoutes[I];
    if lRoute.Verb <> AVerb then
      Continue;
    AParamsTable.Clear;
    if MatchPath(lRoute.PathPattern, APath, AParamsTable) then
      lCandidates := lCandidates + [lRoute];
  end;

  if Length(lCandidates) = 0 then
  begin
    AParamsTable.Clear;
    Exit;
  end;

  // Single candidate: hot path. Repopulate AParamsTable and return.
  if Length(lCandidates) = 1 then
  begin
    ARoute := lCandidates[0];
    AParamsTable.Clear;
    MatchPath(ARoute.PathPattern, APath, AParamsTable);
    Exit(True);
  end;

  // Multiple candidates → content negotiation. Score each by Accept + Content-Type.
  lAcceptL := LowerCase(AAcceptHeader);
  lCtypeL := LowerCase(AContentTypeHeader);
  lBest := lCandidates[0];
  lBestScore := ScoreRouteForNegotiation(lBest.RouteKind, lAcceptL, lCtypeL);
  for I := 1 to High(lCandidates) do
  begin
    lScore := ScoreRouteForNegotiation(lCandidates[I].RouteKind, lAcceptL, lCtypeL);
    if lScore > lBestScore then
    begin
      lBest := lCandidates[I];
      lBestScore := lScore;
    end;
  end;

  ARoute := lBest;
  AParamsTable.Clear;
  MatchPath(ARoute.PathPattern, APath, AParamsTable);
  Result := True;
end;

{ -------------------------------------------------------------------------- }
{ TMVCMinimalArgResolver — helpers (must be class methods because Resolve<T> }
{ is a generic method declared in interface section)                         }
{ -------------------------------------------------------------------------- }

class function TMVCMinimalArgResolver.ConvertStringTo(const AValue: string;
  ATypeInfo: PTypeInfo): TValue;
begin
  // Shared value coercion (see MVCFramework.Serializer.Commons). The classic
  // controller binder (TMVCEngine.GetActualParam) routes through the same
  // function, so minimal handlers and controller actions bind primitives
  // identically.
  Result := MVCStringToTValue(AValue, ATypeInfo);
end;

class function TMVCMinimalArgResolver.GetParamFromContext(const AContext: TWebContext;
  const AName: string; out AValue: string): Boolean;
begin
  // 1. route segment
  if AContext.Request.SegmentParam(AName, AValue) then
    Exit(True);
  // 2. query string
  if AContext.Request.QueryStringParamExists(AName) then
  begin
    AValue := AContext.Request.QueryStringParam(AName);
    Result := True;
    Exit;
  end;
  AValue := '';
  Result := False;
end;

class function TMVCMinimalArgResolver.BindRecordHybrid(const AContext: TWebContext;
  const ARecordTypeInfo: PTypeInfo;
  const ABoundObjects: TObjectList<TObject>): TValue;
var
  lCtx: TRttiContext;
  lType: TRttiType;
  lField: TRttiField;
  lAttr: TCustomAttribute;
  lFromBody: MVCFromBodyAttribute;
  lFromQuery: MVCFromQueryStringAttribute;
  lFromHeader: MVCFromHeaderAttribute;
  lFromCookie: MVCFromCookieAttribute;
  lFromContentField: MVCFromContentFieldAttribute;
  lStrValue: string;
  lFieldValue: TValue;
  lBuf: array of Byte;
  lAddr: Pointer;
  lBound: Boolean;
  lBodyObj: TObject;
  lSerializer: IMVCSerializer;
  lFileFieldName: string;
  lMatchedFiles: TArray<TAbstractWebRequestFile>;
  lFormFile: TMVCFormFile;
  lFormFileArr: TArray<TMVCFormFile>;
  lFileIdx: Integer;
begin
  lCtx := TRttiContext.Create;
  try
    lType := lCtx.GetType(ARecordTypeInfo);
    if lType = nil then
      raise EMVCMinimalAPI.Create(http_status.InternalServerError,
        'Cannot get RTTI for record ' + string(ARecordTypeInfo.Name));

    SetLength(lBuf, lType.TypeSize);
    FillChar(lBuf[0], lType.TypeSize, 0);
    lAddr := @lBuf[0];

    for lField in lType.GetFields do
    begin
      // ---- file binding: TMVCFormFile / TArray<TMVCFormFile> fields ----
      if (lField.FieldType.Handle = TypeInfo(TMVCFormFile))
         or SameText(lField.FieldType.QualifiedName,
              'System.TArray<MVCFramework.MinimalAPI.TMVCFormFile>') then
      begin
        lFileFieldName := lField.Name;
        for lAttr in lField.GetAttributes do
          if (lAttr is MVCFromFileAttribute)
             and (MVCFromFileAttribute(lAttr).ParamName <> '') then
            lFileFieldName := MVCFromFileAttribute(lAttr).ParamName;

        lMatchedFiles := FilesMatching(AContext, lFileFieldName);
        if lField.FieldType.Handle = TypeInfo(TMVCFormFile) then
        begin
          if Length(lMatchedFiles) > 0 then
          begin
            lFormFile := WrapRequestFile(lMatchedFiles[0]);
            ABoundObjects.Add(lFormFile);
            lField.SetValue(lAddr, lFormFile);
          end;
        end
        else
        begin
          SetLength(lFormFileArr, Length(lMatchedFiles));
          for lFileIdx := 0 to High(lMatchedFiles) do
          begin
            lFormFileArr[lFileIdx] := WrapRequestFile(lMatchedFiles[lFileIdx]);
            ABoundObjects.Add(lFormFileArr[lFileIdx]);
          end;
          lField.SetValue(lAddr, TValue.From<TArray<TMVCFormFile>>(lFormFileArr));
        end;
        Continue; // field handled; skip the [MVCFrom*] dispatch below
      end;

      lBound := False;
      lFromBody := nil;
      lFromQuery := nil;
      lFromHeader := nil;
      lFromCookie := nil;
      lFromContentField := nil;

      for lAttr in lField.GetAttributes do
      begin
        if lAttr is MVCFromBodyAttribute then
          lFromBody := MVCFromBodyAttribute(lAttr)
        else if lAttr is MVCFromQueryStringAttribute then
          lFromQuery := MVCFromQueryStringAttribute(lAttr)
        else if lAttr is MVCFromHeaderAttribute then
          lFromHeader := MVCFromHeaderAttribute(lAttr)
        else if lAttr is MVCFromCookieAttribute then
          lFromCookie := MVCFromCookieAttribute(lAttr)
        else if lAttr is MVCFromContentFieldAttribute then
          lFromContentField := MVCFromContentFieldAttribute(lAttr);
      end;

      if lFromBody <> nil then
      begin
        if lField.FieldType.TypeKind = tkClass then
        begin
          lBodyObj := TRttiUtils.CreateObject(lField.FieldType.QualifiedName);
          ABoundObjects.Add(lBodyObj);
          lSerializer := TMVCJsonDataObjectsSerializer.Create;
          lSerializer.DeserializeObject(AContext.Request.Body, lBodyObj,
            stDefault, [], lFromBody.RootNode);
          lField.SetValue(lAddr, lBodyObj);
        end
        else if lField.FieldType.Handle = TypeInfo(string) then
          lField.SetValue(lAddr, AContext.Request.Body)
        else
          raise EMVCMinimalAPI.CreateFmt(http_status.InternalServerError,
            '[MVCFromBody] on record field "%s" supports only string or class types',
            [lField.Name]);
        lBound := True;
      end
      else if lFromQuery <> nil then
      begin
        if lField.FieldType.TypeKind = tkDynArray then
        begin
          // Repeated query keys (?tag=a&tag=b) -> typed dynamic array.
          lField.SetValue(lAddr,
            StringArrayToTypedArray(
              AContext.Request.QueryParamsMulti[lFromQuery.ParamName],
              lField.FieldType));
        end
        else
        begin
          lStrValue := AContext.Request.QueryStringParam(lFromQuery.ParamName);
          if lStrValue.IsEmpty and lFromQuery.CanBeUsedADefaultValue then
            lStrValue := lFromQuery.DefaultValueAsString;
          lFieldValue := TMVCMinimalArgResolver.ConvertStringTo(lStrValue, lField.FieldType.Handle);
          lField.SetValue(lAddr, lFieldValue);
        end;
        lBound := True;
      end
      else if lFromHeader <> nil then
      begin
        lStrValue := AContext.Request.Headers[lFromHeader.ParamName];
        if lStrValue.IsEmpty and lFromHeader.CanBeUsedADefaultValue then
          lStrValue := lFromHeader.DefaultValueAsString;
        lFieldValue := TMVCMinimalArgResolver.ConvertStringTo(lStrValue, lField.FieldType.Handle);
        lField.SetValue(lAddr, lFieldValue);
        lBound := True;
      end
      else if lFromCookie <> nil then
      begin
        lStrValue := AContext.Request.Cookie(lFromCookie.ParamName);
        if lStrValue.IsEmpty and lFromCookie.CanBeUsedADefaultValue then
          lStrValue := lFromCookie.DefaultValueAsString;
        lFieldValue := TMVCMinimalArgResolver.ConvertStringTo(lStrValue, lField.FieldType.Handle);
        lField.SetValue(lAddr, lFieldValue);
        lBound := True;
      end
      else if lFromContentField <> nil then
      begin
        // Mirror the classic controller (MVCFramework.pas) behavior:
        // TArray<string> fields bind to ContentParamsMulti to support
        // multi-value form fields (e.g. <select multiple>, repeated checkboxes).
        // Only TArray<string> is supported for multi-value bind; other dynamic
        // array types raise a clear error rather than producing a TValue cast
        // failure at runtime.
        if SameText(lField.FieldType.QualifiedName, 'System.TArray<System.string>') then
        begin
          lField.SetValue(lAddr,
            TValue.From< TArray<string> >(
              AContext.Request.ContentParamsMulti[lFromContentField.ParamName]));
        end
        else if lField.FieldType.QualifiedName.StartsWith('System.TArray<System.', True) then
        begin
          raise EMVCMinimalAPI.CreateFmt(http_status.InternalServerError,
            '[MVCFromContentField] on record field "%s" supports only TArray<string> for multi-value bind. ' +
            'Field type "%s" is not supported.',
            [lField.Name, lField.FieldType.QualifiedName]);
        end
        else
        begin
          lStrValue := AContext.Request.ContentParam(lFromContentField.ParamName);
          if lStrValue.IsEmpty and lFromContentField.CanBeUsedADefaultValue then
            lStrValue := lFromContentField.DefaultValueAsString;
          lFieldValue := TMVCMinimalArgResolver.ConvertStringTo(lStrValue, lField.FieldType.Handle);
          lField.SetValue(lAddr, lFieldValue);
        end;
        lBound := True;
      end;

      if not lBound then
      begin
        // Default: try route then query, by field name (case-insensitive)
        if TMVCMinimalArgResolver.GetParamFromContext(AContext, lField.Name, lStrValue) then
        begin
          lFieldValue := TMVCMinimalArgResolver.ConvertStringTo(lStrValue, lField.FieldType.Handle);
          lField.SetValue(lAddr, lFieldValue);
        end;
      end;
    end;

    // Records are validated exactly like classes: any field carrying validation
    // attributes is checked, raising EMVCValidationException (422) on failure.
    TMVCValidationEngine.ValidateRecord(lType, lAddr);

    TValue.Make(lAddr, ARecordTypeInfo, Result);
  finally
    lCtx.Free;
  end;
end;

class function TMVCMinimalArgResolver.TryResolveDIService(const AContext: TWebContext;
  const ATypeInfo: PTypeInfo; out AService: IInterface): Boolean;
var
  lResolver: IMVCServiceContainerResolver;
begin
  Result := False;
  AService := nil;
  if ATypeInfo.Kind <> tkInterface then
    Exit;
  try
    lResolver := AContext.ServiceContainerResolver;
    if lResolver = nil then
      Exit;
    AService := lResolver.Resolve(ATypeInfo);
    Result := AService <> nil;
  except
    Result := False;
  end;
end;

class function TMVCMinimalArgResolver.IsHTTPMethodWithBody(
  const AMethod: TMVCHTTPMethodType): Boolean;
begin
  Result := AMethod in [httpPOST, httpPUT, httpPATCH, httpQUERY];
end;

{ -------------------------------------------------------------------------- }
{ TMVCMinimalArgResolver                                                     }
{ -------------------------------------------------------------------------- }

class function TMVCMinimalArgResolver.Resolve<T>(const AContext: TWebContext;
  const ARenderer: TMVCRenderer;
  const ABoundObjects: TObjectList<TObject>): T;
const
  SEG_IDX_KEY = '__mvc_minimal_seg_idx';
var
  lTypeInfo: PTypeInfo;
  lValue: TValue;
  lService: IInterface;
  lOutIntf: IInterface;
  lObj: TObject;
  lReqFile: TAbstractWebRequestFile;
  lSerializer: IMVCSerializer;
  lStrValue: string;
  lParamName: string;
  lCtx: TRttiContext;
  lProps: TArray<TRttiProperty>;
  lP: TRttiProperty;
  lIdx: Integer;
  lKeys: TArray<string>;
  lIdxStr: string;
  lMinRenderer: TMVCMinimalRenderer;
begin
  lTypeInfo := TypeInfo(T);

  // 1. TWebContext
  if lTypeInfo = TypeInfo(TWebContext) then
  begin
    lValue := TValue.From<TWebContext>(AContext);
    Exit(lValue.AsType<T>);
  end;

  // 1b. TMVCFormFile -> first uploaded file (nil when none were posted).
  //     Constructed inline (not via the unit-local WrapRequestFile) because
  //     Resolve<T> is a generic interface-section method and may not reference
  //     implementation-local symbols.
  if lTypeInfo = TypeInfo(TMVCFormFile) then
  begin
    lObj := nil;
    if (AContext.Request.Files <> nil) and (AContext.Request.Files.Count > 0) then
    begin
      lReqFile := AContext.Request.Files[0];
      lObj := TMVCFormFile.Create(lReqFile.FieldName, lReqFile.FileName,
        lReqFile.ContentType, lReqFile.Stream);
      ABoundObjects.Add(lObj);
    end;
    lValue := TValue.From<TObject>(lObj);
    Exit(lValue.AsType<T>);
  end;

  // 2. Interface -> DI service
  if lTypeInfo.Kind = tkInterface then
  begin
    if not TMVCMinimalArgResolver.TryResolveDIService(AContext, lTypeInfo, lService) then
      raise EMVCMinimalAPI.CreateFmt(http_status.InternalServerError,
        'Cannot resolve interface "%s" from DI container', [lTypeInfo.Name]);
    Supports(lService, lTypeInfo.TypeData.GUID, lOutIntf);
    TValue.Make(@lOutIntf, lTypeInfo, lValue);
    Exit(lValue.AsType<T>);
  end;

  // 3. Record -> hybrid binding
  if lTypeInfo.Kind = tkRecord then
  begin
    lValue := TMVCMinimalArgResolver.BindRecordHybrid(AContext, lTypeInfo, ABoundObjects);
    Exit(lValue.AsType<T>);
  end;

  // 4. Class -> precedence:
  //    a) Group data (if the active route's GroupDataTypeInfo matches T)
  //    b) Body (POST/PUT/PATCH JSON)
  //    c) Query string mapping (GET/DELETE)
  //    Concrete classes are intentionally NOT resolved from the DI container;
  //    only interfaces are. This keeps "class == data".
  if lTypeInfo.Kind = tkClass then
  begin
    // (a) group data lookup via the active TMVCMinimalRenderer
    if ARenderer is TMVCMinimalRenderer then
    begin
      lMinRenderer := TMVCMinimalRenderer(ARenderer);
      if (lMinRenderer.Route <> nil)
        and (lMinRenderer.Route.GroupData <> nil)
        and (lMinRenderer.Route.GroupDataTypeInfo = lTypeInfo) then
      begin
        lValue := TValue.From<TObject>(lMinRenderer.Route.GroupData);
        Exit(lValue.AsType<T>);
      end;
    end;

    lCtx := TRttiContext.Create;
    try
      lObj := TRttiUtils.CreateObject(lCtx.GetType(lTypeInfo).QualifiedName);
      ABoundObjects.Add(lObj);

      if TMVCMinimalArgResolver.IsHTTPMethodWithBody(AContext.Request.HTTPMethod) then
      begin
        if not AContext.Request.Body.Trim.IsEmpty then
        begin
          lSerializer := TMVCJsonDataObjectsSerializer.Create;
          lSerializer.DeserializeObject(AContext.Request.Body, lObj,
            stDefault, [], '');
        end;
      end
      else
      begin
        // GET/DELETE: walk public writable properties, fill from query string
        lProps := lCtx.GetType(lTypeInfo).GetProperties;
        for lP in lProps do
        begin
          if not lP.IsWritable then
            Continue;
          if AContext.Request.QueryStringParamExists(lP.Name) then
          begin
            lStrValue := AContext.Request.QueryStringParam(lP.Name);
            lP.SetValue(lObj, TMVCMinimalArgResolver.ConvertStringTo(lStrValue, lP.PropertyType.Handle));
          end;
        end;
      end;

      // Auto-validate the bound class if it carries validation attributes
      // (descends from TMVCValidatable). Raises EMVCValidationException on
      // failure — caught by the middleware and rendered as ProblemDetails 400.
      if TMVCValidationEngine.IsValidatableClass(lObj.ClassType) then
        TMVCValidationEngine.ValidateAndRaise(lObj);
    finally
      lCtx.Free;
    end;

    lValue := TValue.From<TObject>(lObj);
    Exit(lValue.AsType<T>);
  end;

  // 5. Primitive -> next unconsumed route segment, in declaration order.
  //    A per-request counter on TWebContext.Data tracks consumption so that
  //    multiple primitive arguments map to successive segments.
  lParamName := '';
  if AContext.Request.SegmentParamsCount > 0 then
  begin
    lIdx := 0;
    if AContext.Data.ContainsKey(SEG_IDX_KEY) then
    begin
      lIdxStr := AContext.Data.Items[SEG_IDX_KEY];
      lIdx := StrToIntDef(lIdxStr, 0);
    end;

    lKeys := AContext.Request.ParamsTable.Keys.ToArray;
    if lIdx < Length(lKeys) then
    begin
      lParamName := lKeys[lIdx];
      AContext.Data.Items[SEG_IDX_KEY] := IntToStr(lIdx + 1);
    end;
  end;

  if lParamName <> '' then
    lStrValue := AContext.Request.ParamsTable.Items[lParamName]
  else
    raise EMVCMinimalAPI.CreateFmt(http_status.BadRequest,
      'No route segment available to bind primitive parameter of type "%s"',
      [lTypeInfo.Name]);

  lValue := TMVCMinimalArgResolver.ConvertStringTo(lStrValue, lTypeInfo);
  Result := lValue.AsType<T>;
end;

{ -------------------------------------------------------------------------- }
{ Thunk factories — capture user handler, resolve args, invoke               }
{ -------------------------------------------------------------------------- }

class function TMVCThunkFactory.Make0(
  const AHandler: TMVCMinimalFunc): TMVCMinimalThunk;
begin
  Result := function(const AContext: TWebContext;
                     const ARenderer: TMVCRenderer): IMVCResponse
    begin
      Result := AHandler();
    end;
end;

class function TMVCThunkFactory.Make1<T1>(
  const AHandler: TMVCMinimalFunc<T1>): TMVCMinimalThunk;
begin
  Result := function(const AContext: TWebContext;
                     const ARenderer: TMVCRenderer): IMVCResponse
    var
      lA1: T1;
      lBound: TObjectList<TObject>;
    begin
      lBound := TObjectList<TObject>.Create(True);
      try
        lA1 := TMVCMinimalArgResolver.Resolve<T1>(AContext, ARenderer, lBound);
        Result := AHandler(lA1);
      finally
        lBound.Free;
      end;
    end;
end;

class function TMVCThunkFactory.Make2<T1, T2>(
  const AHandler: TMVCMinimalFunc<T1, T2>): TMVCMinimalThunk;
begin
  Result := function(const AContext: TWebContext;
                     const ARenderer: TMVCRenderer): IMVCResponse
    var
      lA1: T1;
      lA2: T2;
      lBound: TObjectList<TObject>;
    begin
      lBound := TObjectList<TObject>.Create(True);
      try
        lA1 := TMVCMinimalArgResolver.Resolve<T1>(AContext, ARenderer, lBound);
        lA2 := TMVCMinimalArgResolver.Resolve<T2>(AContext, ARenderer, lBound);
        Result := AHandler(lA1, lA2);
      finally
        lBound.Free;
      end;
    end;
end;

class function TMVCThunkFactory.Make3<T1, T2, T3>(
  const AHandler: TMVCMinimalFunc<T1, T2, T3>): TMVCMinimalThunk;
begin
  Result := function(const AContext: TWebContext;
                     const ARenderer: TMVCRenderer): IMVCResponse
    var
      lA1: T1;
      lA2: T2;
      lA3: T3;
      lBound: TObjectList<TObject>;
    begin
      lBound := TObjectList<TObject>.Create(True);
      try
        lA1 := TMVCMinimalArgResolver.Resolve<T1>(AContext, ARenderer, lBound);
        lA2 := TMVCMinimalArgResolver.Resolve<T2>(AContext, ARenderer, lBound);
        lA3 := TMVCMinimalArgResolver.Resolve<T3>(AContext, ARenderer, lBound);
        Result := AHandler(lA1, lA2, lA3);
      finally
        lBound.Free;
      end;
    end;
end;

class function TMVCThunkFactory.Make4<T1, T2, T3, T4>(
  const AHandler: TMVCMinimalFunc<T1, T2, T3, T4>): TMVCMinimalThunk;
begin
  Result := function(const AContext: TWebContext;
                     const ARenderer: TMVCRenderer): IMVCResponse
    var
      lA1: T1;
      lA2: T2;
      lA3: T3;
      lA4: T4;
      lBound: TObjectList<TObject>;
    begin
      lBound := TObjectList<TObject>.Create(True);
      try
        lA1 := TMVCMinimalArgResolver.Resolve<T1>(AContext, ARenderer, lBound);
        lA2 := TMVCMinimalArgResolver.Resolve<T2>(AContext, ARenderer, lBound);
        lA3 := TMVCMinimalArgResolver.Resolve<T3>(AContext, ARenderer, lBound);
        lA4 := TMVCMinimalArgResolver.Resolve<T4>(AContext, ARenderer, lBound);
        Result := AHandler(lA1, lA2, lA3, lA4);
      finally
        lBound.Free;
      end;
    end;
end;

var
  // Process-wide engine -> middleware map used by GetOrCreateMiddleware so
  // subsequent MapXxx calls append to the SAME middleware's registry. The
  // middleware destructor removes its slot when its engine is freed so a
  // reallocated engine at the same address does not pick up a dangling
  // pointer.
  gMinimalAPIByEngine: TDictionary<Pointer, TMVCMinimalAPIMiddleware> = nil;
  gMinimalAPILock: TObject = nil;

{ -------------------------------------------------------------------------- }
{ TMVCMinimalAPIMiddleware                                                   }
{ -------------------------------------------------------------------------- }

constructor TMVCMinimalAPIMiddleware.Create(AEngine: TMVCEngine);
begin
  inherited Create;
  fEngine := AEngine;
  fRegistry := TMVCMinimalRegistry.Create;
end;

destructor TMVCMinimalAPIMiddleware.Destroy;
begin
  // Drop this engine's slot from the process-wide registry. Otherwise the
  // dangling Pointer(fEngine) lingers and a new engine reallocated at the
  // same address will pick up THIS (freed) middleware on first lookup.
  if Assigned(gMinimalAPILock) and Assigned(gMinimalAPIByEngine) then
  begin
    TMonitor.Enter(gMinimalAPILock);
    try
      gMinimalAPIByEngine.Remove(Pointer(fEngine));
    finally
      TMonitor.Exit(gMinimalAPILock);
    end;
  end;
  fRegistry.Free;
  inherited;
end;

// Build a problem+json envelope (RFC 7807) from an exception. Always goes
// through the standard render pipeline so we don't poke RawWebResponse
// (which may be nil in OnBeforeRouting under some adapters).
procedure RenderExceptionAsProblem(const ARenderer: TMVCRenderer;
  const AContext: TWebContext; const AStatusCode: Integer;
  const ATitle: string; const E: Exception);
var
  lResp: IMVCResponse;
begin
  lResp := ProblemDetails(AStatusCode, ATitle, E.Message,
    AContext.Request.PathInfo);
  TMVCRenderer.InternalRenderMVCResponse(ARenderer,
    TMVCResponse(lResp as TObject));
end;

// Build the chain of TMVCEndpointFilterNext closures so the FIRST filter
// is the OUTERMOST and the handler is the innermost. Returns the closure
// that, when called, drives the entire chain.
function BuildHTTPFilterChain(
  const AFilters: TArray<TMVCHTTPFilter>;
  const ACtx: TWebContext;
  const ATerminator: TMVCHTTPFilterNext): TMVCHTTPFilterNext;
var
  i: Integer;
  lInner: TMVCHTTPFilterNext;
  lFilter: TMVCHTTPFilter;
begin
  // innermost = the terminator (typically: do the actual minimal-API dispatch)
  lInner := ATerminator;
  // wrap each filter from LAST to FIRST so registration order = outer-to-inner
  // at runtime (first registered fires first pre-Next, last post-Next).
  for i := High(AFilters) downto 0 do
  begin
    lFilter := AFilters[i];
    lInner := (function (const F: TMVCHTTPFilter;
                         const NextRef: TMVCHTTPFilterNext): TMVCHTTPFilterNext
      begin
        Result := procedure
          begin
            F(ACtx, NextRef);
          end;
      end)(lFilter, lInner);
  end;
  Result := lInner;
end;

function BuildFilterChain(const ARoute: TMVCMinimalRoute;
  const ACtx: TWebContext;
  const ARenderer: TMVCRenderer): TMVCEndpointFilterNext;
var
  i: Integer;
  lInner: TMVCEndpointFilterNext;
  lFilter: TMVCEndpointFilter;
begin
  // innermost: invoke the handler thunk
  lInner := function: IMVCResponse
    begin
      Result := ARoute.Thunk(ACtx, ARenderer);
    end;

  // wrap each filter from the LAST to the FIRST so registration order
  // becomes outer-to-inner at runtime
  for i := High(ARoute.Filters) downto 0 do
  begin
    lFilter := ARoute.Filters[i];
    // capture lInner by value into a local so the closure binds to the
    // current loop's lInner, not the next iteration's
    lInner := (function (const F: TMVCEndpointFilter;
                          const NextRef: TMVCEndpointFilterNext): TMVCEndpointFilterNext
      begin
        Result := function: IMVCResponse
          begin
            Result := F(ACtx, NextRef);
          end;
      end)(lFilter, lInner);
  end;

  Result := lInner;
end;

procedure TMVCMinimalAPIMiddleware.OnBeforeRouting(AContext: TWebContext;
  var AHandled: Boolean);
var
  lFilters: TArray<TMVCHTTPFilter>;
  lNextCalled, lInnerHandled: Boolean;
  lTerminator: TMVCHTTPFilterNext;
begin
  if AHandled then
    Exit;

  lFilters := fRegistry.HTTPFilters;
  if Length(lFilters) = 0 then
  begin
    // No HTTPFilters registered: pass straight to the inner dispatch.
    DoMinimalDispatch(AContext, AHandled);
    Exit;
  end;

  // HTTPFilters wrap the entire inner dispatch. Innermost terminator captures
  // whether Next() was called so we can decide AHandled:
  //   * Next called      -> defer to the inner dispatcher's verdict
  //   * Next NOT called  -> the filter short-circuited (e.g. IP block 403,
  //                          static file served, rate limit 429) - mark as
  //                          handled so the engine stops processing
  lNextCalled := False;
  lInnerHandled := False;
  lTerminator := procedure
    begin
      lNextCalled := True;
      DoMinimalDispatch(AContext, lInnerHandled);
    end;
  BuildHTTPFilterChain(lFilters, AContext, lTerminator)();
  if not lNextCalled then
    AHandled := True
  else
    AHandled := lInnerHandled;
end;

procedure TMVCMinimalAPIMiddleware.DoMinimalDispatch(AContext: TWebContext;
  var AHandled: Boolean);
var
  lRoute: TMVCMinimalRoute;
  lRenderer: TMVCMinimalRenderer;
  lResp: IMVCResponse;
  lParamsTable: TMVCRequestParamsTable;
  lOwnedParamsTable: Boolean;
  lStopWatch: TStopwatch;
begin
  if AHandled then
    Exit;

  // The engine creates a fresh ParamsTable per request and only assigns it
  // to the context AFTER routing. At OnBeforeRouting time, AContext.ParamsTable
  // may be nil. Use any pre-existing one (e.g. set by another middleware), or
  // create a temporary one we own.
  lParamsTable := AContext.Request.ParamsTable;
  lOwnedParamsTable := False;
  if lParamsTable = nil then
  begin
    lParamsTable := TMVCRequestParamsTable.Create;
    lOwnedParamsTable := True;
  end;

  try
    if not fRegistry.TryMatch(AContext.Request.HTTPMethod,
      AContext.Request.PathInfo,
      AContext.Request.Headers['Accept'],
      AContext.Request.Headers['Content-Type'],
      lParamsTable, lRoute) then
    begin
      // No minimal-API route matched - let the regular controller router proceed.
      Exit;
    end;

    // Wire ParamsTable into the context so the resolver can read segments.
    AContext.ParamsTable := lParamsTable;

    // Match TMVCEngine.HandleRequest semantics: measure the wall-clock time
    // spent dispatching the matched route, then fire the engine's OnRouterLog
    // hook so the default Gin-style console line is emitted for minimal-API
    // routes too. Without this, only controller-based routes get logged and
    // the wizard-generated minimal-API project appears silent on the console.
    lStopWatch := TStopwatch.StartNew;

    lRenderer := TMVCMinimalRenderer.Create;
    try
      lRenderer.Engine := fEngine;
      lRenderer.SetContext(AContext);
      lRenderer.SetContentType(TMVCMediaType.APPLICATION_JSON);
      lRenderer.Route := lRoute;  // <-- gives Resolve<T> access to group data

      // Build the filter chain (filters wrap the handler call). Any
      // try/except/finally semantics belong INSIDE individual filters
      // — there is no separate Before/Success/Error/Always now.
      // The threadvar set/clear wraps the entire filter chain AND the
      // framework-level exception handlers, so global helpers (e.g.
      // RenderView, ViewData) work from any handler or filter, and so
      // future error-page rendering inside the except blocks below can
      // still see the current context/renderer.
      GCurrentContext := AContext;
      GCurrentMinimalRenderer := lRenderer;
      try
        try
          lResp := BuildFilterChain(lRoute, AContext, lRenderer)();
          if lResp <> nil then
            TMVCRenderer.InternalRenderMVCResponse(lRenderer,
              TMVCResponse(lResp as TObject));
        except
          // Validation failures (EMVCValidationException) carry 422 from
          // their constructor and fall through to the EMVCException handler
          // below, which respects the exception's own status. 422
          // (Unprocessable Entity, RFC 4918) is the right code: the request
          // was syntactically well-formed JSON but failed semantic validation
          // — distinct from 400 (Bad Request) used for parse / binding
          // failures (EMVCMinimalAPI).
          on E: EMVCException do
            if not fEngine.HandleException(E, AContext) then
              RenderExceptionAsProblem(lRenderer, AContext,
                E.HTTPStatusCode, ReasonPhraseFor(E.HTTPStatusCode), E);
          on E: Exception do
            if not fEngine.HandleException(E, AContext) then
              RenderExceptionAsProblem(lRenderer, AContext,
                http_status.InternalServerError, 'Internal Server Error', E);
        end;
      finally
        GCurrentMinimalRenderer := nil;
        GCurrentContext := nil;
      end;
      AHandled := True;
    finally
      lRenderer.Route := nil;  // make the dangling-ref window minimal
      lRenderer.Free;
    end;

    if Assigned(fEngine.OnRouterLog) then
    begin
      AContext.Data['__duration'] := Format('%dms', [lStopWatch.ElapsedMilliseconds]);
      fEngine.OnRouterLog(rlsRouteFound, AContext);
    end;
  finally
    if lOwnedParamsTable then
    begin
      AContext.ParamsTable := nil;
      lParamsTable.Free;
    end;
  end;
end;

procedure TMVCMinimalAPIMiddleware.OnBeforeControllerAction(
  AContext: TWebContext; const AControllerQualifiedClassName: string;
  const AActionName: string; var AHandled: Boolean);
begin
  // not used
end;

procedure TMVCMinimalAPIMiddleware.OnAfterControllerAction(
  AContext: TWebContext; const AControllerQualifiedClassName: string;
  const AActionName: string; const AHandled: Boolean);
begin
  // not used
end;

procedure TMVCMinimalAPIMiddleware.OnAfterRouting(AContext: TWebContext;
  const AHandled: Boolean);
begin
  // not used
end;

{ -------------------------------------------------------------------------- }
{ TMVCEngineMinimalAPIHelper                                                 }
{                                                                            }
{ The middleware is added to the engine's middleware list (engine owns the   }
{ interface reference). We also keep a typed pointer in gMinimalAPIByEngine  }
{ so subsequent MapXxx calls append to the SAME middleware's registry        }
{ without relying on interface-to-class cast tricks that are fragile across  }
{ Delphi versions.                                                           }
{ -------------------------------------------------------------------------- }

function TMVCEngineMinimalAPIHelper.GetOrCreateMiddleware: TMVCMinimalAPIMiddleware;
begin
  TMonitor.Enter(gMinimalAPILock);
  try
    if not gMinimalAPIByEngine.TryGetValue(Pointer(Self), Result) then
    begin
      Result := TMVCMinimalAPIMiddleware.Create(Self);
      gMinimalAPIByEngine.Add(Pointer(Self), Result);
      Self.AddMiddleware(Result);
    end;
  finally
    TMonitor.Exit(gMinimalAPILock);
  end;
end;

function TMVCEngineMinimalAPIHelper.RegisterFromGroup(AVerb: TMVCHTTPMethodType;
  const APath: string; AThunk: TMVCMinimalThunk): TMVCMinimalRoute;
begin
  Result := GetOrCreateMiddleware.Registry.Add(AVerb, APath, AThunk);
end;

function TMVCEngineMinimalAPIHelper.UseHTTPFilter(
  const AFilter: TMVCHTTPFilter): TMVCEngine;
begin
  GetOrCreateMiddleware.Registry.AddHTTPFilter(AFilter);
  Result := Self;
end;

function TMVCEngineMinimalAPIHelper.Root: TMVCRouteGroup<TObject>;
begin
  // Always starts as rkApi. Call .AsWeb on the returned group for HTML routes.
  GetOrCreateMiddleware;
  Result := TMVCRouteGroup<TObject>.Create(Self, '', nil);
end;

function TMVCEngineMinimalAPIHelper.Prefix(
  const APrefix: string): TMVCRouteGroup<TObject>;
begin
  // Make sure the middleware exists (so the group has a working engine to
  // register against). No data to track.
  GetOrCreateMiddleware;
  Result := TMVCRouteGroup<TObject>.Create(Self, APrefix, nil);
end;

function TMVCEngineMinimalAPIHelper.Prefix<T>(const APrefix: string;
  const AData: T; AOwns: Boolean): TMVCRouteGroup<T>;
var
  lMW: TMVCMinimalAPIMiddleware;
begin
  lMW := GetOrCreateMiddleware;
  if AOwns and (AData <> nil) then
    lMW.Registry.TrackOwned(AData);
  Result := TMVCRouteGroup<T>.Create(Self, APrefix, AData);
end;

{ -------------------------------------------------------------------------- }
{ TMVCRouteGroup<T>                                                          }
{ -------------------------------------------------------------------------- }

class function TMVCRouteGroup<T>.Create(AEngine: TMVCEngine;
  const APrefix: string; const AData: T;
  ARouteKind: TMVCRouteKind): TMVCRouteGroup<T>;
begin
  Result.fEngine := AEngine;
  Result.fPrefix := APrefix;
  Result.fData := AData;
  Result.fRouteKind := ARouteKind;
  // hook arrays start nil — TArray refcounted assignment is no-op for nil
end;

function TMVCRouteGroup<T>.RegisterRoute(AVerb: TMVCHTTPMethodType;
  const APath: string; AThunk: TMVCMinimalThunk;
  const AParamTypes: TArray<PTypeInfo>): TMVCMinimalRoute;
begin
  Result := fEngine.RegisterFromGroup(AVerb, fPrefix + APath, AThunk);
  Result.RouteKind := fRouteKind;
  if fData <> nil then
  begin
    Result.GroupData := TObject(fData);
    Result.GroupDataTypeInfo := TypeInfo(T);
  end;
  // copy filter array (TArray = refcount bump, near zero cost)
  Result.Filters := fFilters;
  if Length(AParamTypes) > 0 then
    Result.ParamTypes := AParamTypes;
end;

function TMVCRouteGroup<T>.Prefix(const APath: string): TMVCRouteGroup<T>;
begin
  Result := Self;
  Result.fPrefix := fPrefix + APath;
end;

function TMVCRouteGroup<T>.Prefix<U>(const APath: string; const AData: U;
  AOwns: Boolean): TMVCRouteGroup<U>;
var
  lFilter: TMVCEndpointFilter;
begin
  // Filters propagate across the type boundary: TMVCEndpointFilter is not
  // parametric on T, so the chain carries trivially. Matches the model of
  // ASP.NET Core MapGroup and FastAPI include_router: cross-cutting
  // concerns (logging, auth, rate-limit) added on a parent group are seen
  // by every nested route, regardless of whether the typed data changed.
  // The parent's RouteKind also propagates: a child of an .AsWeb group stays
  // rkWeb, even when the typed data slot changes.
  Result := fEngine.Prefix<U>(fPrefix + APath, AData, AOwns);
  if fRouteKind = rkWeb then
    Result := Result.AsWeb;
  for lFilter in fFilters do
    Result := Result.Use(lFilter);
end;

// ----- endpoint filter ----------------------------------------------------

function TMVCRouteGroup<T>.AsWeb: TMVCRouteGroup<T>;
begin
  Result := Self;
  Result.fRouteKind := rkWeb;
end;

function TMVCRouteGroup<T>.AsApi: TMVCRouteGroup<T>;
begin
  Result := Self;
  Result.fRouteKind := rkApi;
end;

function TMVCRouteGroup<T>.Use(const AFilter: TMVCEndpointFilter): TMVCRouteGroup<T>;
var
  lLen: Integer;
begin
  // Cannot use `arr + [AFilter]` here: the compiler interprets the
  // open-array constructor over a function reference as an INVOCATION.
  // Explicit SetLength append works.
  Result := Self;
  lLen := Length(Result.fFilters);
  SetLength(Result.fFilters, lLen + 1);
  Result.fFilters[lLen] := AFilter;
end;

function TMVCRouteGroup<T>.RegisterMany(
  const AVerbs: array of TMVCHTTPMethodType;
  const APath: string; AThunk: TMVCMinimalThunk;
  const AParamTypes: TArray<PTypeInfo>): TMVCRouteHandle;
var
  V: TMVCHTTPMethodType;
  lRoutes: TArray<TMVCMinimalRoute>;
begin
  lRoutes := nil;
  for V in AVerbs do
    lRoutes := lRoutes + [RegisterRoute(V, APath, AThunk, AParamTypes)];
  // The returned handle covers every verb's route: OpenAPI metadata
  // (WithSummary/WithTags/WithOpenAPI/...) and route-scoped filters set on it
  // apply to all of them. WithName targets the primary (first) verb only,
  // because an operationId must stay unique; register each verb separately if
  // you need per-verb names.
  Result := TMVCRouteHandle.Create(lRoutes);
end;

// ----- Map* (5 verbs x 5 arity = 25) --------------------------------------

// GET
function TMVCRouteGroup<T>.MapGet(const APath: string;
  const AHandler: TMVCMinimalFunc): TMVCRouteHandle;
begin
  Result := TMVCRouteHandle.Create(RegisterRoute(httpGET, APath, TMVCThunkFactory.Make0(AHandler), nil));
end;

function TMVCRouteGroup<T>.MapGet<T1>(const APath: string;
  const AHandler: TMVCMinimalFunc<T1>): TMVCRouteHandle;
begin
  Result := TMVCRouteHandle.Create(RegisterRoute(httpGET, APath, TMVCThunkFactory.Make1<T1>(AHandler), [TypeInfo(T1)]));
end;

function TMVCRouteGroup<T>.MapGet<T1, T2>(const APath: string;
  const AHandler: TMVCMinimalFunc<T1, T2>): TMVCRouteHandle;
begin
  Result := TMVCRouteHandle.Create(RegisterRoute(httpGET, APath, TMVCThunkFactory.Make2<T1, T2>(AHandler), [TypeInfo(T1), TypeInfo(T2)]));
end;

function TMVCRouteGroup<T>.MapGet<T1, T2, T3>(const APath: string;
  const AHandler: TMVCMinimalFunc<T1, T2, T3>): TMVCRouteHandle;
begin
  Result := TMVCRouteHandle.Create(RegisterRoute(httpGET, APath, TMVCThunkFactory.Make3<T1, T2, T3>(AHandler), [TypeInfo(T1), TypeInfo(T2), TypeInfo(T3)]));
end;

function TMVCRouteGroup<T>.MapGet<T1, T2, T3, T4>(const APath: string;
  const AHandler: TMVCMinimalFunc<T1, T2, T3, T4>): TMVCRouteHandle;
begin
  Result := TMVCRouteHandle.Create(RegisterRoute(httpGET, APath, TMVCThunkFactory.Make4<T1, T2, T3, T4>(AHandler), [TypeInfo(T1), TypeInfo(T2), TypeInfo(T3), TypeInfo(T4)]));
end;

// POST
function TMVCRouteGroup<T>.MapPost(const APath: string;
  const AHandler: TMVCMinimalFunc): TMVCRouteHandle;
begin
  Result := TMVCRouteHandle.Create(RegisterRoute(httpPOST, APath, TMVCThunkFactory.Make0(AHandler), nil));
end;

function TMVCRouteGroup<T>.MapPost<T1>(const APath: string;
  const AHandler: TMVCMinimalFunc<T1>): TMVCRouteHandle;
begin
  Result := TMVCRouteHandle.Create(RegisterRoute(httpPOST, APath, TMVCThunkFactory.Make1<T1>(AHandler), [TypeInfo(T1)]));
end;

function TMVCRouteGroup<T>.MapPost<T1, T2>(const APath: string;
  const AHandler: TMVCMinimalFunc<T1, T2>): TMVCRouteHandle;
begin
  Result := TMVCRouteHandle.Create(RegisterRoute(httpPOST, APath, TMVCThunkFactory.Make2<T1, T2>(AHandler), [TypeInfo(T1), TypeInfo(T2)]));
end;

function TMVCRouteGroup<T>.MapPost<T1, T2, T3>(const APath: string;
  const AHandler: TMVCMinimalFunc<T1, T2, T3>): TMVCRouteHandle;
begin
  Result := TMVCRouteHandle.Create(RegisterRoute(httpPOST, APath, TMVCThunkFactory.Make3<T1, T2, T3>(AHandler), [TypeInfo(T1), TypeInfo(T2), TypeInfo(T3)]));
end;

function TMVCRouteGroup<T>.MapPost<T1, T2, T3, T4>(const APath: string;
  const AHandler: TMVCMinimalFunc<T1, T2, T3, T4>): TMVCRouteHandle;
begin
  Result := TMVCRouteHandle.Create(RegisterRoute(httpPOST, APath, TMVCThunkFactory.Make4<T1, T2, T3, T4>(AHandler), [TypeInfo(T1), TypeInfo(T2), TypeInfo(T3), TypeInfo(T4)]));
end;

// PUT
function TMVCRouteGroup<T>.MapPut(const APath: string;
  const AHandler: TMVCMinimalFunc): TMVCRouteHandle;
begin
  Result := TMVCRouteHandle.Create(RegisterRoute(httpPUT, APath, TMVCThunkFactory.Make0(AHandler), nil));
end;

function TMVCRouteGroup<T>.MapPut<T1>(const APath: string;
  const AHandler: TMVCMinimalFunc<T1>): TMVCRouteHandle;
begin
  Result := TMVCRouteHandle.Create(RegisterRoute(httpPUT, APath, TMVCThunkFactory.Make1<T1>(AHandler), [TypeInfo(T1)]));
end;

function TMVCRouteGroup<T>.MapPut<T1, T2>(const APath: string;
  const AHandler: TMVCMinimalFunc<T1, T2>): TMVCRouteHandle;
begin
  Result := TMVCRouteHandle.Create(RegisterRoute(httpPUT, APath, TMVCThunkFactory.Make2<T1, T2>(AHandler), [TypeInfo(T1), TypeInfo(T2)]));
end;

function TMVCRouteGroup<T>.MapPut<T1, T2, T3>(const APath: string;
  const AHandler: TMVCMinimalFunc<T1, T2, T3>): TMVCRouteHandle;
begin
  Result := TMVCRouteHandle.Create(RegisterRoute(httpPUT, APath, TMVCThunkFactory.Make3<T1, T2, T3>(AHandler), [TypeInfo(T1), TypeInfo(T2), TypeInfo(T3)]));
end;

function TMVCRouteGroup<T>.MapPut<T1, T2, T3, T4>(const APath: string;
  const AHandler: TMVCMinimalFunc<T1, T2, T3, T4>): TMVCRouteHandle;
begin
  Result := TMVCRouteHandle.Create(RegisterRoute(httpPUT, APath, TMVCThunkFactory.Make4<T1, T2, T3, T4>(AHandler), [TypeInfo(T1), TypeInfo(T2), TypeInfo(T3), TypeInfo(T4)]));
end;

// DELETE
function TMVCRouteGroup<T>.MapDelete(const APath: string;
  const AHandler: TMVCMinimalFunc): TMVCRouteHandle;
begin
  Result := TMVCRouteHandle.Create(RegisterRoute(httpDELETE, APath, TMVCThunkFactory.Make0(AHandler), nil));
end;

function TMVCRouteGroup<T>.MapDelete<T1>(const APath: string;
  const AHandler: TMVCMinimalFunc<T1>): TMVCRouteHandle;
begin
  Result := TMVCRouteHandle.Create(RegisterRoute(httpDELETE, APath, TMVCThunkFactory.Make1<T1>(AHandler), [TypeInfo(T1)]));
end;

function TMVCRouteGroup<T>.MapDelete<T1, T2>(const APath: string;
  const AHandler: TMVCMinimalFunc<T1, T2>): TMVCRouteHandle;
begin
  Result := TMVCRouteHandle.Create(RegisterRoute(httpDELETE, APath, TMVCThunkFactory.Make2<T1, T2>(AHandler), [TypeInfo(T1), TypeInfo(T2)]));
end;

function TMVCRouteGroup<T>.MapDelete<T1, T2, T3>(const APath: string;
  const AHandler: TMVCMinimalFunc<T1, T2, T3>): TMVCRouteHandle;
begin
  Result := TMVCRouteHandle.Create(RegisterRoute(httpDELETE, APath, TMVCThunkFactory.Make3<T1, T2, T3>(AHandler), [TypeInfo(T1), TypeInfo(T2), TypeInfo(T3)]));
end;

function TMVCRouteGroup<T>.MapDelete<T1, T2, T3, T4>(const APath: string;
  const AHandler: TMVCMinimalFunc<T1, T2, T3, T4>): TMVCRouteHandle;
begin
  Result := TMVCRouteHandle.Create(RegisterRoute(httpDELETE, APath, TMVCThunkFactory.Make4<T1, T2, T3, T4>(AHandler), [TypeInfo(T1), TypeInfo(T2), TypeInfo(T3), TypeInfo(T4)]));
end;

// PATCH
function TMVCRouteGroup<T>.MapPatch(const APath: string;
  const AHandler: TMVCMinimalFunc): TMVCRouteHandle;
begin
  Result := TMVCRouteHandle.Create(RegisterRoute(httpPATCH, APath, TMVCThunkFactory.Make0(AHandler), nil));
end;

function TMVCRouteGroup<T>.MapPatch<T1>(const APath: string;
  const AHandler: TMVCMinimalFunc<T1>): TMVCRouteHandle;
begin
  Result := TMVCRouteHandle.Create(RegisterRoute(httpPATCH, APath, TMVCThunkFactory.Make1<T1>(AHandler), [TypeInfo(T1)]));
end;

function TMVCRouteGroup<T>.MapPatch<T1, T2>(const APath: string;
  const AHandler: TMVCMinimalFunc<T1, T2>): TMVCRouteHandle;
begin
  Result := TMVCRouteHandle.Create(RegisterRoute(httpPATCH, APath, TMVCThunkFactory.Make2<T1, T2>(AHandler), [TypeInfo(T1), TypeInfo(T2)]));
end;

function TMVCRouteGroup<T>.MapPatch<T1, T2, T3>(const APath: string;
  const AHandler: TMVCMinimalFunc<T1, T2, T3>): TMVCRouteHandle;
begin
  Result := TMVCRouteHandle.Create(RegisterRoute(httpPATCH, APath, TMVCThunkFactory.Make3<T1, T2, T3>(AHandler), [TypeInfo(T1), TypeInfo(T2), TypeInfo(T3)]));
end;

function TMVCRouteGroup<T>.MapPatch<T1, T2, T3, T4>(const APath: string;
  const AHandler: TMVCMinimalFunc<T1, T2, T3, T4>): TMVCRouteHandle;
begin
  Result := TMVCRouteHandle.Create(RegisterRoute(httpPATCH, APath, TMVCThunkFactory.Make4<T1, T2, T3, T4>(AHandler), [TypeInfo(T1), TypeInfo(T2), TypeInfo(T3), TypeInfo(T4)]));
end;

// MapQuery

function TMVCRouteGroup<T>.MapQuery(const APath: string;
  const AHandler: TMVCMinimalFunc): TMVCRouteHandle;
begin
  Result := TMVCRouteHandle.Create(RegisterRoute(httpQUERY, APath, TMVCThunkFactory.Make0(AHandler), nil));
end;

function TMVCRouteGroup<T>.MapQuery<T1>(const APath: string;
  const AHandler: TMVCMinimalFunc<T1>): TMVCRouteHandle;
begin
  Result := TMVCRouteHandle.Create(RegisterRoute(httpQUERY, APath, TMVCThunkFactory.Make1<T1>(AHandler), [TypeInfo(T1)]));
end;

function TMVCRouteGroup<T>.MapQuery<T1, T2>(const APath: string;
  const AHandler: TMVCMinimalFunc<T1, T2>): TMVCRouteHandle;
begin
  Result := TMVCRouteHandle.Create(RegisterRoute(httpQUERY, APath, TMVCThunkFactory.Make2<T1, T2>(AHandler), [TypeInfo(T1), TypeInfo(T2)]));
end;

function TMVCRouteGroup<T>.MapQuery<T1, T2, T3>(const APath: string;
  const AHandler: TMVCMinimalFunc<T1, T2, T3>): TMVCRouteHandle;
begin
  Result := TMVCRouteHandle.Create(RegisterRoute(httpQUERY, APath, TMVCThunkFactory.Make3<T1, T2, T3>(AHandler), [TypeInfo(T1), TypeInfo(T2), TypeInfo(T3)]));
end;

function TMVCRouteGroup<T>.MapQuery<T1, T2, T3, T4>(const APath: string;
  const AHandler: TMVCMinimalFunc<T1, T2, T3, T4>): TMVCRouteHandle;
begin
  Result := TMVCRouteHandle.Create(RegisterRoute(httpQUERY, APath, TMVCThunkFactory.Make4<T1, T2, T3, T4>(AHandler), [TypeInfo(T1), TypeInfo(T2), TypeInfo(T3), TypeInfo(T4)]));
end;

// MapMethods (multi-verb)

function TMVCRouteGroup<T>.MapMethods(const AVerbs: array of TMVCHTTPMethodType;
  const APath: string; const AHandler: TMVCMinimalFunc): TMVCRouteHandle;
begin
  Result := RegisterMany(AVerbs, APath, TMVCThunkFactory.Make0(AHandler), nil);
end;

function TMVCRouteGroup<T>.MapMethods<T1>(const AVerbs: array of TMVCHTTPMethodType;
  const APath: string; const AHandler: TMVCMinimalFunc<T1>): TMVCRouteHandle;
begin
  Result := RegisterMany(AVerbs, APath, TMVCThunkFactory.Make1<T1>(AHandler), [TypeInfo(T1)]);
end;

function TMVCRouteGroup<T>.MapMethods<T1, T2>(const AVerbs: array of TMVCHTTPMethodType;
  const APath: string; const AHandler: TMVCMinimalFunc<T1, T2>): TMVCRouteHandle;
begin
  Result := RegisterMany(AVerbs, APath, TMVCThunkFactory.Make2<T1, T2>(AHandler), [TypeInfo(T1), TypeInfo(T2)]);
end;

function TMVCRouteGroup<T>.MapMethods<T1, T2, T3>(const AVerbs: array of TMVCHTTPMethodType;
  const APath: string; const AHandler: TMVCMinimalFunc<T1, T2, T3>): TMVCRouteHandle;
begin
  Result := RegisterMany(AVerbs, APath, TMVCThunkFactory.Make3<T1, T2, T3>(AHandler), [TypeInfo(T1), TypeInfo(T2), TypeInfo(T3)]);
end;

function TMVCRouteGroup<T>.MapMethods<T1, T2, T3, T4>(const AVerbs: array of TMVCHTTPMethodType;
  const APath: string; const AHandler: TMVCMinimalFunc<T1, T2, T3, T4>): TMVCRouteHandle;
begin
  Result := RegisterMany(AVerbs, APath, TMVCThunkFactory.Make4<T1, T2, T3, T4>(AHandler), [TypeInfo(T1), TypeInfo(T2), TypeInfo(T3), TypeInfo(T4)]);
end;

initialization

gMinimalAPILock := TObject.Create;
gMinimalAPIByEngine := TDictionary<Pointer, TMVCMinimalAPIMiddleware>.Create;

finalization

gMinimalAPIByEngine.Free;
gMinimalAPILock.Free;

end.
