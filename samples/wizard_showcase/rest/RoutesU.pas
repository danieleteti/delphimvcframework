// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// ***************************************************************************


unit RoutesU;

// Minimal API Showcase — five endpoints covering the parameter-binding modes
// you will actually use day to day.
//
// URL surface:
//   GET  /                       inline HTML landing (lists all endpoints)
//   GET  /people                 DI: IPeopleService is resolved from the container
//   GET  /people/(id:int)        primitive arg is bound from a route segment
//   POST /people                 class body JSON + auto-validation
//   GET  /search                 record + [MVCFromQueryString] (per-field defaults)
//
// The /people subtree is mounted via Prefix('/people').Use(LogFilter()), so
// every /people/* route also demonstrates endpoint-filter composition without
// dedicating an extra route to it.
//
// What is NOT shown here (covered conceptually by the patterns above):
//   * TWebContext arg              -> any handler can declare it; covered in
//                                     routes_minimal_web.pas.tpro
//   * Multiple primitives          -> add more route segments + matching args,
//                                     they bind in declaration order
//   * [MVCFromHeader]/[MVCFromCookie]/[MVCFromContentField]/[MVCFromBody]
//                                  -> identical record-attribute pattern as
//                                     TSearchQuery; swap the attribute
//   * Group data (Prefix<T>)       -> classes registered as group data take
//                                     precedence over body / query binding
//   * MapMethods (multi-verb)      -> same shape as MapGet, takes a verb array

interface

uses
  MVCFramework, MVCFramework.MinimalAPI;

procedure ConfigureRoutes(const ARoot: TMVCRouteGroup<TObject>);

implementation

uses
  System.SysUtils,
  MVCFramework.Commons,
  MVCFramework.Logger,
  EntitiesU,
  ShowcaseModelsU,
  ServicesU;

// =============================================================================
// Endpoint filter — composition demo, attached via .Use on the /people group.
// Every route registered on that group inherits this filter. Add more filters
// (auth, rate-limit, role checks) the same way.
// =============================================================================
function LogFilter: TMVCEndpointFilter;
begin
  Result := function (const Ctx: TWebContext;
                       const Next: TMVCEndpointFilterNext): IMVCResponse
    begin
      LogI('-> ' + Ctx.Request.PathInfo);
      Result := Next();
      LogI('<- status ' + Result.StatusCode.ToString);
    end;
end;

// =============================================================================
// Landing page — inline HTML. Self-documents every endpoint with a curl
// example. Kept inline (no view engine, no template file) so the showcase
// preset stays a single-file demo with no external assets to ship.
// =============================================================================
function LandingHTML: string;
begin
  Result :=
    '<!DOCTYPE html>' + sLineBreak +
    '<html><head><meta charset="utf-8"><title>Minimal API Showcase</title>' + sLineBreak +
    '<style>body{font-family:system-ui,sans-serif;max-width:880px;margin:2em auto;padding:0 1em;color:#222}' + sLineBreak +
    'h1{margin-bottom:.2em} h2{margin-top:1.6em;font-size:1.1em;color:#555}' + sLineBreak +
    'code,pre{background:#f4f4f4;padding:.1em .3em;border-radius:3px;font-size:.92em}' + sLineBreak +
    'pre{padding:.6em;overflow-x:auto} .verb{display:inline-block;width:3.5em;font-weight:600;color:#0a6}' + sLineBreak +
    '.verb.post{color:#d50} .path{font-family:monospace}</style></head><body>' + sLineBreak +
    '<h1>Minimal API Showcase</h1>' + sLineBreak +
    '<p>Five endpoints, each demonstrating a different parameter-binding mode.</p>' + sLineBreak +
    '<h2><span class="verb">GET</span> <span class="path">/people</span> &mdash; DI interface arg</h2>' + sLineBreak +
    '<pre>curl http://localhost:8080/people</pre>' + sLineBreak +
    '<h2><span class="verb">GET</span> <span class="path">/people/{id}</span> &mdash; primitive route segment</h2>' + sLineBreak +
    '<pre>curl http://localhost:8080/people/42</pre>' + sLineBreak +
    '<h2><span class="verb post">POST</span> <span class="path">/people</span> &mdash; class body JSON + auto-validation</h2>' + sLineBreak +
    '<pre>curl -X POST http://localhost:8080/people \' + sLineBreak +
    '     -H "Content-Type: application/json" \' + sLineBreak +
    '     -d ''{"firstName":"Ada","lastName":"Lovelace","email":"ada@example.com"}''</pre>' + sLineBreak +
    '<p>Try posting an invalid email or empty field &mdash; the validator short-circuits with a 400 ProblemDetails.</p>' + sLineBreak +
    '<h2><span class="verb">GET</span> <span class="path">/search</span> &mdash; record + [MVCFromQueryString]</h2>' + sLineBreak +
    '<pre>curl "http://localhost:8080/search?q=ada&amp;page=2&amp;size=10"</pre>' + sLineBreak +
    '<p>Omit any query param &mdash; the record fields fall back to the defaults declared in the attribute.</p>' + sLineBreak +
    '<p style="margin-top:2em;color:#888;font-size:.85em">Read <code>RoutesU.pas</code> and <code>ShowcaseModelsU.pas</code> &mdash; both files are heavily commented.</p>' + sLineBreak +
    '</body></html>';
end;

// =============================================================================
// Routes
// =============================================================================
procedure ConfigureRoutes(const ARoot: TMVCRouteGroup<TObject>);
var
  lPeople: TMVCRouteGroup<TObject>;
begin
  // 1. Arity-0 handler. No parameter list. Returns an HTML response built
  //    inline; no view engine involved. TMVCHTMLResponse stamps the response
  //    with Content-Type text/html; charset=utf-8 automatically.
  ARoot.MapGet('',
    function: IMVCResponse
    var
      lResp: TMVCHTMLResponse;
    begin
      lResp := TMVCHTMLResponse.Create;
      lResp.HTMLBody := LandingHTML;
      Result := lResp;
    end);

  // 2..4 are mounted under /people with a shared LogFilter. The group's
  // .Use(...) prepends the filter to every route registered on lPeople; routes
  // declared elsewhere on ARoot are unaffected.
  lPeople := ARoot.Prefix('/people').Use(LogFilter());

  // 2. DI INTERFACE arg. Any TMVCMinimalFunc generic parameter whose type
  //    is an interface is resolved from ServiceContainerResolver. IPeopleService
  //    is registered in ServicesU.RegisterServices.
  lPeople.MapGet<IPeopleService>('',
    function (Svc: IPeopleService): IMVCResponse
    begin
      Result := Ok(Svc.GetAll);
    end);

  // 3. PRIMITIVE arg. Integer / Int64 / string / Boolean / Double / TGUID /
  //    TDateTime params are bound to the next unconsumed route segment, in
  //    declaration order. ($id:int) carries a constraint: non-numeric ids
  //    fall through to a 404 before the handler runs.
  lPeople.MapGet<Integer>('/($id:int)',
    function (ID: Integer): IMVCResponse
    begin
      Result := Ok(TPerson.Create(ID, 'Daniele', 'Teti', EncodeDate(1979, 11, 4)));
    end);

  // 4. CLASS BODY arg + AUTO-VALIDATION. TPersonInput descends from
  //    TMVCValidatable and carries [MVCRequired] / [MVCEmail] attributes;
  //    the arg resolver invokes TMVCValidationEngine.ValidateAndRaise
  //    after JSON deserialization, before this handler. Invalid payloads
  //    short-circuit with a ProblemDetails 400.
  //
  //    .WithSummary attaches an OpenAPI operation summary via the chainable
  //    handle. The same record exposes .WithName, .WithDescription, .WithTags,
  //    .Produces<T>, .WithDeprecated, .WithOpenAPI.
  lPeople.MapPost<TPersonInput>('',
    function (Input: TPersonInput): IMVCResponse
    begin
      LogI('Created ' + Input.FirstName + ' ' + Input.LastName + ' <' + Input.Email + '>');
      Result := Created('', 'Person created');
    end).WithSummary('Create a new person (validated)');

  // 5. RECORD with per-field source attributes. TSearchQuery declares its
  //    binding map in ShowcaseModelsU. The same shape works for
  //    [MVCFromHeader], [MVCFromCookie], [MVCFromContentField] (form-urlencoded
  //    fields, including TArray<string> for multi-value selects), and
  //    [MVCFromBody] (raw body or nested-JSON-as-class).
  ARoot.MapGet<TSearchQuery>('/search',
    function (Q: TSearchQuery): IMVCResponse
    begin
      Result := Ok(Format('term="%s" page=%d size=%d', [Q.Term, Q.Page, Q.PageSize]));
    end);

  // 6. FILE UPLOAD. A TMVCFormFile argument binds the first multipart file.
  //    A record can mix [MVCFromFile] with [MVCFromContentField] text fields.
  ARoot.MapPost<TMVCFormFile>('/upload',
    function (Doc: TMVCFormFile): IMVCResponse
    begin
      if Doc = nil then
        Result := BadRequest('no file uploaded')
      else
        Result := Ok(Format('received "%s" (%d bytes, %s)',
          [Doc.FileName, Doc.Size, Doc.ContentType]));
    end).WithSummary('Upload a file (multipart/form-data)');

  // 7. QUERY ARRAY. Repeated ?tag= keys bind to TArray<string>.
  ARoot.MapGet<TTagSearch>('/tags',
    function (Q: TTagSearch): IMVCResponse
    begin
      Result := Ok(Format('%d tag(s)', [Length(Q.Tags)]));
    end);
end;

end.
