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

// Minimal API WebApp Showcase — server-rendered companion to the REST
// showcase. Each handler returns an IMVCResponse produced by RenderView,
// driven by ViewData populated in the handler body.
//
// URL surface:
//   GET    /                       landing index — lists every endpoint with links
//   GET    /people                 DI: IPeopleService resolved from container
//   GET    /people/(id:int)        primitive arg from a route segment
//   DELETE /people/(id:int)        MapDelete + HTMX hx-delete; returns empty body
//   GET    /search                 record + [MVCFromQueryString] with defaults
//   GET    /signup                 form view (no binding)
//   POST   /signup                 record + [MVCFromContentField] including TArray<string>
//   GET    /context                record + [MVCFromHeader] + [MVCFromCookie]
//
// /people/* is mounted via Prefix('/people').Use(LogFilter()), so every route
// under that prefix also demonstrates endpoint-filter composition.
//
// Session is set up via the MemorySession() filter on the public group, exactly
// as in the default minimal-web scaffold — kept here so /context can demo a
// cookie round-trip without extra ceremony.

interface

uses
  MVCFramework, MVCFramework.MinimalAPI, MVCFramework.Filters;

procedure ConfigureRoutes(const ARoot: TMVCRouteGroup<TObject>);

implementation

uses
  System.SysUtils,
  System.Generics.Collections,
  Web.HTTPApp, // TCookie
  MVCFramework.Commons,
  MVCFramework.Logger,
  MVCFramework.HTMX,
  EntitiesU,
  ShowcaseModelsU,
  ServicesU;

// =============================================================================
// Endpoint filter — attached to /people group via .Use.
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
// Helpers — pre-render dynamic chunks as HTML strings so views stay simple
// (no TemplatePro iteration over Pascal collections needed).
// =============================================================================
function PeopleListHTML(const APeople: TObjectList<TPerson>): string;
var
  lP: TPerson;
  lSB: TStringBuilder;
begin
  // Rendered as a Bootstrap table; each row carries an id="person-N" so the
  // DELETE handler can target it via hx-target="closest tr" hx-swap="outerHTML".
  lSB := TStringBuilder.Create;
  try
    lSB.Append('<table class="table table-hover align-middle">');
    lSB.Append('<thead><tr><th>ID</th><th>Name</th><th class="text-end">Actions</th></tr></thead><tbody>');
    for lP in APeople do
      lSB.AppendFormat(
        '<tr id="person-%d">' +
        '<td>%d</td>' +
        '<td><a href="/people/%d">%s %s</a></td>' +
        '<td class="text-end">' +
          '<button type="button" class="btn btn-sm btn-outline-danger"' +
          ' hx-delete="/people/%d"' +
          ' hx-target="closest tr"' +
          ' hx-swap="outerHTML swap:300ms"' +
          ' hx-confirm="Delete person #%d?">Delete</button>' +
        '</td></tr>',
        [lP.ID.Value, lP.ID.Value, lP.ID.Value, lP.FirstName, lP.LastName,
         lP.ID.Value, lP.ID.Value]);
    lSB.Append('</tbody></table>');
    Result := lSB.ToString;
  finally
    lSB.Free;
  end;
end;

function InterestsHTML(const AItems: TArray<string>): string;
var
  lItem: string;
begin
  if Length(AItems) = 0 then
    Exit('<em>none selected</em>');
  Result := '<ul>';
  for lItem in AItems do
    Result := Result + '<li>' + lItem + '</li>';
  Result := Result + '</ul>';
end;

// =============================================================================
// Routes
// =============================================================================
procedure ConfigureRoutes(const ARoot: TMVCRouteGroup<TObject>);
var
  lWeb: TMVCRouteGroup<TObject>;
  lPeople: TMVCRouteGroup<TObject>;
begin
  // .AsWeb stamps every route rkWeb (excluded from OpenAPI by default).
  // MemorySession is a filter inherited by nested groups; /context reads and
  // writes the 'theme' cookie through TWebContext.Cookie() (set below).
  lWeb := ARoot.AsWeb.Use(MemorySession(30));

  // Handlers below follow the ASP.NET Core Minimal APIs discipline: typed
  // args for every dependency, no static ambient helpers. The only ambient
  // is ViewData (data flowing OUT to the view engine — different layer).

  // 1. Landing — TWebContext + RenderView. The view extends baselayout.html
  //    and lists every showcase endpoint with try-it links.
  lWeb.MapGet<TWebContext>('/',
    function (Ctx: TWebContext): IMVCResponse
    begin
      ViewData['ispage'] := not Ctx.Request.IsHTMX;
      Result := RenderView('pages/index');
    end);

  // 2..3 mounted under /people with a shared LogFilter.
  lPeople := lWeb.Prefix('/people').Use(LogFilter());

  // 2. DI INTERFACE arg. IPeopleService is registered in ServicesU; minimal API
  //    resolves any interface argument from the service container.
  lPeople.MapGet<TWebContext, IPeopleService>('',
    function (Ctx: TWebContext; Svc: IPeopleService): IMVCResponse
    var
      lList: TObjectList<TPerson>;
    begin
      lList := Svc.GetAll;
      try
        ViewData['ispage'] := not Ctx.Request.IsHTMX;
        ViewData['people_html'] := PeopleListHTML(lList);
        Result := RenderView('pages/people');
      finally
        lList.Free;
      end;
    end);

  // 3. PRIMITIVE arg. ($id:int) constrains to numerics; non-numeric ids
  //    fall through to a 404 before this handler runs.
  lPeople.MapGet<TWebContext, Integer>('/($id:int)',
    function (Ctx: TWebContext; ID: Integer): IMVCResponse
    begin
      ViewData['ispage'] := not Ctx.Request.IsHTMX;
      ViewData['id'] := ID;
      ViewData['fullname'] := 'Daniele Teti';
      Result := RenderView('pages/person');
    end);

  // 3b. MapDelete + HTMX. Bound to the "Delete" button rendered per row by
  //     PeopleListHTML. Returns an empty 200 response — htmx swaps the matched
  //     <tr> out (hx-swap="outerHTML"), so the row disappears with the
  //     swap:300ms transition. Real implementations would call IPeopleService.
  lPeople.MapDelete<Integer>('/($id:int)',
    function (ID: Integer): IMVCResponse
    begin
      LogI('deleted person id=' + ID.ToString);
      Result := Ok(''); // empty body; htmx swaps out the targeted <tr>
    end);

  // 4. RECORD with [MVCFromQueryString]. TSearchQuery (declared in
  //    ShowcaseModelsU) carries per-field defaults — a request that omits
  //    a query param still produces a valid value. Same pattern works for
  //    [MVCFromHeader], [MVCFromCookie], [MVCFromContentField], [MVCFromBody]:
  //    swap the attribute.
  lWeb.MapGet<TWebContext, TSearchQuery>('/search',
    function (Ctx: TWebContext; Q: TSearchQuery): IMVCResponse
    begin
      ViewData['ispage'] := not Ctx.Request.IsHTMX;
      ViewData['term'] := Q.Term;
      ViewData['page'] := Q.Page;
      ViewData['size'] := Q.PageSize;
      Result := RenderView('pages/search');
    end);

  // 5a. /signup GET — form view, no binding. Just renders the signup view.
  lWeb.MapGet<TWebContext>('/signup',
    function (Ctx: TWebContext): IMVCResponse
    begin
      ViewData['ispage'] := not Ctx.Request.IsHTMX;
      ViewData['submitted'] := False;
      Result := RenderView('pages/signup');
    end);

  // 5b. /signup POST — record bound via [MVCFromContentField]. The Interests
  //     field is TArray<string>, populated from EVERY occurrence of the
  //     'interest' form field (multi-checkbox), not just the last.
  lWeb.MapPost<TWebContext, TSignupForm>('/signup',
    function (Ctx: TWebContext; Form: TSignupForm): IMVCResponse
    begin
      LogI('signup ' + Form.Username + ' <' + Form.Email + '> interests=' +
        IntToStr(Length(Form.Interests)));
      ViewData['ispage'] := not Ctx.Request.IsHTMX;
      ViewData['submitted'] := True;
      ViewData['username'] := Form.Username;
      ViewData['email'] := Form.Email;
      ViewData['newsletter'] := Form.Newsletter;
      ViewData['interests_html'] := InterestsHTML(Form.Interests);
      Result := RenderView('pages/signup');
    end);

  // 6. /context — record with [MVCFromHeader] and [MVCFromCookie]. The handler
  //    also writes the 'theme' cookie back on every request so subsequent
  //    visits return the same Theme value through the record. Toggle the
  //    cookie value in DevTools and reload to see the binding round-trip.
  lWeb.MapGet<TWebContext, TContextInfo>('/context',
    function (Ctx: TWebContext; Info: TContextInfo): IMVCResponse
    var
      lCookie: TCookie;
    begin
      lCookie := Ctx.Response.Cookies.Add;
      lCookie.Name := 'theme';
      lCookie.Value := Info.Theme;
      lCookie.Path := '/';

      ViewData['ispage'] := not Ctx.Request.IsHTMX;
      ViewData['user_agent'] := Info.UserAgent;
      ViewData['language'] := Info.Language;
      ViewData['theme'] := Info.Theme;
      Result := RenderView('pages/context');
    end);

  // 7. WILDCARD. ($slug:*) captures the rest of the path (slashes included),
  //    handy for doc trees, virtual paths, slugs with separators.
  lWeb.MapGet<TWebContext, string>('/docs/($slug:*)',
    function (Ctx: TWebContext; Slug: string): IMVCResponse
    begin
      ViewData['ispage'] := not Ctx.Request.IsHTMX;
      ViewData['slug'] := Slug;
      Result := RenderView('pages/docs');
    end);
end;

end.
