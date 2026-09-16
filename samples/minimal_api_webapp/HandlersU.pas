unit HandlersU;

// =============================================================================
//  Routes for samples/minimal_api_webapp
//
//  Everything is wired against TMVCEngine.Root.AsWeb / Prefix.AsWeb — no controller
//  class, no MVC* attributes on routing. Handlers are anonymous functions
//  returning IMVCResponse. ViewData and RenderView are GLOBAL functions
//  resolved against a per-request threadvar (set by the minimal-API
//  middleware on entry, cleared on exit).
//
//  The /admin subtree is protected by the RequireLogin filter (cookie session
//  driven). Login form posts a TLoginForm record whose fields are filled from
//  application/x-www-form-urlencoded body via [MVCFromContentField].
// =============================================================================

interface

uses
  MVCFramework,
  MVCFramework.MinimalAPI,
  MVCFramework.Filters;

procedure RegisterRoutes(AEngine: TMVCEngine);

implementation

uses
  System.SysUtils,
  System.Classes,
  Data.DB,
  MVCFramework.Commons,
  MVCFramework.HTMX,
  RandomUtilsU;

type
  // Form binding record for the /login POST. Each field declares the
  // corresponding form-urlencoded / multipart field name via
  // [MVCFromContentField]. Default values follow the same '(name, default)'
  // constructor convention as MVCFromQueryString / MVCFromHeader.
  TLoginForm = record
    [MVCFromContentField('username')] Username: string;
    [MVCFromContentField('password')] Password: string;
  end;

// Express-style filter recipe: redirect to ARedirectTo unless the session
// carries a 'user' value. Returned as a TMVCEndpointFilter so it can be
// attached with .Use(...) to a group built via .AsWeb.
function RequireLogin(const ARedirectTo: string): TMVCEndpointFilter;
begin
  Result :=
    function (const Ctx: TWebContext; const Next: TMVCEndpointFilterNext): IMVCResponse
    begin
      if Ctx.Session['user'].IsEmpty then
        Result := Redirect(ARedirectTo)
      else
        Result := Next();
    end;
end;

procedure RegisterRoutes(AEngine: TMVCEngine);
var
  lWeb: TMVCRouteGroup<TObject>;
  lAdmin: TMVCRouteGroup<TObject>;
begin
  // ---------------------------------------------------------------------------
  // Public web routes (no auth) — .AsWeb stamps every route as rkWeb, so
  // none of them surface in the OpenAPI spec.
  //
  // MemorySession is a plain group filter — every nested group (e.g. /admin
  // below) inherits it through the normal filter-inheritance rules. No
  // classic IMVCMiddleware involved.
  // ---------------------------------------------------------------------------
  lWeb := AEngine.Root.AsWeb.Use(MemorySession(10));

  lWeb.MapGet<TWebContext>('/',
    function (Ctx: TWebContext): IMVCResponse
    begin
      ViewData['ispage'] := not Ctx.Request.IsHTMX;
      Result := RenderView('pages/home');
    end);

  lWeb.MapGet<TWebContext>('/users',
    function (Ctx: TWebContext): IMVCResponse
    begin
      ViewData['ispage'] := not Ctx.Request.IsHTMX;
      var lUsers := GetUsers();
      try
        ViewData['users'] := lUsers;
        Result := RenderView('pages/users');
      finally
        lUsers.Free;
      end;
    end);

  lWeb.MapGet<TWebContext>('/customers',
    function (Ctx: TWebContext): IMVCResponse
    begin
      ViewData['ispage'] := not Ctx.Request.IsHTMX;
      var lCustomers := GetPeople();
      try
        ViewData['customers'] := lCustomers;
        Result := RenderView('pages/customers');
      finally
        lCustomers.Free;
      end;
    end);

  lWeb.MapGet<TWebContext>('/posts',
    function (Ctx: TWebContext): IMVCResponse
    begin
      ViewData['ispage'] := not Ctx.Request.IsHTMX;
      var lPosts := GetPosts(20);
      try
        ViewData['posts'] := lPosts;
        Result := RenderView('pages/posts');
      finally
        lPosts.Free;
      end;
    end);

  lWeb.MapGet<TWebContext>('/login',
    function (Ctx: TWebContext): IMVCResponse
    begin
      ViewData['ispage'] := not Ctx.Request.IsHTMX;
      Result := RenderView('pages/login');
    end);

  // Form binding: TLoginForm fields are filled from form-urlencoded /
  // multipart text fields via [MVCFromContentField]. The handler also
  // pulls the context so it can write to the cookie session.
  lWeb.MapPost<TWebContext, TLoginForm>('/login',
    function (Ctx: TWebContext; F: TLoginForm): IMVCResponse
    begin
      if (F.Username = 'admin') and (F.Password = 'admin') then
      begin
        Ctx.Session['user'] := F.Username;
        Result := Redirect('/admin/');
      end
      else
      begin
        ViewData['ispage'] := not Ctx.Request.IsHTMX;
        ViewData['error'] := 'Invalid credentials';
        Result := RenderView('pages/login');
        Result.StatusCode := 401;
      end;
    end);

  // ---------------------------------------------------------------------------
  // Session-protected admin subtree. Built as a nested Prefix of lWeb so it
  // inherits the MemorySession filter applied above. The .Use(RequireLogin)
  // filter sits ahead of every handler in this group; if it short-circuits
  // with a redirect, the handler never runs.
  // ---------------------------------------------------------------------------
  lAdmin := lWeb.Prefix('/admin').Use(RequireLogin('/login'));

  lAdmin.MapGet<TWebContext>('/',
    function (Ctx: TWebContext): IMVCResponse
    begin
      ViewData['ispage'] := not Ctx.Request.IsHTMX;
      ViewData['user'] := Ctx.Session['user'];
      Result := RenderView('pages/admin_home');
    end);

  lAdmin.MapGet<TWebContext>('/logout',
    function (Ctx: TWebContext): IMVCResponse
    begin
      Ctx.SessionStop;
      Result := Redirect('/login');
    end);
end;

end.
