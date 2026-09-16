unit RoutesU;

{$I dmvcframework.inc}

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.MinimalAPI;

procedure ConfigureRoutes(const ARoot: TMVCRouteGroup<TObject>);

implementation

uses
  System.SysUtils,
  JsonDataObjects,
  HooksU;

procedure ConfigureRoutes(const ARoot: TMVCRouteGroup<TObject>);
var
  lPublic: TMVCRouteGroup<TObject>;
  lApi: TMVCRouteGroup<TObject>;
  lAdmin: TMVCRouteGroup<TObject>;
begin
  // ---------------------------------------------------------------------
  // PUBLIC: logging + business-error mapping. No auth.
  //
  // Filter ordering matters: filters wrap each other. Registration order
  // = OUTER-to-INNER. We want LoggingFilter OUTERMOST so it ALWAYS runs
  // (even when an inner filter short-circuits or maps an exception to a
  // response). What it logs:
  //   - on success/short-circuit/mapped error: REQ, RES <final-status>, DONE
  //   - on un-mapped exception: REQ, ERR <classname>, DONE
  // Inner-mapped exceptions are NOT logged as ERR — they appear as RES with
  // the mapped status code. That's the correct chain-of-responsibility
  // semantic: the OUTER caller observes what the INNER returned.
  // ---------------------------------------------------------------------
  // ARoot is a TMVCRouteGroup<TObject>. MapXxx now returns TMVCRouteHandle
  // (per-endpoint), so chaining multiple MapXxx no longer works — each
  // route is a separate statement. Filters are still applied via Use()
  // BEFORE the first MapXxx call; we capture the configured group into a
  // local so we can register many routes against it.
  lPublic := ARoot
    .Use(LoggingFilter())
    .Use(BusinessErrorsFilter());

  lPublic.MapGet('/health',
    function: IMVCResponse
    begin
      Result := Ok('OK');
    end).WithName('health');     // <-- per-endpoint .WithName demo

  lPublic.MapGet('/throw',
    function: IMVCResponse
    begin
      raise Exception.Create('boom');
    end);

  lPublic.MapGet('/throw-token',
    function: IMVCResponse
    begin
      raise ETokenError.Create('token expired');
    end);

  // ---------------------------------------------------------------------
  // /api: Bearer auth required.
  //   Filter stack is applied in registration order:
  //     LoggingFilter (outermost) -> BearerAuthFilter -> BusinessErrors
  //                                                  -> handler
  // ---------------------------------------------------------------------
  lApi := ARoot
    .Prefix('/api')
    .Use(LoggingFilter())         // outermost: always observes
    .Use(BusinessErrorsFilter())  // maps ETokenError -> 401
    .Use(BearerAuthFilter());     // innermost gate: short-circuits 401 if no token

  // POST /api/widgets — demonstrates auto-validation. The TWidgetDto
  // (validatable, requires Name length >= 3 and Qty > 0) is bound from
  // the request body. If validation fails, the framework raises
  // EMVCValidationException which the middleware turns into a
  // ProblemDetails 400 response.
  lApi.MapPost<TWidgetDto>('/widgets',
    function (W: TWidgetDto): IMVCResponse
    begin
      Result := Ok(Format('{"name":"%s","qty":%d}', [W.Name, W.Qty]));
    end);

  lApi.MapGet<TWebContext>('/me',
    function (Ctx: TWebContext): IMVCResponse
    var
      P: TPrincipal;
      J: TJsonObject;
    begin
      P := CurrentPrincipal(Ctx);
      J := TJsonObject.Create;
      try
        J.S['user'] := P.User;
        J.S['role'] := P.Role;
        Result := Ok(J.ToJSON());
      finally
        J.Free;
      end;
    end);

  lApi.MapGet('/throw-token',
    function: IMVCResponse
    begin
      raise ETokenError.Create('token expired');
    end);

  // Route constraints: only an integer matches /api/orders/<id>.
  // Non-numeric IDs fall through to a 404 (no minimal route matches).
  lApi.MapGet<Integer>('/orders/($id:int)',
    function (ID: Integer): IMVCResponse
    begin
      Result := Ok(Format('{"order":%d}', [ID]));
    end);

  // Route with a GUID constraint
  lApi.MapGet<TGUID>('/items/($id:guid)',
    function (ID: TGUID): IMVCResponse
    begin
      Result := Ok(Format('{"item":"%s"}', [GUIDToString(ID)]));
    end);

  // Multi-verb shortcut: same handler for both GET and POST on the
  // same path (think a debugging echo endpoint).
  lApi.MapMethods<TWebContext>([httpGET, httpPOST], '/echo',
    function (Ctx: TWebContext): IMVCResponse
    begin
      Result := Ok(Format('%s %s', [Ctx.Request.HTTPMethodAsString,
        Ctx.Request.PathInfo]));
    end);

  // ---------------------------------------------------------------------
  // /api/admin: nested group. Plain Prefix preserves the inherited
  // filter stack and ADDS RequireRoleFilter('admin') on top.
  // ---------------------------------------------------------------------
  lAdmin := lApi
    .Prefix('/admin')
    .Use(RequireRoleFilter('admin'));

  lAdmin.MapGet('/audit',
    function: IMVCResponse
    var
      lLines: TArray<string>;
      lJson: TJsonArray;
      L: string;
    begin
      lLines := TAuditLog.Instance.Snapshot;
      lJson := TJsonArray.Create;
      try
        for L in lLines do
          lJson.Add(L);
        Result := Ok(lJson.ToJSON());
      finally
        lJson.Free;
      end;
    end);

  lAdmin.MapDelete('/audit',
    function: IMVCResponse
    begin
      TAuditLog.Instance.Clear;
      Result := NoContent;
    end);
end;

end.
