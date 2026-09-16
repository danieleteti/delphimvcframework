unit RoutesU;

{$I dmvcframework.inc}

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.MinimalAPI,
  Entities.PersonU;

type
  TPeopleSearchRequest = record
    [MVCFromQueryString('page', 1)]
    Page: Integer;
    [MVCFromQueryString('pageSize', 20)]
    PageSize: Integer;
    [MVCFromHeader('X-Tenant', 'default')]
    Tenant: string;
  end;

procedure ConfigureRoutes(const ARoot: TMVCRouteGroup<TObject>);

implementation

uses
  System.SysUtils,
  System.Diagnostics,
  JsonDataObjects,
  MVCFramework.Logger,
  MVCFramework.Container,
  Services.PeopleU;

// =============================================================================
// Filter builders
// =============================================================================

// One filter that does request logging + elapsed time, around the entire chain.
function RequestLoggingFilter: TMVCEndpointFilter;
begin
  Result := function (const Ctx: TWebContext;
                       const Next: TMVCEndpointFilterNext): IMVCResponse
    var
      sw: TStopwatch;
    begin
      sw := TStopwatch.StartNew;
      LogI(Format('--> %s %s', [Ctx.Request.HTTPMethodAsString,
        Ctx.Request.PathInfo]));
      try
        try
          Result := Next();
          if Result <> nil then
            LogI(Format('<-- %d %s', [Result.StatusCode, Ctx.Request.PathInfo]));
        except
          on E: Exception do
          begin
            LogE(Format('XX  %s -> %s: %s', [Ctx.Request.PathInfo,
              E.ClassName, E.Message]));
            raise;
          end;
        end;
      finally
        LogI(Format('    elapsed=%dms', [sw.ElapsedMilliseconds]));
      end;
    end;
end;

// "Not found" exceptions become a clean 404 (vs the default 500).
function NotFoundMapperFilter: TMVCEndpointFilter;
begin
  Result := function (const Ctx: TWebContext;
                       const Next: TMVCEndpointFilterNext): IMVCResponse
    begin
      try
        Result := Next();
      except
        on E: Exception do
          if Pos('not found', LowerCase(E.Message)) > 0 then
            Result := NotFound(E.Message)
          else
            raise;
      end;
    end;
end;

// =============================================================================

procedure ConfigureRoutes(const ARoot: TMVCRouteGroup<TObject>);
var
  lV1, lV2: TMVCRouteGroup<TApiVersion>;
begin
  // ---------------------------------------------------------------------
  // Root-level routes (no prefix, no version, no filters).
  // ---------------------------------------------------------------------
  ARoot.MapGet('/health',
    function: IMVCResponse
    begin
      Result := Ok('OK');
    end);

  // ---------------------------------------------------------------------
  // /v1 — DEPRECATED API version. Two filters:
  //   * RequestLoggingFilter (logs + elapsed time)
  //   * NotFoundMapperFilter (turns "not found" exceptions into 404)
  // Group data: TApiVersion handed to handlers as a parameter.
  // ---------------------------------------------------------------------
  lV1 := ARoot
    .Prefix<TApiVersion>('/v1', TApiVersion.Create(1, True, '2026-12-31'))
    .Use(RequestLoggingFilter())
    .Use(NotFoundMapperFilter());

  // GET /v1/people — handler reads group data (TApiVersion) AND a DI
  // service. Returns the people list with a deprecation note.
  // The body is built as a real TJsonObject so the framework serializes
  // it cleanly. Passing a string to Ok() would wrap it in {"message":...}.
  lV1.MapGet<TApiVersion, IPeopleService>('/people',
    function (Ver: TApiVersion; Svc: IPeopleService): IMVCResponse
    var
      lAll: TPeopleList;
      lJson: TJsonObject;
    begin
      lAll := Svc.GetAll;
      try
        lJson := TJsonObject.Create;
        lJson.I['version'] := Ver.Number;
        lJson.I['count'] := lAll.Count;
        if Ver.Deprecated then
        begin
          lJson.B['deprecated'] := True;
          lJson.S['sunset'] := Ver.Sunset;
        end;
      finally
        lAll.Free;
      end;
      Result := Ok(lJson);
    end);

  // GET /v1/error — verifies the NotFoundMapper filter:
  // exception with "not found" in the message turns into a 404.
  lV1.MapGet('/error',
    function: IMVCResponse
    begin
      raise Exception.Create('thing not found');
    end);

  // ---------------------------------------------------------------------
  // /v2 — current. Same filter stack (re-applied — typed Prefix doesn't
  // propagate filters across the type-parameter boundary), different
  // group data (Deprecated=False).
  // ---------------------------------------------------------------------
  lV2 := ARoot
    .Prefix<TApiVersion>('/v2', TApiVersion.Create(2, False))
    .Use(RequestLoggingFilter())
    .Use(NotFoundMapperFilter());

  lV2.MapGet<TApiVersion, IPeopleService>('/people',
    function (Ver: TApiVersion; Svc: IPeopleService): IMVCResponse
    var
      lAll: TPeopleList;
      lJson: TJsonObject;
    begin
      lAll := Svc.GetAll;
      try
        lJson := TJsonObject.Create;
        lJson.I['version'] := Ver.Number;
        lJson.I['count'] := lAll.Count;
      finally
        lAll.Free;
      end;
      Result := Ok(lJson);
    end);

  lV2.MapGet<Integer>('/people/($id)',
    function (ID: Integer): IMVCResponse
    var
      lSvc: IPeopleService;
      lPerson: TPerson;
    begin
      lSvc := NewServiceContainerResolver
        .Resolve(TypeInfo(IPeopleService)) as IPeopleService;
      lPerson := lSvc.GetByID(ID);
      if lPerson = nil then
        Result := NotFound(Format('Person %d not found', [ID]))
      else
        Result := Ok(lPerson);
    end);

  lV2.MapPost<TPerson, IPeopleService>('/people',
    function (Person: TPerson; Svc: IPeopleService): IMVCResponse
    var
      lCreated: TPerson;
    begin
      lCreated := Svc.Add(Person);
      Result := Created(Format('/v2/people/%d', [lCreated.ID]), lCreated);
    end);

  lV2.MapPut<Integer, TPerson, IPeopleService>('/people/($id)',
    function (ID: Integer; Person: TPerson; Svc: IPeopleService): IMVCResponse
    begin
      Svc.Update(ID, Person);
      Result := NoContent;
    end);

  lV2.MapDelete<Integer, IPeopleService>('/people/($id)',
    function (ID: Integer; Svc: IPeopleService): IMVCResponse
    begin
      Svc.Delete(ID);
      Result := NoContent;
    end);

  // ---------------------------------------------------------------------
  // /v2/admin — sub-group of /v2. Adds an api-key gate on top of the
  // inherited filter stack.
  // ---------------------------------------------------------------------
  lV2
    .Prefix('/admin')
    .Use(function (const Ctx: TWebContext;
                    const Next: TMVCEndpointFilterNext): IMVCResponse
      begin
        if Ctx.Request.Headers['X-Admin-Key'] <> 's3cret' then
          Result := Status(401, 'admin key required')
        else
          Result := Next();
      end)
    .MapGet('/stats',
      function: IMVCResponse
      var
        lJson: TJsonObject;
      begin
        lJson := TJsonObject.Create;
        lJson.S['stats'] := 'all good';
        Result := Ok(lJson);
      end);

  // ---------------------------------------------------------------------
  // /search — record hybrid binding (no group data, no extra filters).
  // ---------------------------------------------------------------------
  ARoot.MapGet<TPeopleSearchRequest, IPeopleService>('/search',
    function (Req: TPeopleSearchRequest; Svc: IPeopleService): IMVCResponse
    var
      lAll: TPeopleList;
      lJson: TJsonObject;
    begin
      lAll := Svc.GetAll;
      try
        lJson := TJsonObject.Create;
        lJson.S['tenant'] := Req.Tenant;
        lJson.I['page'] := Req.Page;
        lJson.I['pageSize'] := Req.PageSize;
        lJson.I['total'] := lAll.Count;
      finally
        lAll.Free;
      end;
      Result := Ok(lJson);
    end);
end;

end.
