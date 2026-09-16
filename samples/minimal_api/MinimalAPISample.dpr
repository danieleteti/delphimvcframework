program MinimalAPISample;

{$APPTYPE CONSOLE}

// =============================================================================
//  DMVCFramework Minimal API — parameter binding conventions
// =============================================================================
//
//  Each handler parameter is bound automatically based on its DECLARED TYPE.
//  No attribute on the parameter itself is needed — the framework decides
//  the source from the type:
//
//  +-----------------------+--------------------------------------------------+
//  | Parameter type        | Bound from                                       |
//  +-----------------------+--------------------------------------------------+
//  | TWebContext           | the request context (full access to Request,    |
//  |                       | Response, Session, etc.)                         |
//  +-----------------------+--------------------------------------------------+
//  | interface             | the DI container (DefaultMVCServiceContainer).   |
//  | (e.g. IPeopleService) | If not registered, the request fails with 500.  |
//  +-----------------------+--------------------------------------------------+
//  | class                 | * POST/PUT/PATCH -> deserialized from the JSON  |
//  | (e.g. TPerson)        |   request body                                  |
//  |                       | * GET/DELETE     -> public properties filled    |
//  |                       |   from the query string                          |
//  |                       | The instance is created and freed by the        |
//  |                       | framework, no manual cleanup needed.             |
//  +-----------------------+--------------------------------------------------+
//  | record                | hybrid binding via field-level attributes:       |
//  | (e.g. TSearchRequest) |   [MVCFromBody]        -> request body          |
//  |                       |   [MVCFromQueryString] -> query string param    |
//  |                       |   [MVCFromHeader]      -> HTTP header           |
//  |                       |   [MVCFromCookie]      -> cookie                |
//  |                       | Default-value overloads ('name', defaultValue)  |
//  |                       | are honoured when the source is missing.        |
//  +-----------------------+--------------------------------------------------+
//  | primitive             | next unconsumed route segment (in the order in  |
//  | (Integer, Int64,      | which they appear in the path pattern). Falls   |
//  |  string, Boolean,     | through to query string when no segments left.  |
//  |  Double, TDateTime,   | E.g. '/people/($id)/items/($itemId)' with       |
//  |  TGUID)               | handler args (id: Integer; itemId: Integer)     |
//  |                       | binds id <- {id}, itemId <- {itemId}.            |
//  +-----------------------+--------------------------------------------------+
//
//  Up to 4 parameters per handler. Return type must be IMVCResponse — use
//  the standalone helpers Ok / Created / NoContent / NotFound / BadRequest
//  / Status (or the controller-style equivalents) to construct it.
//
//  Route syntax follows the standard DMVCFramework convention: parameters
//  are written as ($name), e.g. /people/($id).
//
//  Important: register other middlewares (CORS, JWT, RateLimit, ...) BEFORE
//  the first MapXxx call. The minimal-api middleware short-circuits matching
//  routes; middlewares added after it would not run for those routes.
//
//  Routes themselves are wired in RoutesU.ConfigureRoutes.
// =============================================================================

uses
  System.SysUtils,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Logger,
  MVCFramework.Container,
  MVCFramework.Server.Intf,
  MVCFramework.Server.Factory,
  MVCFramework.MinimalAPI in '..\..\sources\MVCFramework.MinimalAPI.pas',
  MVCFramework.OpenAPI3 in '..\..\sources\MVCFramework.OpenAPI3.pas',
  MVCFramework.Middleware.OpenAPI3 in '..\..\sources\MVCFramework.Middleware.OpenAPI3.pas',
  Entities.PersonU in 'Entities.PersonU.pas',
  Services.PeopleU in 'Services.PeopleU.pas',
  RoutesU in 'RoutesU.pas';

{$R *.res}

const
  PORT = 8080;

procedure RegisterServices;
begin
  DefaultMVCServiceContainer
    .RegisterType(TPeopleService, IPeopleService, TRegistrationType.Singleton);
  DefaultMVCServiceContainer.Build;
end;

procedure RunServer;
var
  lEngine: TMVCEngine;
  lOAInfo: TMVCOpenAPIInfo;
begin
  WriteLn('** DMVCFramework Minimal API Sample (PREVIEW) **');
  WriteLn;

  lEngine := TMVCEngine.Create(
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.DefaultContentType] := TMVCMediaType.APPLICATION_JSON;
    end);
  try
    // ConfigureRoutes receives a TMVCRouteGroup (the engine's root group),
    // not the engine itself. From there it spawns /v1, /v2 and /search
    // sub-groups.
    ConfigureRoutes(lEngine.Root);

    // OpenAPI 3.1 emitter. Auto-discovers the minimal API routes from the
    // engine's middleware stack each time /openapi.json is requested.
    lOAInfo.Title := 'DMVC Minimal API Sample';
    lOAInfo.Version := '1.0';
    lOAInfo.Description := 'Example OpenAPI 3.1 emission over a Minimal API server.';
    lOAInfo.ContactName := 'DMVCFramework';
    lOAInfo.ContactUrl := 'https://github.com/danieleteti/delphimvcframework';
    lOAInfo.LicenseName := 'Apache-2.0';
    lEngine.AddMiddleware(TMVCOpenAPI3Middleware.Create(lEngine, lOAInfo));

    WriteLn(Format('Server starting on http://localhost:%d (Indy Direct)', [PORT]));
    WriteLn;
    WriteLn('Try:');
    WriteLn('  GET    /openapi.json     <-- OpenAPI 3.1 spec');
    WriteLn('  GET    /health');
    WriteLn('  GET    /v1/people');
    WriteLn('  GET    /v2/people');
    WriteLn('  GET    /v2/people/1');
    WriteLn('  POST   /v2/people  {"firstName":"Mario","lastName":"Rossi","age":30}');
    WriteLn('  PUT    /v2/people/1');
    WriteLn('  DELETE /v2/people/2');
    WriteLn('  GET    /v2/admin/stats   (Header: X-Admin-Key: s3cret)');
    WriteLn('  GET    /search?page=2&pageSize=10   (Header: X-Tenant: acme)');
    WriteLn('  GET    /v1/error    -> demonstrates OnError handler');
    WriteLn;
    WriteLn('Press Ctrl+C to stop.');
    TMVCServerFactory.CreateIndyDirect(lEngine).RunAndWait(PORT);
    WriteLn('Shutting down...');
  finally
    lEngine.Free;
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  IsMultiThread := True;
  try
    RegisterServices;
    RunServer;
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;
end.
