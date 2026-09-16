program minimal_api_webapp;

{$APPTYPE CONSOLE}

// =============================================================================
//  DMVCFramework Minimal API — Server-rendered Web App (TemplatePro + HTMX)
// =============================================================================
//
//  End-to-end demonstration of the minimal-API web surface introduced on top
//  of the JSON-only minimal API. The whole app is built without a controller
//  class — handlers are lambdas wired through TMVCEngine.WebRoot / WebPrefix.
//
//  Surface exercised:
//    * WebRoot / WebPrefix         -> HTML route groups (rkWeb metadata)
//    * RenderView / ViewData       -> view engine integration
//    * [MVCFromContentField]       -> form-urlencoded binding into a record
//    * Ctx.Request.IsHTMX          -> HTMX-aware layout branching
//    * Ctx.Session                 -> cookie session via session middleware
//    * RequireLogin filter         -> Express-style auth filter recipe
//
//  Counterpart to samples/htmx_website_with_templatepro (which uses the
//  classic controller surface). Same templates, same HTMX behavior, no
//  controllers.
//
//  Try:
//    GET  http://localhost:8080/         -> home (HTML)
//    GET  http://localhost:8080/users    -> users page
//    GET  http://localhost:8080/customers
//    GET  http://localhost:8080/posts
//    GET  http://localhost:8080/login    -> login form
//    POST http://localhost:8080/login    (admin/admin) -> 302 to /admin/
//    GET  http://localhost:8080/admin/   -> requires session (else 302 /login)
//
//  Routes wired in HandlersU.RegisterRoutes.
// =============================================================================

uses
  System.SysUtils,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Logger,
  MVCFramework.Server.Intf,
  MVCFramework.Server.Factory,
  MVCFramework.View.Renderers.TemplatePro,
  MVCFramework.MinimalAPI in '..\..\sources\MVCFramework.MinimalAPI.pas',
  HandlersU in 'HandlersU.pas',
  RandomUtilsU in '..\commons\RandomUtilsU.pas';

{$R *.res}

const
  PORT = 8080;

procedure RunServer;
var
  lEngine: TMVCEngine;
begin
  lEngine := TMVCEngine.Create(
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.DefaultContentType] := TMVCMediaType.TEXT_HTML;
      Config[TMVCConfigKey.ViewPath] := 'templates';
      Config[TMVCConfigKey.DefaultViewFileExtension] := 'html';
      // ViewCache off for the sample so template edits are picked up live.
      Config[TMVCConfigKey.ViewCache] := 'false';
    end);
  try
    // TemplatePro view engine.
    lEngine.SetViewEngine(TMVCTemplateProViewEngine);

    // Wire the lambda routes (WebRoot + WebPrefix groups). Session is added
    // inside RegisterRoutes as a group-level filter via MemorySession(...),
    // no classic IMVCMiddleware involved.
    RegisterRoutes(lEngine);

    LogI('** DMVCFramework Minimal API WebApp Sample **');
    LogI(Format('Server starting on http://localhost:%d (Indy Direct)', [PORT]));
    LogI('Try:  GET / | GET /users | GET /customers | GET /posts | GET /login');
    LogI('      POST /login (admin/admin) -> 302 /admin/  |  GET /admin/');
    LogI('Press Ctrl+C to stop.');
    TMVCServerFactory.CreateIndyDirect(lEngine).RunAndWait(PORT);
    LogI('Shutting down...');
  finally
    lEngine.Free;
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  IsMultiThread := True;
  try
    RunServer;
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;
end.
