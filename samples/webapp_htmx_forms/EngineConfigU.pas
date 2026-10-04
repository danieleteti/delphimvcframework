// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// ***************************************************************************

unit EngineConfigU;

interface

uses
  MVCFramework;

procedure ConfigureEngine(AEngine: TMVCEngine);

implementation

uses
  Controllers.HomeU,
  Controllers.APIU,
  Controllers.PeoplePagesU,
  System.IOUtils,
  System.DateUtils,
  TemplatePro,
  MVCFramework.View.Renderers.TemplatePro,
  MVCFramework.Commons,
  MVCFramework.Logger,
  MVCFramework.Middleware.Redirect,
  MVCFramework.Middleware.StaticFiles,
  MVCFramework.Middleware.Compression,
  System.SysUtils;

procedure ConfigureEngine(AEngine: TMVCEngine);
var
  LWwwPath: string;
begin

  // Static files path (www folder at same level as executable)
  LWwwPath := TPath.Combine(AppPath, 'www');

  // Controllers
  AEngine.AddController(THomeController);
  AEngine.AddController(TAPIController);
  AEngine.AddController(TPeoplePagesController);
  // Controllers - END

  // Server Side View
  AEngine.SetViewEngine(TMVCTemplateProViewEngine);
  // Server Side View - END

  // Middleware
  AEngine.AddMiddleware(TMVCRedirectMiddleware.Create(['/'], '/web'));
  AEngine.AddMiddleware(TMVCStaticFilesMiddleware.Create('/static', LWwwPath));
  AEngine.AddMiddleware(TMVCCompressionMiddleware.Create);
  // Middleware - END

  // Browser requests get the HTML error view; API clients get RFC 7807 problem+json.
  // Emitted only for TemplatePro server-side views (classic web + minimal web app),
  // the only flavors that ship an 'error' view. ehShowDetails leaks the exception
  // message in DEBUG only.
  AEngine.UseExceptionHandler('error', 'WebAppHTMXForms',
    {$IFDEF DEBUG}[ehShowDetails]{$ELSE}[]{$ENDIF});
end;

end.
