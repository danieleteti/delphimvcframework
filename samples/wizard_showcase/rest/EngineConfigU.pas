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
  System.IOUtils,
  System.DateUtils,
  MVCFramework.Commons,
  MVCFramework.Logger,
  System.SysUtils;

procedure ConfigureEngine(AEngine: TMVCEngine);
begin

  // Controllers
  // Controllers - END
  // Minimal API mode: routes are wired by ConfigureRoutes(LEngine.Root)
  // called explicitly from the .dpr right after ConfigureEngine. Cross-
  // cutting concerns (auth, logging, rate-limit) live in RoutesU as
  // endpoint filters applied via .Use() on route groups — NOT as engine-
  // wide AddMiddleware calls.


end;

end.
