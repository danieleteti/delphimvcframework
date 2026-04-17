unit PerfBenchEngineConfigU;

{
  DMVCFramework Performance Benchmark - Shared Engine Config
  -----------------------------------------------------------
  Single configuration reused by the 3 server variants (WebBroker /
  IndyDirect / HttpSys). Keep this file the single source of truth so
  backends differ only in the IMVCServer implementation.

  Middleware chain rationale:
    - /bench/health, /bench/json/*, /bench/upload go through the
      baseline chain: Compression only.
    - /bench/heavy is measured with the chain active, because
      Compression is the most common real-world middleware.

  We avoid JWT/auth/session here to keep the baseline free of external
  state. Add them later in targeted scenarios if needed.
}

interface

uses
  MVCFramework;

procedure ConfigureBenchEngine(AEngine: TMVCEngine);

implementation

uses
  PerfBenchControllerU,
  MVCFramework.Middleware.Compression,
  MVCFramework.Middleware.CORS,
  MVCFramework.Middleware.Analytics,
  MVCFramework.Logger;

procedure ConfigureBenchEngine(AEngine: TMVCEngine);
begin
  AEngine
    .AddController(TPerfBenchController)
    .AddMiddleware(TMVCCORSMiddleware.Create)
    .AddMiddleware(TMVCAnalyticsMiddleware.Create(Log))
    .AddMiddleware(TMVCCompressionMiddleware.Create);
end;

end.
