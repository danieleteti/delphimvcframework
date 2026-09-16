unit SharedU;

interface

uses
  MVCFramework.JWT.RefreshToken;

const
  // Shared between the JWT validation middleware (verifies access tokens) and
  // the refresh core (mints them). They MUST agree on secret + algorithm.
  JWT_SECRET = 'mys3cr37';

var
  gRefreshCfg: TMVCRefreshTokenConfig;
  gRefreshStore: IMVCRefreshTokenStore;
  gRefreshCore: IMVCRefreshTokenCore;

implementation

initialization

gRefreshCfg := TMVCRefreshTokenConfig.Default(JWT_SECRET);
gRefreshCfg.AccessTokenTTLSeconds := 30;        // short, to make refresh observable
gRefreshCfg.RefreshTokenTTLSeconds := 24 * 3600; // 1 day
gRefreshStore := TMVCInMemoryRefreshTokenStore.Create;
gRefreshCore := TMVCRefreshTokenCore.Create(gRefreshCfg, gRefreshStore);

finalization

gRefreshCore := nil;
gRefreshStore := nil;

end.
