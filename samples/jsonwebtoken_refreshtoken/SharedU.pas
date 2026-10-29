unit SharedU;

interface

uses
  MVCFramework.JWT.RefreshToken;

const
  JWT_SECRET = 'mys3cr37';

var
  // One store for the whole process: WebBroker creates a TWebModule1 (and so an
  // engine and a bundle) per concurrent request, and a refresh token issued by one
  // must be accepted by the others.
  gRefreshStore: IMVCRefreshTokenStore;

implementation

initialization

gRefreshStore := TMVCInMemoryRefreshTokenStore.Create;

finalization

gRefreshStore := nil;

end.
