program BasicAndJWT;

// HTTP Basic and JWT on the same engine. Basic credentials and Bearer tokens
// both reach /api/secure; POST /login turns Basic credentials into a token.
// See README.md for the requests to try.

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Logger,
  MVCFramework.JWT,
  MVCFramework.Middleware.Authentication,
  MVCFramework.Middleware.JWT,
  MVCFramework.Middleware.Session,
  MVCFramework.Server.Intf,
  MVCFramework.Server.Factory,
  AuthHandlerU in 'AuthHandlerU.pas',
  SecureControllerU in 'SecureControllerU.pas';

const
  PORT = 8099;
  // demo only: take the real secret from the environment (dotEnv) and keep it out of the sources
  JWT_SECRET = 'a-demo-secret-that-must-be-replaced-before-any-real-use';

procedure RunServer;
var
  lEngine: TMVCEngine;
  lServer: IMVCServer;
begin
  lEngine := TMVCEngine.Create;
  try
    lEngine.AddController(TSecureController);
    // the Basic middleware keeps the login in a session
    lEngine.AddMiddleware(UseMemorySessionMiddleware);
    lEngine.AddMiddleware(TMVCBasicAuthenticationMiddleware.Create(TSchemeAuthHandler.Create(True)));
    lEngine.AddMiddleware(TMVCJWTAuthenticationMiddleware.Create(
      TSchemeAuthHandler.Create(False),
      procedure(const AJWT: TJWT)
      begin
        AJWT.Claims.ExpirationTime := Now + 1;
      end,
      JWT_SECRET,
      '/login',
      [TJWTCheckableClaim.ExpirationTime]));
    lServer := TMVCServerFactory.CreateIndyDirect(lEngine);
    LogI('Listening on http://localhost:%d', [PORT]);
    LogI('GET  /api/secure  with Basic (daniele:secret) or with a Bearer token');
    LogI('POST /login       with Basic credentials, answers {"token": "..."}');
    LogI('CTRL+C to shutdown.');
    lServer.RunAndWait(PORT);
  finally
    lEngine.Free;
  end;
end;

begin
  IsMultiThread := True;
  try
    RunServer;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
