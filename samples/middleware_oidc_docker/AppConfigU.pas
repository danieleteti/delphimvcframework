/// <summary>
/// Application configuration singleton reading values from environment
/// variables via DMVCFramework DotEnv.
/// </summary>
unit AppConfigU;

interface

type
  /// <summary>
  /// Provides typed access to all environment configuration values.
  /// All methods are class functions reading from dotEnv with sensible
  /// defaults for local development.
  /// </summary>
  TAppConfig = class
  public
    { Database }

    /// <summary>PostgreSQL host name (DB_HOST, default: localhost).</summary>
    class function DBHost: string;
    /// <summary>PostgreSQL port (DB_PORT, default: 5432).</summary>
    class function DBPort: Integer;
    /// <summary>PostgreSQL database name (DB_NAME, default: oidc_sample).</summary>
    class function DBName: string;
    /// <summary>PostgreSQL user name (DB_USER, default: oidc_sample).</summary>
    class function DBUser: string;
    /// <summary>PostgreSQL password (DB_PASSWORD).</summary>
    class function DBPassword: string;

    { OIDC }

    /// <summary>OIDC issuer URL (OIDC_ISSUER).</summary>
    class function OIDCIssuer: string;
    /// <summary>OIDC client identifier (OIDC_CLIENT_ID).</summary>
    class function OIDCClientId: string;
    /// <summary>OIDC client secret (OIDC_CLIENT_SECRET).</summary>
    class function OIDCClientSecret: string;
    /// <summary>OIDC redirect URI for the callback endpoint (OIDC_REDIRECT_URI).</summary>
    class function OIDCRedirectUri: string;

    { JWT }

    /// <summary>Secret used to sign the local session JWT cookie (JWT_SECRET).</summary>
    class function JWTSecret: string;
    /// <summary>Session lifetime in hours (JWT_EXPIRATION_HOURS, default: 8).</summary>
    class function JWTExpirationHours: Integer;

    { Application }

    /// <summary>Base URL of this application (BASE_URL, default: http://localhost:8080).</summary>
    class function BaseURL: string;
  end;

implementation

uses
  System.SysUtils,
  MVCFramework.DotEnv,
  MVCFramework.Commons;

{ TAppConfig -- Database }

class function TAppConfig.DBHost: string;
begin
  Result := dotEnv.Env('DB_HOST', 'localhost');
end;

class function TAppConfig.DBPort: Integer;
begin
  Result := dotEnv.Env('DB_PORT', 5432);
end;

class function TAppConfig.DBName: string;
begin
  Result := dotEnv.Env('DB_NAME', 'oidc_sample');
end;

class function TAppConfig.DBUser: string;
begin
  Result := dotEnv.Env('DB_USER', 'oidc_sample');
end;

class function TAppConfig.DBPassword: string;
begin
  Result := dotEnv.Env('DB_PASSWORD', 'oidc_sample_dev');
end;

{ TAppConfig -- OIDC }

class function TAppConfig.OIDCIssuer: string;
begin
  Result := dotEnv.Env('OIDC_ISSUER', '');
end;

class function TAppConfig.OIDCClientId: string;
begin
  Result := dotEnv.Env('OIDC_CLIENT_ID', '');
end;

class function TAppConfig.OIDCClientSecret: string;
begin
  Result := dotEnv.Env('OIDC_CLIENT_SECRET', '');
end;

class function TAppConfig.OIDCRedirectUri: string;
begin
  Result := dotEnv.Env('OIDC_REDIRECT_URI', 'http://localhost:8080/auth/callback');
end;

{ TAppConfig -- JWT }

class function TAppConfig.JWTSecret: string;
begin
  Result := dotEnv.Env('JWT_SECRET', '');
end;

class function TAppConfig.JWTExpirationHours: Integer;
begin
  Result := dotEnv.Env('JWT_EXPIRATION_HOURS', 8);
end;

{ TAppConfig -- Application }

class function TAppConfig.BaseURL: string;
begin
  Result := dotEnv.Env('BASE_URL', 'http://localhost:8080');
end;

end.
