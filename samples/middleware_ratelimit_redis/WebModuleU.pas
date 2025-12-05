unit WebModuleU;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Middleware.RateLimit,
  MVCFramework.Middleware.RateLimit.Redis;

type
  TWebModule1 = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    FMVCEngine: TMVCEngine;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

{$R *.dfm}

uses
  MainControllerU,
  MVCFramework.Logger;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
var
  LRateLimitMiddleware: TMVCRateLimitMiddleware;
  LRedisStorage: IMVCRateLimitStorage;
begin
  FMVCEngine := TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.DefaultContentType] := TMVCMediaType.APPLICATION_JSON;
      Config[TMVCConfigKey.DefaultContentCharset] := TMVCConstants.DEFAULT_CONTENT_CHARSET;
    end);

  // Create Redis storage
  // Make sure Redis is running on localhost:6379
  try
    LRedisStorage := TMVCRedisRateLimitStorage.Create(
      '127.0.0.1',  // Redis host
      6379,         // Redis port
      '',           // Redis password (empty if no auth)
      'ratelimit:'  // Key prefix in Redis
    );

    // Create rate limit middleware with Redis storage
    // 20 requests per 120 seconds (2 minutes) per IP address
    LRateLimitMiddleware := TMVCRateLimitMiddleware.Create(
      20,           // Max requests
      120,          // Window in seconds
      rlkIPAddress, // Rate limit by IP address
      LRedisStorage // Use Redis storage
    );

    // Exclude health check and metrics from rate limiting
    LRateLimitMiddleware.AddExcludedPath('/health');
    LRateLimitMiddleware.AddExcludedPath('/metrics');

    // Set custom callback for rate limit exceeded event
    LRateLimitMiddleware.SetOnRateLimitExceeded(
      procedure(const AContext: TWebContext; const AKey: string;
        const ALimit: Integer; const AWindowSeconds: Integer)
      begin
        LogW(Format('[Redis] Rate limit exceeded for key: %s (Limit: %d requests per %d seconds)',
          [AKey, ALimit, AWindowSeconds]));
      end
    );

    // Add middleware and controllers
    FMVCEngine
      .AddMiddleware(LRateLimitMiddleware)
      .AddController(TMainController)
      .AddController(THealthController);

    LogI('Rate limiting configured with Redis storage');

  except
    on E: Exception do
    begin
      LogE('Failed to connect to Redis: ' + E.Message);
      LogE('Make sure Redis is running on localhost:6379');
      raise;
    end;
  end;
end;

procedure TWebModule1.WebModuleDestroy(Sender: TObject);
begin
  FMVCEngine.Free;
end;

end.
