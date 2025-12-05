unit WebModuleU;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Middleware.RateLimit;

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
begin
  FMVCEngine := TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.DefaultContentType] := TMVCMediaType.APPLICATION_JSON;
      Config[TMVCConfigKey.DefaultContentCharset] := TMVCConstants.DEFAULT_CONTENT_CHARSET;
    end);

  // Create rate limit middleware
  // 10 requests per 60 seconds (1 minute) per IP address
  LRateLimitMiddleware := TMVCRateLimitMiddleware.Create(
    10,           // Max requests
    60,           // Window in seconds
    rlkIPAddress  // Rate limit by IP address
  );

  // Exclude health check endpoint from rate limiting
  LRateLimitMiddleware.AddExcludedPath('/health');
  LRateLimitMiddleware.AddExcludedPath('/metrics');

  // Set custom callback for rate limit exceeded event
  LRateLimitMiddleware.SetOnRateLimitExceeded(
    procedure(const AContext: TWebContext; const AKey: string;
      const ALimit: Integer; const AWindowSeconds: Integer)
    begin
      LogW(Format('Rate limit exceeded for key: %s (Limit: %d requests per %d seconds)',
        [AKey, ALimit, AWindowSeconds]));
    end
  );

  // Add middleware and controllers
  FMVCEngine
    .AddMiddleware(LRateLimitMiddleware)
    .AddController(TMainController)
    .AddController(THealthController);
end;

procedure TWebModule1.WebModuleDestroy(Sender: TObject);
begin
  FMVCEngine.Free;
end;

end.
