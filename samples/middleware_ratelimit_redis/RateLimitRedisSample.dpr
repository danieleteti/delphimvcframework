program RateLimitRedisSample;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Web.WebReq,
  Web.WebBroker,
  MVCFramework.Logger,
  MVCFramework.Commons,
  IdHTTPWebBrokerBridge,
  WebModuleU in 'WebModuleU.pas' {WebModule1: TWebModule},
  MainControllerU in 'MainControllerU.pas';

{$R *.res}

procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  LogI(Format('** DMVCFramework Rate Limit Middleware Sample (Redis) **', []));
  LogI(Format('Starting HTTP Server on port %d', [APort]));
  LogI('');
  LogI('Rate Limit Configuration:');
  LogI('  - Storage: Redis (127.0.0.1:6379)');
  LogI('  - Limit: 20 requests per 120 seconds (2 minutes)');
  LogI('  - Strategy: Fixed Window with Redis');
  LogI('  - Key: IP Address');
  LogI('');
  LogI('Prerequisites:');
  LogI('  - Redis server must be running on localhost:6379');
  LogI('  - Install Redis: https://redis.io/download');
  LogI('  - Or use Docker: docker run -p 6379:6379 redis');
  LogI('');
  LogI('Try these commands:');
  LogI('  curl http://localhost:8080/api/hello');
  LogI('  curl http://localhost:8080/api/data');
  LogI('');
  LogI('Monitor Redis:');
  LogI('  redis-cli MONITOR');
  LogI('  redis-cli KEYS "ratelimit:*"');
  LogI('');

  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;
    LServer.Active := True;
    LogI('Press RETURN to stop the server');
    ReadLn;
  finally
    LServer.Free;
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    WebRequestHandlerProc.MaxConnections := 1024;
    RunServer(8080);
  except
    on E: Exception do
      LogException(E, E.Message);
  end;
end.
